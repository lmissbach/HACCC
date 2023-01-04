if(!require("pacman")) install.packages("pacman")

p_load("countrycode", "haven", "Hmisc", "openxlsx", "rattle", "scales", "tidyverse", "sjlabelled")

options(scipen=999)

# Loading Data

pak_0 <- read_dta("../0_Data/1_Household Data/1_Pakistan/1_Data_Raw/plist.dta") # contains information on household and persons
pak_00 <- read_dta("../0_Data/1_Household Data/1_Pakistan/1_Data_Raw/roster.dta") # more or less the same as pak_0
pak_00a <- read_dta("../0_Data/1_Household Data/1_Pakistan/1_Data_Raw/sec_00a.dta") # information on interview itself
# pak_01b <- read_dta("../0_Data/1_Household Data/1_Pakistan/1_Data_Raw/sec_1b.dta") # information on employment
pak_02a <- read_dta("../0_Data/1_Household Data/1_Pakistan/1_Data_Raw/sec_2ab.dta") # information on education
# pak_03a <- read_dta("../0_Data/1_Household Data/1_Pakistan/1_Data_Raw/sec_3a.dta") # information on child health
# pak_03b <- read_dta("../0_Data/1_Household Data/1_Pakistan/1_Data_Raw/sec_3b.dta") # information on immunisation
# pak_03c <- read_dta("../0_Data/1_Household Data/1_Pakistan/1_Data_Raw/sec_3c.dta") # information on children illness
# pak_04a <- read_dta("../0_Data/1_Household Data/1_Pakistan/1_Data_Raw/sec_4a.dta") # information on pregnancy and marriage (women)
# pak_04b <- read_dta("../0_Data/1_Household Data/1_Pakistan/1_Data_Raw/sec_4b.dta") # information on children
# pak_04c <- read_dta("../0_Data/1_Household Data/1_Pakistan/1_Data_Raw/sec_4c.dta") # information on family planning
# pak_04d <- read_dta("../0_Data/1_Household Data/1_Pakistan/1_Data_Raw/sec_4d.dta") # information on post-natal checks
# pak_04e <- read_dta("../0_Data/1_Household Data/1_Pakistan/1_Data_Raw/sec_4e.dta") # information on female decision making
# pak_04f <- read_dta("../0_Data/1_Household Data/1_Pakistan/1_Data_Raw/sec_4f.dta") # information on female information access
pak_05m <- read_dta("../0_Data/1_Household Data/1_Pakistan/1_Data_Raw/sec_5m.dta") # information on household - hard facts
pak_06a <- read_dta("../0_Data/1_Household Data/1_Pakistan/1_Data_Raw/sec_6abcde.dta") # information on consumption
pak_07m <- read_dta("../0_Data/1_Household Data/1_Pakistan/1_Data_Raw/sec_7m.dta") # information on appliances :)
pak_08 <- read_dta("../0_Data/1_Household Data/1_Pakistan/1_Data_Raw/sec_8.dta") # information on remittances
# pak_09a <- read_dta("../0_Data/1_Household Data/1_Pakistan/1_Data_Raw/sec_9a.dta") # information on land ownership
# pak_09b <- read_dta("../0_Data/1_Household Data/1_Pakistan/1_Data_Raw/sec_9b.dta") # financial assets and liabilites
# pak_10a <- read_dta("../0_Data/1_Household Data/1_Pakistan/1_Data_Raw/sec_10a.dta") # agricultural facts
# pak_10b <- read_dta("../0_Data/1_Household Data/1_Pakistan/1_Data_Raw/sec_10a.dta") # agricultural facts
# pak_11ab <- read_dta("../0_Data/1_Household Data/1_Pakistan/1_Data_Raw/sec_11ab.dta") #  information on non-agricultural activities
pak_12a <- read_dta("../0_Data/1_Household Data/1_Pakistan/1_Data_Raw/sec_12a.dta") # information on income
# pak_12b <- read_dta("../0_Data/1_Household Data/1_Pakistan/1_Data_Raw/sec_12b.dta") # summary on expenditures
# pak_12c <- read_dta("../0_Data/1_Household Data/1_Pakistan/1_Data_Raw/sec_12c.dta") # information on income/expenditure ratio
# pak_12d <- read_dta("../0_Data/1_Household Data/1_Pakistan/1_Data_Raw/sec_12d.dta") # conclusion on whatever (financial assets)
# pak_12e <- read_dta("../0_Data/1_Household Data/1_Pakistan/1_Data_Raw/sec_12e.dta") # summary
# pak_wei <- read_dta("../0_Data/1_Household Data/1_Pakistan/1_Data_Raw/weight_file.dta") # information on weights

# Data Transformation

pak_0_1 <- pak_0 %>%
  select(hhcode, province, region, idc, stratum ,
         s1aq04, s1aq02, age, weights, s1aq11)%>% # we omit hhcode_new because it appears as if it was more accurate
  rename(hh_id = hhcode, urban = region, household.head = s1aq02, hh_weights = weights, member = s1aq11, sex_hhh = s1aq04,
         district = stratum)%>%
  mutate(adults = ifelse(age >= 18, 1, 0))%>%
  mutate(children = ifelse(age < 18, 1, 0))%>%
  group_by(hh_id)%>%
  mutate(hh_size  = sum(member))%>%
  mutate(adults   = sum(adults))%>%
  mutate(children = sum(children))%>%
  ungroup()%>%
  select(-member, - age)%>%
  filter(household.head == 1)%>%
  select(-household.head)%>%
  rename(household.head.ID = idc)%>%
  select(hh_id, hh_size, hh_weights, province, district, urban, adults, children, everything())%>%
  mutate(urban_01 = ifelse(urban == 1,0,1))%>%
  select(-urban)

# Population: 162.846.321

pak_02a_1 <- pak_02a %>%
  select(hhcode, idc, s2bq01, s2bq05, s2bq14)%>%
  mutate(edu_hhh = ifelse(s2bq01 == 1, 0, ifelse(s2bq01 == 2, s2bq05, s2bq14)))%>%
  rename(hh_id = hhcode, household.head.ID = idc)%>%
  select(hh_id, household.head.ID, edu_hhh)

pak_0_1.1 <- pak_0_1 %>%
  left_join(pak_02a_1, by = c("hh_id", "household.head.ID"))%>%
  select(-household.head.ID)

pak_05m_1 <- pak_05m %>%
  select(hhcode, s5q04a, s5q05, s5q14)%>%
  mutate(electricity.access = ifelse(s5q04a == 3, 0, 1))%>%
  select(-s5q04a)%>%
  rename(hh_id = hhcode, water = s5q05, toilet = s5q14)

pak_08_1 <- pak_08 %>%
  select(hhcode, itc, value)%>%
  rename(hh_id = hhcode)%>%
  mutate(type = ifelse(itc %in% c(801, 809, 810), "inc_gov_cash",
                       ifelse(itc %in% c(814), "inc_gov_monetary", "none")))%>%
  filter(type != "none" & value != 2 & value != 1)%>%
  group_by(hh_id, type)%>%
  summarise(value = sum(value))%>%
  ungroup()%>%
  pivot_wider(names_from = "type", values_from = "value")

pak_12a_1 <- pak_12a %>%
  select(hhcode, bs1qc7)%>%
  rename(hh_id = hhcode, inc_gov_monetary = bs1qc7)%>%
  filter(!is.na(inc_gov_monetary))

pak_income <- bind_rows(pak_08_1, pak_12a_1)%>%
  mutate_at(vars(-hh_id), list(~ ifelse(is.na(.),0,.)))%>%
  group_by(hh_id)%>%
  summarise(inc_gov_monetary = sum(inc_gov_monetary),
            inc_gov_cash     = sum(inc_gov_cash))%>%
  ungroup()

pak_0_1.2 <- pak_0_1.1 %>%
  left_join(pak_05m_1, by = "hh_id")%>%
  left_join(pak_income)%>%
  mutate_at(vars(inc_gov_cash, inc_gov_monetary), list(~ ifelse(is.na(.),0,.)))

write_csv(pak_0_1.2, "../0_Data/1_Household Data/1_Pakistan/1_Data_Clean/household_information_Pakistan.csv")

# Expenditures ####

pak_06_1 <- pak_06a %>%
  rename(hh_id = hhcode, item_code = itc)%>%
  select(hh_id, item_code, v1, v2, v3, v4)%>%
  mutate_at(vars(v1, v2, v3, v4), list(~ ifelse(is.na(.),0,.)))%>%
  mutate(expenditures_year = v1,
         expenditures_sp_year = v2 + v3 + v4)%>%
  select(hh_id, item_code, expenditures_year, expenditures_sp_year)%>%
  filter(expenditures_year > 0 | expenditures_sp_year > 0)

write_csv(pak_06_1, "../0_Data/1_Household Data/1_Pakistan/1_Data_Clean/expenditures_items_Pakistan.csv")

pak_07.1 <- pak_07m %>%
  select(hhcode, itc, s7mq02)%>%
  rename(hh_id = hhcode, item_code = itc, number = s7mq02)%>%
  spread(key = "item_code", value = "number", fill = 0)%>%
  select(-'700', - '708', - '709', - '713', - '719', - '723')

colnames(pak_07.1) <- c("hh_id", "refrigerator.01", "freezer.01", "ac.01", "air.cooler.01", "fan.01", "boiler.01", "washing_machine.01", "stove.g.01", "microwave.01", "heater.01", "car.01", "motorcycle.01", "tv.01", "video.01", "radio.01", "vacuum.01", "sewing_machine.01", "computer.01")

pak_07.1 <- pak_07.1 %>%
  mutate_at(vars("refrigerator.01":"computer.01"), function(x){ifelse(x > 0, 1, 0)})%>%
  right_join(select(pak_0_1.2, hh_id))%>%
  # Assumption, potentially problematic
  mutate_at(vars(-hh_id), list(~ ifelse(is.na(.),0,.)))

write_csv(pak_07.1, "../0_Data/1_Household Data/1_Pakistan/1_Data_Clean/appliances_0_1_Pakistan.csv")

# Codierungen ####
District.Code <- distinct(pak_0, stratum)%>%
  arrange(stratum)%>%
  rename(district = stratum)%>%
  mutate(District = district)%>%
  write_csv(., "../0_Data/1_Household Data/1_Pakistan/2_Codes/District.Code.csv")
Province.Code <- stack(attr(pak_0$province, 'labels'))%>%
  rename(province = values, Province = ind)%>%
  write_csv(., "../0_Data/1_Household Data/1_Pakistan/2_Codes/Province.Code.csv")
Gender.Code <- stack(attr(pak_0$s1aq04, 'labels'))%>%
  rename(sex_hhh = values, Gender = ind)%>%
  write_csv(., "../0_Data/1_Household Data/1_Pakistan/2_Codes/Gender.Code.csv")
Education.Code <- stack(attr(pak_02a$s2bq05, 'labels'))%>%
  rename(edu_hhh = values, Education = ind)%>%
  write_csv(., "../0_Data/1_Household Data/1_Pakistan/2_Codes/Education.Code.csv")
Water.Code <- stack(attr(pak_05m$s5q05, 'labels'))%>%
  rename(water = values, Water = ind)%>%
  write_csv(., "../0_Data/1_Household Data/1_Pakistan/2_Codes/Water.Code.csv")
Toilet.Code <- stack(attr(pak_05m$s5q14, 'labels'))%>%
  rename(toilet = values, Toilet = ind)%>%
  write_csv(., "../0_Data/1_Household Data/1_Pakistan/2_Codes/Toilet.Code.csv")