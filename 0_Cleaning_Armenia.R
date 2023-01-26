library("haven")
library("Hmisc")
library("openxlsx")
library("rattle")
library("scales")
library("sjlabelled")
library("tidyverse")
options(scipen=999)

# To clean household data from Armenia
# Author: L. Missbach (missbach@mcc-berlin.net)

# Load Data ####

data_0.1 <- read_dta("../0_Data/1_Household Data/4_Armenia/1_Data_Raw/ARM_2017_ILCS_v01_M_STATA8/basket.dta")
data_0.2 <- read_dta("../0_Data/1_Household Data/4_Armenia/1_Data_Raw/ARM_2017_ILCS_v01_M_STATA8/coicop.dta")
data_0.3 <- read_dta("../0_Data/1_Household Data/4_Armenia/1_Data_Raw/ARM_2017_ILCS_v01_M_STATA8/hh.dta")
data_0.4 <- read_dta("../0_Data/1_Household Data/4_Armenia/1_Data_Raw/ARM_2017_ILCS_v01_M_STATA8/mem.dta")
data_0.5 <- read_dta("../0_Data/1_Household Data/4_Armenia/1_Data_Raw/ARM_2017_ILCS_v01_M_STATA8/weight.dta")

data_1 <- read_dta("../0_Data/1_Household Data/4_Armenia/1_Data_Raw/ARM_2017_ILCS_v01_M_STATA8/x1.dta")
# data_2 <- read_dta("../0_Data/1_Household Data/4_Armenia/1_Data_Raw/ARM_2017_ILCS_v01_M_STATA8/x2.dta")
data_3 <- read_dta("../0_Data/1_Household Data/4_Armenia/1_Data_Raw/ARM_2017_ILCS_v01_M_STATA8/x3.dta")
data_4 <- read_dta("../0_Data/1_Household Data/4_Armenia/1_Data_Raw/ARM_2017_ILCS_v01_M_STATA8/x4.dta")
data_5 <- read_dta("../0_Data/1_Household Data/4_Armenia/1_Data_Raw/ARM_2017_ILCS_v01_M_STATA8/x5.dta")
data_6 <- read_dta("../0_Data/1_Household Data/4_Armenia/1_Data_Raw/ARM_2017_ILCS_v01_M_STATA8/z3.dta")


# Transform Data ####

# Household ####

data_0.5.1 <- data_0.5 %>%
  rename(hh_id = recno, hh_weights = weight)

data_0.3.1 <- data_0.3 %>%
  rename(hh_id = recno, district = marz, water = c13, heating_fuel = c16_1)%>%
  mutate(urban_01 = ifelse(typev == 1 | typev == 2, 1, 0))%>%
  mutate(electricity.access = ifelse(c10_10 == 1 | c10_10 == 3,1,0))%>%
  mutate(inc_gov_cash     = ifelse(m1 != 4,1,0),
         inc_gov_monetary = ifelse(z1 == 1 & !is.na(z1),1,0))%>%
  select(hh_id, district, water, urban_01, electricity.access, heating_fuel, inc_gov_cash, inc_gov_monetary)

data_0.4.1 <- data_0.4 %>%
  rename(hh_id = recno)%>%
  mutate(adults   = ifelse(age > 15,1,0),
         children = ifelse(age < 16,1,0))%>%
  group_by(hh_id)%>%
  summarise(adults   = sum(adults),
            children = sum(children),
            hh_size  = n())%>%
  ungroup()

data_0.4.2 <- data_0.4 %>%
  rename(hh_id = recno, sex_hhh = a1_1, age_hhh = age, ethnicity = a1_7, edu_hhh = a1_10, ind_hhh = d1_4)%>%
  filter(a1_2 == 1)%>%
  select(hh_id, sex_hhh, age_hhh, ethnicity, edu_hhh, ind_hhh)

household_information <- data_0.5.1 %>%
  left_join(data_0.4.1)%>%
  left_join(data_0.4.2)%>%
  left_join(data_0.3.1)

write_csv(household_information, "../0_Data/1_Household Data/4_Armenia/1_Data_Clean/household_information_Armenia.csv")

# Expenditures ####

data_0.3.2 <- data_0.3 %>%
  rename(hh_id = recno)%>%
  select(hh_id, c3_1,
         #c33_1_1, c33_1_2, c33_1_3, c33_2, c33_3
         )%>%
  mutate(expenditures_year = c3_1*12,
         item_code = 301)%>%
  filter(!is.na(expenditures_year))%>%
  select(-c3_1)

data_0.4.3 <- data_0.4 %>%
  rename(hh_id = recno)%>%
  select(hh_id, e2_21drm, e2_23drm, e2_25drm, e2_27drm,
         e2_31drm, i1_13, i1_20, i1_34, i2_10)%>%
  pivot_longer(-hh_id, names_to = "item_code", values_to = "expenditures")%>%
  filter(!is.na(expenditures))%>%
  mutate(expenditures_year = ifelse(item_code == "e2_21drm", expenditures,
                                    ifelse(item_code %in% c("e2_23drm", "e2_25drm", "e2_27drm", "i1_13", "i1_20"), expenditures*12,
                                           ifelse(item_code == "e2_31drm", expenditures*52,
                                                  ifelse(item_code %in% c("i1_34", "i2_10"), expenditures,0)))))%>%
  filter(expenditures_year > 0)%>%
  select(-expenditures)

data_1.1 <- data_1 %>%
  rename(hh_id = recno, item_code = x1_1, expenditures = x1_4)%>%
  select(hh_id, item_code, expenditures)%>%
  group_by(hh_id, item_code)%>%
  summarise(expenditures = sum(expenditures))%>%
  ungroup()%>%
  mutate(expenditures_year = expenditures*12)%>%
  select(-expenditures)

data_3.1 <- data_3 %>%
  rename(hh_id = recno, item_code = x3_1, expenditures = x3_2drm)%>%
  group_by(hh_id, item_code)%>%
  summarise(expenditures = sum(expenditures))%>%
  ungroup()%>%
  mutate(expenditures_year = expenditures*12)%>%
  select(-expenditures)%>%
  mutate(item_code = item_code*1000)

data_4.1 <- data_4 %>%
  rename(hh_id = recno, item_code = x4_1, expenditures = x4_3drm)%>%
  group_by(hh_id, item_code)%>%
  summarise(expenditures = sum(expenditures))%>%
  ungroup()%>%
  mutate(expenditures_year = expenditures*12)%>%
  select(-expenditures)

data_6.1 <- data_6 %>%
  rename(hh_id = recno, expenditures_year = z3_2drm, item_code = row)%>%
  filter(!is.na(z3_1))%>%
  select(hh_id, item_code, expenditures_year)%>%
  arrange(item_code)

expenditures_items_Armenia <- bind_rows(data_1.1, data_3.1, data_4.1, data_6.1)%>%
  bind_rows(data_0.3.2)%>%
  arrange(item_code)%>%
  mutate(item_code = as.character(item_code))%>%
  bind_rows(data_0.4.3)%>%
  left_join(Item.Codes.1)%>%
  select(hh_id, item_code_new, expenditures_year)%>%
  rename(item_code = item_code_new)%>%
  group_by(hh_id, item_code)%>%
  summarise(expenditures_year = sum(expenditures_year))%>%
  ungroup()

write_csv(expenditures_items_Armenia, "../0_Data/1_Household Data/4_Armenia/1_Data_Clean/expenditures_items_Armenia.csv")

# Appliances ####

appliances_0_1.1 <- data_0.3 %>%
  rename(hh_id = recno, computer.01 = c11_3, tv.01 = c11_5, washing_machine.01 = c11_6)%>%
  select(hh_id, computer.01, tv.01, washing_machine.01)%>%
  mutate_at(vars(-hh_id), list(~ ifelse(. == 1 | . == 4,1,0)))

appliances_0_1.2 <- data_6 %>%
  rename(hh_id = recno)%>%
  select(hh_id, row, z3_4)%>%
  mutate(value = ifelse(!is.na(z3_4) & z3_4 == 1,1,0))%>%
  mutate(appliance = ifelse(row == 3, "car.01",
                            ifelse(row == 4, "motorcycle.01",
                                   ifelse(row == 8 | row == 9, "computer.01",
                                          ifelse(row == 10 | row == 11, "tv.01",
                                                 ifelse(row == 13, "ac.01",
                                                        ifelse(row == 18 | row == 21, "stove.e.01",
                                                               ifelse(row == 19, "stove.g.01",
                                                                      ifelse(row == 20, "microwave.01",
                                                                             ifelse(row == 24, "refrigerator.01",
                                                                                    ifelse(row == 25, "washing_machine.01",
                                                                                           ifelse(row == 28, "iron.01",
                                                                                                  ifelse(row == 29, "vacuum.01", NA)))))))))))))%>%
  filter(!is.na(appliance))%>%
  select(hh_id, appliance, value)%>%
  group_by(hh_id, appliance)%>%
  summarise(value = max(value))%>%
  ungroup()%>%
  pivot_wider(names_from = "appliance", values_from = "value")

write_csv(appliances_0_1.2, "../0_Data/1_Household Data/4_Armenia/1_Data_Clean/appliances_0_1_Armenia.csv")

# Codes ####

data_0.2.1 <- data_0.2 %>%
  select(-coicop_text)%>%
  select(prcode, gr4, coicop_text_en)%>%
  mutate(item_code_new = str_replace_all(gr4, "[.]", ""))%>%
  select(prcode, coicop_text_en, item_code_new)%>%
  rename(item_code = prcode, item_name = coicop_text_en)%>%
  mutate(item_code = as.character(item_code))

Item.Codes.0 <- distinct(expenditures_items_Armenia, item_code)%>%
  left_join(data_0.2.1)%>%
  mutate(item_code_new = ifelse(is.na(item_code_new), item_code, item_code_new))

write.xlsx(Item.Codes.0, "../0_Data/1_Household Data/4_Armenia/3_Matching_Tables/Item_Codes_Description_Armenia.xlsx")

Item.Codes.1 <- Item.Codes.0 %>%
  select(-item_name)%>%
  mutate(item_code_new = ifelse(is.na(item_code_new), item_code, item_code_new))

District.Code <- stack(attr(data_0.3$marz, 'labels'))%>%
  rename(district = values, District = ind)%>%
  write_csv(., "../0_Data/1_Household Data/4_Armenia/2_Codes/District.Code.csv")
Heating.Code <- stack(attr(data_0.3$c16_1, 'labels'))%>%
  rename(heating_fuel = values, Heating_Fuel = ind)%>%
  write_csv(., "../0_Data/1_Household Data/4_Armenia/2_Codes/Heating.Code.csv")
Water.Code <- stack(attr(data_0.3$c13, 'labels'))%>%
  rename(water = values, Water = ind)%>%
  write_csv(., "../0_Data/1_Household Data/4_Armenia/2_Codes/Water.Code.csv")
Gender.Code <- stack(attr(data_0.4$a1_1, 'labels'))%>%
  rename(sex_hhh = values, Gender = ind)%>%
  write_csv(., "../0_Data/1_Household Data/4_Armenia/2_Codes/Gender.Code.csv")
Ethnicity.Code <- stack(attr(data_0.4$a1_7, 'labels'))%>%
  rename(ethnicity = values, Ethnicity = ind)%>%
  bind_rows(data.frame("ethnicity" = c(3,4,5,6,7),
                       "Ethnicity" = c("Georgia", "Ukraine", "Iran", "USA", "Other")))%>%
  write_csv(., "../0_Data/1_Household Data/4_Armenia/2_Codes/Ethnicity.Code.csv")
Education.Code <- stack(attr(data_0.4$a1_10, 'labels'))%>%
  rename(edu_hhh = values, Education = ind)%>%
  write_csv(., "../0_Data/1_Household Data/4_Armenia/2_Codes/Education.Code.csv")
Industry.Code <- distinct(data_0.4.2, ind_hhh)%>%
  arrange(ind_hhh)
Industry.Code.0 <- read.xlsx("../0_Data/1_Household Data/4_Armenia/2_Codes/Industry.Codes.xlsx")
Industry.Code <- left_join(Industry.Code, Industry.Code.0)%>%
  write_csv(., "../0_Data/1_Household Data/4_Armenia/2_Codes/Industry.Code.csv")
