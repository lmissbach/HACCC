if(!require("pacman")) install.packages("pacman")

p_load("countrycode", "haven", "Hmisc", "openxlsx", "rattle", "scales", "tidyverse", "sjlabelled")

options(scipen=999)

# Load Data ####

g_1   <- read_dta("R:/MSA/datasets/Household Microdata/Uganda/UNHS 2016/Raw data/GSEC1.dta")      # Information on Households
g_2   <- read_dta("R:/MSA/datasets/Household Microdata/Uganda/UNHS 2016/Raw data/GSEC2.dta")      # Information on Household members
# g_3   <- read_dta("R:/MSA/datasets/Household Microdata/Uganda/UNHS 2016/Raw data/GSEC3.dta")      # Information on further stuff related to household members
g_4   <- read_dta("R:/MSA/datasets/Household Microdata/Uganda/UNHS 2016/Raw data/GSEC4.dta")      # Information on education
g_5   <- read_dta("R:/MSA/datasets/Household Microdata/Uganda/UNHS 2016/Raw data/GSEC5.dta")      # Information on health
g_6a  <- read_dta("R:/MSA/datasets/Household Microdata/Uganda/UNHS 2016/Raw data/GSEC6a.dta")     # Information on consumers
g_6b  <- read_dta("R:/MSA/datasets/Household Microdata/Uganda/UNHS 2016/Raw data/GSEC6b.dta")     # Information on food consumption
g_6bb <- read_dta("R:/MSA/datasets/Household Microdata/Uganda/UNHS 2016/Raw data/GSEC6bb.dta")    # Information on fortification
g_6c  <- read_dta("R:/MSA/datasets/Household Microdata/Uganda/UNHS 2016/Raw data/GSEC6c.dta")     # Information on consumption
g_6c1 <- read_dta("R:/MSA/datasets/Household Microdata/Uganda/UNHS 2016/Raw data/GSEC6c_1.dta")   # Information on consumption
g_6d  <- read_dta("R:/MSA/datasets/Household Microdata/Uganda/UNHS 2016/Raw data/GSEC6d.dta")     # Information on consumption
g_6e  <- read_dta("R:/MSA/datasets/Household Microdata/Uganda/UNHS 2016/Raw data/GSEC6e.dta")     # Information on consumption
g_7a  <- read_dta("R:/MSA/datasets/Household Microdata/Uganda/UNHS 2016/Raw data/GSEC7a.dta")     # Information on Income
g_7b  <- read_dta("R:/MSA/datasets/Household Microdata/Uganda/UNHS 2016/Raw data/GSEC7b.dta")     # Information on Income
g_7c  <- read_dta("R:/MSA/datasets/Household Microdata/Uganda/UNHS 2016/Raw data/GSEC7c.dta")     # Information on Income/Credits
g_9   <- read_dta("R:/MSA/datasets/Household Microdata/Uganda/UNHS 2016/Raw data/GSEC9.dta")      # Information on housing conditions
g_10a <- read_dta("R:/MSA/datasets/Household Microdata/Uganda/UNHS 2016/Raw data/GSEC10A.dta")    # Information on Appliances
g_10b <- read_dta("R:/MSA/datasets/Household Microdata/Uganda/UNHS 2016/Raw data/GSEC10B.dta")    # Information on Information and communication technologies
g_11  <- read_dta("R:/MSA/datasets/Household Microdata/Uganda/UNHS 2016/Raw data/GSEC11.dta")     # Information on Income
# g_12  <- read_dta("R:/MSA/datasets/Household Microdata/Uganda/UNHS 2016/Raw data/GSEC12.dta")     # Information on Welfare Indicators and subjective poverty
# g_12b <- read_dta("R:/MSA/datasets/Household Microdata/Uganda/UNHS 2016/Raw data/GSEC12B.dta")    # Information on Welfare Indicators and subjective poverty
# g_12c <- read_dta("R:/MSA/datasets/Household Microdata/Uganda/UNHS 2016/Raw data/GSEC12C.dta")    # Information on Welfare Indicators and subjective poverty
# g_12d <- read_dta("R:/MSA/datasets/Household Microdata/Uganda/UNHS 2016/Raw data/GSEC12D.dta")    # Information on Welfare Indicators and subjective poverty
# g_13a <- read_dta("R:/MSA/datasets/Household Microdata/Uganda/UNHS 2016/Raw data/GSEC13a.dta")    # Information on farming / non-crop farming
# g_13b <- read_dta("R:/MSA/datasets/Household Microdata/Uganda/UNHS 2016/Raw data/GSEC13b.dta")    # Information on farming / non-crop farming
g_1La <- read_dta("R:/MSA/datasets/Household Microdata/Uganda/UNHS 2016/Raw data/Labour.dta")     # Information on Work
g_pov <- read_dta("R:/MSA/datasets/Household Microdata/Uganda/UNHS 2016/Raw data/pov16_rev1.dta") # Information on summary and poverty indicators

# Transform Data ####

g_1.1 <- g_1 %>%
  select(hhid, finalwgt, urban, s1aq1b, s1aq2b, s1aq6a)%>%
  rename(hh_id = hhid, hh_weights = finalwgt, Village = s1aq6a, Province = s1aq1b, district = s1aq2b)%>%
  mutate(urban_01 = ifelse(urban == 0,0,1))%>%
  select(-urban)

Village.Code <- distinct(g_1.1, Village)%>%
  arrange(Village)%>%
  mutate(village = 1:n())%>%
  write_csv(., "../0_Data/1_Household Data/2_Uganda/2_Codes/Village.Code.csv")

# Province.Code <- distinct(g_1.1, Province)%>%
#   arrange(Province)%>%
#   mutate(province = 1:n())%>%
#   write_csv(., "../0_Data/1_Household Data/2_Uganda/2_Codes/Province.Code.csv")

# weights <- g_1.1 %>%
#   select(village, hh_weights)%>%
#   group_by(village, hh_weights)%>%
#   mutate(number = n())%>%
#   ungroup()%>%
#   group_by(village)%>%
#   filter(number == max(number))%>%
#   ungroup()%>%
#   rename(weights_alt = hh_weights)%>%
#   group_by(village)%>%
#   summarise(weights_alt = first(weights_alt))%>%
#   ungroup()
# 
# g_1.2 <- left_join(g_1.1, weights)%>%
#   mutate(hh_weights = ifelse(is.na(hh_weights), weights_alt, hh_weights))%>%
#   select(-hh_weights)

g_2.1 <- g_2 %>%
  select(hhid, pid, R03, R07)%>%
  rename(hh_id = hhid)%>%
  mutate(R07      = ifelse(is.na(R07), 18, R07))%>%
  mutate(adults   = ifelse(R07 >15, 1,0))%>%
  mutate(children = ifelse(R07 <16, 1, 0))%>%
  group_by(hh_id)%>%
  summarise(adults   = sum(adults),
            children = sum(children),
            hh_size  = n())%>%
  ungroup()

g_4.1 <- g_4 %>%
  select(hhid, pid, E05)%>%
  left_join(select(g_2, hhid, pid, R03))%>%
  filter(R03 == 1)%>%
  rename(hh_id = hhid, edu_hhh = E05)%>%
  select(hh_id, edu_hhh)

g_1La.1 <- g_1La %>%
  select(hhid, pid, B4_oneDigit)%>%
  left_join(select(g_2, hhid, pid, R03))%>%
  filter(R03 == 1)%>%
  rename(hh_id = hhid, ind_hhh = B4_oneDigit)%>%
  select(hh_id, ind_hhh)

g_9.1 <- g_9 %>%
  select(hhid, HC07, HC14, HC18, HC19)%>%
  rename(hh_id = hhid, water = HC07, toilet = HC14, lighting_fuel = HC18, cooking_fuel = HC19)%>%
  remove_all_labels()%>%
  mutate(electricity.access = ifelse(lighting_fuel %in% c(1,2,3,4),1,0))

g_7a.1 <- g_7a %>%
  select(hhid, CB01_7)%>%
  rename(hh_id = hhid, inc_gov_monetary = CB01_7)%>%
  mutate(inc_gov_monetary = ifelse(inc_gov_monetary > 0 & !is.na(inc_gov_monetary),1,0))

g_11.1 <- g_11 %>%
  select(hhid, P13_24)%>%
  rename(hh_id = hhid, inc_gov_monetary = P13_24)%>%
  mutate(inc_gov_monetary = ifelse(is.na(inc_gov_monetary),0,inc_gov_monetary))

g711 <- bind_rows(g_7a.1, g_11.1)%>%
  group_by(hh_id)%>%
  summarise(inc_gov_monetary = sum(inc_gov_monetary))%>%
  ungroup()%>%
  mutate(inc_gov_cash = 0)

household_information <- g_1.1 %>%
  left_join(g_2.1)%>%
  filter(!is.na(hh_size))%>%
  left_join(g_4.1)%>%
  left_join(g_1La.1)%>%
  left_join(Village.Code)%>%
  #left_join(Province.Code)%>%
  select(-Village, - Province)%>%
  left_join(g_9.1)%>%
  left_join(g711)%>%
  # Assumption for three households
  mutate(inc_gov_cash       = ifelse(is.na(inc_gov_cash),0,inc_gov_cash),
         inc_gov_monetary   = ifelse(is.na(inc_gov_monetary),0, inc_gov_monetary))%>%
  # Remove households with missing information on housing
  filter(!is.na(electricity.access))

write_csv(household_information, "../0_Data/1_Household Data/2_Uganda/1_Data_Clean/household_information_Uganda.csv")

# Expenditures

g_6b.1 <- g_6b %>%
  select(hhid, itmcd, ceb05, ceb07, ceb09, ceb11)%>%
  rename(hh_id = hhid, item_code = itmcd, expenditures_year = ceb05)%>%
  rowwise()%>%
  mutate(expenditures_sp_year = sum(ceb07, ceb09, ceb11, na.rm = TRUE))%>%
  ungroup()%>%
  select(hh_id, item_code, expenditures_year, expenditures_sp_year)%>%
  mutate(expenditures_year    = ifelse(!is.na(expenditures_year), expenditures_year*365/7,0))%>%
  mutate(expenditures_sp_year = ifelse(!is.na(expenditures_sp_year), expenditures_sp_year*365/7,0))%>%
  remove_all_labels()

g_6c.1 <- g_6c %>%
  select(hhid, itmcd, cec05, cec07, cec09)%>%
  rename(hh_id = hhid, item_code = itmcd, expenditures_year = cec05)%>%
  rowwise()%>%
  mutate(expenditures_sp_year = sum(cec07, cec09, na.rm = TRUE))%>%
  ungroup()%>%
  select(hh_id, item_code, expenditures_year, expenditures_sp_year)%>%
  mutate(expenditures_year    = ifelse(!is.na(expenditures_year),    expenditures_year*365/30,0))%>%
  mutate(expenditures_sp_year = ifelse(!is.na(expenditures_sp_year), expenditures_sp_year*365/30,0))%>%
  remove_all_labels()%>%
  mutate(item_code = ifelse(item_code == 109, 1090, item_code))

g_6c1.1 <- g_6c1 %>%
  select(hhid, itmcd, CEB05, CEB07, CEB09, CEB011)%>%
  rename(hh_id = hhid, item_code = itmcd)%>%
  mutate_at(vars(CEB05, CEB07, CEB09, CEB011), list(~ ifelse(is.na(.),0,.)))%>%
  mutate(expenditures_year    = CEB05 + CEB07,
         expenditures_sp_year = CEB09 + CEB011)%>%
  select(hh_id, item_code, expenditures_year, expenditures_sp_year)%>%
  mutate(expenditures_year    = ifelse(!is.na(expenditures_year),    expenditures_year*365/7,0))%>%
  mutate(expenditures_sp_year = ifelse(!is.na(expenditures_sp_year), expenditures_sp_year*365/7,0))%>%
  remove_all_labels()%>%
  mutate(item_code = ifelse(item_code == 109, 1090, item_code))

g_6d.1 <- g_6d %>%
  select(hhid, itmcd, ced03, ced05)%>%
  rename(hh_id = hhid, item_code = itmcd, expenditures_year = ced03, expenditures_sp_year = ced05)%>%
  remove_all_labels()%>%
  mutate(item_code = item_code*100)%>% # Why is this? This is because we observe double counting!
  mutate_at(vars(expenditures_year, expenditures_sp_year), list(~ ifelse(is.na(.),0,.)))

g_6e.1 <- g_6e%>%
  select(hhid, CEE02, CEE03)%>%
  rename(hh_id = hhid, item_code = CEE02, expenditures_year = CEE03)%>%
  mutate(expenditures_sp_year = 0)%>%
  remove_all_labels()

g_6_total <- g_6b.1 %>%
  bind_rows(g_6c.1)%>%
  bind_rows(g_6c1.1)%>%
  bind_rows(g_6d.1)%>%
  bind_rows(g_6e.1)%>%
  arrange(hh_id, item_code)%>%
  group_by(hh_id, item_code)%>%
  summarise(expenditures_year    = sum(expenditures_year),
            expenditures_sp_year = sum(expenditures_sp_year))%>%
  ungroup()%>%
  filter(hh_id %in% household_information$hh_id)

write_csv(g_6_total, "../0_Data/1_Household Data/2_Uganda/1_Data_Clean/expenditures_items_Uganda.csv")

# Expenses on Education in g_4
# On Health in g_5
# Both included though

# Appliances

g_10.1 <- g_10a %>%
  select(hhid, starts_with("ha03"))%>%
  rename_at(vars(starts_with("ha03_")), list(~ str_replace(., "ha03_", "")))%>%
  rename(hh_id = hhid)%>%
  select(hh_id,  "6", "7", "8", "9", "11", "13", "14", "16", "17")%>%
  rename(cooker.01 = "6", 
         refrigerator.01 = "7",
         tv.01 = "8", 
         radio.01 = "9", 
         mobile.01 = "11", 
         computer.01 = "13", 
         generator.01 = "14", 
         car.01 = "16", 
         motorcycle.01 = "17")%>%
  mutate_at(vars("cooker.01":"motorcycle.01"), 
            list(~ ifelse(is.na(.) | . == 3,0,1)))%>%
  filter(hh_id %in% household_information$hh_id)

write_csv(g_10.1, "../0_Data/1_Household Data/2_Uganda/1_Data_Clean/appliances_0_1_Uganda.csv")

# Codes ####
Province.Code <- distinct(g_1, s1aq1a, s1aq1b)%>%
  arrange(s1aq1b)%>%
  group_by(s1aq1a)%>%
  summarise(s1aq1b = first(s1aq1b))%>%
  ungroup()%>%
  rename(province = s1aq1b, Province = s1aq1a)%>%
  write_csv(., "../0_Data/1_Household Data/2_Uganda/2_Codes/Province.Code.csv")
Education.Code <- stack(attr(g_4$E05, 'labels'))%>%
  rename(edu_hhh = values, Education = ind)%>%
  write_csv(., "../0_Data/1_Household Data/2_Uganda/2_Codes/Education.Code.csv")
Industry.Code <- stack(attr(g_1La$B4_oneDigit, 'labels'))%>%
  rename(ind_hhh = values, Industry = ind)%>%
  write_csv(., "../0_Data/1_Household Data/2_Uganda/2_Codes/Industry.Code.csv")
District.Code <- stack(attr(g_1$s1aq2b, 'labels'))%>%
  rename(district = values, District = ind)%>%
  write_csv(., "../0_Data/1_Household Data/2_Uganda/2_Codes/District.Code.csv")
Toilet.Code <- stack(attr(g_9$HC14, 'labels'))%>%
  rename(toilet = values, Toilet = ind)%>%
  write_csv(., "../0_Data/1_Household Data/2_Uganda/2_Codes/Toilet.Code.csv")
Water.Code <- stack(attr(g_9$HC07, 'labels'))%>%
  rename(water = values, Water = ind)%>%
  write_csv(., "../0_Data/1_Household Data/2_Uganda/2_Codes/Water.Code.csv")
Lighting.Code <- stack(attr(g_9$HC18, 'labels'))%>%
  rename(lighting_fuel = values, Lighting_Fuel = ind)%>%
  write_csv(., "../0_Data/1_Household Data/2_Uganda/2_Codes/Lighting.Code.csv")
Cooking.Code <- stack(attr(g_9$HC19, 'labels'))%>%
  rename(cooking_fuel = values, Cooking_Fuel = ind)%>%
  write_csv(., "../0_Data/1_Household Data/2_Uganda/2_Codes/Cooking.Code.csv")

Item.Codes <- distinct(g_6_total, item_code)%>%
  arrange(item_code)
Item.Codes.B <- stack(attr(g_6b$itmcd, 'labels'))%>%
  rename(item_code = values, item_name = ind)%>%
  mutate(Type = "B")
Item.Codes.C <- stack(attr(g_6c$itmcd, 'labels'))%>%
  rename(item_code = values, item_name = ind)%>%
  mutate(Type = "C")%>%
  mutate(item_code = ifelse(item_code == 109, 1090, item_code))
Item.Codes.C1 <- stack(attr(g_6c1$itmcd, 'labels'))%>%
  rename(item_code = values, item_name = ind)%>%
  mutate(Type = "C1")%>%
  mutate(item_code = ifelse(item_code == 109, 1090, item_code))
Item.Codes.D <- stack(attr(g_6d$itmcd, 'labels'))%>%
  rename(item_code = values, item_name = ind)%>%
  mutate(Type = "D")%>%
  mutate(item_code = item_code*100)
Item.Codes.E <- stack(attr(g_6e$CEE02, 'labels'))%>%
  rename(item_code = values, item_name = ind)%>%
  mutate(Type = "E")

Item.Codes <- Item.Codes %>%
  left_join(bind_rows(Item.Codes.B, Item.Codes.C, Item.Codes.C1, Item.Codes.D, Item.Codes.E))

write.xlsx(Item.Codes, "../0_Data/1_Household Data/2_Uganda/3_Matching_Tables/Item_Codes_Description_Uganda.xlsx")
