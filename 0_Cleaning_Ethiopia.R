if(!require("pacman")) install.packages("pacman")

p_load("countrycode", "haven", "Hmisc", "openxlsx", "rattle", "scales", "tidyverse", "sjlabelled")


# Load Data ####
cover       <- read_dta("../0_Data/1_Household Data/2_Ethiopia/1_Data_Raw/ETH_2018_ESS_v02_M_Stata/sect_cover_hh_w4.dta")
sect1       <- read_dta("../0_Data/1_Household Data/2_Ethiopia/1_Data_Raw/ETH_2018_ESS_v02_M_Stata/sect1_hh_w4.dta")
sect2       <- read_dta("../0_Data/1_Household Data/2_Ethiopia/1_Data_Raw/ETH_2018_ESS_v02_M_Stata/sect2_hh_w4.dta")
sect4       <- read_dta("../0_Data/1_Household Data/2_Ethiopia/1_Data_Raw/ETH_2018_ESS_v02_M_Stata/sect4_hh_w4.dta")
sect6a      <- read_dta("../0_Data/1_Household Data/2_Ethiopia/1_Data_Raw/ETH_2018_ESS_v02_M_Stata/sect6a_hh_w4.dta")
sect6b1     <- read_dta("../0_Data/1_Household Data/2_Ethiopia/1_Data_Raw/ETH_2018_ESS_v02_M_Stata/sect6b1_hh_w4.dta")
sect6b2     <- read_dta("../0_Data/1_Household Data/2_Ethiopia/1_Data_Raw/ETH_2018_ESS_v02_M_Stata/sect6b2_hh_w4.dta")
sect6b3     <- read_dta("../0_Data/1_Household Data/2_Ethiopia/1_Data_Raw/ETH_2018_ESS_v02_M_Stata/sect6b3_hh_w4.dta")
sect6b4     <- read_dta("../0_Data/1_Household Data/2_Ethiopia/1_Data_Raw/ETH_2018_ESS_v02_M_Stata/sect6b4_hh_w4.dta")
sect7a1     <- read_dta("../0_Data/1_Household Data/2_Ethiopia/1_Data_Raw/ETH_2018_ESS_v02_M_Stata/sect7a_hh_w4.dta")
sect7a2     <- read_dta("../0_Data/1_Household Data/2_Ethiopia/1_Data_Raw/ETH_2018_ESS_v02_M_Stata/sect7b_hh_w4_v2.dta")
sect10a     <- read_dta("../0_Data/1_Household Data/2_Ethiopia/1_Data_Raw/ETH_2018_ESS_v02_M_Stata/sect10a_hh_w4.dta")
sect11      <- read_dta("../0_Data/1_Household Data/2_Ethiopia/1_Data_Raw/ETH_2018_ESS_v02_M_Stata/sect11_hh_w4.dta")
sec11b1     <- read_dta("../0_Data/1_Household Data/2_Ethiopia/1_Data_Raw/ETH_2018_ESS_v02_M_Stata/sect11b1_hh_w4.dta")
sec11b2     <- read_dta("../0_Data/1_Household Data/2_Ethiopia/1_Data_Raw/ETH_2018_ESS_v02_M_Stata/sect11b2_hh_w4.dta")
sec13       <- read_dta("../0_Data/1_Household Data/2_Ethiopia/1_Data_Raw/ETH_2018_ESS_v02_M_Stata/sect13_hh_w4_v2.dta")
sec14       <- read_dta("../0_Data/1_Household Data/2_Ethiopia/1_Data_Raw/ETH_2018_ESS_v02_M_Stata/sect14_hh_w4.dta")
com_03      <- read_dta("../0_Data/1_Household Data/2_Ethiopia/1_Data_Raw/ETH_2018_ESS_v02_M_Stata/sect03_com_w4.dta")
com_04      <- read_dta("../0_Data/1_Household Data/2_Ethiopia/1_Data_Raw/ETH_2018_ESS_v02_M_Stata/sect04_com_w4.dta")


# Note: Spacial Mapping is possible
cover_0 <- cover %>%
  select(household_id, ea_id, saq14, pw_w4, saq01, saq09, saq03)%>%
  mutate(urban_01 = ifelse(saq14 == 1,0,1))%>%
  rename(hh_id = household_id, hh_weights = pw_w4, hh_size = saq09, province = saq01, district = saq03)%>% # district = woreda
  select(-saq14)

sect_1.1 <- sect1 %>%
  select(household_id, individual_id, s1q03a, s1q04)%>%
  rename(age      = s1q03a, hh_id = household_id)%>%
  mutate(adults   = ifelse(age > 15,1,0),
         children = ifelse(age < 16,1,0))%>%
  group_by(hh_id)%>%
  summarise(adults    = sum(adults),
             children = sum(children))%>%
  ungroup()

sect_1.2 <- sect1 %>%
  filter(s1q01 == 1)%>%
  select(household_id, individual_id, s1q08, s1q02, s1q03a)%>%
  rename(hh_id = household_id, religion = s1q08, sex_hhh = s1q02, age_hhh = s1q03a)%>%
  mutate(religion = ifelse(is.na(religion),8,religion))

sect_2.1 <- sect2 %>%
  select(household_id, individual_id, s2q06)%>%
  rename(edu_hhh = s2q06, hh_id = household_id)

sect4.1 <- sect4 %>%
  rename(hh_id = household_id, ind_hhh = s4q34d)%>%
  select(hh_id, ind_hhh, individual_id)

sect_1.2.1 <- left_join(sect_1.2, sect_2.1)%>%
  left_join(sect4.1)%>%
  select(-individual_id)%>%
  remove_all_labels()

housing <- sect10a %>%
  select(household_id, s10aq12, s10aq21, s10aq34, s10aq38)%>%
  rename(cooking_fuel = s10aq38, lighting_fuel = s10aq34, toilet = s10aq12, water = s10aq21, hh_id = household_id)%>%
  mutate(electricity.access = ifelse(lighting_fuel %in% c(1,2,3,4),1,0))

# sec13.1 <- sec13 %>%
#   rename(hh_id = household_id)%>%
#   filter(source_cd == 105)

sec14.1 <- sec14 %>%
  rename(hh_id = household_id)%>%
  filter(assistance_cd != 2)%>%
  mutate(inc_gov_cash = ifelse(!is.na(s14q03), s14q03,0),
         inc_gov_monetary = 0)%>%
  group_by(hh_id)%>%
  summarise(inc_gov_cash = sum(inc_gov_cash),
            inc_gov_monetary = sum(inc_gov_monetary))%>%
  ungroup()

household_information <- cover_0 %>%
  left_join(sect_1.1)%>%
  left_join(sect_1.2.1)%>%
  left_join(housing)%>%
  left_join(sec14.1)%>%
  select(-ea_id)

write_csv(household_information, "../0_Data/1_Household Data/2_Ethiopia/1_Data_Clean/household_information_Ethiopia.csv")

# Expenditures

sect_6a <- sect6a %>%
  select(household_id, item_cd, s6aq04)%>%
  rename(hh_id = household_id, item_code = item_cd, expenditures = s6aq04)%>%
  mutate(expenditures_year = expenditures*52)%>%
  select(-expenditures)%>%
  filter(expenditures_year > 0 & !is.na(expenditures_year))

sect_6b4 <- sect6b4 %>%
  select(household_id, meal_id, s6bq07)%>%
  mutate(expenditures_year = s6bq07*52)%>%
  select(-s6bq07)%>%
  filter(expenditures_year > 0 & !is.na(expenditures_year))%>%
  rename(item_code = meal_id, hh_id = household_id)

sect_7a1 <- sect7a1 %>%
  select(household_id, item_cd_30day, s7q02)%>%
  mutate(item_code = item_cd_30day + 1000,
         expenditures_year = s7q02*12)%>%
  select(-item_cd_30day, -s7q02)%>%
  filter(expenditures_year > 0 & !is.na(expenditures_year))%>%
  rename(hh_id = household_id)
  
sect_7a2 <- sect7a2 %>%
  select(household_id, item_cd_12months, s7q04)%>%
  mutate(item_code = item_cd_12months + 10000,
         expenditures_year = s7q04)%>%
  select(-item_cd_12months, -s7q04)%>%
  filter(expenditures_year > 0 & !is.na(expenditures_year))%>%
  rename(hh_id = household_id)

consumption <- bind_rows(sect_6a, sect_6b4)%>%
  bind_rows(sect_7a1)%>%
  bind_rows(sect_7a2)%>%
  arrange(hh_id)

ely <- sect10a %>%
  select(household_id, s10aq35, s10aq31, s10aq05, s10aq42)%>%
  pivot_longer(-household_id, names_to = "item_code", values_to = "expenditures_month")%>%
  mutate(item_code = ifelse(item_code == "s10aq05", 98,
                            ifelse(item_code == "s10aq31", 91,
                                   ifelse(item_code == "s10aq35", 92,93))),
         expenditures_year = ifelse(item_code != 98, expenditures_month*12, expenditures_month))%>%
  rename(hh_id = household_id)%>%
  select(hh_id, item_code, expenditures_year)%>%
  filter(!is.na(expenditures_year) & expenditures_year > 0)

consumption <- bind_rows(sect_6a, sect_6b4)%>%
  bind_rows(sect_7a1)%>%
  bind_rows(sect_7a2)%>%
  bind_rows(ely)%>%
  arrange(hh_id)

write_csv(consumption, "../0_Data/1_Household Data/2_Ethiopia/1_Data_Clean/expenditures_items_Ethiopia.csv")

# Appliances

appliances <- sect11 %>%
  rename(hh_id = household_id)%>%
  select(hh_id, asset_cd, s11q00)%>%
  mutate(Appliance = ifelse(asset_cd == 1, "stove.k.01",
                            ifelse(asset_cd == 2, "stove.g.01",
                                   ifelse(asset_cd == 3, "stove.e.01",
                                          ifelse(asset_cd == 8, "radio.01",
                                                 ifelse(asset_cd == 9, "tv.01",
                                                        ifelse(asset_cd == 14, "motorcycle.01",
                                                               ifelse(asset_cd == 21, "refrigerator.01",
                                                                      ifelse(asset_cd == 22, "car.01",
                                                                             ifelse(asset_cd == 23, "other", "other"))))))))))%>%
  filter(Appliance != "other")%>%
  mutate(yn = ifelse(s11q00 == 1,1,0))%>%
  select(hh_id, Appliance, yn)%>%
  pivot_wider(names_from = "Appliance", values_from = "yn", values_fill = 0)

write_csv(appliances, "../0_Data/1_Household Data/2_Ethiopia/1_Data_Clean/appliances_0_1_Ethiopia.csv")

# Codierungen

Province.Code <- stack(attr(cover$saq01, 'labels'))%>%
  rename(province = values, Province = ind)%>%
  write_csv(., "../0_Data/1_Household Data/2_Ethiopia/2_Codes/Province.Code.csv")
District.Code <- distinct(cover, saq03)%>%
  arrange(saq03)%>%
  rename(district = saq03)%>%
  write_csv(., "../0_Data/1_Household Data/2_Ethiopia/2_Codes/District.Code.csv")
Religion.Code <- stack(attr(sect1$s1q08, 'labels'))%>%
  rename(religion = values, Religion = ind)%>%
  write_csv(., "../0_Data/1_Household Data/2_Ethiopia/2_Codes/Religion.Code.csv")
Gender.Code <- stack(attr(sect1$s1q02, 'labels'))%>%
  rename(sex_hhh = values, Gender = ind)%>%
  write_csv(., "../0_Data/1_Household Data/2_Ethiopia/2_Codes/Gender.Code.csv")
Education.code <- stack(attr(sect2$s2q06, 'labels'))%>%
  rename(edu_hhh = values, Education = ind)%>%
  write_csv(., "../0_Data/1_Household Data/2_Ethiopia/2_Codes/Education.Code.csv")
Lighting.code <- stack(attr(sect10a$s10aq34, 'labels'))%>%
  rename(lighting_fuel = values, Lighting_Fuel = ind)%>%
  write_csv(., "../0_Data/1_Household Data/2_Ethiopia/2_Codes/Lighting.Code.csv")
Cooking.code <- stack(attr(sect10a$s10aq38, 'labels'))%>%
  rename(cooking_fuel = values, Cooking_Fuel = ind)%>%
  write_csv(., "../0_Data/1_Household Data/2_Ethiopia/2_Codes/Cooking.Code.csv")
Water.code <- stack(attr(sect10a$s10aq21, 'labels'))%>%
  rename(water = values, Water = ind)%>%
  write_csv(., "../0_Data/1_Household Data/2_Ethiopia/2_Codes/Water.Code.csv")
Toilet.code <- stack(attr(sect10a$s10aq12, 'labels'))%>%
  rename(toilet = values, Toilet = ind)%>%
  write_csv(., "../0_Data/1_Household Data/2_Ethiopia/2_Codes/Toilet.Code.csv")
Industry.Code <- stack(attr(sect4$s4q34d, 'labels'))%>%
  rename(ind_hhh = values, Industry = ind)%>%
  write_csv(., "../0_Data/1_Household Data/2_Ethiopia/2_Codes/Industry.Code.csv")

Item.1 <- stack(attr(sect_6a$item_code, 'labels'))%>%
  rename(item_code = values, item_name = ind)
Item.2 <- stack(attr(sect_6b4$item_code, 'labels'))%>%
  rename(item_code = values, item_name = ind)
Item.3 <- stack(attr(sect7a1$item_cd_30day, 'labels'))%>%
  rename(item_code = values, item_name = ind)%>%
  mutate(item_code = item_code + 1000)
Item.4 <- stack(attr(sect7a2$item_cd_12months, 'labels'))%>%
  rename(item_code = values, item_name = ind)%>%
  mutate(item_code = item_code + 10000)

Items <- bind_rows(Item.1, Item.2, Item.3, Item.4)

write.xlsx(Items, "../0_Data/1_Household Data/2_Ethiopia/3_Matching_Tables/Item_Codes_Description_Ethiopia.xlsx")