library("haven")
library("Hmisc")
library("openxlsx")
library("rattle")
library("scales")
library("sjlabelled")
library("tidyverse")
options(scipen=999)

# To clean household data from Maldives

# Author: L. Missbach (missbach@mcc-berlin.net)

# Load Data ####

data_0 <- read_dta("../0_Data/1_Household Data/1_Maldives/1_Data_Raw/Dataset/HIES2019_STATA format/CombinedIncome_HHLevel.dta") # Income information
data_1 <- read_dta("../0_Data/1_Household Data/1_Maldives/1_Data_Raw/Dataset/HIES2019_STATA format/hh_durble_1.dta") # Information on major household appliances
data_2 <- read_dta("../0_Data/1_Household Data/1_Maldives/1_Data_Raw/Dataset/HIES2019_STATA format/hh_durble_2.dta") # Information on minor household appliances
data_3 <- read_dta("../0_Data/1_Household Data/1_Maldives/1_Data_Raw/Dataset/HIES2019_STATA format/hh_furniture.dta") # Information on household furniture and expenditures
data_4 <- read_dta("../0_Data/1_Household Data/1_Maldives/1_Data_Raw/Dataset/HIES2019_STATA format/hh_rep_main.dta") # Information on repairs

data_5 <- read_dta("../0_Data/1_Household Data/1_Maldives/1_Data_Raw/Dataset/HIES2019_STATA format/hh_utensil.dta") # information on kitchen utensils
data_6 <- read_dta("../0_Data/1_Household Data/1_Maldives/1_Data_Raw/Dataset/HIES2019_STATA format/hhlevel.dta") # Information about household members
data_7 <- read_dta("../0_Data/1_Household Data/1_Maldives/1_Data_Raw/Dataset/HIES2019_STATA format/master_exp.dta") # Information on expenditures
data_8 <- read_dta("../0_Data/1_Household Data/1_Maldives/1_Data_Raw/Dataset/HIES2019_STATA format/med_without_conslt.dta") # Medicine
# data_9 <- read_dta("../0_Data/1_Household Data/1_Maldives/1_Data_Raw/Dataset/HIES2019_STATA format/momey_tran_isld.dta") # Transfers paid from Maldives

data_10 <- read_dta("../0_Data/1_Household Data/1_Maldives/1_Data_Raw/Dataset/HIES2019_STATA format/nonfood_expunit.dta") # Non-food expenditures
data_11 <- read_dta("../0_Data/1_Household Data/1_Maldives/1_Data_Raw/Dataset/HIES2019_STATA format/occupation.dta") # data on employment
data_12 <- read_dta("../0_Data/1_Household Data/1_Maldives/1_Data_Raw/Dataset/HIES2019_STATA format/other_exp.dta") # Other expenditures
data_13 <- read_dta("../0_Data/1_Household Data/1_Maldives/1_Data_Raw/Dataset/HIES2019_STATA format/patientin.dta")  # patient treatment
data_14 <- read_dta("../0_Data/1_Household Data/1_Maldives/1_Data_Raw/Dataset/HIES2019_STATA format/patientout.dta") # patient treatment

data_15 <- read_dta("../0_Data/1_Household Data/1_Maldives/1_Data_Raw/Dataset/HIES2019_STATA format/travel_abr.dta") # expenditures on travel
data_16 <- read_dta("../0_Data/1_Household Data/1_Maldives/1_Data_Raw/Dataset/HIES2019_STATA format/travel_nat.dta") # expenditures on travel
data_17 <- read_dta("../0_Data/1_Household Data/1_Maldives/1_Data_Raw/Dataset/HIES2019_STATA format/Usualmembers.dta") # Personal information

# Transform data

data_6.1 <- data_6 %>%
  rename(hh_id = uqhh__id, province = atoll_code, toilet = hh_sewer_typ, water = hh_drkwater,
         cooking_fuel = hh_ckfuel_typ, hh_weights = wgt)%>%
  mutate(toilet = ifelse(is.na(toilet)& hh_toilet_fac == 2, 6,
                         ifelse(is.na(toilet),5, toilet)))%>%
  select(hh_id, hh_weights, province, toilet, water, cooking_fuel)

data_17.1 <- data_17 %>%
  rename(hh_id = uqhh__id)%>%
  mutate(age = ifelse(is.na(Age),0, Age))%>%
  mutate(adults   = ifelse(age > 15,1,0),
         children = ifelse(age < 16,1,0))%>%
  group_by(hh_id)%>%
  summarise(hh_size  = n(),
            adults   = sum(adults),
            children = sum(children))%>%
  ungroup()

data_17.2 <- data_17 %>%
  rename(hh_id = uqhh__id, sex_hhh = Sex, nationality = Nationality, edu_hhh = HighestCert)%>%
  filter(is.na(relhhh))%>%
  distinct(hh_id, .keep_all = TRUE)%>%
  select(hh_id, sex_hhh, nationality, edu_hhh)

data_17.3 <- data_17 %>%
  rename(hh_id = uqhh__id)%>%
  mutate_at(vars(starts_with("otherIncomeYn")), list(~ ifelse(is.na(.),0,.)))%>%
  mutate(inc_gov_cash = 0,
         inc_gov_monetary = otherIncomeYn__4 + otherIncomeYn__5 + otherIncomeYn__6 + otherIncomeYn__7 + otherIncomeYn__8 + otherIncomeYn__9 + otherIncomeYn__n95)%>%
  mutate(inc_gov_monetary = ifelse(inc_gov_monetary > 0,1,0))%>%
  select(hh_id, inc_gov_cash, inc_gov_monetary)%>%
  group_by(hh_id)%>%
  summarise(inc_gov_monetary = sum(inc_gov_monetary),
            inc_gov_cash     = sum(inc_gov_cash))%>%
  ungroup()

data_11.1 <- data_11 %>%
  rename(hh_id = uqhh__id, ind_hhh = isic_section)%>%
  left_join(select(rename(data_17, hh_id = uqhh__id), hh_id, relhhh))%>%
  filter(is.na(relhhh))%>%
  distinct(hh_id, .keep_all = TRUE)%>%
  select(hh_id, ind_hhh)

household_information <- data_6.1 %>%
  left_join(data_17.1)%>%
  left_join(data_17.2)%>%
  left_join(data_17.3)%>%
  left_join(data_11.1)%>%
  remove_all_labels()

write_csv(household_information, "../0_Data/1_Household Data/1_Maldives/1_Data_Clean/household_information_Maldives.csv")

# Expenditure data

# data_1.1 <- data_1 %>%
#   rename(hh_id = uqhh__id, item_code = hh_durble_1__id, expenditures_year = hh_costofgood)%>%
#   filter(hh_numyear == 0)%>%
#   filter(expenditures_year > 0 & !is.na(expenditures_year))

# data_2.1 <- data_2 %>%
#   rename(hh_id = uqhh__id, item_code = hh_durble_2__id, expenditures_year = hh_tamount)%>%
#   select(hh_id, item_code, expenditures_year)%>%
#   filter(!is.na(expenditures_year))

# data_3.1 <- data_3 %>%
#   rename(hh_id = uqhh__id, item_code = hh_furniture__id, expenditures_year = hh_t_amt_2)%>%
#   select(hh_id, item_code, expenditures_year)%>%
#   filter(!is.na(expenditures_year))

# data_4.1 <- data_4 %>%
#   rename(hh_id = uqhh__id, item_code = hh_rep_main__id, expenditures_year = hh_t_amt_1)%>%
#   select(hh_id, item_code, expenditures_year)%>%
#   filter(!is.na(expenditures_year))

# data_5.1 <- data_5 %>%
#   rename(hh_id = uqhh__id, item_code = hh_utensil__id, expenditures_year = hh_t_amt_3)%>%
#   mutate(expenditures_year = expenditures_year*4)%>%
#   filter(!is.na(expenditures_year))%>%
#   select(hh_id, item_code, expenditures_year)

data_7.1 <- data_7 %>%
  rename(hh_id = uqhh__id, item_code = coicop, expenditures_year = annexp)%>%
  select(hh_id, item_code, expenditures_year)%>%
  filter(expenditures_year > 0)%>%
  mutate(item_code = str_sub(item_code,1,-3))%>%
  group_by(hh_id, item_code)%>%
  summarise(expenditures_year = sum(expenditures_year))%>%
  ungroup()

write_csv(data_7.1, "../0_Data/1_Household Data/1_Maldives/1_Data_Clean/expenditures_items_Maldives.csv")

# data_8.1 <- data_8 %>%
#   rename(hh_id = uqhh__id, expenditures_year = t_amount_7)%>%
#   mutate(expenditures_year = expenditures_year*12)%>%
#   mutate(item_code = "A_Medicine")

# Appliance data

appliances_01 <- data_1 %>%
  rename(hh_id = uqhh__id)%>%
  select(hh_id, hh_owned, hh_durble_1__id)%>%
  mutate(appliance = ifelse(hh_durble_1__id == 5311001, "refrigerator.01",
                            ifelse(hh_durble_1__id == 5312001 | hh_durble_1__id == 5312003, "washing_machine.01",
                                   ifelse(hh_durble_1__id == 5313001, "ac.01",
                                          ifelse(hh_durble_1__id == 5329003, "fan.01",
                                                 ifelse(hh_durble_1__id == 7111001, "car.01",
                                                        ifelse(hh_durble_1__id == 7120001 | hh_durble_1__id == 7111002, "motorcycle.01",
                                                               ifelse(hh_durble_1__id == 8131001, "computer.01",
                                                                      ifelse(hh_durble_1__id == 8140004, "radio.01",
                                                                             ifelse(hh_durble_1__id == 8140006 | hh_durble_1__id == 8140011, "tv.01",
                                                                                    ifelse(hh_durble_1__id == 8200004 | hh_durble_1__id == 8120001, "mobile.01", NA)))))))))))%>%
  filter(!is.na(appliance))%>%
  mutate(hh_owned = ifelse(is.na(hh_owned),0, hh_owned))%>%
  group_by(hh_id, appliance)%>%
  summarise(value = sum(hh_owned))%>%
  ungroup()%>%
  mutate(value = ifelse(value > 0, 1, 0))%>%
  pivot_wider(names_from = appliance, values_from = value, values_fill = 0)

write_csv(appliances_01, "../0_Data/1_Household Data/1_Maldives/1_Data_Clean/appliances_0_1_Maldives.csv")

# Codes

Province.Code <- distinct(data_6, atoll_code, atoll)%>%
  rename(province = atoll_code, Province = atoll)%>%
  remove_all_labels()%>%
  write_csv(., "../0_Data/1_Household Data/1_Maldives/2_Codes/Province.Code.csv")
Toilet.Code <- stack(attr(data_6$hh_sewer_typ, 'labels'))%>%
  rename(toilet = values, TLT = ind)%>%
  bind_rows(data.frame(toilet = c(5,6), TLT = c("Toilet", "No Toilet")))%>%
  write_csv(., "../0_Data/1_Household Data/1_Maldives/2_Codes/Toilet.Code.csv")
Water.Code <- stack(attr(data_6$hh_drkwater, 'labels'))%>%
  rename(water = values, WTR = ind)%>%
  write_csv(., "../0_Data/1_Household Data/1_Maldives/2_Codes/Water.Code.csv")
Cooking.Code <- stack(attr(data_6$hh_ckfuel_typ, 'labels'))%>%
  rename(cooking_fuel = values, Cooking_Fuel = ind)%>%
  write_csv(., "../0_Data/1_Household Data/1_Maldives/2_Codes/Cooking.Code.csv")
Gender.Code <- stack(attr(data_17$Sex, 'labels'))%>%
  rename(sex_hhh = values, Gender = ind)%>%
  write_csv(., "../0_Data/1_Household Data/1_Maldives/2_Codes/Gender.Code.csv")
Nationality.Code <- stack(attr(data_17$Nationality, 'labels'))%>%
  rename(nationality = values, Nationality = ind)%>%
  write_csv(., "../0_Data/1_Household Data/1_Maldives/2_Codes/Nationality.Code.csv")
Education.Code <- stack(attr(data_17$HighestCert, 'labels'))%>%
  rename(edu_hhh = values, Education = ind)%>%
  write_csv(., "../0_Data/1_Household Data/1_Maldives/2_Codes/Education.Code.csv")
Industry.Code <- stack(attr(data_11$isic_section, 'labels'))%>%
  rename(ind_hhh = values, Industry = ind)%>%
  write_csv(., "../0_Data/1_Household Data/1_Maldives/2_Codes/Industry.Code.csv")

Item.Code.A <- stack(attr(data_7$coicop, 'labels'))%>%
  rename(item_code = values, item_name = ind)%>%
  mutate(item_code_5 = str_sub(item_code,1,5))%>%
  mutate(Type = "A")
Item.Code.B <- stack(attr(data_10$ex_item__id, 'labels'))%>%
  rename(item_code = values, item_name = ind)%>%
  mutate(item_code_5 = str_sub(item_code, 1,5))%>%
  mutate(Type = "B")
Item.Code.D <- stack(attr(data_2$hh_durble_2__id, 'labels'))%>%
  rename(item_code = values, item_name = ind)%>%
  mutate(item_code_5 = str_sub(item_code, 1,5))%>%
  mutate(Type = "D")
Item.Code.E <- stack(attr(data_1$hh_durble_1__id, 'labels'))%>%
  rename(item_code = values, item_name = ind)%>%
  mutate(item_code_5 = str_sub(item_code, 1,5))%>%
  mutate(Type = "E")
Item.Code.F <- stack(attr(data_3$hh_furniture__id, 'labels'))%>%
  rename(item_code = values, item_name = ind)%>%
  mutate(item_code_5 = str_sub(item_code, 1,5))%>%
  mutate(Type = "F")
# Item.Code.G <- stack(attr(data_4$hh_rep_main__id, 'labels'))%>%
#   rename(item_code = values, item_name = ind)%>%
#   mutate(item_code_5 = str_sub(item_code, 1,5))%>%
#   mutate(Type = "G")
# Item.Code.H <- stack(attr(data_5$hh_utensil__id, 'labels'))%>%
#   rename(item_code = values, item_name = ind)%>%
#   mutate(item_code_5 = str_sub(item_code, 1,5))%>%
#   mutate(Type = "H")

Item.Code.All <- bind_rows(Item.Code.A)%>%
  group_by(item_code)%>%
  mutate(number = n())%>%
  ungroup()%>%
  arrange(item_code)%>%
  select(-number, - Type)

write.xlsx(Item.Code.All, "../0_Data/1_Household Data/1_Maldives/3_Matching_Tables/Item_Codes_Description_Maldives.xlsx")
