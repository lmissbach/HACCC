library("haven")
library("Hmisc")
library("openxlsx")
library("rattle")
library("scales")
library("sjlabelled")
library("tidyverse")
options(scipen=999)

# To clean the household budget survey data from Myanmar
# Author: Leonard Missbach (missbach@mcc-berlin.net)

# Load Data

data_01 <- read_dta("../0_Data/1_Household Data/1_Myanmar/1_Data_Raw/MMR_2015_MPLCS_v01_M_Stata/01_world_bank_mplcs-hh_all_data_set.dta")
data_02 <- read_dta("../0_Data/1_Household Data/1_Myanmar/1_Data_Raw/MMR_2015_MPLCS_v01_M_Stata/02_world_bank_mplcs-hh_member.dta")
data_03 <- read_dta("../0_Data/1_Household Data/1_Myanmar/1_Data_Raw/MMR_2015_MPLCS_v01_M_Stata/03_world_bank_mplcs-hh_3b_health_care.dta")
# data_04 <- read_dta("../0_Data/1_Household Data/1_Myanmar/1_Data_Raw/MMR_2015_MPLCS_v01_M_Stata/04_world_bank_mplcs-hh_q5b_remittance.dta")
data_05 <- read_dta("../0_Data/1_Household Data/1_Myanmar/1_Data_Raw/MMR_2015_MPLCS_v01_M_Stata/05_world_bank_mplcs-hh_sec-7_hh_assets.dta")

data_06 <- read_dta("../0_Data/1_Household Data/1_Myanmar/1_Data_Raw/MMR_2015_MPLCS_v01_M_Stata/06_world_bank_mplcs-hh_sec-8a_food_consumption.dta")
data_07 <- read_dta("../0_Data/1_Household Data/1_Myanmar/1_Data_Raw/MMR_2015_MPLCS_v01_M_Stata/07_world_bank_mplcs-hh_sec-8b_non_food_consumption_last_30_days.dta")
data_08 <- read_dta("../0_Data/1_Household Data/1_Myanmar/1_Data_Raw/MMR_2015_MPLCS_v01_M_Stata/08_world_bank_mplcs-hh_sec-8c_non_food_expenditure_in_6_months.dta")
data_09 <- read_dta("../0_Data/1_Household Data/1_Myanmar/1_Data_Raw/MMR_2015_MPLCS_v01_M_Stata/09_world_bank_mplcs-hh_sec-8c_non_food_expenditure_in_12_months.dta")
# data_10 <- read_dta("../0_Data/1_Household Data/1_Myanmar/1_Data_Raw/MMR_2015_MPLCS_v01_M_Stata/10_world_bank_mplcs-hh_sec-9b_non_farm_enterprise.dta")

# data_11 <- read_dta("../0_Data/1_Household Data/1_Myanmar/1_Data_Raw/MMR_2015_MPLCS_v01_M_Stata/11_world_bank_mplcs-hh_sec-10a_parcel.dta")
# data_12 <- read_dta("../0_Data/1_Household Data/1_Myanmar/1_Data_Raw/MMR_2015_MPLCS_v01_M_Stata/12_world_bank_mplcs-hh_sec-10b_inputs.dta")
# data_13 <- read_dta("../0_Data/1_Household Data/1_Myanmar/1_Data_Raw/MMR_2015_MPLCS_v01_M_Stata/13_world_bank_mplcs-hh_sec-10c_labour.dta")
# data_14 <- read_dta("../0_Data/1_Household Data/1_Myanmar/1_Data_Raw/MMR_2015_MPLCS_v01_M_Stata/14_world_bank_mplcs-hh_sec-10d_harvest_&_crop_disposition.dta")
# data_15 <- read_dta("../0_Data/1_Household Data/1_Myanmar/1_Data_Raw/MMR_2015_MPLCS_v01_M_Stata/15_world_bank_mplcs-hh_sec-10e_livestock.dta")

# data_16 <- read_dta("../0_Data/1_Household Data/1_Myanmar/1_Data_Raw/MMR_2015_MPLCS_v01_M_Stata/16_world_bank_mplcs-hh_sec-10f_machinery_&_equip.dta")
# data_17 <- read_dta("../0_Data/1_Household Data/1_Myanmar/1_Data_Raw/MMR_2015_MPLCS_v01_M_Stata/17_world_bank_mplcs-hh_sec-10g_boat.dta")
# data_18 <- read_dta("../0_Data/1_Household Data/1_Myanmar/1_Data_Raw/MMR_2015_MPLCS_v01_M_Stata/18_world_bank_mplcs-hh_sec-10g_assets.dta")
# data_19 <- read_dta("../0_Data/1_Household Data/1_Myanmar/1_Data_Raw/MMR_2015_MPLCS_v01_M_Stata/19_world_bank_mplcs-hh_sec-11a_loans.dta")
data_20 <- read_dta("../0_Data/1_Household Data/1_Myanmar/1_Data_Raw/MMR_2015_MPLCS_v01_M_Stata/MPLCS_povdata.dta")

# Transform Data

# Household

data_01.1 <- data_01 %>%
  rename(hh_id = questionnaire_no, water = q6_17_1, toilet = q6_21, electricity = q6_23, lighting_fuel = q6_26, cooking_fuel = q6_28)%>%
  mutate(electricity.access = ifelse(electricity %in% c(1,2,3,4,5,6),1,0),
         cooking_fuel = ifelse(is.na(cooking_fuel),9,cooking_fuel))%>%
  select(hh_id, water, toilet, electricity.access, lighting_fuel, cooking_fuel)

data_02.1 <- data_02 %>%
  rename(hh_id = questionnaire_no, province = id1, district = id2, village = id3, hh_weights = hh_wt)%>%
  mutate(urban_01 = ifelse(id5_poststrat == 1,1,0))%>%
  mutate(q1_4_flap = ifelse(is.na(q1_4_flap), 16, 0))%>%
  mutate(adults   = ifelse(q1_4_flap > 15,1,0),
         children = ifelse(q1_4_flap < 16,1,0))%>%
  group_by(hh_id)%>%
  mutate(adults = sum(adults),
         children = sum(children),
         hh_size = n())%>%
  ungroup()%>%
  select(hh_id, province, district, village, urban_01, adults, children, hh_size, hh_weights)%>%
  distinct()

data_02.2 <- data_02 %>%
  rename(hh_id = questionnaire_no, age_hhh = q1_4_flap, sex_hhh = q1_3, religion = q1_9, language = q1_10, edu_hhh = q2_7, ind_hhh = q4_12)%>%
  filter(q1_2 == 1)%>%
  select(hh_id, age_hhh, sex_hhh, religion, language, edu_hhh, ind_hhh)%>%
  mutate(edu_hhh = ifelse(is.na(edu_hhh),20,edu_hhh))

household_information <- data_02.1 %>%
  left_join(data_02.2)%>%
  left_join(data_01.1)

write_csv(household_information, "../0_Data/1_Household Data/1_Myanmar/1_Data_Clean/household_information_Myanmar.csv")

# Expenditures

data_02.3 <- data_02 %>%
  rename(hh_id = questionnaire_no)%>%
  select(hh_id, q2_14, starts_with("q2_16"))%>%
  select(-q2_16_10)%>%
  pivot_longer(-hh_id, names_to = "item_code", values_to = "expenditures_year")%>%
  filter(expenditures_year > 0 & !is.na(expenditures_year))%>%
  group_by(hh_id, item_code)%>%
  summarise(expenditures_year = sum(expenditures_year))%>%
  ungroup()

data_03.1 <- data_03 %>%
  rename(hh_id = questionnaire_no)%>%
  select(hh_id, q3b_15, q3b_16, q3b_23)%>%
  pivot_longer(-hh_id, names_to = "item_code", values_to = "expenditures_year")%>%
  filter(!is.na(expenditures_year))%>%
  group_by(hh_id, item_code)%>%
  summarise(expenditures_year = sum(expenditures_year))%>%
  ungroup()

data_06.1 <- data_06 %>%
  rename(hh_id = questionnaire_no)%>%
  select(-ends_with("_ot_sp"))

# Determine purchased quantity per year in kilogram per household

data_06.1.1 <- data_06.1 %>%
  filter(q8a_5 == 1)%>%
  rename(item_code = q80, unit_supervisor = q8a_6_4)%>%
  mutate(quantity_7_days_purchased = q8a_6_3 - q8a_7b - q8a_8b)%>%
  filter(quantity_7_days_purchased >= 0)%>%
  select(hh_id, item_code, quantity_7_days_purchased, unit_supervisor)%>%
  mutate(conversion_factor_kg = ifelse(unit_supervisor == 1, 0.001, 
                                       ifelse(unit_supervisor == 2,1,
                                              ifelse(unit_supervisor == 3, 0.453592,
                                                     ifelse(unit_supervisor == 4,1,
                                                            ifelse(unit_supervisor == 7, 0.0163293,
                                                                   ifelse(unit_supervisor == 9, 1.63293, NA)))))))%>%
  mutate(quantity_year_purchased = quantity_7_days_purchased*365/7)%>%
  mutate(kg_year_purchased = ifelse(!is.na(conversion_factor_kg), quantity_year_purchased*conversion_factor_kg, quantity_year_purchased))

# determine unit value (in kilogram/liter) per item

data_06.1.2 <- data_06.1 %>%
  rename(item_code = q80)%>%
  filter(!is.na(q8a_11))%>%
  mutate(conversion_factor_kg = ifelse(q8a_10_4 == 1,0.001,
                                       ifelse(q8a_10_4 == 2, 1,
                                              ifelse(q8a_10_4 == 3, 0.453592,
                                                     ifelse(q8a_10_4 == 4, 1,
                                                            ifelse(q8a_10_4 == 7, 0.0163293,
                                                                   ifelse(q8a_10_4 == 9, 1.63293, NA)))))))%>%
  filter(!is.na(conversion_factor_kg))%>%
  mutate(units_consumed = q8a_10_3*conversion_factor_kg)%>%
  mutate(unit_value     = q8a_11/units_consumed)%>% # price per kg

# Determining outliers and replacing them
  mutate(log_unit_value = log(unit_value))%>%
  group_by(item_code, id6)%>%
  mutate(mean_log_unit_value_id6 = mean(log_unit_value),
         sd_log_unit_value_id6   = sqrt(var(log_unit_value)),
         number_id6              = n(),
         median_unit_value_id6   = quantile(unit_value, probs = 0.5))%>%
  ungroup()%>%
  group_by(item_code, id3)%>%
  mutate(mean_log_unit_value_id3 = mean(log_unit_value),
         sd_log_unit_value_id3   = sqrt(var(log_unit_value)),
         number_id3              = n(),
         median_unit_value_id3   = quantile(unit_value, probs = 0.5))%>%
  ungroup()%>%
  group_by(item_code, id2)%>%
  mutate(mean_log_unit_value_id2 = mean(log_unit_value),
         sd_log_unit_value_id2   = sqrt(var(log_unit_value)),
         number_id2              = n(),
         median_unit_value_id2   = quantile(unit_value, probs = 0.5))%>%
  ungroup()%>%
  group_by(item_code, id1)%>%
  mutate(mean_log_unit_value_id1 = mean(log_unit_value),
         sd_log_unit_value_id1   = sqrt(var(log_unit_value)),
         number_id1              = n(),
         median_unit_value_id1   = quantile(unit_value, probs = 0.5))%>%
  ungroup()%>%
  group_by(item_code)%>%
  mutate(mean_log_unit_value_id0 = mean(log_unit_value),
         sd_log_unit_value_id0   = sqrt(var(log_unit_value)),
         number_id0              = n(),
         median_unit_value_id0   = quantile(unit_value, probs = 0.5))%>%
  ungroup()

local_prices_id6 <- select(data_06.1.2, item_code, id6, median_unit_value_id6, number_id6)%>%
  distinct()%>%
  filter(number_id6 > 3)
local_prices_id3 <- select(data_06.1.2, item_code, id3, median_unit_value_id3, number_id3)%>%
  distinct()%>%
  filter(number_id3 > 3)
local_prices_id2 <- select(data_06.1.2, item_code, id2, median_unit_value_id2, number_id2)%>%
  distinct()%>%
  filter(number_id2 > 3)
local_prices_id1 <- select(data_06.1.2, item_code, id1, median_unit_value_id1, number_id1)%>%
  distinct()%>%
  filter(number_id1 > 3)
local_prices_id0 <- select(data_06.1.2, item_code, median_unit_value_id0, number_id0)%>%
  distinct()%>%
  filter(number_id0 > 3)

data_06.1.2.1 <- data_06.1.2 %>%
  mutate(z_score_ea = abs((log_unit_value-mean_log_unit_value_id6)/sd_log_unit_value_id6))%>%
  mutate(outlier = ifelse(number_id6 < 4 | z_score_ea > 2.5 | is.na(sd_log_unit_value_id6) | sd_log_unit_value_id6 == 0,1,0))%>%
  filter(outlier == 0)%>%
  select(hh_id, item_code, unit_value)

# Prices per household per item

spatial_information <- data_06 %>%
  rename(hh_id = questionnaire_no)%>%
  distinct(hh_id, id6, id3, id2, id1)

data_06.1.3 <- expand_grid(hh_id = household_information$hh_id, item_code = distinct(data_06.1.1, item_code)$item_code)%>%
  left_join(spatial_information)%>%
  left_join(data_06.1.2.1)%>%
  left_join(local_prices_id6)%>%
  left_join(local_prices_id3)%>%
  left_join(local_prices_id2)%>%
  left_join(local_prices_id1)%>%
  left_join(local_prices_id0)%>%
  select(-starts_with("number_id"))%>%
  mutate(final_unit_value = ifelse(!is.na(unit_value), unit_value,
                                   ifelse(!is.na(median_unit_value_id6), median_unit_value_id6,
                                          ifelse(!is.na(median_unit_value_id3), median_unit_value_id3,
                                                 ifelse(!is.na(median_unit_value_id2), median_unit_value_id2, 
                                                        ifelse(!is.na(median_unit_value_id1), median_unit_value_id1, 
                                                               ifelse(!is.na(median_unit_value_id0), median_unit_value_id0, NA)))))))%>%
  select(hh_id, item_code, final_unit_value)

data_06.1.4 <- data_06.1.1 %>%
  select(hh_id, item_code, kg_year_purchased)%>%
  filter(kg_year_purchased > 0)%>%
  left_join(data_06.1.3)%>%
  remove_all_labels()%>%
  mutate(expenditures_year = kg_year_purchased*final_unit_value)%>%
  filter(!is.na(expenditures_year))%>%
  select(hh_id, item_code, expenditures_year)

# Other expenditures

data_07.1 <- data_07 %>%
  rename(hh_id = questionnaire_no)%>%
  select(hh_id, q8nf_item, q8nf_purchased)%>%
  mutate(expenditures_year = q8nf_purchased*12)%>%
  filter(!is.na(expenditures_year))%>%
  rename(item_code = q8nf_item)%>%
  select(hh_id, item_code, expenditures_year)

data_08.1 <- data_08 %>%
  rename(hh_id = questionnaire_no)%>%
  select(hh_id, q84, q8c_2)%>%
  mutate(expenditures_year = q8c_2*2)%>%
  filter(!is.na(expenditures_year))%>%
  rename(item_code = q84)%>%
  select(hh_id, item_code, expenditures_year)

data_09.1 <- data_09 %>%
  rename(hh_id = questionnaire_no)%>%
  select(hh_id, q8c_2_item_code, q8c_2_2)%>%
  rename(expenditures_year = q8c_2_2, item_code = q8c_2_item_code)%>%
  filter(!is.na(expenditures_year))

exp_total <- bind_rows(data_07.1, data_08.1, data_09.1, data_06.1.4)%>%
  mutate(item_code = as.character(item_code))%>%
  bind_rows(data_02.3)%>%
  bind_rows(data_03.1)%>%
  remove_all_labels()

write_csv(exp_total, "../0_Data/1_Household Data/1_Myanmar/1_Data_Clean/expenditures_items_Myanmar.csv")

# Appliances

data_05.1 <- data_05 %>%
  rename(hh_id = questionnaire_no)%>%
  select(hh_id, q71, q7_2)%>%
  mutate(q7_2 = ifelse(is.na(q7_2),0,q7_2))%>%
  mutate(q7_2 = ifelse(q7_2 != 0,1,0))%>%
  mutate(appliance = ifelse(q71 == 7104, "stove.g.01",
                            ifelse(q71 == 71707, "stove.e.01",
                                   ifelse(q71 == 7110, "fan.01",
                                          ifelse(q71 == 7111, "refrigerator.01",
                                                 ifelse(q71 == 7112, "washing_machine.01",
                                                        ifelse(q71 == 7113, "ac.01",
                                                               ifelse(q71 == 7114 | q71 == 7115, "heater.01",
                                                                      ifelse(q71 == 7116, "radio.01",
                                                                             ifelse(q71 == 7118, "tv.01",
                                                                                    ifelse(q71 == 7122, "computer.01",
                                                                                           ifelse(q71 == 7127, "motorcycle.01",
                                                                                                  ifelse(q71 == 7128 | q71 == 7131, "car.01", NA)))))))))))))%>%
  filter(!is.na(appliance))%>%
  select(-q71)%>%
  group_by(hh_id, appliance)%>%
  summarise(q7_2 = max(q7_2))%>%
  ungroup()%>%
  pivot_wider(names_from = "appliance", values_from = "q7_2")

write_csv(data_05.1, "../0_Data/1_Household Data/1_Myanmar/1_Data_Clean/appliances_0_1_Myanmar.csv")

# Codes 

Item.Code.A <- stack(attr(data_06$q80, 'labels'))%>%
  mutate(item_name = as.character(ind))
Item.Code.B <- stack(attr(data_07$q8nf_item, 'labels'))%>%
  mutate(item_name = as.character(ind))
Item.Code.C <- stack(attr(data_08$q84, 'labels'))%>%
  mutate(item_name = as.character(ind))
Item.Code.D <- stack(attr(data_09$q8c_2_item_code, 'labels'))%>%
  mutate(item_name = as.character(ind))
Item.Code.E <- bind_rows(Item.Code.A, Item.Code.B, Item.Code.C, Item.Code.D)%>%
  select(-ind)%>%
  rename(item_code = values)%>%
  mutate(item_code = as.character(item_code))
Item.Codes <- distinct(exp_total, item_code)%>%
  left_join(Item.Code.E)%>%
  arrange(item_code)

write.xlsx(Item.Codes, "../0_Data/1_Household Data/1_Myanmar/3_Matching_Tables/Item_Codes_Description_Myanmar.xlsx")

Industry.Code <- stack(attr(data_02$q4_12, 'labels'))%>%
  rename(ind_hhh = values, Industry = ind)%>%
  write_csv(., "../0_Data/1_Household Data/1_Myanmar/2_Codes/Industry.Code.csv")
Education.Code <- stack(attr(data_02$q2_7, 'labels'))%>%
  rename(edu_hhh = values, Education = ind)%>%
  write_csv(., "../0_Data/1_Household Data/1_Myanmar/2_Codes/Education.Code.csv")
Language.Code <- stack(attr(data_02$q1_10, 'labels'))%>%
  rename(language = values, Language = ind)%>%
  write_csv(., "../0_Data/1_Household Data/1_Myanmar/2_Codes/Language.Code.csv")
Religion.Code <- stack(attr(data_02$q1_9, 'labels'))%>%
  rename(religion = values, Religion = ind)%>%
  write_csv(., "../0_Data/1_Household Data/1_Myanmar/2_Codes/Religion.Code.csv")
Gender.Code <- stack(attr(data_02$q1_3, 'labels'))%>%
  rename(sex_hhh = values, Gender = ind)%>%
  write_csv(., "../0_Data/1_Household Data/1_Myanmar/2_Codes/Gender.Code.csv")
Province.Code <- stack(attr(data_02$id1, 'labels'))%>%
  rename(province = values, Province = ind)%>%
  write_csv(., "../0_Data/1_Household Data/1_Myanmar/2_Codes/Province.Code.csv")
District.Code <- stack(attr(data_02$id2, 'labels'))%>%
  rename(district = values, District = ind)%>%
  write_csv(., "../0_Data/1_Household Data/1_Myanmar/2_Codes/District.Code.csv")
Village.Code <- stack(attr(data_02$id3, 'labels'))%>%
  rename(village = values, Village = ind)%>%
  write_csv(., "../0_Data/1_Household Data/1_Myanmar/2_Codes/Village.Code.csv")
Toilet.Code <- stack(attr(data_01.1$toilet, 'labels'))%>%
  rename(toilet = values, TLT = ind)%>%
  write_csv(., "../0_Data/1_Household Data/1_Myanmar/2_Codes/Toilet.Code.csv")
Water.Code <- stack(attr(data_01.1$water, 'labels'))%>%
  rename(water = values, WTR = ind)%>%
  write_csv(., "../0_Data/1_Household Data/1_Myanmar/2_Codes/Water.Code.csv")
Lighting.Code <- stack(attr(data_01.1$lighting_fuel, 'labels'))%>%
  rename(lighting_fuel = values, Lighting_Fuel = ind)%>%
  write_csv(., "../0_Data/1_Household Data/1_Myanmar/2_Codes/Lighting.Code.csv")
Cooking.Code <- stack(attr(data_01.1$cooking_fuel, 'labels'))%>%
  rename(cooking_fuel = values, Cooking_Fuel = ind)%>%
  write_csv(., "../0_Data/1_Household Data/1_Myanmar/2_Codes/Cooking.Code.csv")
