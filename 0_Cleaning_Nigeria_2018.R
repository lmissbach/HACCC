library("haven")
library("Hmisc")
library("openxlsx")
library("rattle")
library("scales")
library("sjlabelled")
library("tidyverse")
options(scipen=999)

# To clean the household budget survey data from Nigeria
# Author: Leonard Missbach (missbach@mcc-berlin.net)

# Load Data

# data_0.0 <- read_dta("../0_Data/1_Household Data/2_Nigeria/1_Data_Raw/2018/NGA_2018_LSS_v01_M_Stata/Household/sect_aux.dta")
# data_0.1 <- read_dta("../0_Data/1_Household Data/2_Nigeria/1_Data_Raw/2018/NGA_2018_LSS_v01_M_Stata/Household/sect_result.dta")
data_0.2  <- read_dta("../0_Data/1_Household Data/2_Nigeria/1_Data_Raw/2018/NGA_2018_LSS_v01_M_Stata/Household/secta_cover.dta")

data_1   <- read_dta("../0_Data/1_Household Data/2_Nigeria/1_Data_Raw/2018/NGA_2018_LSS_v01_M_Stata/Household/sect1_roster.dta")
data_2   <- read_dta("../0_Data/1_Household Data/2_Nigeria/1_Data_Raw/2018/NGA_2018_LSS_v01_M_Stata/Household/sect2_education.dta")
data_3   <- read_dta("../0_Data/1_Household Data/2_Nigeria/1_Data_Raw/2018/NGA_2018_LSS_v01_M_Stata/Household/sect3_health.dta")
data_4.1 <- read_dta("../0_Data/1_Household Data/2_Nigeria/1_Data_Raw/2018/NGA_2018_LSS_v01_M_Stata/Household/sect4a1_labour.dta")
data_4.2 <- read_dta("../0_Data/1_Household Data/2_Nigeria/1_Data_Raw/2018/NGA_2018_LSS_v01_M_Stata/Household/sect4a2_labour.dta")
data_5   <- read_dta("../0_Data/1_Household Data/2_Nigeria/1_Data_Raw/2018/NGA_2018_LSS_v01_M_Stata/Household/sect5_remittances.dta")

data_6.1 <- read_dta("../0_Data/1_Household Data/2_Nigeria/1_Data_Raw/2018/NGA_2018_LSS_v01_M_Stata/Household/sect6a_meals_outside.dta")
data_6.2 <- read_dta("../0_Data/1_Household Data/2_Nigeria/1_Data_Raw/2018/NGA_2018_LSS_v01_M_Stata/Household/sect6b_food_cons.dta")
# data_6.3 <- read_dta("../0_Data/1_Household Data/2_Nigeria/1_Data_Raw/2018/NGA_2018_LSS_v01_M_Stata/Household/sect6c_aggregate_food_1.dta")
# data_6.4 <- read_dta("../0_Data/1_Household Data/2_Nigeria/1_Data_Raw/2018/NGA_2018_LSS_v01_M_Stata/Household/sect6c_aggregate_food_2.dta")
# data_6.5 <- read_dta("../0_Data/1_Household Data/2_Nigeria/1_Data_Raw/2018/NGA_2018_LSS_v01_M_Stata/Household/sect6c_aggregate_food_3.dta")
data_7.1 <- read_dta("../0_Data/1_Household Data/2_Nigeria/1_Data_Raw/2018/NGA_2018_LSS_v01_M_Stata/Household/sect07_7day.dta")
data_7.2 <- read_dta("../0_Data/1_Household Data/2_Nigeria/1_Data_Raw/2018/NGA_2018_LSS_v01_M_Stata/Household/sect07_12month.dta")
data_7.3 <- read_dta("../0_Data/1_Household Data/2_Nigeria/1_Data_Raw/2018/NGA_2018_LSS_v01_M_Stata/Household/sect07_30day.dta")

data_10  <- read_dta("../0_Data/1_Household Data/2_Nigeria/1_Data_Raw/2018/NGA_2018_LSS_v01_M_Stata/Household/sect10_assets.dta")
data_13  <- read_dta("../0_Data/1_Household Data/2_Nigeria/1_Data_Raw/2018/NGA_2018_LSS_v01_M_Stata/Household/sect13_income.dta")
data_14  <- read_dta("../0_Data/1_Household Data/2_Nigeria/1_Data_Raw/2018/NGA_2018_LSS_v01_M_Stata/Household/sect14_housing.dta")
data_12.1  <- read_dta("../0_Data/1_Household Data/2_Nigeria/1_Data_Raw/2018/NGA_2018_LSS_v01_M_Stata/Household/sect12a_beta.dta")
data_12.2  <- read_dta("../0_Data/1_Household Data/2_Nigeria/1_Data_Raw/2018/NGA_2018_LSS_v01_M_Stata/Household/sect12a_safety.dta")
data_12.3  <- read_dta("../0_Data/1_Household Data/2_Nigeria/1_Data_Raw/2018/NGA_2018_LSS_v01_M_Stata/Household/sect12b_safety.dta")

data_tot <- read_dta("../0_Data/1_Household Data/2_Nigeria/1_Data_Raw/2018/NGA_2018_LSS_v01_M_Stata/Household/totcons.dta")
# Transform Data

# Household

data_1.1 <- data_1 %>%
  rename(hh_id = hhid, region = zone, province = state, district = lga, sex_hhh = s01q02,
         age_hhh = s01q04a, ethnicity = s01q11)%>%
  mutate(urban_01 = ifelse(sector == 1,1,0))%>%
  filter(s01q03 == 1)%>%
  select(hh_id, region, province, district, urban_01, sex_hhh, ethnicity, age_hhh)

data_1.2 <- data_1 %>%
  rename(hh_id = hhid)%>%
  mutate(adults   = ifelse(s01q04a > 15,1,0),
         children = ifelse(s01q04a < 16,1,0))%>%
  group_by(hh_id)%>%
  summarise(adults   = sum(adults),
            children = sum(children),
            hh_size  = n())%>%
  ungroup()
  
data_0.2.1 <- data_0.2 %>%
  rename(hh_id = hhid, hh_weights = wt_final)%>%
  select(hh_id, hh_weights)%>%
  filter(!is.na(hh_weights))

data_2.1 <- data_2 %>%
  rename(hh_id = hhid, edu_hhh = s02q08)%>%
  left_join(select(rename(data_1,hh_id = hhid), hh_id, indiv, s01q03))%>%
  filter(s01q03 == 1)%>%
  select(hh_id, edu_hhh)

data_4.1.1 <- data_4.1 %>%
  rename(hh_id = hhid)%>%
  left_join(select(rename(data_1, hh_id = hhid), hh_id, indiv, s01q03))%>%
  filter(s01q03 == 1)%>%
  rename(ind_hhh = s04aq29)%>%
  select(hh_id, ind_hhh)

data_14.1 <- data_14 %>%
  rename(hh_id = hhid)%>%
  select(hh_id, s14q16_1, s14q19, s14q27, s14q40)%>% # s14q21 = source of electricity
  rename(water = s14q27, toilet = s14q40, cooking_fuel = s14q16_1)%>%
  mutate(electricity.access = ifelse(s14q19 == 1,1,0),
         cooking_fuel = ifelse(is.na(cooking_fuel),18,cooking_fuel))%>%
  select(-s14q19)

household <- data_1.1 %>%
  left_join(data_1.2)%>%
  left_join(data_0.2.1)%>%
  left_join(data_2.1)%>%
  left_join(data_14.1)%>%
  left_join(data_4.1.1)%>%
  remove_all_labels()

# Income / Transfers
data_2.2 <- data_2 %>%
  rename(hh_id = hhid)%>%
  select(hh_id, s02q17)%>%
  mutate(inc_gov_monetary = s02q17)%>%
  filter(!is.na(inc_gov_monetary))%>%
  select(hh_id, inc_gov_monetary)

data_13.1 <- data_13 %>%
  rename(hh_id = hhid)%>%
  filter(source_cd == 103)%>%
  filter(!is.na(s13q02))%>%
  mutate(inc_gov_monetary = s13q02)%>%
  select(hh_id, inc_gov_monetary)

data_12.1.1 <- data_12.1 %>%
  rename(hh_id = hhid)%>%
  filter(s12aq02 == 1)%>%
  mutate(inc_gov_cash = s12aq18)%>%
  filter(!is.na(inc_gov_cash))%>%
  select(hh_id, inc_gov_cash)

data_12.3.1 <- data_12.3 %>%
  rename(hh_id = hhid)%>%
  filter(snet_cd < 10)%>%
  select(hh_id, s12q04a)%>%
  mutate(inc_gov_monetary = s12q04a)%>%
  filter(!is.na(inc_gov_monetary))%>%
  select(hh_id, inc_gov_monetary)

data_income <- bind_rows(data_2.2, data_13.1)%>%
  bind_rows(data_12.1.1)%>%
  bind_rows(data_12.3.1)%>%
  mutate(inc_gov_cash = ifelse(is.na(inc_gov_cash),0, inc_gov_cash),
         inc_gov_monetary = ifelse(is.na(inc_gov_monetary),0, inc_gov_monetary))%>%
  group_by(hh_id)%>%
  summarise(inc_gov_cash = sum(inc_gov_cash),
            inc_gov_monetary = sum(inc_gov_monetary))%>%
  ungroup()

household_0 <- left_join(household, data_income)%>%
  mutate(inc_gov_cash     = ifelse(is.na(inc_gov_cash),    0, inc_gov_cash),
         inc_gov_monetary = ifelse(is.na(inc_gov_monetary),0, inc_gov_monetary))

write_csv(household_0, "../0_Data/1_Household Data/2_Nigeria/1_Data_Clean/household_information_Nigeria.csv")

# Expenditures

data_2.3 <- data_2 %>%
  rename(hh_id = hhid)%>%
  select(hh_id, 
         s02q18a:s02q18t,
         s02q19a:s02q19t)%>%
  pivot_longer(-hh_id, names_to = "item_code", values_to = "expenditures_year")%>%
  filter(!is.na(expenditures_year))%>%
  group_by(hh_id, item_code)%>%
  summarise(expenditures_year = sum(expenditures_year))%>%
  ungroup()

data_3.1 <- data_3 %>%
  rename(hh_id = hhid)%>%
  select(hh_id, s03q14, s03q15, s03q18, s03q21, s03q30)%>%
  pivot_longer(-hh_id, names_to = "item_code", values_to = "expenditures_year")%>%
  filter(!is.na(expenditures_year))%>%
  group_by(hh_id, item_code)%>%
  summarise(expenditures_year = sum(expenditures_year))%>%
  ungroup()%>%
  mutate(expenditures_year = ifelse(item_code == "s03q14" | item_code == "s03q15" | item_code == "s03q18", expenditures_year*12, expenditures_year))

data_14.2 <- data_14 %>%
  rename(hh_id = hhid)%>%
  select(hh_id, s14q05a, s14q05b, s14q39, s14q47a, s14q47b)%>%
  mutate(s14q05 = ifelse(s14q05b == 1, s14q05a*12, s14q05a),
         s14q39 = s14q39*12,
         s14q47 = ifelse(s14q47b == 1, s14q47a*365,
                         ifelse(s14q47b == 2, s14q47a*52,
                                ifelse(s14q47b == 3, s14q47a*26,
                                       ifelse(s14q47b == 4, s14q47a*12,
                                              ifelse(s14q47b == 5, s14q47a*4,
                                                     ifelse(s14q47b == 6, s14q47a, s14q47a)))))))%>%
  select(-s14q05b, - s14q05a, - s14q47a, -s14q47b)%>%
  pivot_longer(-hh_id, names_to = "item_code", values_to = "expenditures_year")%>%
  filter(!is.na(expenditures_year))

# Energy is also included in non-food expenditure section

# data_14.3 <- data_14 %>%
#   rename(hh_id = hhid)%>%
#   select(hh_id, s14q16_1, s14q17_1)%>%
#   mutate(expenditures_year = s14q17_1*12)%>%
#   mutate(item_code = 1000 + s14q16_1)%>%
#   select(hh_id, item_code, expenditures_year)%>%
#   filter(!is.na(expenditures_year) & expenditures_year >0)
# 
# data_14.4 <- data_14 %>%
#   rename(hh_id = hhid)%>%
#   select(hh_id, s14q16_2, s14q17_2)%>%
#   mutate(expenditures_year = s14q17_2*12)%>%
#   mutate(item_code = 1000 + s14q16_2)%>%
#   select(hh_id, item_code, expenditures_year)%>%
#   filter(!is.na(expenditures_year) & expenditures_year >0)

data_7.1.1 <- data_7.1 %>%
  rename(hh_id = hhid, item_code = item_cd)%>%
  select(hh_id, item_code, s07q02)%>%
  filter(!is.na(s07q02))%>%
  mutate(expenditures_year = s07q02*52)%>%
  select(hh_id, item_code, expenditures_year)%>%
  mutate(item_code = item_code*10)

data_7.2.1 <- data_7.2 %>%
  rename(hh_id = hhid, item_code = item_cd, expenditures_year = s07q06)%>%
  filter(!is.na(expenditures_year))%>%
  select(hh_id, item_code, expenditures_year)

data_7.3.1 <- data_7.3 %>%
  rename(hh_id = hhid, item_code = item_cd, expenditures_year = s07q04)%>%
  filter(!is.na(expenditures_year))%>%
  mutate(expenditures_year = expenditures_year*12)%>%
  select(hh_id, item_code, expenditures_year)

data_6.1.1 <- data_6.1 %>%
  rename(hh_id = hhid, item_code = item_cd, expenditures_year = s06aq02)%>%
  filter(!is.na(expenditures_year))%>%
  mutate(expenditures_year = expenditures_year*52)%>%
  select(hh_id, item_code, expenditures_year)

data_6.2.1 <- data_6.2 %>%
  rename(hh_id = hhid, item_code = item_cd)%>%
  filter(s06bq01 == 1)%>%
  select(-s06bq01)%>%
  mutate(share_purchased = s06bq03/s06bq02a)%>%
  filter(share_purchased > 0)%>%
  mutate(purchase_quantity_kg_l = s06bq07a*s06bq07_cvn,
         purchase_price_Naira_per_kg_l = s06bq08/purchase_quantity_kg_l)%>%
  select(hh_id, item_code, purchase_price_Naira_per_kg_l, everything())%>%
  mutate(quantity_purchased_kg_l = share_purchased*s06bq02a*s06bq02_cvn)%>%
  mutate(log_unit_value = log(purchase_price_Naira_per_kg_l),
         unit_value     = purchase_price_Naira_per_kg_l)%>%
  group_by(ea, item_code)%>%
  mutate(mean_log_unit_value_ea   = mean(log_unit_value, na.rm = TRUE),
         sd_log_unit_value_ea     = sqrt(var(log_unit_value, na.rm = TRUE)),
         median_unit_value_ea = quantile(unit_value, probs = 0.5, na.rm = TRUE))%>%
  ungroup()%>%
  group_by(lga, item_code)%>%
  mutate(mean_log_unit_value_lga   = mean(log_unit_value, na.rm = TRUE),
         sd_log_unit_value_lga     = sqrt(var(log_unit_value, na.rm = TRUE)),
         median_unit_value_lga = quantile(unit_value, probs = 0.5, na.rm = TRUE))%>%
  ungroup()%>%
  group_by(state, item_code)%>%
  mutate(mean_log_unit_value_state   = mean(log_unit_value, na.rm = TRUE),
         sd_log_unit_value_state     = sqrt(var(log_unit_value, na.rm = TRUE)),
         median_unit_value_state = quantile(unit_value, probs = 0.5, na.rm = TRUE))%>%
  ungroup()%>%
  group_by(zone, item_code)%>%
  mutate(mean_log_unit_value_zone   = mean(log_unit_value, na.rm = TRUE),
         sd_log_unit_value_zone     = sqrt(var(log_unit_value, na.rm = TRUE)),
         median_unit_value_zone = quantile(unit_value, probs = 0.5, na.rm = TRUE))%>%
  ungroup()%>%
  group_by(item_code)%>%
  mutate(mean_log_unit_value_country   = mean(log_unit_value, na.rm = TRUE),
         sd_log_unit_value_country     = sqrt(var(log_unit_value, na.rm = TRUE)),
         median_unit_value_country = quantile(unit_value, probs = 0.5, na.rm = TRUE))%>%
  ungroup()%>%
  mutate(z_score_ea      = abs((log_unit_value-mean_log_unit_value_ea/sd_log_unit_value_ea)))%>%
  mutate(unit_value_clean = ifelse(z_score_ea < 2.5, purchase_price_Naira_per_kg_l, NA))%>%
  mutate(unit_value_clean = ifelse(is.na(unit_value_clean) & !is.na(median_unit_value_ea),      median_unit_value_ea,      unit_value_clean))%>%
  mutate(unit_value_clean = ifelse(is.na(unit_value_clean) & !is.na(median_unit_value_lga),     median_unit_value_lga,     unit_value_clean))%>%
  mutate(unit_value_clean = ifelse(is.na(unit_value_clean) & !is.na(median_unit_value_state),   median_unit_value_state,   unit_value_clean))%>%
  mutate(unit_value_clean = ifelse(is.na(unit_value_clean) & !is.na(median_unit_value_zone),    median_unit_value_zone,    unit_value_clean))%>%
  mutate(unit_value_clean = ifelse(is.na(unit_value_clean) & !is.na(median_unit_value_country), median_unit_value_country, unit_value_clean))%>%
  mutate(expenditures_year = unit_value_clean*quantity_purchased_kg_l*52)%>%
  filter(!is.na(expenditures_year))%>%
  select(hh_id, item_code, expenditures_year)

# Check with data_tot

exp_total <- bind_rows(data_2.3, data_3.1)%>%
  bind_rows(data_14.2)%>%
  arrange(item_code)%>%
  bind_rows(mutate(arrange(data_7.1.1, item_code), item_code = as.character(item_code)))%>%
  bind_rows(mutate(arrange(data_7.2.1, item_code), item_code = as.character(item_code)))%>%
  bind_rows(mutate(arrange(data_7.3.1, item_code), item_code = as.character(item_code)))%>%
  bind_rows(mutate(arrange(data_6.1.1, item_code), item_code = as.character(item_code)))%>%
  bind_rows(mutate(arrange(data_6.2.1, item_code), item_code = as.character(item_code)))

write_csv(exp_total, "../0_Data/1_Household Data/2_Nigeria/1_Data_Clean/expenditures_items_Nigeria.csv")

# Appliances

data_10.1 <- data_10 %>%
  rename(hh_id = hhid)%>%
  select(hh_id, asset_cd, s10q01)%>%
  mutate(value = ifelse(s10q01 == 1,1,0))%>%
  mutate(value = ifelse(is.na(value),0,value))%>%
  mutate(appliance = ifelse(asset_cd == 308 | asset_cd == 310, "stove.g.01",
                            ifelse(asset_cd == 309, "stove.e.01",
                                   ifelse(asset_cd == 311, "stove.k.01",
                                          ifelse(asset_cd == 312 | asset_cd == 313, "refrigerator.01",
                                                 ifelse(asset_cd == 314, "ac.01",
                                                        ifelse(asset_cd == 315, "washing_machine.01",
                                                               ifelse(asset_cd == 316, "dryer.01",
                                                                      ifelse(asset_cd == 318, "motorcycle.01",
                                                                             ifelse(asset_cd == 319, "car.01",
                                                                                    ifelse(asset_cd == 322, "radio.01",
                                                                                           ifelse(asset_cd == 321, "fan.01",
                                                                                                  ifelse(asset_cd == 325, "microwave.01",
                                                                                                         ifelse(asset_cd == 327, "tv.01",
                                                                                                                ifelse(asset_cd == 328, "computer.01", NA)))))))))))))))%>%
  filter(!is.na(appliance))%>%
  group_by(hh_id, appliance)%>%
  summarise(value = max(value))%>%
  ungroup()%>%
  pivot_wider(names_from = "appliance", values_from = "value")%>%
  remove_all_labels()

write_csv(data_10.1, "../0_Data/1_Household Data/2_Nigeria/1_Data_Clean/appliances_0_1_Nigeria.csv")  
  
# Codes

Item.Code.B <- stack(attr(data_7.3$item_cd, 'labels'))%>%
  rename(item_code = values, item_name = ind)
Item.Code.C <- stack(attr(data_7.2$item_cd, 'labels'))%>%
  rename(item_code = values, item_name = ind)
Item.Code.D <- stack(attr(data_7.1$item_cd, 'labels'))%>%
  rename(item_code = values, item_name = ind)%>%
  mutate(item_code = item_code*10)
Item.Code.E <- stack(attr(data_6.1$item_cd, 'labels'))%>%
  rename(item_code = values, item_name = ind)
Item.Code.F <- stack(attr(data_6.2$item_cd, 'labels'))%>%
  rename(item_code = values, item_name = ind)%>%
  mutate(item_name = as.character(item_name))
Item.Codes <- distinct(exp_total, item_code)%>%
  left_join(mutate(Item.Code.B, item_code = as.character(item_code), item_name = as.character(item_name)), by = "item_code")%>%
  left_join(mutate(Item.Code.C, item_code = as.character(item_code), item_name = as.character(item_name)), by = "item_code")%>%
  left_join(mutate(Item.Code.D, item_code = as.character(item_code), item_name = as.character(item_name)), by = "item_code")%>%
  left_join(mutate(Item.Code.E, item_code = as.character(item_code), item_name = as.character(item_name)), by = "item_code")%>%
  left_join(mutate(Item.Code.F, item_code = as.character(item_code), item_name = as.character(item_name)), by = "item_code")%>%
  mutate(item_name_new = ifelse(!is.na(item_name.x), item_name.x,
                               ifelse(!is.na(item_name.y), item_name.y,
                                      ifelse(!is.na(item_name.x.x), item_name.x.x,
                                             ifelse(!is.na(item_name.y.y), item_name.y.y, 
                                                    ifelse(!is.na(item_name), item_name, NA))))))%>%
  select(item_code, item_name_new)

write.xlsx(Item.Codes, "../0_Data/1_Household Data/2_Nigeria/3_Matching_Tables/Item_Code_Description_Nigeria.xlsx")

Item.Codes.0 <- stack(attr(Item.Codes$item_name, 'labels'))

Region.Code <- stack(attr(data_1$zone, 'labels'))%>%
  rename(region = values, Region = ind)%>%
  write_csv("../0_Data/1_Household Data/2_Nigeria/2_Codes/Region.Code.csv")
Province.Code <- stack(attr(data_1$state, 'labels'))%>%
  rename(province = values, Province = ind)%>%
  write_csv("../0_Data/1_Household Data/2_Nigeria/2_Codes/Province.Code.csv")
District.Code <- stack(attr(data_1$lga, 'labels'))%>%
  rename(district = values, District = ind)%>%
  write_csv("../0_Data/1_Household Data/2_Nigeria/2_Codes/District.Code.csv")
Gender.Code <- stack(attr(data_1$s01q02, 'labels'))%>%
  rename(sex_hhh = values, Gender = ind)%>%
  write_csv("../0_Data/1_Household Data/2_Nigeria/2_Codes/Gender.Code.csv")
Ethnicity.Code <- stack(attr(data_1$s01q11, 'labels'))%>%
  rename(ethnicity = values, Ethnicity = ind)%>%
  write_csv("../0_Data/1_Household Data/2_Nigeria/2_Codes/Ethnicity.Code.csv")
Education.Code <- stack(attr(data_2$s02q08, 'labels'))%>%
  rename(edu_hhh = values, Education = ind)%>%
  write_csv("../0_Data/1_Household Data/2_Nigeria/2_Codes/Education.Code.csv")
Water.Code <- stack(attr(data_14$s14q27, 'labels'))%>%
  rename(water = values, WTR = ind)%>%
  write_csv("../0_Data/1_Household Data/2_Nigeria/2_Codes/Water.Code.csv")
Toilet.Code <- stack(attr(data_14$s14q40, 'labels'))%>%
  rename(toilet = values, TLT = ind)%>%
  write_csv("../0_Data/1_Household Data/2_Nigeria/2_Codes/Toilet.Code.csv")
Cooking.Code <- stack(attr(data_14$s14q16_1, 'labels'))%>%
  rename(cooking_fuel = values, Cooking_Fuel = ind)%>%
  write_csv("../0_Data/1_Household Data/2_Nigeria/2_Codes/Cooking.Code.csv")
Industry.Code <- stack(attr(data_4.1$s04aq29, 'labels'))%>%
  rename(ind_hhh = values, Industry = ind)%>%
  write_csv("../0_Data/1_Household Data/2_Nigeria/2_Codes/Industry.Code.csv")
