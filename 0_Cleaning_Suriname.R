library("haven")
library("Hmisc")
library("openxlsx")
library("rattle")
library("scales")
library("sjlabelled")
library("tidyverse")
options(scipen=999)

# To clean household data from Suriname

# Author: L. Missbach (missbach@mcc-berlin.net)

# Load Data ####

data_1  <- read_dta("../0_Data/1_Household Data/3_Suriname/1_Data_Raw/Suriname-Survey-of-Living-Conditions-2016-2017/Suriname_SLC/07_Web/MicroData_Survey/RT001_Public.dta")
data_2  <- read_dta("../0_Data/1_Household Data/3_Suriname/1_Data_Raw/Suriname-Survey-of-Living-Conditions-2016-2017/Suriname_SLC/07_Web/MicroData_Survey/RT002_Public.dta")
data_3  <- read_dta("../0_Data/1_Household Data/3_Suriname/1_Data_Raw/Suriname-Survey-of-Living-Conditions-2016-2017/Suriname_SLC/07_Web/MicroData_Survey/RT003_Public.dta")
data_4  <- read_dta("../0_Data/1_Household Data/3_Suriname/1_Data_Raw/Suriname-Survey-of-Living-Conditions-2016-2017/Suriname_SLC/07_Web/MicroData_Survey/RT121_Public.dta")
data_5  <- read_dta("../0_Data/1_Household Data/3_Suriname/1_Data_Raw/Suriname-Survey-of-Living-Conditions-2016-2017/Suriname_SLC/07_Web/MicroData_Survey/RT122_Public.dta")
data_6  <- read_dta("../0_Data/1_Household Data/3_Suriname/1_Data_Raw/Suriname-Survey-of-Living-Conditions-2016-2017/Suriname_SLC/07_Web/MicroData_Survey/RT123_Public.dta")
data_7  <- read_dta("../0_Data/1_Household Data/3_Suriname/1_Data_Raw/Suriname-Survey-of-Living-Conditions-2016-2017/Suriname_SLC/07_Web/MicroData_Survey/RT124_Public.dta")
data_8  <- read_dta("../0_Data/1_Household Data/3_Suriname/1_Data_Raw/Suriname-Survey-of-Living-Conditions-2016-2017/Suriname_SLC/07_Web/MicroData_Survey/RT125_Public.dta")
data_9  <- read_dta("../0_Data/1_Household Data/3_Suriname/1_Data_Raw/Suriname-Survey-of-Living-Conditions-2016-2017/Suriname_SLC/07_Web/MicroData_Survey/RT126_Public.dta")
data_10 <- read_dta("../0_Data/1_Household Data/3_Suriname/1_Data_Raw/Suriname-Survey-of-Living-Conditions-2016-2017/Suriname_SLC/07_Web/MicroData_Survey/RT140_Public.dta")
data_11 <- read_dta("../0_Data/1_Household Data/3_Suriname/1_Data_Raw/Suriname-Survey-of-Living-Conditions-2016-2017/Suriname_SLC/07_Web/MicroData_Survey/RT141_Public.dta")

# Transform Data ####

data_1.1 <- data_1 %>%
  rename(hh_id = hhid, hh_weights = weight, province = domain, village = resort,
         cooking_fuel = q13_13, toilet = q13_14, water = q13_15, lighting_fuel = q13_17)%>%
  mutate(urban_01 = ifelse(province == 1,1,0))%>% # can be contested
  select(hh_id, hh_weights, province, district, village, urban_01, cooking_fuel, lighting_fuel, toilet, water)%>%
  mutate(lighting_fuel = ifelse(is.na(lighting_fuel),9,lighting_fuel))

data_2.1 <- data_2 %>%
  rename(hh_id = hhid)%>%
  mutate(adults   = ifelse(q1_04 > 15 | is.na(q1_04),1,0),
         children = ifelse(q1_04 < 16 & !is.na(q1_04),1,0))%>%
  group_by(hh_id)%>%
  summarise(hh_size  = n(),
            children = sum(children),
            adults   = sum(adults))%>%
  ungroup()

data_2.2 <- data_2 %>%
  rename(hh_id = hhid, sex_hhh = q1_03, ethnicity = q1_07, religion = q1_08, nationality = q2_01, edu_hhh = q3_20, ind_hhh = q9_21,
         age_hhh = q1_04)%>%
  filter(q1_02 == 1)%>%
  select(hh_id, sex_hhh, age_hhh, ethnicity, religion, nationality, edu_hhh, ind_hhh)%>%
  mutate(age_hhh     = ifelse(is.na(age_hhh),    18, age_hhh),
         ethnicity   = ifelse(is.na(ethnicity),   9, ethnicity),
         nationality = ifelse(is.na(nationality), 7, nationality),
         religion    = ifelse(is.na(religion),   12, religion),
         sex_hhh     = ifelse(is.na(sex_hhh),     1, sex_hhh))

data_2.3 <- data_2 %>%
  rename(hh_id = hhid)%>%
  select(hh_id, starts_with("q10"))%>%
  mutate_at(vars(-hh_id), list(~ ifelse(is.na(.),0,.)))%>%
  mutate(inc_gov_monetary = q10_08 + q10_09 + q10_11,
         inc_gov_cash = q10_13 + q10_18)%>%
  select(hh_id, inc_gov_monetary, inc_gov_cash)%>%
  group_by(hh_id)%>%
  summarise(inc_gov_monetary = sum(inc_gov_monetary),
            inc_gov_cash     = sum(inc_gov_cash))%>%
  ungroup()

household_information <- data_1.1 %>%
  left_join(data_2.1)%>%
  left_join(data_2.2)%>%
  left_join(data_2.3)%>%
  remove_all_labels()

write_csv(household_information, "../0_Data/1_Household Data/3_Suriname/1_Data_Clean/household_information_Suriname.csv")

# Expenditures 

data_1.2 <- data_1 %>%
  rename(hh_id = hhid)%>%
  select(hh_id, q8_09)%>%
  mutate(item_code = "q8_09",
         expenditures_year = ifelse(is.na(q8_09),0,q8_09/2))%>%
  select(-q8_09)

data_2.4 <- data_2 %>%
  rename(hh_id = hhid)%>%
  select(hh_id, starts_with("q3_19"), q5_21, q5_23, q5_25, q5_28)%>%
  mutate_at(vars(-hh_id), list(~ ifelse(is.na(.),0,.)))%>%
  mutate_at(vars(starts_with("q5")), list(~ .*12))%>%
  pivot_longer(-hh_id, names_to = "item_code", values_to = "expenditures_year")%>%
  filter(expenditures_year != 0)

data_2.5 <- data_2 %>%
  rename(hh_id = hhid)%>%
  select(hh_id, starts_with("q11_"), memberid)%>%
  select(-q11_3913_o, -q11_3914_o, -q11_00)%>%
  mutate_at(vars(-hh_id, - memberid), list(~ ifelse(is.na(.),0,.)))%>%
  select(hh_id, memberid, everything())%>%
  pivot_longer(q11_3901b:q11_4402b, names_to = "item_code", values_to = "expenditures_year")%>%
  mutate(expenditures_year = expenditures_year*365/7)%>%
  group_by(hh_id, item_code)%>%
  summarise(expenditures_year = sum(expenditures_year))%>%
  ungroup()

data_10.1 <- data_10 %>%
  rename(hh_id = hhid)%>%
  select(hh_id, foodcode, anncoeff, q14_03c)%>%
  rename(item_code = foodcode)%>%
  mutate(expenditures_year = q14_03c*anncoeff)%>%
  filter(!is.na(expenditures_year))%>%
  remove_all_labels()%>%
  mutate(item_code = as.character(item_code))%>%
  select(hh_id, item_code, expenditures_year)

data_11.1 <- data_11 %>%
  rename(hh_id = hhid)%>%
  select(hh_id, nfcode, anncoeff, q141_02_)%>%
  filter(!is.na(q141_02_))%>%
  mutate(expenditures_year = q141_02_*anncoeff)%>%
  rename(item_code = nfcode)%>%
  select(hh_id, item_code, expenditures_year)%>%
  remove_all_labels()%>%
  mutate(item_code = as.character(item_code))%>%
  select(hh_id, item_code, expenditures_year)

expenditures_items <- bind_rows(data_1.2, data_2.4, data_2.5)%>%
  bind_rows(data_10.1)%>%
  bind_rows(data_11.1)%>%
  group_by(hh_id, item_code)%>%
  summarise(expenditures_year = sum(expenditures_year))%>%
  ungroup()%>%
  filter(expenditures_year > 0)

write_csv(expenditures_items, "../0_Data/1_Household Data/3_Suriname/1_Data_Clean/expenditures_items_Suriname.csv")

# Appliances

data_1.3 <- data_1 %>%
  rename(hh_id = hhid)%>%
  select(hh_id, starts_with("q13_23"),
         q16_01, q16_06, q16_09, q16_11, q16_15, q16_17a, q16_17b, q16_20, q16_22)%>%
  rename(stove.01 = q13_23a, tv.01a = q13_23e, mobile.01 = q13_23g, car.01 = q13_23i, tv.01b = q16_15,
         refrigerator.01 = q16_01, microwave.01 = q16_06, dishwasher.01 = q16_09, washing_machine.01 = q16_11,
         computer.01a = q16_17a, computer.01b = q16_17b, ac.01 = q16_20, fan.01 = q16_22)%>%
  mutate_at(vars(-hh_id), list(~ ifelse(is.na(.),0,.)))%>%
  mutate(tv.01       = tv.01a + tv.01b,
         computer.01 = computer.01a + computer.01b)%>%
  select(hh_id, ends_with(".01"))%>%
  mutate(dishwasher.01 = ifelse(dishwasher.01 == 1,1,0),
         ac.01         = ifelse(ac.01         == 1,1,0))%>%
  mutate_at(vars(-hh_id), list(~ ifelse(. == 0 | . > 1,0,
                                        ifelse(. == 1,1,NA))))

write_csv(data_1.3, "../0_Data/1_Household Data/3_Suriname/1_Data_Clean/appliances_0_1_Suriname.csv")  

# Codes
Item.Code.A <- stack(attr(data_10$foodcode, 'labels'))%>%
  rename(item_code = values, item_name = ind)%>%
  mutate(item_code = as.character(item_code))
Item.Code.B <- stack(attr(data_11$nfcode, 'labels'))%>%
  rename(item_code = values, item_name = ind)%>%
  mutate(item_code = as.character(item_code))
Item.Code.C <- distinct(data_2.4, item_code)%>%
  arrange(item_code)
Item.Code.D <- distinct(data_2.5, item_code)%>%
  arrange(item_code)
Item.Code.E <- distinct(data_1.2, item_code)%>%
  arrange(item_code)

Item.Code.All <- Item.Code.A %>%
  bind_rows(Item.Code.B)%>%
  bind_rows(Item.Code.C)%>%
  bind_rows(Item.Code.D)%>%
  bind_rows(Item.Code.E)

#write.xlsx(Item.Code.All, "../0_Data/1_Household Data/3_Suriname/3_Matching_Tables/Item_Code_Description_Suriname.xlsx")

Province.Code <- stack(attr(data_1$domain, 'labels'))%>%
  rename(province = values, Province = ind)%>%
  write_csv(., "../0_Data/1_Household Data/3_Suriname/2_Codes/Province.Code.csv")
District.Code <- stack(attr(data_1$district, 'labels'))%>%
  rename(district = values, District = ind)%>%
  write_csv(., "../0_Data/1_Household Data/3_Suriname/2_Codes/District.Code.csv")
Village.Code <- stack(attr(data_1$resort, 'labels'))%>%
  rename(village = values, Village = ind)%>%
  write_csv(., "../0_Data/1_Household Data/3_Suriname/2_Codes/Village.Code.csv")
Cooking.Code <- stack(attr(data_1$q13_13, 'labels'))%>%
  rename(cooking_fuel = values, Cooking_Fuel = ind)%>%
  write_csv(., "../0_Data/1_Household Data/3_Suriname/2_Codes/Cooking.Code.csv")
Lighting.Code <- stack(attr(data_1$q13_17, 'labels'))%>%
  rename(lighting_fuel = values, Lighting_Fuel = ind)%>%
  bind_rows(data.frame(lighting_fuel = 9, Lighting_Fuel = "Other"))%>%
  write_csv(., "../0_Data/1_Household Data/3_Suriname/2_Codes/Lighting.Code.csv")
Toilet.Code <- stack(attr(data_1$q13_14, 'labels'))%>%
  rename(toilet = values, Toilet = ind)%>%
  write_csv(., "../0_Data/1_Household Data/3_Suriname/2_Codes/Toilet.Code.csv")
Water.Code <- stack(attr(data_1$q13_15, 'labels'))%>%
  rename(water = values, Water = ind)%>%
  write_csv(., "../0_Data/1_Household Data/3_Suriname/2_Codes/Water.Code.csv")
Gender.Code <- stack(attr(data_2$q1_03, 'labels'))%>%
  rename(sex_hhh = values, Gender = ind)%>%
  write_csv(., "../0_Data/1_Household Data/3_Suriname/2_Codes/Gender.Code.csv")
Ethnicity.Code <- stack(attr(data_2$q1_07, 'labels'))%>%
  rename(ethnicity = values, Ethnicity = ind)%>%
  write_csv(., "../0_Data/1_Household Data/3_Suriname/2_Codes/Ethnicity.Code.csv")
Religion.Code <- stack(attr(data_2$q1_08, 'labels'))%>%
  rename(religion = values, Religion = ind)%>%
  write_csv(., "../0_Data/1_Household Data/3_Suriname/2_Codes/Religion.Code.csv")
Nationality.Code <- stack(attr(data_2$q2_01, 'labels'))%>%
  rename(nationality = values, Nationality = ind)%>%
  write_csv(., "../0_Data/1_Household Data/3_Suriname/2_Codes/Nationality.Code.csv")
Education.Code <- stack(attr(data_2$q3_20, 'labels'))%>%
  rename(edu_hhh = values, Education = ind)%>%
  write_csv(., "../0_Data/1_Household Data/3_Suriname/2_Codes/Education.Code.csv")
Industry.Code <- stack(attr(data_2$q9_21, 'labels'))%>%
  rename(ind_hhh = values, Industry = ind)%>%
  write_csv(., "../0_Data/1_Household Data/3_Suriname/2_Codes/Industry.Code.csv")