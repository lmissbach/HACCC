library("haven")
library("Hmisc")
library("openxlsx")
library("rattle")
library("scales")
library("sjlabelled")
library("tidyverse")
options(scipen=999)

# To clean household data from Cambodia

# Author: L. Missbach (missbach@mcc-berlin.net)

# Load Data ####

data_1 <- read_dta("../0_Data/1_Household Data/1_Cambodia/1_Data_Raw/KHM_2019_LSMS-PLUS_v01_M_STATA13/hh_sec_1.dta")
data_2 <- read_dta("../0_Data/1_Household Data/1_Cambodia/1_Data_Raw/KHM_2019_LSMS-PLUS_v01_M_STATA13/hh_sec_2.dta")
# data_3 <- read_dta("../0_Data/1_Household Data/1_Cambodia/1_Data_Raw/KHM_2019_LSMS-PLUS_v01_M_STATA13/hh_sec_3.dta")
data_4 <- read_dta("../0_Data/1_Household Data/1_Cambodia/1_Data_Raw/KHM_2019_LSMS-PLUS_v01_M_STATA13/hh_sec_4.dta")
data_5 <- read_dta("../0_Data/1_Household Data/1_Cambodia/1_Data_Raw/KHM_2019_LSMS-PLUS_v01_M_STATA13/hh_sec_5.dta")

data_6 <- read_dta("../0_Data/1_Household Data/1_Cambodia/1_Data_Raw/KHM_2019_LSMS-PLUS_v01_M_STATA13/hh_sec_6.dta")
data_7a <- read_dta("../0_Data/1_Household Data/1_Cambodia/1_Data_Raw/KHM_2019_LSMS-PLUS_v01_M_STATA13/hh_sec_7a.dta")
data_7b1 <- read_dta("../0_Data/1_Household Data/1_Cambodia/1_Data_Raw/KHM_2019_LSMS-PLUS_v01_M_STATA13/hh_sec_7b_hhlabor.dta")
data_7b2 <- read_dta("../0_Data/1_Household Data/1_Cambodia/1_Data_Raw/KHM_2019_LSMS-PLUS_v01_M_STATA13/hh_sec_7b_nonhhlabor.dta")
data_7c <- read_dta("../0_Data/1_Household Data/1_Cambodia/1_Data_Raw/KHM_2019_LSMS-PLUS_v01_M_STATA13/hh_sec_7c.dta")

data_7d <- read_dta("../0_Data/1_Household Data/1_Cambodia/1_Data_Raw/KHM_2019_LSMS-PLUS_v01_M_STATA13/hh_sec_7d.dta")
data_8 <- read_dta("../0_Data/1_Household Data/1_Cambodia/1_Data_Raw/KHM_2019_LSMS-PLUS_v01_M_STATA13/hh_sec_8.dta")
data_9 <- read_dta("../0_Data/1_Household Data/1_Cambodia/1_Data_Raw/KHM_2019_LSMS-PLUS_v01_M_STATA13/hh_sec_9.dta")
data_10 <- read_dta("../0_Data/1_Household Data/1_Cambodia/1_Data_Raw/KHM_2019_LSMS-PLUS_v01_M_STATA13/hh_sec_10.dta")
data_11 <- read_dta("../0_Data/1_Household Data/1_Cambodia/1_Data_Raw/KHM_2019_LSMS-PLUS_v01_M_STATA13/hh_sec_11.dta")

data_12 <- read_dta("../0_Data/1_Household Data/1_Cambodia/1_Data_Raw/KHM_2019_LSMS-PLUS_v01_M_STATA13/hh_sec_12.dta")
data_13a <- read_dta("../0_Data/1_Household Data/1_Cambodia/1_Data_Raw/KHM_2019_LSMS-PLUS_v01_M_STATA13/hh_sec_13a.dta")
data_13b <- read_dta("../0_Data/1_Household Data/1_Cambodia/1_Data_Raw/KHM_2019_LSMS-PLUS_v01_M_STATA13/hh_sec_13b.dta")
data_14 <- read_dta("../0_Data/1_Household Data/1_Cambodia/1_Data_Raw/KHM_2019_LSMS-PLUS_v01_M_STATA13/hh_sec_14.dta")
#data_15 <- read_dta("../0_Data/1_Household Data/1_Cambodia/1_Data_Raw/KHM_2019_LSMS-PLUS_v01_M_STATA13/hh_sec_15.dta")

#data_16a <- read_dta("../0_Data/1_Household Data/1_Cambodia/1_Data_Raw/KHM_2019_LSMS-PLUS_v01_M_STATA13/hh_sec_16a.dta")
#data_16b <- read_dta("../0_Data/1_Household Data/1_Cambodia/1_Data_Raw/KHM_2019_LSMS-PLUS_v01_M_STATA13/hh_sec_16b.dta")
#data_16c <- read_dta("../0_Data/1_Household Data/1_Cambodia/1_Data_Raw/KHM_2019_LSMS-PLUS_v01_M_STATA13/hh_sec_16c.dta")
#data_17a <- read_dta("../0_Data/1_Household Data/1_Cambodia/1_Data_Raw/KHM_2019_LSMS-PLUS_v01_M_STATA13/hh_sec_17a.dta")
#data_17b <- read_dta("../0_Data/1_Household Data/1_Cambodia/1_Data_Raw/KHM_2019_LSMS-PLUS_v01_M_STATA13/hh_sec_17b.dta")

#data_18a <- read_dta("../0_Data/1_Household Data/1_Cambodia/1_Data_Raw/KHM_2019_LSMS-PLUS_v01_M_STATA13/hh_sec_18a.dta")
#data_18b <- read_dta("../0_Data/1_Household Data/1_Cambodia/1_Data_Raw/KHM_2019_LSMS-PLUS_v01_M_STATA13/hh_sec_18b.dta")
#data_19a <- read_dta("../0_Data/1_Household Data/1_Cambodia/1_Data_Raw/KHM_2019_LSMS-PLUS_v01_M_STATA13/hh_sec_19a.dta")
#data_19b <- read_dta("../0_Data/1_Household Data/1_Cambodia/1_Data_Raw/KHM_2019_LSMS-PLUS_v01_M_STATA13/hh_sec_19b.dta")
#data_20a <- read_dta("../0_Data/1_Household Data/1_Cambodia/1_Data_Raw/KHM_2019_LSMS-PLUS_v01_M_STATA13/hh_sec_20a.dta")

#data_20b <- read_dta("../0_Data/1_Household Data/1_Cambodia/1_Data_Raw/KHM_2019_LSMS-PLUS_v01_M_STATA13/hh_sec_20b.dta")
# data_0 <- read_dta("../0_Data/1_Household Data/1_Cambodia/1_Data_Raw/KHM_2019_LSMS-PLUS_v01_M_STATA13/individual_cover.dta")

# Transform data ####

data_1.1 <- data_1 %>%
  rename(hh_id = HHID, hh_weights = finalhhweight, province = s01q02, district = s01q03)%>%
  mutate(urban_01 = ifelse(s01q06b == 1,1,0))%>%
  select(hh_id, hh_weights, province, district, urban_01)

hh_id_0 <- distinct(data_1, HHID)%>%
  rename(hh_id = HHID)%>%
  mutate(hh_id_new = 1:n())

data_2.1 <- data_2 %>%
  rename(hh_id = HHID, sex_hhh = s02q03, ethnicity = s02q11a)%>%
  filter(s02q06 == 1)%>%
  select(hh_id, sex_hhh, ethnicity)

head <- filter(data_2, s02q06 == 1)%>%
  rename(hh_id = HHID)%>%
  select(hh_id, PID)%>%
  mutate(head = 1)

data_2.2 <- data_2 %>%
  rename(hh_id = HHID)%>%
  mutate(adults   = ifelse(s02q05a > 15,1,0),
         children = ifelse(s02q05a < 16,1,0))%>%
  group_by(hh_id)%>%
  summarise(adults   = sum(adults),
            children = sum(children),
            hh_size  = n())%>%
  ungroup()

data_4.1 <- data_4 %>%
  rename(hh_id = HHID, lighting_fuel = s04q07, water = s04q08, toilet = s04q19a, cooking_fuel = s04q26)%>%
  select(hh_id, lighting_fuel, water, toilet, cooking_fuel)

data_11.1 <- data_11 %>%
  rename(hh_id = HHID)%>%
  left_join(head)%>%
  filter(!is.na(head))%>%
  mutate(edu_hhh = s11q06)%>%
  select(hh_id, edu_hhh)%>%
  mutate(edu_hhh = ifelse(is.na(edu_hhh), 9999, edu_hhh))

data_14.1 <- data_14 %>%
  rename(hh_id = HHID)%>%
  left_join(head)%>%
  filter(!is.na(head))%>%
  rename(ind_hhh = s14q31a_ISIC)%>%
  select(hh_id, ind_hhh)

household_information <- data_1.1 %>%
  left_join(data_2.1)%>%
  left_join(data_2.2)%>%
  left_join(data_4.1)%>%
  left_join(data_11.1)%>%
  left_join(data_14.1)%>%
  left_join(hh_id_0)%>%
  select(-hh_id)%>%
  rename(hh_id = hh_id_new)%>%
  select(hh_id, everything())

write_csv(household_information, "../0_Data/1_Household Data/1_Cambodia/1_Data_Clean/household_information_Cambodia.csv")

# Expenditures

data_4.2 <- data_4 %>%
  rename(hh_id = HHID)%>%
  select(hh_id, s04q16, s04q20, s04q21, starts_with("s04q27"), s04q29a, s04q30)%>%
  mutate(s04q29a = ifelse(is.na(s04q29a),0, s04q29a))%>%
  mutate_at(vars(-hh_id), list(~ .*12))%>%
  pivot_longer(-hh_id, names_to = "item_code", values_to = "expenditures_year")

data_11.2 <- data_11 %>%
  rename(hh_id = HHID)%>%
  select(hh_id, PID, starts_with("s11q16"))%>%
  mutate_at(vars(-hh_id, - PID), list(~ ifelse(is.na(.),0,.)))%>%
  pivot_longer("s11q16a":"s11q16h", names_to = "item_code", values_to = "expenditures_year")%>%
  group_by(hh_id, item_code)%>%
  summarise(expenditures_year = sum(expenditures_year))%>%
  ungroup()

data_12.1 <- data_12 %>%
  rename(hh_id = HHID)%>%
  select(hh_id, s12q10, s12q11, PID)%>%
  mutate_at(vars(-hh_id, - PID), list(~ ifelse(is.na(.),0,.)))%>%
  pivot_longer("s12q10":"s12q11", names_to = "item_code", values_to = "expenditures_year")%>%
  group_by(hh_id, item_code)%>%
  summarise(expenditures_year = sum(expenditures_year))%>%
  ungroup()

data_5.1 <- data_5 %>%
  rename(hh_id = HHID, item_code = food_consumption_roster_1__id, expenditures_year = s05q05, expenditures_sp_year = s05q06)%>%
  mutate(expenditures_year    = ifelse(is.na(expenditures_year),    0, expenditures_year*365/7),
         expenditures_sp_year = ifelse(is.na(expenditures_sp_year), 0, expenditures_sp_year*365/7))%>%
  select(hh_id, item_code, starts_with("expenditures"))%>%
  mutate(item_code = as.character(item_code))

data_6.1 <- data_6 %>%
  rename(hh_id = HHID, item_code = non_food_roster__id, expenditures_year = s06q04, expenditures_sp_year = s06q05)%>%
  mutate(factor = ifelse(s06q03 == "Last 1 month",12,
                         ifelse(s06q03 == "Last 12 months",1,
                                ifelse(s06q03 == "Last 3 months",4,
                                       ifelse(s06q03 == "Last 6 months",2, NA)))))%>%
  mutate(expenditures_year    = ifelse(is.na(expenditures_year), 0, expenditures_year*factor),
         expenditures_sp_year = ifelse(is.na(expenditures_sp_year), 0, expenditures_sp_year*factor))%>%
  select(hh_id, item_code, expenditures_year, expenditures_sp_year)%>%
  mutate(item_code = paste0("B",item_code))

expenditure_information <- bind_rows(data_5.1, data_6.1)%>%
  mutate(item_code = as.character(item_code))%>%
  bind_rows(data_4.2)%>%
  bind_rows(data_11.2)%>%
  bind_rows(data_12.1)%>%
  left_join(hh_id_0)%>%
  select(-hh_id)%>%
  rename(hh_id = hh_id_new)%>%
  select(hh_id, everything())%>%
  mutate(expenditures_sp_year = ifelse(is.na(expenditures_sp_year),0, expenditures_sp_year))%>%
  filter(expenditures_year != 0 | expenditures_sp_year != 0)

write_csv(expenditure_information, "../0_Data/1_Household Data/1_Cambodia/1_Data_Clean/expenditures_items_Cambodia.csv")

# Appliances 

data_10.1 <- data_10 %>%
  rename(hh_id = HHID)%>%
  select(hh_id, s10q01, durables_roster__id)%>%
  mutate(name_0 = ifelse(durables_roster__id == 1101, "mobile.01",
                         ifelse(durables_roster__id == 1102, "computer.01",
                                ifelse(durables_roster__id == 1104 | durables_roster__id == 1106, "motorcycle.01",
                                       ifelse(durables_roster__id == 1105, "car.01", NA)))))%>%
  filter(!is.na(name_0))%>%
  select(-durables_roster__id)%>%
  group_by(hh_id, name_0)%>%
  summarise(s10q01 = max(s10q01))%>%
  ungroup()%>%
  pivot_wider(names_from = name_0, values_from = s10q01)%>%
  left_join(hh_id_0)%>%
  select(-hh_id)%>%
  rename(hh_id = hh_id_new)%>%
  select(hh_id, everything())

write_csv(data_10.1, "../0_Data/1_Household Data/1_Cambodia/1_Data_Clean/appliances_0_1_Cambodia.csv")

# Codes

Province.Code <- stack(attr(data_1$s01q02, 'labels'))%>%
  rename(province = values, Province = ind)%>%
  separate(Province, c("drop", "Province"), sep = "/")%>%
  select(-drop)%>%
  write_csv(., "../0_Data/1_Household Data/1_Cambodia/2_Codes/Province.Code.csv")
District.Code <- stack(attr(data_1$s01q03, 'labels'))%>%
  rename(district = values, District = ind)%>%
  separate(District, c("drop", "District"), sep = "/")%>%
  select(-drop)%>%
  bind_rows(data.frame(district = c(210,211,2007), District = c("210", "211","2007")))%>%
  write_csv(., "../0_Data/1_Household Data/1_Cambodia/2_Codes/District.Code.csv")
Gender.Code <- stack(attr(data_2$s02q03, 'labels'))%>%
  rename(sex_hhh = values, Gender = ind)%>%
  write_csv(., "../0_Data/1_Household Data/1_Cambodia/2_Codes/Gender.Code.csv")
Ethnicity.Code <- stack(attr(data_2$s02q11a, 'labels'))%>%
  rename(ethnicity = values, Ethnicity = ind)%>%
  write_csv(., "../0_Data/1_Household Data/1_Cambodia/2_Codes/Ethnicity.Code.csv")
Lighting.Code <- stack(attr(data_4$s04q07, 'labels'))%>%
  rename(lighting_fuel = values, Lighting_Fuel = ind)%>%
  write_csv(., "../0_Data/1_Household Data/1_Cambodia/2_Codes/Lighting.Code.csv")
Cooking.Code <- stack(attr(data_4$s04q26, 'labels'))%>%
  rename(cooking_fuel = values, Cooking_Fuel = ind)%>%
  write_csv(., "../0_Data/1_Household Data/1_Cambodia/2_Codes/Cooking.Code.csv")
Water.Code <- stack(attr(data_4$s04q08, 'labels'))%>%
  rename(water = values, Water = ind)%>%
  write_csv(., "../0_Data/1_Household Data/1_Cambodia/2_Codes/Water.Code.csv")
Toilet.Code <- stack(attr(data_4$s04q19a, 'labels'))%>%
  rename(toilet = values, TLT = ind)%>%
  write_csv(., "../0_Data/1_Household Data/1_Cambodia/2_Codes/Toilet.Code.csv")
Education.Code <- stack(attr(data_11$s11q06, 'labels'))%>%
  rename(edu_hhh = values, Education = ind)%>%
  write_csv(., "../0_Data/1_Household Data/1_Cambodia/2_Codes/Education.Code.csv")
Industry.Code <- stack(attr(data_14$s14q31a_ISIC, 'labels'))%>%
  rename(ind_hhh = values, Industry = ind)%>%
  write_csv(., "../0_Data/1_Household Data/1_Cambodia/2_Codes/Industry.Code.csv")


Item.Code.A <- distinct(data_4.2, item_code)%>%
  arrange(item_code)
Item.Code.B <- distinct(data_11.2, item_code)%>%
  arrange(item_code)
Item.Code.C <- distinct(data_12.1, item_code)
Item.Code.D <- stack(attr(data_5$food_consumption_roster_1__id, 'labels'))%>%
  rename(item_code = values, item_name = ind)%>%
  mutate(item_code = as.character(item_code))
Item.Code.E <- stack(attr(data_6$non_food_roster__id, 'labels'))%>%
  rename(item_code = values, item_name = ind)%>%
  mutate(item_code = paste0("B",item_code))

Item.Code.All <- bind_rows(Item.Code.D, Item.Code.E)%>%
  bind_rows(Item.Code.A)%>%
  bind_rows(Item.Code.B)%>%
  bind_rows(Item.Code.C)

# write.xlsx(Item.Code.All, "../0_Data/1_Household Data/1_Cambodia/3_Matching_Tables/Item_Code_Description_Cambodia.xlsx")
