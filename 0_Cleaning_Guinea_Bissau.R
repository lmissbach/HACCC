library("haven")
library("Hmisc")
library("openxlsx")
library("rattle")
library("scales")
library("sjlabelled")
library("tidyverse")
options(scipen=999)

# To clean household data from Guinea-Bissau
# Based on R-Script for Benin (as of 06.09.2022)
# Author: L. Missbach (missbach@mcc-berlin.net)

# Load Data ####

data_0.0   <- read_dta("../0_Data/1_Household Data/2_Guinea-Bissau/1_Data_Raw/ehcvm_conso_gnb2018.dta") # Consumption

data_0.3   <- read_dta("../0_Data/1_Household Data/2_Guinea-Bissau/1_Data_Raw/ehcvm_ponderations_gnb2018.dta") # HH-Weights

data_0.5   <- read_dta("../0_Data/1_Household Data/2_Guinea-Bissau/1_Data_Raw/s00_me_gnb2018.dta") # Control Information
# data_0.6 <- read_dta("../0_Data/1_Household Data/2_Guinea-Bissau/1_Data_Raw/s00a_co_gnb2018.dta")
# data_0.7 <- read_dta("../0_Data/1_Household Data/2_Guinea-Bissau/1_Data_Raw/s00b_co_gnb2018.dta")

data_1.1   <- read_dta("../0_Data/1_Household Data/2_Guinea-Bissau/1_Data_Raw/s01_me_gnb2018.dta") # Household Socio-Demographic Characteristics
# data_1.2 <- read_dta("../0_Data/1_Household Data/2_Guinea-Bissau/1_Data_Raw/s01_co_gnb2018.dta")
data_2.1   <- read_dta("../0_Data/1_Household Data/2_Guinea-Bissau/1_Data_Raw/s02_me_gnb2018.dta") # Education
# data_2.2 <- read_dta("../0_Data/1_Household Data/2_Guinea-Bissau/1_Data_Raw/s02_co_gnb2018.dta")
data_3.1   <- read_dta("../0_Data/1_Household Data/2_Guinea-Bissau/1_Data_Raw/s03_me_gnb2018.dta") # General Health
# data_3.2 <- read_dta("../0_Data/1_Household Data/2_Guinea-Bissau/1_Data_Raw/s03_co_gnb2018.dta")
data_4.1   <- read_dta("../0_Data/1_Household Data/2_Guinea-Bissau/1_Data_Raw/s04_me_gnb2018.dta") # Employment
# data_4.2 <- read_dta("../0_Data/1_Household Data/2_Guinea-Bissau/1_Data_Raw/s04_co_gnb2018.dta")
data_5.1   <- read_dta("../0_Data/1_Household Data/2_Guinea-Bissau/1_Data_Raw/s05_me_gnb2018.dta") # Nonjob Revenues
# data_5.2 <- read_dta("../0_Data/1_Household Data/2_Guinea-Bissau/1_Data_Raw/s05_co_gnb2018.dta")

data_7.1   <- read_dta("../0_Data/1_Household Data/2_Guinea-Bissau/1_Data_Raw/s07a1_me_gnb2018.dta") # Food outside the household
data_7.2   <- read_dta("../0_Data/1_Household Data/2_Guinea-Bissau/1_Data_Raw/s07a2_me_gnb2018.dta") # Food outside the household
data_7.3   <- read_dta("../0_Data/1_Household Data/2_Guinea-Bissau/1_Data_Raw/s07b_me_gnb2018.dta") # Food within the household

# data_8.1 <- read_dta("../0_Data/1_Household Data/2_Guinea-Bissau/1_Data_Raw/s08a_me_gnb2018.dta") # Food security
# data_8.2 <- read_dta("../0_Data/1_Household Data/2_Guinea-Bissau/1_Data_Raw/s08b1_me_gnb2018.dta")# Food security
# data_8.3 <- read_dta("../0_Data/1_Household Data/2_Guinea-Bissau/1_Data_Raw/s08b2_me_gnb2018.dta")# Food security

data_9.1   <- read_dta("../0_Data/1_Household Data/2_Guinea-Bissau/1_Data_Raw/s09a_me_gnb2018.dta") # Nonfood 12 months
data_9.2   <- read_dta("../0_Data/1_Household Data/2_Guinea-Bissau/1_Data_Raw/s09b_me_gnb2018.dta") # Nonfood 7 days
data_9.3   <- read_dta("../0_Data/1_Household Data/2_Guinea-Bissau/1_Data_Raw/s09c_me_gnb2018.dta") # Nonfood 30 days
data_9.4   <- read_dta("../0_Data/1_Household Data/2_Guinea-Bissau/1_Data_Raw/s09d_me_gnb2018.dta") # Nonfood 3 months
data_9.5   <- read_dta("../0_Data/1_Household Data/2_Guinea-Bissau/1_Data_Raw/s09e_me_gnb2018.dta") # Nonfood 6 months
data_9.6   <- read_dta("../0_Data/1_Household Data/2_Guinea-Bissau/1_Data_Raw/s09f_me_gnb2018.dta") # Nonfood 12 months

data_11.1  <- read_dta("../0_Data/1_Household Data/2_Guinea-Bissau/1_Data_Raw/s11_me_gnb2018.dta") # Housing
data_12.1  <- read_dta("../0_Data/1_Household Data/2_Guinea-Bissau/1_Data_Raw/s12_me_gnb2018.dta") # Assets

data_13.1  <- read_dta("../0_Data/1_Household Data/2_Guinea-Bissau/1_Data_Raw/s13a_1_me_gnb2018.dta") # Transfers received (remittances)
data_13.2  <- read_dta("../0_Data/1_Household Data/2_Guinea-Bissau/1_Data_Raw/s13a_2_me_gnb2018.dta") # Transfers sent (remittances)
data_15.1  <- read_dta("../0_Data/1_Household Data/2_Guinea-Bissau/1_Data_Raw/s15_me_gnb2018.dta") # Safety nets

# Transform Data ####

# Household Data

data_0.3.1 <- data_0.3 %>%
  rename(hh_weights = hhweight)

data_0.5.1 <- data_0.5 %>%
  unite(hh_id, c("grappe", "menage"), remove = FALSE, sep = "00")%>%
  rename(province = s00q01, district = s00q02)%>%
  mutate(urban_01 = ifelse(s00q04 == 1,1,0))%>%
  select(hh_id, grappe, province, district, urban_01)

data_1.1.1 <- data_1.1 %>%
  unite(hh_id, c("grappe", "menage"), remove = FALSE, sep = "00")%>%
  filter(s01q02 == 1)%>%
  rename(sex_hhh = s01q01, religion = s01q14, nationality = s01q15, ethnicity = s01q16, id_code = s01q00a)%>%
  mutate(age_hhh = ifelse(!is.na(s01q04a), s01q04a, 2019-s01q03c))%>%
  select(hh_id, id_code, age_hhh, sex_hhh, ethnicity, nationality, religion)%>%
  mutate(ethnicity = ifelse(is.na(ethnicity),10,ethnicity))

data_1.1.2 <- data_1.1 %>%
  unite(hh_id, c("grappe", "menage"), remove = FALSE, sep = "00")%>%
  mutate(age = ifelse(!is.na(s01q04a), s01q04a, 2019-s01q03c))%>%
  mutate(adults   = ifelse(age > 15,1,0),
         children = ifelse(age < 16,1,0))%>%
  group_by(hh_id)%>%
  summarise(adults   = sum(adults),
            children = sum(children),
            hh_size  = n())%>%
  ungroup()

data_1.1.3 <- data_1.1.1 %>%
  select(hh_id, id_code)%>%
  mutate(head = 1)

data_2.1.1 <- data_2.1 %>%
  unite(hh_id, c("grappe", "menage"), remove = FALSE, sep = "00")%>%
  rename(id_code = s01q00a)%>%
  left_join(data_1.1.3)%>%
  filter(head == 1)%>%
  select(hh_id, s02q03, s02q29)%>%
  mutate(edu_hhh = ifelse(!is.na(s02q29), s02q29, 0))%>%
  select(hh_id, edu_hhh)

data_4.1.1 <- data_4.1 %>%
  unite(hh_id, c("grappe", "menage"), remove = FALSE, sep = "00")%>%
  rename(id_code = s01q00a)%>%
  left_join(data_1.1.3)%>%
  filter(head == 1)%>%
  select(hh_id, s04q30c)%>%
  rename(ind_hhh = s04q30c)

data_11.1.1 <- data_11.1 %>%
  unite(hh_id, c("grappe", "menage"), remove = FALSE, sep = "00")%>%
  select(hh_id, s11q27a, s11q27b, s11q34, s11q38, starts_with("s11q53"), s11q55)%>%
  rename(lighting_fuel = s11q38, toilet = s11q55)%>%
  mutate(electricity.access = ifelse(s11q34 == 4,0,1))%>%
  unite(water, c(s11q27a, s11q27b), remove = FALSE, sep = "W")%>%
  # We choose the primary cooking fuel only
  mutate(Cooking_Fuel = ifelse(s11q53__1 == 1, "Firewood",
                                 ifelse(s11q53__2 == 1, "Firewood",
                                        ifelse(s11q53__3 == 1, "Charcoal",
                                               ifelse(s11q53__4 == 1, "Gas",
                                                      ifelse(s11q53__5 == 1, "Electricity",
                                                             ifelse(s11q53__6 == 1, "Petrol",
                                                                    ifelse(s11q53__7 == 1, "Aninmal Waste",
                                                                           ifelse(s11q53__8 == 1, "Other", NA)))))))))%>%
  select(hh_id, lighting_fuel, Cooking_Fuel, toilet, water, electricity.access)

Cooking.Code <- distinct(data_11.1.1, Cooking_Fuel)%>%
  mutate(cooking_fuel = 1:n())%>%
  write_csv(., "../0_Data/1_Household Data/2_Guinea-Bissau/2_Codes/Cooking.Code.csv")

data_11.1.1 <- data_11.1.1 %>%
  left_join(Cooking.Code)%>%
  select(-Cooking_Fuel)

# Information on Transfers

data_5.1.1 <- data_5.1 %>%
  unite(hh_id, c("grappe", "menage"), remove = FALSE, sep = "00")%>%
  mutate_at(vars(s05q02, s05q04, s05q06), list(~ ifelse(is.na(.),0,.)))%>%
  mutate(inc_gov_monetary = s05q02 + s05q04 + s05q06)%>%
  select(hh_id, inc_gov_monetary)%>%
  group_by(hh_id)%>%
  summarise(inc_gov_monetary = sum(inc_gov_monetary))%>%
  ungroup()

# Section 13 only adresses cash transfers from family or friends

data_15.1.1 <- data_15.1 %>%
  unite(hh_id, c("grappe", "menage"), remove = FALSE, sep = "00")%>%
  filter(s15q01 == 6 | s15q01 == 7)%>%
  mutate(inc_gov_cash = ifelse(s15q02 == 1,1,0))%>%
  group_by(hh_id)%>%
  summarise(inc_gov_cash = sum(inc_gov_cash))%>%
  ungroup()

# Important: Transfers could also comprise in-kind transfers. We do not track them here. This is only cash transfers.

household_information <- data_0.5.1 %>%
  left_join(data_0.3.1)%>%
  left_join(select(data_1.1.1, -id_code))%>%
  left_join(data_1.1.2)%>%
  select(-grappe)%>%
  left_join(data_2.1.1)%>%
  left_join(data_4.1.1)%>%
  left_join(data_11.1.1)%>%
  left_join(data_5.1.1)%>%
  left_join(data_15.1.1)%>%
  remove_all_labels()

write_csv(household_information, "../0_Data/1_Household Data/2_Guinea-Bissau/1_Data_Clean/household_information_Guinea-Bissau.csv")

# Expenditures 

data_2.1.2 <- data_2.1 %>%
  unite(hh_id, c("grappe", "menage"), remove = FALSE, sep = "00")%>%
  select(hh_id, s02q20:s02q27)%>%
  pivot_longer(-hh_id, names_to = "item_code", values_to = "expenditures_year")%>%
  filter(!is.na(expenditures_year))%>%
  group_by(hh_id, item_code)%>%
  summarise(expenditures_year = sum(expenditures_year))%>%
  ungroup()

data_3.1.1 <- data_3.1 %>%
  unite(hh_id, c("grappe", "menage"), remove = FALSE, sep = "00")%>%
  select(hh_id, s03q13:s03q18, s03q24, s03q27, s03q29, s03q31)%>%
  pivot_longer(-hh_id, names_to = "item_code", values_to = "expenditures_year")%>%
  filter(!is.na(expenditures_year))%>%
  group_by(hh_id, item_code)%>%
  summarise(expenditures_year = sum(expenditures_year))%>%
  ungroup()%>%
  mutate(expenditures_year = ifelse(item_code %in% c("s03q13", "s03q14", "s03q15", "s03q16", "s03q17", "s03q18"), expenditures_year*4, expenditures_year))

data_11.1.2 <- data_11.1 %>%
  unite(hh_id, c("grappe", "menage"), remove = FALSE, sep = "00")%>%
  select(hh_id, s11q05, s11q24a, s11q24b, s11q26, s11q37a, s11q37b, s11q45a, s11q45b, s11q48a, s11q48b,
         s11q52a, s11q52b)%>%
  mutate(s11q05 = s11q05*12,
         s11q26 = s11q26,
         s11q24 = ifelse(s11q24b == 1, s11q24a*52,
                         ifelse(s11q24b == 2, s11q24a*12,
                                ifelse(s11q24b == 3, s11q24a*6,
                                       ifelse(s11q24b == 4, s11q24a*4, s11q24a)))),
         s11q37 = ifelse(s11q37b == 1, s11q37a*52,
                         ifelse(s11q37b == 2, s11q37a*12,
                                ifelse(s11q37b == 3, s11q37a*6,
                                       ifelse(s11q37b == 4, s11q37a*4, s11q37a)))),
         s11q45 = ifelse(s11q45b == 1, s11q45a*52,
                         ifelse(s11q45b == 2, s11q45a*12,
                                ifelse(s11q45b == 3, s11q45a*6,
                                       ifelse(s11q45b == 4, s11q45a*4, s11q45a)))),
         s11q48 = ifelse(s11q48b == 1, s11q48a*52,
                         ifelse(s11q48b == 2, s11q48a*12,
                                ifelse(s11q48b == 3, s11q48a*6,
                                       ifelse(s11q48b == 4, s11q48a*4, s11q48a)))),
         s11q52 = ifelse(s11q52b == 1, s11q52a*52,
                         ifelse(s11q52b == 2, s11q52a*12,
                                ifelse(s11q52b == 3, s11q52a*6,
                                       ifelse(s11q52b == 4, s11q52a*4, s11q52a)))))%>%
  select(-ends_with("a"), -ends_with("b"))%>%
  pivot_longer(-hh_id, names_to = "item_code", values_to = "expenditures_year")%>%
  filter(!is.na(expenditures_year))

# data_7.1.1 <- data_7.1 %>%
#   unite(hh_id, c("grappe", "menage"), remove = FALSE, sep = "00")%>%
#   select(hh_id,
#          s07aq02b,
#          s07aq05b,
#          s07aq08b,
#          s07aq11b,
#          s07aq14b,
#          s07aq17b,
#          s07aq20b)

# data_7.2.1 <- data_7.2 %>%
#   unite(hh_id, c("grappe", "menage"), remove = FALSE, sep = "00")%>%
#   select(hh_id,
#          s07aq02,
#          s07aq05,
#          s07aq08,
#          s07aq11,
#          s07aq14,
#          s07aq17,
#          s07aq20)%>%
#   pivot_longer(-hh_id, names_to = "item_code", values_to = "expenditures_year")%>%
#   filter(!is.na(expenditures_year))%>%
#   group_by(hh_id, item_code)%>%
#   summarise(expenditures_year = sum(expenditures_year))%>%
#   ungroup()%>%
#   mutate(expenditures_year = expenditures_year*52)

# Take this expenditure data from the aggregated dataframe

# data_7.3.1 <- data_7.3 %>%
#   unite(hh_id, c("grappe", "menage"), remove = FALSE, sep = "00")

data_9.1.1 <- data_9.1 %>%
  unite(hh_id, c("grappe", "menage"), remove = FALSE, sep = "00")%>%
  select(hh_id, s09aq03:s09aq07)%>%
  pivot_longer(-hh_id, names_to = "item_code", values_to = "expenditures_year")%>%
  filter(!is.na(expenditures_year))%>%
  group_by(hh_id, item_code)%>%
  summarise(expenditures_year = sum(expenditures_year))%>%
  ungroup()%>%
  mutate(type = "A")

data_9.2.1 <- data_9.2 %>%
  unite(hh_id, c("grappe", "menage"), remove = FALSE, sep = "00")%>%
  select(hh_id, s09bq01, s09bq03)%>%
  mutate(expenditures_year = s09bq03*52)%>%
  rename(item_code = s09bq01)%>%
  select(hh_id, item_code, expenditures_year)%>%
  mutate(type = "B")

data_9.3.1 <- data_9.3 %>%
  unite(hh_id, c("grappe", "menage"), remove = FALSE, sep = "00")%>%
  select(hh_id, s09cq01, s09cq03)%>%
  mutate(expenditures_year = s09cq03*12)%>%
  rename(item_code = s09cq01)%>%
  select(hh_id, item_code, expenditures_year)%>%
  mutate(type = "C")

data_9.4.1 <- data_9.4 %>%
  unite(hh_id, c("grappe", "menage"), remove = FALSE, sep = "00")%>%
  select(hh_id, s09dq01, s09dq03)%>%
  mutate(expenditures_year = s09dq03*4)%>%
  rename(item_code = s09dq01)%>%
  select(hh_id, item_code, expenditures_year)%>%
  mutate(type = "D")

data_9.5.1 <- data_9.5 %>%
  unite(hh_id, c("grappe", "menage"), remove = FALSE, sep = "00")%>%
  select(hh_id, s09eq01, s09eq03)%>%
  mutate(expenditures_year = s09eq03*2)%>%
  rename(item_code = s09eq01)%>%
  select(hh_id, item_code, expenditures_year)%>%
  mutate(type = "E")

data_9.6.1 <- data_9.6 %>%
  unite(hh_id, c("grappe", "menage"), remove = FALSE, sep = "00")%>%
  select(hh_id, s09fq01, s09fq03)%>%
  filter(!is.na(s09fq03))%>%
  mutate(expenditures_year = s09fq03)%>%
  rename(item_code = s09fq01)%>%
  select(hh_id, item_code, expenditures_year)%>%
  mutate(type = "F")

expenditures_df <- bind_rows(data_9.2.1, data_9.3.1, data_9.4.1, data_9.5.1, data_9.6.1)%>%
  mutate(item_code = as.character(item_code))%>%
  bind_rows(data_9.1.1)%>%
  arrange(hh_id, item_code)%>%
  filter(!is.na(expenditures_year))

# Clearly dwell on Other_exp-dataframe

Other_exp <- data_0.0 %>%
  unite(hh_id, c("grappe", "menage"), remove = FALSE, sep = "00")%>%
  select(hh_id, codpr, modep, depan)%>%
  filter(modep == 1)%>%
  rename(item_code = codpr, expenditures_year = depan)%>%
  filter(!item_code %in% expenditures_df$item_code)%>%
  mutate(item_code = as.character(item_code))%>%
  select(-modep)

Other_exp_sp <- data_0.0 %>%
  unite(hh_id, c("grappe", "menage"), remove = FALSE, sep = "00")%>%
  select(hh_id, codpr, modep, depan)%>%
  filter(modep == 2 | modep == 3)%>%
  rename(item_code = codpr, expenditures_sp_year = depan)%>%
  select(-modep)%>%
  mutate(item_code = as.character(item_code))

Other_exp_join <- full_join(Other_exp, Other_exp_sp)%>%
  mutate(expenditures_year    = ifelse(is.na(expenditures_year),   0, expenditures_year),
         expenditures_sp_year = ifelse(is.na(expenditures_sp_year),0, expenditures_sp_year))%>%
  arrange(hh_id, item_code)

expenditures_df_2 <- bind_rows(Other_exp_join, expenditures_df)%>%
  arrange(hh_id)%>%
  remove_all_labels()%>%
  mutate(expenditures_sp_year = ifelse(is.na(expenditures_sp_year),0, expenditures_sp_year))%>%
  select(-type)%>%
  filter(expenditures_year > 0 | expenditures_sp_year > 0)%>%
  group_by(hh_id, item_code)%>%
  summarise(expenditures_year = sum(expenditures_year),
            expenditures_sp_year = sum(expenditures_sp_year))%>%
  ungroup()

# Please check and correct if deemed necessary

write_csv(expenditures_df_2, "../0_Data/1_Household Data/2_Guinea-Bissau/1_Data_Clean/expenditures_items_Guinea-Bissau.csv")

# Appliances 

data_12.1.1 <- data_12.1 %>%
  unite(hh_id, c("grappe", "menage"), remove = FALSE, sep = "00")%>%
  select(hh_id, s12q01, s12q02)%>%
  mutate(appliance = ifelse(s12q01 == 7 | s12q01 == 8, "iron.01",
                            ifelse(s12q01 == 9 | s12q01 == 11 | s12q01 == 13, "stove.g.01",
                                   ifelse(s12q01 == 12, "microwave.01",
                                          ifelse(s12q01 == 16 | s12q01 == 17, "refrigerator.01",
                                                 ifelse(s12q01 == 18, "fan.01",
                                                        ifelse(s12q01 == 25, "ac.01",
                                                               ifelse(s12q01 == 23, "washing_machine.01",
                                                                      ifelse(s12q01 == 20, "tv.01",
                                                                             ifelse(s12q01 == 24, "vacuum.01",
                                                                                    ifelse(s12q01 == 28, "car.01",
                                                                                           ifelse(s12q01 == 29, "motorcycle.01",
                                                                                                  ifelse(s12q01 == 27, "generator.01",
                                                                                                         ifelse(s12q01 == 35, "mobile.01",
                                                                                                                ifelse(s12q01 == 37, "computer.01", NA)))))))))))))))%>%
  mutate(value = ifelse(s12q02 == 1,1,0))%>%
  filter(!is.na(appliance))%>%
  select(hh_id, appliance, value)%>%
  group_by(hh_id, appliance)%>%
  summarise(value = max(value))%>%
  ungroup()%>%
  pivot_wider(names_from = "appliance", values_from = "value")

write_csv(data_12.1.1, "../0_Data/1_Household Data/2_Guinea-Bissau/1_Data_Clean/appliances_0_1_Guinea-Bissau.csv")

# Codes ####
Item.Code.All <- stack(attr(data_0.0$codpr, 'labels'))
Item.Code.B   <- stack(attr(data_9.2$s09bq01, 'labels'))
Item.Code.C   <- stack(attr(data_9.3$s09cq01, 'labels'))
Item.Code.D   <- stack(attr(data_9.4$s09dq01, 'labels'))
Item.Code.E   <- stack(attr(data_9.5$s09eq01, 'labels'))
Item.Code.F   <- stack(attr(data_9.6$s09fq01, 'labels'))

Item.Joint.1 <- bind_rows(Item.Code.B, Item.Code.C, Item.Code.D, Item.Code.E, Item.Code.F, Item.Code.All)%>%
  rename(item_code = values, item_name = ind)%>%
  distinct(item_code, .keep_all = TRUE)%>%
  arrange(item_code)%>%
  mutate(item_code = as.character(item_code))%>%
  bind_rows(distinct(expenditures_df_2, item_code))%>%
  distinct(item_code, .keep_all = TRUE)

# write.xlsx(Item.Joint.1, "../0_Data/1_Household Data/2_Guinea-Bissau/3_Matching_Tables/Item_Code_Description_Guinea-Bissau.xlsx")

Education.Code <- stack(attr(data_2.1$s02q29, 'labels'))%>%
  rename(edu_hhh = values, Education = ind)%>%
  mutate(edu_hhh = as.character(edu_hhh))%>%
  bind_rows(c(edu_hhh = 0, Education = "No formal schooling"))%>%
  write_csv(., "../0_Data/1_Household Data/2_Guinea-Bissau/2_Codes/Education.Code.csv")
Province.Code <- stack(attr(data_0.5$s00q01, 'labels'))%>%
  rename(province = values, Province = ind)%>%
  write_csv(., "../0_Data/1_Household Data/2_Guinea-Bissau/2_Codes/Province.Code.csv")
District.Code <- stack(attr(data_0.5$s00q02, 'labels'))%>%
  rename(district = values, District = ind)%>%
  write_csv(., "../0_Data/1_Household Data/2_Guinea-Bissau/2_Codes/District.Code.csv")
Gender.Code <- stack(attr(data_1.1$s01q01, 'labels'))%>%
  rename(sex_hhh = values, Gender = ind)%>%
  write_csv(., "../0_Data/1_Household Data/2_Guinea-Bissau/2_Codes/Gender.Code.csv")
Ethnicity.Code <- stack(attr(data_1.1$s01q16, 'labels'))%>%
  rename(ethnicity = values, Ethnicity = ind)%>%
  write_csv(., "../0_Data/1_Household Data/2_Guinea-Bissau/2_Codes/Ethnicity.Code.csv")
Nationality.Code <- stack(attr(data_1.1$s01q15, 'labels'))%>%
  rename(nationality = values, Nationality = ind)%>%
  write_csv(., "../0_Data/1_Household Data/2_Guinea-Bissau/2_Codes/Nationality.Code.csv")
Religion.Code <- stack(attr(data_1.1$s01q14, 'labels'))%>%
  rename(religion = values, Religion = ind)%>%
  write_csv(., "../0_Data/1_Household Data/2_Guinea-Bissau/2_Codes/Religion.Code.csv")
Lighting.Code <- stack(attr(data_11.1$s11q38, 'labels'))%>%
  rename(lighting_fuel = values, Lighting_Fuel = ind)%>%
  write_csv(., "../0_Data/1_Household Data/2_Guinea-Bissau/2_Codes/Lighting.Code.csv")
Toilet.Code <- stack(attr(data_11.1$s11q55, 'labels'))%>%
  rename(toilet = values, TLT = ind)%>%
  write_csv(., "../0_Data/1_Household Data/2_Guinea-Bissau/2_Codes/Toilet.Code.csv")
Water.Code.A <- stack(attr(data_11.1$s11q27a, 'labels'))%>%
  mutate(NameA = paste0(ind, " (Dry Season)"))
Water.Code.B <- stack(attr(data_11.1$s11q27b, 'labels'))%>%
  mutate(NameB = paste0(ind, " (Wet Season)"))
Water.Code <- expand_grid(WaterA = Water.Code.A$values, WaterB = Water.Code.B$values)%>%
  left_join(Water.Code.A, by = c("WaterA" = "values"))%>%
  left_join(Water.Code.B, by = c("WaterB" = "values"))%>%
  mutate(WTR = ifelse(WaterB %in% c(1,2,3,4,7,8,14,16), "Basic",
                      ifelse(WaterB %in% c(5,6,9,10,11,12,13,15), "Limited", "Unknown")))%>%
  unite(water, c(WaterA, WaterB), sep = "W")%>%
  unite(Water, c(NameA, NameB), sep = ", ", remove = TRUE)%>%
  select(water, Water, WTR)%>%
  filter(water %in% data_11.1.1$water)

write_csv(Water.Code, "../0_Data/1_Household Data/2_Guinea-Bissau/2_Codes/Water.Code.csv")

Industry.Code <- stack(attr(data_4.1$s04q30c, 'labels'))%>%
  rename(ind_hhh = values, Industry = ind)%>%
  write_csv(., "../0_Data/1_Household Data/2_Guinea-Bissau/2_Codes/Industry.Code.csv")
