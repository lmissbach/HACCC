if(!require("pacman")) install.packages("pacman")

p_load("countrycode", "haven", "Hmisc", "openxlsx", "rattle", "scales", "tidyverse", "vietnameseConverter", "sjlabelled")

options(scipen=999)

# Author: L. Missbach (missbach@mcc-berlin.net)

# Loading Data ####

data_1a <- read_sav("../0_Data/1_Household Data/4_Albania/1_Data_Raw/lsms_2012_eng/LSMS 2012_eng/Data_LSMS 2012/Modul_1A_householdroster.sav")
data_1b <- read_sav("../0_Data/1_Household Data/4_Albania/1_Data_Raw/lsms_2012_eng/LSMS 2012_eng/Data_LSMS 2012/Modul_1B_householdroster.sav")
data_2a <- read_sav("../0_Data/1_Household Data/4_Albania/1_Data_Raw/lsms_2012_eng/LSMS 2012_eng/Data_LSMS 2012/Modul_2A_education.sav")
data_2b <- read_sav("../0_Data/1_Household Data/4_Albania/1_Data_Raw/lsms_2012_eng/LSMS 2012_eng/Data_LSMS 2012/Modul_2B_education.sav")
data_2c <- read_sav("../0_Data/1_Household Data/4_Albania/1_Data_Raw/lsms_2012_eng/LSMS 2012_eng/Data_LSMS 2012/Modul_2C_education.sav")

data_3a <- read_sav("../0_Data/1_Household Data/4_Albania/1_Data_Raw/lsms_2012_eng/LSMS 2012_eng/Data_LSMS 2012/Modul_3_Communication_Mobile_phones_1.sav")
data_3b <- read_sav("../0_Data/1_Household Data/4_Albania/1_Data_Raw/lsms_2012_eng/LSMS 2012_eng/Data_LSMS 2012/Modul_3_internet.sav")
data_4a <- read_sav("../0_Data/1_Household Data/4_Albania/1_Data_Raw/lsms_2012_eng/LSMS 2012_eng/Data_LSMS 2012/Modul_4A_labor.sav")
data_4b <- read_sav("../0_Data/1_Household Data/4_Albania/1_Data_Raw/lsms_2012_eng/LSMS 2012_eng/Data_LSMS 2012/Modul_4B_labor.sav")
data_5a <- read_sav("../0_Data/1_Household Data/4_Albania/1_Data_Raw/lsms_2012_eng/LSMS 2012_eng/Data_LSMS 2012/Modul_5_Non-Farm_Enterprises.sav")

data_6a <- read_sav("../0_Data/1_Household Data/4_Albania/1_Data_Raw/lsms_2012_eng/LSMS 2012_eng/Data_LSMS 2012/Modul_6A_Internal_Migration_of_household_Members.sav")
data_6b <- read_sav("../0_Data/1_Household Data/4_Albania/1_Data_Raw/lsms_2012_eng/LSMS 2012_eng/Data_LSMS 2012/Modul_6B_International_Migration_of_household_Members.sav")
data_6c <- read_sav("../0_Data/1_Household Data/4_Albania/1_Data_Raw/lsms_2012_eng/LSMS 2012_eng/Data_LSMS 2012/Modul_6C_Sons_and_Doughter_living_away.sav")
data_6d <- read_sav("../0_Data/1_Household Data/4_Albania/1_Data_Raw/lsms_2012_eng/LSMS 2012_eng/Data_LSMS 2012/Modul_6D_Shocks to the household.sav")
data_7a <- read_sav("../0_Data/1_Household Data/4_Albania/1_Data_Raw/lsms_2012_eng/LSMS 2012_eng/Data_LSMS 2012/Modul_7_Subjective_Poverty.sav")

data_9a <- read_sav("../0_Data/1_Household Data/4_Albania/1_Data_Raw/lsms_2012_eng/LSMS 2012_eng/Data_LSMS 2012/Modul_9a_health.sav")
data_9b <- read_sav("../0_Data/1_Household Data/4_Albania/1_Data_Raw/lsms_2012_eng/LSMS 2012_eng/Data_LSMS 2012/Modul_9B_Access_to_Health_Care.sav")
data_10a <- read_sav("../0_Data/1_Household Data/4_Albania/1_Data_Raw/lsms_2012_eng/LSMS 2012_eng/Data_LSMS 2012/Modul_10_Fertility.sav")
data_12a <- read_sav("../0_Data/1_Household Data/4_Albania/1_Data_Raw/lsms_2012_eng/LSMS 2012_eng/Data_LSMS 2012/Modul_12A_Purchases_past30_days.sav")
data_12b <- read_sav("../0_Data/1_Household Data/4_Albania/1_Data_Raw/lsms_2012_eng/LSMS 2012_eng/Data_LSMS 2012/Modul_12b_Purchases_past6_months.sav")

data_12c <- read_sav("../0_Data/1_Household Data/4_Albania/1_Data_Raw/lsms_2012_eng/LSMS 2012_eng/Data_LSMS 2012/Modul_12C_Purchases_past12_months.sav")
data_13a <- read_sav("../0_Data/1_Household Data/4_Albania/1_Data_Raw/lsms_2012_eng/LSMS 2012_eng/Data_LSMS 2012/Modul_13A_Description_of_Dwelling.sav")
data_13b <- read_sav("../0_Data/1_Household Data/4_Albania/1_Data_Raw/lsms_2012_eng/LSMS 2012_eng/Data_LSMS 2012/Modul_13B_Utilities.sav")
data_13c1 <- read_sav("../0_Data/1_Household Data/4_Albania/1_Data_Raw/lsms_2012_eng/LSMS 2012_eng/Data_LSMS 2012/Modul_13C1_Household_Durables.sav")
# data_13c2 <- read_sav("../0_Data/1_Household Data/4_Albania/1_Data_Raw/lsms_2012_eng/LSMS 2012_eng/Data_LSMS 2012/Modul_13C2_Household_Durables.sav")

data_14 <- read_sav("../0_Data/1_Household Data/4_Albania/1_Data_Raw/lsms_2012_eng/LSMS 2012_eng/Data_LSMS 2012/Modul_14_Social_Protection.sav")
data_15 <- read_sav("../0_Data/1_Household Data/4_Albania/1_Data_Raw/lsms_2012_eng/LSMS 2012_eng/Data_LSMS 2012/Modul_15_Other_Income.sav")
data_16a <- read_sav("../0_Data/1_Household Data/4_Albania/1_Data_Raw/lsms_2012_eng/LSMS 2012_eng/Data_LSMS 2012/Modul_16A_Social_participation.sav")
data_16b <- read_sav("../0_Data/1_Household Data/4_Albania/1_Data_Raw/lsms_2012_eng/LSMS 2012_eng/Data_LSMS 2012/Modul_16B_Social_Capital.sav")
data_17a <- read_sav("../0_Data/1_Household Data/4_Albania/1_Data_Raw/lsms_2012_eng/LSMS 2012_eng/Data_LSMS 2012/Modul_17_Identification_of_Agriculture_Hh_Q1-2.sav")

data_17b <- read_sav("../0_Data/1_Household Data/4_Albania/1_Data_Raw/lsms_2012_eng/LSMS 2012_eng/Data_LSMS 2012/Modul_17_Identification_of_Agriculture_Hh_Q3-9.sav")
data_17c <- read_sav("../0_Data/1_Household Data/4_Albania/1_Data_Raw/lsms_2012_eng/LSMS 2012_eng/Data_LSMS 2012/Modul_17_Identification_of_Agriculture_Hh_Q10-11.sav")
data_18 <- read_sav("../0_Data/1_Household Data/4_Albania/1_Data_Raw/lsms_2012_eng/LSMS 2012_eng/Data_LSMS 2012/poverty.sav")
data_19 <- read_sav("../0_Data/1_Household Data/4_Albania/1_Data_Raw/lsms_2012_eng/LSMS 2012_eng/Data_LSMS 2012/Weight_lsms2012_retro.sav")

# Transforming data ####

data_19.1 <- data_19 %>%
  mutate(hh_id = psu*100+hh)%>%
  rename(hh_weights = pesha10tetor)

data_13a.1 <- data_13a %>%
  mutate(hh_id = psu*100+hh)%>%
  rename(toilet = m13a_q10)%>%
  select(hh_id, toilet)

data_13b.1 <- data_13b %>%
  mutate(hh_id = psu*100+hh)%>%
  rename(water = m13b_q01, heating_fuel = m13b_q11)%>%
  mutate(electricity.access = ifelse(m13b_m12_H == 1,0,1))%>%
  select(hh_id, heating_fuel, water, electricity.access)

data_13c1.1 <- data_13c1%>%
  mutate(hh_id = psu*100+hh)%>%
  select(hh_id, m13c1_q1b, m13c1_q1c)%>%
  arrange(hh_id, m13c1_q1b)%>%
  pivot_wider(names_from = "m13c1_q1b", values_from = "m13c1_q1c", names_prefix = "A_")%>%
  rename(refrigerator.01 = A_106, washing_machine.01 = A_108, dishwasher.01 = A_109, stove.01 = A_110, microwave.01 = A_125,
         computer.01 = A_118, motorcycle.01 = A_121, car.01 = A_122, mobile.01 = A_126)%>%
  mutate(tv.01 = ifelse(A_101 == 1 | A_102 == 1,1,0))%>%
  select(hh_id, ends_with(".01"))%>%
  write_csv(., "../0_Data/1_Household Data/4_Albania/1_Data_Clean/appliances_0_1_Albania.csv")

Toilet.Code <- stack(attr(data_13a$m13a_q10, 'labels'))%>%
  rename(toilet = values, Toilet = ind)%>%
  write_csv(., "../0_Data/1_Household Data/4_Albania/2_Codes/Toilet.Code.csv")
Water.Code <- stack(attr(data_13b$m13b_q01, 'labels'))%>%
  rename(water = values, Water = ind)%>%
  write_csv(., "../0_Data/1_Household Data/4_Albania/2_Codes/Water.Code.csv")
Heating.Code <- stack(attr(data_13b$m13b_q11, 'labels'))%>%
  rename(heating_fuel = values, Heating_Fuel = ind)%>%
  write_csv(., "../0_Data/1_Household Data/4_Albania/2_Codes/Heating.Code.csv")
