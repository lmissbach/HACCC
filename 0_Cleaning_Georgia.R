if(!require("pacman")) install.packages("pacman")

p_load("countrycode", "haven", "Hmisc", "openxlsx", "rattle", "scales", "tidyverse", "vietnameseConverter", "sjlabelled")

options(scipen=999)

# Author: L. Missbach (missbach@mcc-berlin.net)

# Loading Data ####

# data_1  <- read_sav("../0_Data/1_Household Data/4_Georgia/1_Data_Raw/2019-DB-_eng/2019 DB/123.sav")

# data_3  <- read_sav("../0_Data/1_Household Data/4_Georgia/1_Data_Raw/2019-DB-_eng/2019 DB/deadmen.sav")
data_4  <- read_sav("../0_Data/1_Household Data/4_Georgia/1_Data_Raw/2019-DB-_eng/2019 DB/familysize.sav") # Household size
data_5  <- read_sav("../0_Data/1_Household Data/4_Georgia/1_Data_Raw/2019-DB-_eng/2019 DB/sysschedule.sav") # Household information
data_9  <- read_sav("../0_Data/1_Household Data/4_Georgia/1_Data_Raw/2019-DB-_eng/2019 DB/tblincomes.sav") # Household income
data_10 <- read_sav("../0_Data/1_Household Data/4_Georgia/1_Data_Raw/2019-DB-_eng/2019 DB/tblshinda01.sav") # Household information / housing

# data_11 <- read_sav("../0_Data/1_Household Data/4_Georgia/1_Data_Raw/2019-DB-_eng/2019 DB/tblshinda01_animals.sav")
data_12 <- read_sav("../0_Data/1_Household Data/4_Georgia/1_Data_Raw/2019-DB-_eng/2019 DB/tblshinda01_comfort.sav") # Housing
# data_13 <- read_sav("../0_Data/1_Household Data/4_Georgia/1_Data_Raw/2019-DB-_eng/2019 DB/tblshinda01_cultures.sav")
# data_14 <- read_sav("../0_Data/1_Household Data/4_Georgia/1_Data_Raw/2019-DB-_eng/2019 DB/tblshinda01_lands.sav")
data_15 <- read_sav("../0_Data/1_Household Data/4_Georgia/1_Data_Raw/2019-DB-_eng/2019 DB/tblshinda01_subjects.sav") # Appliances
data_16 <- read_sav("../0_Data/1_Household Data/4_Georgia/1_Data_Raw/2019-DB-_eng/2019 DB/tblshinda02.sav") #  Household member information

# Somehow linked to Consumption

data_17 <- read_sav("../0_Data/1_Household Data/4_Georgia/1_Data_Raw/2019-DB-_eng/2019 DB/tblshinda03_1.sav") # expenditure diary
data_18 <- read_sav("../0_Data/1_Household Data/4_Georgia/1_Data_Raw/2019-DB-_eng/2019 DB/tblshinda04.sav")   # expenditures (Shinda04) - monthly data
data_19 <- read_sav("../0_Data/1_Household Data/4_Georgia/1_Data_Raw/2019-DB-_eng/2019 DB/tblshinda04_1.sav") # Self consumption (I assume)

data_2  <- read_sav("../0_Data/1_Household Data/4_Georgia/1_Data_Raw/2019-DB-_eng/2019 DB/conspurch_03.sav")  # Coicop-Level
# data_6  <- read_sav("../0_Data/1_Household Data/4_Georgia/1_Data_Raw/2019-DB-_eng/2019 DB/tblavgprices.sav")  # Average prices
# data_7  <- read_sav("../0_Data/1_Household Data/4_Georgia/1_Data_Raw/2019-DB-_eng/2019 DB/tblconsumption.sav") # Consumption / quantities
data_8  <- read_sav("../0_Data/1_Household Data/4_Georgia/1_Data_Raw/2019-DB-_eng/2019 DB/tblexpenditures.sav") # Aggregate consumption

# Transform data ####

data_10.1 <- data_10 %>%
  rename(hh_id = UID, water = WaterSource, toilet = TypeOfToilet)%>%
  select(hh_id, water, toilet)

data_12.1 <- data_12 %>%
  rename(hh_id = UID)%>%
  filter(Comfort == 4 | Comfort == 5)%>%
  group_by(hh_id)%>%
  mutate(number = n())%>%
  ungroup()%>%
  mutate(cooking_fuel = ifelse(number == 0, 0,
                               ifelse(number == 1 & Comfort == 4,1,
                                      ifelse(number == 1 & Comfort == 5,2,3))))%>%
  mutate(electricity.access = 1)%>%
  select(hh_id, cooking_fuel, electricity.access)%>%
  distinct()

data_16.1 <- data_16 %>%
  rename(hh_id = UID, sex_hhh = Gender, edu_hhh = Education, ethnicity = Nationality, age_hhh = Age)%>%
  filter(Relations == 1)%>%
  select(hh_id, sex_hhh, edu_hhh, ethnicity, age_hhh)

data_4.1 <- data_4 %>%
  rename(hh_id = UID, adults = Adult, children = Childern, hh_size = FamilySize)%>%
  select(hh_id, adults, children, hh_size)

data_5.1 <- data_5 %>%
  rename(hh_id = UID, hh_weights = Weights, province = RegNo, urban_01 = UrbanOrRural)%>%
  mutate(urban_01 = ifelse(urban_01 == 2,0, 
                           ifelse(urban_01 == 1,1,NA)))%>%
  select(hh_id, hh_weights, province, urban_01)

data_9.1 <- data_9 %>%
  rename(hh_id = UID, inc_gov_monetary = PensStipDaxm)%>%
  select(hh_id, inc_gov_monetary)%>%
  mutate(inc_gov_cash = 0)

household_information <- data_5.1 %>%
  left_join(data_10.1)%>%
  left_join(data_12.1)%>%
  left_join(data_16.1)%>%
  left_join(data_4.1)%>%
  left_join(data_9.1)%>%
  mutate(hh_weights = hh_weights/hh_size)%>% # Assumption such that hh_size*hh_weights fits overall population
  filter(!is.na(edu_hhh))%>%
  mutate(electricity.access = ifelse(is.na(electricity.access),1,electricity.access),
         cooking_fuel       = ifelse(is.na(cooking_fuel),4,cooking_fuel))

write_csv(household_information, "../0_Data/1_Household Data/4_Georgia/1_Data_Clean/household_information_Georgia.csv")

# Appliances

appliances_0_1 <- data_15 %>%
  rename(hh_id = UID)%>%
  filter(Subject %in% c(1,2,3,4,12,14,15,17,22,23,26,28))%>%
  select(hh_id, Subject)%>%
  mutate(Value = 1)%>%
  pivot_wider(names_from = "Subject", values_from = "Value", values_fill = 0)%>%
  rename(refrigerator.01 = "1", washing_machine.01 = "2", radio.01 = "3", tv.01 = "4", computer.01 = "12", motorcycle.01 = "14",
         car.01 = "15", car.01b = "17", stove.01 = "22", mobile.01 = "23", ac.01 = "26", heater.01 = "28")%>%
  filter(hh_id %in% household_information$hh_id)%>%
  mutate(car.01 = ifelse(car.01 == 1 | car.01b == 1,1,0))%>%
  select(-car.01b)

appliances_0.1.1 <- distinct(household_information, hh_id)%>%
  left_join(appliances_0_1)%>%
  mutate_at(vars(-hh_id), list(~ ifelse(is.na(.),0,.)))%>%
  write_csv(., "../0_Data/1_Household Data/4_Georgia/1_Data_Clean/appliances_0_1_Georgia.csv")

# Expenditure information ####

# Food expenditures and tobacco and beverages - rather clean

data_2.1 <- data_2 %>%
  rename(hh_id = UID, item_code = ProductCode)%>%
  mutate(expenditures_year = Paid*12)%>%
  select(hh_id, item_code, expenditures_year)%>%
  mutate(item_code = as.character(item_code))

# data_17.1 <- data_17 %>%
#   rename(hh_id = UID, item_code = ProductCode)%>%
#   mutate(expenditures = PriceL + PriceT/100)%>%
#   mutate(expenditures_year = expenditures*365)%>%
#   select(hh_id, item_code, expenditures_year)%>%
#   group_by(hh_id, item_code)%>%
#   summarise(expenditures_year = sum(expenditures_year))%>%
#   ungroup()%>%
#   mutate(item_code = as.character(item_code))

data_18.1 <- data_18 %>%
  rename(hh_id = UID)%>%
  filter(TableNo < 211 & TableNo > 1)%>%
  mutate(item_code = paste0("B",TableNo, "00", ItemNo))%>%
  group_by(hh_id, item_code)%>%
  summarise(expenditures = sum(Value))%>%
  ungroup()%>%
  mutate(expenditures_year = expenditures*4)%>%
  select(-expenditures)

expenditure_information <- bind_rows(data_2.1, data_18.1)%>%
  arrange(hh_id, item_code)%>%
  filter(hh_id %in% household_information$hh_id)

write_csv(expenditure_information, "../0_Data/1_Household Data/4_Georgia/1_Data_Clean/expenditures_items_Georgia.csv")

Item.Codes <- distinct(expenditure_information, item_code)%>%
  arrange(item_code)

COICOP <- read.xlsx("../0_Data/1_Household Data/4_Georgia/9_Documentation/COICOP.xlsx")%>%
  select(ITEMS, 'SUB-CLASS', DESCRIPTION)%>%
  rename(item_code_4 = 'SUB-CLASS', item_name = DESCRIPTION)%>%
  # rename(item_code = ITEMS, item_name = DESCRIPTION)%>%
  mutate(item_code_4 = str_replace_all(item_code_4, "\\.",""))%>%
  mutate(item_code_4 = as.numeric(item_code_4))%>%
  filter(!is.na(item_code_4))%>%
  select(-ITEMS)%>%
  mutate(item_code_4 = as.character(item_code_4))
  # mutate(item_code_4 = str_sub(item_code,1,-3),
  #        item_code_4a = str_sub(item_code,-2,-1))%>%
  # mutate(item_code_4a = as.numeric(item_code_4a))%>%
  # mutate(item_code_4 = as.numeric(item_code_4))%>%
  # mutate(item_code = paste0(item_code_4, item_code_4a))%>%
  # filter(item_code_4 < 3000)
  
Item.Codes_0 <- Item.Codes %>%
  mutate(item_code_4 = str_sub(item_code,1,-2))%>%
  left_join(COICOP)%>%
  select(-item_code_4)

write.xlsx(Item.Codes_0, "../0_Data/1_Household Data/4_Georgia/3_Matching_Tables/Item_Codes_Description_Georgia_R.xlsx")

# Codes ####
Province.Code <- stack(attr(data_5.1$province, 'labels'))%>%
  rename(province = values, Province = ind)%>%
  write_csv(., "../0_Data/1_Household Data/4_Georgia/2_Codes/Province.Code.csv")
Ethnicity.Code <- stack(attr(data_16$Nationality, 'labels'))%>%
  rename(ethnicity = values, Ethnicity = ind)%>%
  write_csv(., "../0_Data/1_Household Data/4_Georgia/2_Codes/Ethnicity.Code.csv")
Education.Code <- stack(attr(data_16$Education, 'labels'))%>%
  rename(edu_hhh = values, Education = ind)%>%
  write_csv(., "../0_Data/1_Household Data/4_Georgia/2_Codes/Education.Code.csv")
Gender.Code <- stack(attr(data_16$Gender, 'labels'))%>%
  rename(sex_hhh = values, Gender = ind)%>%
  write_csv(., "../0_Data/1_Household Data/4_Georgia/2_Codes/Gender.Code.csv")
Water.Code <- stack(attr(data_10$WaterSource, 'labels'))%>%
  rename(water = values, Water = ind)%>%
  write_csv(., "../0_Data/1_Household Data/4_Georgia/2_Codes/Water.Code.csv")
Toilet.Code <- stack(attr(data_10$TypeOfToilet, 'labels'))%>%
  rename(toilet = values, Toilet = ind)%>%
  write_csv(., "../0_Data/1_Household Data/4_Georgia/2_Codes/Toilet.Code.csv")
Cooking.Code <- data.frame(cooking_fuel = c(1,2,3,4), Cooking_Fuel = c("Natural gas", "LPG", "LPG and Natural gas","Unknown"))%>%
  write_csv(., "../0_Data/1_Household Data/4_Georgia/2_Codes/Cooking.Code.csv")
