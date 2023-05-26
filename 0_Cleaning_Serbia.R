# Author: L. Missbach (missbach@mcc-berlin.net)

if(!require("pacman")) install.packages("pacman")

p_load("countrycode", "haven", "Hmisc", "openxlsx", "rattle", "scales", "tidyverse", "sjlabelled")

options(scipen=999)

# Load Data ####

data_0 <- read_sav("../0_Data/1_Household Data/4_Serbia/1_Data_Raw/Demog_2019.sav")
data_1 <- read_sav("../0_Data/1_Household Data/4_Serbia/1_Data_Raw/Diary_2019.sav")
data_2 <- read_sav("../0_Data/1_Household Data/4_Serbia/1_Data_Raw/Hhold_2019.sav")
# data_3 <- read_sav("../0_Data/1_Household Data/4_Serbia/1_Data_Raw/Household consumption expenditure_Two-digit COICOP division_2019.sav")
data_4 <- read_sav("../0_Data/1_Household Data/4_Serbia/1_Data_Raw/Ident_2019.sav")
# data_5 <- read_sav("../0_Data/1_Household Data/4_Serbia/1_Data_Raw/Total household income_2019.sav")

# Transform data ####

# Household information

data_0.1 <- data_0 %>%
  rename(hh_id = rbdom, sex_hhh = p1_4, age_hhh = p1_6, nationality = p1_8, edu_hhh = p1_12)%>%
  filter(p1_2 == "1")%>%
  select(hh_id, sex_hhh, age_hhh, nationality, edu_hhh)

data_0.2 <- data_0 %>%
  rename(hh_id = rbdom)%>%
  mutate(p1_6 = ifelse(is.na(p1_6),18,p1_6))%>%
  mutate(adults   = ifelse(p1_6 > 15,1,0),
         children = ifelse(p1_6 < 16,1,0),
         inc_gov_monetary = p3_12 + p3_13 + p3_17,
         inc_gov_cash = p3_6 + p3_7 + p3_8 + p3_9)%>%
  group_by(hh_id)%>%
  summarise(adults           = sum(adults),
            children         = sum(children),
            inc_gov_monetary = sum(inc_gov_monetary),
            inc_gov_cash     = sum(inc_gov_cash),
            hh_size_a        = n())%>%
  ungroup()

data_4.1 <- data_4 %>%
  rename(hh_id = rbdom, hh_size = total_hhmember, hh_weights = weight, province = regionntsj2)%>%
  mutate(urban_01 = ifelse(urbanoother == "1",1,0))%>%
  select(hh_id, hh_size, hh_weights, urban_01, province)

data_2.1 <- data_2 %>%
  rename(hh_id = rbdom, heating_fuel = p2S_10)%>%
  mutate(electricity.access = p2S_7_3)%>%
  select(hh_id, heating_fuel, electricity.access)

household_information <- data_4.1 %>%
  left_join(data_0.2)%>%
  left_join(data_0.1)%>%
  left_join(data_2.1)%>%
  select(-hh_size_a)

write_csv(household_information, "../0_Data/1_Household Data/4_Serbia/1_Data_Clean/household_information_Serbia.csv")

# Appliance ownership

data_2.2 <- data_2 %>%
  rename(hh_id = rbdom)%>%
  select(hh_id, p2_13_1:p2_13_15)%>%
  mutate_at(vars(-hh_id), list(~ ifelse(. > 0,1,0)))%>%
  rename(stove.01 = p2_13_1, microwave.01 = p2_13_2, washing_machine.01 = p2_13_6, dishwasher.01 = p2_13_7,
         ac.01 = p2_13_8, tv.01 = p2_13_9, computer.01 = p2_13_11, car.01 = p2_13_15)%>%
  mutate(refrigerator.01 = ifelse(p2_13_3 > 0 | p2_13_5 > 0,1,0))%>%
  select(hh_id, ends_with(".01"))

write_csv(data_2.2, "../0_Data/1_Household Data/4_Serbia/1_Data_Clean/appliances_0_1_Serbia.csv")

# Expenditures

# E-Mail from Stat.Gov.RS:
# All data on expenditures are on monthly level
# Total household expenditures equal Division 01 and 02 from Diary_2019
# and COICOP-Division 03 to 12 from Hhold_2019

# Diary

data_1.1 <- data_1 %>%
  rename(hh_id = rbdom, item_code = group, expenditures_year = d5)%>%
  filter(d6 == 1)%>%
  select(hh_id, item_code, expenditures_year)%>%
  mutate(expenditures_year = expenditures_year*12)%>%
  mutate(item_code = str_remove_all(item_code, "\\."))%>%
  mutate(item_code = str_sub(item_code, 1,-2))

Item.Codes.A <- distinct(data_1.1, item_code)%>%
  arrange(item_code)%>%
  mutate(item_code = as.character(item_code))

COICOP.Codes <- read.xlsx("../0_Data/1_Household Data/4_Europe_EU27/3_Matching_Tables/Item_Codes_Description_EU.xlsx")

Item.Codes.A1 <- left_join(Item.Codes.A, COICOP.Codes)

data_2.3 <- data_2 %>%
  rename(hh_id = rbdom)%>%
  select(hh_id, p2PTD_1:p3TD_63)

Item.Codes.B <- map_dfc(data_2.3, attr, 'label')%>%
  pivot_longer(cols = everything(), names_to = "item_code_old", values_to = "item_name")%>%
  mutate(item_code = paste0("A", 1:n()))%>%
  select(item_code, item_name, item_code_old)%>%
  filter(item_code != "A1")

data_2.3.1 <- data_2.3 %>%
  mutate_at(vars(p2PTD_1:p2PTD_169),list(~ .*12))%>%
  pivot_longer(-hh_id, names_to = "item_code_old", values_to = "expenditures_year")%>%
  left_join(select(Item.Codes.B, -item_name))%>%
  select(hh_id, item_code, expenditures_year)

Item.Codes.Serbia <- Item.Codes.A1 %>%
  bind_rows(Item.Codes.B)%>%
  write.xlsx(., "../0_Data/1_Household Data/4_Serbia/3_Matching_Tables/Item_Codes_Description_Serbia_R.xlsx")

data_1.2 <- data_1.1 %>%
  # select only divisions 01 and 02 
  mutate(division = str_sub(item_code,1,2))%>%
  filter(division == "01" | division == "02")%>%
  select(-division)

expenditures_items <- bind_rows(data_1.2, data_2.3.1)%>%
  arrange(hh_id, item_code)%>%
  filter(expenditures_year > 0)%>%
  group_by(hh_id, item_code)%>%
  summarise(expenditures_year = sum(expenditures_year))%>%
  ungroup()

write_csv(expenditures_items, "../0_Data/1_Household Data/4_Serbia/1_Data_Clean/expenditures_items_Serbia.csv")

# Matching - GTAP

Item.Codes.Serbia.A <- Item.Codes.A1 %>%
  bind_rows(Item.Codes.B)%>%
  mutate(item_name = ifelse(item_code == "09430", "Other cultural services", item_name))%>%
  mutate(item_name = ifelse(item_code == "12510", "Life insurance", item_name))

Item.Codes.Serbia.A1 <- Item.Codes.Serbia.A %>%
  filter(is.na(item_code_old))%>%
  select(-item_code_old)

Item.Codes.Serbia.A2 <- Item.Codes.Serbia.A %>%
  filter(!is.na(item_code_old))%>%
  rename(item_code_A = item_code)

Item.Codes.Serbia.B <- full_join(Item.Codes.Serbia.A1, Item.Codes.Serbia.A2)%>%
  select(-item_code_old)

Matching_GTAP <- read.xlsx("../0_Data/1_Household Data/4_Europe_EU27/3_Matching_Tables/Item_GTAP_Concordance_EU_incl_Artificial.xlsx")%>%
  pivot_longer(X3:X178, names_to = "drop", values_to = "item_code")%>%
  filter(!is.na(item_code))%>%
  select(-drop)

Item.Codes.Serbia.C <- Item.Codes.Serbia.B %>%
  left_join(Matching_GTAP)

Item.Codes.Serbia.C1 <- Item.Codes.Serbia.C %>%
  filter(!is.na(GTAP))

Item.Codes.Serbia.C1.1 <- Item.Codes.Serbia.C1 %>%
  select(item_code, GTAP, Explanation)

Item.Codes.Serbia.C1.2 <- Item.Codes.Serbia.C1 %>%
  select(item_code_A, GTAP, Explanation)%>%
  filter(!is.na(item_code_A))%>%
  rename(item_code = item_code_A)

Item.Codes.Serbia.C2 <- bind_rows(Item.Codes.Serbia.C1.1,
                                  Item.Codes.Serbia.C1.2)%>%
  group_by(GTAP, Explanation)%>%
  mutate(number = 1:n())%>%
  ungroup()%>%
  pivot_wider(names_from = number, values_from = item_code)

write.xlsx(Item.Codes.Serbia.C2, "../0_Data/1_Household Data/4_Serbia/3_Matching_Tables/Item_GTAP_Concordance_Serbia_0.xlsx")

Item.Codes.Serbia.C3 <- Item.Codes.Serbia.C %>%
  filter(is.na(GTAP))%>%
  arrange(item_code_A)%>%
  select(-GTAP, -Explanation)

write.xlsx(Item.Codes.Serbia.C3, "../0_Data/1_Household Data/4_Serbia/3_Matching_Tables/Item_GTAP_Conconrdance_Serbia_to_be_matched.xlsx")

# Matching Categories

Matching_Categories <- read.xlsx("../0_Data/1_Household Data/4_Europe_EU27/3_Matching_Tables/Item_Categories_Concordance_Europe.xlsx", colNames = FALSE)%>%
  pivot_longer(X2:X187, names_to = "drop", values_to = "item_code")%>%
  filter(!is.na(item_code))%>%
  select(-drop)

Item.Codes.Serbia.D <- Item.Codes.Serbia.B %>%
  left_join(Matching_Categories)

Item.Codes.Serbia.D1 <- Item.Codes.Serbia.D %>%
  filter(!is.na(X1))

Item.Codes.Serbia.D1.1 <- Item.Codes.Serbia.D1 %>%
  select(item_code, X1)

Item.Codes.Serbia.D1.2 <- Item.Codes.Serbia.D1 %>%
  select(item_code_A, X1)%>%
  filter(!is.na(item_code_A))%>%
  rename(item_code = item_code_A)

Item.Codes.Serbia.D2 <- bind_rows(Item.Codes.Serbia.D1.1,
                                  Item.Codes.Serbia.D1.2)%>%
  group_by(X1)%>%
  mutate(number = 1:n())%>%
  ungroup()%>%
  pivot_wider(names_from = number, values_from = item_code)

write.xlsx(Item.Codes.Serbia.D2, "../0_Data/1_Household Data/4_Serbia/3_Matching_Tables/Item_Categories_Concordance_Serbia_0.xlsx")

Item.Codes.Serbia.D3 <- Item.Codes.Serbia.D %>%
  filter(is.na(X1))%>%
  arrange(item_code_A)%>%
  select(-X1)

write.xlsx(Item.Codes.Serbia.D3, "../0_Data/1_Household Data/4_Serbia/3_Matching_Tables/Item_Categories_Concordance_Serbia_to_be_matched.xlsx")

# Matching - Fuels



# Codes 

Province.Code <- stack(attr(data_4$regionntsj2, 'labels'))%>%
  rename(province = values, Province = ind)%>%
  write_csv(., "../0_Data/1_Household Data/4_Serbia/2_Codes/Province.Code.csv")
Gender.Code <- stack(attr(data_0$p1_4, 'labels'))%>%
  rename(sex_hhh = values, Gender = ind)%>%
  write_csv(., "../0_Data/1_Household Data/4_Serbia/2_Codes/Gender.Code.csv")
Education.Code <- stack(attr(data_0$p1_12, 'labels'))%>%
  rename(edu_hhh = values, Education = ind)%>%
  mutate(ISCED = c(0,1,1,2,2,3,3,4,6,6,7,7,8))%>%
  write_csv(., "../0_Data/1_Household Data/4_Serbia/2_Codes/Education.Code.csv")
Nationality.Code <- stack(attr(data_0$p1_8, 'labels'))%>%
  rename(nationality = values, Nationality = ind)%>%
  write_csv(., "../0_Data/1_Household Data/4_Serbia/2_Codes/Nationality.Code.csv")
Heating.Code <- stack(attr(data_2$p2S_10, 'labels'))%>%
  rename(heating_fuel = values, Heating_Fuel = ind)%>%
  write_csv(., "../0_Data/1_Household Data/4_Serbia/2_Codes/Heating.Code.csv")
