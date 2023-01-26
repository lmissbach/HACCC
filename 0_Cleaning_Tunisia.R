library("haven")
library("Hmisc")
library("openxlsx")
library("rattle")
library("scales")
library("sjlabelled")
library("tidyverse")
options(scipen=999)

# To clean household data from Tunisia

# Author: L. Missbach (missbach@mcc-berlin.net)

# Load Data ####

household_0  <- read_dta("../0_Data/1_Household Data/2_Tunisia/1_Data_Raw/Tunisia10-HH-V2.dta")
individual_0 <- read_dta("../0_Data/1_Household Data/2_Tunisia/1_Data_Raw/Tunisia10-IND-V2.dta")

# Transform data

household_1 <- household_0 %>%
  rename(hh_id = caseser, hh_weights = hweight, province = area, district = reg,
         hh_size_a = hnum, lighting_fuel = slight, cooking_fuel = scook, water = wat, toilet = toif)%>%
  mutate(urban_01           = ifelse(rururb == 0,0,1),
         electricity.access = ifelse(elect == 0,0,1))%>%
  select(hh_id, hh_weights, province, district, urban_01, lighting_fuel, cooking_fuel, electricity.access, water, toilet)

individual_1 <- individual_0 %>%
  rename(sex_hhh = psex, age_hhh = page, edu_hhh = peduc_d, ind_hhh = pind, hh_id = caseser)%>%
  filter(prel == 1)%>%
  select(hh_id, age_hhh, sex_hhh, edu_hhh, ind_hhh)

individual_2 <- individual_0 %>%
  rename(hh_id = caseser)%>%
  mutate(adults   = ifelse(page > 15 & !is.na(page),1,0),
         children = ifelse(page < 16 | is.na(page),1,0))%>%
  group_by(hh_id)%>%
  summarise(adults   = sum(adults),
            children = sum(children),
            hh_size  = n())%>%
  ungroup()

household_information <- household_1 %>%
  left_join(individual_1)%>%
  left_join(individual_2)%>%
  mutate(test = hh_weights*hh_size)

write_csv(household_information, "../0_Data/1_Household Data/2_Tunisia/1_Data_Clean/household_information_Tunisia.csv")

# Expenditure data

# Unfortunately, expenditure data is highly aggregated

# hardly usable

# Appliance data

appliances_0 <- household_0 %>%
  rename(hh_id = caseser, car.01 = car, tv.01 = telv, computer.01 = computer, refrigerator.01 = refrg, stove.01 = cooker,
         washing_machine.01 = wash, dishwasher.01 = dshwsh, fan.01 = fan)%>%
  select(hh_id, ends_with(".01"))

write_csv(appliances_0, "../0_Data/1_Household Data/2_Tunisia/1_Data_Clean/appliances_0_1_Tunisia.csv")

# Codes
Lighting.Code <- stack(attr(household_0$slight, 'labels'))%>%
  rename(lighting_fuel = values, Lighting_Fuel = ind)%>%
  write_csv(., "../0_Data/1_Household Data/2_Tunisia/2_Codes/Lighting.Code.csv")

Cooking.Code <- stack(attr(household_0$scook, 'labels'))%>%
  rename(cooking_fuel = values, Cooking_Fuel = ind)%>%
  write_csv(., "../0_Data/1_Household Data/2_Tunisia/2_Codes/Cooking.Code.csv")

Toilet.Code <- stack(attr(household_0$toif, 'labels'))%>%
  rename(toilet = values, Toilet = ind)%>%
  write_csv(., "../0_Data/1_Household Data/2_Tunisia/2_Codes/Toilet.Code.csv")

Water.Code <- stack(attr(household_0$wat, 'labels'))%>%
  rename(water = values, Water = ind)%>%
  write_csv(., "../0_Data/1_Household Data/2_Tunisia/2_Codes/Water.Code.csv")

District.Code <- stack(attr(household_0$reg, 'labels'))%>%
  rename(district = values, District = ind)%>%
  write_csv(., "../0_Data/1_Household Data/2_Tunisia/2_Codes/District.Code.csv")

Province.Code <- stack(attr(household_0$area, 'labels'))%>%
  rename(province = values, Province = ind)%>%
  write_csv(., "../0_Data/1_Household Data/2_Tunisia/2_Codes/Province.Code.csv")

Education.Code <- stack(attr(individual_0$peduc_d, 'labels'))%>%
  rename(edu_hhh = values, Education = ind)%>%
  filter(edu_hhh %in% individual_1$edu_hhh)%>%
  write_csv(., "../0_Data/1_Household Data/2_Tunisia/2_Codes/Education.Code.csv")

Gender.Code <- stack(attr(individual_0$psex, 'labels'))%>%
  rename(sex_hhh = values, Gender = ind)%>%
  write_csv(., "../0_Data/1_Household Data/2_Tunisia/2_Codes/Gender.Code.csv")

Industry.Code <- stack(attr(individual_0$pind, 'labels'))%>%
  rename(ind_hhh = values, Industry = ind)%>%
  filter(ind_hhh %in% individual_1$ind_hhh)%>%
  write_csv(., "../0_Data/1_Household Data/2_Tunisia/2_Codes/Industry.Code.csv")
