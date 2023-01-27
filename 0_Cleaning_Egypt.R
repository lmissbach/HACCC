library("haven")
library("Hmisc")
library("openxlsx")
library("rattle")
library("scales")
library("sjlabelled")
library("tidyverse")
options(scipen=999)

# To clean household data from Egypt

# Author: L. Missbach (missbach@mcc-berlin.net)

# Load Data ####

household_0  <- read_dta("../0_Data/1_Household Data/2_Egypt/1_Data_Raw/Egypt17-HH-V2.dta")
individual_0 <- read_dta("../0_Data/1_Household Data/2_Egypt/1_Data_Raw/Egypt17-IND-V2.dta")

# Transform data

household_1 <- household_0 %>%
  rename(hh_id = caseser, hh_weights = hweight, province = area, district = reg,
         hh_size_a = hnum, lighting_fuel = slight, cooking_fuel = scook, water = wat, toilet = toif, inc_gov_monetary = transf)%>%
  mutate(urban_01           = ifelse(rururb == 0,0,1),
         electricity.access = ifelse(elect == 0,0,1),
         inc_gov_cash       = 0,
         inc_gov_monetary   = ifelse(is.na(inc_gov_monetary),0,inc_gov_monetary))%>%
  select(hh_id, hh_weights, province, district, urban_01, lighting_fuel, cooking_fuel, electricity.access, water, toilet, 
         inc_gov_cash, inc_gov_monetary)

individual_1 <- individual_0 %>%
  rename(sex_hhh = psex, age_hhh = page, edu_hhh = peduc_d, ind_hhh = pind, hh_id = caseser)%>%
  filter(prel == 1)%>%
  select(hh_id, age_hhh, sex_hhh, edu_hhh, ind_hhh)%>%
  mutate(age_hhh = ifelse(is.na(age_hhh),18,age_hhh))

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
  # Documentation is inaccessible and inconclusive
  # Scaling up such that it fits aggregate population in 2017
  mutate(hh_weights = hh_weights*1956.15)

write_csv(household_information, "../0_Data/1_Household Data/2_Egypt/1_Data_Clean/household_information_Egypt.csv")

# Expenditure data

expenditure_information <- household_0 %>%
  rename(hh_id = caseser)%>%
  select(hh_id, foodexp:totexp)%>%
  pivot_longer(-hh_id, names_to = "item_name_0", values_to = "expenditures_year")%>%
  filter(!is.na(expenditures_year))

Labels <- as.data.frame(lapply(household_0, attr, 'label'))%>%
  pivot_longer(everything(), names_to = "item_name_0", values_to = "item_name")%>%
  filter(item_name_0 %in% expenditure_information$item_name_0)%>%
  mutate(item_code = 1:n())%>%
  select(item_code, item_name, item_name_0)

write.xlsx(Labels, "../0_Data/1_Household Data/2_Egypt/3_Matching_Tables/Item_Codes_Description_Egypt.xlsx")

expenditure_information_2 <- expenditure_information %>%
  left_join(select(Labels, item_name_0, item_code))%>%
  select(hh_id, item_code, expenditures_year)
  
# Banerjee et al. (2017) highlight that households spend approximately 50% of residential energy on electricity across income groups

# Cooking and heating energy --> p_c
expenditure_information_2.1 <- expenditure_information_2 %>%
  filter(item_code == 26)%>%
  mutate(expenditures_year_2 = expenditures_year/2)%>%
  select(hh_id, item_code, expenditures_year_2)%>%
  rename(expenditures_year = expenditures_year_2)

# Electricity --> ely
expenditure_information_2.2 <- expenditure_information_2.1 %>%
  mutate(item_code = 261)

expenditure_information_2.3 <- expenditure_information_2 %>%
  filter(item_code != 26)%>%
  bind_rows(expenditure_information_2.1)%>%
  bind_rows(expenditure_information_2.2)%>%
  arrange(hh_id, item_code)

write_csv(expenditure_information_2, "../0_Data/1_Household Data/2_Egypt/1_Data_Clean/expenditures_items_Egypt.csv")

# Appliance data

appliances_0 <- household_0 %>%
  rename(hh_id = caseser, car.01 = car, tv.01 = telv, computer.01 = computer, refrigerator.01 = refrg, stove.01 = cooker,
         washing_machine.01 = wash, dishwasher.01 = dshwsh, ac.01 = cond, fan.01 = fan, heater.01 = heater)%>%
  select(hh_id, ends_with(".01"))

write_csv(appliances_0, "../0_Data/1_Household Data/2_Egypt/1_Data_Clean/appliances_0_1_Egypt.csv")

# Codes
Lighting.Code <- stack(attr(household_0$slight, 'labels'))%>%
  rename(lighting_fuel = values, Lighting_Fuel = ind)%>%
  write_csv(., "../0_Data/1_Household Data/2_Egypt/2_Codes/Lighting.Code.csv")

Cooking.Code <- stack(attr(household_0$scook, 'labels'))%>%
  rename(cooking_fuel = values, Cooking_Fuel = ind)%>%
  write_csv(., "../0_Data/1_Household Data/2_Egypt/2_Codes/Cooking.Code.csv")

Toilet.Code <- stack(attr(household_0$toif, 'labels'))%>%
  rename(toilet = values, Toilet = ind)%>%
  bind_cols(TLT = c("Basic", "Limited", "No Service"))%>%
  write_csv(., "../0_Data/1_Household Data/2_Egypt/2_Codes/Toilet.Code.csv")

Water.Code <- stack(attr(household_0$wat, 'labels'))%>%
  rename(water = values, Water = ind)%>%
  bind_cols(WTR = c("Basic", "Basic", "Limited", "Limited", "Limited"))%>%
  write_csv(., "../0_Data/1_Household Data/2_Egypt/2_Codes/Water.Code.csv")

District.Code <- stack(attr(household_0$reg, 'labels'))%>%
  rename(district = values, District = ind)%>%
  write_csv(., "../0_Data/1_Household Data/2_Egypt/2_Codes/District.Code.csv")

Province.Code <- stack(attr(household_0$area, 'labels'))%>%
  rename(province = values, Province = ind)%>%
  write_csv(., "../0_Data/1_Household Data/2_Egypt/2_Codes/Province.Code.csv")

Education.Code <- stack(attr(individual_0$peduc_d, 'labels'))%>%
  rename(edu_hhh = values, Education = ind)%>%
  filter(edu_hhh %in% individual_1$edu_hhh)%>%
  mutate(ISCED = c(0,0,1,1,1,2,3,3,4,6,7))%>%
  write_csv(., "../0_Data/1_Household Data/2_Egypt/2_Codes/Education.Code.csv")

Gender.Code <- stack(attr(individual_0$psex, 'labels'))%>%
  rename(sex_hhh = values, Gender = ind)%>%
  write_csv(., "../0_Data/1_Household Data/2_Egypt/2_Codes/Gender.Code.csv")

Industry.Code <- stack(attr(individual_0$pind, 'labels'))%>%
  rename(ind_hhh = values, Industry = ind)%>%
  filter(ind_hhh %in% individual_1$ind_hhh)%>%
  write_csv(., "../0_Data/1_Household Data/2_Egypt/2_Codes/Industry.Code.csv")
