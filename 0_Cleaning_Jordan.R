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

household_0  <- read_dta("../0_Data/1_Household Data/2_Jordan/1_Data_Raw/Jordan13-V2 STATA/Jordan13-HH-V2.dta")
individual_0 <- read_dta("../0_Data/1_Household Data/2_Jordan/1_Data_Raw/Jordan13-V2 STATA/Jordan13-IND-V2.dta")

# Transform data

household_1 <- household_0 %>%
  rename(hh_id = caseser, hh_weights = hweight, province = reg, district = area,
         cooking_fuel = scook, water = wat, toilet = toif, inc_gov_monetary = transf, ethnicity = nathd_d)%>%
  mutate(urban_01           = ifelse(rururb == 0,0,1),
         inc_gov_cash       = 0,
         inc_gov_monetary   = ifelse(is.na(inc_gov_monetary),0,inc_gov_monetary))%>%
  select(hh_id, hh_weights, province, district, urban_01, cooking_fuel, water, toilet, ethnicity,
         inc_gov_cash, inc_gov_monetary)

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
  left_join(individual_2)

write_csv(household_information, "../0_Data/1_Household Data/2_Jordan/1_Data_Clean/household_information_Jordan.csv")

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

write.xlsx(Labels, "../0_Data/1_Household Data/2_Jordan/3_Matching_Tables/Item_Codes_Description_Jordan.xlsx")

expenditure_information_2 <- expenditure_information %>%
  left_join(select(Labels, item_name_0, item_code))%>%
  select(hh_id, item_code, expenditures_year)

matching <- read.xlsx("../0_Data/1_Household Data/2_Jordan/3_Matching_Tables/Item_GTAP_Concordance_Jordan.xlsx")%>%
  select (-Explanation) %>%
  pivot_longer(-GTAP, names_to = "drop", values_to = "item_code")%>%
  filter(!is.na(item_code))%>%
  select(GTAP, item_code)

expenditure_information_3 <- expenditure_information_2 %>%
  left_join(matching)%>%
  filter(GTAP != "deleted")%>%
  group_by(hh_id)%>%
  summarise(hh_expenditures = sum(expenditures_year))%>%
  ungroup()%>%
  left_join(select(household_information, hh_id, hh_weights, hh_size))%>%
  mutate(hh_expenditures_pc = hh_expenditures/hh_size)%>%
  distinct()%>%
  mutate(Income_Group_5  = as.numeric(binning(hh_expenditures_pc, bins = 5,  method = c("wtd.quantile"), weights = hh_weights)))

expenditure_information_4.1 <- expenditure_information_2 %>%
  filter(item_code != 56)

expenditure_information_4.2 <- expenditure_information_2 %>%
  filter(item_code == 56)%>%
  left_join(expenditure_information_3)%>%
  # electricity consumption shares from Atamanov (2015)
  # Impute quintile-specific electricity shares and subsequent heating costs
  mutate(factor_electricity = ifelse(Income_Group_5 == 1, 0.035,
                                     ifelse(Income_Group_5 == 2, 0.03,
                                            ifelse(Income_Group_5 == 3, 0.026,
                                                   ifelse(Income_Group_5 == 4, 0.025, 0.024)))))%>%
  mutate(exp_ely = hh_expenditures*factor_electricity)%>%
  mutate(remaining = expenditures_year - exp_ely)%>%
  # in case of outliers (households spending less on aggregate energy compared to what would be expected for electricity overall)
  mutate(exp_ely = ifelse(remaining < 0, expenditures_year, exp_ely))%>%
  mutate(exp_rem = expenditures_year - exp_ely)%>%
  select(hh_id, exp_ely, exp_rem)%>%
  pivot_longer(-hh_id, names_to = "item_transformed", values_to = "expenditures_year")%>%
  mutate(item_code = ifelse(item_transformed == "exp_ely", 561,
                            ifelse(item_transformed == "exp_rem", 56, NA)))%>%
  select(hh_id, item_code, expenditures_year)

expenditure_information_5 <- bind_rows(expenditure_information_4.1, expenditure_information_4.2)%>%
  arrange(hh_id, item_code)%>%
  filter(expenditures_year > 0)
  
write_csv(expenditure_information_5, "../0_Data/1_Household Data/2_Jordan/1_Data_Clean/expenditures_items_Jordan.csv")

# Appliance data

appliances_0 <- household_0 %>%
  rename(hh_id = caseser, car.01 = car, tv.01 = telv, computer.01 = computer, refrigerator.01 = refrg, stove.01 = cooker,
         washing_machine.01 = wash, dishwasher.01 = dshwsh, ac.01 = cond, fan.01 = fan, microwave.01 = microwave)%>%
  select(hh_id, ends_with(".01"))

write_csv(appliances_0, "../0_Data/1_Household Data/2_Jordan/1_Data_Clean/appliances_0_1_Jordan.csv")

# Codes

Cooking.Code <- stack(attr(household_0$scook, 'labels'))%>%
  rename(cooking_fuel = values, Cooking_Fuel = ind)%>%
  write_csv(., "../0_Data/1_Household Data/2_Jordan/2_Codes/Cooking.Code.csv")

Toilet.Code <- stack(attr(household_0$toif, 'labels'))%>%
  rename(toilet = values, Toilet = ind)%>%
  bind_cols(TLT = c("Basic", "Limited", "No Service"))%>%
  write_csv(., "../0_Data/1_Household Data/2_Jordan/2_Codes/Toilet.Code.csv")

Water.Code <- stack(attr(household_0$wat, 'labels'))%>%
  rename(water = values, Water = ind)%>%
  bind_cols(WTR = c("Basic", "Basic", "Limited", "Limited", "Limited"))%>%
  write_csv(., "../0_Data/1_Household Data/2_Jordan/2_Codes/Water.Code.csv")

District.Code <- stack(attr(household_0$area, 'labels'))%>%
  rename(district = values, District = ind)%>%
  write_csv(., "../0_Data/1_Household Data/2_Jordan/2_Codes/District.Code.csv")

Province.Code <- stack(attr(household_0$reg, 'labels'))%>%
  rename(province = values, Province = ind)%>%
  write_csv(., "../0_Data/1_Household Data/2_Jordan/2_Codes/Province.Code.csv")

Education.Code <- stack(attr(individual_0$peduc_d, 'labels'))%>%
  rename(edu_hhh = values, Education = ind)%>%
  filter(edu_hhh %in% individual_1$edu_hhh)%>%
  mutate(ISCED = c(1,1,1,2,2,3,3,4,5,6,7,8))%>%
  write_csv(., "../0_Data/1_Household Data/2_Jordan/2_Codes/Education.Code.csv")

Gender.Code <- stack(attr(individual_0$psex, 'labels'))%>%
  rename(sex_hhh = values, Gender = ind)%>%
  write_csv(., "../0_Data/1_Household Data/2_Jordan/2_Codes/Gender.Code.csv")

Industry.Code <- stack(attr(individual_0$pind, 'labels'))%>%
  rename(ind_hhh = values, Industry = ind)%>%
  filter(ind_hhh %in% individual_1$ind_hhh)%>%
  write_csv(., "../0_Data/1_Household Data/2_Jordan/2_Codes/Industry.Code.csv")

Ethnicity.Code <- stack(attr(household_0$nathd_d, 'labels'))%>%
  rename(ethnicity = values, Ethnicity = ind)%>%
  write_csv(., "../0_Data/1_Household Data/2_Jordan/2_Codes/Ethnicity.Code.csv")
