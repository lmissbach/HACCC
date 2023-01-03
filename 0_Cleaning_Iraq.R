library(tidyverse)
library(Hmisc)
library(haven)
library(readr)
library(openxlsx)
library(sjlabelled)

# This script cleans household data for Iraq
# Author: L. Missbach (missbach@mcc-berlin.net)

# Read Data ####

data_000 <- read_dta("../0_Data/1_Household Data/1_Iraq/1_Data_Raw/IRQ_2012_IHSES_v02_M_Stata8/2012ihses_summary.dta")
data_00 <- read_dta("../0_Data/1_Household Data/1_Iraq/1_Data_Raw/IRQ_2012_IHSES_v02_M_Stata8/2012ihses00_cover_page.dta")
data_01 <- read_dta("../0_Data/1_Household Data/1_Iraq/1_Data_Raw/IRQ_2012_IHSES_v02_M_Stata8/2012ihses01_household_roster.dta")
#data_02 <- read_dta("../0_Data/1_Household Data/1_Iraq/1_Data_Raw/IRQ_2012_IHSES_v02_M_Stata8/2012ihses02_migration.dta")
#data_03 <- read_dta("../0_Data/1_Household Data/1_Iraq/1_Data_Raw/IRQ_2012_IHSES_v02_M_Stata8/2012ihses03_rations.dta")
data_04 <- read_dta("../0_Data/1_Household Data/1_Iraq/1_Data_Raw/IRQ_2012_IHSES_v02_M_Stata8/2012ihses04_housing.dta")
data_05 <- read_dta("../0_Data/1_Household Data/1_Iraq/1_Data_Raw/IRQ_2012_IHSES_v02_M_Stata8/2012ihses05_education.dta")
#data_061 <- read_dta("../0_Data/1_Household Data/1_Iraq/1_Data_Raw/IRQ_2012_IHSES_v02_M_Stata8/2012ihses06_p1_health_members.dta")
#data_062 <- read_dta("../0_Data/1_Household Data/1_Iraq/1_Data_Raw/IRQ_2012_IHSES_v02_M_Stata8/2012ihses06_p2_deceased.dta")
#data_07 <- read_dta("../0_Data/1_Household Data/1_Iraq/1_Data_Raw/IRQ_2012_IHSES_v02_M_Stata8/2012ihses07_anthropometrics.dta")
#data_08 <- read_dta("../0_Data/1_Household Data/1_Iraq/1_Data_Raw/IRQ_2012_IHSES_v02_M_Stata8/2012ihses08_job_search.dta")
data_09 <- read_dta("../0_Data/1_Household Data/1_Iraq/1_Data_Raw/IRQ_2012_IHSES_v02_M_Stata8/2012ihses09_non_food_30_day.dta")
data_10 <- read_dta("../0_Data/1_Household Data/1_Iraq/1_Data_Raw/IRQ_2012_IHSES_v02_M_Stata8/2012ihses10_non_food_90_day.dta")
data_11 <- read_dta("../0_Data/1_Household Data/1_Iraq/1_Data_Raw/IRQ_2012_IHSES_v02_M_Stata8/2012ihses11_non_food_12_month.dta")
data_121 <- read_dta("../0_Data/1_Household Data/1_Iraq/1_Data_Raw/IRQ_2012_IHSES_v02_M_Stata8/2012ihses12_p1_diary_expenditure.dta")
#data_122 <- read_dta("../0_Data/1_Household Data/1_Iraq/1_Data_Raw/IRQ_2012_IHSES_v02_M_Stata8/2012ihses12_p2_meals.dta")
#data_123 <- read_dta("../0_Data/1_Household Data/1_Iraq/1_Data_Raw/IRQ_2012_IHSES_v02_M_Stata8/2012ihses12_p3_shared_meals.dta")
#data_13 <- read_dta("../0_Data/1_Household Data/1_Iraq/1_Data_Raw/IRQ_2012_IHSES_v02_M_Stata8/2012ihses13_jobs.dta")
data_14 <- read_dta("../0_Data/1_Household Data/1_Iraq/1_Data_Raw/IRQ_2012_IHSES_v02_M_Stata8/2012ihses14_wage_jobs.dta")
#data_15 <- read_dta("../0_Data/1_Household Data/1_Iraq/1_Data_Raw/IRQ_2012_IHSES_v02_M_Stata8/2012ihses15_agriculture.dta")
#data_16 <- read_dta("../0_Data/1_Household Data/1_Iraq/1_Data_Raw/IRQ_2012_IHSES_v02_M_Stata8/2012ihses16_enterprises.dta")
data_17 <- read_dta("../0_Data/1_Household Data/1_Iraq/1_Data_Raw/IRQ_2012_IHSES_v02_M_Stata8/2012ihses17_other_income.dta")
data_18 <- read_dta("../0_Data/1_Household Data/1_Iraq/1_Data_Raw/IRQ_2012_IHSES_v02_M_Stata8/2012ihses18_durables.dta")
#data_19 <- read_dta("../0_Data/1_Household Data/1_Iraq/1_Data_Raw/IRQ_2012_IHSES_v02_M_Stata8/2012ihses19_loans_credits.dta")
#data_20 <- read_dta("../0_Data/1_Household Data/1_Iraq/1_Data_Raw/IRQ_2012_IHSES_v02_M_Stata8/2012ihses20_shocks.dta")
#data_21 <- read_dta("../0_Data/1_Household Data/1_Iraq/1_Data_Raw/IRQ_2012_IHSES_v02_M_Stata8/2012ihses21_time_use.dta")
#data_22 <- read_dta("../0_Data/1_Household Data/1_Iraq/1_Data_Raw/IRQ_2012_IHSES_v02_M_Stata8/2012ihses22_justice.dta")
#data_23 <- read_dta("../0_Data/1_Household Data/1_Iraq/1_Data_Raw/IRQ_2012_IHSES_v02_M_Stata8/2012ihses23_life_satisfaction.dta")

# Transform Data ####

# Household

data_00.1 <- data_00 %>%
  rename(hh_id = questid)%>%
  select(hh_id, q00_16)%>%
  mutate(urban_01 = ifelse(q00_16 == 1,1,0))%>%
  select(hh_id, urban_01)

data_01.1 <- data_01 %>%
  rename(hh_id = questid, hh_weights = weight, hh_size = hsize, district = governorate)%>% # weight to be debated
  select(hh_id, hh_size, hh_weights, district)%>%
  distinct()

data_01.2 <- data_01 %>%
  rename(hh_id = questid, sex_hhh = q0102, age_hhh = q0104)%>%
  filter(q0105 == 1)%>%
  select(hh_id, sex_hhh, age_hhh)%>%
  mutate(age_hhh = ifelse(is.na(age_hhh),18,0))

data_01.3 <- data_01 %>%
  rename(hh_id = questid)%>%
  mutate(adults   = ifelse(q0104 > 15 | is.na(q0104),1,0),
         children = ifelse(q0104 < 16 & !is.na(q0104), 1,0))%>%
  group_by(hh_id)%>%
  summarise(adults = sum(adults),
            children = sum(children))%>%
  ungroup()

data_04.1 <- data_04 %>%
  rename(hh_id = questid, water = q0414, toilet_a = q0419, toilet_b = q0420)%>%
  select(hh_id, water, toilet_a, toilet_b, starts_with("q0421"), q0426_1_1, q0426_2_1, q0426_3_1)%>%
  rename(cooking_fuel = q0426_1_1, lighting_fuel = q0426_2_1, heating_fuel = q0426_3_1)%>%
  mutate(electricity.access = ifelse(q0421_1 == 1,1,0))%>%
  unite(toilet, c(toilet_a, toilet_b), sep = "0")%>%
  select(hh_id, water, toilet, cooking_fuel, lighting_fuel, heating_fuel, electricity.access)%>%
  left_join(Toilet.Code.B, by = c("toilet" = "toilet_old"))%>%
  select(-toilet)%>%
  rename(toilet = toilet.y)%>%
  mutate(cooking_fuel  = ifelse(is.na(cooking_fuel),4,  cooking_fuel),
         lighting_fuel = ifelse(is.na(lighting_fuel),8, lighting_fuel),
         heating_fuel  = ifelse(is.na(heating_fuel),13, heating_fuel),
         water         = ifelse(is.na(water),        9, water))

data_05.1 <- data_05%>%
  rename(hh_id = questid, edu_hhh = q0503)%>%
  filter(idcode == 1)%>%
  select(hh_id, edu_hhh, q0502, q0507_1)

Education.Code.A <- stack(attr(data_05.1$edu_hhh, 'labels'))%>%
  rename(edu_hhh = values, Education_0 = ind)
Education.Code.B <- stack(attr(data_05.1$q0507_1, 'labels'))%>%
  rename(q0507_1 = values, Education_1 = ind)

data_05.1 <- left_join(data_05.1, Education.Code.A)%>%
  left_join(Education.Code.B)%>%
  mutate(Education_0 = as.character(Education_0),
         Education_1 = as.character(Education_1))%>%
  mutate(Education_2 = ifelse(is.na(Education_0) & q0502 == 2,"No certificate",Education_0))%>%
  mutate(Education_2 = ifelse(is.na(Education_2), Education_1, Education_2))

Education.Code <- distinct(data_05.1, Education_2)%>%
  mutate(edu_hhh = 1:n())

data_05.1 <- data_05.1 %>%
  left_join(Education.Code, by = "Education_2")%>%
  select(hh_id, edu_hhh.y)%>%
  rename(edu_hhh = edu_hhh.y)

Education.Code <- Education.Code %>%
  rename(Education = Education_2)%>%
  write_csv(., "../0_Data/1_Household Data/1_Iraq/2_Codes/Education.Code.csv")

# Transfers

data_17.1 <- data_17 %>%
  rename(hh_id = questid)%>%
  filter(q1701 == 1)%>%
  rowwise()%>%
  mutate(income = sum(q1702_a2, q1702_b2, q1702_c2, na.rm = TRUE))%>%
  select(hh_id, inc_code, income)%>%
  mutate(type = ifelse(inc_code %in% c(13,14,15,16,17,18,19,20,21,22), "inc_gov_monetary",
                       ifelse(inc_code %in% c(25,30), "inc_gov_cash", NA)))%>%
  filter(!is.na(type))%>%
  group_by(hh_id, type)%>%
  summarise(income = sum(income))%>%
  ungroup()%>%
  pivot_wider(names_from = "type", values_from = "income")


data_01.4 <- data_00.1 %>%
  left_join(data_01.1)%>%
  left_join(data_01.2)%>%
  left_join(data_01.3)%>%
  left_join(data_04.1)%>%
  left_join(data_05.1)%>%
  left_join(data_17.1)%>%
  mutate(inc_gov_cash = ifelse(is.na(inc_gov_cash),0, inc_gov_cash),
         inc_gov_monetary = ifelse(is.na(inc_gov_monetary),0,inc_gov_monetary))%>%
  remove_all_labels()

write_csv(data_01.4, "../0_Data/1_Household Data/1_Iraq/1_Data_Clean/household_information_Iraq.csv")

# Remove labels before saving 

# Expenditures 

# Chapter 3 includes information on rations
# Chapter 4 from question 435 on housing related expenditures

data_09.1 <- data_09 %>%
  rename(hh_id = questid, item_code = q0901c, expenditures = q0903, type = q0904)%>%
  select(hh_id, item_code, expenditures, type)%>%
  mutate(sp = ifelse(type == 1,0,1),
         expenditures_year = expenditures*12)%>%
  mutate(expenditures_sp_year = ifelse(sp == 1, expenditures_year,0),
         expenditures_year    = ifelse(sp == 0, expenditures_year,0))%>%
  group_by(hh_id, item_code)%>%
  summarise(expenditures_year = sum(expenditures_year),
            expenditures_sp_year = sum(expenditures_sp_year))%>%
  ungroup()

data_10.1 <- data_10 %>%
  rename(hh_id = questid, item_code = q1001c, expenditures = q1003, type = q1004)%>%
  select(hh_id, item_code, expenditures, type)%>%
  mutate(sp = ifelse(type == 1,0,1),
         expenditures_year = expenditures*4)%>%
  mutate(expenditures_sp_year = ifelse(sp == 1, expenditures_year,0),
         expenditures_year    = ifelse(sp == 0, expenditures_year,0))%>%
  group_by(hh_id, item_code)%>%
  summarise(expenditures_year = sum(expenditures_year),
            expenditures_sp_year = sum(expenditures_sp_year))%>%
  ungroup()

data_11.1 <- data_11 %>%
  rename(hh_id = questid, item_code = q1101c, expenditures_year = q1103, type = q1104)%>%
  select(hh_id, item_code, expenditures_year, type)%>%
  mutate(sp = ifelse(type == 1,0,1))%>%
  mutate(expenditures_sp_year = ifelse(sp == 1, expenditures_year,0),
         expenditures_year    = ifelse(sp == 0, expenditures_year,0))%>%
  group_by(hh_id, item_code)%>%
  summarise(expenditures_year = sum(expenditures_year),
            expenditures_sp_year = sum(expenditures_sp_year))%>%
  ungroup()

data_121.1 <- data_121 %>%
  rename(hh_id = questid, item_code = q1201, expenditures = q1203, type = q1204)%>%
  select(hh_id, item_code, expenditures, type)%>%
  mutate(sp = ifelse(type == 1,0,1),
         expenditures_year = expenditures*52/1000)%>%
  mutate(expenditures_sp_year = ifelse(sp == 1, expenditures_year,0),
         expenditures_year    = ifelse(sp == 0, expenditures_year,0))%>%
  group_by(hh_id, item_code)%>%
  summarise(expenditures_year = sum(expenditures_year),
            expenditures_sp_year = sum(expenditures_sp_year))%>%
  ungroup()

data_04.0 <- data_04 %>%
  rename(hh_id = questid)%>%
  select(hh_id, q0435_1:q0438_5)%>%
  mutate(q11 = q0436_1*365/q0437_1,
         q12 = q0436_2*365/q0437_2,
         q13 = q0436_3*365/q0437_3,
         q14 = q0436_4*365/q0437_4,
         q15 = q0436_5*365/q0437_5)%>%
  select(hh_id, q11:q15)%>%
  pivot_longer(q11:q15, names_to = "item_code", values_to = "expenditures_year", names_prefix = "q")%>%
  filter(!is.na(expenditures_year))%>%
  mutate(expenditures_sp_year = 0)%>%
  mutate(item_code = as.numeric(item_code))

data_expenditures_1 <- bind_rows(data_09.1, data_10.1)%>%
  bind_rows(data_11.1)%>%
  bind_rows(data_121.1)%>%
  #mutate(item_code = as.character(item_code))%>%
  bind_rows(data_04.0)%>%
  arrange(hh_id, item_code)%>%
  mutate(item_code_4 = str_sub(item_code,1,-3))%>%
  mutate(item_code_new = ifelse(item_code_4 %in% c(4521, 4522, 4531, 4541, 7221) | nchar(item_code) < 5, item_code, item_code_4))
  
data_expenditures_2 <- data_expenditures_1 %>%
  select(hh_id, item_code_new, expenditures_year, expenditures_sp_year)%>%
  rename(item_code = item_code_new)%>%
  group_by(hh_id,item_code)%>%
  summarise(expenditures_year    = sum(expenditures_year),
            expenditures_sp_year = sum(expenditures_sp_year))%>%
  ungroup()
  
# Always in thousand Dinares

write_csv(data_expenditures_2, "../0_Data/1_Household Data/1_Iraq/1_Data_Clean/expenditures_items_Iraq.csv")

# Appliances

# data_04.2 <- data_04 %>%
#   select(questid, q0441)%>%
#   mutate(ac.01     = ifelse(q0441 == 1,1,0),
#          cooler.01 = ifelse(q0441 == 2,1,0),
#          fan.01    = ifelse(q0441 == 3,1,0))%>%
#   rename(hh_id = questid)%>%
#   select(-q0441)

data_18.1 <- data_18 %>%
  rename(hh_id = questid)%>%
  select(hh_id, durable_code, q1801)%>%
  distinct()%>%
  filter(durable_code != 23)%>%
  mutate(car.01             = ifelse((durable_code == 1 | durable_code == 2) & q1801 > 0,1,0),
         motorcycle.01      = ifelse(durable_code == 3 & q1801 > 0,1,0),
         cooler.01          = ifelse(durable_code == 4 & q1801 > 0,1,0),
         refrigerator.01    = ifelse(durable_code == 5 & q1801 > 0,1,0),
         washing_machine.01 = ifelse(durable_code == 6 & q1801 > 0,1,0),
         generator.01       = ifelse(durable_code == 7 & q1801 > 0,1,0),
         cooker.01          = ifelse(durable_code == 9 & q1801 > 0,1,0),
         heater.01          = ifelse((durable_code == 10 | durable_code == 11) & q1801 > 0,1,0),
         fan.01             = ifelse(durable_code == 12 & q1801 > 0,1,0),
         ac.01              = ifelse(durable_code == 13 & q1801 > 0,1,0),
         dishwasher.01      = ifelse(durable_code == 16 & q1801 > 0,1,0),
         computer.01        = ifelse(durable_code == 18 & q1801 > 0,1,0))%>%
  group_by(hh_id)%>%
  summarise(car.01             = max(car.01             ),
            motorcycle.01      = max(motorcycle.01      ),
            cooler.01          = max(cooler.01          ),
            refrigerator.01    = max(refrigerator.01    ),
            washing_machine.01 = max(washing_machine.01 ),
            generator.01       = max(generator.01       ),
            cooker.01          = max(cooker.01          ),
            heater.01          = max(heater.01          ),
            fan.01             = max(fan.01             ),
            ac.01              = max(ac.01              ),
            dishwasher.01      = max(dishwasher.01      ),
            computer.01        = max(computer.01        ))%>%
  ungroup()%>%
  right_join(select(data_01.4, hh_id))%>%
  # Assumption for 15 households
  mutate_at(vars(-hh_id), list(~ ifelse(is.na(.),0,.)))

write_csv(data_18.1, "../0_Data/1_Household Data/1_Iraq/1_Data_Clean/appliances_0_1_Iraq.csv")

# Codes ####

Item.Code.A <- stack(attr(data_09$q0901c, 'labels'))
Item.Code.B <- stack(attr(data_10$q1001c, 'labels'))
Item.Code.C <- stack(attr(data_11$q1101c, 'labels'))
Item.Code.D <- stack(attr(data_121$q1201, 'labels'))
Item.Codes <- distinct(data_expenditures_1, item_code)%>%
  arrange(item_code)%>%
  mutate(item_code = as.numeric(item_code))%>%
  left_join(Item.Code.A, by = c("item_code" = "values"))%>%
  left_join(Item.Code.B, by = c("item_code" = "values"))%>%
  left_join(Item.Code.C, by = c("item_code" = "values"))%>%
  left_join(Item.Code.D, by = c("item_code" = "values"))%>%
  select(item_code, ind.x, ind.y.y)%>%
  mutate(item_name = ifelse(!is.na(ind.x), as.character(ind.x), as.character(ind.y.y)))%>%
  select(item_code, item_name)%>%
  mutate(item_name = ifelse(item_code == 11, "Water and Sewerage",
                            ifelse(item_code == 12, "Electricity (Public grid)",
                                   ifelse(item_code == 13, "Electricity (Generator)",
                                          ifelse(item_code == 14, "Land / Phone Line",
                                                 ifelse(item_code == 15, "Housing rent", item_name))))))%>%
  mutate(item_code_4 = str_sub(item_code,1,-3))%>%
  mutate(item_code_new = ifelse(item_code_4 %in% c(4521, 4522, 4531, 4541, 7221) | nchar(item_code) < 5, item_code, item_code_4))

write.xlsx(Item.Codes, "../0_Data/1_Household Data/1_Iraq/3_Matching_Tables/Item_Code_Description_Iraq.xlsx")

Toilet.A <- stack(attr(data_04$q0419, 'labels'))
Toilet.B <- stack(attr(data_04$q0420, 'labels'))
Toilet.Code <- distinct(data_04, q0419, q0420)%>%
  unite(toilet, c(q0419, q0420), sep = "0", remove = FALSE)%>%
  left_join(Toilet.A, by = c("q0419" = "values"))%>%
  left_join(Toilet.B, by = c("q0420" = "values"))%>%
  rename(toilet_old = toilet)%>%
  mutate(toilet = 1:n())%>%
  unite(Toilet, c(ind.x, ind.y), sep = " ")
Toilet.Code.A <- select(Toilet.Code, toilet, Toilet)%>%
  write_csv(., "../0_Data/1_Household Data/1_Iraq/2_Codes/Toilet.Code.csv")
Toilet.Code.B <- select(Toilet.Code, toilet, toilet_old)

Gender.Code <- stack(attr(data_01$q0102, 'labels'))%>%
  rename(Gender = ind, sex_hhh = values)%>%
  write_csv(., "../0_Data/1_Household Data/1_Iraq/2_Codes/Gender.Code.csv")
District.Code <- stack(attr(data_01$governorate, 'labels'))%>%
  rename(District = ind, district = values)%>%
  write_csv(., "../0_Data/1_Household Data/1_Iraq/2_Codes/District.Code.csv")
Water.Code <- stack(attr(data_04$q0414, 'labels'))%>%
  rename(Water = ind, water = values)%>%
  write_csv(., "../0_Data/1_Household Data/1_Iraq/2_Codes/Water.Code.csv")
Lighting.Code <- stack(attr(data_04$q0426_2_1, 'labels'))%>%
  rename(Lighting_Fuel = ind, lighting_fuel = values)%>%
  write_csv(., "../0_Data/1_Household Data/1_Iraq/2_Codes/Lighting.Code.csv")
Heating.Code <- stack(attr(data_04$q0426_3_1, 'labels'))%>%
  rename(Heating_Fuel = ind, heating_fuel = values)%>%
  bind_rows(data.frame(heating_fuel = 13, Heating_Fuel = "Other"))%>%
  write_csv(., "../0_Data/1_Household Data/1_Iraq/2_Codes/Heating.Code.csv")
Cooking.Code <- stack(attr(data_04$q0426_1_1, 'labels'))%>%
  rename(Cooking_Fuel = ind, cooking_fuel = values)%>%
    write_csv(., "../0_Data/1_Household Data/1_Iraq/2_Codes/Cooking.Code.csv")
