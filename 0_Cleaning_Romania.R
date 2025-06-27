# 1        Packages ####

if(!require("pacman")) install.packages("pacman")

p_load("countrycode", "foreign", "ggsci", "haven", "Hmisc", "openxlsx", "rattle", "scales", "tidyverse", "sjlabelled")

options(scipen=999)

# 2. Load data ####

data_0   <- read.dbf("../0_Data/1_Household Data/4_Romania/1_Data_Raw/ABF 2019/S0.dbf")
data_1   <- read.dbf("../0_Data/1_Household Data/4_Romania/1_Data_Raw/ABF 2019/S1.dbf") # Household members
data_2   <- read.dbf("../0_Data/1_Household Data/4_Romania/1_Data_Raw/ABF 2019/S2.dbf") # Occupation
data_4   <- read.dbf("../0_Data/1_Household Data/4_Romania/1_Data_Raw/ABF 2019/S4.dbf") # Food
data_5   <- read.dbf("../0_Data/1_Household Data/4_Romania/1_Data_Raw/ABF 2019/S5.dbf") # Non-food
data_6   <- read.dbf("../0_Data/1_Household Data/4_Romania/1_Data_Raw/ABF 2019/S6.dbf") # Services
data_7   <- read.dbf("../0_Data/1_Household Data/4_Romania/1_Data_Raw/ABF 2019/S7.dbf") # Income
data_8   <- read.dbf("../0_Data/1_Household Data/4_Romania/1_Data_Raw/ABF 2019/S8.dbf") # Other expenses
# data_9a  <- read.dbf("../0_Data/1_Household Data/4_Romania/1_Data_Raw/ABF 2019/S9a.dbf") # Food outside home
# data_9b  <- read.dbf("../0_Data/1_Household Data/4_Romania/1_Data_Raw/ABF 2019/S9b.dbf")
# data_9c  <- read.dbf("../0_Data/1_Household Data/4_Romania/1_Data_Raw/ABF 2019/S9c.dbf")
data_10a <- read.dbf("../0_Data/1_Household Data/4_Romania/1_Data_Raw/ABF 2019/S10a.dbf") # Living conditions
data_10b <- read.dbf("../0_Data/1_Household Data/4_Romania/1_Data_Raw/ABF 2019/S10b.dbf") # Assets
# data_11  <- read.dbf("../0_Data/1_Household Data/4_Romania/1_Data_Raw/ABF 2019/S11.dbf") # Satisfaction
# data_sre <- read.dbf("../0_Data/1_Household Data/4_Romania/1_Data_Raw/ABF 2019/SRE.dbf")
# data_sve <- read.dbf("../0_Data/1_Household Data/4_Romania/1_Data_Raw/ABF 2019/SVE.dbf")

# Should be 30,876

# 3. Transform data ####

data_0.1 <- data_0 %>%
  mutate(urban_01 = ifelse(MEDIU == 1, 1,0))%>%
  rename(province = REGIUNE, hh_weights = COEFJ)%>%
  filter(RI == 1)%>%
  unite("hh_id", c(CODLA, CENTRA,NRGL), sep = "0")%>%
  select(hh_id, urban_01, province, hh_weights)

data_1.1 <- data_1 %>%
  filter(CPERS == 1)%>%
  unite("hh_id", c(CODLA, CENTRA,NRGL), sep = "0")%>%
  rename(sex_hhh = SEX, nationality = NAT, nationality_1 = TARAO, edu_hhh = NIVE, hhh_occupation = STOCUPAN)%>%
  mutate(age_hhh = 2019 - ANN)%>%
  select(hh_id, sex_hhh, nationality, nationality_1, edu_hhh, hhh_occupation, age_hhh)

data_1.2 <- data_1 %>%
  mutate(age = 2019 - ANN)%>%
  unite("hh_id", c(CODLA, CENTRA,NRGL), sep = "0")%>%
  mutate(adult    = ifelse(age > 15,1,0),
         children = ifelse(age < 16,1,0))%>%
  group_by(hh_id)%>%
  summarise(hh_size  = n(),
            adults   = sum(adult),
            children = sum(children))%>%
  ungroup()

data_2.1 <- data_2 %>%
  unite("hh_id", c(CODLA, CENTRA,NRGL), sep = "0")%>%
  filter(CPERS == 1)%>%
  rename(occupation_hhh = OCUP, activity_hhh = ACTIV)%>%
  select(hh_id, occupation_hhh, activity_hhh)

data_10a.1 <- data_10a %>%
  unite("hh_id", c(CODLA, CENTRA,NRGL), sep = "0")%>%
  rename(construction_year = ANNC, rooms = NRCAM, area = SMP, house_type = TIPLOC, lighting_fuel = ILUMIN)%>%
  # INCALZ
  mutate(Heating_Fuel = ifelse(INCALZ_1 == 1, "District heating",
                               ifelse(INCALZ_21 == 21, "Wood",
                                      ifelse(INCALZ_22 == 22, "Natural gas",
                                             ifelse(INCALZ_23 == 23, "Electricity",
                                                    ifelse(INCALZ_24 == 24, "Other heating",
                                                           ifelse(INCALZ_3 == 3, "Natural gas stove",
                                                                  ifelse(INCALZ_4 == 4, "Wood, coal or oil stove",
                                                                         ifelse(INCALZ_5 == 5, "No heating",
                                                                                ifelse(INCALZ_6 == 6, "Not connected","Other"))))))))))%>%
  # COMBUST
  mutate(Cooking_Fuel = ifelse(COMBUST_1 == 1, "Electricity",
                               ifelse(COMBUST_2 == 2, "Natural gas",
                                      ifelse(COMBUST_3 == 1, "Wood, coal, oil",
                                             ifelse(COMBUST_4 == 4, "LPG",
                                                    ifelse(COMBUST_5 == 5, "Other", "Other"))))))%>%
  select(hh_id, Cooking_Fuel, Heating_Fuel, lighting_fuel, house_type, area, rooms, construction_year)

household_information <- data_0.1 %>%
  left_join(data_1.1)%>%
  left_join(data_1.2)%>%
  left_join(data_2.1)%>%
  left_join(data_10a.1)

data_10b.1 <- data_10b %>%
  unite("hh_id", c(CODLA, CENTRA,NRGL), sep = "0")%>%
  select(hh_id, INZES_03, INZES_04, INZES_07, INZES_20, INZES_21, INZES_24, INZES_25, INZES_26)%>%
  rename(number_of_cars = INZES_26)%>%
  mutate_at(vars(starts_with("INZES")), ~ ifelse(. > 0,1,0))%>%
  rename(refrigerator.01 = INZES_03, freezer.01 = INZES_04, washing_machine.01 = INZES_07, motorcycle.01 = INZES_20, bicycle.01 = INZES_21, tv.01 = INZES_24, mobile.01 = INZES_25)

write_csv(data_10b.1, "../0_Data/1_Household Data/4_Romania/1_Data_Clean/appliances_0_1_Romania.csv")

rm(data_0, data_1, data_10a, data_10b)

# Have reason to assume its monthly data

data_4.1 <- data_4 %>%
  unite("hh_id", c(CODLA, CENTRA,NRGL), sep = "0")%>%
  rename(item_code = CODP, expenditures = SUMPL)%>%
  select(hh_id, item_code, expenditures)%>%
  mutate(item_code = as.character(item_code))

data_5.1 <- data_5 %>%
  unite("hh_id", c(CODLA, CENTRA,NRGL), sep = "0")%>%
  rename(item_code = CODM, expenditures = VQC)%>%
  select(hh_id, item_code, expenditures)%>%
  mutate(item_code = as.character(item_code))

data_6.1 <- data_6 %>%
  unite("hh_id", c(CODLA, CENTRA,NRGL), sep = "0")%>%
  select(hh_id, starts_with("R5"))%>%
  pivot_longer(starts_with("R5"), names_to = "item_code", values_to = "expenditures", names_prefix = "R")%>%
  filter(expenditures != 0)%>%
  mutate(item_code = as.character(item_code))

data_8.1 <- data_8 %>%
  unite("hh_id", c(CODLA, CENTRA,NRGL), sep = "0")%>%
  select(hh_id, starts_with("R6"))%>%
  pivot_longer(starts_with("R6"), names_to = "item_code", values_to = "expenditures", names_prefix = "R")%>%
  filter(expenditures != 0)%>%
  mutate(item_code = as.character(item_code))

expenses <- bind_rows(data_4.1, data_5.1, data_6.1, data_8.1)%>%
  arrange(hh_id, item_code)%>%
  filter(expenditures > 0)%>%
  # Expenditures refer to monthly values
  mutate(expenditures_year = expenditures*12)

write_csv(expenses, "../0_Data/1_Household Data/4_Romania/1_Data_Clean/expenditures_items_Romania.csv")

# Codes ####

Province.Code <- distinct(data_0, REGIUNE)%>% # REGIUNE
  rename(province = REGIUNE)%>%
  arrange(province)%>%
  mutate(Province = c("Nord-Est", "Sud-Est", "Sud-Muntenia", "Sud Vest-Oltenia", "Vest", "Nord-Vest", "Centru", "Bucuresti-Ilfov"))%>%
  write_csv(., "T:/MSA/papers_internal/work_in_progress/Mi_Homogenized_Datainfrastructure/0_Data/1_Household Data/4_Romania/2_Codes/Province.Code.csv")

Gender.Code <- distinct(data_1, SEX)%>% # SEX
  rename(sex_hhh = SEX)%>%
  arrange(sex_hhh)%>%
  mutate(Gender = c("Male", "Female"))%>%
  write_csv(., "T:/MSA/papers_internal/work_in_progress/Mi_Homogenized_Datainfrastructure/0_Data/1_Household Data/4_Romania/2_Codes/Gender.Code.csv")

Nationality.Code <- distinct(data_1, NAT)%>% # NAT
  rename(nationality = NAT)%>%
  arrange(nationality)%>%
  mutate(Nationality = c("Romanian", "Hungarian", "Gypsy", "German", "Other"))%>%
  write_csv(., "T:/MSA/papers_internal/work_in_progress/Mi_Homogenized_Datainfrastructure/0_Data/1_Household Data/4_Romania/2_Codes/Nationality.Code.csv")

Nationality.1.Code <- distinct(data_1, TARAO)%>% # TARAO
  rename(nationality_1 = TARAO)%>%
  arrange(nationality_1)%>%
  mutate(Nationality_1 = c("Romania", "Other EU member countries", "Non-EU countries"))%>%
  write_csv(., "T:/MSA/papers_internal/work_in_progress/Mi_Homogenized_Datainfrastructure/0_Data/1_Household Data/4_Romania/2_Codes/Nationality.1.Code.csv")

Education.Code <- distinct(data_1, NIVE)%>% # NIVE
  rename(edu_hhh = NIVE)%>%
  arrange(edu_hhh)%>%
  mutate(Education = c("no completed school", "preschool (kindergarten)", "primary school (grades 0–4)", "middle school (grades 5–8)", "vocational school", 
                       "high school (grade 9 or 10)", "high school (grade 11 or 12/13)", "post-secondary school / master's school", "short-term university (college 2-4 years)",
                       "university – bachelor's degree (study duration 3 or 4 years)", "university – bachelor's degree (study duration 5 or 6 years, master's degree, postgraduate studies)", 
                       "university – doctorate or post-doctorate"))%>%
  write_csv(., "T:/MSA/papers_internal/work_in_progress/Mi_Homogenized_Datainfrastructure/0_Data/1_Household Data/4_Romania/2_Codes/Education.Code.csv")

Occupation.Code <- distinct(data_1, STOCUPAN)%>% # STOCUPAN
  rename(hhh_occupation = STOCUPAN)%>%
  arrange(hhh_occupation)%>%
  mutate(Occupation = c("employee",
                        "employer",
                        "self-employed in non-agricultural activities",
                        "member of a non-agricultural cooperative",
                        "self-employed in agriculture",
                        "member of an agricultural association",
                        "family helper",
                        "unemployed",
                        "pensioner",
                        "pupil",
                        "student",
                        "housewife",
                        "other status (elderly person,
                                      preschooler, dependent person,
                                      disabled person, born
                                      in the month of the survey, etc.)"))%>%
  write_csv(., "T:/MSA/papers_internal/work_in_progress/Mi_Homogenized_Datainfrastructure/0_Data/1_Household Data/4_Romania/2_Codes/Occupation.Code.csv")

# https://www.mmuncii.ro/j33/index.php/ro/2014-domenii/munca/c-o-r

Occupation.2.Code <- distinct(data_2, OCUP)%>% # OCUP
  rename(occupation_hhh = OCUP)%>%
  arrange(occupation_hhh)%>%
  mutate(Occupation_2 = c())%>%
  write_csv(., "T:/MSA/papers_internal/work_in_progress/Mi_Homogenized_Datainfrastructure/0_Data/1_Household Data/4_Romania/2_Codes/Occupation.2.Code.csv")

# Activity.Code <- distinct(data_2, ACTIV)%>% # ACTIV
#   rename(activity_hhh = ACTIV)%>%
#   arrange(activity_hhh)%>%
#   mutate(Activity = c())%>%
#   write_csv(., "T:/MSA/papers_internal/work_in_progress/Mi_Homogenized_Datainfrastructure/0_Data/1_Household Data/4_Romania/2_Codes/Activity.Code.csv")

Cooking.Code <- distinct(household_information, Cooking_Fuel)%>% # COMBUST
  arrange(Cooking_Fuel)%>%
  mutate(cooking_fuel = c(1:n()))%>%
  write_csv(., "T:/MSA/papers_internal/work_in_progress/Mi_Homogenized_Datainfrastructure/0_Data/1_Household Data/4_Romania/2_Codes/Cooking.Code.csv")

Heating.Code <- distinct(household_information, Heating_Fuel)%>% # INCALZ
  arrange(Heating_Fuel)%>%
  mutate(heating_fuel = c(1:n()))%>%
  write_csv(., "T:/MSA/papers_internal/work_in_progress/Mi_Homogenized_Datainfrastructure/0_Data/1_Household Data/4_Romania/2_Codes/Heating.Code.csv")

Lighting.Code <- distinct(data_10a, ILUMIN)%>% # ILUMIN
  rename(lighting_fuel = ILUMIN)%>%
  arrange(lighting_fuel)%>%
  mutate(Lighting_Fuel = c("No lighting", "Electricity", "Oil", "Other"))%>%
  write_csv(., "T:/MSA/papers_internal/work_in_progress/Mi_Homogenized_Datainfrastructure/0_Data/1_Household Data/4_Romania/2_Codes/Lighting.Code.csv")

House.Code <- distinct(data_10a, TIPLOC)%>% # TIPLOC
  rename(house_type = TIPLOC)%>%
  arrange(house_type)%>%
  mutate(Housing_Type = c("Other", "Apartment", "Detached house"))%>%
  write_csv(., "T:/MSA/papers_internal/work_in_progress/Mi_Homogenized_Datainfrastructure/0_Data/1_Household Data/4_Romania/2_Codes/Housing.Code.csv")

household_information_1 <- household_information %>%
  left_join(Cooking.Code)%>%
  left_join(Heating.Code)%>%
  select(-Cooking_Fuel, - Heating_Fuel)%>%
  select(-activity_hhh)
    
write_csv(household_information_1, "../0_Data/1_Household Data/4_Romania/1_Data_Clean/household_information_Romania.csv")
    