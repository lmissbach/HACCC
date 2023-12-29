if(!require("pacman")) install.packages("pacman")

p_load("haven", "Hmisc", "openxlsx", "rattle", "scales", "tidyverse", "sjlabelled")

options(scipen=999)

# Loading Data ####

Level1_68 <- read_dta("T:/MSA/papers_internal/work_in_progress/HH Surveys Paper/India_analysis/Data/nss_68/Raw/Identification of Sample Household - Block 1 and 2 - Level 1 -  68.dta") # information on survey
Level2_68 <- read_dta("T:/MSA/papers_internal/work_in_progress/HH Surveys Paper/India_analysis/Data/nss_68/Raw/Level2_68.dta") # information on household characteristics a
Level3_68 <- read_dta("T:/MSA/papers_internal/work_in_progress/HH Surveys Paper/India_analysis/Data/nss_68/Raw/Level3_68.dta") # information on household characteristics b
Level4_68 <- read_dta("T:/MSA/papers_internal/work_in_progress/HH Surveys Paper/India_analysis/Data/nss_68/Raw/Level4_68.dta") # information on household members
Level5_68 <- read_dta("T:/MSA/papers_internal/work_in_progress/HH Surveys Paper/India_analysis/Data/nss_68/Raw/Level5_68.dta") # expenditures on food and energy
Level6_68 <- read_dta("T:/MSA/papers_internal/work_in_progress/HH Surveys Paper/India_analysis/Data/nss_68/Raw/Level6_68.dta") # expenditures on clothing, bedding, footwear
Level7_68 <- read_dta("T:/MSA/papers_internal/work_in_progress/HH Surveys Paper/India_analysis/Data/nss_68/Raw/Level7_68.dta") # expenditures on education and services
Level8_68 <- read_dta("T:/MSA/papers_internal/work_in_progress/HH Surveys Paper/India_analysis/Data/nss_68/Raw/Level8_68.dta") # miscellaneous goods
Level9_68 <- read_dta("T:/MSA/papers_internal/work_in_progress/HH Surveys Paper/India_analysis/Data/nss_68/Raw/Level9_68.dta") # information on appliances
# Level10_68 <- read_dta("T:/MSA/papers_internal/work_in_progress/HH Surveys Paper/India_analysis/Data/nss_68/Raw/Level10_68.dta") # information on Ayurveda and Indian traditional medicine
# Level11_68 <- read_dta("T:/MSA/papers_internal/work_in_progress/HH Surveys Paper/India_analysis/Data/nss_68/Raw/Level11_68.dta") # information on a condensed level

# Transforming Data ####

level_1 <- Level1_68 %>%
  select(HHID, Combined_multiplier, Sector, State_code, District_code)%>%
  rename(hh_id = HHID, urban = Sector, hh_weights = Combined_multiplier, province = State_code, district = District_code)%>%
  mutate(urban_01 = ifelse(urban == 2, 1,0))%>%
  remove_all_labels()%>%
  select(-urban)

level_2 <- Level2_68 %>%
  select(HHID, HH_Size, Religion, Social_Group, NIC_2008)%>%
  rename(hh_id = HHID, hh_size = HH_Size, religion = Religion, ethnicity = Social_Group, ind_hhh = NIC_2008)%>%
  mutate(religion  = ifelse(religion == "",  "9", religion),
         ethnicity = ifelse(ethnicity == "", "9", ethnicity))%>%
  remove_all_labels()

Level_12 <- left_join(level_1, level_2, by = "hh_id")%>%
  mutate(hh_size = as.numeric(hh_size))

# Population: 1.108.970.933

level_3 <- Level3_68%>%
  select(HHID, Cooking_Code, Lighting_Code)%>%
  rename(hh_id = HHID, lighting_fuel = Lighting_Code, cooking_fuel = Cooking_Code)%>%
  remove_all_labels()%>%
  mutate(electricity.access = ifelse(lighting_fuel == 5,1,0))%>%
  mutate(cooking_fuel = as.numeric(cooking_fuel))%>%
  mutate(cooking_fuel = ifelse(is.na(cooking_fuel), 10, cooking_fuel),
         lighting_fuel = ifelse(lighting_fuel == "",9, lighting_fuel))

Level_123 <- left_join(Level_12, level_3, by = "hh_id")%>%
  mutate(electricity.access = ifelse(lighting_fuel == 5 | cooking_fuel == 8, 1, 0))# Attention: Imputed

level_4 <- Level4_68%>%
  select(HHID, Person_sr_no,Age, Relation, Education, Sex)

level_4$Age[is.na(level_4$Age)] <- 0

level_4 <- level_4%>%
  mutate(adults = ifelse(Age >= 18, 1, 0))%>%
  mutate(children = ifelse(Age < 18, 1, 0))%>%
  group_by(HHID)%>%
  mutate(adults = sum(adults))%>%
  mutate(children = sum(children))%>%
  ungroup()

level_41 <- level_4 %>%
  filter(Relation == 1)%>%
  select(HHID, Education, adults, children, Sex)%>%
  rename(hh_id = HHID, edu_hhh = Education, sex_hhh = Sex)%>%
  remove_all_labels()%>%
  mutate(edu_hhh = as.numeric(edu_hhh))%>%
  mutate(edu_hhh = ifelse(is.na(edu_hhh),14,edu_hhh))

Level_1234 <- left_join(Level_123, level_41, by = "hh_id")

write_csv(Level_1234, "../0_Data/1_Household Data/1_India/1_Data_Clean/household_information_India.csv")

# Expenditures

level_5 <- Level5_68%>%
  select(HHID, Item_Code, Home_Produce_Value, Total_Consumption_Value, Source_Code)%>%
  rename(hh_id = HHID, item_code = Item_Code)%>%
  mutate(expenditures_selfproduced = ifelse(Source_Code !=1 & Source_Code != "", Total_Consumption_Value, 0))%>%
  mutate(expenditures              = ifelse(Source_Code == 1 | Source_Code == "", Total_Consumption_Value, 0))%>%
  remove_all_labels()%>%
  mutate(expenditures              = expenditures*365/30)%>%
  mutate(expenditures_selfproduced = expenditures_selfproduced*365/30)%>%
  select(hh_id, item_code, expenditures, expenditures_selfproduced)

level_6 <- Level6_68%>%
  select(HHID, Item_Code, Last_365days_Value)%>%
  rename(hh_id = HHID, item_code = Item_Code, expenditures = Last_365days_Value)%>%
  remove_all_labels()%>%
  mutate(expenditures_selfproduced = 0)

level_7 <- Level7_68 %>%
  select(HHID, Item_Code, Expenditure_in_Rs_last_365_days)%>%
  rename(hh_id = HHID, item_code = Item_Code, expenditures = Expenditure_in_Rs_last_365_days)%>%
  remove_all_labels()%>%
  mutate(expenditures_selfproduced = 0)

level_8 <- Level8_68 %>%
  select(HHID, Item_code, Value)%>%
  rename(hh_id = HHID, item_code = Item_code, expenditures = Value)%>%
  remove_all_labels()%>%
  mutate(expenditures_selfproduced = 0)%>%
  mutate(expenditures = expenditures*365/30)

level_9_exp <- Level9_68 %>%
  select(HHID, Item_Code, Total_expenditure_365_days)%>%
  rename(hh_id = HHID, item_code = Item_Code, expenditures = Total_expenditure_365_days)%>%
  remove_all_labels()%>%
  mutate(expenditures_selfproduced = 0)%>%
  filter(!is.na(expenditures))%>%
  filter(expenditures != 0)

expenditure_information <- level_5 %>%
  bind_rows(level_6)%>%
  bind_rows(level_7)%>%
  bind_rows(level_8)%>%
  bind_rows(level_9_exp)%>%
  rename(expenditures_year = expenditures, expenditures_sp_year = expenditures_selfproduced)%>%
  filter(expenditures_year > 0 | expenditures_sp_year > 0)

write_csv(expenditure_information, "../0_Data/1_Household Data/1_India/1_Data_Clean/expenditures_items_India.csv")

level_9 <- Level9_68 %>%
  select(HHID, Item_Code, Whether_Possesses)%>%
  rename(hh_id = HHID, item_code = Item_Code, YN = Whether_Possesses)%>%
  remove_all_labels()%>%
  mutate(YN = ifelse(YN == 1, 1, 0))%>%
  spread(key = item_code, value = YN)%>%
  select(hh_id, "560", "561", "562", "580", "581", "582", "584", "585", "586", "587", "588", "601", "602", "622", "623")
  
level_91 <- level_9 %>%
  rename(radio.01 = "560", tv.01 = "561", video.01 = "562", fan.01 = "580", ac.01 = "581", generator.01 = "582", sewing.machine.01 = "584", washing_machine.01 = "585", stove.g.01 = "586", cooker.01 = "587", refrigerator.01 = "588", motorcycle.01 = "601", car.01 = "602", computer.01 = "622", mobile.01 = "623")%>%
  right_join(select(level_1,hh_id))%>%
  mutate_at(vars(-hh_id), list(~ ifelse(is.na(.),0,.)))

write_csv(level_91, "../0_Data/1_Household Data/1_India/1_Data_Clean/appliances_0_1_India.csv")

# Codierungen ####

Cooking.Code   <- data.frame("cooking_fuel" = c(seq(1,10, by = 1)),
                             "Cooking_Fuel" = c("Coke, Coal", "Firewood and chips", "LPG", "Gobar gas", "Dung cake", "Charcoal", "Kerosene", "Electricity", "Others", "No cooking arrangement"))%>%
  write_csv(., "../0_Data/1_Household Data/1_India/2_Codes/Cooking.Code.csv")
Lighting.Code  <- data.frame("lighting_fuel" = c(1,2,3,4,5,6,9), 
                             "Lighting_Fuel" = c("Kerosene", "Other oil", "Gas", "Candle", "Electricity", "Others", "No lighting"))%>%
  write_csv(., "../0_Data/1_Household Data/1_India/2_Codes/Lighting.Code.csv")
Education.Code <- data.frame("edu_hhh" = c(seq(1,8, by = 1), 10, 11, 12, 13),
                             "Education" = c("not literate", "literate without formal schooling", "literatre through TLC", "literate - others", "below primary school", "primary school", "middle school", "secondary school", "higher secondary school", "diploma/certificate course", "graduate", "postgraduate and above"))%>%
  write_csv(., "../0_Data/1_Household Data/1_India/2_Codes/Education.Code.csv")
Religion.Code <- data.frame("religion" = c(1,2,3,4,5,6,7,9), 
                            "Religion" = c("Hinduism", "Islam", "Christianity", "Sikhism", "Jainism", "Buddhism", "Zoroastrianism", "Others"))%>%
  write_csv(., "../0_Data/1_Household Data/1_India/2_Codes/Religion.Code.csv")
Ethnicity.Code <- data.frame("ethnicity" = c(1,2,3,9),
                             "Ethnicity" = c("Scheduled Tribes", "Scheduled Castes", "Other Backward Castes", "Others"))%>%
  write_csv(., "../0_Data/1_Household Data/1_India/2_Codes/Ethnicity.Code.csv")
Provinces <- read.xlsx("T:/MSA/papers_internal/work_in_progress/Mi_Homogenized_Datainfrastructure/0_Data/1_Household Data/1_India/2_Codes/Province.xlsx", colNames = FALSE)%>%
  mutate(Province = substr(X1,1,2))%>%
  mutate(Province_India = str_sub(X1,4,-1))%>%
  mutate(Province_India = str_to_title(Province_India))%>%
  select(-X1)
Province.Code <- distinct(level_1, province)%>%
  arrange(province)%>%
  mutate(Province = province)%>%
  left_join(Provinces)%>%
  select(-Province)%>%
  rename(Province = Province_India)%>%
  write_csv(., "../0_Data/1_Household Data/1_India/2_Codes/Province.Code.csv")
Distrct.Code  <- distinct(level_1, district)%>%
  mutate(District = district)%>%
  write_csv(., "../0_Data/1_Household Data/1_India/2_Codes/District.Code.csv")
