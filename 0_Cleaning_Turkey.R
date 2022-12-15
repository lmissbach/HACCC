if(!require("pacman")) install.packages("pacman")

p_load("countrycode", "haven", "Hmisc", "openxlsx", "rattle", "scales", "tidyverse", "readxl")

options(scipen=999)

# Loading Data ####

Consumption <- read.csv2("T:/MSA/papers_internal/work_in_progress/HH Surveys Paper/Turkey_analysis/Data/Turkey_2013/consumption_dataset_2013_(tuketim_cd_2013).csv")
Persons     <- read.csv2("T:/MSA/papers_internal/work_in_progress/HH Surveys Paper/Turkey_analysis/Data/Turkey_2013/individual_dataset_2013_(fert_cd_2013).csv")
Household   <- read.csv2("T:/MSA/papers_internal/work_in_progress/HH Surveys Paper/Turkey_analysis/Data/Turkey_2013/household_dataset_2013_(hane_cd_2013).csv")

# 10060 households

# Transforming Data ####

Consumption_v1 <- Consumption %>%
  rename(hh_id = BULTEN, type = TABNO, item_code = HBS_KOD, expenditures = DEGERD)%>%
  mutate(expenditures = as.numeric(expenditures))%>%
  mutate(expenditures = expenditures*12)%>% # expenditures have been monthly before
  mutate(type_0 = ifelse(type == 1, "expenditures_year", "expenditures_sp_year"))%>%
  group_by(hh_id, type_0, item_code)%>%
  summarise(value = sum(expenditures))%>%
  ungroup()%>%
  pivot_wider(names_from = "type_0", values_from = "value")%>%
  mutate_at(vars(expenditures_year, expenditures_sp_year), list(~ ifelse(is.na(.),0,.)))
  
write_csv(Consumption_v1, "../0_Data/1_Household Data/1_Turkey/1_Data_Clean/expenditures_items_Turkey.csv")

# Household information

Persons_v1 <- Persons %>%
  select(BULTEN, YAS, YAKINLIK, EGITIM, CINSIYET, SEKKOD)%>%
  rename(hh_id = BULTEN, age = YAS, household.head = YAKINLIK, edu_hhh = EGITIM, sex_hhh = CINSIYET, ind_hhh = SEKKOD)

Persons_v1.1 <- Persons_v1 %>%
  select(hh_id, age)%>%
  mutate(adults = ifelse(age >= 18, 1, 0))%>%
  mutate(children = ifelse(age < 18, 1, 0))%>%
  select(-age)%>%
  group_by(hh_id)%>%
  summarise(
    adults = sum(adults),
    children = sum(children)
  )%>%
  ungroup()

Persons_edu <- Persons_v1 %>%
  select(hh_id, household.head, edu_hhh, sex_hhh, ind_hhh)%>%
  filter(household.head == 0)%>% # 0 refers to household head
  select(-household.head)

Persons_v1.2 <- Persons_v1.1 %>%
  left_join(Persons_edu, by = "hh_id")

Persons_3 <- Persons %>%
  rename(hh_id = BULTEN)%>%
  select(hh_id, EMEKL_YL, YASLI_YL, SOSY_YL, DUL_YL, HASTA_YL, GAZI_YL, BURS_YL, ISSIZ_YL, DDESTEK_YL)%>%
  mutate_at(vars(-hh_id), list(~ ifelse(is.na(.),0,.)))%>%
  mutate(inc_gov_cash = SOSY_YL,
         inc_gov_monetary = EMEKL_YL + YASLI_YL + DUL_YL + HASTA_YL + GAZI_YL + BURS_YL + ISSIZ_YL + DDESTEK_YL)%>%
  group_by(hh_id)%>%
  summarise(inc_gov_cash     = sum(inc_gov_cash),
            inc_gov_monetary = sum(inc_gov_monetary))%>%
  ungroup()

Household_v1 <- Household %>%
  select(BULTEN, YAKIT_1, YAKIT_MUT, TUVALET, SU_SIS, JENRATOR, CEPTEL, BILGISAY, INTERN, LCDTELE, BUZDOLAB, DERINDON, BULASIK, M_FIRIN, CAMASIR, KURUTMA, HALIYIKA, KLIMA, OTOADET, MOTOADET, HHB, HARCAMA, KIRKNTKD, faktor)%>%
  rename(hh_id = BULTEN, 
         heating_fuel = YAKIT_1,
         cooking_fuel = YAKIT_MUT,
         toilet = TUVALET,
         water = SU_SIS,
         generator.01 = JENRATOR, mobile.01 = CEPTEL, computer.01 = BILGISAY, tv.01 = LCDTELE, refrigerator.01 = BUZDOLAB, freezer.01 = DERINDON, dishwasher.01 = BULASIK, microwave.01 = M_FIRIN, washing_machine.01 = CAMASIR, dryer.01 = KURUTMA, vacuum.01 = HALIYIKA, ac.01 = KLIMA, car.01 = OTOADET, motorcycle.01 = MOTOADET, 
         hh_size = HHB, urban = KIRKNTKD, hh_weights = faktor, expenditures_0 = HARCAMA)

# We exclude OTOADET2 and OTOADET3 because they seem to be linked to OTOADET..  

Household_v1.1 <- Household_v1 %>%
  select(hh_id, heating_fuel, cooking_fuel, toilet, water, hh_size, expenditures_0, hh_weights, urban)

household_information <- Persons_v1.2 %>%
  left_join(Household_v1.1, by = "hh_id")%>%
  select(hh_id, adults, children, hh_size, hh_weights, urban, everything())%>%
  mutate(urban_01 = ifelse(urban == 2,1,0))%>%
  select(-urban)%>%
  left_join(Persons_3)%>%
  select(-expenditures_0)

write_csv(household_information, "../0_Data/1_Household Data/1_Turkey/1_Data_Clean/household_information_Turkey.csv")

Appliances_v1.2 <- Household_v1 %>%
  select(hh_id, ends_with(".01"))%>%
  mutate_at(vars(-hh_id), list(~ ifelse(. > 0,1,0)))

write_csv(Appliances_v1.2, "../0_Data/1_Household Data/1_Turkey/1_Data_Clean/appliances_0_1_Turkey.csv")

# Creating Codes ####

Education.Code <- data.frame("edu_hhh" = c(seq(1,11, by = 1)),
                             "Education" = c("Illiterate", "Literate, not completed a school", "Primary school", "Primary education", "Secondary School", "Junior Vocational High School", "High School", "Senior Vocational High School", "2-3 year College", "4-year College or Univeristy", "Post Graduate/PhD"))%>%
  write_csv(., "../0_Data/1_Household Data/1_Turkey/2_Codes/Education.Code.csv")
Heating.Code <- data.frame("heating_fuel" = c(1,2,3,4,5,6), 
                           "Heating_Fuel" = c("Wood", "Coal", "Natural Gas", "Electric", "Dried cow dung", "Other"))%>%
  write_csv(., "../0_Data/1_Household Data/1_Turkey/2_Codes/Heating.Code.csv")
Cooking.Code <- data.frame("cooking_fuel" = c(1,2,3,4,5,6,7,8),
                           "Cooking_Fuel" = c("Wood", "Coal", "Natural Gas", "LPG", "Electric", "Dried cow dung", "Solar energy system", "Other"))%>%
  write_csv(., "../0_Data/1_Household Data/1_Turkey/2_Codes/Cooking.Code.csv")
Water.Code <- data.frame("water" = c(0,1),
                         "Water" = c("Not available", "available"))%>%
  write_csv(., "../0_Data/1_Household Data/1_Turkey/2_Codes/Water.Code.csv")
Toilet.Code <- data.frame("toilet" = c(0,1),
                          "Toilet" = c("Not available","Available"))%>%
  write_csv(., "../0_Data/1_Household Data/1_Turkey/2_Codes/Toilet.Code.csv")
Gender.Code <- data.frame("sex_hhh" = c(1,2),
                          "Gender" = c("Male", "Female"))%>%
  write_csv(., "../0_Data/1_Household Data/1_Turkey/2_Codes/Gender.Code.csv")
Industry.Code <- data.frame("ind_hhh" = c(seq(1,18,1)),
                            "Industry" = c("1. Agriculture, hunting, forestry",
                            "2. Fishery","3. Mining and quarry","4. Manufacturing Industry","5. Electricity, gas and water",
                            "6. Construction and public works","7. Wholesale and retail business, motor vehicles, repair of motorcycles",
                            "8. Hotel and restaurants","9. Transportation, communications and storage services","10. Financial brokerage services",
                            "11. Real estate agency, rentals and business activities","12. Public management and defense, mandatory social security",
                            "13. Administrative and support service activities","14. Public administration and defense, compulsory social security",
                            "15. Education","16. Human health and social work activities","17. Arts, entertainment and recreation","18. Other social, comminity and personal service activities"))%>%
  write_csv(., "../0_Data/1_Household Data/1_Turkey/2_Codes/Industry.Code.csv")

