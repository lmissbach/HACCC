if(!require("pacman")) install.packages("pacman")

p_load("countrycode", "haven", "Hmisc", "openxlsx", "rattle", "scales", "tidyverse", "sjlabelled")

options(scipen=999)

# Load Data ####

# Loading Data ####

households_v1    <- read_csv("H:/4_Action/1_South_Africa_data/Data/lcs-2014-2015-v1-csv/csv/lcs-2014-2015-households-v1.csv",       col_types = cols(UQNO = col_character())) # general information on households
assets_v2        <- read_csv("H:/4_Action/1_South_Africa_data/Data/lcs-2014-2015-v1-csv/csv/lcs-2014-2015-household-assets-v1.csv", col_types = cols(UQNO = col_character())) # generel information on asset ownership
income_v3        <- read_csv("H:/4_Action/1_South_Africa_data/Data/lcs-2014-2015-v1-csv/csv/lcs-2014-2015-personincome-v1.csv",     col_types = cols(UQNO = col_character())) # not necessarily needed as we are not interested in income
persons_v4       <- read_csv("H:/4_Action/1_South_Africa_data/Data/lcs-2014-2015-v1-csv/csv/lcs-2014-2015-persons-final-v1.csv",    col_types = cols(UQNO = col_character())) # general information on eduaction and age structure
consumption_v5   <- read_dta("H:/4_Action/1_South_Africa_data/Data/lcs-2014-2015-total-v1-stata14/lcs-2014-2017-total-v1-stata14/lcs-2014-2015-total-v1.dta") # expenditures

# Transforming Data ####

# Households 

households_v1.1 <- households_v1 %>%
  select(UQNO, Q56DRINK, Q518TOILET, Q524ELECT, Q527COOK, Q527LIGHT, Q527SPACE, hhsize,SETTLEMENT_TYPE, hholds_wgt, province_code)%>%
  rename(hh_id              = UQNO, 
         water              = Q56DRINK, 
         toilet             = Q518TOILET, 
         electricity.source = Q524ELECT, 
         cooking_fuel       = Q527COOK, 
         lighting_fuel      = Q527LIGHT, 
         heating_fuel       = Q527SPACE, 
         hh_size            = hhsize, 
         Urban              = SETTLEMENT_TYPE, 
         hh_weights         = hholds_wgt,
         province           = province_code)%>%
  mutate(urban_01           = ifelse(Urban == 1 | Urban == 2,1,0),
         electricity.access = ifelse(electricity.source == 1,1,0))%>%
  select(-Urban, -electricity.source)

# Persons

persons_v4.1 <- persons_v4%>%
  select(UQNO, Q14AGE)%>%
  mutate(children = ifelse(Q14AGE < 16, 1, 0))%>%
  mutate(adults = ifelse(Q14AGE   > 15, 1, 0))%>%
  select(-Q14AGE)%>%
  group_by(UQNO)%>%
  summarise(
    children = sum(children),
    adults = sum(adults)
  )%>%
  ungroup()

persons_edu <- persons_v4 %>%
  select(UQNO, Q16RELATION, Q13POPGROUP, Q21HIGHLEVEL, Q12SEX)%>%
  filter(Q16RELATION == 1)%>%
  select(-Q16RELATION)

persons_v4.2 <- persons_v4.1 %>%
  left_join(persons_edu, by = "UQNO")%>%
  rename(hh_id = UQNO, edu_hhh = Q21HIGHLEVEL, ethnicity = Q13POPGROUP, sex_hhh = Q12SEX)

persons_v4.3 <- persons_v4 %>%
  select(UQNO, starts_with("Q41"))%>%
  rename(hh_id = UQNO)%>%
  mutate(inc_gov_monetary = ifelse(Q41ASOCGRANT == 1 | Q41B1OLDAGE == 1 | Q41B2DISAB == 1 | Q41B3CHILD == 1 | Q41B4CAREDEP == 1 | Q41B5FOSTER == 1 | Q41B6WARVET == 1 | Q41B7INAID == 1,1,0))%>%
  group_by(hh_id)%>%
  summarise(inc_gov_monetary = max(inc_gov_monetary),
            inc_gov_cash     = 0)%>%
  ungroup()

income_v3.1 <- income_v3 %>%
  select(UQNO, PERSONNO, Coicop, Valueannualized_adj)%>%
  rename(hh_id = UQNO)%>%
  mutate(inc_gov_monetary = ifelse(Coicop %in% c(50331000, 50332000, 50332100, 50333100, 50333200, 50333300, 50333500), Valueannualized_adj,0))%>%
  group_by(hh_id)%>%
  summarise(inc_gov_monetary = sum(inc_gov_monetary),
            inc_gov_cash     = 0)%>%
  ungroup()

# remains transfers

household_information <- households_v1.1 %>%
  left_join(persons_v4.2, by = "hh_id")%>%
  left_join(income_v3.1) # more precise

# write_csv(households_v1.2, "H:/OwnCloud/Action/1_South_Africa_data/Data_Step_1/household_information_South_Africa.csv")

write_csv(household_information, "../0_Data/1_Household Data/2_South Africa/1_Data_Clean/household_information_South Africa.csv")

# Assets

assets_v2.1 <- assets_v2 %>%
  select(UQNO, Q69101RADIO, Q69104TV, Q69105DVD, Q69106FREEZER, Q69107FRIDGE, Q69108STOVE, Q69110DISH, Q69111WASHIN, Q69112DRYER, Q69113VACU, Q69114GEYSER, Q69119DESKTOP, Q69123CELL, Q69125INTERNET, Q69126VEHICLE, Q69127MOCYCLE, Q69128BICYCLE, Q69130GENERATOR)%>%
  rename(hh_id = UQNO, radio.01 = Q69101RADIO, tv.01 = Q69104TV, video.01 = Q69105DVD, freezer.01 = Q69106FREEZER, refrigerator.01 = Q69107FRIDGE, stove.e.01 = Q69108STOVE, dishwasher.01 = Q69110DISH, washing_machine.01 = Q69111WASHIN, dryer.01 = Q69112DRYER, vacuum.01 = Q69113VACU, boiler.01 = Q69114GEYSER, computer.01 = Q69119DESKTOP, mobile.01 = Q69123CELL, internet.access = Q69125INTERNET, car.01 = Q69126VEHICLE, motorcycle.01 = Q69127MOCYCLE, bicycle.01 = Q69128BICYCLE, generator.01 = Q69130GENERATOR)%>%
  mutate_at(vars(radio.01:generator.01), list(~ ifelse(. == 1,1,0)))

write_csv(assets_v2.1, "../0_Data/1_Household Data/2_South Africa/1_Data_Clean/appliances_0_1_South Africa.csv")

# Expenditures 

consumption_v5.1 <- consumption_v5 %>%
  select(UQNO, Coicop, ThirdGroup, ValueDiaryannualized_adj, ValueMainannualized_adj)%>%
  rename(hh_id = UQNO, item_code = Coicop, item_group = ThirdGroup, expenditures_diary = ValueDiaryannualized_adj, expenditures_main = ValueMainannualized_adj)

# "Value" is deleted because we find mostly income measures 

test <- consumption_v5.1 %>%
  mutate(test = ifelse((expenditures_main > 0 & expenditures_diary > 0), 1, 0))%>%
  mutate(test.2 = expenditures_main - expenditures_diary)

# Attention: For Motor Car Fuel we find differences between the diary entries and the outcomes of the main questionnaire
# Solution: Insert the average as most of the households report for their expenditures on fuel only in main questionnaire

# test.2 <- consumption_v5.1 %>%
#   filter(item_code == 7221110)

consumption_v5.2 <- consumption_v5.1 %>%
  mutate(expenditures = ifelse((expenditures_diary > 0 & expenditures_main > 0), ((expenditures_diary+expenditures_main)/2), (expenditures_diary + expenditures_main)))%>%
  select(-expenditures_diary, - expenditures_main)

# write_csv(consumption_v5.2, "expenditures_items_South_Africa.csv")

item_codes <- stack(attr(consumption_v5.2$item_code, 'labels'))%>%
  rename(item_code = values, label = ind)%>%
  arrange(item_code)%>%
  mutate(item_code_new = 1:n())

write.xlsx(item_codes, "T:/MSA/papers_internal/work_in_progress/Mi_Homogenized_Datainfrastructure/0_Data/1_Household Data/2_South Africa/3_Matching_Tables/Item_Codes_Description_South Africa.xlsx")

consumption_v5.3 <- consumption_v5.2 %>%
  select(-item_group)%>%
  group_by(hh_id, item_code)%>%
  summarise(
    expenditures_year = sum(expenditures)
  )%>%
  ungroup()%>%
  left_join(item_codes)%>%
  select(-label)%>%
  select(-item_code)%>%
  rename(item_code = item_code_new)%>%
  filter(!is.na(expenditures_year) & expenditures_year > 0)

write_csv(consumption_v5.3, "T:/MSA/papers_internal/work_in_progress/Mi_Homogenized_Datainfrastructure/0_Data/1_Household Data/2_South Africa/1_Data_Clean/Old/expenditures_items_South Africa.csv")


# write.csv(consumption_v5.3, "expenditures_items_condensed_South_Africa.csv")

# Codes ####

Ethnicity.Code <- data.frame("ethnicity" = c(1,2,3,4,5), 
                             "Ethnicity" = c("Black African", "Coloured", "Indian/Asian", "White", "Other (specify)"))%>%
  write_csv(., "../0_Data/1_Household Data/2_South Africa/2_Codes/Ethnicity.Code.csv")
Gender.Code    <- data.frame("sex_hhh" = c(1,2),
                             "Gender"  = c("Male", "Female"))%>%
  write_csv(., "../0_Data/1_Household Data/2_South Africa/2_Codes/Gender.Code.csv")
Water.Code     <- data.frame("water" = c(seq(1,13, by = 1), 99), 
                             "Water" = c("Piped Water in-house", "Piped Water Yard", "Borehole Yard", "Rain Water Tank", "Neighbours Tap", "Public Tap", "Water carrier", "Borehole outside Yard", "River", "Dam", "Well", "Spring", "Other", "Unspecified"))%>%
  write_csv(., "../0_Data/1_Household Data/2_South Africa/2_Codes/Water.Code.csv")
Toilet.Code    <- data.frame("toilet" = c(seq(1,10, by = 1), 99), 
                             "Toilet" = c("Flush toilet (sewerage system)", "Flush toilet (tank)", "chemical toilet", "pit latrine / ventilation pipe", "pit latrine / without ventilation pipe", "bucket toilet (municipality)", "bucket toilet (household)", "ecological", "none", "Other", "unspecified"))%>%
  write_csv(., "../0_Data/1_Household Data/2_South Africa/2_Codes/Toilet.Code.csv")
Cooking.Code   <- data.frame("cooking_fuel" = c(seq(1, 11, by = 1), 99), 
                             "Cooking_Fuel" = c("Electricity", "Other source of electricity", "Gas", "Paraffin", "Wood", "Coal", "Candles", "Animal dung", "Solar energy", "Other, specify", "None", "Unspecified"))%>%
  write_csv(., "../0_Data/1_Household Data/2_South Africa/2_Codes/Cooking.Code.csv")
Lighting.Code  <- data.frame("lighting_fuel" = c(1,2,3,4,7,9,10,99), 
                             "Lighting_Fuel" = c("Electricity", "Other Source of Electricitcy", "Gas", "Paraffin", "Candles", "Solar Energy", "Other", "Unspecified"))%>%
  write_csv(., "../0_Data/1_Household Data/2_South Africa/2_Codes/Lighting.Code.csv")
Heating.Code   <- data.frame("heating_fuel" = c(seq(1, 11, by = 1), 99), 
                             "Heating_Fuel" = c("Electricity", "Other source of electricity", "Gas", "Paraffin", "Wood", "Coal", "Candles", "Animal dung", "Solar energy", "Other, specify", "None", "Unspecified"))%>%
  write_csv(., "../0_Data/1_Household Data/2_South Africa/2_Codes/Heating.Code.csv")
Education.Code <- data.frame("edu_hhh" = c(seq(1, 32, by = 1), 98, 99),
                             "Education" = c("Grade R", "Grade 1", "Grade 2", "Grade 3", "Grade 4", "Grade 5", "Grade 6", "Grade 7", "Grade 8", "Grade 9", "Grade 10", "Grade 11", "Grade 12", "Grade 12 (exemption)", "NTC 1 Level 2",
                                                                                      "NTC 2 Level 3", "NTC 3 Level 4", "N4/NTC 4", "N5/NTC 5", "N6/NTC 6", "Certificate with less than Grade 12", "Diploma with less than Grade 12", "Certificate with Grade 12",
                                                                                      "Diploma with Grade12", "Higher Diploma", "Post Higher Diploma", "Bachelors Degree", "Bachelors Degree and post-graduate diploma", "Honours Degree", "Higher Degree (Masters, Doctorate)",
                                                                                      "Other, specify", "Don't know", "Not applicable", "Unspecified"))%>%
  write_csv(., "../0_Data/1_Household Data/2_South Africa/2_Codes/Education.Code.csv")
Province.Code <- data.frame("province" = c(seq(1,9,1)),
                            "Province" = c("Western Cape", "Eastern Cape", "Northern Cape",
                                           "Free State", "Kwazulu Natal", "North West", "Gauteng", "Mpumalanga", "Limpopo"))%>%
  write_csv(., "../0_Data/1_Household Data/2_South Africa/2_Codes/Province.Code.csv")
