if(!require("pacman")) install.packages("pacman")

p_load("arrow","haven", "Hmisc", "openxlsx", "rattle", "scales", "tidyverse", "sjlabelled", "tidymodels")

options(scipen=999)

# Loading Data ####

data_1 <- read_delim("../0_Data/1_Household Data/1_India/1_Data_Raw/hces22_lvl_01.txt", delim = ",", col_names = FALSE)
data_2 <- read_delim("../0_Data/1_Household Data/1_India/1_Data_Raw/hces22_lvl_02.txt", delim = ",", col_names = FALSE)
data_3 <- read_delim("../0_Data/1_Household Data/1_India/1_Data_Raw/hces22_lvl_03.txt", delim = ",", col_names = FALSE)
data_4 <- read_delim("../0_Data/1_Household Data/1_India/1_Data_Raw/hces22_lvl_04.txt", delim = ",", col_names = FALSE)
data_5 <- read_delim("../0_Data/1_Household Data/1_India/1_Data_Raw/hces22_lvl_05/hces22_lvl_05/hces22_lvl_05.txt", delim = ",", col_names = FALSE)
data_6 <- read_delim("../0_Data/1_Household Data/1_India/1_Data_Raw/hces22_lvl_06.txt", delim = ",", col_names = FALSE)
data_7 <- read_delim("../0_Data/1_Household Data/1_India/1_Data_Raw/hces22_lvl_07.txt", delim = ",", col_names = FALSE)
data_8 <- read_delim("../0_Data/1_Household Data/1_India/1_Data_Raw/hces22_lvl_08.txt", delim = ",", col_names = FALSE)
data_9 <- read_delim("../0_Data/1_Household Data/1_India/1_Data_Raw/hces22_lvl_09/hces22_lvl_09.txt", delim = ",", col_names = FALSE)
data_10 <- read_delim("../0_Data/1_Household Data/1_India/1_Data_Raw/hces22_lvl_10.txt", delim = ",", col_names = FALSE)
data_11 <- read_delim("../0_Data/1_Household Data/1_India/1_Data_Raw/hces22_lvl_11.txt", delim = ",", col_names = FALSE)
data_12 <- read_delim("../0_Data/1_Household Data/1_India/1_Data_Raw/hces22_lvl_12/hces22_lvl_12.txt", delim = ",", col_names = FALSE)
data_13 <- read_delim("../0_Data/1_Household Data/1_India/1_Data_Raw/hces22_lvl_13/hces22_lvl_13.txt", delim = ",", col_names = FALSE)
data_14 <- read_delim("../0_Data/1_Household Data/1_India/1_Data_Raw/hces22_lvl_14/hces22_lvl_14.txt", delim = ",", col_names = FALSE)
data_15 <- read_delim("../0_Data/1_Household Data/1_India/1_Data_Raw/hces22_lvl_15.txt", delim = ",", col_names = FALSE)

# Transform data to be readable

data_1.1 <- data_1 %>%
  mutate(common_ID             = str_sub(X1, 1,38),
         survey_name           = str_sub(X1, 1, 4),
         year                  = str_sub(X1, 5, 8),
         fsu_serial_no         = str_sub(X1, 9, 13),
         sector                = str_sub(X1, 14, 14),
         state                 = str_sub(X1, 15, 16),
         nss_region            = str_sub(X1, 17, 19),
         district              = str_sub(X1, 20, 21),
         stratum               = str_sub(X1, 22, 23),
         sub_stratum           = str_sub(X1, 24, 25),
         panel                 = str_sub(X1, 25, 27),
         subsample             = str_sub(X1, 28, 28),
         fod_subregion         = str_sub(X1, 29, 32),
         sample_su_num         = str_sub(X1, 33, 34),
         sample_subdiv_num     = str_sub(X1, 35, 35),
         sec_stage_stratum_num = str_sub(X1, 36, 36),
         sample_hhld_num       = str_sub(X1, 37, 38),
         questionnaire_num     = str_sub(X1, 39, 39),
         level                 = str_sub(X1, 40, 41),
         svy_code              = str_sub(X1, 42, 42),
         subcode_reason        = str_sub(X1, 43, 43),
         multiplier            = str_sub(X1, 44, 58))%>%
  select(-X1)

write_parquet(data_1.1, "../0_Data/1_Household Data/1_India/1_Data_Raw/data_1.parquet")

rm(data_1, data_1.1)

data_2.1 <- data_2 %>%
  mutate(
    common_id         = str_sub(X1,  1, 38),
    questionnaire_num = str_sub(X1, 39, 39),
    level             = str_sub(X1, 40, 41),
    person_id         = str_sub(X1, 42, 43),
    rel_to_hoh        = str_sub(X1, 44, 44),
    gender            = str_sub(X1, 45, 45),
    age               = str_sub(X1, 46, 48),
    marital_status    = str_sub(X1, 49, 49),
    edu_level         = str_sub(X1, 50, 51),
    edu_years         = str_sub(X1, 52, 53),
    used_internet     = str_sub(X1, 54, 54),
    days_outside_home = str_sub(X1, 55, 56),
    meals_daily       = str_sub(X1, 57, 57),
    meals_school      = str_sub(X1, 58, 59),
    meals_employer    = str_sub(X1, 60, 61),
    meals_others      = str_sub(X1, 62, 63),
    meals_payment     = str_sub(X1, 64, 65),
    meals_home        = str_sub(X1, 66, 67),
    member_status     = str_sub(X1, 68, 68),
    original_member   = str_sub(X1, 69, 69),
    multiplier        = str_sub(X1, 70, 84)
  )%>%
  select(-X1)

write_parquet(data_2.1, "../0_Data/1_Household Data/1_India/1_Data_Raw/data_2.parquet")

rm(data_2, data_2.1)

data_3.1 <- data_3 %>%
  mutate(
    common_id              = str_sub(X1,  1, 38),
    questionnaire_num      = str_sub(X1, 39, 39),
    level                  = str_sub(X1, 40, 41),
    hh_size                = str_sub(X1, 42, 43),
    employed_annual        = str_sub(X1, 44, 44),
    nco_2015               = str_sub(X1, 54, 47),
    nic_2008               = str_sub(X1, 48, 52),
    num_activities_income  = str_sub(X1, 53, 53),
    hasincome_selfemp_agri = str_sub(X1, 54, 54),
    hasincome_wage_agri    = str_sub(X1, 55, 55),
    hasincome_casual_agri  = str_sub(X1, 56, 56),
    hh_type                = str_sub(X1, 57, 57),
    hoh_religion           = str_sub(X1, 58, 58),
    caste                  = str_sub(X1, 59, 59),
    has_land               = str_sub(X1, 60, 60),
    land_type              = str_sub(X1, 61, 61),
    land_area              = str_sub(X1, 62, 70),
    has_house              = str_sub(X1, 71, 71),
    type_house             = str_sub(X1, 72, 72),
    material_wall          = str_sub(X1, 73, 73),
    material_roof          = str_sub(X1, 74, 74),
    material_floor         = str_sub(X1, 75, 75),
    source_cooking         = str_sub(X1, 76, 77),
    source_lighting        = str_sub(X1, 78, 78),
    source_water           = str_sub(X1, 79, 80),
    time_water             = str_sub(X1, 81, 83),
    level_access_latrine   = str_sub(X1, 84, 84),
    type_latrine           = str_sub(X1, 85, 86),
    type_rationcard        = str_sub(X1, 87, 87),
    is_rent                = str_sub(X1, 88, 88),
    has_pmgky              = str_sub(X1, 89, 89),
    has_young_died         = str_sub(X1, 90, 90),
    num_young_died         = str_sub(X1, 91, 92),
    multiplier             = str_sub(X1, 93, 107)
  )%>%
  select(-X1)

write_parquet(data_3.1, "../0_Data/1_Household Data/1_India/1_Data_Raw/data_3.parquet")

rm(data_3, data_3.1)

data_4.1 <- data_4 %>%
  mutate(
    common_id              = str_sub(X1,  1, 38),
    questionnaire_num      = str_sub(X1, 39, 39),
    level                  = str_sub(X1, 40, 41),
    used_ration            = str_sub(X1, 42, 42),
    procured_rice          = str_sub(X1, 43, 43),
    procured_wheat         = str_sub(X1, 44, 44),
    procured_grain         = str_sub(X1, 45, 45),
    procured_sugar         = str_sub(X1, 46, 46),
    procured_pulses        = str_sub(X1, 47, 47),
    procured_edible_oil    = str_sub(X1, 48, 48),
    procured_other         = str_sub(X1, 49, 49),
    online_groceries       = str_sub(X1, 50, 50),
    online_milkprod        = str_sub(X1, 51, 51),
    online_veg             = str_sub(X1, 52, 52),
    online_fruits          = str_sub(X1, 53, 53),
    online_dryfruits       = str_sub(X1, 54, 54),
    online_eggfishmeat     = str_sub(X1, 55, 55),
    online_processed_served= str_sub(X1, 56, 56),
    online_processed_packed= str_sub(X1, 57, 57),
    online_other           = str_sub(X1, 58, 58),
    performed_ceremony     = str_sub(X1, 59, 59),
    meals_nonhh            = str_sub(X1, 60, 63),
    multiplier             = str_sub(X1, 64, 78)
  )%>%
  select(-X1)

write_parquet(data_4.1, "../0_Data/1_Household Data/1_India/1_Data_Raw/data_4.parquet")

rm(data_4, data_4.1)

data_5.1 <- data_5 %>%
  mutate(
    common_id              = str_sub(X1,  1, 38),
    questionnaire_num      = str_sub(X1, 39, 39),
    level                  = str_sub(X1, 40, 41),
    item_code              = str_sub(X1, 42, 44),
    cons_home_qty          = str_sub(X1, 45, 54),
    cons_home_value        = str_sub(X1, 55, 62),
    cons_total_qty         = str_sub(X1, 63, 72),
    cons_total_value       = str_sub(X1, 73, 80),
    source                 = str_sub(X1, 81, 81),
    multiplier             = str_sub(X1, 82, 96)
  )%>%
  select(-X1)

write_parquet(data_5.1, "../0_Data/1_Household Data/1_India/1_Data_Raw/data_5.parquet")

rm(data_5, data_5.1)

data_6.1 <- data_6 %>%
  mutate(
    common_id              = str_sub(X1,  1, 38),
    questionnaire_num      = str_sub(X1, 39, 39),
    level                  = str_sub(X1, 40, 41),
    item_code              = str_sub(X1, 42, 44),
    cons_total_qty         = str_sub(X1, 45, 54),
    cons_total_value       = str_sub(X1, 55, 62),
    source                 = str_sub(X1, 63, 63),
    multiplier             = str_sub(X1, 64, 78)
  )%>%
  select(-X1)

write_parquet(data_6.1, "../0_Data/1_Household Data/1_India/1_Data_Raw/data_6.parquet")

rm(data_6, data_6.1)

data_7.1 <- data_7 %>%
  mutate(
    common_id                 = str_sub(X1,  1, 38),
    questionnaire_num         = str_sub(X1, 39, 39),
    level                     = str_sub(X1, 40, 41),
    procured_kerosene_ration  = str_sub(X1, 42, 42),
    receieved_subsidy_lpg     = str_sub(X1, 43, 43),
    num_subsidized_lpg        = str_sub(X1, 44, 45),
    received_free_electricity = str_sub(X1, 46, 46),
    is_hhmem_edu              = str_sub(X1, 47, 47),
    num_hhmem_publicedu       = str_sub(X1, 48, 50),
    num_hhmem_privedu         = str_sub(X1, 51, 53),
    recieved_free_textbook    = str_sub(X1, 54, 54),
    num_free_textbook         = str_sub(X1, 55, 57),
    recieved_free_stationary  = str_sub(X1, 58, 58),
    num_free_stationary       = str_sub(X1, 59, 61),
    recieved_free_schoolbag   = str_sub(X1, 62, 62),
    num_free_schoolbag        = str_sub(X1, 63, 65),
    recieved_free_oth         = str_sub(X1, 66, 66),
    num_free_oth              = str_sub(X1, 67, 69),
    recieved_fee_waiver       = str_sub(X1, 70, 70),
    num_hhmem_waiver          = str_sub(X1, 71, 72),
    is_hhmem_pmjay            = str_sub(X1, 73, 73),
    num_hhmem_pmjay           = str_sub(X1, 74, 75),
    is_hospitalization        = str_sub(X1, 76, 76),
    is_benefit_healthscheme   = str_sub(X1, 77, 77),
    num_benefit_healthscheme  = str_sub(X1, 78, 79),
    amt_benefit_healthscheme  = str_sub(X1, 80, 87),
    online_fuel               = str_sub(X1, 88, 88),
    online_toilet             = str_sub(X1, 89, 89),
    online_edu                = str_sub(X1, 90, 90),
    online_medicine           = str_sub(X1, 91, 91),
    online_services           = str_sub(X1, 92, 92),
    has_internet              = str_sub(X1, 93, 93),
    multiplier                = str_sub(X1, 94, 108)
  ) %>%
  select(-X1)

write_parquet(data_7.1, "../0_Data/1_Household Data/1_India/1_Data_Raw/data_7.parquet")

rm(data_7, data_7.1)

data_8.1 <- data_8 %>%
  mutate(common_id         = substr(X1,1,38),
         questionnaire_num = substr(X1,39,39),
         level             = substr(X1,40,41),
         item_code         = substr(X1,42,44),
         cons_home_qty     = substr(X1,45,54),
         cons_home_value   = substr(X1,55,62),
         cons_total_qty    = substr(X1,63,72),
         cons_total_value  = substr(X1,73,80),
         source            = substr(X1,81,81),
         multiplier        = substr(X1,82,96))%>%
  select(-X1)

write_parquet(data_8.1, "../0_Data/1_Household Data/1_India/1_Data_Raw/data_8.parquet")

rm(data_8, data_8.1)

data_9.1 <- data_9 %>%
  mutate(common_id         = substr(X1,1,38),
         questionnaire_num = substr(X1,39,39),
         level             = substr(X1,40,41),
         item_code         = substr(X1,42,44),
         cons_total_value  = substr(X1,45,52),
         multiplier        = substr(X1,53,67)
  )%>%
  select(-X1)

write_parquet(data_9.1, "../0_Data/1_Household Data/1_India/1_Data_Raw/data_9.parquet")

rm(data_9, data_9.1)

data_10.1 <- data_10 %>%
  mutate(common_id         = substr(X1,1,38),
         questionnaire_num = substr(X1,39,39),
         level             = substr(X1,40,41),
         item_code         = substr(X1,42,44),
         cons_home_qty     = substr(X1,45,54),
         cons_home_value   = substr(X1,55,62),
         cons_total_qty    = substr(X1,63,72),
         cons_total_value  = substr(X1,73,80),
         source            = substr(X1,81,81),
         multiplier        = substr(X1,82,96)
  )%>%
  select(-X1)

write_parquet(data_10.1, "../0_Data/1_Household Data/1_India/1_Data_Raw/data_10.parquet")

rm(data_10, data_10.1)

data_11.1 <- data_11 %>%
  mutate(common_id               = substr(X1,1,38),
         questionnaire_num       = substr(X1,39,39),
         level                   = substr(X1,40,41),
         online_clothing         = substr(X1,42,42),
         online_footwear         = substr(X1,43,43),
         online_furniture        = substr(X1,44,44),
         online_mobile           = substr(X1,45,45),
         online_personal_goods   = substr(X1,46,46),
         online_rec              = substr(X1,47,47),
         online_cooking          = substr(X1,48,48),
         online_crockery         = substr(X1,49,49),
         online_sports           = substr(X1,50,50),
         online_medical          = substr(X1,51,51),
         online_bedding          = substr(X1,52,52),
         receieved_free_laptop   = substr(X1,53,53),
         num_free_laptop         = substr(X1,54,56),
         receieved_free_tab      = substr(X1,57,57),
         num_free_tab            = substr(X1,58,60),
         receieved_free_mobile   = substr(X1,61,61),
         num_free_mobile         = substr(X1,62,64),
         receieved_free_bicycle  = substr(X1,65,65),
         num_free_bicycle        = substr(X1,66,68),
         receieved_free_bike     = substr(X1,69,69),
         num_free_bike           = substr(X1,70,72),
         receieved_free_clothing = substr(X1,73,73),
         num_free_clothing       = substr(X1,74,76),
         receieved_free_footwear = substr(X1,77,77),
         num_free_footwear       = substr(X1,78,80),
         receieved_free_oth      = substr(X1,81,81),
         num_free_oth            = substr(X1,82,84),
         has_tv                  = substr(X1,85,85),
         has_radio               = substr(X1,86,86),
         has_laptop              = substr(X1,87,87),
         has_mobile              = substr(X1,88,88),
         has_bicycle             = substr(X1,89,89),
         has_bike                = substr(X1,90,90),
         has_car                 = substr(X1,91,91),
         has_truck               = substr(X1,92,92),
         has_animalcart          = substr(X1,93,93),
         has_fridge              = substr(X1,94,94),
         has_washingmachine      = substr(X1,95,95),
         has_ac                  = substr(X1,96,96),
         type_tv                 = substr(X1,97,97),
         multiplier              = substr(X1,98,112))%>%
  select(-X1)

write_parquet(data_11.1, "../0_Data/1_Household Data/1_India/1_Data_Raw/data_11.parquet")

rm(data_11, data_11.1)

data_12.1 <- data_12 %>%
   mutate(common_id         = substr(X1,1,38),
          questionnaire_num = substr(X1,39,39),
          level             = substr(X1,40,41),
          item_code         = substr(X1,42,44),
          cons_total_qty    = substr(X1,45,54),
          cons_total_value  = substr(X1,55,62),
          multiplier        = substr(X1,63,77))%>%
  select(-X1)

write_parquet(data_12.1, "../0_Data/1_Household Data/1_India/1_Data_Raw/data_12.parquet")

rm(data_12, data_12.1)

data_13.1 <- data_13 %>%#
  mutate(common_id               = substr(X1,1,38),
         questionnaire_num       = substr(X1,39,39),
         level                   = substr(X1,40,41),
         item_code               = substr(X1,42,44),
         num_firsthand_purchase  = substr(X1,45,47),
         is_purchased_onhire     = substr(X1,48,48),
         val_firsthand_purchase  = substr(X1,49,56),
         repair_cost             = substr(X1,57,64),
         num_secondhand_purchase = substr(X1,65,67),
         val_secondhand_purchase = substr(X1,68,75),
         tot_expenditure         = substr(X1,76,83),
         multiplier              = substr(X1,84,98))%>%
  select(-X1)

write_parquet(data_13.1, "../0_Data/1_Household Data/1_India/1_Data_Raw/data_13.parquet")

rm(data_13, data_13.1)

data_14.1 <- data_14 %>%
  mutate(common_id         = substr(X1,1,38),
         questionnaire_num = substr(X1,39,39),
         level             = substr(X1,40,41),
         section           = substr(X1,42,46),
         item_code         = substr(X1,47,49),
         value             = substr(X1,50,59),
         multiplier        = substr(X1,60,74))%>%
  select(-X1)

write_parquet(data_14.1, "../0_Data/1_Household Data/1_India/1_Data_Raw/data_14.parquet")

rm(data_14, data_14.1)

data_15.1 <- data_15 %>%
  mutate(common_id              = substr(X1,1,38),
         questionnaire_num      = substr(X1,39,39),
         level                  = substr(X1,40,41),
         section                = substr(X1,42,43),
         time_taken             = substr(X1,44,46),
         hh_usual_cons_exp_mnth = substr(X1,47,54),
         tot_online_exp         = substr(X1,55,62),
         informant_code         = substr(X1,63,64),
         response_code          = substr(X1,65,65),
         hh_size                = substr(X1,66,68),
         multiplier             = substr(X1,69,83))%>%
  select(-X1)

write_parquet(data_15.1, "../0_Data/1_Household Data/1_India/1_Data_Raw/data_15.parquet")

rm(data_15, data_15.1)

# Load prepped data ####

data_1  <- read_parquet("../0_Data/1_Household Data/1_India/1_Data_Raw/data_1.parquet")
data_2  <- read_parquet("../0_Data/1_Household Data/1_India/1_Data_Raw/data_2.parquet")
data_3  <- read_parquet("../0_Data/1_Household Data/1_India/1_Data_Raw/data_3.parquet")
# data_4  <- read_parquet("../0_Data/1_Household Data/1_India/1_Data_Raw/data_4.parquet") # not used
data_5  <- read_parquet("../0_Data/1_Household Data/1_India/1_Data_Raw/data_5.parquet")
data_6  <- read_parquet("../0_Data/1_Household Data/1_India/1_Data_Raw/data_6.parquet")
data_7  <- read_parquet("../0_Data/1_Household Data/1_India/1_Data_Raw/data_7.parquet")
data_8  <- read_parquet("../0_Data/1_Household Data/1_India/1_Data_Raw/data_8.parquet")
data_9  <- read_parquet("../0_Data/1_Household Data/1_India/1_Data_Raw/data_9.parquet")
data_10 <- read_parquet("../0_Data/1_Household Data/1_India/1_Data_Raw/data_10.parquet")
data_11 <- read_parquet("../0_Data/1_Household Data/1_India/1_Data_Raw/data_11.parquet")
data_12 <- read_parquet("../0_Data/1_Household Data/1_India/1_Data_Raw/data_12.parquet")
data_13 <- read_parquet("../0_Data/1_Household Data/1_India/1_Data_Raw/data_13.parquet")
# data_14 <- read_parquet("../0_Data/1_Household Data/1_India/1_Data_Raw/data_14.parquet") # not used
# data_15 <- read_parquet("../0_Data/1_Household Data/1_India/1_Data_Raw/data_15.parquet") # not used

hh_id_0 <- distinct(data_1, common_ID)%>%
  mutate(hh_id = seq(1,261746,1))

# Transform data ####

data_1.1 <- data_1 %>%
  left_join(hh_id_0)%>%
  select(hh_id, everything(),-common_ID)%>%
  rename(hh_weights = multiplier, urban = sector, province = state, province_1 = nss_region, district = district)%>%
  mutate(urban_01 = ifelse(urban == 1,0,1))%>%
  select(hh_id, hh_weights, urban_01, province, province_1, district)

rm(data_1)

data_2.1 <- data_2 %>%
  rename(common_ID = common_id)%>%
  left_join(hh_id_0)%>%
  select(hh_id, everything(),-common_ID)%>%
  filter(rel_to_hoh == 1)%>%
  rename(sex_hhh = gender, age_hhh = age, edu_hhh = edu_level)%>%
  select(hh_id, sex_hhh, age_hhh, edu_hhh)

data_2.2 <- data_2 %>% 
  rename(common_ID = common_id)%>%
  left_join(hh_id_0)%>%
  select(hh_id, everything(),-common_ID)%>%
  mutate(adults = ifelse(age > 15,1,0),
         children = ifelse(age < 16,1,0))%>%
  group_by(hh_id)%>%
  summarise(adults = sum(adults),
            children = sum(children),
            hh_size_test = n())%>%
  ungroup()
  
rm(data_2)

data_3.1 <- data_3 %>%
  rename(common_ID = common_id)%>%
  left_join(hh_id_0)%>%
  select(hh_id, everything(),-common_ID)%>%
  rename(occupation = nco_2015, ind_hhh = nic_2008, religion = hoh_religion, ethnicity = caste, renting = type_house,
         wall = material_wall, roof = material_roof, floor = material_floor, cooking_fuel = source_cooking, lighting_fuel = source_lighting)%>%
  select(hh_id, hh_size, occupation, ind_hhh, religion, ethnicity, renting, wall, roof, floor, cooking_fuel, lighting_fuel)

data_7.1 <- data_7 %>%
  rename(common_ID = common_id)%>%
  left_join(hh_id_0)%>%
  select(hh_id, everything(),-common_ID)%>%
  rename(kerosene_ration = procured_kerosene_ration, lpg_subsidy = receieved_subsidy_lpg, electricity_free = received_free_electricity)%>%
  select(hh_id, kerosene_ration, lpg_subsidy, electricity_free)

household_information <- data_1.1 %>%
  left_join(data_2.1)%>%
  left_join(data_2.2)%>%
  left_join(data_3.1)%>%
  left_join(data_7.1)%>%
  select(-hh_size_test)

write_parquet(household_information, "../0_Data/1_Household Data/1_India/1_Data_Clean/household_information_India.parquet")

rm(data_3, data_2.1, data_2.2, data_1.1, data_3.1, data_7, data_7.1)

data_11.1 <- data_11 %>%
  rename(common_ID = common_id)%>%
  left_join(hh_id_0)%>%
  select(hh_id, everything(),-common_ID)%>%
  rename(tv.01 = has_tv, motorcycle.01 = has_bike, car.01 = has_car, car.01b = has_truck, refrigerator.01 = has_fridge, washing_machine.01 = has_washingmachine, ac.01 = has_ac)%>%
  select(hh_id, ends_with(".01"), car.01b)%>%
  mutate_all(~ ifelse(. == " ",0,as.numeric(.)))

write_parquet(data_11.1, "../0_Data/1_Household Data/1_India/1_Data_Clean/appliances_0_1_India.parquet")

rm(data_11, data_11.1)

# Expenditures

data_5.1 <- data_5 %>%
  rename(common_ID = common_id)%>%
  left_join(hh_id_0)%>%
  select(hh_id, everything(),-common_ID)%>%
  select(hh_id, item_code, cons_home_value, cons_total_value)%>%
  mutate_at(vars(cons_home_value, cons_total_value), ~ifelse(. == "        ", 0, as.numeric(.)))%>%
  mutate(cons_home_value = ifelse(is.na(cons_home_value),0,cons_home_value))%>%
  mutate(expenditures = cons_total_value - cons_home_value)%>%
  mutate(expenditures_total = cons_total_value)%>%
  mutate(item_code = as.numeric(item_code))%>%
  mutate(factor = ifelse(item_code %in% c(1,2,61,62,70,122,129,139, 152, 158, 159, 71, 72, 73, 74) | (item_code >= 101 & item_code <= 114) | (item_code >= 140 & item_code <= 150) |
                           (item_code >= 170 & item_code <= 179), 12, 365/7))%>%
  mutate(expenditures_year = expenditures*factor)%>%
  mutate(expenditures_total_year = expenditures_total*factor)%>%
  select(hh_id, item_code, expenditures_year, expenditures_total_year)

rm(data_5)

data_6.1 <- data_6 %>%
  rename(common_ID = common_id)%>%
  left_join(hh_id_0)%>%
  select(hh_id, everything(),-common_ID)%>%
  select(hh_id, item_code, cons_total_value)%>%
  mutate(expenditures = as.numeric(cons_total_value),
         item_code = as.numeric(item_code))%>%
  mutate(expenditures_year = expenditures*365/7)%>%
  select(hh_id, item_code, expenditures_year)

rm(data_6)

data_8.1 <- data_8 %>%
  rename(common_ID = common_id)%>%
  left_join(hh_id_0)%>%
  select(hh_id, everything(),-common_ID)%>%
  select(hh_id, item_code, cons_home_value, cons_total_value)%>%
  mutate_at(vars(starts_with("cons")), ~ as.numeric(.))%>%
  mutate(cons_home_value = ifelse(is.na(cons_home_value),0,cons_home_value))%>%
  mutate(expenditures = cons_total_value - cons_home_value)%>%
  mutate(expenditures_total = cons_total_value)%>%
  mutate(expenditures_year = expenditures*12)%>%
  mutate(expenditures_total_year = expenditures_total*12)%>%
  select(hh_id, item_code, expenditures_year, expenditures_total_year)%>%
  mutate(item_code = as.numeric(item_code))

rm(data_8)

data_9.1 <- data_9 %>%
  rename(common_ID = common_id)%>%
  left_join(hh_id_0)%>%
  select(hh_id, everything(),-common_ID)%>%
  select(hh_id, item_code, cons_total_value)%>%
  mutate(expenditures = as.numeric(cons_total_value))%>%
  mutate(item_code = as.numeric(item_code))%>%
  mutate(expenditures_year = ifelse(item_code %in% c(400,401,404,405,406,408,409,410,411,412,413,414,419,520,521,522,523,529,539,899), expenditures, expenditures*12))%>%
  select(hh_id, item_code, expenditures_year)

rm(data_9)

data_10.1 <- data_10 %>%
  rename(common_ID = common_id)%>%
  left_join(hh_id_0)%>%
  select(hh_id, everything(),-common_ID)%>%
  select(hh_id, item_code, cons_home_value, cons_total_value)%>%
  mutate_at(vars(starts_with("cons")), ~ as.numeric(.))%>%
  mutate(cons_home_value = ifelse(is.na(cons_home_value),0,cons_home_value))%>%
  mutate(expenditures = cons_total_value - cons_home_value)%>%
  mutate(expenditures_total = cons_total_value)%>%
  mutate(expenditures_year = expenditures*365/7)%>%
  mutate(expenditures_total_year = expenditures_total*365/7)%>%
  select(hh_id, item_code, expenditures_year, expenditures_total_year)%>%
  mutate(item_code = as.numeric(item_code))

rm(data_10)

data_12.1 <- data_12 %>%
  rename(common_ID = common_id)%>%
  left_join(hh_id_0)%>%
  select(hh_id, everything(),-common_ID)%>%
  select(hh_id, item_code, cons_total_value)%>%
  mutate_at(vars(-hh_id), ~ as.numeric(.))%>%
  rename(expenditures_year = cons_total_value)%>%
  select(hh_id, item_code, expenditures_year)

rm(data_12)

data_13.1 <- data_13 %>%
  rename(common_ID = common_id)%>%
  left_join(hh_id_0)%>%
  select(hh_id, everything(),-common_ID)%>%
  select(hh_id, item_code, tot_expenditure)%>%
  mutate(item_code = as.numeric(item_code))%>%
  mutate(expenditures_year = as.numeric(tot_expenditure))%>%
  select(hh_id, item_code, expenditures_year)

rm(data_13)

expenditures_items <- bind_rows(data_5.1, data_6.1, data_8.1, data_9.1, data_10.1, data_12.1, data_13.1)%>%
  arrange(hh_id, item_code)%>%
  mutate(expenditures_total_year = ifelse(is.na(expenditures_total_year), expenditures_year, expenditures_total_year))

write_parquet(expenditures_items, "../0_Data/1_Household Data/1_India/1_Data_Clean/expenditures_items_India.parquet")

rm(data_5.1, data_6.1, data_8.1, data_9.1, data_10.1, data_12.1, data_13.1, hh_id_0, data_11)

# Codes

Province.Code <- distinct(household_information, province)%>%
  arrange(province)%>%#
  write.xlsx("../0_Data/1_Household Data/1_India/2_Codes/2022/Province.Code.xlsx")

Province.1.Code <- distinct(household_information, province_1)%>%
  arrange(province_1)%>%#
  write.xlsx("../0_Data/1_Household Data/1_India/2_Codes/2022/Province.1.Code.xlsx")

District.Code <- distinct(household_information, province, district)%>%
  arrange(province, district)%>%#
  write.xlsx("../0_Data/1_Household Data/1_India/2_Codes/2022/District.Code.xlsx")

Gender.Code <- distinct(household_information, sex_hhh)%>%
  arrange(sex_hhh)%>%
  mutate(gender = c("Male", "Female", "Transgender"))%>%
  write.xlsx("../0_Data/1_Household Data/1_India/2_Codes/2022/Gender.Code.xlsx")

Education.Code <- distinct(household_information, edu_hhh)%>%
  arrange(edu_hhh)%>%
  mutate(Education = c("Not literate", "Literate with non-formal education", "Below primary", "Primary", "Upper primary", "Secondary", "Higher secondary", "Diplomate - up to secondary",
                       "Diploma - higher secondary", "Diploma - graduation", "Graduate", "Post-graduate"))
  write.xlsx("../0_Data/1_Household Data/1_India/2_Codes/2022/Education.Code.xlsx")

Occupation.Code <- distinct(household_information, occupation)%>%
  arrange(occupation)%>%#
  write.xlsx("../0_Data/1_Household Data/1_India/2_Codes/2022/Occupation.Code.xlsx")

Industry.Code <- distinct(household_information, ind_hhh)%>%
  arrange(ind_hhh)%>%#
  write.xlsx("../0_Data/1_Household Data/1_India/2_Codes/2022/Industry.Code.xlsx")

Religion.Code <- distinct(household_information, religion)%>%
  arrange(religion)%>%#
  mutate(Religion = c("Not reported","Hinduism", "Islam", "Christianity", "Sikhism", "Jainism", "Buddhism", "Zoroastrianism", "Others"))
  write.xlsx("../0_Data/1_Household Data/1_India/2_Codes/2022/Religion.Code.xlsx")

Ethnicity.Code <- distinct(household_information, ethnicity)%>%
  arrange(ethnicity)%>%#
  mutate(Ethnicity = c("Not reported","Scheduled Tribe", "Scheduled Caste", "Other backward class", "Others"))%>%
  write.xlsx("../0_Data/1_Household Data/1_India/2_Codes/2022/Ethnicity.Code.xlsx")

Renting.Code <- distinct(household_information, renting)%>%
  arrange(renting)%>%
  mutate(Renting = c("Not reported", "Owned", "Hired", "Others"))%>%
  write.xlsx("../0_Data/1_Household Data/1_India/2_Codes/2022/Renting.Code.xlsx")

Wall.Code <- distinct(household_information, wall)%>%
  arrange(wall)%>%
  mutate(Wall = c("Not reported", "grass/ straw/ leaves/ reeds/ bamboo", "mud (with /without bamboo) /unburnt brick", "canvas / cloth",
                  "other katcha", "timber", "burnt brick /stone/ lime stone", "iron or other metal sheet", "cement / RBC / RCC", "other pucca"))%>%
  write.xlsx("../0_Data/1_Household Data/1_India/2_Codes/2022/Wall.Code.xlsx")

Roof.Code <- distinct(household_information, roof)%>%
  arrange(roof)%>%
  mutate(Roof = c("Not reported", "grass/ straw/ leaves/ reeds/ bamboo", "mud (with /without bamboo) /unburnt brick", "canvas / cloth",
                  "other katcha","tiles/slate","burnt brick /stone/ lime stone", "iron/zinc or other metal sheet/asbestos", "cement / RBC / RCC", "other pucca"))%>%
  write.xlsx("../0_Data/1_Household Data/1_India/2_Codes/2022/Roof.Code.xlsx")

Floor.Code <- distinct(household_information, floor)%>%
  arrange(floor)%>%
  mutate(Floor = c("Not reported", "grass/ straw/ leaves/ reeds/ bamboo", "mud (with /without bamboo) /unburnt brick", "canvas / cloth",
                  "other katcha","tiles/slate","burnt brick /stone/ lime stone", "iron/zinc or other metal sheet/asbestos", "cement / RBC / RCC", "other pucca"))%>%
  write.xlsx("../0_Data/1_Household Data/1_India/2_Codes/2022/Floor.Code.xlsx")

Cooking.Code <- distinct(household_information, cooking_fuel)%>%
  arrange(cooking_fuel)%>%
  mutate(Cooking_Fuel = c("Firewood", "LPG", "Natural gas", "Dung", "Kerosene", "Coal", "Gobar gas", "Other biogas", "Others", "Charcoal", "Electricity", "No cooking"))%>%
  write.xlsx("../0_Data/1_Household Data/1_India/2_Codes/2022/Cooking.Code.xlsx")

Lighting.Code <- distinct(household_information, lighting_fuel)%>%
  arrange(lighting_fuel)%>%
  mutate(Lighting_fuel = c("Electricity", "Kerosene", "Other oil", "Gas", "Candle", "No lighting", "Others"))%>%
  write.xlsx("../0_Data/1_Household Data/1_India/2_Codes/2022/Lighting.Code.xlsx")

# Work with data ####

# 0       General ####

carbon.price <- 40 # in USD/tCO2
GTAP_version <- "11B" # Choose between 10 and 11A and 11B
GTAP_year    <- 2017 # Choose between 2014 and 2017

# 1.1     Setup ####

Country.Name <- "India"
  
  # 2       Load Household and Expenditure File ####
  
path_0                  <-list.files("../0_Data/1_Household Data/")[grep(Country.Name, list.files("../0_Data/1_Household Data/"), ignore.case = T)][1]
  
household_information   <- read_parquet("../0_Data/1_Household Data/1_India/1_Data_Clean/household_information_India.parquet")%>%
  mutate(hh_weights = as.numeric(hh_weights),
         hh_size    = as.numeric(hh_size))%>%
  mutate(hh_weights = hh_weights/100)%>%
  mutate(pop = hh_weights*hh_size)
    
clean_0 <- nrow(household_information)
    
expenditure_information <- read_parquet("../0_Data/1_Household Data/1_India/1_Data_Clean/expenditures_items_India.parquet")
    
household_information <- filter(household_information, hh_id %in% expenditure_information$hh_id)
clean_0.1 <- nrow(household_information)
    
appliances_0            <- read_parquet(sprintf("../0_Data/1_Household Data/1_India/1_Data_Clean/appliances_0_1_India.parquet"))

  # 3       Data Cleaning ####
  # 3.1     Check for Duplicates ####
  
  household_information_1 <- household_information %>%
    group_by_at(vars(-hh_id))%>%
    mutate(number = n(),
           flag = ifelse(number > 1,1,0))%>%
    ungroup()
  
  if(nrow(filter(household_information_1, flag != 0))>0) View(filter(household_information_1, flag != 0))
  
  hh_duplicates_information <- household_information_1 %>%
    filter(flag != 0)%>%
    select(hh_id)
  
  print(paste0(sprintf("For %s, ", Country.Name), nrow(hh_duplicates_information), " households have duplicate information."))
  
  if("expenditures_sp_year" %in% colnames(expenditure_information)){
    expenditure_information <- expenditure_information %>%
      select(-expenditures_sp_year)
  }
  
  # Exact duplicates for expenditures - see below for more detailed approach
  expenditure_information_1 <- expenditure_information %>%
    select(-expenditures_total_year)%>%
    pivot_wider(names_from = "item_code", values_from = "expenditures_year")%>%
    group_by_at(vars(-hh_id))%>%
    mutate(number = n(),
           flag = ifelse(number > 1,1,0))%>%
    ungroup()%>%
    arrange(desc(flag))
  
  print(paste("There are ", nrow(count(expenditure_information, hh_id)) - nrow(count(filter(expenditure_information_1, flag == 0), hh_id)), sprintf(" cases of exact duplicates of expenditures on the item level in %s.", Country.Name)))
  
  hh_duplicates_expenditures_1 <- expenditure_information_1 %>%
    filter(flag != 0)%>%
    select(hh_id)
  
  # Alternative: calculates share of duplicates on the item level
  # Use this as a monitoring tool
  expenditure_information_2 <- expenditure_information %>%
    filter(!is.na(expenditures_year) & expenditures_year != 0)%>%
    group_by(item_code, expenditures_year)%>%
    mutate(duplicate_flag = ifelse(n()>1,1,0))%>%
    ungroup()%>%
    group_by(hh_id)%>%
    mutate(duplicate_share = sum(duplicate_flag)/n())%>%
    ungroup()
  
  hh_duplicates_expenditures_2 <- expenditure_information_2 %>%
    filter(duplicate_share == 1)%>%
    select(hh_id)%>%
    distinct()
  
  # Could be sufficient to monitor
  
  expenditure_information_3 <- expenditure_information %>%
    filter(!is.na(expenditures_year) & expenditures_year != 0)%>%
    group_by(hh_id)%>%
    summarise(hh_expenditures = sum(expenditures_year))%>%
    ungroup()%>%
    group_by(hh_expenditures)%>%
    mutate(duplicate_flag_2 = ifelse(n()>1,1,0))%>%
    ungroup()
  
  if(nrow(filter(expenditure_information_3, duplicate_flag_2 ==1))>1) print("Warning! Two or more households spend exactly the same amount of money on all their items.")
  print(paste0(nrow(filter(expenditure_information_3, duplicate_flag_2 == 1)), sprintf(" households report the same amount of expenditures on all their items in %s.", Country.Name)))
  
  # Probably more important than searching for duplicates at item expenditure level
  
  hh_duplicates_expenditures_3 <- expenditure_information_3 %>%
    filter(duplicate_flag_2 == 1)%>%
    select(hh_id)
  
  # Negative total expenditures
  
  expenditure_information_4 <- expenditure_information_3 %>%
    mutate(flag_negative = ifelse(hh_expenditures < 0,1,0))
  
  hh_negative_expenditures_4 <- expenditure_information_4 %>%
    filter(flag_negative == 1)%>%
    select(hh_id)
  
  # 3.1.1   Duplicate Removal ####
  
  # hh_duplicates_information captures all households, whose characteristics are identical with another --> needs careful consideration on whether these are actual duplicates
  # hh_duplicates_expenditures_1 captures all households, who spend exactly the same amount of money on each item than any other households --> likely duplicate
  # hh_duplicates_expenditures_2 captures all households, who do not report any individual amount of expenditures on any items. 
  # Each level of expenditures for any item is shared with another household --> needs careful consideration on whether these are actual duplicates --> likely no duplicate
  # hh_duplicates_expenditures_3 captures all households, who report the same amount of total expenditures as some other household --> likely no duplicate, but check individually for your country
  
  # If you have identified duplicates and want to delete them, do the following:
  # select the corresponding line with hh_ids
  
  household_information <- household_information %>%
      filter(!hh_id %in% hh_duplicates_information$hh_id)%>%
      filter(!hh_id %in% hh_duplicates_expenditures_1$hh_id)
    
    expenditure_information <- expenditure_information %>%
      filter(!hh_id %in% hh_duplicates_information$hh_id)%>%
      filter(!hh_id %in% hh_duplicates_expenditures_1$hh_id)
  
  rm(expenditure_information_1, expenditure_information_2, expenditure_information_3, household_information_1, 
     hh_duplicates_expenditures_1, hh_duplicates_expenditures_2, hh_duplicates_expenditures_3, hh_duplicates_information,
     hh_negative_expenditures_4, expenditure_information_4)
  
  # 3.2     Cleaning per Item_code ####
  
  expenditure_information_4 <- expenditure_information %>%
    # pivot_longer(-hh_id, names_to = "item_code", values_to = "expenditures") %>%
    left_join(select(household_information, hh_id, hh_weights), by = "hh_id")%>%
    mutate(hh_weights = as.numeric(hh_weights))%>%
    # here: negative values are deleted (@EU)
    filter(!is.na(expenditures_year) & expenditures_year > 0 )%>%
    group_by(item_code)%>%
    mutate(outlier_95 = wtd.quantile(expenditures_year, weights = hh_weights, probs = 0.95),
           outlier_99 = wtd.quantile(expenditures_year, weights = hh_weights, probs = 0.99),
           median_exp = wtd.quantile(expenditures_year, weights = hh_weights, probs = 0.5),
           mean_exp   = wtd.mean(    expenditures_year, weights = hh_weights))%>%
    ungroup()%>%
    mutate(flag_outlier_95 = ifelse(expenditures_year>= outlier_95,1,0),
           flag_outlier_99 = ifelse(expenditures_year>= outlier_99,1,0))%>%
    # this line replaces all expenditures which are above the 99th percentile for each item to the median
    mutate(expenditures = ifelse(flag_outlier_99 == 1, median_exp, expenditures_year))%>%
    select(hh_id, item_code, expenditures, hh_weights)
  
  expenditure_information <- expenditure_information_4 %>%
    select(-hh_weights)
  
  # 3.2.1   Cleaning per Total Expenditures (99%) ####
  
  expenditure_information_4.1 <- expenditure_information_4 %>%
    group_by(hh_id) %>%
    summarise(total_expenditures = sum(expenditures),
              hh_weights         = first(hh_weights)) %>%
    ungroup() %>%
    mutate(outlier_95 = wtd.quantile(total_expenditures, weights = hh_weights, probs = 0.95),
           outlier_99 = wtd.quantile(total_expenditures, weights = hh_weights, probs = 0.99),
           mean_total = wtd.mean(total_expenditures,     weights = hh_weights),
           sd_total   = sqrt(wtd.var(total_expenditures, weights = hh_weights)))%>%
    mutate(z_score = (total_expenditures-mean_total)/sd_total)
  
  expenditure_outlier <- expenditure_information_4.1 %>%
    filter(z_score > 5)%>%
    select(hh_id)%>%
    distinct()
  
  # Deleting outliers for some countries
  

    
    household_information <- household_information %>%
      filter(!hh_id %in% expenditure_outlier$hh_id)
    
    expenditure_information <- expenditure_information%>%
      filter(!hh_id %in% expenditure_outlier$hh_id)

  
  clean_5 <- nrow(household_information)
  
  print("Expenditure data cleaned!")
  
  rm(expenditure_information_4.1, expenditure_information_4, expenditure_outlier)
  
  # 4       Summary Statistics ####
  # _____   ####
  # 5       Transformation and Modelling ####
  
  # 5.1     Load Additional Data ####
  
  Year_0 <- 2022
  CNTRY  <- "IND"
  
  # 5.1.1   Supplementary Data ####
  # Exchange Rates
  
  # information.ex <- read.xlsx("../0_Data/9_Supplementary Data/Exchange_Rates_2014.xlsx") # from World Bank
  
  information.ex <- read.xlsx("../0_Data/9_Supplementary Data/Exchange_Rates_2014_2017.xlsx") # from World Bank
  
  exchange.rate <- 1/78.6
  
  # CPI-Adjustment (Inflation/Deflation)
  
  cpis <- read.xlsx("../0_Data/9_Supplementary Data/IMF_Consumer_Price_Index_Inflation_Average.xlsx")
  
  cpis_0 <- cpis %>%
    select(ISO, Country, starts_with("2"))%>%
    filter(Country == Country.Name | ISO == CNTRY)
  
  inflation_factor <- 1/((1+3.414/100)*(1+4.769/100)*(1+6.165/100)*(1+5.506/100)*(1+6.653/100))
  
  
  rm(cpis_1, cpis_0, information.ex, cpis, Country_Year, Year_0, custom_match)
  
  # 5.1.2   Matching GTAP Concordance ####
  
  
matching <- read.xlsx(sprintf("../0_Data/1_Household Data/%s/3_Matching_Tables/2022/Item_GTAP_Concordance_%s.xlsx", path_0, Country.Name))

  
 matching <- matching %>%
    select (-Explanation) %>%
    pivot_longer(-GTAP, names_to = "drop", values_to = "item_code")%>%
    filter(!is.na(item_code))%>%
    select(GTAP, item_code)%>%
    mutate(GTAP = ifelse(GTAP == "gas" | GTAP == "gdt", "gasgdt", GTAP))
  
  # Check if single item codes are assigned to two different GTAP categories
  
  item_codes <- select(expenditure_information, item_code)%>%
    distinct()%>%
    full_join(matching, by = "item_code")%>%
    filter(is.na(GTAP) | is.na(item_code))
  
  if(nrow(item_codes != 0))(paste("WARNING! Item-Codes missing in Excel-File!"))
  
  matching.check <- count(matching, item_code)%>%
    filter(n != 1)
  
  if(nrow(matching.check) != 0) (paste("WARNING! Item-Codes existing with two different GTAP-categories in Excel-File"))
  
  if(nrow(item_codes != 0) | nrow(matching.check) != 0) break
  
  rm(matching.check, item_codes)
  
  # 5.1.3   Matching Category Concordance ####
  
  categories <- read.xlsx(sprintf("../0_Data/1_Household Data/%s/3_Matching_Tables/2022/Item_Categories_Concordance_%s.xlsx", path_0, Country.Name), colNames = FALSE)
  
  categories <- categories %>%
    pivot_longer(-X1, names_to = "drop", values_to = "item_code")%>%
    filter(!is.na(item_code))%>%
    select(X1, item_code)%>%
    rename(category = X1)%>%
    distinct(category, item_code)
  
  item_codes <- select(expenditure_information, item_code)%>%
    distinct()%>%
    full_join(categories, by = "item_code")%>%
    filter(is.na(category) | is.na(item_code))
  
  if(nrow(item_codes != 0))(paste("WARNING! Item-Codes missing in Category-Excel-File!"))
  
  matching.check <- count(categories, item_code)%>%
    filter(n != 1)
  
  if(nrow(matching.check) != 0) (paste("WARNING! Item-Codes existing with two different Categories-categories in Excel-File"))
  
  if(nrow(item_codes != 0) | nrow(matching.check) != 0) break
  
  rm(matching.check, item_codes)
  
  # 5.1.4   Add Codes if necessary (TBD) ####
  
  # 5.1.5   Matching Fuel Concordance ####
  
  fuels <- read.xlsx(sprintf("../0_Data/1_Household Data/%s/3_Matching_Tables/2022/Item_Fuel_Concordance_%s.xlsx", path_0, Country.Name), colNames = FALSE)
  
  fuels <- fuels %>%
    pivot_longer(-X1, names_to = "drop", values_to = "item_code")%>%
    filter(!is.na(item_code))%>%
    rename(fuel = X1)%>%
    select(fuel, item_code)
  
  energy <- filter(categories, category == "energy")%>%
    full_join(fuels, by = "item_code")%>%
    filter(is.na(fuel) | is.na(category))
  
  if(nrow(energy) >0) print("Warning. Watch out for energy item codes.")
  
  rm(energy)
  
  # 5.1.6   Vector with Carbon Intensities ####
  
    if(GTAP_year == 2017 & GTAP_version == "11B"){
    
    if(Country.Name != "Europe"){
      if(!Country.Name %in% c("Barbados", "Liberia", "Suriname", "Myanmar", "Maldives", "Guinea-Bissau")){
        carbon_intensities_0 <- read.xlsx("../0_Data/2_IO Data/GTAP_11_MRIO/GTAP11B_2017/Carbon_Intensities_Full_All_Gas.xlsx", sheet = Country.Name)
      }
      
      GTAP_code            <- read_delim("../0_Data/2_IO Data/GTAP_10_MRIO/GTAP10.csv", ";", escape_double = FALSE, trim_ws = TRUE, show_col_types = FALSE)
      
      carbon_intensities   <- left_join(GTAP_code, carbon_intensities_0, by = c("Number"="GTAP"))%>%
        select(-Explanation, - Number)%>%
        mutate(GTAP = ifelse(GTAP == "gas" | GTAP == "gdt", "gasgdt", GTAP))%>%
        group_by(GTAP)%>%
        summarise(across(CO2_Mt:Total_HH_Consumption_MUSD, ~ sum(.)))%>%
        ungroup()%>%
        mutate(CO2_t_per_dollar_global      = CO2_Mt/            Total_HH_Consumption_MUSD,
               CO2_t_per_dollar_national    = CO2_Mt_within/     Total_HH_Consumption_MUSD,
               CO2_t_per_dollar_electricity = CO2_Mt_Electricity/Total_HH_Consumption_MUSD,
               CO2_t_per_dollar_transport   = CO2_Mt_Transport/  Total_HH_Consumption_MUSD,
               # CH4_t_per_dollar_national    = CH4_MtCO2_within/  Total_HH_Consumption_MUSD,
               # N2O_t_per_dollar_national    = N2O_MtCO2_within/  Total_HH_Consumption_MUSD,
               # FGAS_t_per_dollar_national   = FGAS_MtCO2_within/ Total_HH_Consumption_MUSD
        )%>%
        select(GTAP, starts_with("CO2_t"), ends_with("national"))
      
      rm(carbon_intensities_0, GTAP_code)
    }
    
    if(Country.Name == "Europe"){
      GTAP_code            <- read_delim("../0_Data/2_IO Data/GTAP_10_MRIO/GTAP10.csv", ";", escape_double = FALSE, trim_ws = TRUE, show_col_types = FALSE)
      
      carbon_intensities_EU <- data.frame()
      
      for(i in countries_b){
        carbon_intensities_0 <- read.xlsx("../0_Data/2_IO Data/GTAP_11_MRIO/GTAP11B_2017/Carbon_Intensities_Full_All_Gas.xlsx", sheet = i)
        carbon_intensities   <- left_join(GTAP_code, carbon_intensities_0, by = c("Number"="GTAP"))%>%
          select(-Explanation, - Number)%>%
          mutate(GTAP = ifelse(GTAP == "gas" | GTAP == "gdt", "gasgdt", GTAP))%>%
          group_by(GTAP)%>%
          summarise(across(CO2_Mt:Total_HH_Consumption_MUSD, ~ sum(.)))%>%
          ungroup()%>%
          mutate(CO2_t_per_dollar_global      = CO2_Mt/            Total_HH_Consumption_MUSD,
                 CO2_t_per_dollar_national    = CO2_Mt_within/     Total_HH_Consumption_MUSD,
                 CO2_t_per_dollar_electricity = CO2_Mt_Electricity/Total_HH_Consumption_MUSD,
                 CO2_t_per_dollar_transport   = CO2_Mt_Transport/  Total_HH_Consumption_MUSD,
                 # CH4_t_per_dollar_national    = CH4_MtCO2_within/  Total_HH_Consumption_MUSD,
                 # N2O_t_per_dollar_national    = N2O_MtCO2_within/  Total_HH_Consumption_MUSD,
                 # FGAS_t_per_dollar_national   = FGAS_MtCO2_within/ Total_HH_Consumption_MUSD
          )%>%
          select(GTAP, starts_with("CO2_t"), ends_with("national"))%>%
          mutate(Country = i)
        
        carbon_intensities_EU <- carbon_intensities_EU %>%
          bind_rows(carbon_intensities)
      }
      
      rm(carbon_intensities_0, GTAP_code)
    }
    
  }
  
  # ____    ####
  # 6       Transformation of Data ####
  
  # 6.1     Anonymising Household-ID ####
  
  household_ids <- select(household_information, hh_id)%>%
    distinct()%>%
    mutate(hh_id_new = 1:n())
  
  household_information <- left_join(household_information, household_ids, by = "hh_id")%>%
    select(hh_id_new, everything(), -hh_id)%>%
    rename(hh_id = hh_id_new)
  
  expenditure_information <- left_join(expenditure_information, household_ids, by = "hh_id")%>%
    select(hh_id_new, everything(), -hh_id)%>%
    rename(hh_id = hh_id_new)
  
  
    appliances_1 <- left_join(appliances_0, household_ids)%>%
      select(hh_id_new, everything(), - hh_id)%>%
      rename(hh_id = hh_id_new)
    write_parquet(appliances_1, sprintf("../0_Data/1_Household Data/1_India/1_Data_Clean/appliances_0_1_new_India.parquet"))
    rm(appliances_1, appliances_0)

  
  basic_household_information <- household_information %>%
    select(hh_id, hh_size, hh_weights)
  
 # 6.2     Merging Expenditure Data and GTAP ####
  
  expenditure_information_1 <- left_join(expenditure_information, matching, by = "item_code")%>%
    filter(GTAP != "deleted")
  
  # 6.3     Assign Households to Expenditure Bins ####
  
  if(Country.Name != "Europe"){
    binning_0 <- expenditure_information_1 %>%
      group_by(hh_id)%>%
      mutate(hh_expenditures = sum(expenditures))%>%
      ungroup()%>%
      left_join(basic_household_information, by = "hh_id")%>%
      mutate(hh_expenditures_pc = hh_expenditures/hh_size)%>%
      select(hh_id, hh_expenditures, hh_expenditures_pc, hh_weights)%>%
      filter(!duplicated(hh_id))%>%
      mutate(Income_Group_5  = as.numeric(binning(hh_expenditures_pc, bins = 5,  method = c("wtd.quantile"), weights = hh_weights)),
             Income_Group_10 = as.numeric(binning(hh_expenditures_pc, bins = 10, method = c("wtd.quantile"), weights = hh_weights)))%>%
      select(hh_id, hh_expenditures, hh_expenditures_pc, starts_with("Income"))
  }
  
  # 6.4     Calculating Expenditure Shares on Energy/Food/Goods/Services ####
  
  expenditures_categories_0 <- left_join(expenditure_information, categories, by = "item_code")%>%
    filter(category != "deleted" & category != "in-kind" & category != "self-produced")%>%
    group_by(hh_id, category)%>%
    summarise(expenditures_category = sum(expenditures))%>%
    ungroup()%>%
    group_by(hh_id)%>%
    mutate(share_category = expenditures_category/sum(expenditures_category))%>%
    ungroup()%>%
    select(hh_id, category, share_category)%>%
    pivot_wider(names_from = "category", values_from = "share_category", names_prefix = "share_", values_fill = 0)
  
  # 6.5     Calculating Expenditure Shares on detailed Energy Items ####
  
  if(Country.Name != "Europe"){
    expenditures_fuels <- left_join(expenditure_information, fuels, by = "item_code")%>%
      filter(!is.na(fuel))%>%
      group_by(hh_id, fuel)%>%
      summarise(expenditures = sum(expenditures))%>%
      ungroup()%>%
      mutate(expenditures = expenditures*inflation_factor*exchange.rate)%>%
      pivot_wider(names_from = "fuel", values_from = "expenditures", names_prefix = "exp_USD_", values_fill = 0)
    
    expenditures_fuels <- distinct(household_information, hh_id)%>%
      left_join(expenditures_fuels, by = "hh_id")%>%
      mutate_at(vars(-hh_id), list(~ ifelse(is.na(.),0,.)))
  }
  
    # 6.6     Summarising Expenditures on the GTAP Level ####
  
  if(Country.Name != "Europe"){
    expenditure_information_1 <- expenditure_information_1 %>%
      group_by(hh_id, GTAP)%>%
      summarise(expenditures = sum(expenditures))%>%
      ungroup()%>%
      # We inflate/deflate expenditures to 2014 and convert 2014 expenditures to USD (no PPP-adjustment)
      mutate(expenditures_USD_2017 = expenditures*inflation_factor*exchange.rate)%>%
      group_by(hh_id)%>%
      mutate(hh_expenditures_USD_2017 = sum(expenditures_USD_2017))%>%
      ungroup()
  }
  
  expenditure_information_out <- expenditure_information_1 %>%
    mutate(share = expenditures_USD_2017/hh_expenditures_USD_2017)
  
  write_parquet(expenditure_information_out, "../0_Data/1_Household Data/1_India/1_Data_Clean/expenditure_share_GTAP_India.parquet")
  
  expenditure_information_2 <- expenditure_information_1 %>%
    group_by(hh_id)%>%
    summarise(hh_expenditures_LCU = sum(expenditures))%>%
    ungroup()
  
  clean_6 <- nrow(distinct(expenditure_information_1, hh_id))
  
  rm(household_ids)
  
  # 6.7     Merging Expenditures and Carbon Intensities ####
  
  if(Country.Name != "Europe"){
    
    household_carbon_footprint <- left_join(expenditure_information_1, carbon_intensities, by = "GTAP")%>%
      filter(GTAP != "other")%>%
      mutate(CO2_t_global      = expenditures_USD_2017*CO2_t_per_dollar_global,
             CO2_t_national    = expenditures_USD_2017*CO2_t_per_dollar_national,
             CO2_t_electricity = expenditures_USD_2017*CO2_t_per_dollar_electricity,
             CO2_t_transport   = expenditures_USD_2017*CO2_t_per_dollar_transport,
             # CH4_t_national    = expenditures_USD_2014*CH4_t_per_dollar_national,
             # N2O_t_national    = expenditures_USD_2014*N2O_t_per_dollar_national,
             # FGAS_t_national   = expenditures_USD_2014*FGAS_t_per_dollar_national
      )%>%
      select(-starts_with("CO2_t_per"), 
             # -CH4_t_per_dollar_national, -N2O_t_per_dollar_national, -FGAS_t_per_dollar_national
      )%>%
      group_by(hh_id)%>%
      summarise(hh_expenditures_USD_2017 = first(hh_expenditures_USD_2017),
                CO2_t_global      = sum(CO2_t_global),    
                CO2_t_national    = sum(CO2_t_national),  
                CO2_t_electricity = sum(CO2_t_electricity),
                CO2_t_transport   = sum(CO2_t_transport),
                # CH4_t_national    = sum(CH4_t_national),
                # N2O_t_national    = sum(N2O_t_national),
                # FGAS_t_national   = sum(FGAS_t_national)
      )%>%
      ungroup()
  }
  
  rm(expenditure_information_1)
  
  # 6.8     Add-On: Sectoral Emissions and additional expenditures ####
  
#  if(Country.Name != "Europe"){
#    household_sectoral_carbon_footprint <- left_join(expenditure_information, matching, by = "item_code")%>%
#      left_join(categories, by = "item_code")%>%
#      left_join(fuels, by = "item_code")%>%
#      filter(GTAP != "deleted")%>%
#      filter(category != "deleted" & category != "in-kind" & category != "self-produced" & category != "other_binning" & GTAP != "other")%>%
#      mutate(expenditures_USD_2017 = expenditures*inflation_factor*exchange.rate)%>%
#      mutate(aggregate_category = ifelse(category == "food" | category == "goods" | category == "services", category, 
#                                         ifelse(is.na(category), "NA_1", 
#                                                ifelse(category == "energy" & (is.na(fuel)| fuel == "Biomass"), "other_energy",
#                                                       ifelse(category == "energy" & (fuel == "Diesel" | fuel == "Petrol"), "transport_fuels",
#                                                              ifelse(category == "energy" & fuel == "Electricity", "Electricity",
#                                                                     ifelse(category == "energy" & (fuel == "Gas" | fuel == "LPG" | fuel == "Kerosene" | fuel == "Coal" | fuel == "Firewood" | fuel == "Charcoal"), "cooking_fuels", "NA_2")))))))%>%
#      left_join(carbon_intensities, by = "GTAP")%>%
#      mutate(CO2_s_t_national    = expenditures_USD_2017*CO2_t_per_dollar_national)%>%
#      select(-starts_with("CO2_t_per"))%>%
#      group_by(hh_id, aggregate_category)%>%
#      summarise(CO2_s_t_national      = sum(CO2_s_t_national))%>%
#      ungroup()%>%
#      mutate(exp_s_CO2_national    = CO2_s_t_national*carbon.price)%>%
#      select(-CO2_s_t_national)%>%
#      pivot_wider(names_from = "aggregate_category", values_from = "exp_s_CO2_national", values_fill = 0, names_prefix = "exp_s_")%>%
#      rename(exp_s_Goods = exp_s_goods, exp_s_Services = exp_s_services, exp_s_Food = exp_s_food)
#    
#    dir.create("../1_Carbon_Pricing_Incidence/1_Data_Incidence_Analysis/1_Transformed_and_Modeled", showWarnings = FALSE)
#    
#    write_csv(household_sectoral_carbon_footprint, 
#              sprintf("../1_Carbon_Pricing_Incidence/1_Data_Incidence_Analysis/1_Transformed_and_Modeled/%s_%s/Sectoral_Burden_%s.csv",  GTAP_year, GTAP_version, Country.Name))
#    
#    rm(basic_household_information, expenditure_information, matching, exchange.rate, inflation_factor, fuels, categories, household_sectoral_carbon_footprint, carbon_intensities)
#    
#  }
  
    # ____    ####
  # 7       Model / Calculating Carbon Incidence ####
  # 7.1     Analysis of Carbon Pricing Incidence ####
  
  household_carbon_incidence <- household_carbon_footprint %>%
    mutate(exp_CO2_global              = CO2_t_global*carbon.price,
           exp_CO2_national            = CO2_t_national*carbon.price,
           exp_CO2_electricity         = CO2_t_electricity*carbon.price,
           exp_CO2_transport           = CO2_t_transport*carbon.price)%>%
    mutate(burden_CO2_global           = exp_CO2_global/     hh_expenditures_USD_2017,
           burden_CO2_national         = exp_CO2_national/   hh_expenditures_USD_2017,
           burden_CO2_electricity      = exp_CO2_electricity/hh_expenditures_USD_2017,
           burden_CO2_transport        = exp_CO2_transport/  hh_expenditures_USD_2017)
  
  final_incidence_information <- household_carbon_incidence %>%
    left_join(binning_0, by = "hh_id")%>%
    left_join(expenditures_categories_0, by = "hh_id")%>%
    left_join(expenditures_fuels, by = "hh_id")
  
  if(max(final_incidence_information$CO2_t_global) == "Inf") "Warning! Check Intensities."
  
  if(max(final_incidence_information$CO2_t_global) == "Inf") break
  
    NA_ids <- household_information %>%
      filter(!hh_id %in% final_incidence_information$hh_id)
    
    household_information <- household_information %>%
      mutate(Country = CNTRY)%>%
      filter(!hh_id %in% NA_ids$hh_id)
    
    write_parquet(final_incidence_information, "../0_Data/1_Household Data/1_India/1_Data_Clean/final_incidence_information.parquet")
    write_parquet(household_information,       "../0_Data/1_Household Data/1_India/1_Data_Clean/household_information.parquet")

# 8.  Analysis ####
    
incidence_information <- read_parquet("../0_Data/1_Household Data/1_India/1_Data_Clean/final_incidence_information.parquet")
household_information <- read_parquet("../0_Data/1_Household Data/1_India/1_Data_Clean/household_information.parquet")
appliances <- read_parquet("../0_Data/1_Household Data/1_India/1_Data_Clean/appliances_0_1_India.parquet")

# Codes
Cooking.Codes.Aggregate   <- read.xlsx("T:/MSA/papers_internal/work_in_progress/Mi_Homogenized_Datainfrastructure/0_Data/1_Household Data/1_India/2_Codes/2022/Cooking.Code.xlsx")
Lighting.Codes.Aggregate  <- read.xlsx("T:/MSA/papers_internal/work_in_progress/Mi_Homogenized_Datainfrastructure/0_Data/1_Household Data/1_India/2_Codes/2022/Lighting.Code.xlsx")
Floor.Codes.Aggregate     <- read.xlsx("T:/MSA/papers_internal/work_in_progress/Mi_Homogenized_Datainfrastructure/0_Data/1_Household Data/1_India/2_Codes/2022/Floor.Code.xlsx")
Gender.Codes.Aggregate    <- read.xlsx("T:/MSA/papers_internal/work_in_progress/Mi_Homogenized_Datainfrastructure/0_Data/1_Household Data/1_India/2_Codes/2022/Gender.Code.xlsx")
Province.Codes.Aggregate  <- read.xlsx("T:/MSA/papers_internal/work_in_progress/Mi_Homogenized_Datainfrastructure/0_Data/1_Household Data/1_India/2_Codes/2022/Province.Code.xlsx")
District.Codes.Aggregate  <- read.xlsx("T:/MSA/papers_internal/work_in_progress/Mi_Homogenized_Datainfrastructure/0_Data/1_Household Data/1_India/2_Codes/2022/District.Code.xlsx")
Ethnicity.Codes.Aggregate <- read.xlsx("T:/MSA/papers_internal/work_in_progress/Mi_Homogenized_Datainfrastructure/0_Data/1_Household Data/1_India/2_Codes/2022/Ethnicity.Code.xlsx")
Renting.Codes.Aggregate   <- read.xlsx("T:/MSA/papers_internal/work_in_progress/Mi_Homogenized_Datainfrastructure/0_Data/1_Household Data/1_India/2_Codes/2022/Renting.Code.xlsx")
Religion.Codes.Aggregate  <- read.xlsx("T:/MSA/papers_internal/work_in_progress/Mi_Homogenized_Datainfrastructure/0_Data/1_Household Data/1_India/2_Codes/2022/Religion.Code.xlsx")
Roof.Codes.Aggregate      <- read.xlsx("T:/MSA/papers_internal/work_in_progress/Mi_Homogenized_Datainfrastructure/0_Data/1_Household Data/1_India/2_Codes/2022/Roof.Code.xlsx")
Wall.Codes.Aggregate      <- read.xlsx("T:/MSA/papers_internal/work_in_progress/Mi_Homogenized_Datainfrastructure/0_Data/1_Household Data/1_India/2_Codes/2022/Wall.Code.xlsx")
Education.Codes.Aggregate   <- read.xlsx("T:/MSA/papers_internal/work_in_progress/Mi_Homogenized_Datainfrastructure/0_Data/1_Household Data/1_India/2_Codes/2022/Education.Code.xlsx")

# Province.1.Codes.Aggregate    <- read.xlsx("T:/MSA/papers_internal/work_in_progress/Mi_Homogenized_Datainfrastructure/0_Data/1_Household Data/1_India/2_Codes/2022/Religion.Code.csv")


data_0 <- left_join(incidence_information, household_information)%>%
  mutate(burden_CO2_national_1000 = burden_CO2_national*11.5/40)%>%
  left_join(appliances)%>%
  left_join(Cooking.Codes.Aggregate)%>%
  left_join(Lighting.Codes.Aggregate)%>%
  left_join(Floor.Codes.Aggregate)%>%
  left_join(Gender.Codes.Aggregate)%>%
  left_join(Province.Codes.Aggregate)%>%
  left_join(District.Codes.Aggregate)%>%
  left_join(Ethnicity.Codes.Aggregate)%>%
  left_join(Renting.Codes.Aggregate)%>%
  left_join(Religion.Codes.Aggregate)%>%
  left_join(Roof.Codes.Aggregate)%>%
  left_join(Wall.Codes.Aggregate)%>%
  left_join(Education.Codes.Aggregate)%>%
  select(-hh_id, -starts_with("CO2_"), -starts_with("exp_CO2"), -burden_CO2_global, -burden_CO2_electricity, -burden_CO2_transport,
         -hh_expenditures, -hh_expenditures_pc, -starts_with("share"), -starts_with("exp_USD_"))

data_6.2 <- data_0 %>%
  group_by(Income_Group_5)%>%
  summarise(
    y5  = wtd.quantile(burden_CO2_national_1000, weights = hh_weights, probs = 0.05),
    y25 = wtd.quantile(burden_CO2_national_1000, weights = hh_weights, probs = 0.25),
    y50 = wtd.quantile(burden_CO2_national_1000, weights = hh_weights, probs = 0.5),
    y75 = wtd.quantile(burden_CO2_national_1000, weights = hh_weights, probs = 0.75),
    y95 = wtd.quantile(burden_CO2_national_1000, weights = hh_weights, probs = 0.95),
    mean = wtd.mean(   burden_CO2_national_1000, weights = hh_weights))%>%
  ungroup()%>%
  mutate(interest = ifelse(Income_Group_5 == 1 | Income_Group_5 == 5,"1", "0"))

data_6.2.1 <- data_6.2 %>%
  summarise(min_median = min(y50),
            max_median = max(y50))

P.6.2.2 <- ggplot(data_6.2, aes(x = as.character(Income_Group_5)))+
  #geom_rect(aes(ymin = min_median, ymax = max_median), xmin = 0, xmax = 6, alpha = 0.2, fill = "lightblue", inherit.aes = FALSE)+
  geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95), 
               stat = "identity", position = position_dodge(0.5), outlier.shape = NA, width = 0.5, size = 0.3, alpha = 1, fill = "grey") +
  theme_bw()+
  xlab("Expenditure quintiles")+ ylab(expression(paste("Incidence for a carbon price of INR 1,000/", tCO[2], sep = "")))+
  geom_point(aes(y = mean), shape = 23, size = 2, fill = "white")+
  scale_y_continuous(expand = c(0,0), labels = scales::percent_format(accuracy = 1))+
  scale_x_discrete(labels = c("1 \n Poorest \n 20 Percent", "2", "3", "4", "5 \n Richest \n 20 Percent"))+
  coord_flip(ylim = c(0,0.03))+
  #coord_cartesian(ylim = c(0,0.1))+
  scale_fill_discrete(guide = "none")+
  theme(axis.text.y = element_text(size = 7), 
        axis.text.x = element_text(size = 7),
        axis.title  = element_text(size = 8),
        plot.title = element_text(size = 11),
        legend.position = "bottom",
        strip.text = element_text(size = 7),
        #strip.text.y = element_text(angle = 180),
        #panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(size = 0.2),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.3,0.3,0.3,0.3), "cm"),
        panel.border = element_rect(size = 0.3))

P.6.2.3 <- ggplot(data_6.2, aes(x = as.character(Income_Group_5)))+
  #geom_rect(aes(ymin = min_median, ymax = max_median), xmin = 0, xmax = 6, alpha = 0.2, fill = "lightblue", inherit.aes = FALSE)+
  geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95, fill = interest, colour = interest, size = interest), 
               stat = "identity", position = position_dodge(0.5), outlier.shape = NA, width = 0.5, alpha = 1) +
  theme_bw()+
  xlab("Expenditure quintiles")+ ylab(expression(paste("Incidence for a carbon price of INR 1,000/", tCO[2], sep = "")))+
  geom_point(aes(y = mean, colour = interest), shape = 23, size = 2, fill = "white")+
  scale_y_continuous(expand = c(0,0), labels = scales::percent_format(accuracy = 1))+
  scale_x_discrete(labels = c("1 \n Poorest \n 20 Percent", "2", "3", "4", "5 \n Richest \n 20 Percent"))+
  coord_flip(ylim = c(0,0.03))+
  scale_fill_manual(guide = "none", values = c("grey", "lightgrey"))+
  scale_size_manual(guide = "none", values = c(0.3, 0.7))+
  scale_colour_manual(guide = "none", values = c("black", "darkred"))+
  theme(axis.text.y = element_text(size = 7), 
        axis.text.x = element_text(size = 7),
        axis.title  = element_text(size = 8),
        plot.title = element_text(size = 11),
        legend.position = "bottom",
        strip.text = element_text(size = 7),
        #strip.text.y = element_text(angle = 180),
        #panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(size = 0.2),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.3,0.3,0.3,0.3), "cm"),
        panel.border = element_rect(size = 0.3))

P.6.2.4 <- ggplot(data_6.2, aes(x = as.character(Income_Group_5)))+
  geom_rect(data = data_6.2.1, aes(ymin = min_median, ymax = max_median), xmin = 0, xmax = 6, alpha = 0.5, fill = "#0072B5FF", inherit.aes = FALSE)+
  geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95), 
               stat = "identity", position = position_dodge(0.5), outlier.shape = NA, width = 0.5, size = 0.3, alpha = 1, fill = "grey") +
  theme_bw()+
  xlab("Expenditure quintiles")+ ylab(expression(paste("Incidence for a carbon price of INR 1,000/", tCO[2], sep = "")))+
  geom_point(aes(y = mean), shape = 23, size = 2, fill = "white")+
  scale_y_continuous(expand = c(0,0), labels = scales::percent_format(accuracy = 1))+
  scale_x_discrete(labels = c("1 \n Poorest \n 20 Percent", "2", "3", "4", "5 \n Richest \n 20 Percent"))+
  coord_flip(ylim = c(0,0.03))+
  scale_fill_discrete(guide = "none")+
  theme(axis.text.y = element_text(size = 7), 
        axis.text.x = element_text(size = 7),
        axis.title  = element_text(size = 8),
        plot.title = element_text(size = 11),
        legend.position = "bottom",
        strip.text = element_text(size = 7),
        #strip.text.y = element_text(angle = 180),
        #panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(size = 0.2),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.3,0.3,0.3,0.3), "cm"),
        panel.border = element_rect(size = 0.3))

P.6.2.5 <- ggplot(data_6.2, aes(x = as.character(Income_Group_5)))+
  geom_rect(data = data_6.2.1, aes(ymin = min_median, ymax = max_median), xmin = 0, xmax = 6, alpha = 0.5, fill = "#0072B5FF", inherit.aes = FALSE)+
  geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95, fill = interest, colour = interest, size = interest), 
               stat = "identity", position = position_dodge(0.5), outlier.shape = NA, width = 0.5, alpha = 1) +
  theme_bw()+
  xlab("Expenditure quintiles")+ ylab(expression(paste("Incidence for a carbon price of INR 1,000/", tCO[2], sep = "")))+
  geom_point(aes(y = mean, colour = interest), shape = 23, size = 2, fill = "white")+
  scale_y_continuous(expand = c(0,0), labels = scales::percent_format(accuracy = 1))+
  scale_x_discrete(labels = c("1 \n Poorest \n 20 Percent", "2", "3", "4", "5 \n Richest \n 20 Percent"))+
  coord_flip(ylim = c(0,0.03))+
  scale_fill_manual(guide = "none", values = c("grey", "lightgrey"))+
  scale_size_manual(guide = "none", values = c(0.3, 0.7))+
  scale_colour_manual(guide = "none", values = c("black", "darkred"))+
  theme(axis.text.y = element_text(size = 7), 
        axis.text.x = element_text(size = 7),
        axis.title  = element_text(size = 8),
        plot.title = element_text(size = 11),
        legend.position = "bottom",
        strip.text = element_text(size = 7),
        #strip.text.y = element_text(angle = 180),
        #panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(size = 0.2),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.3,0.3,0.3,0.3), "cm"),
        panel.border = element_rect(size = 0.3))

jpeg("C:/Users/misl/Nextcloud/1_MCC/2_Distributional_Map/1_Slides/2_202503_India/Figure_Boxplot_%d.jpg", width = 10, height = 10, unit = "cm", res = 600)
print(P.6.2.2)
print(P.6.2.3)
print(P.6.2.4)
print(P.6.2.5)
dev.off()

data_6.3 <- data_0 %>%
  group_by(Province)%>%
  summarise(
    y5  = wtd.quantile(burden_CO2_national_1000, weights = hh_weights, probs = 0.05),
    y25 = wtd.quantile(burden_CO2_national_1000, weights = hh_weights, probs = 0.25),
    y50 = wtd.quantile(burden_CO2_national_1000, weights = hh_weights, probs = 0.5),
    y75 = wtd.quantile(burden_CO2_national_1000, weights = hh_weights, probs = 0.75),
    y95 = wtd.quantile(burden_CO2_national_1000, weights = hh_weights, probs = 0.95),
    mean = wtd.mean(   burden_CO2_national_1000, weights = hh_weights))%>%
  ungroup()

P.6.2.6 <- ggplot(data_6.3, aes(x = reorder(Province,y50)))+
  geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95), fill = "grey", 
               stat = "identity", position = position_dodge(0.5), outlier.shape = NA, width = 0.5, alpha = 1) +
  theme_bw()+
  xlab("Subnational entity")+ ylab(expression(paste("Incidence for a carbon price of INR 1,000/", tCO[2], sep = "")))+
  geom_point(aes(y = mean), shape = 23, size = 2, fill = "white")+
  scale_y_continuous(expand = c(0,0), labels = scales::percent_format(accuracy = 1))+
  #scale_x_discrete(labels = c("1 \n Poorest \n 20 Percent", "2", "3", "4", "5 \n Richest \n 20 Percent"))+
  coord_cartesian(ylim = c(0,0.03))+
  #scale_fill_manual(guide = "none", values = c("grey", "lightgrey"))+
  #scale_size_manual(guide = "none", values = c(0.3, 0.7))+
  #scale_colour_manual(guide = "none", values = c("black", "darkred"))+
  theme(axis.text.y = element_text(size = 7), 
        axis.text.x = element_text(size = 7, angle = 90, hjust = 1, vjust = 0.5),
        axis.title  = element_text(size = 8),
        plot.title = element_text(size = 11),
        legend.position = "bottom",
        strip.text = element_text(size = 7),
        #strip.text.y = element_text(angle = 180),
        #panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(size = 0.2),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.3,0.3,0.3,0.3), "cm"),
        panel.border = element_rect(size = 0.3))

jpeg("C:/Users/misl/Nextcloud/1_MCC/2_Distributional_Map/1_Slides/2_202503_India/Figure_Province.jpg", width = 18, height = 15, unit = "cm", res = 600)
print(P.6.2.6)
dev.off()

# LST 

data_6.2.0 <- data_0 %>%
  mutate(exp_CO2_national = hh_expenditures_USD_2017*burden_CO2_national_1000)%>%
  mutate(pop     = hh_size*hh_weights,
         sum_exp = exp_CO2_national*hh_weights)

sum_exp_all <- sum(data_6.2.0$sum_exp)*0.5
sum_exp_all_pc <- sum_exp_all/sum(data_6.2.0$pop)

data_6.2.0 <- data_6.2.0 %>%
  mutate(transfer = sum_exp_all_pc*hh_size)%>%
  mutate(exp_CO2_national_transfer = exp_CO2_national - transfer)%>%
  mutate(burden_CO2_national_transfer = exp_CO2_national_transfer/hh_expenditures_USD_2017)

data_6.2.0.1 <- data_6.2.0 %>%
  group_by(Income_Group_5)%>%
  summarise(y5  = wtd.quantile(burden_CO2_national_transfer, weights = hh_weights, probs = 0.05),
            y25 = wtd.quantile(burden_CO2_national_transfer, weights = hh_weights, probs = 0.25),
            y50 = wtd.quantile(burden_CO2_national_transfer, weights = hh_weights, probs = 0.5),
            y75 = wtd.quantile(burden_CO2_national_transfer, weights = hh_weights, probs = 0.75),
            y95 = wtd.quantile(burden_CO2_national_transfer, weights = hh_weights, probs = 0.95),
            mean    = wtd.mean(burden_CO2_national_transfer, weights = hh_weights))%>%
  ungroup()%>%
  mutate(interest = ifelse(Income_Group_5 == 1 | Income_Group_5 == 5,"1", "0"))%>%
  mutate(Type = "Carbon pricing and lump-sum transfer (50%)")%>%
  bind_rows(data_6.2)%>%
  mutate(Type = ifelse(!is.na(Type), Type, "Carbon pricing"))

P.6.2.X <- ggplot(data_6.2.0.1, aes(x = as.character(Income_Group_5), group = interaction(Income_Group_5, Type)))+
  geom_hline(aes(yintercept = 0))+
  #geom_rect(aes(ymin = min_median, ymax = max_median), xmin = 0, xmax = 6, alpha = 0.2, fill = "lightblue", inherit.aes = FALSE)+
  geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95, fill = Type), 
               stat = "identity", position = position_dodge(0.6), outlier.shape = NA, width = 0.5, size = 0.3, alpha = 1) +
  theme_bw()+
  xlab("Expenditure quintiles")+ ylab(expression(paste("Incidence for a carbon price of INR 1,000/", tCO[2], " (net budget change)", sep = "")))+
  geom_point(aes(y = mean), shape = 23, size = 2, fill = "white", position = position_dodge(0.6))+
  scale_y_continuous(expand = c(0,0), labels = scales::percent_format(accuracy = 1))+
  scale_x_discrete(labels = c("1 \n Poorest \n 20 Percent", "2", "3", "4", "5 \n Richest \n 20 Percent"))+
  coord_flip(ylim = c(-0.015,0.03))+
  scale_fill_nejm()+
  labs(fill = " ")+
  #scale_fill_discrete(guide = "none")+
  theme(axis.text.y = element_text(size = 8), 
        axis.text.x = element_text(size = 8),
        axis.title  = element_text(size = 8),
        plot.title = element_text(size = 11),
        legend.position = "bottom",
        strip.text = element_text(size = 7),
        #strip.text.y = element_text(angle = 180),
        #panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(size = 0.2),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.3,0.3,0.3,0.3), "cm"),
        panel.border = element_rect(size = 0.3))

jpeg("C:/Users/misl/Nextcloud/1_MCC/2_Distributional_Map/1_Slides/2_202503_India/Figure_LST.jpg", width = 14, height = 12, unit = "cm", res = 600)
print(P.6.2.X)
dev.off()

# SHAP ####

data_6.4 <- data_0 %>%
  select(-Income_Group_5, -Income_Group_10, -hh_weights, -province, -province_1, -district,-sex_hhh,
         -adults, -children, -edu_hhh, -occupation, -ind_hhh, -religion, -ethnicity, -renting, -wall, -roof, -floor,
         -cooking_fuel, -lighting_fuel, -pop, -Country, -burden_CO2_national,-distict.name)%>%
  # Debatable, but should do the job for now
  mutate_if(vars(is.character(.)), list(~ as.factor(.)))

data_0.1 <- data_6.4 %>%
  # Create noise parameter
  mutate(noise = rnorm(nrow(.),0,1))

data_0.2 <- data_0.1 %>%
  initial_split(prop = 0.8)

# Data for training
data_0.2.train <- data_0.2 %>%
  training()

# Data for testing
data_0.2.test <- data_0.2 %>%
  testing()

rm(data_0.1, data_0.2)

# Feature engineering

recipe_0 <- recipe(burden_CO2_national_1000 ~ .,
                   data = data_0.2.train)%>%
  # Deletes all columns with any NA
  step_filter_missing(all_predictors(), threshold = 0)%>%
  # Remove minimum number of columns such that correlations are less than 0.9
  step_corr(all_numeric(), -all_outcomes(), threshold = 0.9)%>%
  # should have very few unique observations for factors
  # step_other(all_nominal(), -Language, -CF, -Province, -electricity.access, threshold = 0.05)%>%
  step_dummy(all_nominal())

data_0.2.training <- recipe_0 %>%
  prep(training = data_0.2.train)%>%
  bake(new_data = NULL)

data_0.2.testing <- recipe_0 %>%
  prep(training = data_0.2.test)%>%
  bake(new_data = NULL) 

# Five-fold cross-validation

folds_1 <- vfold_cv(data_0.2.training, v = 5)

# Setup model to be tuned

model_brt <- boost_tree(
  trees         = 1000,
  tree_depth    = tune(), # maximum depth of tree
  learn_rate    = tune(), # the higher the learning rate the faster - default 0.3
  # min_n       = tune(),
  mtry          = tune(), # fraction of features to be selected for each tree (0.5/0.7/1)
  # stop_iter   = tune(),
  # sample_size = tune()
)%>%
  set_mode("regression")%>%
  set_engine("xgboost")

# Create a tuning grid - 16 different models for the tuning space

grid_0 <- grid_latin_hypercube(
  tree_depth(),
  learn_rate(c(-3,-0.5)),# tuning parameters
  mtry(c(round((ncol(data_0.2.training)-1)/2,0), ncol(data_0.2.training)-1)),
  size = 15)%>%
  # default parameters
  bind_rows(data.frame(tree_depth = 6, learn_rate = 0.3, mtry = ncol(data_0.2.training)-1))

# Tune the model - cover the entire parameter space without running every combination

print("Start computing")

doParallel::registerDoParallel()

time_1 <- Sys.time()

model_brt_1 <- tune_grid(model_brt,
                         burden_CO2_national_1000 ~ .,
                         resamples = folds_1,
                         grid      = grid_0,
                         metrics   = metric_set(mae, rmse, rsq))

time_2 <- Sys.time()

doParallel::stopImplicitCluster()

print("End computing")

# Collect metrics of tuned models

metrics_1 <- collect_metrics(model_brt_1)

model_brt_1.1 <- select_best(model_brt_1, metric = "mae")

metrics_1.1 <- metrics_1 %>%
  filter(.config == model_brt_1.1$.config[1])

# Fit best model after tuning
model_brt <- boost_tree(
  trees         = 1000,
  tree_depth    = 20,   # metrics_1.1$tree_depth[1], # 15
  learn_rate    = 0.01, # metrics_1.1$learn_rate[1], # 0.010
  mtry          = 33    # metrics_1.1$mtry[1]        # 33
)%>%
  set_mode("regression")%>%
  set_engine("xgboost")

model_brt_2 <- model_brt %>%
  fit(burden_CO2_national_1000 ~ .,
      data = data_0.2.training)

predictions_0 <- augment(model_brt_2, new_data = data_0.2.testing)
rsq_0  <- rsq(predictions_0,  truth = burden_CO2_national_1000, estimate = .pred)

data_0.2.testing_matrix <- data_0.2.testing %>%
  sample_n(.,2000)%>%
  select(-burden_CO2_national_1000)%>%
  as.matrix()

shap_1 <- predict(extract_fit_engine(model_brt_2),
                  data_0.2.testing_matrix,
                  predcontrib = TRUE,
                  approxcontrib = FALSE)

write_parquet(as_tibble(shap_1), "C:/Users/misl/Nextcloud/1_MCC/2_Distributional_Map/1_Slides/2_202503_India/SHAP_India_Snippet.parquet")

shap_1.1 <- shap_1 %>%
  as_tibble()%>%
  summarise_all(~ mean(abs(.)))%>%
  select(-BIAS)%>%
  pivot_longer(everything(), names_to = "variable", values_to = "SHAP_contribution")%>%
  arrange(desc(SHAP_contribution))%>%
  mutate(tot_contribution = sum(SHAP_contribution))%>%
  mutate(share_SHAP       = SHAP_contribution/tot_contribution)%>%
  select(-tot_contribution)

shap_1.2 <- shap_1.1 %>%
  mutate(VAR_0 = ifelse(grepl("education", variable), "Education", 
                        ifelse(grepl("Cooking_Fuel", variable), "Cooking fuel", 
                               ifelse(grepl("building_type", variable), "Building type", 
                                      ifelse(grepl("urban_type", variable), "Urban/Rural", 
                                             ifelse(grepl("renting", variable), "House ownership", 
                                                    ifelse(grepl("employment", variable), "Employment", 
                                                           ifelse(grepl("Province", variable), "Province", 
                                                                  ifelse(grepl("heating_type", variable), "Heating_Type", 
                                                                         ifelse(grepl("building_year", variable), "Building year", 
                                                                                ifelse(grepl("industry", variable), "Industry", 
                                                                                       ifelse(grepl("ausbildung", variable), "Ausbildung", 
                                                                                              ifelse(grepl("cooking_fuel", variable), "Cooking_Fuel", 
                                                                                                     ifelse(grepl("housing_type", variable), "Housing type", 
                                                                                                            ifelse(grepl("Language", variable), "Language", 
                                                                                                                   ifelse(grepl("occupation", variable), "Occupation", 
                                                                                                                          ifelse(grepl("province", variable), "Province", 
                                                                                                                                 ifelse(grepl("Ethnicity", variable), "Ethnicity", variable))))))))))))))))))%>%
  mutate(VAR_0 = ifelse(VAR_0 == "car.01", "Car ownership",
                        ifelse(VAR_0 == "hh_expenditures_USD_2014", "HH expenditures", 
                               ifelse(VAR_0 == "ac.01", "Air conditioning", VAR_0))))%>%
  mutate(VAR_0 = ifelse(grepl("Floor", variable), "Floor", 
                        ifelse(grepl("Wall", variable), "Wall",
                               ifelse(grepl("Roof", variable), "Roof",
                                      ifelse(grepl("Lighting", variable), "Lighting fuel", 
                                             ifelse(grepl("Renting", variable), "Renting",
                                                    ifelse(grepl("age_hhh", variable), "Age", VAR_0 )))))))%>%
  group_by(VAR_0)%>%
  summarise(share_SHAP = sum(share_SHAP))%>%
  ungroup()%>%
  arrange(desc(share_SHAP))%>%
  mutate(help_0 = c(1,1,1,1,1, rep(0,20)))%>%
  mutate(order = 1:n())%>%
  filter(VAR_0 != "noise")%>%
  mutate(VAR_0 = ifelse(help_0 == 0, "Other features (Sum)", VAR_0))%>%
  group_by(VAR_0)%>%
  summarise(order      = first(order),
            share_SHAP = sum(share_SHAP))%>%
  ungroup()%>%
  arrange(order)

P_6.4.1 <- ggplot(shap_1.2)+
  geom_col(aes(x = share_SHAP, y = reorder(VAR_0, desc(order)), fill = factor(order)), width = 0.7, colour = "black", size = 0.3)+
  theme_bw()+
  coord_cartesian(xlim = c(0,0.31))+
  scale_x_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0))+
  guides(fill = "none")+
  xlab("Feature importance (SHAP)")+
  ylab("Feature")+
  ggtitle(bquote(.("India (") *R^2* .("= 0.32)")))+
  scale_fill_manual(values = c("#6F99ADFF","#6F99ADFF", "#6F99ADFF", "#6F99ADFF", "#6F99ADFF","#6F99ADFF","#6F99ADFF"))+
  # ggtitle(paste0("Cluster ", data_8.5.2.C$cluster[data_8.5.2.C$Country == i],
  #                ": ", Country.Set$Country_long[Country.Set$Country == i]), " (")+
  theme(axis.text.y = element_text(size = 8), 
        axis.text.x = element_text(size = 7),
        axis.title  = element_text(size = 7),
        plot.title = element_text(size = 9),
        plot.title.position = "plot",
        legend.position = "bottom",
        # strip.text = element_text(size = 7),
        #strip.text.y = element_text(angle = 180),
        #panel.grid.major = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.ticks = element_line(size = 0.2),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.3,0.3,0.3,0.3), "cm"),
        panel.border = element_rect(size = 0.3))

jpeg("C:/Users/misl/Nextcloud/1_MCC/2_Distributional_Map/1_Slides/2_202503_India/Figure_Features.jpg", width = 10, height = 10, unit = "cm", res = 600)
print(P_6.4.1)
dev.off()

data_6.4 <- data_0 %>%
  mutate(CF = ifelse(Cooking_Fuel %in% c("Charcoal", "Gobar gas", "Kerosene", "No cooking", "Other biogas", "Others"), "Other fuel", 
                     ifelse(Cooking_Fuel %in% c("Dung", "Firewood"), "Firewood/Biomass", Cooking_Fuel)))%>%
  group_by(CF)%>%
  summarise(
    y5  = wtd.quantile(burden_CO2_national_1000, weights = hh_weights, probs = 0.05),
    y25 = wtd.quantile(burden_CO2_national_1000, weights = hh_weights, probs = 0.25),
    y50 = wtd.quantile(burden_CO2_national_1000, weights = hh_weights, probs = 0.5),
    y75 = wtd.quantile(burden_CO2_national_1000, weights = hh_weights, probs = 0.75),
    y95 = wtd.quantile(burden_CO2_national_1000, weights = hh_weights, probs = 0.95),
    mean = wtd.mean(   burden_CO2_national_1000, weights = hh_weights))%>%
  ungroup()

P.6.2.7 <- ggplot(data_6.4, aes(x = reorder(CF,y50)))+
  geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95), fill = "grey", 
               stat = "identity", position = position_dodge(0.5), outlier.shape = NA, width = 0.5, alpha = 1) +
  theme_bw()+
  xlab("Major cooking fuel")+ ylab(expression(paste("Incidence for a carbon price of INR 1,000/", tCO[2], sep = "")))+
  geom_point(aes(y = mean), shape = 23, size = 2, fill = "white")+
  scale_y_continuous(expand = c(0,0), labels = scales::percent_format(accuracy = 1))+
  #scale_x_discrete(labels = c("1 \n Poorest \n 20 Percent", "2", "3", "4", "5 \n Richest \n 20 Percent"))+
  coord_cartesian(ylim = c(0,0.04))+
  #scale_fill_manual(guide = "none", values = c("grey", "lightgrey"))+
  #scale_size_manual(guide = "none", values = c(0.3, 0.7))+
  #scale_colour_manual(guide = "none", values = c("black", "darkred"))+
  theme(axis.text.y = element_text(size = 7), 
        axis.text.x = element_text(size = 7, angle = 90, hjust = 1, vjust = 0.5),
        axis.title  = element_text(size = 8),
        plot.title = element_text(size = 11),
        legend.position = "bottom",
        strip.text = element_text(size = 7),
        #strip.text.y = element_text(angle = 180),
        #panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(size = 0.2),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.3,0.3,0.3,0.3), "cm"),
        panel.border = element_rect(size = 0.3))

jpeg("C:/Users/misl/Nextcloud/1_MCC/2_Distributional_Map/1_Slides/2_202503_India/Figure_Cooking.jpg", width = 14, height = 12, unit = "cm", res = 600)
print(P.6.2.7)
dev.off()
