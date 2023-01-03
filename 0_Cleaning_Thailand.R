if(!require("pacman")) install.packages("pacman")

p_load("countrycode", "haven", "Hmisc", "openxlsx", "rattle", "scales", "sjlabelled", "tidyverse")

options(scipen=999)

# Loading Data ####

rec_01   <- read_sav("../../HH Surveys Paper/Thailand_analysis/Data_set/ses56 for general user_record 01.sav")
rec_02   <- read_sav("../../HH Surveys Paper/Thailand_analysis/Data_set/ses56 for general user_record 02.sav")
rec_03   <- read_sav("../../HH Surveys Paper/Thailand_analysis/Data_set/ses56 for general user_record 03.sav") 
rec_04   <- read_sav("../../HH Surveys Paper/Thailand_analysis/Data_set/ses56 for general user_record 04.sav")
rec_05   <- read_sav("../../HH Surveys Paper/Thailand_analysis/Data_set/ses56 for general user_record 05.sav")
rec_06   <- read_sav("../../HH Surveys Paper/Thailand_analysis/Data_set/ses56 for general user_record 06.sav")
rec_07   <- read_sav("../../HH Surveys Paper/Thailand_analysis/Data_set/ses56 for general user_record 07.sav")
rec_08   <- read_sav("../../HH Surveys Paper/Thailand_analysis/Data_set/ses56 for general user_record 08.sav")
rec_09   <- read_sav("../../HH Surveys Paper/Thailand_analysis/Data_set/ses56 for general user_record 09.sav")
rec_10   <- read_sav("../../HH Surveys Paper/Thailand_analysis/Data_set/ses56 for general user_record 10.sav")
rec_11   <- read_sav("../../HH Surveys Paper/Thailand_analysis/Data_set/ses56 for general user_record 11.sav")
rec_12   <- read_sav("../../HH Surveys Paper/Thailand_analysis/Data_set/ses56 for general user_record 12_monetary.sav")
rec_13   <- read_sav("../../HH Surveys Paper/Thailand_analysis/Data_set/ses56 for general user_record 13.sav") 
rec_14   <- read_sav("../../HH Surveys Paper/Thailand_analysis/Data_set/ses56 for general user_record 14.sav")
rec_15   <- read_sav("../../HH Surveys Paper/Thailand_analysis/Data_set/ses56 for general user_record 15.sav")
rec_16   <- read_sav("../../HH Surveys Paper/Thailand_analysis/Data_set/ses56 for general user_record 16.sav")
rec_17   <- read_sav("../../HH Surveys Paper/Thailand_analysis/Data_set/ses56 for general user_record 17.sav")
rec_18_1 <- read_sav("../../HH Surveys Paper/Thailand_analysis/Data_set/ses56 for general user_record 18 part 1.sav")
rec_18_2 <- read_sav("../../HH Surveys Paper/Thailand_analysis/Data_set/ses56 for general user_record 18 part 2.sav")
rec_18_3 <- read_sav("../../HH Surveys Paper/Thailand_analysis/Data_set/ses56 for general user_record 18 part 3.sav")
rec_25   <- read_sav("../../HH Surveys Paper/Thailand_analysis/Data_set/ses56 for general user_record 25.sav") 
rec_30   <- read_sav("../../HH Surveys Paper/Thailand_analysis/Data_set/ses56 for general user_record 30.sav")

# Transforming Data ####

Urban.Code <- data.frame(urban = c(1,2), Urban = c("Urban", "Rural"))

rec_01.1 <- rec_01 %>%
  select(NEW_HH_N, REG, CWT, AREA, C04, A04_1, A52, C05, C01)%>%
  rename(hh_id = NEW_HH_N,
         province = REG, district = CWT,
         edu_hhh = C04, urban = AREA, hh_size = A04_1, hh_weights = A52, children = C05,
         sex_hhh = C01, )%>%
  mutate(adults              = hh_size - children)%>%
  mutate(urban_01 = ifelse(urban == 1,1,0))%>%
  select(hh_id, hh_size, hh_weights, adults, children, urban_01, province, district, edu_hhh)%>%
  remove_all_labels()%>%
  mutate(edu_hhh = ifelse(is.na(edu_hhh),980,edu_hhh))

rec_02.1 <- rec_02 %>%
  select(NEW_HH_N, HM01, HM05, HM06, HM38)%>%
  rename(hh_id = NEW_HH_N)%>%
  filter(HM01 == 1)%>%
  rename(religion = HM05, ethnicity = HM06, ind_hhh = HM38)%>%
  select(hh_id, religion, ethnicity, ind_hhh)%>%
  remove_all_labels()

rec_03.1 <- rec_03 %>%
  select(NEW_HH_N, HH09, HH10, HH11, HH15)%>%
  rename(hh_id = NEW_HH_N, electricity.source = HH09, 
         cooking_fuel = HH10, 
         water = HH11, 
         toilet = HH15)%>%
  remove_all_labels()%>%
  mutate(electricity.access = ifelse(electricity.source == 1,1,0))%>%
  select(hh_id, electricity.access, cooking_fuel, water, toilet)

rec_16.1 <- rec_16 %>%
  select(NEW_HH_N, IO021, IO051, IO061)%>%
  mutate_at(vars(-NEW_HH_N), list(~ ifelse(is.na(.),0,.)))%>%
  mutate(inc_gov_monetary = IO021 + IO051 + IO061,
         inc_gov_cash     = 0)%>%
  rename(hh_id = NEW_HH_N)%>%
  select(hh_id, starts_with("inc"))

household_information <- left_join(rec_01.1, rec_02.1, by = "hh_id")%>%
  left_join(rec_03.1, by = "hh_id")%>%
  left_join(rec_16.1)

write_csv(household_information, "../0_Data/1_Household Data/1_Thailand/1_Data_Clean/household_information_Thailand.csv")

rec_03.2 <- rec_03 %>%
  select(NEW_HH_N, HH17, HH18, HH19, HH20, HH23, HH24, HH25, HH27, HH28, HH30, HH31, HH32, HH33, HH34, HH35, HH36, HH37, HH42, HH43, HH44)%>%
  rename(hh_id = NEW_HH_N,  motorcycle.01 = HH17, car.01 = HH18, truck.01 = HH19, truck1.01 = HH20, stove.g.01 = HH23, stove.e.01 = HH24, microwave.01 = HH25, refrigerator.01 = HH27, iron.01 = HH28, fan.01 = HH30, radio.01 = HH31, tv.01 = HH32, video.01 = HH33, washing_machine.01 = HH34, ac.01 = HH35, boiler.01 = HH36, computer.01 = HH37, fluorescences.01 = HH42, light_bulb.01 = HH43, fluorescences1.01 = HH44)%>%
  mutate_at(vars(-hh_id), list(~ ifelse(. > 0,1,0)))%>%
  select(-fluorescences.01, -fluorescences1.01, -light_bulb.01)

write_csv(rec_03.2, "../0_Data/1_Household Data/1_Thailand/1_Data_Clean/appliances_0_1_Thailand.csv")

# Adjusting the remaining data

rec_04.1 <- rec_04 %>%
  select(NEW_HH_N, EG011:EG122)%>%
  rename(hh_id = NEW_HH_N)

rec_05.1 <- rec_05 %>%
  select(NEW_HH_N, EG131:EG272)%>%
  rename(hh_id = NEW_HH_N)

rec_06.1 <- rec_06 %>%
  select(NEW_HH_N, EG281:EG462)%>%
  rename(hh_id = NEW_HH_N)

rec_07.1 <- rec_07 %>%
  select(NEW_HH_N, EG471:EG602)%>%
  rename(hh_id = NEW_HH_N)

rec_08.1 <- rec_08 %>%
  select(NEW_HH_N, EG611:EG7922)%>%
  rename(hh_id = NEW_HH_N)

rec_09.1 <- rec_09 %>%
  select(NEW_HH_N, EG801:EG932)%>%
  rename(hh_id = NEW_HH_N)

rec_10.1 <- rec_10 %>%
  select(NEW_HH_N, EG941:EG1122)%>%
  rename(hh_id = NEW_HH_N)

rec_11.1 <- rec_11 %>%
  select(NEW_HH_N, EG1131:EG1212)%>%
  rename(hh_id = NEW_HH_N)

rec_12.1 <- rec_12 %>%
  select(NEW_HH_N, EF01A, EF02A, EF03A, EF04A, EF05A, EF06A, EF07A, EF08A, EF09A, EF10A, EF11A, EF12A, EF13A, EF14A, EF15A, EF16A, EF17A)%>%
  rename(hh_id = NEW_HH_N, EF11000A = EF11A, EF12000A = EF12A)

rec_rest <- rec_04.1 %>%
  left_join(rec_05.1, by = "hh_id")%>%
  left_join(rec_06.1, by = "hh_id")%>%
  left_join(rec_07.1, by = "hh_id")%>%
  left_join(rec_08.1, by = "hh_id")%>%
  left_join(rec_09.1, by = "hh_id")%>%
  left_join(rec_10.1, by = "hh_id")%>%
  left_join(rec_11.1, by = "hh_id")%>%
  left_join(rec_12.1, by = "hh_id")%>%
  mutate_at(vars(-hh_id), list(~ .*12))

colnames(rec_rest) <- sub("EG", "", colnames(rec_rest))
colnames(rec_rest) <- sub("EF", "", colnames(rec_rest))
colnames(rec_rest) <- sub("A", "", colnames(rec_rest))

expenditures_items <- rec_rest %>%
  pivot_longer(-hh_id, names_to = "item_code", values_to = "expenditures_year")%>%
  filter(!is.na(expenditures_year))%>%
  arrange(hh_id, item_code)%>%
  mutate(item_code = as.numeric(item_code))%>%
  filter(expenditures_year > 0)

write_csv(expenditures_items, "../0_Data/1_Household Data/1_Thailand/1_Data_Clean/expenditures_items_Thailand.csv")

# saving data ####

Cooking.Code      <- data.frame("cooking_fuel" = as.integer(c(0,1,2,3,4,5,6)), 
                                "Cooking_Fuel" = c("No cooking", "Charcoal", "Wood", "Kerosene", "Gas", "Electricity", "Others"))%>%
  write_csv(., "../0_Data/1_Household Data/1_Thailand/2_Codes/Cooking.Code.csv")
Toilet.Code       <- data.frame("toilet" = as.integer(c(0,1,2,3,4)),
                                "Toilet" = c("No facility nearby", "Flush latrine", "Squat", "Bath flush and squat", "Pit/bucket/discharge into water/others"))%>%
  write_csv(., "../0_Data/1_Household Data/1_Thailand/2_Codes/Toilet.Code.csv")
Water.Code        <- data.frame("water" = c(seq(0,8)),
                                "Water" = c("Bottled", "Inside Piped Water Supply", "Inside piped underground water", "Outside piped or public tab", "Well or underground water", "River, stream etc", "Rain water", "Treated Water Supply", "Others"))%>%
  write_csv(., "../0_Data/1_Household Data/1_Thailand/2_Codes/Water.Code.csv")
Religion.Code   <- data.frame("religion" = c(1,2,3,4),
                              "Religion" = c("Buddhist", "Islam", "Christ", "Other"))%>%
  write_csv(., "../0_Data/1_Household Data/1_Thailand/2_Codes/Religion.Code.csv")
Ethnicity.Code  <- data.frame("ethnicity" = c(1,2,3,4,5,6,7), 
                              "Ethnicity" = c("Thai", "Malay/Yawi", "Chinese", "Mon/Burmese", "Cambodian/Souy", "Karen", "Other"))%>%
  write_csv(., "../0_Data/1_Household Data/1_Thailand/2_Codes/Ethnicity.Code.csv")
Industry.Code <- stack(attr(rec_02$HM38, 'labels'))%>%
  rename(ind_hhh = values, Industry = ind)%>%
  write_csv(., "../0_Data/1_Household Data/1_Thailand/2_Codes/Industry.Code.csv")
# Education.Code <- read_csv("../0_Data/1_Household Data/1_Thailand/2_Codes/Education.Code.csv")%>%
#   rename(edu_hhh = education, Education = Label)%>%
#   write_csv(., "../0_Data/1_Household Data/1_Thailand/2_Codes/Education.Code.csv")
Province.Code <- stack(attr(rec_01$REG, 'labels'))%>%
  rename(province = values, Province = ind)%>%
  write_csv(., "../0_Data/1_Household Data/1_Thailand/2_Codes/Province.Code.csv")
District.Code <- stack(attr(rec_01$CWT, 'labels'))%>%
  rename(district = values, District = ind)%>%
  write_csv(., "../0_Data/1_Household Data/1_Thailand/2_Codes/District.Code.csv")