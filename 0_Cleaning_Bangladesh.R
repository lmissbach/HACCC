if(!require("pacman")) install.packages("pacman")

p_load("countrycode", "haven", "Hmisc", "openxlsx", "rattle", "scales", "tidyverse", "sjlabelled")

options(scipen=999)

# Loading Data ####

# Confidential Microdata

rt001 <- read_dta("T:/MSA/papers_internal/work_in_progress/HH Surveys Paper/Bangladesh_analysis/Data/HIES2010 Bangladesh household survey/HIES2010/rt001.dta")
rt002 <- read_dta("T:/MSA/papers_internal/work_in_progress/HH Surveys Paper/Bangladesh_analysis/Data/HIES2010 Bangladesh household survey/HIES2010/rt002.dta")
rt003 <- read_dta("T:/MSA/papers_internal/work_in_progress/HH Surveys Paper/Bangladesh_analysis/Data/HIES2010 Bangladesh household survey/HIES2010/rt003.dta") # Includes Information on Work and Payment, Wages etc.
# rt004 <- read_dta("T:/MSA/papers_internal/work_in_progress/HH Surveys Paper/Bangladesh_analysis/Data/HIES2010 Bangladesh household survey/HIES2010/rt004.dta") # Includes Information on Enterprises, Customers and Revenues
# rt005 <- read_dta("T:/MSA/papers_internal/work_in_progress/HH Surveys Paper/Bangladesh_analysis/Data/HIES2010 Bangladesh household survey/HIES2010/rt005.dta") # Includes Information on Shocks (maybe interesting: Droughts, Floods, Landslides, Erosion)
# rt006 <- read_dta("T:/MSA/papers_internal/work_in_progress/HH Surveys Paper/Bangladesh_analysis/Data/HIES2010 Bangladesh household survey/HIES2010/rt006.dta") # Includes Information on Self-Produced Agricultural Products --> Excluded because of comparability
# rt007 <- read_dta("T:/MSA/papers_internal/work_in_progress/HH Surveys Paper/Bangladesh_analysis/Data/HIES2010 Bangladesh household survey/HIES2010/rt007.dta") # Includes Information on Livestock
# rt008 <- read_dta("T:/MSA/papers_internal/work_in_progress/HH Surveys Paper/Bangladesh_analysis/Data/HIES2010 Bangladesh household survey/HIES2010/rt008.dta") # Includes Information on Products from Livestock
# rt009 <- read_dta("T:/MSA/papers_internal/work_in_progress/HH Surveys Paper/Bangladesh_analysis/Data/HIES2010 Bangladesh household survey/HIES2010/rt009.dta") # Includes Information on Fishery Products
# rt010 <- read_dta("T:/MSA/papers_internal/work_in_progress/HH Surveys Paper/Bangladesh_analysis/Data/HIES2010 Bangladesh household survey/HIES2010/rt010.dta") # Includes Information on Forestry
# rt011 <- read_dta("T:/MSA/papers_internal/work_in_progress/HH Surveys Paper/Bangladesh_analysis/Data/HIES2010 Bangladesh household survey/HIES2010/rt011.dta") # Includes Information on agricultural assets
# rt012 <- read_dta("T:/MSA/papers_internal/work_in_progress/HH Surveys Paper/Bangladesh_analysis/Data/HIES2010 Bangladesh household survey/HIES2010/rt012.dta") # Includes Information on agricultural assets
rt013 <- read_dta("T:/MSA/papers_internal/work_in_progress/HH Surveys Paper/Bangladesh_analysis/Data/HIES2010 Bangladesh household survey/HIES2010/rt013.dta") # Includes Information on Remittances
rt014 <- read_dta("T:/MSA/papers_internal/work_in_progress/HH Surveys Paper/Bangladesh_analysis/Data/HIES2010 Bangladesh household survey/HIES2010/rt014.dta") # Includes Information on Micro Credits
rt015 <- read_dta("T:/MSA/papers_internal/work_in_progress/HH Surveys Paper/Bangladesh_analysis/Data/HIES2010 Bangladesh household survey/HIES2010/rt015.dta")
rt016 <- read_dta("T:/MSA/papers_internal/work_in_progress/HH Surveys Paper/Bangladesh_analysis/Data/HIES2010 Bangladesh household survey/HIES2010/rt016.dta")
rt017 <- read_dta("T:/MSA/papers_internal/work_in_progress/HH Surveys Paper/Bangladesh_analysis/Data/HIES2010 Bangladesh household survey/HIES2010/rt017.dta")
rt018 <- read_dta("T:/MSA/papers_internal/work_in_progress/HH Surveys Paper/Bangladesh_analysis/Data/HIES2010 Bangladesh household survey/HIES2010/rt018.dta")
rt019 <- read_dta("T:/MSA/papers_internal/work_in_progress/HH Surveys Paper/Bangladesh_analysis/Data/HIES2010 Bangladesh household survey/HIES2010/rt019.dta")
rt020 <- read_dta("T:/MSA/papers_internal/work_in_progress/HH Surveys Paper/Bangladesh_analysis/Data/HIES2010 Bangladesh household survey/HIES2010/rt020.dta")

# Transforming Data ####

rt001.1 <- as_tibble(rt001)%>%
  remove_all_labels()%>%
  unite("hh_id", c("psu","hhold"), sep = "")%>%
  #left_join(Urban.New, by = "urbanrur")%>%
  select(hh_id, urbanrur, wgt, s06a_q08, s06a_q14, s06a_q09, region, district, mouza, s08b_q11)%>%
  rename(hh_weights = wgt, toilet = s06a_q08, electricity.access = s06a_q14, water = s06a_q09,
         province = region, village = mouza, inc_gov_monetary_a = s08b_q11)%>%
  mutate(electricity.access = ifelse(electricity.access == 1,1,0))%>%
  mutate(urban_01 = ifelse(urbanrur == 1 | urbanrur == 3,0,1))%>%
  select(hh_id, hh_weights, urban_01, province, district, village, toilet, water, electricity.access, inc_gov_monetary_a)

rt002.1 <- as_tibble(rt002)%>%
  unite("hh_id", c("psu", "hhold"), sep = "")%>%
  select(hh_id, idcode, s01a_q02, s01a_q04, s01a_q05, s02a_q05)%>%
  remove_all_labels()

rt002.1.1 <- rt002.1%>%
  group_by(hh_id)%>%
  mutate(hh_size = n())%>%
  mutate(adults = ifelse(s01a_q04 > 18, 1, 0))%>%
  mutate(children = ifelse(s01a_q04 < 18, 1, 0))%>%
  summarise(
    hh_size = first(hh_size),
    children = sum(children),
    adults = sum(adults)
  )%>%
  ungroup()

rt002.1.2 <- rt002.1 %>%
  filter(idcode == "01")%>%
  rename(religion = s01a_q05, edu_hhh = s02a_q05, sex_hhh = s01a_q02)%>%
  select(hh_id, religion, edu_hhh, sex_hhh)

rt002.2 <- as_tibble(rt002)%>%
  unite("hh_id", c("psu", "hhold"), sep = "")%>%
  select(hh_id, starts_with("s01c"))%>%
  select(hh_id, s01c_q02, s01c_q05)%>%
  group_by(hh_id)%>%
  summarise(inc_gov_monetary = sum(s01c_q05),
            inc_gov_cash     = 0)%>%
  ungroup()

household_information <- rt001.1 %>%
  left_join(rt002.1.1)%>%
  left_join(rt002.1.2)%>%
  left_join(rt002.2)%>%
  mutate(inc_gov_monetary = inc_gov_monetary + inc_gov_monetary_a)%>%
  select(-inc_gov_monetary_a)

write_csv(household_information, "../0_Data/1_Household Data/1_Bangladesh/1_Data_Clean/household_information_Bangladesh.csv")

# expenditures

rt002.3 <- rt002 %>%
  unite("hh_id", c("psu","hhold"), sep = "")%>%
  select(hh_id, s02b_q08, starts_with("s02b_q_"), starts_with("s02b__"),
         s03a_q17, s03a_q_8, s03a_q_9, starts_with("s03a__"))%>%
  select(-c("s03a__18", "s03a__19"))%>%
  mutate_at(vars(s03a_q17, s03a_q_8, s03a_q_9, starts_with("s03a__")), list(~ .*12))%>%
  pivot_longer(-hh_id, names_to = "item_code", values_to = "expenditures_year")%>%
  filter(expenditures_year > 0)%>%
  mutate(expenditures_sp_year  = 0)%>%
  group_by(hh_id, item_code)%>%
  summarise(expenditures_year    = sum(expenditures_year),
            expenditures_sp_year = sum(expenditures_sp_year))%>%
  ungroup()

rt015.1 <- remove_all_labels(rt015) %>%
  filter(!is.na(hhold))%>%
  mutate(psu = formatC(psu, flag = "0", width = 3), sep = "")%>%
  mutate(hhold = formatC(hhold, flag = "0", width = 3), sep = "")%>%
  unite("hh_id", c("psu", "hhold"), sep = "")%>%
  rename(D.1 = s09a1d_2, D.2 = s09a1d_5, D.3 = s09a1d_8, D.4 = s09a1_11, D.5 = s09a1_14, D.6 = s09a1_17, D.7 = s09a1_20, D.8 = s09a1_23, D.9 = s09a1_26, D.10 =  s09a1_29, D.11 =  s09a1_32, D.12 =  s09a1_35, D.13 =  s09a1_38,  D.14 = s09a1_41)%>%
  rename(T.1 = s09a1d_3, T.2 = s09a1d_6, T.3 = s09a1d_9, T.4 = s09a1_12, T.5 = s09a1_15, T.6 = s09a1_18, T.7 = s09a1_21, T.8 = s09a1_24, T.9 = s09a1_27, T.10 =  s09a1_30, T.11 =  s09a1_33, T.12 =  s09a1_36, T.13 =  s09a1_39,  T.14 = s09a1_42)%>%
  mutate(   exp.1 = ifelse(T.1 == 1, D.1, 0))%>%
  mutate(exp.sp.1 = ifelse(T.1 != 1, D.1, 0))%>%
  mutate(   exp.2 = ifelse(T.2 == 1, D.2, 0))%>%
  mutate(exp.sp.2 = ifelse(T.2 != 1, D.2, 0))%>%
  mutate(   exp.3 = ifelse(T.3 == 1, D.3, 0))%>%
  mutate(exp.sp.3 = ifelse(T.3 != 1, D.3, 0))%>%
  mutate(   exp.4 = ifelse(T.4 == 1, D.4, 0))%>%
  mutate(exp.sp.4 = ifelse(T.4 != 1, D.4, 0))%>%
  mutate(   exp.5 = ifelse(T.5 == 1, D.5, 0))%>%
  mutate(exp.sp.5 = ifelse(T.5 != 1, D.5, 0))%>%
  mutate(   exp.6 = ifelse(T.6 == 1, D.6, 0))%>%
  mutate(exp.sp.6 = ifelse(T.6 != 1, D.6, 0))%>%
  mutate(   exp.7 = ifelse(T.7 == 1, D.7, 0))%>%
  mutate(exp.sp.7 = ifelse(T.7 != 1, D.7, 0))%>%
  mutate(   exp.8 = ifelse(T.8 == 1, D.8, 0))%>%
  mutate(exp.sp.8 = ifelse(T.8 != 1, D.8, 0))%>%
  mutate(   exp.9 = ifelse(T.9 == 1, D.9, 0))%>%
  mutate(exp.sp.9 = ifelse(T.9 != 1, D.9, 0))%>%
  mutate(   exp.10 = ifelse(T.10 == 1, D.10, 0))%>%
  mutate(exp.sp.10 = ifelse(T.10 != 1, D.10, 0))%>%
  mutate(   exp.11 = ifelse(T.11 == 1, D.11, 0))%>%
  mutate(exp.sp.11 = ifelse(T.11 != 1, D.11, 0))%>%
  mutate(   exp.12 = ifelse(T.12 == 1, D.12, 0))%>%
  mutate(exp.sp.12 = ifelse(T.12 != 1, D.12, 0))%>%
  mutate(   exp.13 = ifelse(T.13 == 1, D.13, 0))%>%
  mutate(exp.sp.13 = ifelse(T.13 != 1, D.13, 0))%>%
  mutate(   exp.14 = ifelse(T.14 == 1, D.14, 0))%>%
  mutate(exp.sp.14 = ifelse(T.14 != 1, D.14, 0))%>%
  select(hh_id, item, exp.1, exp.sp.1, exp.2, exp.sp.2, exp.3, exp.sp.3, exp.4, exp.sp.4, exp.5, exp.sp.5, exp.6, exp.sp.6, exp.7, exp.sp.7, exp.8, exp.sp.8, exp.9, exp.sp.9, exp.10, exp.sp.10, exp.11, exp.sp.11, exp.12, exp.sp.12, exp.13, exp.sp.13, exp.14, exp.sp.14)

rt015.1[is.na(rt015.1)] <- 0
  
rt015.2 <- rt015.1 %>%
  mutate(expenditures = exp.1 + exp.2 + exp.3 + exp.4 + exp.5 + exp.6 + exp.7 + exp.8 + exp.9 + exp.10 + exp.11 + exp.12 + exp.13 + exp.14)%>%
  mutate(expenditures.sp = exp.sp.1 + exp.sp.2 + exp.sp.3 + exp.sp.4 + exp.sp.5 + exp.sp.6 + exp.sp.7 + exp.sp.8 + exp.sp.9 + exp.sp.10 + exp.sp.11 + exp.sp.12 + exp.sp.13 + exp.sp.14)%>%
  select(hh_id, item, expenditures, expenditures.sp)%>%
  mutate(expenditures    = (expenditures/14)*365)%>%
  mutate(expenditures.sp = (expenditures.sp/14)*365)%>%
  mutate(expenditures    = expenditures / 100)%>%
  mutate(expenditures.sp = expenditures.sp / 100)%>%
  rename(expenditures_year = expenditures, expenditures_sp_year = expenditures.sp)

# rt016

rt016.1 <- remove_all_labels(rt016) %>%
  mutate(psu = formatC(psu, flag = "0", width = 3), sep = "")%>%
  mutate(hhold = formatC(hhold, flag = "0", width = 3), sep = "")%>%
  unite("hh_id", c("psu", "hhold"), sep = "" )%>%
  select(hh_id, item, s09b1w_2, s09b1w_3, s09b1w_5, s09b1w_6)%>%
  rename(W.1 = s09b1w_2, W.2 = s09b1w_5, T.1 = s09b1w_3, T.2 = s09b1w_6)%>%
  mutate(expenditures.1 = ifelse(T.1 == 1, W.1, 0))%>%
  mutate(expenditures.2 = ifelse(T.2 == 1, W.2, 0))%>%
  mutate(expenditures.sp.1 = ifelse(T.1 != 1, W.1, 0))%>%
  mutate(expenditures.sp.2 = ifelse(T.1 != 1, W.2, 0))%>%
  mutate(expenditures = expenditures.1 + expenditures.2)%>%
  mutate(expenditures.sp = expenditures.sp.1 + expenditures.sp.2)%>%
  select(hh_id, item, expenditures, expenditures.sp)%>%
  mutate(expenditures = expenditures*52/(2*100))%>%
  mutate(expenditures.sp = expenditures.sp*52/(2*100))%>%
  rename(expenditures_year = expenditures,
         expenditures_sp_year = expenditures.sp)

rt017.1 <- remove_all_labels(rt017) %>%
  mutate(psu = formatC(psu, flag = "0", width = 3), sep = "")%>%
  mutate(hhold = formatC(hhold, flag = "0", width = 3), sep = "")%>%
  unite("hh_id", c("psu", "hhold"), sep = "" )%>%
  select(hh_id, item, s09c1_q0, s09c1__1)%>%
  rename(expenditures_year = s09c1_q0, expenditures_sp_year = s09c1__1)%>%
  mutate(expenditures_year = expenditures_year*12)%>%
  mutate(expenditures_sp_year = expenditures_sp_year*12)

rt018.1 <- remove_all_labels(rt018) %>%
  mutate(psu = formatC(psu, flag = "0", width = 3), sep = "")%>%
  mutate(hhold = formatC(hhold, flag = "0", width = 3), sep = "")%>%
  unite("hh_id", c("psu", "hhold"), sep = "" )%>%
  select(hh_id, item, s09d1__1)%>%
  rename(expenditures_year = s09d1__1)%>%
  mutate(expenditures_sp_year = 0)

rt019.1 <- remove_all_labels(rt019) %>%
  mutate(psu = formatC(psu, flag = "0", width = 3), sep = "")%>%
  mutate(hhold = formatC(hhold, flag = "0", width = 3), sep = "")%>%
  unite("hh_id", c("psu", "hhold"), sep = "" )%>%
  select(hh_id, item, s09d2_q0)%>%
  rename(expenditures_year = s09d2_q0)%>%
  filter(!item == 718)%>%
  mutate(expenditures_sp_year = 0)

# Merging Purchased and Self-Produced Items

expenditures_items <- rt015.2 %>%
  bind_rows(rt016.1)%>%
  bind_rows(rt017.1)%>%
  bind_rows(rt018.1)%>%
  bind_rows(rt019.1)%>%
  rename(item_code = item)%>%
  mutate(item_code = as.character(item_code))%>%
  bind_rows(rt002.3)%>%
  filter(expenditures_year > 0 | expenditures_sp_year > 0)%>%
  group_by(hh_id, item_code)%>%
  summarise(expenditures_year = sum(expenditures_year),
            expenditures_sp_year = sum(expenditures_sp_year))%>%
  ungroup()

write_csv(expenditures_items, "../0_Data/1_Household Data/1_Bangladesh/1_Data_Clean/expenditures_items_Bangladesh.csv")

# Saving Data ####

Water.Code  <- data.frame("water" = c(1:6), 
                          "Water" = c("Supply Water", "Tubewell", "Pond/river", "Well", "Waterfall/Spring", "Other, specify"))%>%
  write_csv(., "../0_Data/1_Household Data/1_Bangladesh/2_Codes/Water.Code.csv")

Toilet.Code <- data.frame("toilet" = c(1,2,3,4,5,6), 
                          "Toilet" = c("Sanitary", "Pacca Latrine (Water Seal)", "Pacca Latrine (Pit)", "Kacha Latrine (perm)", "Kacha Latrine (temp)", "Other, specify"))%>%
  write_csv(., "../0_Data/1_Household Data/1_Bangladesh/2_Codes/Toilet.Code.csv")
Education.Code <- data.frame("edu_hhh" = c(seq(0,19)),
                             "Education" = c("No class passed", "Class 1", "Class 2", "Class 3", "Class 4", "Class 5", "Class 6", "Class 7", "Class 8", "Class 9", "SSC equivalent", "HSV equivalent", "Graduate equivalent", "Post graduate equivalent", "Medical", "Engineering", "Vocational", "Technical Education", "Nursing", "Other specify"))%>%
  write_csv(., "../0_Data/1_Household Data/1_Bangladesh/2_Codes/Education.Code.csv")
Ethnicity.Code <- data.frame("religion" = c(1,2,3,4,5), 
                             "Religion" = c("Islam", "Hinduism", "Buddhism", "Christianity", "Other, specify"))%>%
  write_csv(., "../0_Data/1_Household Data/1_Bangladesh/2_Codes/Ethnicity.Code.csv")

Province.Code <- distinct(rt001, region)%>%
  arrange(region)%>%
  rename(province = region)%>%
  mutate(Province = c("Barisal", "Chittagong", "Dhaka", "Khulna", "Rajshani", "Rajshani/Sylhet","Sylhet"))%>%
  write_csv(., "../0_Data/1_Household Data/1_Bangladesh/2_Codes/Province.Code.csv")

District.Code <- distinct(rt001, district)%>%
  arrange(district)%>%
  mutate(District = c())%>%
  write_csv(., "../0_Data/1_Household Data/1_Bangladesh/2_Codes/District.Code.csv")

Religion.Code <- distinct(rt002, s01a_q05)%>%
  arrange(s01a_q05)%>%
  rename(religion = s01a_q05)%>%
  mutate(Religion = c("Islam", "Hinduism", "Buddhism", "Christianity", "Other"))%>%
  write_csv(., "../0_Data/1_Household Data/1_Bangladesh/2_Codes/Religion.Code.csv")

# Add if necessary

# Appliances ####

rt020.1 <- remove_all_labels(rt020) %>%
  mutate(psu = formatC(psu, flag = "0", width = 3), sep = "")%>%
  mutate(hhold = formatC(hhold, flag = "0", width = 3), sep = "")%>%
  unite("hh_id", c("psu", "hhold"), sep = "")%>%
  select(hh_id, dg_code, s09e_q01, s09e_q02)

# Appliances Yes/No

Appliance.Code <- data.frame("s09e_q01" = c(0,1,2,5), "YESNO" = c(0,1,0,0))

rt020.1 <- rt020.1 %>%
  left_join(Appliance.Code, by = "s09e_q01")%>%
  mutate(s09e_q02 = ifelse(YESNO == 1, s09e_q02, 0))

rt020.2 <- rt020.1 %>%
  select(-s09e_q01, - s09e_q02)%>%
  spread(key = dg_code, value = YESNO, fill = 0)%>%
  rename(radio.01 = "561", motorcycle.01 = "565", car.01 = "566", refrigerator.01 = "567", washing_machine.01 = "568", fan.01 = "569", heater.01 = "571", tv.01 = "572", video.01 = "573", microwave.01 = "584", computer.01 = "588", mobile.01 = "587")%>%
  # Attention. Microwave is Microoven/Kitchen Items
  select(-c("562", "563", "564", "585", "586", "589", "600", "574":"583"))%>%
  right_join(select(household_information, hh_id))%>%
  # Assumption for 21 households
  mutate_at(vars(-hh_id), list(~ ifelse(is.na(.),0,.)))

write_csv(rt020.2, "../0_Data/1_Household Data/1_Bangladesh/1_Data_Clean/appliances_0_1_Bangladesh.csv")
