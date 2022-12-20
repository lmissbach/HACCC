if(!require("pacman")) install.packages("pacman")

p_load("countrycode", "haven", "Hmisc", "openxlsx", "rattle", "scales", "tidyverse", "sjlabelled")

options(scipen=999)

# Load Data ####

# data_01    <- read_data("../0_Data/1_Household Data/2_Liberia/1_Data_Raw/AG_01_&_FILT.dta")
# data_02    <- read_data("../0_Data/1_Household Data/2_Liberia/1_Data_Raw/AG_02.dta")
# data_03    <- read_data("../0_Data/1_Household Data/2_Liberia/1_Data_Raw/AG_03.dta")
# data_04    <- read_data("../0_Data/1_Household Data/2_Liberia/1_Data_Raw/AG_04.dta")
# data_051   <- read_data("../0_Data/1_Household Data/2_Liberia/1_Data_Raw/AG_5A_1.dta")
# data_052   <- read_data("../0_Data/1_Household Data/2_Liberia/1_Data_Raw/AG_5A_2.dta")
# data_053   <- read_data("../0_Data/1_Household Data/2_Liberia/1_Data_Raw/AG_5A_3.dta")
# data_054   <- read_data("../0_Data/1_Household Data/2_Liberia/1_Data_Raw/AG_5A_4.dta")
# data_055   <- read_data("../0_Data/1_Household Data/2_Liberia/1_Data_Raw/AG_5B_1.dta")
# data_056   <- read_data("../0_Data/1_Household Data/2_Liberia/1_Data_Raw/AG_5B_2.dta")
# data_057   <- read_data("../0_Data/1_Household Data/2_Liberia/1_Data_Raw/AG_5B_3.dta")
# data_058   <- read_data("../0_Data/1_Household Data/2_Liberia/1_Data_Raw/AG_5B_4.dta")
# data_06    <- read_data("../0_Data/1_Household Data/2_Liberia/1_Data_Raw/AG_06.dta")
# data_07    <- read_data("../0_Data/1_Household Data/2_Liberia/1_Data_Raw/AG_07.dta")
# data_08    <- read_data("../0_Data/1_Household Data/2_Liberia/1_Data_Raw/AG_08.dta")
# data_09    <- read_data("../0_Data/1_Household Data/2_Liberia/1_Data_Raw/AG_09.dta")
# data_101   <- read_data("../0_Data/1_Household Data/2_Liberia/1_Data_Raw/AG_10A_1.dta")
# data_102   <- read_data("../0_Data/1_Household Data/2_Liberia/1_Data_Raw/AG_10A_2.dta")
# data_103   <- read_data("../0_Data/1_Household Data/2_Liberia/1_Data_Raw/AG_10B.dta")
# data_11    <- read_data("../0_Data/1_Household Data/2_Liberia/1_Data_Raw/AG_11.dta")
# data_12    <- read_data("../0_Data/1_Household Data/2_Liberia/1_Data_Raw/AG_12.dta")
# data_13    <- read_data("../0_Data/1_Household Data/2_Liberia/1_Data_Raw/ALLHH_A_weighted.dta")

data_a    <- read_dta("../0_Data/1_Household Data/2_Liberia/1_Data_Raw/HH_A&FILT.dta")
data_b    <- read_dta("../0_Data/1_Household Data/2_Liberia/1_Data_Raw/HH_B.dta")
data_c    <- read_dta("../0_Data/1_Household Data/2_Liberia/1_Data_Raw/HH_C.dta")
data_d    <- read_dta("../0_Data/1_Household Data/2_Liberia/1_Data_Raw/HH_D.dta")
data_e    <- read_dta("../0_Data/1_Household Data/2_Liberia/1_Data_Raw/HH_E.dta")
data_f    <- read_dta("../0_Data/1_Household Data/2_Liberia/1_Data_Raw/HH_F.dta")
# data_g    <- read_data("../0_Data/1_Household Data/2_Liberia/1_Data_Raw/HH_G.dta")
# data_h    <- read_data("../0_Data/1_Household Data/2_Liberia/1_Data_Raw/HH_H.dta")
# data_i1   <- read_data("../0_Data/1_Household Data/2_Liberia/1_Data_Raw/HH_I1.dta")
# data_i2   <- read_data("../0_Data/1_Household Data/2_Liberia/1_Data_Raw/HH_I2.dta")
data_j1   <- read_dta("../0_Data/1_Household Data/2_Liberia/1_Data_Raw/HH_J1.dta")
data_j2   <- read_dta("../0_Data/1_Household Data/2_Liberia/1_Data_Raw/HH_J2.dta")
data_k1   <- read_dta("../0_Data/1_Household Data/2_Liberia/1_Data_Raw/HH_K1.dta")
# data_k2   <- read_data("../0_Data/1_Household Data/2_Liberia/1_Data_Raw/HH_K2.dta")
data_l1a  <- read_dta("../0_Data/1_Household Data/2_Liberia/1_Data_Raw/HH_L1A.dta")
data_l1b  <- read_dta("../0_Data/1_Household Data/2_Liberia/1_Data_Raw/HH_L1B.dta")
data_l2   <- read_dta("../0_Data/1_Household Data/2_Liberia/1_Data_Raw/HH_L2.dta")
data_m    <- read_dta("../0_Data/1_Household Data/2_Liberia/1_Data_Raw/HH_M.dta")
data_n1   <- read_dta("../0_Data/1_Household Data/2_Liberia/1_Data_Raw/HH_N1.dta")
data_n2   <- read_dta("../0_Data/1_Household Data/2_Liberia/1_Data_Raw/HH_N2.dta")
# data_n3   <- read_data("../0_Data/1_Household Data/2_Liberia/1_Data_Raw/HH_N3.dta")
# data_o    <- read_data("../0_Data/1_Household Data/2_Liberia/1_Data_Raw/HH_O.dta")
# data_p1   <- read_data("../0_Data/1_Household Data/2_Liberia/1_Data_Raw/HH_P1.dta")
# data_p2   <- read_data("../0_Data/1_Household Data/2_Liberia/1_Data_Raw/HH_P2.dta")
# data_q    <- read_data("../0_Data/1_Household Data/2_Liberia/1_Data_Raw/HH_Q.dta")
# data_r    <- read_data("../0_Data/1_Household Data/2_Liberia/1_Data_Raw/HH_R.dta")
# data_s    <- read_data("../0_Data/1_Household Data/2_Liberia/1_Data_Raw/HH_S.dta")

# Transform data

data_a.1 <- data_a %>%
  select(hhid, hh_a_01b, hh_a_02b, hh_a_03b, hh_a_07)%>%
  mutate(urban_01 = ifelse(hh_a_07 == 1,1,0))%>%
  select(-hh_a_07)%>%
  rename(hh_id = hhid, Province = hh_a_01b, District = hh_a_02b, Village = hh_a_03b)

Province.Code <- distinct(data_a, hh_a_01b)%>%
  rename(Province = hh_a_01b)%>%
  mutate(province = 1:n())%>%
  write_csv(., "../0_Data/1_Household Data/2_Liberia/2_Codes/Province.Code.csv")
District.Code <- distinct(data_a, hh_a_02b)%>%
  rename(District = hh_a_02b)%>%
  mutate(district = 1:n())%>%
  write_csv(., "../0_Data/1_Household Data/2_Liberia/2_Codes/District.Code.csv")
Village.Code <- distinct(data_a, hh_a_03b)%>%
  rename(Village = hh_a_03b)%>%
  mutate(village = 1:n())%>%
  write_csv(., "../0_Data/1_Household Data/2_Liberia/2_Codes/Village.Code.csv")

data_a.2 <- data_a.1 %>%
  left_join(Province.Code)%>%
  left_join(District.Code)%>%
  left_join(Village.Code)%>%
  select(hh_id, urban_01, province, district, village)

data_b.1 <- data_b %>%
  filter(hh_b_07 == 1)%>%
  select(hhid, hh_b_02, hh_b_11, ind_id)%>%
  rename(hh_id = hhid, sex_hhh = hh_b_02, ethnicity = hh_b_11)%>%
  arrange(ind_id)%>%
  distinct(hh_id, .keep_all = TRUE)
  
data_b.2 <- data_b %>%
  mutate(adults   = ifelse(hh_b_06 > 15,1,0),
         children = ifelse(hh_b_06 < 16,1,0))%>%
  group_by(hhid)%>%
  summarise(adults = sum(adults),
            children = sum(children),
            hh_size = n(),
            hh_weights = first(weight_adjusted))%>%
  ungroup()%>%
  rename(hh_id = hhid)

data_c.1 <- data_c %>%
  select(hhid, hh_c_11, ind_id)%>%
  rename(hh_id = hhid, edu_hhh = hh_c_11)%>%
  left_join(mutate(select(data_b.1, hh_id, ind_id), head = 1))%>%
  filter(head == 1)%>%
  select(hh_id, edu_hhh)

data_e.1 <- data_e %>%
  rename(hh_id = hhid, ind_hhh = hh_e_19_2)%>%
  left_join(mutate(select(data_b.1, hh_id, ind_id), head = 1))%>%
  filter(head == 1)%>%
  select(hh_id, ind_hhh)

data_j1.1 <- data_j1 %>%
  rename(hh_id = hhid, toilet = hh_j_12, lighting_fuel = hh_j_16, cooking_fuel = hh_j_17, water = hh_j_18)%>%
  mutate(electricity.access = ifelse(hh_j_15 %in% c(2,3,4,5),1,0))%>%
  select(hh_id, toilet, water, lighting_fuel, cooking_fuel, electricity.access)%>%
  mutate(cooking_fuel = ifelse(is.na(cooking_fuel),6,cooking_fuel))

data_n1.1 <- data_n1 %>%
  rename(hh_id = hhid)%>%
  filter(hh_n_02 == 2)%>%
  filter(!hh_n_01_1 %in% c("D", "E", "H", "J","K"))%>%
  select(hh_id, everything())%>%
  mutate(inc_gov_cash     = hh_n_03_1 + (hh_n_03_2*94.43),
         inc_gov_monetary = 0)%>%
  group_by(hh_id)%>%
  summarise(inc_gov_cash     = sum(inc_gov_cash),
            inc_gov_monetary = sum(inc_gov_monetary))%>%
  ungroup()%>%
  mutate(hh_id = as.numeric(hh_id))

household_information <- data_a.2 %>%
  left_join(data_b.1)%>%
  left_join(data_b.2)%>%
  left_join(data_c.1)%>%
  left_join(data_e.1)%>%
  left_join(data_j1.1)%>%
  left_join(data_n1.1)%>%
  mutate(inc_gov_cash     = ifelse(is.na(inc_gov_cash), 0, inc_gov_cash),
         inc_gov_monetary = ifelse(is.na(inc_gov_monetary),0,inc_gov_monetary))%>%
  select(-ind_id)

write_csv(household_information, "../0_Data/1_Household Data/2_Liberia/1_Data_Clean/household_information_Liberia.csv")

# Expenditures 

data_c.2 <- data_c %>%
  select(hhid, starts_with("hh_c_39"))%>%
  rename(hh_id = hhid)%>%
  pivot_longer(-hh_id, names_to = "item_code", values_to = "expenditures_year")%>%
  mutate(expenditures_year = ifelse(is.na(expenditures_year),0,expenditures_year))%>%
  mutate(expenditures_year = ifelse(item_code %in% c("hh_c_39_a_2", "hh_c_39_b_2", "hh_c_39_c_2", "hh_c_39_d_2", "hh_c_39_e_2", "hh_c_39_f_2", "hh_c_39_g_2", "hh_c_39_h_2"),
                                    expenditures_year*94.43, expenditures_year))%>% # exchange rate from world bank
  filter(expenditures_year > 0)%>%
  group_by(hh_id, item_code)%>%
  summarise(expenditures_year    = sum(expenditures_year),
            expenditures_sp_year = 0)%>%
  ungroup()

data_d.1 <- data_d %>%
  select(hhid, hh_d_11_1, hh_d_11_2, hh_d_15_1, hh_d_15_2, hh_d_16_1, hh_d_16_2, hh_d_20_1, hh_d_20_2, hh_d_22_1, hh_d_22_2)%>%
  rename(hh_id = hhid)%>%
  mutate_at(vars(starts_with("hh_d_11"), starts_with("hh_d_15"), starts_with("hh_d_16")), list(~ .*12))%>%
  pivot_longer(-hh_id, names_to = "item_code", values_to = "expenditures_year")%>%
  mutate(expenditures_year = ifelse(is.na(expenditures_year),0, expenditures_year))%>%
  mutate(expenditures_year = ifelse(item_code %in% c("hh_d_11_2", "hh_d_15_2", "hh_d_16_2", "hh_d_20_2", "hh_d_22_2"),
                                    expenditures_year*94.43, expenditures_year))%>% # exchange rate from world bank
  group_by(hh_id, item_code)%>%
  summarise(expenditures_year    = sum(expenditures_year),
            expenditures_sp_year = 0)%>%
  ungroup()

data_f.1 <- data_f %>%
  rename(hh_id = hhid)%>%
  select(hh_id, ends_with("_1"), ends_with("_2"))%>%
  mutate_at(vars(ends_with("_2")), list(~ .*94.43))%>%
  pivot_longer(-hh_id, names_to = "item_code", values_to = "expenditures_year")%>%
  filter(!is.na(expenditures_year))%>%
  mutate(expenditures_year = expenditures_year*365/7)%>%
  group_by(hh_id, item_code)%>%
  summarise(expenditures_year    = sum(expenditures_year),
            expenditures_sp_year = 0)%>%
  ungroup()

data_j1.2 <- data_j1 %>%
  select(hhid, hh_j_04_1, hh_j_04_2, hh_j_06_1, hh_j_06_2, hh_j_14_1, hh_j_14_2)%>%
  rename(hh_id = hhid)%>%
  mutate(hh_j_04_1 = hh_j_04_1*12,
         hh_j_04_2 = hh_j_04_2*12)%>%
  mutate_at(vars(ends_with("_2")), list(~ .*94.43))%>%
  pivot_longer(-hh_id, names_to = "item_code", values_to = "expenditures_year")%>%
  filter(!is.na(expenditures_year))%>%
  mutate(expenditures_sp_year = 0)

data_j2.1 <- data_j2 %>%
  rename(hh_id = hhid, item_code = hh_j_27_1, expenditures_LD_year = hh_j_28_1, expenditures_USD_year = hh_j_28_2)%>%
  select(hh_id, item_code, expenditures_LD_year, expenditures_USD_year)%>%
  filter(!is.na(expenditures_LD_year) | !is.na(expenditures_USD_year))%>%
  mutate_at(vars(starts_with("expenditures")), list(~ ifelse(is.na(.),0,.)))%>%
  mutate_at(vars(starts_with("expenditures")), list(~ .*365/7))

data_k1.1 <- data_k1 %>%
  rename(hh_id = hhid, item_code = hh_k_00_b, expenditures_LD_year = hh_k_05_1, expenditures_USD_year = hh_k_05_2)%>%
  select(hh_id, item_code, expenditures_LD_year, expenditures_USD_year)%>%
  filter(!is.na(expenditures_LD_year) | !is.na(expenditures_USD_year))%>%
  mutate_at(vars(starts_with("expenditures")), list(~ ifelse(is.na(.),0,.)))%>%
  mutate_at(vars(starts_with("expenditures")), list(~ .*365/7))

data_l1a.1 <- data_l1a %>%
  rename(hh_id = hhid, item_code = hh_l1a_00, expenditures_LD_year = hh_l1a_02_1, expenditures_USD_year = hh_l1a_02_2)%>%
  select(hh_id, item_code, expenditures_LD_year, expenditures_USD_year)%>%
  filter(!is.na(expenditures_LD_year) | !is.na(expenditures_USD_year))%>%
  mutate_at(vars(starts_with("expenditures")), list(~ ifelse(is.na(.),0,.)))%>%
  mutate_at(vars(starts_with("expenditures")), list(~ .*365/7))%>%
  mutate(item_code = item_code*100)

data_l1b.1 <- data_l1b %>%
  rename(hh_id = hhid, item_code = hh_l1b_00, expenditures_LD_year = hh_l1b_02_1, expenditures_USD_year = hh_l1b_02_2)%>%
  select(hh_id, item_code, expenditures_LD_year, expenditures_USD_year)%>%
  filter(!is.na(expenditures_LD_year) | !is.na(expenditures_USD_year))%>%
  mutate_at(vars(starts_with("expenditures")), list(~ ifelse(is.na(.),0,.)))%>%
  mutate_at(vars(starts_with("expenditures")), list(~ .*12))%>%
  mutate(item_code = item_code*100)

data_l2.1 <- data_l2 %>%
  rename(hh_id = hhid, item_code = hh_l2_00, expenditures_LD_year = hh_l2_02_1, expenditures_USD_year = hh_l2_02_2)%>%
  select(hh_id, item_code, expenditures_LD_year, expenditures_USD_year)%>%
  filter(!is.na(expenditures_LD_year) | !is.na(expenditures_USD_year))%>%
  mutate_at(vars(starts_with("expenditures")), list(~ ifelse(is.na(.),0,.)))%>%
  mutate(item_code = item_code*100)

data_m2 <- data_m %>%
  rename(hh_id = hhid, item_code = hh_m_00, expenditures_LD_year = hh_m_03_1, expenditures_USD_year = hh_m_03_2)%>%
  select(hh_id, item_code, expenditures_LD_year, expenditures_USD_year)%>%
  filter(!is.na(expenditures_LD_year) | !is.na(expenditures_USD_year))%>%
  mutate_at(vars(starts_with("expenditures")), list(~ ifelse(is.na(.),0,.)))%>%
  mutate(item_code = item_code*100)

exp_data_1 <- bind_rows(data_c.2, data_d.1, data_f.1, data_j1.2)

exp_data_2 <- bind_rows(data_j2.1, data_k1.1, data_l1a.1, data_l1b.1, data_l2.1, data_m2)%>%
  mutate(expenditures_year    = expenditures_LD_year + (expenditures_USD_year*94.43),
         expenditures_sp_year = 0)%>%
  filter(expenditures_year > 0)%>%
  select(hh_id, item_code, expenditures_year, expenditures_sp_year)%>%
  mutate(item_code = as.character(item_code))

expenditures_items <- bind_rows(exp_data_1, exp_data_2)%>%
  arrange(hh_id, item_code)

write_csv(expenditures_items, "../0_Data/1_Household Data/2_Liberia/1_Data_Clean/expenditures_items_Liberia.csv")

# Appliances

data_m1 <- data_m %>%
  rename(hh_id = hhid)%>%
  select(hh_id, hh_m_00, hh_m_01_1, hh_m_01_2)%>%
  mutate(hh_m_01_2 = ifelse(is.na(hh_m_01_2), 0, hh_m_01_2))%>%
  mutate(hh_m_01_2 = ifelse(hh_m_01_2 > 0,1,0))%>%
  mutate(name = ifelse(hh_m_00 == 401, "radio.01",
                       ifelse(hh_m_00 == 402, "mobile.01",
                              ifelse(hh_m_00 == 403, "refrigerator.01",
                                     ifelse(hh_m_00 == 405, "tv.01",
                                            ifelse(hh_m_00 == 414, "computer.01",
                                                   ifelse(hh_m_00 == 418, "stove.01",
                                                          ifelse(hh_m_00 == 422 | hh_m_00 == 423, "car.01",
                                                                 ifelse(hh_m_00 == 424, "motorcycle.01",
                                                                        ifelse(hh_m_00 == 426, "fan.01",
                                                                               ifelse(hh_m_00 == 427, "ac.01", NA)))))))))))%>%
  filter(!is.na(name))%>%
  group_by(hh_id, name)%>%
  summarise(hh_m_01_2 = max(hh_m_01_2))%>%
  ungroup()%>%
  select(hh_id, name, hh_m_01_2)%>%
  pivot_wider(names_from = "name", values_from = "hh_m_01_2")

write_csv(data_m1, "../0_Data/1_Household Data/2_Liberia/1_Data_Clean/appliances_0_1_Liberia.csv")

# Codes

Gender.Code <- distinct(data_b, hh_b_02)%>%
  arrange(hh_b_02)%>%
  rename(sex_hhh = hh_b_02)%>%
  mutate(Gender = c("Male", "Female"))%>%
  write_csv(., "../0_Data/1_Household Data/2_Liberia/2_Codes/Gender.Code.csv")
Ethnicity.Code <- distinct(data_b, hh_b_11)%>%
  arrange(hh_b_11)%>%
  rename(ethnicity = hh_b_11)%>%
  mutate(Ethnicity = c("Gio", "Kpelle", "Mano", "Vai", "Kru", "Gola", "Grebo", "Bassa", "Mandingo", "Mende", "Krahn",
                       "Lorma", "Dei", "Kissi", "Belle", "Gbandi", "Sarpo", "Congo", "Other", "None"))%>%
  write_csv(., "../0_Data/1_Household Data/2_Liberia/2_Codes/Ethnicity.Code.csv")
Education.Code <- distinct(data_c, hh_c_11)%>%
  arrange(hh_c_11)%>%
  rename(edu_hhh = hh_c_11)%>%
  mutate(Education = c("No schooling", rep("Primary", 6), rep("Junior High",3), rep("Senior High",3), rep("University",5), "Masters and above", NA))%>%
  write_csv(., "../0_Data/1_Household Data/2_Liberia/2_Codes/Education.Code.csv")

Cooking.Code <- distinct(data_j1, hh_j_17)%>%
  arrange(hh_j_17)%>%
  rename(cooking_fuel = hh_j_17)%>%
  mutate(Cooking_Fuel = c("Electricity", "Kerosene", "Gas", "Charcoal", "Wood", "Other", NA))%>%
  write_csv(., "../0_Data/1_Household Data/2_Liberia/2_Codes/Cooking.Code.csv")

Lighting.Code <- distinct(data_j1, hh_j_16)%>%
  arrange(hh_j_16)%>%
  rename(lighting_fuel = hh_j_16)%>%
  mutate(Lighting_Fuel = c("None", "Electricity", "Kerosene", "Candle", "Palm Oil", "Chinese Lamp", "Wood", "Torchlight", "Other"))%>%
  write_csv(., "../0_Data/1_Household Data/2_Liberia/2_Codes/Lighting.Code.csv")

Toilet.Code <- distinct(data_j1, hh_j_12)%>%
  arrange(hh_j_12)%>%
  rename(toilet = hh_j_12)%>%
  mutate(Toilet = c("Flush toilet", "Flush toilet shared", "Covered pit latrine", "Open pit latrine", "Ventilated improved pit latrine", "Bush/river", "Other"))%>%
  write_csv(., "../0_Data/1_Household Data/2_Liberia/2_Codes/Toilet.Code.csv")

Water.Code <- distinct(data_j1, hh_j_18)%>%
  arrange(hh_j_18)%>%
  rename(water = hh_j_18)%>%
  mutate(Water = c("Pipe or Pump", "Pipe or pump outdoors", "Public standpipe", "Boreholes/tubewell",
                   "Neighboring household", "Water vendor", "Push push water vendor", "Closed well", "Open well", "River, lake", "Rainwater", "Mineral water", "Other"))%>%
  write_csv(., "../0_Data/1_Household Data/2_Liberia/2_Codes/Water.Code.csv")

# Item-Codes

Item.C <- data_c.2 %>%
  distinct(item_code)%>%
  arrange(item_code)%>%
  mutate(Type = "C")

Item.D <- data_d.1 %>%
  distinct(item_code)%>%
  arrange(item_code)%>%
  mutate(Type = "D")

Item.F <- data_f.1 %>%
  distinct(item_code)%>%
  arrange(item_code)%>%
  mutate(Type = "F")

Item.J1 <- data_j1.2 %>%
  distinct(item_code)%>%
  arrange(item_code)%>%
  mutate(Type = "J1")

Item.J2 <- data_j2 %>%
  distinct(hh_j_27_1, hh_j_27_2)%>%
  mutate(Type = "J2")%>%
  rename(item_code = hh_j_27_1, item_name = hh_j_27_2)%>%
  mutate(item_code = as.character(item_code))

Item.K <- data_k1 %>%
  distinct(hh_k_00_b, hh_k_00_a)%>%
  mutate(Type = "K")%>%
  rename(item_code = hh_k_00_b, item_name = hh_k_00_a)%>%
  mutate(item_code = as.character(item_code))

Item.L1a <- data_l1a %>%
  distinct(hh_l1a_00, hh_l1a_01_1)%>%
  rename(item_code = hh_l1a_00, item_name = hh_l1a_01_1)%>%
  mutate(Type = "L1A")%>%
  mutate(item_code = item_code*100)%>%
  mutate(item_code = as.character(item_code))

Item.L1b <- data_l1b %>%
  distinct(hh_l1b_00, hh_l1b_01_1)%>%
  rename(item_code = hh_l1b_00, item_name = hh_l1b_01_1)%>%
  mutate(Type = "L1B")%>%
  mutate(item_code = item_code*100)%>%
  mutate(item_code = as.character(item_code))

Item.L2 <- data_l2 %>%
  distinct(hh_l2_00, hh_l2_01_1)%>%
  rename(item_code = hh_l2_00, item_name = hh_l2_01_1)%>%
  mutate(Type = "L2")%>%
  mutate(item_code = item_code*100)%>%
  mutate(item_code = as.character(item_code))

Item.M <- data_m %>%
  distinct(hh_m_00, hh_m_01_1)%>%
  rename(item_code = hh_m_00, item_name = hh_m_01_1)%>%
  mutate(Type = "M")%>%
  mutate(item_code = item_code*100)%>%
  mutate(item_code = as.character(item_code))

Item.Codes <- bind_rows(Item.C, Item.D, Item.F, Item.J1, Item.J2, Item.K, Item.L1a, Item.L1b, Item.L2, Item.M)

# write.xlsx(Item.Codes, "../0_Data/1_Household Data/2_Liberia/3_Matching_Tables/Item_Code_Description_Liberia.xlsx")
