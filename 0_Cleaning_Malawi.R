library("haven")
library("Hmisc")
library("openxlsx")
library("rattle")
library("scales")
library("sjlabelled")
library("tidyverse")
options(scipen=999)

# To clean household data from Malawi
# Author: L. Missbach (missbach@mcc-berlin.net)

# Load Data ####

data_a <- read_dta("../0_Data/1_Household Data/2_Malawi/1_Data_Raw/hh_mod_a_filt.dta")
data_b    <- read_dta("../0_Data/1_Household Data/2_Malawi/1_Data_Raw/HH_MOD_B.dta")
data_c    <- read_dta("../0_Data/1_Household Data/2_Malawi/1_Data_Raw/HH_MOD_C.dta")
data_d    <- read_dta("../0_Data/1_Household Data/2_Malawi/1_Data_Raw/HH_MOD_D.dta")
data_e    <- read_dta("../0_Data/1_Household Data/2_Malawi/1_Data_Raw/HH_MOD_E.dta")
data_f    <- read_dta("../0_Data/1_Household Data/2_Malawi/1_Data_Raw/HH_MOD_F.dta")
data_f1   <- read_dta("../0_Data/1_Household Data/2_Malawi/1_Data_Raw/HH_MOD_F1.dta")
data_g1   <- read_dta("../0_Data/1_Household Data/2_Malawi/1_Data_Raw/HH_MOD_G1.dta")
# data_g2   <- read_dta("../0_Data/1_Household Data/2_Malawi/1_Data_Raw/HH_MOD_G2.dta")
# data_g3   <- read_dta("../0_Data/1_Household Data/2_Malawi/1_Data_Raw/HH_MOD_G3.dta")
# data_h    <- read_dta("../0_Data/1_Household Data/2_Malawi/1_Data_Raw/HH_MOD_H.dta")
data_i1   <- read_dta("../0_Data/1_Household Data/2_Malawi/1_Data_Raw/HH_MOD_I1.dta")
data_i2   <- read_dta("../0_Data/1_Household Data/2_Malawi/1_Data_Raw/HH_MOD_I2.dta")
data_j    <- read_dta("../0_Data/1_Household Data/2_Malawi/1_Data_Raw/HH_MOD_J.dta")
data_k1   <- read_dta("../0_Data/1_Household Data/2_Malawi/1_Data_Raw/HH_MOD_K1.dta")
# data_k2   <- read_dta("../0_Data/1_Household Data/2_Malawi/1_Data_Raw/HH_MOD_K2.dta")
data_l    <- read_dta("../0_Data/1_Household Data/2_Malawi/1_Data_Raw/HH_MOD_L.dta")
# data_m    <- read_dta("../0_Data/1_Household Data/2_Malawi/1_Data_Raw/HH_MOD_M.dta")
# data_meta <- read_dta("../0_Data/1_Household Data/2_Malawi/1_Data_Raw/HH_MOD_META.dta")
# data_n1   <- read_dta("../0_Data/1_Household Data/2_Malawi/1_Data_Raw/HH_MOD_N1.dta")
# data_n2   <- read_dta("../0_Data/1_Household Data/2_Malawi/1_Data_Raw/HH_MOD_N2.dta")
# data_o    <- read_dta("../0_Data/1_Household Data/2_Malawi/1_Data_Raw/HH_MOD_O.dta")
data_p    <- read_dta("../0_Data/1_Household Data/2_Malawi/1_Data_Raw/HH_MOD_P.dta")
# data_q    <- read_dta("../0_Data/1_Household Data/2_Malawi/1_Data_Raw/HH_MOD_Q.dta")
data_r    <- read_dta("../0_Data/1_Household Data/2_Malawi/1_Data_Raw/HH_MOD_R.dta")
# data_s1   <- read_dta("../0_Data/1_Household Data/2_Malawi/1_Data_Raw/HH_MOD_S1.dta")
# data_s2   <- read_dta("../0_Data/1_Household Data/2_Malawi/1_Data_Raw/HH_MOD_S2.dta")
# data_t    <- read_dta("../0_Data/1_Household Data/2_Malawi/1_Data_Raw/HH_MOD_T.dta")
# data_u    <- read_dta("../0_Data/1_Household Data/2_Malawi/1_Data_Raw/HH_MOD_U.dta")
# data_v    <- read_dta("../0_Data/1_Household Data/2_Malawi/1_Data_Raw/HH_MOD_V.dta")
# data_w    <- read_dta("../0_Data/1_Household Data/2_Malawi/1_Data_Raw/HH_MOD_W.dta")
# data_x    <- read_dta("../0_Data/1_Household Data/2_Malawi/1_Data_Raw/HH_MOD_X.dta")


# Transform Data ####

# Households ####

data_a.1 <- data_a %>%
  rename(hh_id = case_id, hh_weights = hh_wgt, hh_size = hhsize)%>%
  mutate(urban_01 = ifelse(reside == 1,1,0))%>%
  select(hh_id, district, urban_01, hh_weights, hh_size)

data_b.1 <- data_b %>%
  rename(hh_id = case_id, sex_hhh = hh_b03, age_hhh = hh_b05a, language = hh_b22, ethnicity = hh_b23)%>%
  filter(hh_b04 == 1)%>%
  select(hh_id, sex_hhh, age_hhh, language, ethnicity)%>%
  mutate(language = ifelse(is.na(language),15,language))

data_b.2 <- data_b %>%
  rename(hh_id = case_id)%>%
  mutate(adults = ifelse(hh_b05a > 15,1,0),
         children = ifelse(hh_b05a < 16,1,0))%>%
  group_by(hh_id)%>%
  summarise(adults = sum(adults),
            children = sum(children))%>%
  ungroup()

data_b.3 <- data_b %>%
  select(case_id, PID, hh_b04)%>%
  filter(hh_b04 == 1)%>%
  rename(hh_id = case_id)

data_c.1 <- data_c %>%
  rename(hh_id = case_id)%>%
  left_join(data_b.3)%>%
  filter(!is.na(hh_b04))%>%
  rename(edu_hhh = hh_c09)%>%
  select(hh_id, edu_hhh)%>%
  mutate(edu_hhh = ifelse(is.na(edu_hhh),1,edu_hhh))

data_e.1 <- data_e %>%
  rename(hh_id = case_id, ind_hhh = hh_e20b)%>%
  left_join(data_b.3)%>%
  filter(!is.na(hh_b04))%>%
  select(hh_id, ind_hhh)

data_f.1 <- data_f %>%
  rename(hh_id = case_id)%>%
  select(hh_id, hh_f11, hh_f12, hh_f19, hh_f36, hh_f41)%>%
  rename(lighting_fuel = hh_f11, cooking_fuel = hh_f12, water = hh_f36, toilet = hh_f41)%>%
  mutate(electricity.access = ifelse(hh_f19 == 1,1,0))%>%
  select(hh_id, lighting_fuel, cooking_fuel, water, toilet, electricity.access)

data_p.1 <- data_p %>%
  rename(hh_id = case_id)%>%
  filter(hh_p0a == 105)%>%
  rename(inc_gov_monetary = hh_p02)

data_r.1 <- data_r %>%
  rename(hh_id = case_id)%>%
  mutate(inc_gov_cash = ifelse(hh_r0a %in% c(111,112), hh_r02a, 0),
         inc_gov_monetary = ifelse(hh_r0a %in% c(1031, 1032, 104, 108, 1091), hh_r02a, 
                                   ifelse(hh_r0a %in% c(101, 102, 105, 106, 107) & hh_r01 == 1, 1,0)))%>%
  bind_rows(data_p.1)%>%
  group_by(hh_id)%>%
  summarise(inc_gov_cash = sum(inc_gov_cash, na.rm = TRUE),
            inc_gov_monetary = sum(inc_gov_monetary, na.rm = TRUE))%>%
  ungroup()

household_information <- data_a.1 %>%
  left_join(data_b.1)%>%
  left_join(data_b.2)%>%
  left_join(data_c.1)%>%
  left_join(data_e.1)%>%
  left_join(data_f.1)%>%
  left_join(data_r.1)
  

write_csv(household_information, "../0_Data/1_Household Data/2_Malawi/1_Data_Clean/household_information_Malawi.csv")

# Expenditures ####

data_c.2 <- data_c %>%
  rename(hh_id = case_id)%>%
  select(hh_id, starts_with("hh_c22"))%>%
  pivot_longer(-hh_id, names_to = "item_code", values_to = "expenditures_year")%>%
  filter(!is.na(expenditures_year) & expenditures_year > 0)%>%
  group_by(hh_id, item_code)%>%
  summarise(expenditures_year = sum(expenditures_year))%>%
  ungroup()%>%
  arrange(item_code)

data_d.1 <- data_d %>%
  rename(hh_id = case_id)%>%
  select(hh_id, hh_d10:hh_d21, hh_d48)%>%
  select(-hh_d13, - hh_d17, - hh_d18)%>%
  pivot_longer(-hh_id, names_to = "item_code", values_to = "expenditures_year")%>%
  filter(expenditures_year >0)%>%
  mutate(expenditures_year = ifelse(item_code %in% c("hh_d10", "hh_d11", "hh_d12"), expenditures_year*12, expenditures_year))%>%
  group_by(hh_id, item_code)%>%
  summarise(expenditures_year = sum(expenditures_year))%>%
  ungroup()%>%
  arrange(item_code)

data_f.2 <- data_f %>%
  rename(hh_id = case_id)%>%
  select(hh_id, starts_with("hh_f04"))%>%
  mutate(hh_f04_rent = ifelse(hh_f04b == 1, hh_f04a,
                              ifelse(hh_f04b == 2, hh_f04a*12, hh_f04a)))%>%
  select(hh_id, hh_f04_rent, hh_f04_4, hh_f04_6)%>%
  pivot_longer(-hh_id, names_to = "item_code", values_to = "expenditures_year")%>%
  filter(!is.na(expenditures_year) & expenditures_year > 0)%>%
  arrange(item_code)

data_f.3 <- data_f %>%
  rename(hh_id = case_id)%>%
  select(hh_id, 
         hh_f25, hh_f26a, hh_f26b,
         hh_f32, hh_f33a, hh_f33b,
         hh_f35)%>%
  mutate(hh_f_electricity = ifelse(hh_f26b == 3, hh_f25*365*hh_f26a,
                                   ifelse(hh_f26b == 4, hh_f25*52*hh_f26a,
                                          ifelse(hh_f26b == 5, hh_f25*12*hh_f26a,0))))%>%
  mutate(hh_f_telephone = hh_f32*12,
         hh_f_cell_phone = hh_f35*12)%>%
  select(hh_id, hh_f_electricity, hh_f_telephone, hh_f_cell_phone)%>%
  pivot_longer(-hh_id, names_to = "item_code", values_to = "expenditures_year")%>%
  filter(expenditures_year > 0 & !is.na(expenditures_year))%>%
  arrange(item_code)

data_g1.1 <- data_g1 %>%
  rename(hh_id = case_id, item_code = hh_g02, expenditures = hh_g05)%>%
  select(hh_id, item_code, expenditures)%>%
  filter(expenditures > 0 & !is.na(expenditures))%>%
  mutate(expenditures_year = expenditures*52)%>%
  select(-expenditures)%>%
  remove_all_labels()%>%
  mutate(item_code = paste0("A", item_code))%>%
  arrange(item_code)

data_i1.1 <- data_i1 %>%
  rename(hh_id = case_id, item_code = hh_i02, expenditures = hh_i03)%>%
  mutate(expenditures_year = expenditures*52)%>%
  select(hh_id, item_code, expenditures_year)%>%
  filter(expenditures_year > 0 & !is.na(expenditures_year))%>%
  remove_all_labels()%>%
  mutate(item_code = paste0("B",item_code))%>%
  arrange(item_code)

data_i2.1 <- data_i2 %>%
  rename(hh_id = case_id, item_code = hh_i05, expenditures = hh_i06)%>%
  mutate(expenditures_year = expenditures*12)%>%
  select(hh_id, item_code, expenditures_year)%>%
  filter(expenditures_year > 0 & !is.na(expenditures_year))%>%
  remove_all_labels()%>%
  mutate(item_code = paste0("B", item_code))%>%
  arrange(item_code)

data_j.1 <- data_j %>%
  rename(hh_id = case_id, item_code = hh_j02, expenditures = hh_j03)%>%
  mutate(expenditures_year = expenditures*4)%>%
  select(hh_id, item_code, expenditures_year)%>%
  filter(expenditures_year > 0 & !is.na(expenditures_year))%>%
  remove_all_labels()%>%
  mutate(item_code = paste0("B", item_code))%>%
  arrange(item_code)

data_k1.1 <- data_k1 %>%
  rename(hh_id = case_id, item_code = hh_k02, expenditures_year = hh_k03)%>%
  select(hh_id, item_code, expenditures_year)%>%
  filter(expenditures_year > 0 & !is.na(expenditures_year))%>%
  remove_all_labels()%>%
  mutate(item_code = paste0("B", item_code))%>%
  arrange(item_code)

data_l.2 <- data_l %>%
  rename(hh_id = case_id, item_code = hh_l02, expenditures_year = hh_l07)%>%
  select(hh_id, item_code, expenditures_year)%>%
  filter(expenditures_year > 0 & !is.na(expenditures_year))%>%
  remove_all_labels()%>%
  mutate(item_code = paste0("B", item_code))%>%
  arrange(item_code)

data_expenditures <- bind_rows(data_c.2, data_d.1, data_f.2, data_f.3)%>%
  bind_rows(data_g1.1)%>%
  bind_rows(data_i1.1)%>%
  bind_rows(data_i2.1)%>%
  bind_rows(data_j.1)%>%
  bind_rows(data_k1.1)%>%
  bind_rows(data_l.2)

write_csv(data_expenditures, "../0_Data/1_Household Data/2_Malawi/1_Data_Clean/expenditures_items_Malawi.csv")

# Appliances ####

data_l.1 <- data_l %>%
  rename(hh_id = case_id)%>%
  mutate(value = ifelse(hh_l01 == 1,1,0))%>%
  mutate(appliance = ifelse(hh_l02 == 505, "fan.01",
                            ifelse(hh_l02 == 506, "ac.01",
                                   ifelse(hh_l02 == 507 | hh_l02 == 5801, "radio.01",
                                          ifelse(hh_l02 == 509, "tv.01",
                                                 ifelse(hh_l02 == 512, "stove.k.01",
                                                        ifelse(hh_l02 == 513, "stove.g.01",
                                                               ifelse(hh_l02 == 514, "refrigerator.01",
                                                                      ifelse(hh_l02 == 515, "washing_machine.01",
                                                                             ifelse(hh_l02 == 517, "motorcycle.01",
                                                                                    ifelse(hh_l02 == 518, "car.01",
                                                                                           ifelse(hh_l02 == 529, "computer.01", NA))))))))))))%>%
  filter(!is.na(appliance))%>%
  select(hh_id, appliance, value)%>%
  pivot_wider(names_from = "appliance", values_from = "value")

write_csv(data_l.1, "../0_Data/1_Household Data/2_Malawi/1_Data_Clean/appliances_0_1_Malawi.csv")

# Codes ####

Item.Codes <- distinct(data_expenditures, item_code)

Item.Codes.A <- stack(attr(data_g1$hh_g02, 'labels'))%>%
  rename(item_code = values, item_name_A = ind)%>%
  mutate(item_code = paste0("A", item_code))
Item.Codes.B1 <- stack(attr(data_i1$hh_i02, 'labels'))%>%
  rename(item_code = values, item_name_B = ind)%>%
  mutate(item_code = paste0("B", item_code))
Item.Codes.B2 <- stack(attr(data_i2$hh_i05, 'labels'))%>%
  rename(item_code = values, item_name_B = ind)%>%
  mutate(item_code = paste0("B", item_code))
Item.Codes.B3 <- stack(attr(data_j$hh_j02, 'labels'))%>%
  rename(item_code = values, item_name_B = ind)%>%
  mutate(item_code = paste0("B", item_code))
Item.Codes.B4 <- stack(attr(data_k1$hh_k02, 'labels'))%>%
  rename(item_code = values, item_name_B = ind)%>%
  mutate(item_code = paste0("B", item_code))
Item.Codes.B5 <- stack(attr(data_l$hh_l02, 'labels'))%>%
  rename(item_code = values, item_name_B = ind)%>%
  mutate(item_code = paste0("B", item_code))
Item.Codes.B <- bind_rows(Item.Codes.B1, Item.Codes.B2, Item.Codes.B3, Item.Codes.B4, Item.Codes.B5)

Item.Codes.final <- Item.Codes %>%
  left_join(Item.Codes.A)%>%
  left_join(Item.Codes.B)%>%
  mutate(item_name = ifelse(!is.na(item_name_A), as.character(item_name_A), as.character(item_name_B)))%>%
  select(-item_name_A, -item_name_B)

write.xlsx(Item.Codes.final, "../0_Data/1_Household Data/2_Malawi/3_Matching_Tables/Item_Codes_Description_Malawi.xlsx")

Cooking.Code <- stack(attr(data_f$hh_f12, 'labels'))%>%
  rename(cooking_fuel = values, Cooking_Fuel = ind)%>%
  write_csv(., "../0_Data/1_Household Data/2_Malawi/2_Codes/Cooking.Code.csv")
Lighting.Code <- stack(attr(data_f$hh_f11, 'labels'))%>%
  rename(lighting_fuel = values, Lighting_Fuel = ind)%>%
  write_csv(., "../0_Data/1_Household Data/2_Malawi/2_Codes/Lighting.Code.csv")
Water.Code <- stack(attr(data_f$hh_f36, 'labels'))%>%
  rename(water = values, Water = ind)%>%
  write_csv(., "../0_Data/1_Household Data/2_Malawi/2_Codes/Water.Code.csv")
Toilet.Code <- stack(attr(data_f$hh_f41, 'labels'))%>%
  rename(toilet = values, Toilet = ind)%>%
  write_csv(., "../0_Data/1_Household Data/2_Malawi/2_Codes/Toilet.Code.csv")
Language.Code <- stack(attr(data_b$hh_b22, 'labels'))%>%
  rename(language = values, Language = ind)%>%
  write_csv(., "../0_Data/1_Household Data/2_Malawi/2_Codes/Language.Code.csv")
Ethnicity.Code <- stack(attr(data_b$hh_b23, 'labels'))%>%
  rename(ethnicity = values, Ethnicity = ind)%>%
  write_csv(., "../0_Data/1_Household Data/2_Malawi/2_Codes/Ethnicity.Code.csv")
Gender.Code <- stack(attr(data_b$hh_b03, 'labels'))%>%
  rename(sex_hhh = values, Gender = ind)%>%
  write_csv(., "../0_Data/1_Household Data/2_Malawi/2_Codes/Gender.Code.csv")
Education.Code <- stack(attr(data_c$hh_c09, 'labels'))%>%
  rename(edu_hhh = values, Education = ind)%>%
  write_csv(., "../0_Data/1_Household Data/2_Malawi/2_Codes/Education.Code.csv")
Industry.Code <- distinct(data_e, hh_e20b)%>%
  rename(ind_hhh = hh_e20b)%>%
  mutate(Industry = "")%>%
  write_csv(., "../0_Data/1_Household Data/2_Malawi/2_Codes/Industry.Code.csv")
District.Code <- stack(attr(data_a$district, 'labels'))%>%
  rename(district = values, District = ind)%>%
  write_csv(., "../0_Data/1_Household Data/2_Malawi/2_Codes/District.Code.csv")
