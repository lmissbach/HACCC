library("haven")
library("Hmisc")
library("openxlsx")
library("rattle")
library("scales")
library("sjlabelled")
library("tidyverse")
options(scipen=999)

# To clean household data from Rwanda

# Author: L. Missbach (missbach@mcc-berlin.net)

# Load Data ####

data_0 <- read_dta("../0_Data/1_Household Data/2_Rwanda/1_Data_Raw/cs_S0_S5_Household.dta")
data_1 <- read_dta("../0_Data/1_Household Data/2_Rwanda/1_Data_Raw/cs_S1_S2_S3_S4_S6A_S6E_Person.dta")
# data_2 <- read_dta("../0_Data/1_Household Data/2_Rwanda/1_Data_Raw/cs_S5F_Access_to_services.dta")
data_3 <- read_dta("../0_Data/1_Household Data/2_Rwanda/1_Data_Raw/cs_S6B_Employement_6C_Salaried_S6D_Business.dta")
# data_4a <- read_dta("../0_Data/1_Household Data/2_Rwanda/1_Data_Raw/cs_S7A1_livestock.dta")

# data_4b <- read_dta("../0_Data/1_Household Data/2_Rwanda/1_Data_Raw/cs_S7A2_livestock.dta")
# data_4c <- read_dta("../0_Data/1_Household Data/2_Rwanda/1_Data_Raw/cs_S7A3_livestock.dta")
# data_4d <- read_dta("../0_Data/1_Household Data/2_Rwanda/1_Data_Raw/cs_S7A4_livestock.dta")
# data_5a <- read_dta("../0_Data/1_Household Data/2_Rwanda/1_Data_Raw/cs_S7B1_land_Agriculture.dta")
# data_5b <- read_dta("../0_Data/1_Household Data/2_Rwanda/1_Data_Raw/cs_S7B2_land_Agriculture.dta")
# 
# data_5c <- read_dta("../0_Data/1_Household Data/2_Rwanda/1_Data_Raw/cs_S7C_parcels.dta")
# data_5d <- read_dta("../0_Data/1_Household Data/2_Rwanda/1_Data_Raw/cs_S7D_large_crop.dta")
# data_5e <- read_dta("../0_Data/1_Household Data/2_Rwanda/1_Data_Raw/cs_S7E_small_crop.dta")
# data_5f <- read_dta("../0_Data/1_Household Data/2_Rwanda/1_Data_Raw/cs_S7F_income_agriculture.dta")
# data_5g <- read_dta("../0_Data/1_Household Data/2_Rwanda/1_Data_Raw/cs_S7G_expenditure_agriculture.dta")

# data_5h <- read_dta("../0_Data/1_Household Data/2_Rwanda/1_Data_Raw/cs_S7H_transformation_agriculture.dta")
data_6a <- read_dta("../0_Data/1_Household Data/2_Rwanda/1_Data_Raw/cs_S8A1_expenditure.dta")
data_6b <- read_dta("../0_Data/1_Household Data/2_Rwanda/1_Data_Raw/cs_S8A2_expenditure.dta")
data_6c <- read_dta("../0_Data/1_Household Data/2_Rwanda/1_Data_Raw/cs_S8A3_expenditure.dta")
data_6d <- read_dta("../0_Data/1_Household Data/2_Rwanda/1_Data_Raw/cs_S8B_expenditure.dta")

# data_6e <- read_dta("../0_Data/1_Household Data/2_Rwanda/1_Data_Raw/cs_S8C_farming.dta")
# data_7a <- read_dta("../0_Data/1_Household Data/2_Rwanda/1_Data_Raw/cs_S9A_transfers_out.dta")
# data_7b <- read_dta("../0_Data/1_Household Data/2_Rwanda/1_Data_Raw/cs_S9B_transfers_in.dta")
data_7c <- read_dta("../0_Data/1_Household Data/2_Rwanda/1_Data_Raw/cs_S9C_Vup_ubudehe_and_Rssp_schemes.dta")
# data_7d <- read_dta("../0_Data/1_Household Data/2_Rwanda/1_Data_Raw/cs_S9C3_Vup_ubudehe_and_Rssp_schemes.dta")

# data_7e <- read_dta("../0_Data/1_Household Data/2_Rwanda/1_Data_Raw/cs_S9C4_Vup_ubudehe_and_Rssp_schemes.dta")
data_7f <- read_dta("../0_Data/1_Household Data/2_Rwanda/1_Data_Raw/cs_S9D_other_income.dta")
data_7g <- read_dta("../0_Data/1_Household Data/2_Rwanda/1_Data_Raw/cs_S9E_Other_expenditure.dta")
# data_8a <- read_dta("../0_Data/1_Household Data/2_Rwanda/1_Data_Raw/cs_S10A1_Credits.dta")
# data_8b <- read_dta("../0_Data/1_Household Data/2_Rwanda/1_Data_Raw/cs_S10A2_Listing_of_credits.dta")

# data_8c <- read_dta("../0_Data/1_Household Data/2_Rwanda/1_Data_Raw/cs_S10B1_Durable_household_goods.dta")
data_8d <- read_dta("../0_Data/1_Household Data/2_Rwanda/1_Data_Raw/cs_S10B2_Durable_household_goods.dta")
#data_8e <- read_dta("../0_Data/1_Household Data/2_Rwanda/1_Data_Raw/cs_S10C1_Savings.dta")
#data_8f <- read_dta("../0_Data/1_Household Data/2_Rwanda/1_Data_Raw/cs_S10C2_Tontine.dta")
data_9 <- read_dta("../0_Data/1_Household Data/2_Rwanda/1_Data_Raw/EICV5_Poverty_file.dta")

# Transform data

data_0.1 <- data_0 %>%
  rename(hh_id = hhid, hh_weights = weight, water = s5cq1, lighting_fuel = s5cq16, cooking_fuel = s5cq18, toilet = s5cq20)%>%
  mutate(urban_01 = ifelse(ur == 1,1,0))%>%
  select(hh_id, province, district, urban_01, hh_weights, water, lighting_fuel, cooking_fuel, toilet)

data_1.1 <- data_1 %>%
  rename(hh_id = hhid, sex_hhh = s1q1, nationality = s1q6, edu_hhh = s4aq2)%>%
  filter(s1q2 == 1)%>%
  select(hh_id, sex_hhh, nationality, edu_hhh)%>%
  mutate(edu_hhh = ifelse(is.na(edu_hhh),1,edu_hhh))

data_1.2 <- data_1 %>%
  rename(hh_id = hhid)%>%
  mutate(adults   = ifelse(s1q3y > 15,1,0),
         children = ifelse(s1q3y < 16,1,0))%>%
  group_by(hh_id)%>%
  summarise(adults   = sum(adults),
            children = sum(children),
            hh_size  = n())%>%
  ungroup()

data_3.1 <- data_3 %>%
  rename(hh_id = hhid)%>%
  left_join(select(rename(mutate(filter(data_1, s1q2 == 1), head = 1), hh_id = hhid), hh_id, pid, head))%>%
  filter(head == 1)%>%
  filter(eid == 1)%>%
  rename(ind_hhh = s6bq4b)%>%
  select(hh_id, ind_hhh)

data_7f.1 <- data_7f %>%
  rename(hh_id = hhid)%>%
  filter(s9dq0 %in% c(1,3,4,5,6,7,8,9))%>%
  mutate(s9dq3 = ifelse(is.na(s9dq3),0,s9dq3))%>%
  group_by(hh_id)%>%
  summarise(inc_gov_monetary = sum(s9dq3))%>%
  ungroup()

data_7c.1 <- data_7c %>%
  rename(hh_id = hhid)%>%
  mutate(inc_gov_cash = ifelse(s9cq2 == 2 & !is.na(s9cq8),s9cq8,
                               ifelse(s9cq2 == 2 & is.na(s9cq8),1,0)))%>%
  select(hh_id, inc_gov_cash)

household_information <- left_join(data_0.1, data_1.1)%>%
  left_join(data_1.2)%>%
  left_join(data_3.1)%>%
  left_join(data_7c.1)%>%
  left_join(data_7f.1)%>%
  remove_all_labels()%>%
  mutate(inc_gov_cash     = ifelse(is.na(inc_gov_cash),     0, inc_gov_cash),
         inc_gov_monetary = ifelse(is.na(inc_gov_monetary), 0, inc_gov_monetary))

write_csv(household_information, "../0_Data/1_Household Data/2_Rwanda/1_Data_Clean/household_information_Rwanda.csv")

data_0.2 <- data_0 %>%
  rename(hh_id = hhid)%>%
  select(hh_id, s5bq4a, s5bq4b, s5cq9a, s5cq9b, s5cq11, s5cq14, s5cq17)%>%
  mutate(s5bq4a = ifelse(s5bq4b == 1, s5bq4a*12,
                         ifelse(s5bq4b == 2, s5bq4a*4,
                                ifelse(s5bq4b == 3, s5bq4a,
                                       ifelse(is.na(s5bq4b),0,NA)))))%>%
  mutate(s5cq9b = ifelse(!is.na(s5cq9b), s5cq9b*12/s5cq9a,0))%>%
  mutate(s5cq9b = ifelse(s5cq9a == 0,0,s5cq9b))%>%
  mutate(s5cq11 = s5cq11*365/7)%>%
  mutate(s5cq14 = s5cq14*12)%>%
  mutate(s5cq17 = s5cq17*12)%>%
  select(-s5bq4b, - s5cq9a)%>%
  pivot_longer(-hh_id, names_to = "item_code", values_to = "expenditures_year")%>%
  filter(expenditures_year != 0 & !is.na(expenditures_year))

data_0.3a <- data_0 %>%
  rename(hh_id = hhid)%>%
  select(hh_id, s5cq18b71, s5cq18b8a)%>%
  mutate(s5cq18b71 = paste0("s5cq18_", s5cq18b71))%>%
  mutate(s5cq18b8a = ifelse(s5cq18b8a == 888888,0,s5cq18b8a*12))%>%
  rename(item_code = s5cq18b71, expenditures_year = s5cq18b8a)%>%
  filter(expenditures_year != 0 & !is.na(expenditures_year))
  
data_0.3b <- data_0 %>%
  rename(hh_id = hhid)%>%
  select(hh_id, s5cq18b72, s5cq18b8b)%>%
  mutate(s5cq18b72 = paste0("s5cq18_", s5cq18b72))%>%
  mutate(s5cq18b8b = ifelse(s5cq18b8b == 888888,0,s5cq18b8b*12))%>%
  rename(item_code = s5cq18b72, expenditures_year = s5cq18b8b)%>%
  filter(expenditures_year != 0 & !is.na(expenditures_year))

data_1.3 <- data_1 %>%
  rename(hh_id = hhid)%>%
  select(hh_id, pid, starts_with("s4aq11"), s4bq2)%>%
  pivot_longer(s4aq11a:s4bq2, names_to = "item_code", values_to = "expenditures_year")%>%
  filter(!is.na(expenditures_year) & expenditures_year > 0)%>%
  group_by(hh_id, item_code)%>%
  summarise(expenditures_year = sum(expenditures_year))%>%
  ungroup()

data_6a.1 <- data_6a %>%
  rename(hh_id = hhid, item_code = s8a1q0, expenditures_year = s8a1q3)%>%
  select(hh_id, item_code, expenditures_year)%>%
  filter(!is.na(expenditures_year))%>%
  arrange(hh_id, item_code)%>%
  mutate(item_code = paste0("A", item_code))

data_6b.1 <- data_6b %>%
  rename(hh_id = hhid, item_code = s8a2q0, expenditures_year = s8a2q3)%>%
  select(hh_id, item_code, expenditures_year)%>%
  filter(!is.na(expenditures_year))%>%
  mutate(expenditures_year = expenditures_year*12)%>%
  arrange(hh_id, item_code)%>%
  mutate(item_code = paste0("B", item_code))

data_6c.1 <- data_6c %>%
  rename(hh_id = hhid, item_code = s8a3q0)%>%
  mutate(item_code = paste0("C", item_code))%>%
  #mutate(expenditures_year_A = s8a3q3*365/7)%>%
  mutate(expenditures_year_B = s8a3q4 + s8a3q5 + s8a3q6 + s8a3q7 + s8a3q8 + s8a3q9 + s8a3q10 + s8a3q11 + s8a3q12 + s8a3q13)%>%
  #mutate(expenditures_year_B = ifelse(ur == 1, expenditures_year_B*365/33,
  #                                    ifelse(ur == 2, expenditures_year_B*365/16, NA)))%>%
  filter(expenditures_year_B > 0)%>%
  group_by(hh_id)%>%
  mutate(expenditures_year_C = sum(expenditures_year_B))%>%
  ungroup()%>%
  mutate(share_exp = expenditures_year_B/expenditures_year_C)%>%
  left_join(select(data_9, hhid, exp14_2), by = c("hh_id" = "hhid"))%>%
  mutate(expenditures_year = exp14_2*share_exp)%>%
  select(hh_id, item_code, expenditures_year)

data_6d.1 <- data_6d %>%
  rename(hh_id = hhid, item_code = s8bq0)%>%
  mutate(item_code = paste0("D", item_code))%>%
  #mutate(expenditures_year_A = s8bq3*365/7)%>%
  mutate(expenditures_year_B = s8bq4 + s8bq5 + s8bq6 + s8bq7 + s8bq8 + s8bq9 + s8bq10 + s8bq11 + s8bq12 + s8bq13)%>%
  filter(expenditures_year_B > 0)%>%
  group_by(hh_id)%>%
  mutate(expenditures_year_C = sum(expenditures_year_B))%>%
  ungroup()%>%
  mutate(share_exp = expenditures_year_B/expenditures_year_C)%>%
  left_join(select(data_9, hhid, exp15_2), by = c("hh_id" = "hhid"))%>%
  mutate(expenditures_year = exp15_2*share_exp)%>%
  select(hh_id, item_code, expenditures_year)

data_7g.1 <- data_7g %>%
  rename(hh_id = hhid, item_code = s9eq1, expenditures_year = s9eq2)%>%
  mutate(item_code = paste0("E", item_code))%>%
  filter(!is.na(expenditures_year) & expenditures_year > 0)%>%
  select(hh_id, item_code, expenditures_year)

# Attention - possible doubling of cooking fuel expenditures

expenditures_items <- bind_rows(data_0.2, data_0.3a, data_0.3b, data_1.3, data_6a.1, data_6b.1, data_6c.1, data_6d.1, data_7g.1)%>%
  arrange(hh_id, item_code)%>%
  mutate(edit_LPG = ifelse(item_code == "C19" | item_code == "s5cq18_10",    1,0),
         edit_Ker = ifelse(item_code == "C20" | item_code == "s5cq18_1",     1,0),
         edit_Cha = ifelse(item_code == "C21" | item_code == "s5cq18_2",     1,0),
         edit_Woo = ifelse(item_code == "C22" | item_code == "s5cq18_3",     1,0),
         edit_Ely = ifelse(item_code == "s5cq17" | item_code == "s5cq18_11", 1,0)) %>%
  group_by(hh_id)%>%
  mutate(edit_LPG_sum = sum(edit_LPG),
         edit_Ker_sum = sum(edit_Ker),
         edit_Cha_sum = sum(edit_Cha),
         edit_Woo_sum = sum(edit_Woo),
         edit_Ely_sum = sum(edit_Ely))%>%
  ungroup()%>%
  mutate(expenditures_year = ifelse(edit_LPG_sum == 2 & item_code == "s5cq18_10", 0, expenditures_year))%>%
  mutate(expenditures_year = ifelse(edit_Ker_sum == 2 & item_code == "s5cq18_1",  0, expenditures_year))%>%
  mutate(expenditures_year = ifelse(edit_Cha_sum == 2 & item_code == "s5cq18_2",  0, expenditures_year))%>%
  mutate(expenditures_year = ifelse(edit_Woo_sum == 2 & item_code == "s5cq18_3",  0, expenditures_year))%>%
  mutate(expenditures_year = ifelse(edit_Ely_sum == 2 & item_code == "s5cq18_11", 0, expenditures_year))%>%
  filter(expenditures_year > 0)%>%
  
  #group_by(hh_id)%>%
  #summarise(hh_exp_year = sum(expenditures_year))%>%
  #ungroup()%>%
  select(hh_id, item_code, expenditures_year)

write_csv(expenditures_items, "../0_Data/1_Household Data/2_Rwanda/1_Data_Clean/expenditures_items_Rwanda.csv")

appliances_Rwanda <- data_8d %>%
  rename(hh_id = hhid)%>%
  select(hh_id, s10b2q1, s10b2q2)%>%
  mutate(value = ifelse(s10b2q2 > 0 ,1,0))%>%
  mutate(name = ifelse(s10b2q1 == 5, "radio.01",
                       ifelse(s10b2q1 == 6, "mobile.01",
                              ifelse(s10b2q1 == 7, "tv.01",
                                     ifelse(s10b2q1 == 12, "computer.01",
                                            ifelse(s10b2q1 == 5, "radio.01",
                                                   ifelse(s10b2q1 == 18, "fan.01",
                                                          ifelse(s10b2q1 == 17, "washing_machine.01",
                                                                 ifelse(s10b2q1 == 20, "refrigerator.01",
                                                                        ifelse(s10b2q1 == 21, "generator.01",
                                                                               ifelse(s10b2q1 == 26, "motorcycle.01",
                                                                                      ifelse(s10b2q1 == 27, "car.01", NA))))))))))))%>%
  filter(!is.na(name))%>%
  select(hh_id, name, value)%>%
  pivot_wider(names_from = "name", values_from = "value")%>%
  # Assumption for 6 households
  right_join(select(household_information, hh_id))%>%
  mutate_at(vars(-hh_id), list(~ ifelse(is.na(.),0,.)))

write_csv(appliances_Rwanda, "../0_Data/1_Household Data/2_Rwanda/1_Data_Clean/appliances_0_1_Rwanda.csv")

# Codes
Industry.Code <- stack(attr(data_3$s6bq4b, 'labels'))%>%
  rename(ind_hhh = values, Industry = ind)%>%
  write_csv(., "../0_Data/1_Household Data/2_Rwanda/2_Codes/Industry.Code.csv")
Gender.Code <- stack(attr(data_1$s1q1, 'labels'))%>%
  rename(sex_hhh = values, Gender = ind)%>%
  write_csv(., "../0_Data/1_Household Data/2_Rwanda/2_Codes/Gender.Code.csv")
Education.Code <- stack(attr(data_1$s4aq2, 'labels'))%>%
  rename(edu_hhh = values, Education = ind)%>%
  write_csv(., "../0_Data/1_Household Data/2_Rwanda/2_Codes/Education.Code.csv")
Nationality.Code <- stack(attr(data_1$s1q6, 'labels'))%>%
  rename(nationality = values, Nationality = ind)%>%
  write_csv(., "../0_Data/1_Household Data/2_Rwanda/2_Codes/Nationality.Code.csv")
Cooking.Code <- stack(attr(data_0$s5cq16, 'labels'))%>%
  rename(cooking_fuel = values, Cooking_Fuel = ind)%>%
  write_csv(., "../0_Data/1_Household Data/2_Rwanda/2_Codes/Cooking.Code.csv")
Lighting.Code <- stack(attr(data_0$s5cq16, 'labels'))%>%
  rename(lighting_fuel = values, Lighting_Fuel = ind)%>%
  write_csv(., "../0_Data/1_Household Data/2_Rwanda/2_Codes/Lighting.Code.csv")
Water.Code <- stack(attr(data_0$s5cq1, 'labels'))%>%
  rename(water = values, WTR = ind)%>%
  write_csv(., "../0_Data/1_Household Data/2_Rwanda/2_Codes/Water.Code.csv")
Province.Code <- stack(attr(data_0$province, 'labels'))%>%
  rename(province = values, Province = ind)%>%
  write_csv(., "../0_Data/1_Household Data/2_Rwanda/2_Codes/Province.Code.csv")
Toilet.Code <- stack(attr(data_0$s5cq20, 'labels'))%>%
  rename(toilet = values, TLT = ind)%>%
  write_csv(., "../0_Data/1_Household Data/2_Rwanda/2_Codes/Toilet.Code.csv")
District.Code <- stack(attr(data_0$district, 'labels'))%>%
  rename(district = values, District = ind)%>%
  write_csv(., "../0_Data/1_Household Data/2_Rwanda/2_Codes/District.Code.csv")

Item.Code.A <- distinct(data_0.2, item_code)
Item.Code.B <- distinct(data_0.3a, item_code)%>%
  arrange(item_code)
Item.Code.C <- distinct(data_0.3b, item_code)%>%
  arrange(item_code)
Item.Code.ABC <- bind_rows(Item.Code.A, Item.Code.B, Item.Code.C)%>%
  distinct(item_code)
Item.Code.D <- distinct(data_1.3, item_code)%>%
  arrange(item_code)
Item.Code.E <- stack(attr(data_6a$s8a1q0, 'labels'))%>%
  rename(item_code = values, item_name = ind)%>%
  mutate(item_code = paste0("A", item_code))
Item.Code.F <- stack(attr(data_6b$s8a2q0, 'labels'))%>%
  rename(item_code = values, item_name = ind)%>%
  mutate(item_code = paste0("B", item_code))
Item.Code.G <- stack(attr(data_6c$s8a3q0, 'labels'))%>%
  rename(item_code = values, item_name = ind)%>%
  mutate(item_code = paste0("C", item_code))
Item.Code.H <- stack(attr(data_6d$s8bq0, 'labels'))%>%
  rename(item_code = values, item_name = ind)%>%
  mutate(item_code = paste0("D", item_code))
Item.Code.I <- stack(attr(data_7g$s9eq1, 'labels'))%>%
  rename(item_code = values, item_name = ind)%>%
  mutate(item_code = paste0("E", item_code))

Item.Code.All <- bind_rows(Item.Code.ABC, Item.Code.D)%>%
  bind_rows(Item.Code.E)%>%
  bind_rows(Item.Code.F)%>%
  bind_rows(Item.Code.G)%>%
  bind_rows(Item.Code.H)%>%
  bind_rows(Item.Code.I)

# write.xlsx(Item.Code.All, "../0_Data/1_Household Data/2_Rwanda/3_Matching_Tables/Item_Codes_Description_Rwanda.xlsx")
