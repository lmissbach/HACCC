# 1        Packages ####

if(!require("pacman")) install.packages("pacman")

p_load("countrycode", "ggsci", "haven", "Hmisc", "openxlsx", "rattle", "scales", "tidyverse", "sjlabelled")

options(scipen=999)


# 2        Load Data ####

data_01 <- read_dta("../0_Data/1_Household Data/1_Mongolia/1_Data_Raw/01_hhold.dta")
data_02 <- read_dta("../0_Data/1_Household Data/1_Mongolia/1_Data_Raw/02_indiv.dta")
data_03 <- read_dta("../0_Data/1_Household Data/1_Mongolia/1_Data_Raw/03_livestock.dta")
data_04 <- read_dta("../0_Data/1_Household Data/1_Mongolia/1_Data_Raw/04_livestock_exp.dta")
data_05 <- read_dta("../0_Data/1_Household Data/1_Mongolia/1_Data_Raw/05_by_product.dta")
data_06 <- read_dta("../0_Data/1_Household Data/1_Mongolia/1_Data_Raw/06_crop.dta")
data_07 <- read_dta("../0_Data/1_Household Data/1_Mongolia/1_Data_Raw/07_agric_exp.dta")
data_08 <- read_dta("../0_Data/1_Household Data/1_Mongolia/1_Data_Raw/08_enterprise.dta")
data_09 <- read_dta("../0_Data/1_Household Data/1_Mongolia/1_Data_Raw/09_other_income.dta")
data_10 <- read_dta("../0_Data/1_Household Data/1_Mongolia/1_Data_Raw/10_savings_loan.dta")
data_11 <- read_dta("../0_Data/1_Household Data/1_Mongolia/1_Data_Raw/11_energy.dta")
data_12 <- read_dta("../0_Data/1_Household Data/1_Mongolia/1_Data_Raw/12_payment_serv.dta")
data_13 <- read_dta("../0_Data/1_Household Data/1_Mongolia/1_Data_Raw/13_durable.dta")
data_14 <- read_dta("../0_Data/1_Household Data/1_Mongolia/1_Data_Raw/14_non_food.dta")
data_15 <- read_dta("../0_Data/1_Household Data/1_Mongolia/1_Data_Raw/15_urb_diary.dta")
data_16 <- read_dta("../0_Data/1_Household Data/1_Mongolia/1_Data_Raw/16_rur_food_7d.dta")
data_18 <- read_dta("../0_Data/1_Household Data/1_Mongolia/1_Data_Raw/18_foodout.dta")
data_00 <- read_dta("../0_Data/1_Household Data/1_Mongolia/1_Data_Raw/basicvars.dta")

# Not used

rm(data_03, data_04, data_05, data_06, data_07, data_08, data_10)

# 3        Data Cleaning ####
# 3.1      Household Data ####

data_00.1 <- data_00 %>%
  select(identif, newaimag, region, urban, hhweight, hhsize)%>%
  rename(hh_id = identif, district = newaimag, province = region, hh_weights = hhweight, hh_size = hhsize)%>%
  mutate(urban_01 = ifelse(urban == 1, 1, 0))%>%
  select(-urban)

data_01.1 <- data_01 %>%
  select(identif, q0701, q0702, q0703, q0709, q0712)%>%
  rename(hh_id = identif, water = q0709, toilet = q0712, 
         dwelling = q0701, rooms = q0702, area = q0703)

data_02.1 <- data_02 %>%
  select(identif, ind_id, q0102, q0103, q0105y, q0207, q0109)%>%
  rename(hh_id = identif, sex_hhh = q0103, ind_hhh = q0207)

data_02.11 <- data_02.1 %>%
  filter(q0102 == 1)%>%
  rename(age_hhh = q0105y)%>%
  select(hh_id, sex_hhh, ind_hhh, age_hhh)

data_02.12 <- data_02.1 %>%
  # Is HH member?
  filter(q0109 == 1)%>%
  mutate(adult    = ifelse(q0105y > 15,1,0),
         children = ifelse(q0105y < 16,1,0))%>%
  group_by(hh_id)%>%
  summarise(adults   = sum(adult),
            children = sum(children))%>%
  ungroup()

data_09.1 <- data_09 %>%
  rename(hh_id = identif)%>%
  mutate_at(vars(q0502b, q0503b, q0504b, q0505b, q0506b), list(~ ifelse(is.na(.),0,.)))%>%
  mutate(income = q0502b + q0503b + q0504b + q0505b + q0506b)%>%
  filter(income != 0)%>%
  mutate(gov_monetary_indicator = ifelse(income_id %in% c(1,2,3,4,5,6,7,8),1,0),
         gov_cash_indicator     = ifelse(income_id %in% c(9,10,11,12) ,1,0))%>%
  group_by(hh_id, gov_monetary_indicator, gov_cash_indicator)%>%
  summarise(income = sum(income))%>%
  ungroup()%>%
  mutate(inc_gov_monetary = gov_monetary_indicator*income,
         inc_gov_cash     = gov_cash_indicator*income)%>%
  group_by(hh_id)%>%
  summarise(inc_gov_monetary = sum(inc_gov_monetary),
            inc_gov_cash     = sum(inc_gov_cash))%>%
  ungroup()

data_0102 <- left_join(data_00.1, data_01.1, by = "hh_id")%>%
  left_join(data_02.11, by = "hh_id")%>%
  left_join(data_02.12, by = "hh_id")%>%
  remove_all_labels()%>%
  left_join(data_09.1, by = "hh_id")%>%
  mutate(inc_gov_monetary = ifelse(is.na(inc_gov_monetary),0, inc_gov_monetary),
         inc_gov_cash     = ifelse(is.na(inc_gov_cash), 0, inc_gov_cash))

write_csv(data_0102, "../0_Data/1_Household Data/1_Mongolia/1_Data_Clean/household_information_Mongolia_2023.csv")

rm(data_00.1, data_01.1, data_02.11, data_02.12, data_09.1, data_09, data_0102, data_02.1)

# 3.2      Home Production ####

# data_05, data_06, data_07 reports quantities only
# data_08 reports expenditures on fuels for enterprises

# 3.3      Expenditures on Energy Items ####

data_11.1 <- data_11 %>%
  rename(hh_id = identif, item_code = energy_id)%>%
  remove_all_labels()%>%
  mutate(Fuel = ifelse(item_code == 1, "Electricity",
                       ifelse(item_code == 2 | item_code == 3, "Firewood",
                              ifelse(item_code == "4" | item_code == "5", "Coal",
                                     ifelse(item_code == "7" | item_code == "8", "Dung_Cake",
                                            ifelse(item_code == "9", "Gas", 
                                                   ifelse(item_code == "6", "Improved_fuel", "Other")))))))%>%
  mutate(Unit = ifelse(item_code == 2, "m3",
                       ifelse(item_code == 3 | item_code == 6, "bag",
                              ifelse(item_code == 4 | item_code == 7, "ton",
                                     ifelse(item_code == 8, "l", 
                                            ifelse(item_code == 5 | item_code == 8, "kg", NA))))))%>%
  filter(q0718 == 1)%>%
  select(-q0718)

# Collected

# data_11.11 <- data_11.1 %>%
#   filter(q0719 != 2)%>%
#   select(-q0721,-q0720b, -q0722,-q0719)%>%
#   filter(q0723 != 0 | q0724 != 0)%>%
#   mutate(expenditures_sp = q0723 + q0724)%>%
#   select(-q0723, -q0724)%>%
#   rename(Quantity_sp = q0720a)%>%
#   mutate(implicit_price = ifelse(Quantity_sp != 0, expenditures_sp/Quantity_sp, NA))

# Purchased

data_11.12 <- data_11.1 %>%
  filter(q0719 == 2 | is.na(q0719) | q0719 == 3 | q0719 == 4)%>%
  select(-q0719, -q0720b, -q0721, -q0723)%>%
  rename(expenditures_year = q0722, Quantity = q0720a)%>%
  select(-Quantity, - Unit, - Fuel)%>%
  mutate(item_code = item_code + 100)

# data_11.11.exp <- select(data_11.11, hh_id, item_code, expenditures_sp)
# data_11.12.exp <- select(data_11.12, hh_id, item_code, expenditures)
# data_11.exp <- full_join(data_11.11.exp, data_11.12.exp)

# 3.4      Expenditures ####

data_12.1 <- data_12 %>%
  rename(hh_id = identif, item_code = item)%>%
  filter(q0724 == 1)%>%
  mutate(expenditures = q0726)%>%
  filter(expenditures > 0 & !is.na(expenditures))%>%
  rename(expenditures_year = expenditures)%>%
  select(hh_id, item_code, expenditures_year)%>%
  remove_all_labels()

# Annually
data_14.1 <- data_14 %>%
  rename(hh_id = identif, item_code = item)%>%
  filter(q0801 == 1)%>%
  mutate(expenditures_year = q0803)%>%
  filter(expenditures_year > 0 & !is.na(expenditures_year))%>%
  select(hh_id, item_code, expenditures_year)%>%
  remove_all_labels()

# For Urban Households - Weekly
data_15.1 <- data_15 %>%
  rename(hh_id = identif, item_code = item)%>%
  filter(q0902 > 0)%>%
  mutate(expenditures_year = q0902*q0903*365/7)%>%
  select(hh_id, item_code, expenditures_year)%>%
  remove_all_labels()

# For Rural Households - Weekly
data_16.1 <- data_16 %>%
  rename(hh_id = identif, item_code = item)%>%
  filter(q1003 > 0)%>%
  mutate(expenditures_year = q1003*q1004*365/7)%>%
  select(hh_id, item_code, expenditures_year)%>%
  remove_all_labels()

# Consumption away from home - Weekly
data_18.1 <- data_18 %>%
  rename(hh_id = identif, item_code = item)%>%
  filter(q0906 == 1)%>%
  mutate(expenditures_year    = q0908*365/7)%>%
  filter(expenditures_year > 0)%>%
  select(hh_id, item_code, expenditures_year)%>%
  remove_all_labels()%>%
  mutate(item_code = 900 + item_code)

expenditures_all <- data_11.12 %>% # Energy items
  bind_rows(data_12.1)%>% # Housing
  bind_rows(data_14.1)%>% # Durables
  bind_rows(data_15.1)%>% # Urban households
  bind_rows(data_16.1)%>% # Rural households
  bind_rows(data_18.1)%>% # Foot outside
  arrange(hh_id, item_code)%>%
  # Not necessary anymore
  group_by(hh_id, item_code)%>%
  summarise(expenditures_year = sum(expenditures_year))%>%
  ungroup()%>%
  remove_all_labels()

write_csv(expenditures_all, "../0_Data/1_Household Data/1_Mongolia/1_Data_Clean/expenditures_items_Mongolia_2023.csv")

rm(data_11.12, data_12.1, data_14.1, data_15.1, data_16.1, data_18.1, expenditures_all, data_11.1)

# data_11.11.1 <- data_11.11 %>%
#   select(hh_id, Fuel, expenditures_sp)%>%
#   group_by(hh_id, Fuel)%>%
#   summarise(expenditures_sp = sum(expenditures_sp))%>%
#   ungroup()%>%
#   pivot_wider(names_from = "Fuel", values_from = "expenditures_sp", names_prefix = "exp_sp_")
# 
# data_11.12.1 <- data_11.12 %>%
#   select(hh_id, Fuel, expenditures)%>%
#   group_by(hh_id, Fuel)%>%
#   summarise(expenditures = sum(expenditures))%>%
#   ungroup()%>%
#   pivot_wider(names_from = "Fuel", values_from = "expenditures", names_prefix = "exp_")
# 
# expenditures_final <- select(data_00.1, hh_id)%>%
#   left_join(expenditures_all)%>%
#   left_join(data_11.12.1)%>%
#   left_join(data_14.fuels)%>%
#   left_join(data_11.11.1)
# 
# write_csv(expenditures_final, "1_Mongolia/Data/Expenditure_data_Mongolia.csv")
# 
# data_impl_1 <- data_11.11 %>%
#   select(hh_id, Fuel, Quantity_sp, Unit, expenditures_sp, implicit_price)%>%
#   rename(implicit_price_sp = implicit_price)
# 
# data_impl_2 <- data_11.12 %>%
#   select(hh_id, Fuel, Quantity, Unit, expenditures, implicit_price)
# 
# data_impl_3 <- data_0102 %>%
#   select(hh_id, hh_weights, province, district, urban_01)
# 
# data_impl_4 <- data_impl_2 %>%
#   full_join(data_impl_2)%>%
#   full_join(data_impl_3)
# 
# write_csv(data_impl_4, "1_Mongolia/Data/Implicit_price_data_Mongolia.csv")

# 3.5      Appliances ####

data_13.1 <- data_13 %>%
  rename(hh_id = identif)%>%
  mutate(Appliance = ifelse(durable_id == 1, "radio.01",
                            ifelse(durable_id == 2, "tv.01",
                                   ifelse(durable_id == 8, "phone.01",
                                          ifelse(durable_id == 7, "computer.01", NA)))))%>%
  filter(!is.na(Appliance))%>%
  mutate(value = ifelse(q0728 > 0,1,0))%>%
  select(hh_id, Appliance, value)%>%
  pivot_wider(names_from = "Appliance", values_from = "value")

write_csv(data_13.1, "../0_Data/1_Household Data/1_Mongolia/1_Data_Clean/appliances_0_1_Mongolia_2023.csv")

rm(data_13, data_13.1)

# 3.6      Codes needed ####

Gender.Code <- stack(attr(data_02$q0103, 'labels'))%>%
  rename(sex_hhh = values, Gender = ind)%>%
  write_csv(., "../0_Data/1_Household Data/1_Mongolia/2_Codes/Gender.Code.csv")
Industry.Code <- stack(attr(data_02$q0207, 'labels'))%>%
  rename(ind_hhh = values, Industry = ind)%>%
  write_csv(., "../0_Data/1_Household Data/1_Mongolia/2_Codes/Industry.Code.csv")
Water.Code <- stack(attr(data_01$q0709, 'labels'))%>%
  rename(water = values, Water = ind)%>%
  write_csv(., "../0_Data/1_Household Data/1_Mongolia/2_Codes/Water.Code.csv")
Toilet.Code <- stack(attr(data_01$q0712, "labels"))%>%
  rename(toilet = values, Toilet = ind)%>%
  write_csv(., "../0_Data/1_Household Data/1_Mongolia/2_Codes/Toilet.Code.csv")
Housing.Code <- stack(attr(data_01$q0701, "labels"))%>%
  rename(dwelling = values, Dwelling = ind)%>%
  write_csv(., "../0_Data/1_Household Data/1_Mongolia/2_Codes/Housing.Code.csv")
District.Code <- stack(attr(data_00$newaimag, "labels"))%>%
  rename(district = values, District = ind)%>%
  write_csv(., "../0_Data/1_Household Data/1_Mongolia/2_Codes/District.Code.csv")
Province.Code <- stack(attr(data_00$region, "labels"))%>%
  rename(province = values, Province = ind)%>%
  write_csv(., "../0_Data/1_Household Data/1_Mongolia/2_Codes/Province.Code.csv")

rm(Gender.Code, Industry.Code, Water.Code, Toilet.Code, Housing.Code, District.Code, Province.Code, data_02, data_01, data_00)

Item.Code <- stack(attr(data_11$energy_id, "labels"))%>%
  rename(item_code = values, item_name = ind)%>%
  mutate(item_code = item_code+100)
Item.Code.a <- stack(attr(data_12$item, "labels"))%>%
  rename(item_code = values, item_name = ind)
Item.Code.c <- stack(attr(data_14$item, "labels"))%>%
  rename(item_code = values, item_name = ind)
Item.Code.d <- stack(attr(data_15$item, "labels"))%>%
  rename(item_code = values, item_name = ind)
Item.Code.e <- stack(attr(data_16$item, "labels"))%>%
  rename(item_code = values, item_name = ind)
Item.Code.f <- stack(attr(data_18$item, "labels"))%>%
  rename(item_code = values, item_name = ind)%>%
  mutate(item_code = item_code + 900)

Item.Code.all <- bind_rows(Item.Code, Item.Code.a)%>%
  bind_rows(Item.Code.c)%>%
  bind_rows(Item.Code.d)%>%
  bind_rows(Item.Code.e)%>%
  bind_rows(Item.Code.f)%>%
  distinct()%>%
  arrange(item_code)

write.xlsx(Item.Code.all, "../0_Data/1_Household Data/1_Mongolia/3_Matching_Tables/Item_Code_Description_Mongolia_2023.xlsx")

rm(Item.Code, Item.Code.a, Item.Code.c, Item.Code.d, Item.Code.e, Item.Code.f, Item.Code.all, data_11, data_12, data_14, data_15, data_16, data_18)

rm(list=ls())

# 3.7      Comparison item codes ####

codes_2016 <- read.xlsx("../0_Data/1_Household Data/1_Mongolia/3_Matching_Tables/Item_Codes_Description_Mongolia.xlsx")%>%
  rename(item_name_2016 = item_name)
codes_2023 <- read.xlsx("../0_Data/1_Household Data/1_Mongolia/3_Matching_Tables/Item_Code_Description_Mongolia_2023.xlsx")%>%
  rename(item_name_2023 = item_name)

codes_2016_2023 <- full_join(codes_2016, codes_2023, by = "item_code")%>%
  mutate(equal = ifelse(item_name_2016 == item_name_2023, T,F))%>%
  filter(!is.na(item_name_2023))%>%
  arrange(item_code)%>%
  select(-equal)

write.xlsx(codes_2016_2023, "../0_Data/1_Household Data/1_Mongolia/3_Matching_Tables/Item_Code_Comparison_Mongolia_2023_2016.xlsx")
