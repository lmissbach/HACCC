# 1        Packages ####

library("cowplot")
library("data.table")
library("dplyr")
library("forcats")
library("foreign")
library("ggthemes")
# library("ggExtra")
library("ggplot2")
library("ggsci")
library("haven")
library("Hmisc")
library("janitor")
library("officer")
library("openxlsx")
library("patchwork") # patchwork requires installation process: install.packages("devtools"), devtools::install_github("thomasp85/patchwork")
library("quantreg")
library("rattle")
library("readr")
library("readxl")
library("reshape2")
library("scales") 
library("sjlabelled")
library("stringr")
library("tidyr")
#library("tidyverse")
library("utils")
library("wesanderson")
library("weights")
options(scipen=999)


# 2        Load Data ####

data_01 <- read_dta("H:/4_Action/1_Mongolia_data/Microdata_Mongolia/01_hhold.dta")
data_02 <- read_dta("H:/4_Action/1_Mongolia_data/Microdata_Mongolia/02_indiv.dta")
data_03 <- read_dta("H:/4_Action/1_Mongolia_data/Microdata_Mongolia/03_livestock.dta")
data_04 <- read_dta("H:/4_Action/1_Mongolia_data/Microdata_Mongolia/04_livestock_exp.dta")
data_05 <- read_dta("H:/4_Action/1_Mongolia_data/Microdata_Mongolia/05_by_product.dta")
data_06 <- read_dta("H:/4_Action/1_Mongolia_data/Microdata_Mongolia/06_crop.dta")
data_07 <- read_dta("H:/4_Action/1_Mongolia_data/Microdata_Mongolia/07_agric_exp.dta")
data_08 <- read_dta("H:/4_Action/1_Mongolia_data/Microdata_Mongolia/08_enterprise.dta")
data_09 <- read_dta("H:/4_Action/1_Mongolia_data/Microdata_Mongolia/09_other_income.dta")
data_10 <- read_dta("H:/4_Action/1_Mongolia_data/Microdata_Mongolia/10_savings_loan.dta")
data_11 <- read_dta("H:/4_Action/1_Mongolia_data/Microdata_Mongolia/11_energy.dta")
data_12 <- read_dta("H:/4_Action/1_Mongolia_data/Microdata_Mongolia/12_payment_serv.dta")
data_13 <- read_dta("H:/4_Action/1_Mongolia_data/Microdata_Mongolia/13_durable.dta")
data_14 <- read_dta("H:/4_Action/1_Mongolia_data/Microdata_Mongolia/14_non_food.dta")
data_15 <- read_dta("H:/4_Action/1_Mongolia_data/Microdata_Mongolia/15_urb_diary.dta")
data_16 <- read_dta("H:/4_Action/1_Mongolia_data/Microdata_Mongolia/16_rur_food_7d.dta")
data_18 <- read_dta("H:/4_Action/1_Mongolia_data/Microdata_Mongolia/18_foodout.dta")
data_00 <- read_dta("H:/4_Action/1_Mongolia_data/Microdata_Mongolia/basicvars.dta")

# 3        Data Cleaning ####
# 3.1      Household Data ####

data_00.1 <- data_00 %>%
  select(identif, newaimag, region, urban, hhweight, hhsize)%>%
  rename(hh_id = identif, district = newaimag, province = region, hh_weights = hhweight, hh_size = hhsize)%>%
  mutate(urban_01 = ifelse(urban == 1, 1, 0))%>%
  select(-urban)

data_01.1 <- data_01 %>%
  select(identif, q0709, q0712)%>%
  rename(hh_id = identif, water = q0709, toilet = q0712)

data_02.1 <- data_02 %>%
  select(identif, ind_id, q0102, q0103, q0105y, q0207, q0109)%>%
  rename(hh_id = identif, sex_hhh = q0103, ind_hhh = q0207)

data_02.11 <- data_02.1 %>%
  filter(q0102 == 1)%>%
  select(hh_id, sex_hhh, ind_hhh)

data_02.12 <- data_02.1 %>%
  filter(q0109 == 1)%>%
  mutate(adult = ifelse(q0105y > 17,1,0),
         children = ifelse(q0105y < 18,1,0))%>%
  group_by(hh_id)%>%
  summarise(adults = sum(adult),
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

data_0102 <- left_join(data_00.1, data_01.1)%>%
  left_join(data_02.11)%>%
  left_join(data_02.12)%>%
  remove_all_labels()%>%
  left_join(data_09.1)%>%
  mutate(inc_gov_monetary = ifelse(is.na(inc_gov_monetary),0, inc_gov_monetary),
         inc_gov_cash     = ifelse(is.na(inc_gov_cash), 0, inc_gov_cash))

write_csv(data_0102, "../0_Data/1_Household Data/1_Mongolia/1_Data_Clean/household_information_Mongolia.csv")

# For Raavi: Month and Year

time_01 <- data_01 %>%
  rename(hh_id = identif)%>%
  mutate(year  = ifelse(year == 19, 2019,NA),
         month = v1_mm)%>%
  select(hh_id, year, month)

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
                                     ifelse(item_code == "6" | item_code == "7", "Dung_Cake",
                                            ifelse(item_code == "8", "Gas", "Other"))))))%>%
  mutate(Unit = ifelse(item_code == 2, "m3",
                       ifelse(item_code == 3 | item_code == 5 | item_code == 7, "bag",
                              ifelse(item_code == 4 | item_code == 6, "ton",
                                     ifelse(item_code == 8, "l", NA)))))%>%
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

data_11.12 <- data_11.1 %>%
  filter(q0719 == 2 | is.na(q0719) | q0719 == 3 | q0719 == 4)%>%
  select(-q0719, -q0720b, -q0721, -q0723, -q0724)%>%
  rename(expenditures_year = q0722, Quantity = q0720a)%>%
  select(-Quantity, - Unit, - Fuel)%>%
  mutate(item_code = item_code + 100)

# data_11.11.exp <- select(data_11.11, hh_id, item_code, expenditures_sp)
# data_11.12.exp <- select(data_11.12, hh_id, item_code, expenditures)
# data_11.exp <- full_join(data_11.11.exp, data_11.12.exp)

# 3.4      Expenditures ####

data_12.1 <- data_12 %>%
  rename(hh_id = identif, item_code = item)%>%
  filter(q0725 == 1)%>%
  select(-q0725, -q0726)%>%
  mutate(expenditures = q0727,
         expenditures_sp = q0728+q0729)%>%
  mutate(expenditures_sp = ifelse(item_code == 6 | item_code == 10 | item_code == 11,0,expenditures_sp))%>%
  select(-starts_with("q"),-expenditures_sp)%>%
  remove_all_labels()%>%
  filter(expenditures >0 & !is.na(expenditures))%>%
  rename(expenditures_year = expenditures)

data_14.1 <- data_14 %>%
  rename(hh_id = identif, item_code = item)%>%
  filter(q0801 == 1)%>%
  mutate(expenditures_year    = q0803, 
         expenditures_sp = q0804 + q0805)%>%
  select(-q0801, -q0802, -q0803, -q0804, -q0805, - expenditures_sp)%>%
  filter(expenditures_year > 0 & !is.na(expenditures_year))%>%
  remove_all_labels()

# For Urban Households - Monthly
data_15.1 <- data_15 %>%
  rename(hh_id = identif, item_code = item)%>%
  mutate(quantity_purchased = q0901_2 + q0902_2 + q0903_2,
         quantity_produced  = q0901_3 + q0901_4 + q0902_3 + q0902_4 + q0903_3 + q0903_4)%>%
  select(-starts_with("q0901"), - starts_with("q0902"), - starts_with("q0903"))%>%
  filter(quantity_purchased != 0 | quantity_produced != 0)%>%
  mutate(expenditures    = quantity_purchased*q0904*12,
         expenditures_sp = NA)%>%
  select(-q0904)%>%
  remove_all_labels()%>%
  select(-quantity_produced, - quantity_purchased, - expenditures_sp)%>%
  filter(expenditures > 0 & !is.na(expenditures))%>%
  rename(expenditures_year = expenditures)

# For Rural Households - Weekly
data_16.1 <- data_16 %>%
  rename(hh_id = identif, item_code = item)%>%
  filter(q1001 == 1)%>%
  mutate(quantity_purchased = q1003,
         quantity_produced  = (q1005 + q1006))%>%
  select(-q1001, -q1003, -q1005, -q1006,-row10)%>%
  mutate(expenditures    = quantity_purchased*q1004*52,
         expenditures_sp = NA)%>%
  select(-q1002, -q1004)%>%
  remove_all_labels()%>%
  select(-quantity_produced, -quantity_purchased, - expenditures_sp)%>%
  filter(expenditures > 0 & !is.na(expenditures))%>%
  rename(expenditures_year = expenditures)

data_18.1 <- data_18 %>%
  rename(hh_id = identif, item_code = item)%>%
  filter(q1007 == 1)%>%
  mutate(expenditures    = q1008*52,
         expenditures_sp = q1009*52)%>%
  select(-starts_with("q"), - expenditures_sp)%>%
  filter(expenditures > 0 & !is.na(expenditures))%>%
  rename(expenditures_year = expenditures)

expenditures_all <- bind_rows(data_11.12, data_12.1)%>%
  bind_rows(data_14.1)%>%
  bind_rows(data_15.1)%>%
  bind_rows(data_16.1)%>%
  bind_rows(data_18.1)%>%
  arrange(hh_id, item_code)%>%
  group_by(hh_id, item_code)%>%
  summarise(expenditures_year = sum(expenditures_year))%>%
  ungroup()%>%
  #left_join(time_01)%>%
  remove_all_labels()

write_csv(expenditures_all, "../0_Data/1_Household Data/1_Mongolia/1_Data_Clean/expenditures_items_Mongolia.csv")

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
  filter(q0730 != 0)%>%
  mutate(Appliance = ifelse(durable_id == 1, "radio.01",
                            ifelse(durable_id == 2, "tv_bw.01",
                                   ifelse(durable_id == 3, "tv.01",
                                          ifelse(durable_id == 8, "computer.01", NA)))))%>%
  filter(!is.na(Appliance))%>%
  mutate(owning = 1,
         number = q0730)%>%
  select(-q0730, -durable_id)

data_13.11 <- data_13.1 %>%
  select(-number)%>%
  pivot_wider(names_from = "Appliance", values_from = "owning", values_fill = 0)%>%
  remove_all_labels()%>%
  mutate(tv.01 = ifelse(tv.01 == 1 | tv_bw.01 == 1,1,0))%>%
  select(-tv_bw.01)%>%
  # Adjustment for 324 households
  mutate(hh_id = as.character(hh_id))%>%
  right_join(select(household_information, hh_id))%>%
  mutate_at(vars(-hh_id), list(~ ifelse(is.na(.),0,.)))

write_csv(data_13.11, "../0_Data/1_Household Data/1_Mongolia/1_Data_Clean/appliances_0_1_Mongolia.csv")

# 3.6      Codes needed ####

Gender.Code <- stack(attr(data_02.11$sex_hhh, 'labels'))%>%
  rename(sex_hhh = values, Gender = ind)%>%
  write_csv(., "../0_Data/1_Household Data/1_Mongolia/2_Codes/Gender.Code.csv")
Industry.Code <- stack(attr(data_02.11$ind_hhh, 'labels'))%>%
  rename(ind_hhh, Industry = ind)
Water.Code <- stack(attr(data_01.1$water, 'labels'))%>%
  rename(water = values, Water = ind)%>%
  write_csv(., "../0_Data/1_Household Data/1_Mongolia/2_Codes/Water.Code.csv")
District.Code <- stack(attr(data_00.1$district, "labels"))%>%
  rename(district = values, District = ind)%>%
  write_csv(., "../0_Data/1_Household Data/1_Mongolia/2_Codes/District.Code.csv")
Province.Code <- stack(attr(data_00.1$province, "labels"))%>%
  rename(province = values, Province = ind)%>%
  write_csv(., "../0_Data/1_Household Data/1_Mongolia/2_Codes/Province.Code.csv")
Toilet.Code <- stack(attr(data_01.1$toilet, "labels"))%>%
  rename(toilet = values, Toilet = ind)%>%
  write_csv(., "../0_Data/1_Household Data/1_Mongolia/2_Codes/Toilet.Code.csv")

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
  rename(item_code = values, item_name = ind)

Item.Code.all <- bind_rows(Item.Code, Item.Code.a)%>%
  bind_rows(Item.Code.c)%>%
  bind_rows(Item.Code.d)%>%
  bind_rows(Item.Code.e)%>%
  bind_rows(Item.Code.f)%>%
  distinct()

write.xlsx(Item.Code.all, "../0_Data/1_Household Data/1_Mongolia/3_Matching_Tables/Item_Code_Description_Mongolia.xlsx")

rm(list=ls())
