if(!require("pacman")) install.packages("pacman")

p_load("countrycode", "haven", "Hmisc", "openxlsx", "rattle", "scales", "tidyverse")

options(scipen=999)

# Author: L. Missbach

# Load data ####

data_0 <- read_dta("../0_Data/1_Household Data/1_Taiwan/1_Data_Raw/AA170044en/inc2019.dta")

# Transform data

data_0.1 <- data_0 %>%
  rename(hh_id = id, ind_hhh = a4, age_hhh = a6, sex_hhh = a7, hh_size = a8, edu_hhh = a11, adults = a12,
         water = c4, hh_weights = a20)%>%
  mutate(children = hh_size - adults)%>%
  select(hh_id, hh_size, hh_weights, adults, children, water, ind_hhh, age_hhh, sex_hhh, edu_hhh, water, itm430)%>%
  mutate(inc_gov_monetary = ifelse(!is.na(itm430), itm430,0),
         inc_gov_cash     = 0)%>%
  select(-itm430)%>%
  write_csv(., "../0_Data/1_Household Data/1_Taiwan/1_Data_Clean/household_information_Taiwan.csv")

# Appliances

data_0.2 <- data_0 %>%
  select(id, starts_with("f"))%>%
  rename(hh_id = id, tv.01 = f1, computer.01 = f38, car.01 = f34, motorcycle.01 = f23, ac.01 = f6, 
         washing_machine.01 = f12, vacuum.01 = f18, microwave.01 = f39, tv.01b = f59)%>%
  mutate(tv.01 = ifelse(tv.01 > 0 | tv.01b > 0,1,0))%>%
  mutate_at(vars(ends_with(".01")), list(~ ifelse(. > 0,1,0)))%>%
  select(hh_id, ends_with(".01"))%>%
  write_csv(., "../0_Data/1_Household Data/1_Taiwan/1_Data_Clean/appliances_0_1_Taiwan.csv")

# Expenditures

data_0.3 <- data_0 %>%
  select(id, itm1000:itm1228)%>%
  rename(hh_id = id)%>%
  pivot_longer(-hh_id, names_to = "item_code", values_to = "expenditures_year")%>%
  mutate(item_code = str_sub(item_code, 4,-1))%>%
  filter(!is.na(expenditures_year))%>%
  write_csv(., "../0_Data/1_Household Data/1_Taiwan/1_Data_Clean/expenditures_items_Taiwan.csv")

Item.Codes <- data_0 %>%
  select(itm1000:itm1228)%>%
  map_dfc(attr, 'label')%>%
  pivot_longer(itm1000:itm1228, values_to = "item_name", names_to = "item_code")%>%
  mutate(item_code = str_sub(item_code, 4,-1))%>%
  write.xlsx(., "../0_Data/1_Household Data/1_Taiwan/3_Matching_Tables/Item_Codes_Description_Taiwan.xlsx")

# Codes

Industry.Code <- stack(attr(data_0$a4, 'labels'))%>%
  rename(ind_hhh = values, Industry = ind)%>%
  write_csv(., "../0_Data/1_Household Data/1_Taiwan/2_Codes/Industry.Code.csv")
Gender.Code <- stack(attr(data_0$a7, 'labels'))%>%
  rename(sex_hhh = values, Gender = ind)%>%
  write_csv(., "../0_Data/1_Household Data/1_Taiwan/2_Codes/Gender.Code.csv")
Education.Code <- stack(attr(data_0$a11, 'labels'))%>%
  rename(edu_hhh = values, Education = ind)%>%
  mutate(ISCED = c(0,0,1,2,3,4,5,6,7,8))%>%
  write_csv(., "../0_Data/1_Household Data/1_Taiwan/2_Codes/Education.Code.csv")
Water.Code <- stack(attr(data_0$c4, 'labels'))%>%
  rename(water = values, Water = ind)%>%
  mutate(Water = c("No piped water equipment", "Piped water equipment"))%>%
  write_csv(., "../0_Data/1_Household Data/1_Taiwan/2_Codes/Water.Code.csv")
