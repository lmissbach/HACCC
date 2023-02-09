if(!require("pacman")) install.packages("pacman")

p_load("countrycode", "haven", "Hmisc", "openxlsx", "rattle", "scales", "tidyverse", "vietnameseConverter", "sjlabelled")

options(scipen=999)

# Loading Data ####

# Confidential Microdata
data_0a <- read_dta("R:/MSA/datasets/Household Microdata/Vietnam/VHLSS 2014/VHLSS2014_Households/Ho1.dta")
data_0b <- read_dta("R:/MSA/datasets/Household Microdata/Vietnam/VHLSS 2014/VHLSS2014_Households/Ho2.dta")
data_0c <- read_dta("R:/MSA/datasets/Household Microdata/Vietnam/VHLSS 2014/VHLSS2014_Households/Ho3.dta")
data_0d <- read_dta("R:/MSA/datasets/Household Microdata/Vietnam/VHLSS 2014/VHLSS2014_Households/Ho4.dta")

data_1a <- read_dta("R:/MSA/datasets/Household Microdata/Vietnam/VHLSS 2014/VHLSS2014_Households/Muc1A.dta")
data_1b <- read_dta("R:/MSA/datasets/Household Microdata/Vietnam/VHLSS 2014/VHLSS2014_Households/Muc1B.dta")
data_1c <- read_dta("R:/MSA/datasets/Household Microdata/Vietnam/VHLSS 2014/VHLSS2014_Households/Muc1C.dta")

data_2a <- read_dta("R:/MSA/datasets/Household Microdata/Vietnam/VHLSS 2014/VHLSS2014_Households/Muc2A.dta")
data_2b <- read_dta("R:/MSA/datasets/Household Microdata/Vietnam/VHLSS 2014/VHLSS2014_Households/Muc2X.dta")

data_3a <- read_dta("R:/MSA/datasets/Household Microdata/Vietnam/VHLSS 2014/VHLSS2014_Households/Muc3A.dta")
data_3b <- read_dta("R:/MSA/datasets/Household Microdata/Vietnam/VHLSS 2014/VHLSS2014_Households/Muc3B.dta")
data_3c <- read_dta("R:/MSA/datasets/Household Microdata/Vietnam/VHLSS 2014/VHLSS2014_Households/Muc3C.dta")

data_4a <- read_dta("R:/MSA/datasets/Household Microdata/Vietnam/VHLSS 2014/VHLSS2014_Households/Muc4a.dta")
# data_4b11 <- read_dta("R:/MSA/datasets/Household Microdata/Vietnam/VHLSS 2014/VHLSS2014_Households/Muc4B11.dta")
# data_4b12 <- read_dta("R:/MSA/datasets/Household Microdata/Vietnam/VHLSS 2014/VHLSS2014_Households/Muc4B12.dta")
# data_4b13 <- read_dta("R:/MSA/datasets/Household Microdata/Vietnam/VHLSS 2014/VHLSS2014_Households/Muc4B13.dta")
# data_4b14 <- read_dta("R:/MSA/datasets/Household Microdata/Vietnam/VHLSS 2014/VHLSS2014_Households/Muc4B14.dta")
# 
# data_4b15 <- read_dta("R:/MSA/datasets/Household Microdata/Vietnam/VHLSS 2014/VHLSS2014_Households/Muc4B15.dta")
# data_4b16 <- read_dta("R:/MSA/datasets/Household Microdata/Vietnam/VHLSS 2014/VHLSS2014_Households/Muc4B16.dta")
# data_4b17 <- read_dta("R:/MSA/datasets/Household Microdata/Vietnam/VHLSS 2014/VHLSS2014_Households/Muc4B17.dta")
# data_4b21 <- read_dta("R:/MSA/datasets/Household Microdata/Vietnam/VHLSS 2014/VHLSS2014_Households/Muc4B21.dta")
# data_4b22 <- read_dta("R:/MSA/datasets/Household Microdata/Vietnam/VHLSS 2014/VHLSS2014_Households/Muc4B22.dta")
# 
# data_4b31 <- read_dta("R:/MSA/datasets/Household Microdata/Vietnam/VHLSS 2014/VHLSS2014_Households/Muc4B31.dta")
# data_4b32 <- read_dta("R:/MSA/datasets/Household Microdata/Vietnam/VHLSS 2014/VHLSS2014_Households/Muc4B32.dta")
# data_4b41 <- read_dta("R:/MSA/datasets/Household Microdata/Vietnam/VHLSS 2014/VHLSS2014_Households/Muc4B41.dta")
# data_4b42 <- read_dta("R:/MSA/datasets/Household Microdata/Vietnam/VHLSS 2014/VHLSS2014_Households/Muc4B42A.dta")
# data_4b51 <- read_dta("R:/MSA/datasets/Household Microdata/Vietnam/VHLSS 2014/VHLSS2014_Households/Muc4B51.dta")
# 
# data_4b52 <- read_dta("R:/MSA/datasets/Household Microdata/Vietnam/VHLSS 2014/VHLSS2014_Households/Muc4B52.dta")
# data_4c1 <- read_dta("R:/MSA/datasets/Household Microdata/Vietnam/VHLSS 2014/VHLSS2014_Households/Muc4C1.dta")
# data_4c2 <- read_dta("R:/MSA/datasets/Household Microdata/Vietnam/VHLSS 2014/VHLSS2014_Households/Muc4C2.dta")
data_4d <- read_dta("R:/MSA/datasets/Household Microdata/Vietnam/VHLSS 2014/VHLSS2014_Households/Muc4D.dta")

data_5a1 <- read_dta("R:/MSA/datasets/Household Microdata/Vietnam/VHLSS 2014/VHLSS2014_Households/Muc5a1.dta")
data_5a2 <- read_dta("R:/MSA/datasets/Household Microdata/Vietnam/VHLSS 2014/VHLSS2014_Households/Muc5a2.dta")
data_5b1 <- read_dta("R:/MSA/datasets/Household Microdata/Vietnam/VHLSS 2014/VHLSS2014_Households/Muc5b1.dta")
data_5b2 <- read_dta("R:/MSA/datasets/Household Microdata/Vietnam/VHLSS 2014/VHLSS2014_Households/Muc5b2.dta")
data_5b3 <- read_dta("R:/MSA/datasets/Household Microdata/Vietnam/VHLSS 2014/VHLSS2014_Households/Muc5b3.dta")

#data_6a <- read_dta("R:/MSA/datasets/Household Microdata/Vietnam/VHLSS 2014/VHLSS2014_Households/Muc6.dta")  # Appliances
data_6b <- read_dta("R:/MSA/datasets/Household Microdata/Vietnam/VHLSS 2014/VHLSS2014_Households/Muc6b.dta") # Appliances

data_7 <- read_dta("R:/MSA/datasets/Household Microdata/Vietnam/VHLSS 2014/VHLSS2014_Households/Muc7.dta")   # Housing and electricity
data_8 <- read_dta("R:/MSA/datasets/Household Microdata/Vietnam/VHLSS 2014/VHLSS2014_Households/Muc8.dta")
data_9 <- read_dta("R:/MSA/datasets/Household Microdata/Vietnam/VHLSS 2014/VHLSS2014_Households/Muc9.dta")

data_91 <- read_dta("R:/MSA/datasets/Household Microdata/Vietnam/VHLSS 2014/VHLSS2014_Households/Muc82.dta")
data_91 <- read_dta("R:/MSA/datasets/Household Microdata/Vietnam/VHLSS 2014/VHLSS2014_Households/Muc91.dta")
data_92 <- read_dta("R:/MSA/datasets/Household Microdata/Vietnam/VHLSS 2014/VHLSS2014_Households/Muc92.dta")
data_92a <- read_dta("R:/MSA/datasets/Household Microdata/Vietnam/VHLSS 2014/VHLSS2014_Households/Muc92T.dta")
data_93 <- read_dta("R:/MSA/datasets/Household Microdata/Vietnam/VHLSS 2014/VHLSS2014_Households/Muc93.dta")
data_94 <- read_dta("R:/MSA/datasets/Household Microdata/Vietnam/VHLSS 2014/VHLSS2014_Households/Muc94.dta")
data_95 <- read_dta("R:/MSA/datasets/Household Microdata/Vietnam/VHLSS 2014/VHLSS2014_Households/Muc95.dta")
data_96 <- read_dta("R:/MSA/datasets/Household Microdata/Vietnam/VHLSS 2014/VHLSS2014_Households/Muc96.dta")
data_97 <- read_dta("R:/MSA/datasets/Household Microdata/Vietnam/VHLSS 2014/VHLSS2014_Households/Muc97.dta")
data_92004 <- read_dta("R:/MSA/datasets/Household Microdata/Vietnam/VHLSS 2014/VHLSS2014_Households/Muc92004.dta")
data_92014 <- read_dta("R:/MSA/datasets/Household Microdata/Vietnam/VHLSS 2014/VHLSS2014_Households/Muc92014.dta")

data_c_00 <- read_dta("R:/MSA/datasets/Household Microdata/Vietnam/VHLSS 2014/VHLSS2014_Commues/Bia.dta")
data_c_0 <- read_dta("R:/MSA/datasets/Household Microdata/Vietnam/VHLSS 2014/VHLSS2014_Commues/Muc0.dta")
data_c_1 <- read_dta("R:/MSA/datasets/Household Microdata/Vietnam/VHLSS 2014/VHLSS2014_Commues/Muc1.dta")
data_c_2 <- read_dta("R:/MSA/datasets/Household Microdata/Vietnam/VHLSS 2014/VHLSS2014_Commues/Muc2.dta")
data_c_3 <- read_dta("R:/MSA/datasets/Household Microdata/Vietnam/VHLSS 2014/VHLSS2014_Commues/Muc3.dta")
data_c_4a <- read_dta("R:/MSA/datasets/Household Microdata/Vietnam/VHLSS 2014/VHLSS2014_Commues/Muc4A.dta")
data_c_4b <- read_dta("R:/MSA/datasets/Household Microdata/Vietnam/VHLSS 2014/VHLSS2014_Commues/Muc4B.dta")
data_c_5a <- read_dta("R:/MSA/datasets/Household Microdata/Vietnam/VHLSS 2014/VHLSS2014_Commues/Muc5A.dta")
data_c_5b <- read_dta("R:/MSA/datasets/Household Microdata/Vietnam/VHLSS 2014/VHLSS2014_Commues/Muc5B.dta")
data_c_6a <- read_dta("R:/MSA/datasets/Household Microdata/Vietnam/VHLSS 2014/VHLSS2014_Commues/Muc6A.dta")
data_c_6b <- read_dta("R:/MSA/datasets/Household Microdata/Vietnam/VHLSS 2014/VHLSS2014_Commues/Muc6B.dta")
data_c_6c <- read_dta("R:/MSA/datasets/Household Microdata/Vietnam/VHLSS 2014/VHLSS2014_Commues/Muc6C.dta")
data_c_7 <- read_dta("R:/MSA/datasets/Household Microdata/Vietnam/VHLSS 2014/VHLSS2014_Commues/Muc7.dta")
data_c_9a <- read_dta("R:/MSA/datasets/Household Microdata/Vietnam/VHLSS 2014/VHLSS2014_Commues/Muc9A.dta")
data_c_9b <- read_dta("R:/MSA/datasets/Household Microdata/Vietnam/VHLSS 2014/VHLSS2014_Commues/Muc9B.dta")

# No weights for 2014 provided --> weights for 2012
wt2012new <- read_dta("T:/MSA/papers_internal/work_in_progress/HH Surveys Paper/Vietnam_analysis/Data/Data_2012/wt2012new.dta")%>%
  rename(hh_weights = wt9)

# Transforming data ####

# Household information 

data_0a.1 <- data_0a %>%
  unite("hh_id", tinh:hoso, sep = "", remove = FALSE)%>%
  left_join(select(wt2012new, -diaban), by = c("tinh", "huyen", "xa"))%>%
  mutate(urban_01 = ifelse(ttnt == 1,1,0))%>%
  rename(province = tinh, district = huyen, ethnicity = dantoc)%>%
  select(hh_id, province, district, urban_01, ethnicity, hh_weights)%>%
  group_by(province, district)%>%
  mutate(min_weights_district = min(hh_weights, na.rm = TRUE))%>%
  ungroup()%>%
  group_by(province)%>%
  mutate(min_weights_province = min(hh_weights, na.rm = TRUE))%>%
  ungroup()%>%
  mutate(hh_weights = ifelse(is.na(hh_weights) & min_weights_district != Inf, min_weights_district, 
                             ifelse(is.na(hh_weights), min_weights_province, hh_weights)))%>%
  select(hh_id, province, district, urban_01, ethnicity, hh_weights)

data_1a.1 <- data_1a %>%
  unite("hh_id", tinh:hoso, sep = "", remove = FALSE)%>%
  filter(m1ac3 == 1)%>%
  rename(sex_hhh = m1ac2, age_hhh = m1ac5)%>%
  select(hh_id, matv, sex_hhh, age_hhh)

data_1a.2 <- data_1a %>%
  unite("hh_id", tinh:hoso, sep = "", remove = FALSE)%>%
  mutate(adults   = ifelse(m1ac5 > 15,1,0),
         children = ifelse(m1ac5 < 16,1,0))%>%
  group_by(hh_id)%>%
  summarise(adults   = sum(adults),
            children = sum(children),
            hh_size  = n())%>%
  ungroup()

data_2a.1 <- data_2a %>%
  unite("hh_id", tinh:hoso, sep = "", remove = TRUE)%>%
  select(hh_id, matv, m2ac2a)%>%
  rename(edu_hhh = m2ac2a)%>%
  mutate(edu_hhh = ifelse(is.na(edu_hhh),0,edu_hhh))

data_4a.1 <- data_4a %>%
  unite("hh_id", tinh:hoso, sep = "", remove = TRUE)%>%
  select(hh_id, matv, m4ac4)%>%
  rename(ind_hhh = m4ac4)

data_4d.1 <- data_4d %>%
  unite("hh_id", tinh:hoso, sep = "", remove = TRUE)%>%
  mutate(inc_gov_monetary = m4dc2_13 + m4dc2_14 + m4dc2_15)%>%
  select(hh_id, inc_gov_monetary)%>%
  mutate(inc_gov_cash = 0)

data_8.1 <- data_8 %>%
  unite("hh_id", tinh:hoso, sep = "", remove = TRUE)%>%
  select(hh_id, m8c2aa:m8c2ae)%>%
  mutate(inc_gov_monetary = m8c2aa + m8c2ab + m8c2ac + m8c2ad + m8c2ae)%>%
  mutate(inc_gov_cash = 0)%>%
  select(hh_id, inc_gov_cash, inc_gov_monetary)
  

data_48 <- bind_rows(data_4d.1, data_8.1)%>%
  group_by(hh_id)%>%
  summarise(inc_gov_cash     = sum(inc_gov_cash),
            inc_gov_monetary = sum(inc_gov_monetary))%>%
  ungroup()

data_7.1 <- data_7 %>%
  unite("hh_id", tinh:hoso, sep = "", remove = TRUE)%>%
  select(hh_id, m7c18, m7c21, m7c22, m7c23, m7c24)%>%
  mutate(electricity.access = ifelse(m7c22 == 1 | m7c23 > 0 | m7c24 > 0,1,0))%>%
  rename(water = m7c18, toilet = m7c21, lighting_fuel = m7c22)%>%
  select(hh_id, water, toilet, lighting_fuel, electricity.access)

household_information <- data_1a.1 %>%
  left_join(data_0a.1)%>%
  left_join(data_1a.2)%>%
  left_join(data_2a.1)%>%
  left_join(data_48)%>%
  left_join(data_4d.1)%>%
  left_join(data_7.1)%>%
  select(-matv)

write_csv(household_information, "../0_Data/1_Household Data/1_Vietnam/1_Data_Clean/household_information_Vietnam.csv")

# Expenditure information

data_2b.1 <- data_2b %>%
  unite("hh_id", tinh:hoso, sep = "", remove = TRUE)%>%
  select(hh_id, m2xc11a:m2xc11i)%>%
  rename("38" = "m2xc11a", 
         "39" = "m2xc11b", 
         "40" = "m2xc11c", 
         "41" = "m2xc11d", 
         "42" = "m2xc11e", 
         "43" = "m2xc11f", 
         "44" = "m2xc11g", 
         "45" = "m2xc11h", 
         "46" = "m2xc11i")%>%
  pivot_longer(-hh_id, names_to = "item_code", values_to = "expenditures_year")%>%
  filter(!is.na(expenditures_year) & expenditures_year > 0)%>%
  mutate(item_code = as.numeric(item_code))%>%
  mutate(expenditures_sp_year = 0)%>%
  mutate(Type = "2B")

data_7.2 <- data_7 %>%
  unite("hh_id", tinh:hoso, sep = "", remove = TRUE)%>%
  select(hh_id, m7c8, m7c14, m7c20, m7c24, m7c26)%>%
  rename("8" = m7c8, "14" = m7c14, "20" = m7c20, "24" = m7c24, "26" = m7c26)%>%
  remove_all_labels()%>%
  pivot_longer(-hh_id, names_to = "item_code", values_to = "expenditures_year")%>%
  filter(expenditures_year > 0 & !is.na(expenditures_year))%>%
  mutate(item_code = str_replace(item_code, "X",""))%>%
  mutate(item_code = as.numeric(item_code)+1000)%>%
  remove_all_labels()%>%
  mutate(expenditures_sp_year = 0)%>%
  mutate(Type = "7")

# Consumption 

# Food at festive occasions

# Check item codes

data_5a1.1 <- data_5a1 %>%
  unite("hh_id", tinh:hoso, sep = "", remove = TRUE)%>%
  rename(item_code = m5a1ma, expenditures_year = m5a1c2b, expenditures_sp_year = m5a1c3b)%>%
  select(hh_id, item_code, expenditures_year, expenditures_sp_year)%>%
  arrange(hh_id, item_code)%>%
  mutate(expenditures_year    = ifelse(is.na(expenditures_year),0, expenditures_year),
         expenditures_sp_year = ifelse(is.na(expenditures_sp_year),0, expenditures_sp_year))%>%
  group_by(hh_id, item_code)%>%
  summarise(expenditures_year = sum(expenditures_year),
            expenditures_sp_year = sum(expenditures_sp_year))%>%
  ungroup()%>%
  mutate(expenditures_year    = ifelse(item_code == 118, expenditures_year/2, expenditures_year),
         expenditures_sp_year = ifelse(item_code == 118, expenditures_sp_year/2, expenditures_sp_year))%>%
  remove_all_labels()%>%
  mutate(Type = "5A1")

# Regular food consumption

# Check item codes

data_5a2.1 <- data_5a2 %>%
  unite("hh_id", tinh:hoso, sep = "", remove = TRUE)%>%
  rename(item_code = m5a2ma, expenditures_year = m5a2c4b, expenditures_sp_year = m5a2c5b)%>%
  mutate(expenditures_year    = ifelse(is.na(expenditures_year),0,expenditures_year*12),
         expenditures_sp_year = ifelse(is.na(expenditures_sp_year),0,expenditures_sp_year*12))%>%
  group_by(hh_id, item_code)%>%
  summarise(expenditures_year    = sum(expenditures_year),
            expenditures_sp_year = sum(expenditures_sp_year))%>%
  ungroup()%>%
  mutate(expenditures_year    = ifelse(item_code %in% c(101,117,118,141,153), expenditures_year/2, expenditures_year),
         expenditures_sp_year = ifelse(item_code %in% c(101,117,118,141,153), expenditures_sp_year/2, expenditures_sp_year))%>%
  remove_all_labels()%>%
  mutate(Type = "5A2")

# Regular Consumption on a monthly basis

data_5b1.1 <- data_5b1 %>%
  unite("hh_id", tinh:hoso, sep = "", remove = TRUE)%>%
  rename(item_code = m5b1ma, expenditures_year = m5b1c3, expenditures_sp_year = m5b1c4)%>%
  mutate(expendtiures_year    = expenditures_year*12,
         expenditures_sp_year = expenditures_sp_year*12)%>%
  select(hh_id, item_code, expenditures_year, expenditures_sp_year)%>%
  remove_all_labels()%>%
  mutate(Type = "5B1")

data_5b2.1 <- data_5b2 %>%
  unite("hh_id", tinh:hoso, sep = "", remove = TRUE)%>%
  rename(item_code = m5b2ma, expenditures_year = m5b2c2, expenditures_sp_year = m5b2c3)%>%
  select(hh_id, item_code, expenditures_year, expenditures_sp_year)%>%
  remove_all_labels()%>%
  mutate(Type = "5B2")

data_5b3.1 <- data_5b3 %>%
  unite("hh_id", tinh:hoso, sep = "", remove = TRUE)%>%
  rename(item_code = m5b3ma, expenditures_year = m5b3c2)%>%
  select(hh_id, item_code, expenditures_year)%>%
  mutate(expenditures_sp_year = 0)%>%
  remove_all_labels()%>%
  mutate(Type = "5B3")

data_6b.2 <- data_6b %>%
  unite("hh_id", tinh:hoso, sep = "", remove = TRUE)%>%
  select(hh_id, m6bma, m6bc5)%>%
  filter(!is.na(m6bc5))%>%
  rename(item_code = m6bma, expenditures_year = m6bc5)%>%
  group_by(hh_id, item_code)%>%
  summarise(expenditures_year = sum(expenditures_year))%>%
  ungroup()%>%
  remove_all_labels()%>%
  mutate(expenditures_sp_year = 0)%>%
  mutate(Type = "6B")

expenditures_items <- data_7.2 %>%
  bind_rows(data_2b.1)%>%
  bind_rows(data_5a1.1)%>%
  bind_rows(data_5a2.1)%>%
  bind_rows(data_5b1.1)%>%
  bind_rows(data_5b2.1)%>%
  bind_rows(data_5b3.1)%>%
  bind_rows(data_6b.2)%>%
  arrange(hh_id, item_code)

Item.Codes <- expenditures_items %>%
  group_by(item_code, Type)%>%
  summarise(number = n())%>%
  ungroup()%>%
  group_by(item_code)%>%
  mutate(number_b = n())%>%
  ungroup()%>%
  select(-number, -number_b)

write.xlsx(Item.Codes, "../0_Data/1_Household Data/1_Vietnam/3_Matching_Tables/Item_Code_Description_Vietnam_Raw.xlsx")

expenditures_items_1 <- expenditures_items %>%
  group_by(hh_id, item_code)%>%
  summarise(expenditures_year    = sum(expenditures_year),
            expenditures_sp_year = sum(expenditures_sp_year))%>%
  ungroup()

write_csv(expenditures_items_1, "../0_Data/1_Household Data/1_Vietnam/1_Data_Clean/expenditures_items_Vietnam.csv")

# Appliances

data_6b.1 <- data_6b %>%
  unite("hh_id", tinh:hoso, sep = "", remove = TRUE)%>%
  select(hh_id, m6bma, m6bc3)%>%
  mutate(m6bc3 = ifelse(m6bc3 > 0,1,m6bc3))%>%
  distinct()%>%
  pivot_wider(names_from = m6bma, values_from = m6bc3, values_fill = 0)%>%
  select(hh_id, sort(names(.)))%>%
  rename(car.01 = '1', 
         motorcycle.01 = '2',   
         mobile.01 = '12', 
         TV.01a = '15', TV.01b = '16', 
         computer.01 = '20', 
         refrigerator.01 = '22', 
         ac.01 = '23', 
         washing_machine.01 = '24', 
         fan.01 = '25', 
         gas.cookers.01 = '27', electric.cookers.01 = '28', 
         vacuum.01 = '33', microwave.01 = '34')%>%
  select(hh_id, ends_with(".01"), TV.01a, TV.01b)%>%
  right_join(distinct(household_information, hh_id))%>%
  mutate_at(vars(-hh_id), list(~ ifelse(is.na(.),0,.)))%>%
  arrange(hh_id)%>%
  mutate(tv.01     = ifelse(TV.01a > 0 | TV.01b > 0,1,0),
         stove.01  = ifelse(gas.cookers.01 > 0 | electric.cookers.01 > 0,1,0))%>%
  select(-gas.cookers.01, -electric.cookers.01, -TV.01a, -TV.01b)

write_csv(data_6b.1, "../0_Data/1_Household Data/1_Vietnam/1_Data_Clean/appliances_0_1_Vietnam.csv")

# Codes

Province.Code <- stack(attr(data_0a$tinh, 'labels'))%>%
  mutate(ind = stri_trans_general(ind, "Latin-ASCII"))%>%
  decodeVN(., to = "Unicode", from = "TCVN3")%>%
  rename(Province = ind, province = values)%>%
  write_csv(., "../0_Data/1_Household Data/1_Vietnam/2_Codes/Province.Code.csv")

District.Code <- distinct(data_0a, huyen)%>%
  arrange(huyen)%>%
  rename(district = huyen)%>%
  mutate(District = district)%>%
  write_csv(., "../0_Data/1_Household Data/1_Vietnam/2_Codes/District.Code.csv")

Ethnicity.Code <- data.frame("ethnicity" = c(seq(1,56, by = 1)),
                             "Ethnicity" = c("kinh", "tay", "thai", "chinese", "khmer", "muong", "nung", "hmong (MEO)", "dao", "jrai", "ngai", "ede", "bana", "sedang", "sanchay (Cao lan - San chi)", "co ho", "Cham ", "san diu", "hre", "mnong", "raglai", "stieng", "bru - Van Kieu", "tho", "giay", "co tu", "gie- trieng", "ma", "kho mu", "co", "ta - oi", "choro", "khang", "singmun", "hanhi", "churu", "lao", "lachi", "laha", "Phula", "lahu", "lu", "lolo", "Chut", "Mang", "pathen", "colao", "cong", "bo y", "si la", "pu peo", "brau", "odu", "romam", "foreigner", "unspecified"))%>% # Attention - comes from coding of 2010 (should be checked if used)
  filter(ethnicity %in% data_0a.1$ethnicity)%>%
  write_csv(., "../0_Data/1_Household Data/1_Vietnam/2_Codes/Ethnicity.Code.csv")

Education.Code <- stack(attr(data_2a$m2ac2a, 'labels'))%>%
  rename(edu_hhh = values)%>%
  mutate(Education = c("No answer", "No degree", "Primary","Lower secondary",
                       "Higher secondary", "College", "University", "Masters", "PhD", "Others"))%>%
  mutate(ISCED = c(9,0,1,2,3,3,6,7,8,9))%>%
  select(-ind)%>%
  write_csv(., "../0_Data/1_Household Data/1_Vietnam/2_Codes/Education.Code.csv")

Gender.Code <- stack(attr(data_1a$m1ac2, 'labels'))%>%
  rename(sex_hhh = values)%>%
  mutate(Gender = c("Male", "Female"))%>%
  write_csv(., "../0_Data/1_Household Data/1_Vietnam/2_Codes/Gender.Code.csv")

Industry.Code <- distinct(data_4a, m4ac4)%>%
  arrange(m4ac4)%>%
  rename(ind_hhh = m4ac4)%>%
  write_csv(., "../0_Data/1_Household Data/1_Vietnam/2_Codes/Industry.Code.csv")

Water.Code <- stack(attr(data_7$m7c18, 'labels'))%>%
  rename(water = values)%>%
  mutate(Water = c("Tap water", "Public tap water", "Drilled well", "Protected dug well", "Unprotected dug well", "Protected stream water",
                   "Unprotected stream water", "Bought water", "Rain water", "Other"))%>%
  mutate(WTR = c("Basic", "Basic", "Basic", "Basic", "Limited", "Basic", "Limited", "Basic", "Limited", "Limited"))%>%
  select(-ind)%>%
  write_csv(., "../0_Data/1_Household Data/1_Vietnam/2_Codes/Water.Code.csv")

Toilet.Code <- stack(attr(data_7$m7c21, 'labels'))%>%
  rename(toilet = values)%>%
  mutate(Toilet = c("Septic tank", "Suilabh", "Improved toilet with vent", "Double septic tank",
                    "Barrel/pot", "Fishing bridge", "Others", "None"))%>%
  mutate(TLT = c("Basic", "Basic", "Basic", "Basic", "Limited", "Limited", "Limited", "No Service"))%>%
  select(-ind)%>%
  write_csv(., "../0_Data/1_Household Data/1_Vietnam/2_Codes/Toilet.Code.csv")

Lighting.Code <- stack(attr(data_7$m7c22, 'labels'))%>%
  rename(lighting_fuel = values)%>%
  mutate(Lighting_Fuel = c("Electricity", "Battery or generator", "Gas, oil lmaps of various kinds", "Others"))%>%
  select(-ind)%>%
  write_csv(., "../0_Data/1_Household Data/1_Vietnam/2_Codes/Lighting.Code.csv")
