# 1        Packages ####

if(!require("pacman")) install.packages("pacman")

p_load("haven", "Hmisc", "openxlsx", "rattle", "scales", "tidyverse")

options(scipen=999)

# 2        Load Data ####

c1 <- read_dta("H:/4_Action/1_Kenia_data/Data/community.dta")
c2 <- read_dta("H:/4_Action/1_Kenia_data/Data/Consumption_aggregate.dta") # contains overall information on households
# c3 <- read_dta("Data/Credit1.dta")             # contains information on applied an denied credits
# c4 <- read_dta("Data/Credit2.dta")             # contains information an applied an denied credits  
# c5 <- read_dta("Data/Credit3.dta")             # contains information on rejection of credits
e1 <- read_dta("H:/4_Action/1_Kenia_data/Data/Energy_use.dta")            # information on use of energy related items/fuels 
#(ONLY on use, not on consumption.) Might be relevant as an extension
f1 <- read_dta("H:/4_Action/1_Kenia_data/Data/food.dta")                  # information on food consumption
h1 <- read_dta("H:/4_Action/1_Kenia_data/Data/HH_Information.dta")        # information dwelling, housing and income
h2 <- read_dta("H:/4_Action/1_Kenia_data/Data/HH_Members_Information.dta") # employment and education of household members
# h3 <- read_dta("Data/Household_Enterprises.dta") # information on household enterprises
# j1 <- read_dta("Data/Justice.dta")               # bribes / informal payments
n1 <- read_dta("H:/4_Action/1_Kenia_data/Data/nonfood.dta")
# r1 <- read_dta("Data/recent_shocks.dta") # recent shocks
# l1 <- read_dta("Data/Livestock Output (M17_M22).dta")
# a1 <- read_dta("Data/Agriculture output (L1_L20).dta")

# Transform Data ####

# Household Data ####

c2.1 <- c2 %>%
  unite("hh_id", c("clid", "hhid"), sep = "0")%>%
  select(hh_id, resid, hhsize, weight, county)%>%
  rename(urban = resid, hh_size = hhsize, hh_weights = weight, province = county)%>%
  mutate(urban_01 = ifelse(urban == 1,0,1))%>%
  select(hh_id, hh_size, hh_weights, province, urban_01)

h1.1 <- h1 %>%
  unite("hh_id", c("clid", "hhid"), sep = "0")%>%
  select(hh_id, j01_dr, j10, j17, j18, j20)%>%
  rename(water = j01_dr, toilet = j10, lighting_fuel = j17, cooking_fuel = j18, electricity.access = j20)%>%
  mutate(electricity.access = ifelse(is.na(electricity.access),0,electricity.access),
         lighting_fuel      = ifelse(is.na(lighting_fuel),96, lighting_fuel),
         cooking_fuel       = ifelse(is.na(cooking_fuel),96,cooking_fuel),
         toilet             = ifelse(is.na(toilet),96, toilet),
         water              = ifelse(is.na(water),96,water))%>%
  mutate(electricity.access = ifelse(electricity.access == 1 | lighting_fuel == 1 | lighting_fuel == 2 | lighting_fuel == 3,1,0))

h2.1 <- h2 %>%
  unite("hh_id", c("clid", "hhid"), sep = "0")%>%
  select(hh_id, b03, b04, b05_yy, b14, c10_l)%>% # starts_with("c13"), starts_with("e13"), starts_with("e15"), starts_with("h04"))
  rename(sex_hhh = b04, age = b05_yy, religion = b14, edu_hhh = c10_l)

h2.11 <- h2.1 %>%
  select(hh_id, age)%>%
  mutate(adults   = ifelse(age > 15, 1,0),
         children = ifelse(age < 16,1,0))%>%
  group_by(hh_id)%>%
  summarise(adults   = sum(adults),
            children = sum(children))%>%
  ungroup()

h2.12 <- h2.1 %>%
  filter(b03 == 1)%>%
  select(hh_id, sex_hhh, religion, edu_hhh)

h1.3 <- h1 %>%
  unite("hh_id", c("clid", "hhid"), sep = "0")%>%
  select(hh_id, starts_with("o"), starts_with("p"))%>%
  select(hh_id, starts_with("o04"))%>%
  pivot_longer(-hh_id, names_to = "type", values_to = "inc_gov_cash")%>%
  mutate(inc_gov_cash     = ifelse(is.na(inc_gov_cash),0,inc_gov_cash))%>%
  mutate(inc_gov_monetary = 0)%>%
  group_by(hh_id)%>%
  summarise(inc_gov_cash     = sum(inc_gov_cash),
            inc_gov_monetary = sum(inc_gov_monetary))%>%
  ungroup()

household_information <- c2.1 %>%
  left_join(h1.1)%>%
  left_join(h2.11)%>%
  left_join(h2.12)%>%
  left_join(h1.3)

write_csv(household_information, "../0_Data/1_Household Data/2_Kenya/1_Data_Clean/household_information_Kenya.csv")

# For Raavi: Adding month and year

time_01 <- h1 %>%
  unite("hh_id", c("clid", "hhid"), sep = "0")%>%
  mutate(month = month(iday),
         year = year(iday))%>%
  select(hh_id, month, year)

# Expenditure Data / Enegery Expenditures ####

# Education in h2, also health - but also included in general survey

# h1.exp <- h1 %>%
#   unite("hh_id", c("clid", "hhid"), sep = "0")%>%
#   select(hh_id, i05, i10)%>%
#   pivot_longer(-hh_id, names_to = "item_code", values_to = "expenditures_year")%>%
#   filter(!is.na(expenditures_year))%>%
#   mutate(expenditures_year = expenditures_year*12)
# 
# h2.exp <- h2 %>%
#   unite("hh_id", c("clid", "hhid"), sep = "0")%>%
#   select(hh_id, starts_with("c13"), starts_with("e13"), starts_with("e15"), starts_with("h04"))%>%
#   mutate_at(vars(starts_with("e13")), list(~ .*12))%>%
#   mutate_at(vars(starts_with("h04")), list(~ .*4))%>%
#   pivot_longer(-hh_id, names_to = "item_code", values_to = "expenditures_year")%>%
#   filter(!is.na(expenditures_year))

f1.1 <- f1 %>%
  unite("hh_id", c("clid", "hhid"), sep = "0")%>%
  select(hh_id, item_code, cpurc_val, ownp_val, stk_val, gift_val)%>%
  rename(expenditures_year      = cpurc_val)%>%
  mutate(expenditures_sp_yearly = ownp_val + stk_val + gift_val)%>%
  select(hh_id, item_code, expenditures_year)

n1.1 <- n1%>%
  unite("hh_id", c("clid", "hhid"), sep = "0")%>%
  select(hh_id, nf01, recall, nf04_amt)%>%
  filter(nf01 != 2101 & nf01 != 2102)%>%
  rename(item_code = nf01, expenditures = nf04_amt)%>%
  mutate(expenditures_year = ifelse(recall == 1, expenditures*365/7, # weekly
                                      ifelse(recall == 2, expenditures*12, #monthly
                                             ifelse(recall == 3, expenditures*4, # quarterly
                                                    ifelse(recall == 4, expenditures, NA)))))%>% # yearly 
  select(-recall)

consumption <- bind_rows(f1.1, n1.1)%>%
  select(-expenditures)%>%
  #mutate(item_code = as.character(item_code))%>%
  #bind_rows(h1.exp)%>%
  #bind_rows(h2.exp)%>%
  filter(expenditures_year > 0 & !is.na(expenditures_year))

consumption <- left_join(consumption, time_01)

write_csv(consumption, "../0_Data/1_Household Data/2_Kenya/1_Data_Clean/expenditures_items_Kenya.csv")

# Items of Interest 

# items_of_interest <- read.xlsx("C:/Users/misl/OwnCloud/Fuel_Elasticities_in_LMIC/Items_Of_Interest.xlsx", sheet = "Kenya", colNames = FALSE)%>%
#   pivot_longer(-X1, names_to = "drop", values_to = "item_code")%>%
#   select(-drop)%>%
#   filter(!is.na(item_code))%>%
#   rename(Fuel = X1)
# 
# n1.2 <- n1.1 %>%
#   left_join(items_of_interest)%>%
#   filter(!is.na(Fuel))%>%
#   select(-expenditures, -item_code)%>%
#   pivot_wider(names_from = "Fuel", values_from = "expenditures_yearly")%>%
#   full_join(consumption)%>%
#   filter(!is.na(expenditures_yearly))
# 
# e1.1 <- e1 %>%
#   unite("hh_id", c("clid", "hhid"), sep = "0")%>%
#   select(-starts_with("j35"), -j26, -j27, - starts_with("j28"), - j34)%>%
#   # information on energy use purpose could theoretically be included (J28)
#   rename(expenditures_month = j33)%>%
#   filter(j25 == "BIOGAS" | j25 == "CHARCOAL" | j25 == "COLLECTED FIREWOOD" | j25 == "FARM RESIDUE (ANIMAL/CROP RESIDUE)" |
#            j25 == "GRID ELECTRICITY" | j25 == "KEROSENE/PARAFFIN" | j25 == "LPG" | j25 == "PURCHASED FIREWOOD" | j25 == "WOOD/PROCESS WASTE")%>%
#   mutate(quantity_kg = j29_q*j30)%>%
#   select(-starts_with("j29"), -j30)%>%
#   mutate(quantity = j32_n)%>%
#   select(-j32_u, -starts_with("j31"), -j32_n)%>%
#   rename(energy_item = j25)%>%
#   filter(!is.na(expenditures_month) | !is.na(quantity_kg) | !is.na(quantity))%>%
#   filter(expenditures_month > 0 | quantity_kg > 0 | quantity > 0)%>%
#   filter(expenditures_month < 9000)%>%
#   mutate(implicit_price = ifelse(!is.na(quantity_kg) & quantity_kg != 0, expenditures_month/quantity_kg, NA))%>%
#   mutate(Unit = ifelse(!is.na(implicit_price), "kg", NA))%>%
#   mutate(Unit = ifelse(energy_item == "KEROSENE/PARAFFIN", "litres",
#                        ifelse(energy_item == "LPG", "kg",
#                               ifelse(energy_item == "GRID ELECTRICITY", "kWh", Unit))))%>%
#   mutate(implicit_price = ifelse(is.na(implicit_price) & !is.na(quantity) & quantity != 0, expenditures_month/quantity, implicit_price))%>%
#   filter(!is.na(implicit_price))%>%
#   mutate(Fuel = ifelse(energy_item == "CHARCOAL", "Charcoal",
#                        ifelse(energy_item == "GRID ELECTRICITY", "Electricity",
#                               ifelse(energy_item == "KEROSENE/PARAFFIN", "Kerosene",
#                                      ifelse(energy_item == "LPG", "LPG",
#                                             ifelse(energy_item == "COLLECTED FIREWOOD" | energy_item == "PURCHASED FIREWOOD", "Firewood",
#                                                    ifelse(energy_item == "WOOD/PROCESS WASTE" | energy_item == "FARM RESIDUE (ANIMAL/CROP RESIDUE)", "Other Biomass", NA)))))))%>%
#   select(-j24)
# 
# h13 <- h12 %>%
#   select(hh_id, hh_weights, urban_01, province )
# 
# e1.1 <- left_join(e1.1, h13)
# 
# write_csv(e1.1, "Implicit_price_data_Kenya.csv")

# Appliance Data ####

# Fail

# Codierungen ####


Province.Code <- stack(attr(c2$county, 'labels'))%>%
  rename(province = values, Province = ind)%>%
  write_csv(., "../0_Data/1_Household Data/2_Kenya/2_Codes/Province.Code.csv")
Toilet.Code <- stack(attr(h1$j10, 'labels'))%>%
  rename(toilet = values, Toilet = ind)%>%
  write_csv(., "../0_Data/1_Household Data/2_Kenya/2_Codes/Toilet.Code.csv")
Water.Code <- stack(attr(h1$j01_dr, 'labels'))%>%
  rename(water = values, Water = ind)%>%
  write_csv(., "../0_Data/1_Household Data/2_Kenya/2_Codes/Water.Code.csv")
Lighting.Code <- stack(attr(h1$j17, 'labels'))%>%
  rename(lighting = values, Lighting_Fuel = ind)%>%
  write_csv(., "../0_Data/1_Household Data/2_Kenya/2_Codes/Lighting.Code.csv")
Cooking.Code <- stack(attr(h1$j18, 'labels'))%>%
  rename(cooking = values, Cooking_Fuel = ind)%>%
  write_csv(., "../0_Data/1_Household Data/2_Kenya/2_Codes/Cooking.Code.csv")
Education.Code <- stack(attr(h2$c10_l, 'labels'))%>%
  rename(education = values, Education = ind)%>%
  write_csv(., "../0_Data/1_Household Data/2_Kenya/2_Codes/Education.Code.csv")
Gender.Code <- stack(attr(h2$b04, 'labels'))%>%
  rename(gender = values, Gender = ind)%>%
  write_csv(., "../0_Data/1_Household Data/2_Kenya/2_Codes/Gender.Code.csv")
Religion.Code <- stack(attr(h2$b14, 'labels'))%>%
  rename(religion = values, Religion = ind)%>%
  write_csv(., "../0_Data/1_Household Data/2_Kenya/2_Codes/Religion.Code.csv")

Item.Code.1 <- stack(attr(f1$item_code, "labels"))%>%
  rename(item_code = values, item_name = ind)
Item.Code.2 <- stack(attr(n1$nf01, "labels"))%>%
  rename(item_code = values, item_name = ind)

Item.Code.Kenya <- bind_rows(Item.Code.1, Item.Code.2)%>%
  full_join(arrange(distinct(consumption, item_code), item_code))

write.xlsx(Item.Code.Kenya, "../0_Data/1_Household Data/2_Kenya/3_Matching_Tables/Item_Code_Description_Kenya.xlsx")

rm(list=ls())
