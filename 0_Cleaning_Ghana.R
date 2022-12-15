if(!require("pacman")) install.packages("pacman")

p_load("haven", "Hmisc", "openxlsx", "rattle", "scales", "tidyverse", "sjlabelled")

options(scipen=999)

# Load Data ####
basic <- read_dta("R:/MSA/datasets/Household Microdata/Ghana/GLSS7stata/g7stata/g7aggregates/13_GHA_2017_E.dta")
sec_0 <- read_dta("R:/MSA/datasets/Household Microdata/Ghana/GLSS7stata/g7stata/g7PartA/g7sec0.dta")
sec_1 <- read_dta("R:/MSA/datasets/Household Microdata/Ghana/GLSS7stata/g7stata/g7PartA/g7sec1.dta") # information on persons living in household
sec_2 <- read_dta("R:/MSA/datasets/Household Microdata/Ghana/GLSS7stata/g7stata/g7PartA/g7sec2.dta") # information on education
sec_4 <- read_dta("R:/MSA/datasets/Household Microdata/Ghana/GLSS7stata/g7stata/g7PartA/g7sec4-reviewed.dta")
sec_7 <- read_dta("R:/MSA/datasets/Household Microdata/Ghana/GLSS7stata/g7stata/g7PartA/g7sec7.dta") # information on dwelling
sec_9a <- read_dta("R:/MSA/datasets/Household Microdata/Ghana/GLSS7stata/g7stata/g7PartB/g7sec9a.dta")
sec_9b <- read_dta("R:/MSA/datasets/Household Microdata/Ghana/GLSS7stata/g7stata/g7PartB/g7sec9b.dta")
sec_11 <- read_dta("R:/MSA/datasets/Household Microdata/Ghana/GLSS7stata/g7stata/g7PartB/g7sec11c.dta")
sec_12 <- read_dta("R:/MSA/datasets/Household Microdata/Ghana/GLSS7stata/g7stata/g7PartB/g7sec12b.dta")
price <- read_dta("R:/MSA/datasets/Household Microdata/Ghana/GLSS7stata/g7stata/g7price/g7price.dta")

# Transform data ####

# Household information ####
# 14009 households

sec_0.1 <- sec_0 %>%
  select(hid, clust, district)%>%
  rename(hh_id = hid)

sec_1.1 <- sec_1 %>%
  select(hid, phid, s1q3, s1q5y, s1q10, s1q13, loc2, WTA_S, region)%>%
  rename(province = region, hh_id = hid, household.head = s1q3, age = s1q5y, religion = s1q10, ethnicity = s1q13, urban = loc2, hh_weights = WTA_S)%>%
  mutate(urban_01 = ifelse(urban == 1,1,0))%>%
  select(-urban)

sec_1.2 <- sec_1.1 %>%
  select(hh_id, phid, household.head, ethnicity, religion)%>%
  filter(household.head == 1)%>%
  rename(household.head.ID = phid)%>%
  select(-household.head)

sec_1.3 <- sec_1.1 %>%
  select(hh_id, phid, age)%>%
  mutate(adults   = ifelse(age >=18, 1, 0))%>%
  mutate(children = ifelse(age < 18, 1, 0))%>%
  group_by(hh_id)%>%
  summarise(adults = sum(adults),
            children = sum(children),
            hh_size = n())%>%
  ungroup()

sec_1.4 <- sec_1.1 %>%
  select(hh_id, urban_01, province, hh_weights)%>%
  filter(!duplicated(hh_id))%>%
  left_join(sec_1.2, by = "hh_id")%>%
  left_join(sec_1.3, by = "hh_id")%>%
  left_join(sec_0.1)

sec_2.1 <- sec_2%>%
  select(phid, s2aq3)%>%
  rename(edu_hhh = s2aq3, household.head.ID = phid)

sec_7.1 <- sec_7 %>%
  select(hid, s7dq1a1, s7dq11a, s7dq13b, s7dq19, s7dq26a)%>% # , starts_with("s7dq20")
  rename(hh_id = hid, water = s7dq1a1, electricity.access = s7dq11a, lighting_fuel = s7dq13b, cooking_fuel = s7dq19, 
         toilet = s7dq26a)%>%
  mutate(electricity.access = ifelse(electricity.access %in% c(1,2,3,4,5,6,7),1,0))

sec_4.1 <- sec_4 %>%
  select(phid, s4aq34b)%>%
  rename(ind_hhh = s4aq34b, household.head.ID = phid)

sec_11.1 <- sec_11%>%
  rename(hh_id = hid)%>%
  select(hh_id, 
         s11cq3, s11cq4,
         s11cq7, s11cq8,
         s11cq11, s11cq12,
         s11cq15, s11cq16)%>%
  mutate(s11cq34   = s11cq3*s11cq4,
         s11cq78   = s11cq7*s11cq8,
         s11cq1112 = s11cq11*s11cq12,
         s11cq1516 = s11cq15*s11cq16)%>%
  select(hh_id, s11cq34, s11cq78, s11cq1112, s11cq1516)%>%
  rename(inc_gov_cash = s11cq1112)%>%
  pivot_longer(c(s11cq34, s11cq78, s11cq1516), names_to = "item", values_to = "inc_gov_monetary")%>%
  mutate(inc_gov_monetary = ifelse(is.na(inc_gov_monetary), 0, inc_gov_monetary),
         inc_gov_cash     = ifelse(is.na(inc_gov_cash), 0, inc_gov_cash))%>%
  group_by(hh_id)%>%
  summarise(inc_gov_cash     = sum(inc_gov_cash),
            inc_gov_monetary = sum(inc_gov_monetary))%>%
  ungroup()

sec_127 <- left_join(sec_1.4, sec_2.1, by = "household.head.ID")%>%
  left_join(sec_4.1, by = "household.head.ID")%>%
  select(-household.head.ID)%>%
  left_join(sec_7.1, by = "hh_id")%>%
  left_join(sec_11.1, by = "hh_id")%>%
  mutate(inc_gov_cash     = ifelse(is.na(inc_gov_cash),     0, inc_gov_cash),
         inc_gov_monetary = ifelse(is.na(inc_gov_monetary), 0, inc_gov_monetary))

sec_127$hh_id <- str_remove(sec_1.4$hh_id, "[/]")

write_csv(sec_127, "../0_Data/1_Household Data/2_Ghana/1_Data_Clean/household_information_Ghana.csv")

# Expenditures ####

sec_7.2 <- sec_7 %>%
  select(hid, s7cq1a, s7cq1b, s7cq10, s7dq18a, s7dq18b)%>%
  rename(hh_id = hid, rent = s7cq1a, repair = s7cq10, ely = s7dq18a)%>%
  mutate(rent = ifelse(s7cq1b == 1, rent*365,
                       ifelse(s7cq1b == 2, rent*365/7,
                              ifelse(s7cq1b == 3, rent*12,
                                     ifelse(s7cq1b == 4, rent*4,
                                            ifelse(s7cq1b == 5, rent*2,
                                                   ifelse(s7cq1b == 6, rent, rent)))))))%>%
  arrange(rent)%>%
  mutate(ely = ifelse(s7dq18b == 1, ely*365,
                      ifelse(s7dq18b == 2, ely*365/7,
                             ifelse(s7dq18b == 3, ely*12,
                                    ifelse(s7dq18b == 4, ely*4,
                                           ifelse(s7dq18b == 5, ely*2,
                                                  ifelse(s7dq18b == 6, ely, ely)))))))%>%
  arrange(ely)%>%
  arrange(hh_id)%>%
  select(hh_id, rent, repair, ely)%>%
  remove_all_labels()

sec_7.2$hh_id <- str_remove(sec_7.2$hh_id, "[/]")
colnames(sec_7.2) <- c("hh_id", 100000,200000, 300000)


sec_9a.1 <- sec_9a%>%
  select(hid, lfreqcd, s9aname, s9aq1, s9aq2)%>%
  rename(hh_id = hid, item_code = lfreqcd, item_name = s9aname, expenditures = s9aq2)%>%
  filter(!is.na(item_code))%>%
  filter(!is.na(expenditures))%>%
  mutate(item_code = as.numeric(item_code))%>%
  mutate(item_code = ifelse(item_code <10, item_code*1000+1,
                                ifelse(item_code<100, item_code*1000+2, item_code*100)))

sec_9a.2 <- sec_9a.1 %>%
  select(hh_id, item_code, expenditures)%>%
  spread(key = item_code, value = expenditures)

sec_9a.2$hh_id <- str_remove(sec_9a.2$hh_id, "[/]") # yearly expenditures on less frequently purchased items

sec_9b.1 <- sec_9b %>% 
  select(hid, freqcd, s9bq1a, s9bq2a, s9bq3a, s9bq4a, s9bq5a, s9bq6a)%>%
  mutate(item_code = as.numeric(freqcd))%>%
  group_by(hid, item_code)%>%
  summarise(s9bq1a = sum(s9bq1a, na.rm = TRUE),
            s9bq2a = sum(s9bq2a, na.rm = TRUE),
            s9bq3a = sum(s9bq3a, na.rm = TRUE),
            s9bq4a = sum(s9bq4a, na.rm = TRUE),
            s9bq5a = sum(s9bq5a, na.rm = TRUE),
            s9bq6a = sum(s9bq6a, na.rm = TRUE))%>%
  mutate(expenditures = s9bq1a + s9bq2a + s9bq3a + s9bq4a + s9bq5a + s9bq6a)%>%
  ungroup()%>%
  mutate(expenditures = expenditures/30*365)%>%
  rename(hh_id = hid)

sec_9b.2 <- sec_9b.1%>%
  select(hh_id, item_code, expenditures)%>%
  filter(expenditures != 0)%>%
  spread(key = item_code, value = expenditures)

sec_9b.2$hh_id <- str_remove(sec_9b.2$hh_id, "[/]")

sec_final <- left_join(sec_9a.2, sec_9b.2, by = "hh_id")%>%
  left_join(sec_7.2, by = "hh_id")%>%
  pivot_longer(-hh_id, names_to = "item_code", values_to = "expenditures_year")%>%
  filter(!is.na(expenditures_year))

t <- data.frame(colnames(sec_final))%>%
  rename(item_code = colnames.sec_final.)%>%
  filter(item_code != "hh_id")%>%
  mutate(item_code = as.character(item_code))%>%
  mutate(item_code = as.numeric(item_code))%>%
  arrange(item_code)

write_csv(sec_final, "../0_Data/1_Household Data/2_Ghana/1_Data_Clean/expenditures_items_Ghana.csv")

# write_csv(t, "item_codes_Ghana.csv")
# write.xlsx(t, "T:/MSA/papers_internal/work_in_progress/Ag_Mi_Fuel_Elasticities_in_LMIC/Fuel_Elasticities_in_LMIC/1_Ghana/Data/Item_Codes_Ghana.xlsx")

# Price data for elasticities

# price_1 <- price %>%
#   rename(cluster = clust, province = region)%>%
#   select(cluster, province, district, everything())%>%
#   filter(ln %in% c(612,613,614,615,616,617,
#                    425, 428, 429, 430, 431)) # these are energy items of interest
# 
# price_1.1 <- price_1 %>%
#   select(cluster, province, district, ln, bname, pricea, quantitya, unita)%>%
#   rename(item_code = ln, item_name = bname, price = pricea, quantity = quantitya, unit = unita)
# 
# price_1.2 <- price_1 %>%
#   select(cluster, province, district, ln, bname, priceb, quantityb, unitb)%>%
#   rename(item_code = ln, item_name = bname, price = priceb, quantity = quantityb, unit = unitb)
# 
# price_1.3 <- price_1 %>%
#   select(cluster, province, district, ln, bname, pricec, quantityc, unitc)%>%
#   rename(item_code = ln, item_name = bname, price = pricec, quantity = quantityc, unit = unitc)
# 
# price_1.4 <- bind_rows(price_1.1, price_1.2, price_1.3)%>%
#   filter(!is.na(price))%>%
#   filter(unit != 3 & unit != 5 & unit != 6 & unit != 10 & unit != 14 & unit != 24 & unit != 47 & unit != 64 & unit != 67 & unit != 99)%>%
#   filter(item_code != 425 | unit != 30)%>%
#   filter(item_code != 428 | unit != 16)%>%
#   filter(item_code != 429 | unit != 74)%>%
#   filter(item_code == 430 & (unit == 73 | unit == 74) | item_code != 430)%>%
#   filter(item_code == 431 & unit == 1 | item_code != 431)%>%
#   mutate(price_per_unit = ifelse(item_code == 425, price/quantity,NA))%>%
#   filter(item_code != 425 | quantity != 1)%>%
#   mutate(price_per_unit = ifelse(item_code == 430 & unit == 74, price*1000/quantity, 
#                                  ifelse(item_code == 430 & unit == 73, price/quantity, price_per_unit)))%>%
#   filter(item_code != 430 | price_per_unit < 100)%>%
#   mutate(price_per_unit = ifelse(item_code %in% c(612,613,614,615,616,617), price/quantity, price_per_unit))%>%
#   mutate(price_per_unit = ifelse(item_code == 429 & unit == 38, price*1000/quantity,
#                                  ifelse(item_code == 429 & unit == 16, price/quantity, price_per_unit)))%>%
#   mutate(price_per_unit = ifelse(item_code == 428 & unit == 74, price*1000/quantity,
#                                  ifelse(item_code == 428 & unit == 73, price/quantity, price_per_unit)))%>%
#   filter(item_code != 428 | price_per_unit > 0.5 & price_per_unit < 21)%>%
#   mutate(Fuel = ifelse(item_name == "CHARCOAL", "Charcoal",
#                        ifelse(item_name == "DIESEL  PREMIER" | item_name == "DIESELEFFIMAX" | item_name == "GOIL DIESEL XP", "Diesel",
#                               ifelse(item_name == "ELECTRICITY, CENTRAL SUPPLY (5", "Electricity",
#                                      ifelse(item_name == "KEROSENE", "Kerosene",
#                                             ifelse(item_name == "LIQUEFIED GAS: PROPANE", "LPG",
#                                                    ifelse(item_name == "SUPER EFFIMAX" | item_name == "SUPER PREMIER" | item_name == "GOIL  SUPER XP", "Petrol", NA)))))))%>%
#   select(cluster, province, district, Fuel, price_per_unit)%>%
#   arrange(cluster, Fuel, price_per_unit)%>%
#   mutate(district = ifelse(nchar(district) == 1, paste0(province, "0", district),
#                            ifelse(nchar(district) == 2, paste0(province, district), NA)))
# write_csv(price_1.4, "T:/MSA/papers_internal/work_in_progress/Ag_Mi_Fuel_Elasticities_in_LMIC/Fuel_Elasticities_in_LMIC/1_Ghana/Data/Price_data_Ghana.csv")
# 
# 
# t <- count(price_1.4, item_code, item_name, unit)

# Appliances ####

sec_12.1 <- sec_12 %>%
  select(hid, assetcd, astnm, s12bq1a) # We are only interested if one household posseses one item, otherwise add bq1b and bq1c

sec_12.1$assetcd[is.na(sec_12.1$assetcd)] <- 0
sec_12.1$s12bq1a[is.na(sec_12.1$s12bq1a)] <- 0

sec_12.2 <- sec_12.1 %>% # we decide to rank ownership over usership "Yes, not working" is evaluated to "Yes"
  mutate(own = ifelse(s12bq1a == 1, 1,
                      ifelse(s12bq1a == 2, 1, 0)))%>%
  select(hid, astnm, own)%>%
  spread(key = "astnm", value = "own")%>%
  rename(hh_id = hid)%>%
  select(-V1, -'Tablet PC (e.g .ipad, galaxy tab,etc)', - 'Box iron', -'Toaster', -'Tree Crop/Plantation', -'Satellite dish', -'Shares', -'Outboard motor', -'Poultry/Livestock', -'Land/Plot',-"Electric kettle", -'House/building', -'Jewellery', -'Furniture (stuffed)', -'Furniture (not stuffed)', -'Food Processor/blender', -'Bed-furniture', -Boat, -'Cooking utensils', -'Clothes (Wax, Kente, etc.)', -'Camera/digital camera', -'Camera/Video', - 'CD-player', - 'Clock/Watch', -'Computer Accessories')

sec_12.3 <- sec_12.2 %>%
  rename(radio.01.1 = '3-in-one-radio  system/home theatre', ac.01 = 'Air conditioner', bicycle.01 = Bicycle, car.01 = 'Car', computer.01.2 = 'Desktop computer', fan.01 = Fan, freezer.01 = Freezer, generator.01 = 'Generator/plant', vacuum.01 = 'Hoover/Vacuum Cleaner', iron.01 = 'Iron (Electric)', computer.01.1 = 'Laptop computer', microwave.01 = 'Microwave', mobile.01 = 'Mobile phone', motorcycle.01 = 'Motor cycle', printer.01 = Printer, radio.01.2 = Radio, radio.01.3 = 'Radio Cassette', refrigerator.01 = 'Refrigerator', cooker.01 = 'Rice Cooker', sewing_machine.01 = 'Sewing machine', stove.e.01 = 'Stove (electric)', stove.g.01 = 'Stove (gas)', stove.k.01 = 'Stove (kerosene)', tv.01 = 'Television', video.01.1 = 'VCD/DVD/mp3/mp4 player/ipod', video.01.2 = 'Video cassette player', washing_machine.01 = 'Washing machine', boiler.01 = 'Water heater (bathroom)')

sec_12.4 <- sec_12.3 %>%
  mutate(computer.01 = ifelse(computer.01.2 == 1 | computer.01.1 == 1, 1, 0))%>%
  mutate(radio.01    = ifelse(radio.01.1 == 1 | radio.01.2 == 1 | radio.01.3 == 1, 1, 0))%>%
  mutate(video.01    = ifelse(video.01.1 == 1 | video.01.2 == 1, 1, 0))%>%
  select(-c(computer.01.1, computer.01.2, radio.01.1, radio.01.2, radio.01.3, video.01.1, video.01.2))%>%
  # Assumption affecting 105 households
  mutate_at(vars(-hh_id), list(~ ifelse(is.na(.),0,.)))

sec_12.4$hh_id <- str_remove(sec_12.4$hh_id, "[/]")

write_csv(sec_12.4, "../0_Data/1_Household Data/2_Ghana/1_Data_Clean/appliances_0_1_Ghana.csv")

# Codierungen ####

Education.Code <- stack(attr(sec_2$s2aq3, 'labels'))%>%
  rename(edu_hhh = values, Education = ind)%>%
  write_csv(., "../0_Data/1_Household Data/2_Ghana/2_Codes/Education.Code.csv")
Ethnicity.Code <- stack(attr(sec_1$s1q10, 'labels'))%>%
  rename(ethnicity = values, Ethnicity = ind)%>%
  write_csv(., "../0_Data/1_Household Data/2_Ghana/2_Codes/Ethnicity.Code.csv")
Water.Code <- stack(attr(sec_7$s7dq1a1, 'labels'))%>%
  rename(water = values, Water = ind)%>%
  write_csv(., "../0_Data/1_Household Data/2_Ghana/2_Codes/Water.Code.csv")
Lighting.Code <- stack(attr(sec_7$s7dq13b, 'labels'))%>%
  rename(lighting_fuel = values, Lighting_Fuel = ind)%>%
  write_csv(., "../0_Data/1_Household Data/2_Ghana/2_Codes/Lighting.Code.csv")
Cooking.Code <- stack(attr(sec_7$s7dq19, 'labels'))%>%
  rename(cooking_fuel = values, Cooking_Fuel = ind)%>%
  write_csv(., "../0_Data/1_Household Data/2_Ghana/2_Codes/Cooking.Code.csv")
Toilet.Code <- stack(attr(sec_7$s7dq26a, 'labels'))%>%
  rename(toilet = values, Toilet = ind)%>%
  write_csv(., "../0_Data/1_Household Data/2_Ghana/2_Codes/Toilet.Code.csv")
Industry.Code <- stack(attr(sec_4$s4aq34b, 'labels'))%>%
  rename(ind_hhh = values, Industry = ind)%>%
  write_csv(., "../0_Data/1_Household Data/2_Ghana/2_Codes/Industry.Code.csv")
District.Code <- distinct(sec_0, district)%>%
  arrange(district)%>%
  mutate(District = c())%>%
  write_csv(., "../0_Data/1_Household Data/2_Ghana/2_Codes/District.Code.csv")
Province.Code <- stack(attr(sec_1$region, 'labels'))%>%
  rename(province = values, Province = ind)%>%
  write_csv(., "../0_Data/1_Household Data/2_Ghana/2_Codes/Province.Code.csv")
Religion.Code <- stack(attr(sec_1$s1q10, 'labels'))%>%
  rename(religion = values, Religion = ind)%>%
  write_csv(., "../0_Data/1_Household Data/2_Ghana/2_Codes/Religion.Code.csv")
