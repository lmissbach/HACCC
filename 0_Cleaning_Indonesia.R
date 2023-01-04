# 1        Packages ####

if(!require("pacman")) install.packages("pacman")

p_load("countrycode", "haven", "Hmisc", "openxlsx", "rattle", "scales", "tidyverse", "foreign")

# 2        Set Working Directory ####

# Loading Data ####

kor_a_0    <- read.dbf("R:/MSA/datasets/Household Microdata/Indonesia/2018/Data_Kor_2018~/Susenas 2018 Maret Kor/kor18rt_diseminasi.dbf")
kor_b_0    <- read.dbf("R:/MSA/datasets/Household Microdata/Indonesia/2018/Data_Kor_2018~/Susenas 2018 Maret Kor/kor18ind_revisi_diseminasi.dbf")
mod_41_a_0 <- read.dbf("R:/MSA/datasets/Household Microdata/Indonesia/2018/Data_Mod_2018~/blok41_gab_prop11-33_diseminasi.dbf")
mod_41_b_0 <- read.dbf("R:/MSA/datasets/Household Microdata/Indonesia/2018/Data_Mod_2018~/blok41_gab_prop34-94_diseminasi.dbf")
mod_42_0   <- read.dbf("R:/MSA/datasets/Household Microdata/Indonesia/2018/Data_Mod_2018~/blok42.dbf")

# Transforming Data ####

# nrow(count(kor_a_0, URUT))    # 295,155
# nrow(count(kor_b_0, URUT))    # 295,155
# nrow(count(mod_41_a_0, URUT)) # 140,683
# nrow(count(mod_41_b_0, URUT)) # 154,472
# nrow(count(mod_42_0, URUT))   # 295,155
 
# Household Information ####

kor_a_1 <- kor_a_0 %>%
  select(URUT, R101, R102, KABU, R105, R1510A, R1511A, R1518A, R1519, starts_with("R1801"), FWT, R301)%>%
  rename(hh_id = URUT, urban = R105, hh_weights = FWT, hh_size = R301)%>%
  rename(province = R101, district = R102, village = KABU)

kor_a_2 <- kor_a_1 %>%
  select(hh_id, urban, hh_weights, hh_size)%>%
  mutate(urban_01 = ifelse(urban == 1, 1, 0))

# Detailed household information

kor_a_3 <- kor_a_1 %>%
  rename(toilet = R1510A, water = R1511A, lighting_fuel = R1518A, cooking_fuel = R1519)%>%
  select(-starts_with("R1801"), - hh_size, - hh_weights, - urban)%>%
  mutate(electricity.access = ifelse(lighting_fuel == 4,0,1))

# lighting source is electricity indicator

kor_a_4 <- kor_a_0 %>%
  rename(hh_id = URUT)%>%
  select(hh_id, R1606I_K3, R1606II_K3, R1606IIIK3,
         R1612A_I, R1612A_II, R1612A_III, R1612A_IV, R1612B_I)%>%
  mutate_at(vars(-hh_id), list(~ifelse(is.na(.),0,.)))%>%
  pivot_longer(-hh_id, names_to = "type", values_to = "value")%>%
  group_by(hh_id)%>%
  summarise(inc_gov_cash     = sum(value),
            inc_gov_monetary = 0)%>%
  ungroup()

# see codes below

appliances_01 <- kor_a_1 %>%
  select(hh_id, starts_with("R1801"))%>%
  mutate_at(vars(-hh_id), list(function(x) x = ifelse(x == 5, 0, x)))%>%
  rename(refrigerator.01 = R1801B,
          ac.01          = R1801C,
          boiler.01      = R1801D,
          computer.01    = R1801F,
          motorcycle.01  = R1801H,
          car.01         = R1801K,
          tv.01          = R1801L)%>%
  select(-starts_with("R1801"))

write_csv(appliances_01, "../0_Data/1_Household Data/1_Indonesia/1_Data_Clean/appliances_0_1_Indonesia.csv")

# Information on household members 

kor_b_1 <- kor_b_0 %>%
  select(URUT, R401, R403, R405, R407, R613, R619, R717, R805)%>%
  rename(hh_id = URUT, ID = R401, household.head = R403, sex_hhh = R405, age_hhh = R407, 
         edu_hhh = R613)

kor_b_2 <- kor_b_1 %>%
  filter(household.head == 1)%>%
  select(hh_id, edu_hhh, sex_hhh, age_hhh)

kor_b_3 <- kor_b_1 %>%
  select(hh_id, age_hhh)%>%
  mutate(adult = ifelse(age_hhh >= 18, 1, 0),
         child = ifelse(age_hhh < 18, 1, 0))%>%
  group_by(hh_id)%>%
  summarise(adults = sum(adult),
            children = sum(child))%>%
  ungroup()

kor_a_2 <- kor_a_2 %>%
  left_join(kor_b_2, by = "hh_id")%>%
  left_join(kor_b_3, by = "hh_id")%>%
  left_join(kor_a_3, by = "hh_id")%>%
  left_join(kor_a_4, by = "hh_id")

kor_a_x <- kor_a_2 %>%
  select(hh_id, hh_size, hh_weights, province, district, village, everything())%>%
  arrange(hh_id)%>%
  select(-urban)

write_csv(kor_a_x, "../0_Data/1_Household Data/1_Indonesia/1_Data_Clean/household_information_Indonesia.csv")

rm(kor_b_3, kor_b_1, kor_a_1, kor_a_3, appliances_01, kor_a_0, kor_b_0, kor_b_2)

# 264.230.759 people

# Information on consumption ####

# Weekly consumption in Rps (excluding self-production)

mod_41_a_1 <- mod_41_a_0 %>%
  select(URUT, KODE, B41K5, B41K6, B41K7, B41K8)%>%
  rename(hh_id = URUT, item_code = KODE, expenditures_weekly = B41K6, expenditures_sp_weekly = B41K8)%>%
  mutate(expenditures_year    = expenditures_weekly*52,
         expenditures_sp_year = expenditures_sp_weekly*52)%>%
  select(hh_id, item_code, expenditures_year, expenditures_sp_year)

mod_41_b_1 <- mod_41_b_0 %>%
  select(URUT, KODE, B41K5, B41K6, B41K7, B41K8)%>%
  rename(hh_id = URUT, item_code = KODE, expenditures_weekly = B41K6, expenditures_sp_weekly = B41K8)%>%
  mutate(expenditures_year    = expenditures_weekly*52,
         expenditures_sp_year = expenditures_sp_weekly*52)%>%
  select(hh_id, item_code, expenditures_year, expenditures_sp_year)

mod_42_1 <- mod_42_0 %>%
  select(URUT, KODE, B42K3, B42K4, B42K5)%>%
  rename(hh_id = URUT, item_code = KODE, expenditures_monthly = B42K4, expenditures_yearly = B42K5)%>%
  mutate(expenditures_year = expenditures_yearly + expenditures_monthly*12)%>%
  mutate(expenditures_sp_year = 0)%>%
  select(hh_id, item_code, expenditures_year, expenditures_sp_year)

# First: Aggregate Expenditure Levels ####

expenditures_0 <- bind_rows(mod_41_a_1, mod_41_b_1)%>%
  bind_rows(mod_42_1)%>%
  arrange(hh_id, item_code)%>%
  remove_all_labels()%>%
  filter(expenditures_year > 0 | expenditures_sp_year > 0)

write_csv(expenditures_0, "../0_Data/1_Household Data/1_Indonesia/1_Data_Clean/expenditures_items_Indonesia.csv")

# Deleting Subcategories
# Item_concordance <- read.xlsx("C:/users/misl/OwnCloud/Fuel_Elasticities_in_LMIC/Item_Categories_Concordance.xlsx", sheet = "Item_Indonesia", colNames = FALSE)
# 
# Item_concordance_1 <- Item_concordance %>%
#   filter(X1 == "deleted")%>%
#   pivot_longer(-X1, names_to = "drop", values_to = "item_code")%>%
#   select(-drop)%>%
#   filter(!is.na(item_code))
# 
# expenditures_1 <- expenditures_1 %>%
#   left_join(Item_concordance_1)%>%
#   filter(is.na(X1))%>%
#   group_by(hh_id)%>%
#   summarise(hh_expenditures_year = sum(expenditures_yearly),
#             hh_expenditures_sp_year = sum(expenditures_sp_yearly, na.rm = TRUE))%>%
#   ungroup()

# Expenditures and Quantities on Energy Items ####

# items_of_interest <- read.xlsx("C:/Users/misl/OwnCloud/Fuel_Elasticities_in_LMIC/Items_of_Interest.xlsx", colNames = FALSE)%>%
#   pivot_longer(-X1, names_to = "drop", values_to = "item_code")%>%
#   select(-drop)%>%
#   filter(!is.na(item_code))%>%
#   rename(Fuel = X1)
# 
# units_of_interest <- read.xlsx("C:/Users/misl/OwnCloud/Fuel_Elasticities_in_LMIC/Items_Units.xlsx")
# 
# expenditures_2 <- expenditures_0 %>%
#   select(hh_id, item_code, 
#          B42K3, expenditures_monthly, expenditures_year)%>%
#   filter(item_code >= 196 & item_code <= 224)%>%
#   left_join(items_of_interest)%>%
#   filter(!is.na(Fuel))%>%
#   group_by(hh_id)%>%
#   mutate(generator = ifelse(item_code == 200 & B42K3 == 1, "Gasoline", 
#                             ifelse(item_code == 200 & B42K3 == 2, "Diesel",
#                                    ifelse(item_code == 200 & B42K3 == 3, "Kerosene", NA))))%>%
#   ungroup()%>%
#   arrange(hh_id, item_code)%>%
#   select(-expenditures_year)%>%
#   group_by(hh_id, Fuel)%>%
#   summarise(Quantity = sum(B42K3),
#             expenditures_monthly = sum(expenditures_monthly),
#             generator = first(generator))%>%
#   ungroup()%>%
#   left_join(units_of_interest)%>%
#   mutate(implicit_price = ifelse((Quantity != 0),expenditures_monthly/Quantity, NA))
# 
# # Price-Data
# kor_a_x1 <- kor_a_x %>%
#   select(hh_id, hh_weights, province, district, kabupaten, urban_01)
# 
# expenditures_2.1 <- expenditures_2 %>%
#   select(hh_id, Fuel, generator, Unit, implicit_price)%>%
#   left_join(kor_a_x1)
# 
# write_csv(expenditures_2.1, "Implicit_price_data_Indonesia.csv")
# 
# # For Households 
# 
# expenditures_2.2 <- expenditures_2 %>%
#   select(hh_id, Fuel, expenditures_monthly, generator)%>%
#   unite("Fuel", c("Fuel", "generator"), na.rm = TRUE)%>%
#   mutate(expenditures_yearly = expenditures_monthly*12)%>%
#   select(-expenditures_monthly)%>%
#   pivot_wider(names_from = "Fuel", values_from = "expenditures_yearly")%>%
#   full_join(expenditures_1)%>%
#   arrange(hh_id)
# 
# write_csv(expenditures_2.2, "Expenditure_data_Indonesia.csv")
# 
# t <- expenditures_2.1 %>%
#   filter(Fuel == "Gasoline")
# 
# ggplot(filter(expenditures_2.1, Fuel == "Electricity"))+
#   geom_boxplot(aes(y = implicit_price, x = factor(province), group = factor(province)), outlier.shape = NA)+
#   coord_cartesian(ylim = c(0,5000))+
#   ggtitle("Electricity Price per kWh")
# 
# ggplot(filter(expenditures_2.1, Fuel == "Gasoline"))+
#   geom_boxplot(aes(y = implicit_price, x = factor(province), group = factor(province)), outlier.shape = NA)+
#   coord_cartesian(ylim = c(0,15000))+
#   ggtitle("Gasoline Price per l")
# 
# ggplot(filter(expenditures_2.1, Fuel == "Charcoal"))+
#   geom_boxplot(aes(y = implicit_price, x = factor(province), group = factor(province)), outlier.shape = NA)+
#   coord_cartesian(ylim = c(0,50000))+
#   ggtitle("Charcoal Price per kg")



# For the distributional analysis, we should use kor_a_2 and expenditures_0

rm(mod_41_a_0, mod_41_a_1, mod_41_b_0, mod_41_b_1, mod_42_1, mod_42_0)

# Codierungen - TBA (2021_01_20)####

Education.Code <- count(kor_a_x, edu_hhh)%>%
  select(-n)
Education.Code$Education <- c("Paket A","SDLB","SD","MI","Paket B","SMPLB","SMP","MTs","Paket C","SMLB","SMA","MA","SMK","MAK","D1/D2","D3","D4","S1","S2","S3",NA)

write_csv(Education.Code, "../0_Data/1_Household Data/1_Indonesia/2_Codes/Education.Code.csv")

Toilet.Code <- count(kor_a_x, toilet)%>%
  select(-n)
Toilet.Code$Toilet <- c("Used Alone", "Used with Household Members", "Public Toilet", "No Toilet", "No Facilities")
write_csv(Toilet.Code, "../0_Data/1_Household Data/1_Indonesia/2_Codes/Toilet.Code.csv")

Water.Code <- count(kor_a_x, water)%>%
  select(-n)
Water.Code$Water <- c("Branded bottled water","Refill water","Plumbing","Borehole / pump","Protected well","An unprotected well","Protected spring","Unprotected spring","Surface water (rivers, lakes, reservoirs, ponds, irrigation)","Rainwater","Others")
write_csv(Water.Code, "../0_Data/1_Household Data/1_Indonesia/2_Codes/Water.Code.csv")

Cooking.Code <- count(kor_a_x, cooking_fuel)%>%
  select(-n)
Cooking.Code$Cooking_Fuel <- c("Don't cook at home","Electricity","Elpiji 5.5 kg / blue gaz","LPG 12 kg","LPG 3 kg","City gas","biogas","Kerosene","Briquettes","Charcoal","Firewood","Others")
write_csv(Cooking.Code, "../0_Data/1_Household Data/1_Indonesia/2_Codes/Cooking.Code.csv")

Lighting.Code <- count(kor_a_x, lighting_fuel)%>%
  select(-n)
Lighting.Code$Lighting_Fuel <- c("PLN electricity with meter", "PLN electricity without meter", "Non PLN electricity","Not electricity" )
write_csv(Lighting.Code, "../0_Data/1_Household Data/1_Indonesia/2_Codes/Lighting.Code.csv")

Province.Code <- count(kor_a_0, R101)%>%
  select(-n)%>%
  rename(province = R101)%>%
  mutate(Province = c("Aceh","Sumatera Utara","Sumatera Barat","Riau","Jambi","Sumatera Selatan","Bengkulu","Lampung","Kepulauan Bangka Belitung","Kepulauan Riau","DKI Jakarta","Jawa Barat","Jawa Tengah","DI Yogyakarta","Jawa Timur","Banten","Bali","Nusa Tenggara Barat","Nusa Tenggara Timur","Kalimantan Barat","Kalimantan Tengah","Kalimantan Selatan","Kalimantan Timur","Kalimantan Utara","Sulawesi Utara","Sulawesi Tengah","Sulawesi Selatan","Sulawesi Tenggara","Gorontalo","Sulawesi Barat","Maluku","Maluku Utara","Papua Barat","Papua"))%>%
  write_csv(., "../0_Data/1_Household Data/1_Indonesia/2_Codes/Province.Code.csv")

District.Code <- count(kor_a_0, R102)%>%
  select(-n)%>%
  rename(district = R102)%>%
  mutate(District = district)%>%
  write_csv(., "../0_Data/1_Household Data/1_Indonesia/2_Codes/District.Code.csv")
