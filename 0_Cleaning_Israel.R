# Author: L.Missbach
# missbach@mcc-berlin.net

# 0.1   Packages ####

if(!require("pacman")) install.packages("pacman")

p_load("haven", "Hmisc", "openxlsx", "rattle", "scales", "tidyverse", "readxl")


# 0.2   Working Directory ####

# 0.3   Data Loading ####

mb           <- read_csv("H:/5_Israel_08_2020/Data/Household Survey/Israel_Household_data_Update/H20181023/H20181023datamb.csv")          # Household information
prat         <- read_csv("H:/5_Israel_08_2020/Data/Household Survey/Israel_Household_data_Update/H20181023/H20181023dataprat.csv")        # Person information
prod         <- read_csv("H:/5_Israel_08_2020/Data/Household Survey/Israel_Household_data_Update/H20181023/H20181023dataprod.csv")        # expenditures long format
veg          <- read_csv("H:/5_Israel_08_2020/Data/Household Survey/Israel_Household_data_Update/H20181023/H20181023dataveg.csv")         # expenditures
cloth_health <- read_csv("H:/5_Israel_08_2020/Data/Household Survey/Israel_Household_data_Update/H20181023/H20181023dataClothHealth.csv") # expenditures
educ_mother  <- read_csv("H:/5_Israel_08_2020/Data/Household Survey/Israel_Household_data_Update/H20181023/H20181023dataeducomother.csv") # expenditures
food         <- read_csv("H:/5_Israel_08_2020/Data/Household Survey/Israel_Household_data_Update/H20181023/H20181023datafood.csv")        # expenditures
house        <- read_csv("H:/5_Israel_08_2020/Data/Household Survey/Israel_Household_data_Update/H20181023/H20181023datahouse.csv")       # expenditures
incmissim    <- read_csv("H:/5_Israel_08_2020/Data/Household Survey/Israel_Household_data_Update/H20181023/H20181023dataincmissim.csv")   # income
yoman        <- read_csv("H:/5_Israel_08_2020/Data/Household Survey/Israel_Household_data_Update/H20181023/H20181023datayoman.csv")       # yoman

# 1   DATA TRANSFORMATION ####

# 1.1 Household Information ####

mb_1 <- mb %>%
  rename(hh_id = misparmb, hh_weights = weight, hh_size = nefashot, 
         age_u_18 = nefashotad18, age_18 = nefashot18up, 
         ethnicity = Nationality, 
         district = yshuv, province = zurat_y, 
         rating_residence = cluster, rating_peripheral = madadpereferia, rating_potentiality = madadpotentziali)%>%
  rename(religiosity = RamatDatiyut)%>%
  select(hh_id, hh_weights, hh_size, 
         age_u_18, age_18, 
         ethnicity, district, province,  
         religion, religiosity)%>%
  mutate(children = ifelse(!is.na(age_u_18), age_u_18, 0),
         adults   = ifelse(!is.na(age_18),   age_18, 0))%>%
  select(-age_u_18, -age_18)

# 1.2 Persons Information ####

prat_1 <- prat %>%
  rename(hh_id = misparMb, person = prat, household_head = y_kalkali, 
         sex_hhh = min, 
         edu_hhh = sug_teuda, ind_hhh = anaf1, age_hhh = age_group)%>% # additional information on income, type of work, type of occupation
  select(hh_id, person, household_head, sex_hhh, 
         edu_hhh, ind_hhh)%>%
  filter(household_head == 1)%>%
  select(-person, -household_head)



Education.Code <- data.frame(edu_hhh   = c(seq(1,9,1),99),
                             Education = c("Elementary School / Junior High", "High School Diploma", "High School Diploma", "High School Graduation", "Bachelor", "Master", "PhD", "Another Certificate", "Unknown", "Unknown"))%>%
  write_csv(., "../0_Data/1_Household Data/1_Israel/2_Codes/Education.Code.csv")
Industry.Code <- data.frame(ind_hhh  = c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","X"),
                            Industry = c("Agriculture, Forestry, Fishing", "Mining", "Industry", "Energy", "Water, Waste", "Construction", "Wholesale, Retail, Trade", "Transportation", "Accomodation", "Information", "Financial Services", "Real Estate", "Science", "Management", "Public Administration", "Education", "Health", "Art, Entertainment", "Other Services", "Household Employment", "NGO", "Unknown"))%>%
  write_csv(., "../0_Data/1_Household Data/1_Israel/2_Codes/Industry.Code.csv")
Gender.Code <-  data.frame(sex_hhh = c(1,2),
                           Gender  = c("Male", "Female"))%>%
  write_csv(., "../0_Data/1_Household Data/1_Israel/2_Codes/Gender.Code.csv")
Ethnicity.Code <- distinct(mb, Nationality)%>%
  arrange(Nationality)%>%
  rename(ethnicity = Nationality)%>%
  mutate(Ethnicity = c("Jewish", "Arabic", "Other"))%>%
  write_csv(., "../0_Data/1_Household Data/1_Israel/2_Codes/Ethnicity.Code.csv")
Religion.Code <- distinct(mb, religion)%>%
  arrange(religion)%>%
  mutate(Religion = c("Jewish", "Christian", "Moslem", "Druze", "Other"))%>%
  write_csv(., "../0_Data/1_Household Data/1_Israel/2_Codes/Religion.Code.csv")
Religiosity.Code <- distinct(mb, RamatDatiyut)%>%
  arrange(RamatDatiyut)%>%
  rename(religiosity = RamatDatiyut)%>%
  mutate(Religiosity = c("Secular", "Traditional", "Religious", "Orthodox", "Mixed Lifestyle", "Other"))%>%
  write_csv(., "../0_Data/1_Household Data/1_Israel/2_Codes/Religiosity.Code.csv")
Province.Code <- data.frame(province = c(0,1,2,3,4,5,6,7,8,9,10,12,13,14,15), 
                            Province = c("Rishon Lezion", "Jerusalem", "Tel Aviv", "Haifa", "Cities (100k to 200k)", "Cities (50k to 100k)", "Cities (20k to 50k)", "Cities (10k to 20k)", "Cities (2k to 10k)","Urban", "Rural", "Ashdod", "Petah Tikva", "Netanya", "Beer Sheva"), 
                            urban_01 = c(1,1,1,1,1,1,1,1,0,1,0,1,1,1,1))
Province.Code.1 <- Province.Code %>%
  select(province, Province)%>%
  write_csv(., "../0_Data/1_Household Data/1_Israel/2_Codes/Province.Code.csv")

District.Code <- read_excel("R:/MSA/datasets/Household Microdata/Israel/Data_2017_2018/H20181021/Yshv_code.xls", sheet = "Tabelle1")%>%
  rename(district = region, District = Region)%>%
  arrange(district)%>%
  write_csv(., "../0_Data/1_Household Data/1_Israel/2_Codes/District.Code.csv")

# 1.3 Income ####

prod_0 <- prod %>%
  rename(hh_id = misparMb, income = schum)%>%
  mutate(income_year = income*12)%>%
  filter(prodcode %in% c(131011,131029,
                         141010,141028,141036,141044,141051,141069,141077,141085,141093,141101,
                         142018,142026,142034,142042,142059))%>%
  mutate(type = ifelse(prodcode %in% c(131029,142034,142042), "inc_gov_cash", "inc_gov_monetary"))%>%
  group_by(hh_id, type)%>%
  summarise(income_year = sum(income_year))%>%
  ungroup()%>%
  pivot_wider(names_from = "type", values_from = "income_year", values_fill = 0)

household_information <- mb_1 %>%
  left_join(prat_1)%>%
  left_join(prod_0)%>%
  mutate(inc_gov_monetary = ifelse(is.na(inc_gov_monetary),0, inc_gov_monetary),
         inc_gov_cash     = ifelse(is.na(inc_gov_cash),0,inc_gov_cash))%>%
  left_join(Province.Code)%>%
  select(-Province)

write_csv(household_information, "../0_Data/1_Household Data/1_Israel/1_Data_Clean/household_information_Israel.csv")

# Appliances

app_0 <- mb %>%
  rename(hh_id = misparmb, stove_cooking_baking = tanur_ba, stove.01 = tanur_b, stove_baking = tanur_a, microwave.01 = microwave, refrigerator.01 = refrigerator, freezer.01 = deep_freezer,
         dishwasher.01 = dish_washer, washing_machine.01 = washing_m, dryer.01 = dryer, vacuum.01 = v_cleaner, tv.01 = television)%>%
  rename(ac.01 = a_conditioner, central_heating = c_heating, apartment_heating = f_heating, boiler.01 = water_heating,
         security_room = security_room, motorcycle.01 = Electric_Scooter, bicycle.01 = Electric_Bicycle, car.01 = cars)%>%
  select(hh_id, ends_with(".01"))

appliances_no_Israel  <- app_0 %>%
  mutate_at(vars(-hh_id), .funs = list(~ ifelse(. > 0,1,0)))%>%
  mutate(car.01 = ifelse(is.na(car.01),0,car.01))

write_csv(appliances_no_Israel, "../0_Data/1_Household Data/1_Israel/1_Data_Clean/appliances_0_1_Israel.csv")

# 1.4 Expenditures ####

# exp_total <- left_join(veg, educ_mother, by = "MisparMB")%>%
#  left_join(food, by = "MisparMB")%>%
#  left_join(house, by = "MisparMB")

prod_1 <- prod %>%
  rename(hh_id = misparMb, item_code = prodcode, expenditures = schum)%>%# monthly expenditures
  mutate(expenditures_year = expenditures*12)%>%
  select(hh_id, item_code, expenditures_year)

write_csv(prod_1, "../0_Data/1_Household Data/1_Israel/1_Data_Clean/expenditures_items_Israel.csv")

# 1.5  Item Codes ####

item_codes <- read.xlsx("R:/MSA/datasets/Household Microdata/Israel/Data_2017_2018/H20181021/item_codes.xlsx", sheetName = "item_codes")%>%
  filter(!is.na(item_name))

item_codes$item_code <- str_remove(item_codes$item_code, "i")
item_codes$item_code <- str_remove(item_codes$item_code, "s")
item_codes$item_code <- str_remove(item_codes$item_code, "c")
item_codes$item_code <- str_remove(item_codes$item_code, "t")

item_codes <- item_codes %>%
  mutate(item_code = as.numeric(item_code))

write.xlsx(item_codes, "R:/MSA/datasets/Household Microdata/Israel/Data_2017_2018/H20181021/item_codes.xlsx", sheetName = "item_codes_clean", append = TRUE)

