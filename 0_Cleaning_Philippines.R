if(!require("pacman")) install.packages("pacman")

p_load("countrycode", "haven", "Hmisc", "openxlsx", "rattle", "scales", "tidyverse", "readxl")

options(scipen=999)

# Load Data ####

total     <- read_csv("../0_Data/1_Household Data/1_Philippines/1_Data_Raw/totals.csv") 
household <- read_csv("../0_Data/1_Household Data/1_Philippines/1_Data_Raw/household.raw.csv")
summary   <- read_csv("../0_Data/1_Household Data/1_Philippines/1_Data_Raw/summary.hhold.csv") 
nonfood   <- read_csv("R:/MSA/datasets/Household Microdata/Philippines/Philippines_2015/FIES2015_vol2/FIES2015_vol2/NFOODEXP_1.csv")
food      <- read_csv("R:/MSA/datasets/Household Microdata/Philippines/Philippines_2015/FIES2015_vol2/FIES2015_vol2/FOODEXP_1.csv")

# Transform data ####

household_1 <- household %>%
  select(w_id, w_regn, hgc, members, ageless5, age5_17, toilet, electric, water)%>%
  rename(hh_id = w_id, province = w_regn, electricity.access = electric, edu_hhh = hgc, hh_size = members)%>%
  mutate(ageless5 = ifelse(is.na(ageless5),0,ageless5),
         age5_17   = ifelse(is.na(age5_17),0,age5_17))%>%
  mutate(children = ageless5 + age5_17)%>%
  mutate(children = ifelse(children > hh_size, hh_size, children))%>%
  select(-ageless5, -age5_17)%>%
  mutate(adults = hh_size - children)%>% # adults as imputed
  select(hh_id, hh_size, adults, children, everything())%>%
  mutate(electricity.access = ifelse(electricity.access == 1, 1, 0))

summary_1 <- summary %>% # what on earth is pop_adj?
  select(w_id, urb, rfact)%>%
  rename(hh_id = w_id, urban = urb, hh_weights = rfact)%>%
  mutate(urban_01 = ifelse(urban == 1,1,0))%>%
  select(-urban)
  
household_information <- left_join(household_1, summary_1)

write_csv(household_information, "../0_Data/1_Household Data/1_Philippines/1_Data_Clean/household_information_Philippines.csv")

# Expenditures

nonfood_1 <- nonfood %>%
  select(W_ID, starts_with("C"), TOTHERDISB:TODISBOTHER, -starts_with("G"), - starts_with("K"))%>%
  rename(otherdisb = TOTHERDISB, realprop = TODISBREALPROP, cashloan = TODISBCASHLOAN, appliance = TODISBAPPLIANCE, perstransport = TODISBTRANSPORT,
         loans.outside = TODISBLOANSOUTSIDE, deposits = TODISBDEPOSITS, repair = TODISBMAJREPAIR, construction = TODISBCONSTRUCTION, otherdisbu = TODISBOTHER)%>%
  select(- starts_with("T"))%>%
  rename_at(vars(starts_with("C")), funs(sub("C", "", .)))

colnames(nonfood_1) <- tolower(colnames(nonfood_1))

nonfood_11 <- nonfood_1 %>%
  mutate_at(vars(c(alcohol:otherdisbu)), function(x){x = as.numeric(x)})%>%
  rename(hh_id = w_id)

nonfood_names <- as.data.frame(colnames(nonfood_11))

colnames(nonfood_names) <- "item"
nonfood_names <- nonfood_names %>%
  filter(item != "hh_id")%>%
  mutate(no = 1000:1296)

colnames(nonfood_11) <- c("hh_id", seq(1000, 1296, by = 1))

food_1 <- food %>%
  select(W_ID, starts_with("C"))%>%
  rename_at(vars(starts_with("C")), funs(sub("C", "", .)))%>%
  mutate_at(vars(c(CEREAL:CANTEENMILITARY)), function(x){x = as.numeric(x)})%>%
  rename(hh_id = W_ID)

colnames(food_1) <- tolower(colnames(food_1))

food_names <- as.data.frame(colnames(food_1))

colnames(food_names) <- "item"
food_names <- food_names %>%
  filter(item != "hh_id")%>%
  mutate(no = 1:n())

colnames(food_1) <- c("hh_id", seq(1, 273, by = 1))

expenditures <- left_join(food_1, nonfood_11, by = "hh_id")%>%
  pivot_longer(-hh_id, names_to = "item_code", values_to = "expenditures_year")%>%
  filter(expenditures_year >0 & !is.na(expenditures_year))

write_csv(expenditures, "../0_Data/1_Household Data/1_Philippines/1_Data_Clean/expenditures_items_Philippines.csv")

# Appliances 

appliances <- household %>%
  select(w_id, radio_qty, tv_qty, cd_qty, ref_qty, wash_qty, aircon_qty, car_qty, cellphone_qty, pc_qty, oven_qty, motorcycle_qty)%>%
  rename(hh_id           = w_id, 
         radio.01        = radio_qty,
         tv.01           = tv_qty,
         video.01        = cd_qty,
         refrigerator.01 = ref_qty,
         washing_machine.01 = wash_qty,
         ac.01         = aircon_qty,
         mobile.01     = cellphone_qty,
         car.01        = car_qty,
         computer.01   = pc_qty,
         oven.01       = oven_qty,
         motorcycle.01 = motorcycle_qty)%>%
  mutate_at(vars(-hh_id), list(~ ifelse(. > 0 &!is.na(.),1,0)))

write_csv(appliances, "../0_Data/1_Household Data/1_Philippines/1_Data_Clean/appliances_0_1_Philippines.csv")

# Codes

Toilet.Code <- data.frame("toilet" = c(seq(0,7, by = 1)), 
                          "Toilet" = c("None", "Water-sealed, sewer septic tank, used exclusively", "Water-sealed, sewer septic tank, shared use", "Water-sealed, other depository, used exclusively", "Water-sealed, other depository, shared use", "closed pit", "open pit", "others"))%>%
  write_csv(., "../0_Data/1_Household Data/1_Philippines/2_Codes/Toilet.Code.csv")
Water.Code <- data.frame("water" = c(seq(1,11, by = 1)), 
                         "Water" = c("Own use, faucet, community water system", "Shared, faucet, community water system", "Own use, tubed/piped deep well", "Shared, tubed/piped deep well", "tubed/piped shallow well", "dug well", "protected spting, river, stream", "unprotected spring, river, stream", "lake, river, rain and others", "Peddler", "Others"))%>%
  write_csv(., "../0_Data/1_Household Data/1_Philippines/2_Codes/Water.Code.csv")
Education_Code <- read_excel("../0_Data/1_Household Data/1_Philippines/9_Documentation/Education.Code.xlsx", 
                             col_names = c("Education", "edu_hhh"), col_types = c("text", "numeric"))%>%
  select(edu_hhh, Education)%>%
  write_csv(., "../0_Data/1_Household Data/1_Philippines/2_Codes/Education.Code.csv")
Province.Code <- distinct(household_information, province)%>%
  arrange(province)%>%
  mutate(Province = c("Ilocos Region", "Cagayan Valley", "Central Luzon", "Bicol Region", "Western Visayas",
                      "Central Visayas", "Eastern Visayas", "Zasmboanga Peninsula", "Nothern Mindanao",
                      "Davao Region", "Soccsksargen", "NCR","CAR", "ARMM", "Caraga", "Calabarzon", "Mimaropa"))%>%
  write_csv(., "../0_Data/1_Household Data/1_Philippines/2_Codes/Province.Code.csv")
