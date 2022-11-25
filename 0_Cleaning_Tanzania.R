# Packages ####

library("haven")
library("Hmisc")
library("openxlsx")
library("rattle")
library("scales")
library("sjlabelled")
library("tidyverse")
options(scipen=999)

# Load Data ####

Individual_0 <- read_dta("./2_Tanzania/1_Data_Raw/HBS_2017-18_Final_Individual_Data/HBS 2017-18 _Final_Individual_Data.dta")
Form_2_0     <- read_dta("./2_Tanzania/1_Data_Raw/HBS_2017-18_Form_II_Data/HBS__Form_2.dta")

# load other from form 1
files <- list.files("./2_Tanzania/1_Data_Raw/Other_Sections_of_Form_I", full.names = TRUE)
form1 <- read_dta(files[1])
for (file in c(2:length(files))) {
  tmp<- read_dta(files[file])
  form1 <- merge(form1, tmp, all.x=TRUE)
}

# load other from form 2
files <- list.files("./2_Tanzania/1_Data_Raw/Other_Sections_of_Form_II", full.names = TRUE)
form2 <- read_dta(files[1])
for (file in c(2:length(files))) {
  tmp<- read_dta(files[file])
  form2 <- merge(form1, tmp, all.x=TRUE)
}

# load other from form 3
files <- list.files("./2_Tanzania/1_Data_Raw/Other_Sections_of_Form_III", full.names = TRUE)
form3 <- read_dta(files[1])
for (file in c(2:length(files))) {
  tmp<- read_dta(files[file])
  form2 <- merge(form3, tmp, all.x=TRUE)
}

#remove two faulty interviews
data <-Individual_0 %>%
  filter(interview__id != "9dba3e778ea04ccc92d404032d3aff13", interview__id !="a54f1fc3a2cf4a88b114a03ea1cdc0a5")

#count adults and children
counts <- count(Individual_0, HHID)
counts <- merge(counts, count(filter(Individual_0, S1_4<16), HHID, name = "children"), by ="HHID", all.x=TRUE)
counts <- merge(counts, count(filter(Individual_0, S1_4>=16), HHID, name = "adults"), by ="HHID", all.x=TRUE)

data<-data%>%
  merge(., counts, by = "HHID")%>%
  mutate(children = ifelse(is.na(children), ifelse(is.na(adults), NA, 0), children))%>%
  mutate(adults = ifelse(is.na(adults), ifelse(is.na(children), NA, 0), adults))

#extract vars of interest for household information
hh_information_all <- data %>%
  select(interview__id, S1_1, hh_id = HHID, hh_size = n,  hh_weights = weight,  
         age_hhh=S1_4, sex_hhh = S1_2, edu_hhh_ever_01 = S5_1, edu_hhh = S5_9, 
         province = REGION, urban_01 = LOC) %>% # employment_hhh = S9_1 in questions but not in dataset
  mutate(urban_01 = ifelse(is.na(urban_01), 2, ifelse(urban_01 == 2, 0, 1)),
         edu_hhh_ever_01 = ifelse(is.na(edu_hhh_ever_01), 2, ifelse(edu_hhh_ever_01 == 2, 0, 1)))%>%
  merge(., select(Form_2_0, S12_09, interview__id), by = "interview__id")%>%
  rename(toilet_condition = S12_09)

household_information_Tanzania<- hh_information_all %>%
  filter(S1_1 == 1)%>%
  select(!S1_1)%>%
  write_csv(., "./2_Tanzania/1_Data_Clean/household_information_Tanzania.csv")

#households without declared head of household (count = 3)
#missing<-hh_information_all$hh_id[!hh_information_all$hh_id %in% tmp$hh_id] 

#write codes
Toilet_Condition.Code <- stack(attr(Form_2_0$S12_09, 'labels'))%>%
  rename(toilet_condition = values, Toilet_Condition = ind)%>%
  write_csv(., "2_Tanzania/2_Codes/Toilet_Condition.Code.csv")
Province.Code <- stack(attr(data$REGION, 'labels'))%>%
  rename(province = values, Province = ind)%>%
  write_csv(., "2_Tanzania/2_Codes/Province.Code.csv")
Gender.Code <- stack(attr(data$S1_2, 'labels'))%>%
  rename(sex_hhh = values, Gender = ind)%>%
  write_csv(., "2_Tanzania/2_Codes/Gender.Code.csv")
Education.Code <- stack(attr(data$S5_9, 'labels'))%>%
  rename(edu_hhh = values, Education = ind)%>%
  write_csv(., "2_Tanzania/2_Codes/Employment.Code.csv")
Edu_hhh_ever_01.Code <- stack(attr(data$S5_1, 'labels'))%>%
  rename(edu_hhh_ever_01 = values, Had_Education_Ever = ind)%>%
  write_csv(., "2_Tanzania/2_Codes/Employment.Code.csv")
Urban01.Code <- data.frame(urban_01 = c(0,1), Urban_Rural = c("Rural", "Urban"))%>%
  write_csv(., "2_Tanzania/2_Codes/Urban01.Code.csv")

#appliances 
appliances_0_1_Tanzania <- Form_2_0 %>%
  select(interview__id, energy_supply = S12_06, toilet = S12_09, running_water_rainy = S12_15, running_water_dry = S12_16, 
         el_stove = S13_01__531301, char_stove = S13_01__531302, firew_stove =S13_01__531303, fridge =S13_01__531101,
         water_heater = S13_01__531402, aircon = S13_01__531401, washing_machine =S13_01__531202, generator = S13_01__531701,
         water_pump = S13_01__551101,
         
         diesel_car =S17_01__711101, pet_car = S17_01__711102, motorcycle = S17_01__712101, scooter = S17_01__712102, 
         bajaji = S17_01__712103, trailer = S17_01__921101, bicycle = S17_01__713101, boat =S17_01__734101, 
         landline = S18_00__821101, mobile_phone = S18_00__821102, pc = S18_00__913101) %>%#idiesel_car =S8B_6__1, ipet_car = S8B_6__2, imotorcycle = S8B_6__3, iscooter = S8B_6__4, ibajaji = S8B_6__5, itrailer = S8B_6__6, ibicycle = S8B_6__7, iboat =S8B_6__8, ilandline = S8B_6__9, imobile_phone = S8B_6__10, ipc = S8B_6__11, 
         #starts_with("S18_00"))%>%
  mutate(toilet = ifelse(toilet < 2, 0, ifelse(is.na(toilet), NA, 1)),
         running_water_rainy = ifelse(running_water_rainy <3, 1, ifelse(is.na(running_water_rainy), NA, 0)), 
         running_water_dry = ifelse(running_water_dry <3, 1, ifelse(is.na(running_water_dry), NA, 0)), 
         energy_supply = ifelse(energy_supply == 2, 0, ifelse(is.na(energy_supply), NA, 1)))%>%
  merge(., select(data, HHID, interview__id), by = "interview__id")%>%
  distinct(.)%>%
  select(hh_id = HHID, !interview__id)%>%
  write_csv(., "2_Tanzania/1_Data_Clean/appliances_0_1_Tanzania.csv")

#read the other dataset in search of the expenditure data
consumptionsdd<- read_dta("./2_Tanzania/1_Data_Raw/TZA_2019_NPS-SDD_v03_M_Stata12/consumptionsdd.dta")
          