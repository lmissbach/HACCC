if(!require("pacman")) install.packages("pacman")

p_load("countrycode", "haven", "Hmisc", "openxlsx", "rattle", "scales", "tidyverse")

options(scipen=999)

# Author: L. Missbach

# Load data ####

data_0 <- read_sas("T:/MSA/papers_internal/work_in_progress/Mi_Homogenized_Datainfrastructure/0_Data/1_Household Data/4_Austria/1_Data_Raw/KE14_File_detailliert/extk14_ausg_det.sas7bdat", NULL)
data_1 <- read_sas("T:/MSA/papers_internal/work_in_progress/Mi_Homogenized_Datainfrastructure/0_Data/1_Household Data/4_Austria/1_Data_Raw/KE14_File_detailliert/extk14_hhvar.sas7bdat", NULL)
data_2 <- read_sas("T:/MSA/papers_internal/work_in_progress/Mi_Homogenized_Datainfrastructure/0_Data/1_Household Data/4_Austria/1_Data_Raw/KE14_File_detailliert/extk14_persvar.sas7bdat", NULL)

# Transform data

# Household data

data_1.1 <- data_1 %>%
  rename(hh_id = OB, hh_weights = HGEW, province = NUTS1, district = nuts2,
         heating_fuel = ENERGIE)%>%
  mutate(urban_01 = ifelse(urb_det %in% c(101,102,103),1,0))%>%
  select(hh_id, hh_weights, province, district, heating_fuel)

data_2.1 <- data_2 %>%
  rename(hh_id = OB, sex_hhh = geschl, age_hhh = alt, nationality = STAAT, edu_hhh = BILD)%>%
  filter(STELLHV == 1)%>%
  select(hh_id, sex_hhh, age_hhh, nationality, edu_hhh)

data_2.2 <- data_2 %>%
  rename(hh_id = OB)%>%
  mutate(adults   = ifelse(alt > 15,1,0),
         children = ifelse(alt < 16,1,0))%>%
  group_by(hh_id)%>%
  summarise(hh_size  = n(),
            adults   = sum(adults),
            children = sum(children))%>%
  ungroup()

household_information <- left_join(data_1.1, data_2.2)%>%
  left_join(data_2.1)

write_csv(household_information, "../0_Data/1_Household Data/4_Austria/1_Data_Clean/household_information_Austria.csv")

# Appliances

data_1.2 <- data_1 %>%
  rename(hh_id = OB, ac.01 = KLIMA, car.01 = PKW, motorcycle.01 = MOTO, refrigerator.01 = KUEHL,
         dishwasher.01 = SPUEL, washing_machine.01 = WASCH, tv.01 = TV, computer.01 = PC)%>%
  select(hh_id, ends_with(".01"))%>%
  mutate_at(vars(ends_with(".01")), list(~ ifelse(. == 1,1,0)))

write_csv(data_1.2, "../0_Data/1_Household Data/4_Austria/1_Data_Clean/appliances_0_1_Austria.csv")

# Expenditures

data_0.1 <- data_0 %>%
  rename(hh_id = OB)%>%
  filter(COI_UNTEBENE == 1)%>%
  filter(online == "00")%>%
  mutate(expenditures_year = ausgHH*12)%>%
  mutate(item_code_COICOP = paste0(c1,c2,c3,c4,c5,c6))

item_codes <- data_0.1 %>%
  select(item_code_COICOP, COICOP_TEXT)%>%
  distinct()%>%
  arrange(item_code_COICOP)%>%
  mutate(item_code = 1:n())%>%
  rename(item_name = COICOP_TEXT)%>%
  select(item_code, everything())

write.xlsx(item_codes, "../0_Data/1_Household Data/4_Austria/3_Matching_Tables/Item_Codes_Description_Austria.xlsx")

data_0.2 <- data_0.1 %>%
  left_join(item_codes)%>%
  select(hh_id, item_code, expenditures_year)

write_csv(data_0.2, "../0_Data/1_Household Data/4_Austria/1_Data_Clean/expenditures_items_Austria.csv")

# Codes

Province.Code <- data.frame("province" = c(1,2,3),
                            "Province" = c("Ostösterreich", "Südösterreich", "Westösterreich"))%>%
  write_csv(., "../0_Data/1_Household Data/4_Austria/2_Codes/Province.Code.csv")
District.Code <- data.frame("district" = c(seq(1,9,1)),
                            "District" = c("Burgenland", "Kärnten", "Niederösterreich", "Oberösterreich", "Salzburg",
                                           "Steiermark", "Tirol", "Vorarlberg", "Wien"))%>%
  write_csv(., "../0_Data/1_Household Data/4_Austria/2_Codes/District.Code.csv")
Heating.Code <- data.frame("heating_fuel" = c(-3,-1, seq(1,7,1)),
                           "Heating_Fuel" = c("trifft nicht zu", "keine Angabe", "Strom", "Gas", "Heizöl",
                                              "Holzpellets", "Brennholz, Hackschnitzel, Holzbriketts", "Kohle, Koks, Kohlenbriketts",
                                              "Alternative Energieträger wie Solarwärme, Photovoltaik, Wärmepumpe"))%>%
  write_csv(., "../0_Data/1_Household Data/4_Austria/2_Codes/Heating.Code.csv")
Gender.Code <- data.frame("sex_hhh" = c(1,2),
                          "Gender" = c("male", "female"))%>%
  write_csv(., "../0_Data/1_Household Data/4_Austria/2_Codes/Gender.Code.csv")
Nationality.Code <- data.frame("nationality" = c(-1, seq(1,11,1)),
                               "Nationality" = c("keine Angabe", "Österreich", "Deutschland", "Kroatien", "Polen", "Rumänien",
                                                 "Andere EU_Staaten", "EFTA-Staaten", "Bosnien und Herzegowina", "Serbien", "Türkei",
                                                 "Andere Nicht-EU-Staaten"))%>%
  write_csv(., "../0_Data/1_Household Data/4_Austria/2_Codes/Nationality.Code.csv")
Education.Code <- data.frame("edu_hhh" = c(-3, seq(1,7,1)),
                             "Education" = c("trifft nicht zu", "Pflichtschule nicht abgeschlossen", "Pflichtschule abgeschlossen",
                                             "Lehre mit Berufsschule", "Fach- oder Handelsschule", "Matura", "Abschluss an einer Universität, (Fach-)Hochschule", "anderer Abschluss nach der Matura"),
                             "ISCED" = c(9,1,2,4,4,3,6,6))%>%
  write_csv(., "../0_Data/1_Household Data/4_Austria/2_Codes/Education.Code.csv")
