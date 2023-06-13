# Author: L. Missbach (missbach@mcc-berlin.net)

if(!require("pacman")) install.packages("pacman")

p_load("countrycode", "haven", "Hmisc", "openxlsx", "rattle", "scales", "tidyverse", "sjlabelled")

options(scipen=999)

# Load Data ####

data_0 <- read_table("T:/MSA/papers_internal/work_in_progress/Mi_Homogenized_Datainfrastructure/0_Data/1_Household Data/4_Switzerland/1_Data_Raw/HABE151617_220426UOe/HABE151617_Standard_220426UOe.txt")
data_1 <- read_table("T:/MSA/papers_internal/work_in_progress/Mi_Homogenized_Datainfrastructure/0_Data/1_Household Data/4_Switzerland/1_Data_Raw/HABE151617_220426UOe/HABE151617_Personen_220426UOe.txt")
data_2 <- read_table("T:/MSA/papers_internal/work_in_progress/Mi_Homogenized_Datainfrastructure/0_Data/1_Household Data/4_Switzerland/1_Data_Raw/HABE151617_220426UOe/HABE151617_Ausgaben_220426UOe.txt")
# data_3 <- read_table("T:/MSA/papers_internal/work_in_progress/Mi_Homogenized_Datainfrastructure/0_Data/1_Household Data/4_Switzerland/1_Data_Raw/HABE151617_220426UOe/HABE151617_Mengen_220426UOe.txt")
data_4 <- read_table("T:/MSA/papers_internal/work_in_progress/Mi_Homogenized_Datainfrastructure/0_Data/1_Household Data/4_Switzerland/1_Data_Raw/HABE151617_220426UOe/HABE151617_Konsumgueter_220426UOe.txt")

# Transform data #####

data_0.1 <- data_0 %>%
  rename(hh_id = HaushaltID, hh_weights = Gewicht20_151617, hh_size = AnzahlPersonen98,
         children = AnzahlKinder05, sex_hhh = FrauAlsReferenzperson05,
         province = Grossregion01, district = Kanton08)%>%
  mutate(inc_gov_monetary = E21 + E22,
         inc_gov_cash     = E23,
         adults           = hh_size - children)%>%
  select(hh_id, hh_weights, hh_size, adults, children, sex_hhh, province, district, inc_gov_cash, inc_gov_monetary)

data_1.1 <- data_1 %>%
  rename(hh_id = HAUSHALTID, age_hhh = Lebensalter98, nationality = Nationalitaet01)%>%
  filter(StellungImHaushalt11 == 1)%>%
  select(hh_id, age_hhh, nationality)

household_information <- data_0.1 %>%
  left_join(data_1.1)

write_csv(household_information, "../0_Data/1_Household Data/4_Switzerland/1_Data_Clean/household_information_Switzerland.csv")

data_4.1 <- data_4 %>%
  rename(hh_id = HAUSHALTID)%>%
  mutate(car.01             = ifelse(AnzahlNeuwagen11 > 0 | AnzahlGebrauchtwagen11 > 0,1,0),
         motorcycle.01      = ifelse(AnzahlMotorraeder11 > 0,1,0),
         refrigerator.01    = ifelse(AnzahlTiefkuehler11 > 0,1,0),
         dishwasher.01      = ifelse(AnzahlGeschirrspueler11 > 0,1,0),
         washing_machine.01 = ifelse(AnzahlWaschmaschinen11 > 0,1,0),
         dryer.01           = ifelse(AnzahlWaeschetrockner11 > 0,1,0),
         tv.01              = ifelse(AnzahlRoehrenfernseher11 > 0 | AnzahlLCDFernseher11 > 0,1,0),
         computer.01        = ifelse(AnzahlDesktopComputer11 > 0 | AnzahlLaptopComputer11 > 0,1,0))%>%
  select(hh_id, ends_with(".01"))

write_csv(data_4.1, "../0_Data/1_Household Data/4_Switzerland/1_Data_Clean/appliances_0_1_Switzerland.csv")

data_2.1 <- data_2 %>%
  rename(hh_id = HaushaltID)%>%
  pivot_longer(-hh_id, names_to = "item_code", values_to = "expenditures_year")%>%
  filter(expenditures_year != 0)%>%
  mutate(expenditures_year = expenditures_year*12)

write_csv(data_2.1, "../0_Data/1_Household Data/4_Switzerland/1_Data_Clean/expenditures_items_Switzerland.csv")

# Codes

Gender.Code <- distinct(data_0, FrauAlsReferenzperson05)%>%
  rename(sex_hhh = FrauAlsReferenzperson05)%>%
  arrange(sex_hhh)%>%
  mutate(Gender = c("Male", "Female"))%>%
  write_csv(., "../0_Data/1_Household Data/4_Switzerland/2_Codes/Gender.Code.csv")

Province.Code <- distinct(data_0, Grossregion01)%>%
  rename(province = Grossregion01)%>%
  arrange(province)%>%
  mutate(Province = c("Genferseeregion", "Espace Mittelland", "Nordwestschweiz", "Zürich", "Ostschweiz", "Zentralschweiz", "Tessin"))%>%
  write_csv(., "../0_Data/1_Household Data/4_Switzerland/2_Codes/Province.Code.csv")

District.Code <- distinct(data_0, Kanton08)%>%
  rename(district = Kanton08)%>%
  arrange(district)%>%
  mutate(District = c("Zürich", "Bern", "Luzern", "St. Gallen", "Aargau", "Tessin", "Waadt", "Genf", "Other"))%>%
  write_csv(., "../0_Data/1_Household Data/4_Switzerland/2_Codes/District.Code.csv")

Nationality.Code <- distinct(data_1, Nationalitaet01)%>%
  rename(nationality = Nationalitaet01)%>%
  arrange(nationality)%>%
  mutate(Nationality = c("Swiss", "Non-Swiss"))%>%
  write_csv(., "../0_Data/1_Household Data/4_Switzerland/2_Codes/Nationality.Code.csv")

Item.Codes.Data <- distinct(data_2.1, item_code)%>%
  arrange(item_code)

Item.Codes.Description <- read_excel("T:/MSA/papers_internal/work_in_progress/Mi_Homogenized_Datainfrastructure/0_Data/1_Household Data/4_Switzerland/1_Data_Raw/HABE151617_220426UOe/HABE151617_Datenbeschreibung_220426UOe.xlsx", 
                                     sheet = "Ausgaben", skip = 12)

colnames(Item.Codes.Description) <- c("Name_A", "Name_B", "Name_C", "Name_D", "Name_E", "drop", "item_code", "drop_b", "drop_c", "drop_d")

Item.Codes.Description.1 <- Item.Codes.Description %>%
  select(-drop, -drop_b, -drop_c, -drop_d)%>%
  filter(!is.na(item_code))%>%
  mutate(item_name = ifelse(!is.na(Name_E), Name_E, 
                            ifelse(!is.na(Name_D), Name_D,
                                   ifelse(!is.na(Name_C), Name_C,
                                          ifelse(!is.na(Name_B), Name_B, Name_A)))))%>%
  mutate(category = ifelse(!is.na(Name_E), "Yes", 
                            ifelse(!is.na(Name_D), "No",
                                   ifelse(!is.na(Name_C), "No",
                                          ifelse(!is.na(Name_B), "No", "No")))))%>%
  select(item_code, item_name, category)

Item.Codes.Description.2 <- Item.Codes.Data %>%
  left_join(Item.Codes.Description.1)

write.xlsx(Item.Codes.Description.2, "../0_Data/1_Household Data/4_Switzerland/3_Matching_Tables/Item_Codes_Description_Switzerland.xlsx")
