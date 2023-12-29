if(!require("pacman")) install.packages("pacman")

p_load("countrycode", "haven", "Hmisc", "openxlsx", "rattle", "scales", "tidyverse", "readr")

options(scipen=999)

# Author: L. Missbach

# Load data ####

# Basic CURF / Survey of Household Expenditures

# data_B <- read_csv("../0_Data/1_Household Data/1_Australia/1_Data_Raw/HES15B.csv", col_names = FALSE) # all levels data
data_H_H <- read_csv("../0_Data/1_Household Data/1_Australia/1_Data_Raw/HES15BH.csv") # Household data
data_H_I <- read_csv("../0_Data/1_Household Data/1_Australia/1_Data_Raw/HES15BI.csv") # income unit data
data_H_L <- read_csv("../0_Data/1_Household Data/1_Australia/1_Data_Raw/HES15BL.csv") # Loans data

data_H_P <- read_csv("../0_Data/1_Household Data/1_Australia/1_Data_Raw/HES15BP.csv") # Person data
data_H_X <- read_csv("../0_Data/1_Household Data/1_Australia/1_Data_Raw/HES15BX.csv") # Expenditure level data
data_H_XC <- read_csv("../0_Data/1_Household Data/1_Australia/1_Data_Raw/HES15BXC.csv") # COICOP level data

# Basic CURF / Survey of Income and Housing

# data_S_B <- read_csv("../0_Data/1_Household Data/1_Australia/1_Data_Raw/SIH15B.csv", col_names = FALSE) # all levels data
# data_S_H <- read_csv("../0_Data/1_Household Data/1_Australia/1_Data_Raw/SIH15BH.csv") # Household level data
# data_S_I <- read_csv("../0_Data/1_Household Data/1_Australia/1_Data_Raw/SIH15BI.csv") # Income unit level data
# data_S_L <- read_csv("../0_Data/1_Household Data/1_Australia/1_Data_Raw/SIH15BL.csv") # Person level data
# data_S_P <- read_csv("../0_Data/1_Household Data/1_Australia/1_Data_Raw/SIH15BP.csv") # Loans level data

# Transform data

data_H_H_1 <- data_H_H %>%
  select(everything())%>%
  rename(hh_id      = ABSHID, 
         hh_size    = PERSHBC, 
         province   = STATEHEC,
         age_hhh    = AGERHEC, 
         sex_hhh    = SEXRH,
         hh_weights = HESHHWT,
         children   = NUMU15BC)%>%
  # 1 Greater capital city area, 2 Rest of state
  mutate(urban_01 = ifelse(GCCSA11C == 1, 1,0))%>%
  select(hh_id, hh_size, province, urban_01, age_hhh, sex_hhh, hh_weights, children)%>%
  mutate(adults = hh_size - children)

# Transfers received

data_H_H_2 <- data_H_H %>%
  mutate(inc_gov_monetary = (ITGCBCH8 + CWKCRAH)*52,
         inc_gov_cash     = (DBEN)*52)%>%
  rename(hh_id = ABSHID)%>%
  select(hh_id, inc_gov_cash, inc_gov_monetary)

data_H_P_1 <- data_H_P %>%
  select(everything())%>%
  filter(HHPOS == 1)%>%
  rename(hh_id = ABSHID, nationality = COBCB, edu_hhh = LVLEDUA, ind_hhh = INDCE, language = FLANGB)%>%
  select(hh_id, edu_hhh, ind_hhh, nationality, language)

household_information <- left_join(data_H_H_1, data_H_P_1)%>%
  left_join(data_H_H_2)

write_csv(household_information, "../0_Data/1_Household Data/1_Australia/1_Data_Clean/household_information_Australia.csv")

# Appliances: Fehlanzeige

# Expenditures: COICOP

data_H_X_1 <- data_H_X %>%
  select(ABSHID, COMCODE, WKLYEXP)%>%
  rename(hh_id = ABSHID, item_code = COMCODE, expenditures_year = WKLYEXP)%>%
  # Weekly expenditures
  mutate(expenditures_year = expenditures_year*52)%>%
  filter(expenditures_year > 0)

write_csv(data_H_X_1, "../0_Data/1_Household Data/1_Australia/1_Data_Clean/expenditures_items_Australia.csv")

# Codes

Province.Code <- data.frame(province = seq(1,8,1),
                            Province = c("New South Wales", "Victoria", "Queensland", "South Australia", "Western Australia", "Tasmania", "Northern Territory", "Australian Capital Territory"))%>%
  write_csv("../0_Data/1_Household Data/1_Australia/2_Codes/Province.Code.csv")

Nationality.Code <- data.frame(nationality = c(1,2,3),
                               Nationality = c("Australia", "Main English Speaking Countries", "Other"))%>%
  write_csv("../0_Data/1_Household Data/1_Australia/2_Codes/Nationality.Code.csv")

Education.Code <- data.frame(edu_hhh = c(seq(1,12,1)),
                             Education = c("Postgraduate", "Graduate", "Bachelor", "Advanced Diploma",
                                           "Certificate III/IV", "Certificate I/II", "Certificate n.f.d.",
                                           "Year 12", "Year 11", "Year 10", "Year 9", "Year 8 or lower"))%>%
  write_csv("../0_Data/1_Household Data/1_Australia/2_Codes/Education.Code.csv")

Industry.Code <- data.frame(ind_hhh = distinct(data_H_P, INDCE))%>%
  arrange(INDCE)%>%
  write_csv("../0_Data/1_Household Data/1_Australia/2_Codes/Industry.Code.csv")

Language.Code <- data.frame(language = seq(0,9,1),
                            Language = c("Not applicable", "English", "Northern European",
                                         "Southern European", "Eastern European", "Southwest and Central Asian",
                                         "Southern Asian", "Southeast Asian", "Eastern Asian", "Other"))%>%
  write_csv("../0_Data/1_Household Data/1_Australia/2_Codes/Language.Code.csv")

# Item codes

Item.Codes <- distinct(data_H_X_1, item_code)%>%
  arrange(item_code)

Item.Codes.Raw <- read.xlsx("../0_Data/1_Household Data/1_Australia/9_Documentation/Item_Codes_Description_Australia.xlsx", sheet = "Item_Australia", colNames = FALSE)%>%
  rename(item_code = X1, item_name = X2)%>%
  mutate(item_code = str_remove(item_code, "\\*"))%>%
  mutate(item_code = as.numeric(item_code))%>%
  filter(item_code %in% Item.Codes$item_code)

write.xlsx(Item.Codes.Raw, "../0_Data/1_Household Data/1_Australia/3_Matching_Tables/Item_Codes_Description_Australia.xlsx")
