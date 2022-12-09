# Authors: P. Blechschmidt, L. Missbach (missbach@mcc-berlin.net)

if(!require("pacman")) install.packages("pacman")

p_load("countrycode", "haven", "Hmisc", "openxlsx", "rattle", "scales", "tidyverse", "sjlabelled")

options(scipen=999)

# Load Data ####

data_raw     <- read_dta("../0_Data/1_Household Data/4_Norway/1_Data_Raw/Consumer Expenditure Survey, 2012.dta")

# data_labeled <- read_dta("../0_Data/1_Household Data/4_Norway/1_Data_Raw/data_labeled.dta")

# Process of translating the first-level variable labels ####

labels <- data.frame("Label" = c(""))

for(n in c(1:1258)){
labels_1 <- data.frame("Label" = attr(data_raw[[n]], 'label'))
labels <- bind_rows(labels, labels_1)
}

# write.table(labels, "../0_Data/1_Household Data/4_Norway/1_Data_Clean/labels.txt", row.names = FALSE)
 
# Import translated labels and overwrite old labels

labels_translated <- read_csv("../0_Data/1_Household Data/4_Norway/1_Data_Clean/labelsEN.txt", 
                              col_names = FALSE)

data_labeled <- data_raw

for(n in c(1:1258)){
  attr(data_labeled[[n]], 'label') <- labels_translated$X1[n]
}

new_labels <- scan("../0_Data/1_Household Data/4_Norway/1_Data_Clean/labelsEN.txt", what = "c", sep= "\n")

Variable.Code.Description <- data.frame()

for(n in c(1:1258)){
  Var.Code_0 <- data.frame("Variable" = colnames(data_labeled[n]),
                         "Label"      = attr(data_labeled[[n]], 'label'))
  Variable.Code.Description <- bind_rows(Variable.Code.Description, Var.Code_0)
}

# write.xlsx(Variable.Code.Description, "../0_Data/1_Household Data/4_Norway/9_Documentation/Variable_Description.xlsx")

# Transform data ####

data_0 <- data_labeled %>%
  rename(hh_id = lopenr, hh_size = antpersh,
         sex_hhh = kjonn1, ind_hhh = knaer1, edu_hhh = utdanning, province = nuts2)%>%
  mutate(hh_weights = 537.8268)%>% # STRONG ASSUMPTION
  mutate(aggi_13_2012 = ifelse(is.na(aggi_13_2012),0,aggi_13_2012),
         aggi_16_2012 = ifelse(is.na(aggi_16_2012),0,aggi_16_2012))%>%
  mutate(children         = antbu16,
         adults           = hh_size - children,
         inc_gov_monetary = aggi_13_2012 + aggi_16_2012,
         inc_gov_cash     = 0,
         urban_01         = ifelse(bstroek3 == 1,0,1))%>%
  select(hh_id, hh_size, hh_weights, sex_hhh, ind_hhh, edu_hhh, age_hhh = ald1, adults, children, inc_gov_monetary, inc_gov_cash, province, urban_01)%>%
  write_csv(., "../0_Data/1_Household Data/4_Norway/1_Data_Clean/household_information_Norway.csv")

# Codes ####
Province.Code <- distinct(data_labeled, nuts2)%>%
  arrange(nuts2)%>%
  rename(province = nuts2)%>%
  mutate(Province = c("Akershus og Oslo", "Hedmark op Oppland", "Sor-Ostlandet", "Agder og Rogaland", "Vestlandet", "Trondelag", "Nord-Norge"))%>%
  write_csv(., "../0_Data/1_Household Data/4_Norway/2_Codes/Province.Code.csv")
  
Gender.Code <- stack(attr(data_labeled$kjonn1, 'labels'))%>%
  rename(sex_hhh = values, Gender = ind)%>%
  write_csv(., "../0_Data/1_Household Data/4_Norway/2_Codes/Gender.Code.csv")
Industry.Code <- stack(attr(data_labeled$knaer1, 'labels')) %>%
  rename(ind_hhh = values, Industry = ind)%>%
  write_csv(., "../0_Data/1_Household Data/4_Norway/2_Codes/Industry.Code.csv")
Education.Code <- distinct(data_labeled, utdanning)%>%
  arrange(utdanning)%>%
  rename(edu_hhh = utdanning)%>%
  mutate(Education = NA)%>%
  write_csv(., "../0_Data/1_Household Data/4_Norway/2_Codes/Education.Code.csv")

# Education.Code <- read.csv("4_Norway/2_Codes/1659.csv", header = TRUE, sep = ";") %>%
#   filter(code < 10) %>%
#   filter(level ==1) %>%
#   select(edu_hhh = code, Education = name) %>%
#   write_csv(., "4_Norway/2_Codes/Education.Code.csv")

# Expenditures ####

expenditures_items <- data_labeled %>%
  rename(hh_id = lopenr)%>%
  select(hh_id, starts_with("cvnr"))%>%
  select(hh_id, ends_with("belop"))%>%
  remove_all_labels()%>%
  pivot_longer(-hh_id, names_to = "item_code", values_to = "expenditures_year")%>%
  filter(expenditures_year > 0)%>%
  mutate(item_code = str_replace(item_code, "cvnr_",""))%>%
  mutate(item_code = str_replace(item_code, "_belop",""))%>%
  write_csv(., "../0_Data/1_Household Data/4_Norway/1_Data_Clean/expenditures_items_Norway.csv")

Item.Matching <- read.xlsx("../0_Data/1_Household Data/4_Norway/3_Matching_Tables/Old/Item_GTAP_Concordance_Norway.xlsx")%>%
  mutate_at(vars(X3:X33), list(~ str_replace(., "cvnr_","")))%>%
  mutate_at(vars(X3:X33), list(~ str_replace(., "_belop", "")))%>%
  write.xlsx(., "../0_Data/1_Household Data/4_Norway/3_Matching_Tables/Item_GTAP_Concordance_Norway.xlsx")

Item.Categories <- read.xlsx("../0_Data/1_Household Data/4_Norway/3_Matching_Tables/Old/Item_Categories_Concordance_Norway.xlsx", colNames = FALSE)%>%
  mutate_at(vars(X2:X67), list(~ str_replace(., "cvnr_","")))%>%
  mutate_at(vars(X2:X67), list(~ str_replace(., "_belop", "")))%>%
  write.xlsx(., "../0_Data/1_Household Data/4_Norway/3_Matching_Tables/Item_Categories_Concordance_Norway.xlsx", colNames = FALSE)

Item.Fuels <- read.xlsx("../0_Data/1_Household Data/4_Norway/3_Matching_Tables/Old/Item_Fuel_Concordance_Norway.xlsx", colNames = FALSE)%>%
  mutate_at(vars(X2:X3), list(~ str_replace(., "cvnr_","")))%>%
  mutate_at(vars(X2:X3), list(~ str_replace(., "_belop", "")))%>%
  write.xlsx(., "../0_Data/1_Household Data/4_Norway/3_Matching_Tables/Item_Fuel_Concordance_Norway.xlsx", colNames = FALSE)

# labels <- scan("./4_Norway/1_Data_Clean/labelsEN.txt", what = "c", sep= "\n")
# item_codes <- expenses1%>%
#   filter(lopenr == 2)%>%
#   pivot_longer(-c(lopenr), names_to="item_code", values_to = "expenditures_year")%>%
#   mutate(item_name = labels[222:415]) %>%
#   select(item_code, item_name)%>%
#   write.xlsx(., "4_Norway/3_Matching_Tables/Item_Codes_Description_Norway.xlsx")

# Appliances ####

appliances_0_1_Norway <- data_labeled %>%
  rename(hh_id = lopenr)%>%
  select(hh_id, bil, motors, komfyr:pc)%>%
  rename(car.01 = bil, motorcycle.01 = motors, stove.01 = komfyr, dishwasher.01 = oppvask, 
         refrigerator.01a = kjfrys,  refrigerator.01b = kjskap, freezer.01 = fryser, washing_machine.01 = vaskem,
         dryer.01 = toerktr, tv.01 = tv, computer.01 = pc)%>%
  mutate(refrigerator.01 = ifelse(refrigerator.01a > 0 | refrigerator.01b > 0, 1,0),
         freezer.01      = ifelse(refrigerator.01a > 0 | freezer.01 > 0,1,0))%>%
  select(hh_id, ends_with(".01"))%>%
  mutate_at(vars(-hh_id), list(~ ifelse(. > 0,1,0)))

write_csv(appliances_0_1_Norway, "../0_Data/1_Household Data/4_Norway/1_Data_Clean/appliances_0_1_Norway.csv")
