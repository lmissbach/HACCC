if(!require("pacman")) install.packages("pacman")

p_load("countrycode", "haven", "Hmisc", "openxlsx", "rattle", "scales", "tidyverse", "readxl")

options(scipen=999)
  
# Authors: C. Buder, L. Missbach (missbach@mcc-berlin.net)
  
# Loading data ####

#reading data into R: Excel sheets trimesters agrado familiar, daily expenditures, monthly expenditures and individual expenditures 
data_households_moz_1.1       <- read_csv2("../0_Data/1_Household Data/2_Mozambique/1_Data_Raw/Base IOF-14-15/Base IOF-14-15/Bases de agregado familiar/Bases de agregado familiar_Excel/Base-de-AF-1.csv", locale=locale(encoding="latin1"))
data_households_moz_1.2       <- read_csv2("../0_Data/1_Household Data/2_Mozambique/1_Data_Raw/Base IOF-14-15/Base IOF-14-15/Bases de agregado familiar/Base-de-AF-2.csv", locale=locale(encoding="latin1"))
data_households_moz_1.3       <- read_csv2("../0_Data/1_Household Data/2_Mozambique/1_Data_Raw/Base IOF-14-15/Base IOF-14-15/Bases de agregado familiar/Base-de-AF-3.csv", locale=locale(encoding="latin1"))

transfers_1.1 <- read_excel("T:/MSA/papers_internal/work_in_progress/Mi_Homogenized_Datainfrastructure/0_Data/1_Household Data/2_Mozambique/1_Data_Raw/Base IOF-14-15/Base IOF-14-15/Bases de receitas e transferências/TRANSF_I TRIMESTRE_FINAL.xlsx", 
                            sheet = "TRANSF_I TRIM Trabalhado")
transfers_1.2 <- read_excel("T:/MSA/papers_internal/work_in_progress/Mi_Homogenized_Datainfrastructure/0_Data/1_Household Data/2_Mozambique/1_Data_Raw/Base IOF-14-15/Base IOF-14-15/Bases de receitas e transferências/TRANSF_II TRIMESTRE_FINAL.xlsx", 
                            sheet = "original trabalho")
transfers_1.3 <- read_excel("T:/MSA/papers_internal/work_in_progress/Mi_Homogenized_Datainfrastructure/0_Data/1_Household Data/2_Mozambique/1_Data_Raw/Base IOF-14-15/Base IOF-14-15/Bases de receitas e transferências/TRANSF_IV TRIMESTRE_FINAL.xlsx", 
                            sheet = "TF_IV TRIM trabalhado")

housing <- read_sav("T:/MSA/papers_internal/work_in_progress/Mi_Homogenized_Datainfrastructure/0_Data/1_Household Data/2_Mozambique/1_Data_Raw/Base IOF-14-15/Base IOF-14-15/Base de Habitação/Base de habitação_SPSS/HABITAÇÃO_I TRIMESTRE.sav")

appliances <- read_sav("T:/MSA/papers_internal/work_in_progress/Mi_Homogenized_Datainfrastructure/0_Data/1_Household Data/2_Mozambique/1_Data_Raw/Base IOF-14-15/Base IOF-14-15/Bases de posse de bens/Bases de bens duráveis_ SPSS/Base  Bens Duráveis_I Trimestre 12012016.sav")

data_expenditures_moz_1.1     <- read_sav("../0_Data/1_Household Data/2_Mozambique/1_Data_Raw/Base IOF-14-15/Base IOF-14-15/Bases de despesas mensais/Bases de despesas mensais_SPSS/DM_I TRIMESTRE_12012016.sav")
data_expenditures_moz_1.2     <- read_sav("../0_Data/1_Household Data/2_Mozambique/1_Data_Raw/Base IOF-14-15/Base IOF-14-15/Bases de despesas mensais/Bases de despesas mensais_SPSS/DM_II TRIMESTRE_12012016.sav")%>%
  select(-Trimestre)
data_expenditures_moz_1.3     <- read_csv2("../0_Data/1_Household Data/2_Mozambique/1_Data_Raw/Base IOF-14-15/Base IOF-14-15/Bases de despesas mensais/DM_IV TRIMESTRE_FINAL_04012016.csv", locale=locale(encoding="latin1"))

data_expenditures_moz_2.1     <- read_sav("../0_Data/1_Household Data/2_Mozambique/1_Data_Raw/Base IOF-14-15/Base IOF-14-15/Bases de Despesas Diárias/Bases de despesas diárias_SPSS/DD_I TRIMESTRE_12012016.sav")
data_expenditures_moz_2.2     <- read_sav("../0_Data/1_Household Data/2_Mozambique/1_Data_Raw/Base IOF-14-15/Base IOF-14-15/Bases de Despesas Diárias/Bases de despesas diárias_SPSS/DD_II TRIMESTRE_12012016.sav")
data_expenditures_moz_2.3     <- read_sav("../0_Data/1_Household Data/2_Mozambique/1_Data_Raw/Base IOF-14-15/Base IOF-14-15/Bases de Despesas Diárias/Bases de despesas diárias_SPSS/DD_IV TRIMESTRE_12012016.sav")

data_expenditures_3.1 <- read_csv2("../0_Data/1_Household Data/2_Mozambique/1_Data_Raw/Base IOF-14-15/Base IOF-14-15/Bases de despesas individuais/bases-de-despesas-individuais-1.csv", locale=locale(encoding="latin1"))
data_expenditures_3.2 <- read_csv2("../0_Data/1_Household Data/2_Mozambique/1_Data_Raw/Base IOF-14-15/Base IOF-14-15/Bases de despesas individuais/bases-de-despesas-individuais-2.csv", locale=locale(encoding="latin1"))
data_expenditures_3.3 <- read_csv2("../0_Data/1_Household Data/2_Mozambique/1_Data_Raw/Base IOF-14-15/Base IOF-14-15/Bases de despesas individuais/bases-de-despesas-individuais-3.csv", locale=locale(encoding="latin1")) 

# Household information 

#household ID, size, weights, province, district, rural/urban, age, sex, education, industry, religion, ethnicity, language, number of child&adults
hi_1 <- data_households_moz_1.1 %>% 
  select(`IOF_ ID & AF`, `Tamanho do AF`, Província, Distrito, `Código de Área de Residência`, Ponderadores,
         AF04, AF03, AF05, AF13, AF14) %>%
  rename(hh_id      = `IOF_ ID & AF`, 
         hh_size    = `Tamanho do AF`, 
         hh_weights = Ponderadores, 
         Province   = Província, 
         District   = Distrito, 
         urban_01   = `Código de Área de Residência`) %>%
  filter(AF03 == 01) %>% 
  select(-AF03) %>%
  rename(sex_hhh = AF04, age_hhh = AF05, edu_hhh_1 = AF14, edu_hhh_2 = AF13)%>%
  mutate(urban_01 = ifelse(urban_01 == 1,1,0),
         edu_hhh = ifelse(edu_hhh_2 == 2,"99",str_sub(edu_hhh_1, 1,2)))%>%
  mutate(edu_hhh = ifelse(edu_hhh %in% c("20","60","98","2"),"98",edu_hhh))%>%
  select(-edu_hhh_1, -edu_hhh_2)

hi_2 <- data_households_moz_1.2 %>% 
  select(`IOF_ ID & AF`, `Tamanho do AF`, Província, Distrito, `Código de Área de Residência`, 'Ponderadores_2º Trimestre',
         AF04, AF03, AF05, AF13, AF14) %>%
  rename(hh_id      = `IOF_ ID & AF`, 
         hh_size    = `Tamanho do AF`, 
         hh_weights = "Ponderadores_2º Trimestre",
         Province   = Província, 
         District   = Distrito, 
         urban_01   = `Código de Área de Residência`) %>%
  filter(AF03 == 01) %>% 
  select(-AF03) %>%
  rename(sex_hhh = AF04, age_hhh = AF05, edu_hhh_1 = AF14, edu_hhh_2 = AF13)%>%
  mutate(urban_01 = ifelse(urban_01 == 1,1,0),
         edu_hhh = ifelse(edu_hhh_2 == 2,"99",str_sub(edu_hhh_1, 1,2)))%>%
  mutate(edu_hhh = ifelse(edu_hhh %in% c("20","60","98","2"),"98",edu_hhh))%>%
  select(-edu_hhh_1, -edu_hhh_2)%>%
  filter(!hh_id %in% hi_1$hh_id)

hi_1 <- bind_rows(hi_1, hi_2)%>%
  arrange(hh_id)

housing_1 <- housing %>%
  mutate(id_0 = ifelse(nchar(ID07)==1,paste0("0",ID07),ID07),
         id_1 = ifelse(nchar(ID06)==1,paste0("000",ID06),
                       ifelse(nchar(ID06)==2,paste0("00",ID06),
                              ifelse(nchar(ID06)==3,paste0("0",ID06),ID06))))%>%
  mutate(hh_id = paste0(id_1,id_0))%>%
  select(hh_id, AF25, AF28, AF29, AF30)%>%
  rename(water = AF25, toilet = AF28, cooking_fuel = AF29, lighting_fuel = AF30)%>%
  mutate(electricity.access = ifelse(cooking_fuel == 1 | lighting_fuel == 1,1,0))

# Transfers #

transfers_1.3 <- bind_rows(transfers_1.1, transfers_1.2) %>%
  rename(hh_id = cc, 
         a1 =  "Dinheiro recebido pela pensão de reforma",	
         a2 =  "Dinheiro recebido pela pensão de divórcio",
         a3 =  "Dinheiro recebido pela pensão de sangue (viuvez)/orfandade",
         a4 =  "Dinheiro recebido pela pensão de alimentação",
         a5 =  "Dinheiro recebido em juros do banco ou dos devedores",
         a6 =  "Valor recebido pelos seguros",
         a7 =  "Dinheiro  oferecido pelas instituições sem fins lucrativos e religiosas",
         a71 =  "Valor estimado de ofertas em espécie de instituições sem fins lucrativos e religiosas",
         a9 =  "Dinheiro recebido de familiares que vivem fora do agregado",
         a91 = "Valor estimado de ofertas em espécie, que recebeu de familiares que vivem fora do AF",
         a11 = "Dinheiro recebido de familiares, que trabalham no estrangeiro",
         a12 = "Valor estimado de ofertas em espécie, que  recebeu de familiares que trabalham no estrangeiro",
         a13 = "Valor recebido em outras transferências",
         a14 = "Valor recebido em xitique")%>%
  select(hh_id, starts_with("a"))%>%
  mutate(inc_gov_monetary = a1 + a2 + a3 + a13,
         inc_gov_cash = 0)%>%
  group_by(hh_id)%>%
  summarise(inc_gov_monetary = sum(inc_gov_monetary),
            inc_gov_cash     = sum(inc_gov_cash))%>%
  ungroup()

Water.Code <- stack(attr(housing$AF25, 'labels'))%>%
  rename(water = values, Water = ind)%>%
  mutate(WTR = c("Basic", "Basic", "Basic", "Limited", "Limited", "Basic",
                 "Basic", "Basic", "Limited", "Basic", "Limited", "No Service", "No Service", "Basic", "Limited"))%>%
  write_csv(., "../0_Data/1_Household Data/2_Mozambique/2_Codes/Water.Code.csv")

Toilet.Code <- stack(attr(housing$AF28, 'labels'))%>%
  rename(toilet = values, Toilet = ind)%>%
  mutate(TLT = c("Basic", "Basic","Basic","Limited","No Service"))%>%
  write_csv(., "../0_Data/1_Household Data/2_Mozambique/2_Codes/Toilet.Code.csv")

Cooking.Code <- stack(attr(housing$AF29, 'labels'))%>%
  rename(cooking_fuel = values, Cooking_Fuel = ind)%>%
  mutate(CF = c("Electricity", "Gas", "Kerosene", "Charcoal", "Coal", "Firewood", "Other biomass", "Unknown"))%>%
  write_csv(., "../0_Data/1_Household Data/2_Mozambique/2_Codes/Cooking.Code.csv")

Lighting.Code <- stack(attr(housing$AF30, 'labels'))%>%
  rename(lighting_fuel = values, Lighting_Fuel = ind)%>%
  mutate(LF = c("Electricity", "Electricity", "Electricity", "Gas", "Kerosene", "Other lighting", "Other lighting", "Other lighting", "Firewood","Other lighting"))%>%
  write_csv(., "../0_Data/1_Household Data/2_Mozambique/2_Codes/Lighting.Code.csv")

Province.Code <- distinct(data_households_moz_1.1, Província)%>%
  arrange(Província)%>%
  rename(Province = Província)%>%
  mutate(province = 1:n())%>%
  write_csv("../0_Data/1_Household Data/2_Mozambique/2_Codes/Province.Code.csv")

District.Code <- distinct(data_households_moz_1.1, Distrito)%>%
  arrange(Distrito)%>%
  rename(District = Distrito)%>%
  mutate(district = 1:n())%>%
  write_csv("../0_Data/1_Household Data/2_Mozambique/2_Codes/District.Code.csv")

Gender.Code <- distinct(data_households_moz_1.1, AF04)%>%
  arrange(AF04)%>%
  rename(sex_hhh = AF04)%>%
  mutate(Gender = c("Male", "Female"))%>%
  write_csv("../0_Data/1_Household Data/2_Mozambique/2_Codes/Gender.Code.csv")

Education.Code <- distinct(hi_1, edu_hhh)%>%
  arrange(edu_hhh)%>%
  mutate(Education = c("Literacy", "Primary EP1", "Primary EP2", "Secondary ESG1",
                       "Secondary EGS2", "Elementary Technician", "Basic technician", "Medium technician", "For. of teachers", "Higher",
                       "Do not know", "No schooling"))%>%
  mutate(ISCED = c(1,1,1,2,3,4,4,4,6,6,9,0))%>%
  write_csv("../0_Data/1_Household Data/2_Mozambique/2_Codes/Education.Code.csv")

household_information <- left_join(hi_1, housing_1)%>%
  # deleting 22 households with missing information
  filter(!is.na(water))%>%
  left_join(transfers_1.3)%>%
  mutate(inc_gov_cash = ifelse(is.na(inc_gov_cash),0,inc_gov_cash),
         inc_gov_monetary = ifelse(is.na(inc_gov_monetary),0,inc_gov_monetary))%>%
  left_join(Province.Code)%>%
  left_join(District.Code)%>%
  select(-Province,-District)

write_csv(household_information, "../0_Data/1_Household Data/2_Mozambique/1_Data_Clean/household_information_Mozambique.csv")

# Appliance data ####

appliances_1.1 <- appliances %>%
  rename(hh_id = `IOF_IDampAF`, item_code = Código, item_name = Designação, value = Quantosbenspossui)%>%
  select(hh_id, item_name, value)%>%
  mutate(value = ifelse(value > 0,1,0))%>%
  pivot_wider(names_from = "item_name", values_from = "value", values_fill = 0)%>%
  rename(refrigerator.01a = Geleira,
         refrigerator.01b = Congelador,
         washing_machine.01 = "Máquinas de lavar roupa (inclui de secar)",
         stove.01a = "Fogões a carvão e/ou lenha",
         stove.01b = "Fogões a gás",
         stove.01c = "Fogões eléctricos",
         stove.01d = "Fogões mistos (eléctricos e a gás)",
         microwave.01 = "Micro-ondas",
         ac.01 = "Aparelhos de ar condicionado",
         fan.01 = "Ventoinhas/Ventiladores",
         car.01a = "Veículos automóveis novos",
         car.01b = "Veículos automóveis usados",
         motorcycle.01 = "Motorizadas",
         tv.01 = "Televisores")%>%
  mutate(car.01 = ifelse(car.01a > 0 | car.01b > 0,1,0),
         stove.01 = ifelse(stove.01a > 0 | stove.01b > 0 | stove.01c > 0 | stove.01d > 0,1,0),
         refrigerator.01 = ifelse(refrigerator.01a > 0 | refrigerator.01b > 0,1,0))%>%
  select(hh_id, ends_with(".01"))%>%
  right_join(select(household_information, hh_id))%>%
  mutate_at(vars(-hh_id), ~ ifelse(is.na(.),0,.))

write_csv(appliances_1.1, "../0_Data/1_Household Data/2_Mozambique/1_Data_Clean/appliances_0_1_Mozambique.csv")

# Expenditure data ####

# Monthly expenditures

data_expenditures_moz_1.1.1 <- data_expenditures_moz_1.1 %>%
  select(IOF_ID_AF, Quantidade_padrao, Valor_Original, Código, Designação)%>%
  rename(hh_id = IOF_ID_AF)

data_expenditures_moz_1.2.1 <- data_expenditures_moz_1.2 %>%
  select(IOF_IDampAF, Quantidade_padrao, Valor_Original, Código, Designação)%>%
  rename(hh_id = IOF_IDampAF)

data_expenditures_moz_1.3.1 <- data_expenditures_moz_1.3 %>%
  select('IOF_ ID & AF', Quantidade_padrao, Valor_Original, Código, Designação)%>%
  rename(hh_id = 'IOF_ ID & AF')

data_expenditures_1 <- bind_rows(data_expenditures_moz_1.1.1,
                                 data_expenditures_moz_1.2.1,
                                 data_expenditures_moz_1.3.1)%>%
  rename(item_code = Código, expenditures_year = Valor_Original)%>%
  arrange(hh_id, item_code)%>%
  group_by(hh_id, item_code)%>%
  summarise(expenditures_year = sum(expenditures_year))%>%
  ungroup()%>%
  mutate(expenditures_year = expenditures_year*12)%>%
  select(hh_id, item_code, expenditures_year)%>%
  mutate(item_code = as.numeric(item_code))

# Daily expenditures

data_expenditures_2 <- bind_rows(data_expenditures_moz_2.1,
                                 mutate_at(data_expenditures_moz_2.2, vars(Trimestre, "Unidade_padrao"), ~ as.character(.)),
                                 data_expenditures_moz_2.3)%>%
  rename(hh_id = IOF_IDampAF, item_code = CódigodeProduto)%>%
  arrange(hh_id, item_code)%>%
  select(hh_id, item_code, Valor_Original, Dias)%>%
  mutate(expenditures_year = Valor_Original*365/Dias)%>%
  filter(hh_id != "")%>%
  group_by(hh_id, item_code)%>%
  summarise(expenditures_year = sum(expenditures_year))%>%
  ungroup()%>%
  mutate(item_code = as.numeric(item_code))

#daily expenditures - individual

data_expenditures_3 <- bind_rows(data_expenditures_3.1,
                                 data_expenditures_3.2,
                                 data_expenditures_3.3)%>%
  rename(hh_id = `IOF_ ID & AF`, item_code = Código)%>%
  mutate(expenditures_year = Valor*365/7)%>%
  group_by(hh_id, item_code)%>%
  summarise(expenditures_year = sum(expenditures_year))%>%
  ungroup()%>%
  mutate(item_code = as.numeric(item_code))

# Education and health

data_expenditures_4.1 <- data_households_moz_1.1 %>%
  rename(hh_id      = `IOF_ ID & AF`)%>%
  select(hh_id, starts_with("AF18"), AF24A, AF24C, AF24E)%>%
  select(-AF18)

data_expenditures_4.1.A <- data_expenditures_4.1 %>%
  select(hh_id, AF18A, AF18A_CODIGO)%>%
  filter(!is.na(AF18A_CODIGO))%>%
  rename(item_code = AF18A_CODIGO, expenditures_year = AF18A)%>%
  mutate(expenditures_year = as.numeric(expenditures_year))%>%
  group_by(hh_id, item_code)%>%
  summarise(expenditures_year = sum(expenditures_year))%>%
  ungroup()
  
data_expenditures_4.1.B <- data_expenditures_4.1 %>%
  select(hh_id, AF18B, AF18B_CODIGO)%>%
  filter(!is.na(AF18B_CODIGO))%>%
  rename(item_code = AF18B_CODIGO, expenditures_year = AF18B)%>%
  mutate(expenditures_year = as.numeric(expenditures_year))%>%
  group_by(hh_id, item_code)%>%
  summarise(expenditures_year = sum(expenditures_year))%>%
  ungroup()%>%
  mutate(item_code = as.numeric(item_code))

data_expenditures_4.1.C <- data_expenditures_4.1 %>%
  select(hh_id, AF18C, AF18C_CODIGO)%>%
  filter(!is.na(AF18C_CODIGO))%>%
  rename(item_code = AF18C_CODIGO, expenditures_year = AF18C)%>%
  mutate(expenditures_year = as.numeric(expenditures_year))%>%
  group_by(hh_id, item_code)%>%
  summarise(expenditures_year = sum(expenditures_year))%>%
  ungroup()%>%
  mutate(item_code = as.numeric(item_code))

data_expenditures_4.1.D <- data_expenditures_4.1 %>%
  select(hh_id, AF18D, AF18D_CODIGO)%>%
  filter(!is.na(AF18D_CODIGO))%>%
  rename(item_code = AF18D_CODIGO, expenditures_year = AF18D)%>%
  mutate(expenditures_year = as.numeric(expenditures_year))%>%
  filter(!is.na(expenditures_year))%>%
  group_by(hh_id, item_code)%>%
  summarise(expenditures_year = sum(expenditures_year))%>%
  ungroup()%>%
  mutate(item_code = as.numeric(item_code))

data_expenditures_4.1.E <- data_expenditures_4.1 %>%
  select(hh_id, AF24A, AF24C, AF24E)%>%
  pivot_longer(-hh_id, names_to = "item_code", values_to = "expenditures_year")%>%
  filter(expenditures_year != "#NULL!")%>%
  mutate(expenditures_year = as.numeric(expenditures_year)*12)

# Expenses for appliances

data_expenditures_5 <- appliances %>%
  rename(hh_id = `IOF_IDampAF`, item_code = Código, item_name = Designação, value = Quantosbenspossui, expenditures_year = Valor_Total)%>%
  select(hh_id, item_code, expenditures_year)%>%
  group_by(hh_id, item_code)%>%
  summarise(expenditures_year = sum(expenditures_year))%>%
  ungroup()%>%
  mutate(item_code = as.numeric(item_code))

# Rents

data_expenditures_6 <- housing %>%
  mutate(id_0 = ifelse(nchar(ID07)==1,paste0("0",ID07),ID07),
         id_1 = ifelse(nchar(ID06)==1,paste0("000",ID06),
                       ifelse(nchar(ID06)==2,paste0("00",ID06),
                              ifelse(nchar(ID06)==3,paste0("0",ID06),ID06))))%>%
  mutate(hh_id = paste0(id_1,id_0))%>%
  select(hh_id, AF31A)%>%
  filter(!is.na(AF31A))%>%
  mutate(item_code = "AF31A")%>%
  mutate(expenditures_year = AF31A*12)%>%
  select(hh_id, item_code, expenditures_year)

data_expenditures <- bind_rows(data_expenditures_1,
                               data_expenditures_2,
                               data_expenditures_3,
                               data_expenditures_4.1.A,
                               data_expenditures_4.1.B,
                               data_expenditures_4.1.C, 
                               data_expenditures_4.1.D,
                               #data_expenditures_4.1.E,
                               data_expenditures_5,
                               #data_expenditures_6
                               )%>%
  mutate(item_code = as.character(item_code))%>%
  bind_rows(data_expenditures_4.1.E, data_expenditures_6)%>%
  arrange(hh_id, item_code)%>%
  filter(expenditures_year > 0 & !is.na(expenditures_year) & expenditures_year < 5000000)%>%
  group_by(hh_id, item_code)%>%
  summarise(expenditures_year = sum(expenditures_year))%>%
  ungroup()%>%
  filter(hh_id %in% household_information$hh_id)

write_csv(data_expenditures, "../0_Data/1_Household Data/2_Mozambique/1_Data_Clean/expenditures_items_Mozambique.csv")

household_information <- left_join(hi_1, housing_1)%>%
  # deleting 22 households with missing information
  filter(!is.na(water))%>%
  left_join(transfers_1.3)%>%
  mutate(inc_gov_cash = ifelse(is.na(inc_gov_cash),0,inc_gov_cash),
         inc_gov_monetary = ifelse(is.na(inc_gov_monetary),0,inc_gov_monetary))%>%
  left_join(Province.Code)%>%
  left_join(District.Code)%>%
  select(-Province,-District)%>%
  filter(hh_id %in% data_expenditures$hh_id)

write_csv(household_information, "../0_Data/1_Household Data/2_Mozambique/1_Data_Clean/household_information_Mozambique.csv")

# Item Codes

item_codes_0 <- read.xlsx("../0_Data/1_Household Data/2_Mozambique/3_Matching_Tables/Item.Codes.unmatched.xlsx")

item_1.1 <- bind_rows(data_expenditures_moz_1.1.1,
                      data_expenditures_moz_1.2.1,
                      data_expenditures_moz_1.3.1) %>%
  count(Código, Designação)%>%
  rename(item_code = Código, item_name = Designação)%>%
  group_by(item_code)%>%
  slice(which.max(n))%>%
  ungroup()%>%
  mutate(item_code = as.numeric(item_code))%>%
  mutate(item_code = as.character(item_code))%>%
  select(-n)

item_2.1 <- bind_rows(data_expenditures_moz_2.1,
                      mutate_at(data_expenditures_moz_2.2, vars(Trimestre, "Unidade_padrao"), ~ as.character(.)),
                      data_expenditures_moz_2.3) %>%
  count(CódigodeProduto, Designação)%>%
  rename(item_code = CódigodeProduto, item_name_2 = Designação)%>%
  group_by(item_code)%>%
  slice(which.max(n))%>%
  ungroup()%>%
  mutate(item_code = as.numeric(item_code))%>%
  mutate(item_code = as.character(item_code))%>%
  select(-n)

item_3.1 <- bind_rows(data_expenditures_3.1,
                                data_expenditures_3.2,
                                data_expenditures_3.3)%>%
  count(Código, Designação)%>%
  rename(item_code = Código, item_name_3 = Designação)%>%
  group_by(item_code)%>%
  slice(which.max(n))%>%
  ungroup()%>%
  mutate(item_code = as.numeric(item_code))%>%
  mutate(item_code = as.character(item_code))%>%
  select(-n)

item_4.1 <- appliances %>%
  count(Código, Designação)%>%
  rename(item_code = Código, item_name_4 = Designação)%>%
  group_by(item_code)%>%
  slice(which.max(n))%>%
  ungroup()%>%
  mutate(item_code = as.numeric(item_code))%>%
  mutate(item_code = as.character(item_code))%>%
  select(-n)


item_codes_0.1 <- left_join(item_codes_0, item_1.1)%>%
  left_join(item_2.1, by = "item_code")%>%
  left_join(item_3.1, by = "item_code")%>%
  left_join(item_4.1, by = "item_code")%>%
  arrange(item_code)%>%
  mutate(item_name_final = ifelse(!is.na(item_name), item_name,
                                  ifelse(!is.na(item_name_2), item_name_2,
                                         ifelse(!is.na(item_name_3), item_name_3, 
                                                ifelse(!is.na(item_name_4), item_name_4, NA)))))%>%
  select(item_code, item_name_final)%>%
  rename(item_name = item_name_final)%>%
  mutate(item_name = tolower(item_name))%>%
  arrange(item_code)

write.xlsx(item_codes_0.1, "../0_Data/1_Household Data/2_Mozambique/3_Matching_Tables/Item.Codes.unmatched_DESC.xlsx")
