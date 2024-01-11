# Author: L. Missbach, missbach@mcc-berlin.net (based on work from H. Ward, h.ward@cml.leidenuniv.nl)

# Initial - set working directory according to your needs 

# setwd("T:/MSA/papers_internal/work_in_progress/Mi_Homogenized_Datainfrastructure/0_Data/2_IO Data/GTAP_11_MRIO/")

# Packages 

library(RcppCNPy)
library(readr)
library(openxlsx)
library(readxl)
library(reticulate)
library(tidyverse)
options(scipen = "999")

list_0 <- list()

# 0   Load Data ####

# year_0 <- "2014"
year_0 <- "2017"

Countries <- read.xlsx("../0_Data/2_IO Data/GTAP_11_MRIO/Countries_Overview.xlsx")%>%
  mutate(Name = tolower(Code),
         Country_Name = Description,
         Country = Number)%>%
  select(Name, Country_Name, Country)

# Insert here v
# uncomment the following lines if you want to loop over many countries --> uncomment code at section 1.4.2

Country_Number <- 158 # South Africa

Country.Name <- Countries$Country_Name[Countries$Country == Country_Number]

#Country_Number <- Countries$Country[Countries$Country_Name == Country.Name]

# GTAP 10:

# open anaconda terminal and use
# conda config --remove-key channels
np <- import("numpy", convert = FALSE)

CO2_0 <- np$load("T:/MSA/papers_internal/work_in_progress/Mi_Homogenized_Datainfrastructure/0_Data/2_IO Data/GTAP_10_MRIO/CO2_Gtap_10_2014_re.npy")
Y_0   <- np$load("T:/MSA/papers_internal/work_in_progress/Mi_Homogenized_Datainfrastructure/0_Data/2_IO Data/GTAP_10_MRIO/gtap10_2014_re_y.npy")
Z_0   <- np$load("T:/MSA/papers_internal/work_in_progress/Mi_Homogenized_Datainfrastructure/0_Data/2_IO Data/GTAP_10_MRIO/gtap10_2014_re_z.npy")

CO2_R_10 <- py_to_r(CO2_0)
Y_R   <- py_to_r(Y_0)
Z_R   <- py_to_r(Z_0)

rm(Y_0, Z_0, CO2_0)

dim(Y_R) # 141 65 141     --> 141 --> Columns
dim(Z_R) # 141 65 141 65 

Y_R1_10 <- array_reshape(Y_R, c(9165, 141))%>%
  as.data.frame()
Z_R1_10 <- array_reshape(Z_R, c(9165, 9165))%>%
  as.data.frame()

rm(Z_R, Y_R)

# GTAP 11

CO2_R <- read_csv(sprintf("R:/MSA/datasets/GTAP/gtap11_data/GTAP11B_GTAP_%s_65x160/CO2_GTAP_11_%s_re.csv", year_0, year_0), col_names = FALSE, show_col_types = FALSE)
Y_0   <- np$load(sprintf("R:/MSA/datasets/GTAP/gtap11_data/GTAP11B_GTAP_%s_65x160/GTAP11_%s_re_y.npy", year_0, year_0))
Z_0   <- np$load(sprintf("R:/MSA/datasets/GTAP/gtap11_data/GTAP11B_GTAP_%s_65x160/GTAP11_%s_re_z.npy", year_0, year_0))

Y_R   <- py_to_r(Y_0)
Z_R   <- py_to_r(Z_0)

rm(Y_0, Z_0)

dim(Y_R) # 160 65 160     --> 141 --> Columns
dim(Z_R) # 160 65 160 65 

Y_R1 <- array_reshape(Y_R, c(10400, 160))%>%
  as.data.frame()
Z_R1 <- array_reshape(Z_R, c(10400, 10400))%>%
  as.data.frame()

rm(Z_R, Y_R)

# Comparing GTAP 10 and GTAP 11

# sum(Z_R1_10) # 80.846.828
# sum(Z_R1) # 80.616.775
# 
# sum(Y_R1_10) # 74.176.910
# sum(Y_R1) # 64.855.270
# 
# sum(CO2_R_11) # 28.541
# sum(CO2_R_10) # 25.963

rm(CO2_R_10, Y_R1_10, Z_R1_10)

# 0.1 Create Column Indices ####
# Dataframes are identical to Python ! 

colnames(Y_R1) <- paste0("Country_",1:ncol(Y_R1))
Y_R2 <- Y_R1 %>%
  mutate(Sector    = rep(1:65,160),
         n         = 1:n(),
         Country_A = ceiling(n/65))%>%
  select(-n)

# Column Names: Country First, Sector Second

help <- data.frame('A' = rep(1:65,160), 'B' = 1:10400)%>%
  mutate(C = ceiling(B/65))%>%
  unite(name, c(C,A), sep = "_")%>%
  select(name)

colnames(Z_R1) <- help$name

Z_R2 <- Z_R1 %>%
  mutate(Sector    = rep(1:65,160),
         n         = 1:n(),
         Country_A = ceiling(n/65))%>%
  select(-n)

colnames(CO2_R) <- paste0("Sector_", 1:ncol(CO2_R))
CO2_R <- as.data.frame(CO2_R)%>%
  mutate(Country = 1:n())

rm(Y_R1, Z_R1)

# 1.0 Calculations ####

# 1.1 Sectoral Output ####

Y_R2.1 <-Y_R2 %>%
  mutate(Y_total = select(., Country_1:Country_160) %>% rowSums(na.rm = TRUE))%>%
  select(Country_A, Sector, Y_total)

Z_R2.1 <- Z_R2 %>%
  mutate(Z_total = select(., '1_1':'160_65') %>% rowSums(na.rm = TRUE))%>%
  select(Country_A, Sector, Z_total)
 
OUT <- left_join(Y_R2.1, Z_R2.1)%>%
  mutate(OUT = Y_total + Z_total)%>%
  select(-Y_total, -Z_total)%>%
  mutate(Country_A = as.character(Country_A),
         Sector_A  = as.character(Sector))%>%
  select(Country_A, Sector_A, OUT)

rm(Y_R2.1, Z_R2.1)

# 1.2 Technology Matrix ####

# Quite the other way around!

A_0 <- Z_R2 %>%
  pivot_longer(c(1:10400), names_to = c("Country_B", "Sector_B"), names_sep = "_", values_to = 'Z')%>%
  left_join(OUT, by = c("Sector_B" = "Sector_A", "Country_B" = "Country_A"))%>%
  mutate(Z=Z/OUT)

# Stupid Coding, but pivot_wider does not work easily

memory.limit(size = 9999999999)

A_1 <- expand_grid("Country_A" = 1:160, "Sector_A" = 1:65)

for (i in c(1:160)){
  data_0 <- A_0 %>%
    filter(Country_B == i)%>%
    unite(VOI, c(Country_B, Sector_B), sep = "_")%>%
    rename(Sector_A = Sector)%>%
    select(-OUT)%>%
    pivot_wider(names_from = VOI, values_from = Z)
  
  A_1 <- left_join(A_1, data_0)
  print(i)
  
}

A_2 <- A_1 %>%
  select(-Country_A, -Sector_A)%>%
  data.matrix()

# 1.3 Leontief-Inverse ####

I_0 <- diag(10400)
I_1 <- I_0 - A_2

rm(I_0, A_2, A_0, A_1, Z_R2, data_0)

# library(matlib)
# Need Python-Support here -  do not know exactly why 
scipy.linalg <- import("scipy.linalg", convert = FALSE)

I_2 <- scipy.linalg$inv(I_1)
L <- py_to_r(I_2)%>%
  as.data.frame()

colnames(L) <- help$name


L_1 <- L %>%
  mutate(Sector_A    = rep(1:65,160),
         n         = 1:n(),
         Country_A = ceiling(n/65))%>%
  select(-n)

rm(I_2, I_1, help)

# 1.A Electricity Calculations ####

library(openxlsx)

# uncomment the following lines (and last lines of script) if you would like to loop over all countries

if(year_0 == "2017"){
  L_2 <- read_csv("../0_Data/2_IO Data/GTAP_11_MRIO/GTAP11B_2017/L_2.csv")%>%
    arrange(Country_B, Sector_B)
  
  # L_2_NON <- read_csv("L_2_NON.csv")%>%
  #   arrange(Country_B, Sector_B)
  
  national_carbon_intensities             <- read_csv("../0_Data/2_IO Data/GTAP_11_MRIO/GTAP11B_2017/National_Carbon_Intensities_L.csv")
  transport_national_carbon_intensities   <- read_csv("../0_Data/2_IO Data/GTAP_11_MRIO/GTAP11B_2017/Transport_National_Carbon_Intensities_L.csv")
  electricity_national_carbon_intensities <- read_csv("../0_Data/2_IO Data/GTAP_11_MRIO/GTAP11B_2017/Electricity_National_Carbon_Intensities_L.csv")
  
  # national_non_carbon_intensities         <- read_csv("National_NON_Carbon_Intensities_L.csv")
  gas_national_carbon_intensities         <- read_csv("../0_Data/2_IO Data/GTAP_11_MRIO/GTAP11B_2017/Gas_National_Carbon_Intensities_L.csv")
  # gas_intensities_pure                    <- read_csv("Gas_Intensities_Pure_L.csv")
  # coal_intensities_pure                   <- read_csv("Coal_Intensities_Pure_L.csv")
  # p_c_intensities_pure                    <- read_csv("P_C_Intensities_Pure_L.csv")
  # oil_intensities_pure                    <- read_csv("OIL_Intensities_Pure_L.csv")
  
  cons_dom <- read_delim("R:/MSA/datasets/GTAP/gtap11_data/GTAP11B_GTAP_2017_65x160/VDPB.csv", ",", escape_double = FALSE, trim_ws = TRUE, col_names = FALSE)
  cons_int <- read_delim("R:/MSA/datasets/GTAP/gtap11_data/GTAP11B_GTAP_2017_65x160/VMPB.csv", ",", escape_double = FALSE, trim_ws = TRUE, col_names = FALSE)
  
  CO2_dir_imp <- read_delim("R:/MSA/datasets/GTAP/gtap11_data/GTAP11B_GTAP_2017_65x160/MMP.csv", ",", escape_double = FALSE, col_names = FALSE, trim_ws = TRUE)%>%
    bind_rows(data.frame(X2 = "ita", X1 = "coa", X3 = 0))
  CO2_dir_dom <- read_delim("R:/MSA/datasets/GTAP/gtap11_data/GTAP11B_GTAP_2017_65x160/MDP.csv", ",", escape_double = FALSE, col_names = FALSE, trim_ws = TRUE)%>%
    bind_rows(data.frame(X2 = "ita", X1 = "coa", X3 = 0))
  
  # NONCO2 <- read_csv("Non-CO2/Output_2014_NonCO2_MtCO2.csv")%>%
  #   select(-Sector_Name, - Country_Name)%>%
  #   rename(Country_A = Country, Sector_A = Sector)%>%
  #   #mutate(Country_A = as.character(Country_A),
  #   #       Sector_A  = as.character(Sector_A))%>%
  #   filter(Sector_A != 66) # to be clarified what Sector 66 is (CGDS)
}

if(year_0 == "2014"){
  L_2 <- read_csv("../0_Data/2_IO Data/GTAP_11_MRIO/GTAP11A_2014/L_2.csv")%>%
    arrange(Country_B, Sector_B)
  
  # L_2_NON <- read_csv("L_2_NON.csv")%>%
  #   arrange(Country_B, Sector_B)
  
  national_carbon_intensities             <- read_csv("../0_Data/2_IO Data/GTAP_11_MRIO/GTAP11A_2014/National_Carbon_Intensities_L.csv")
  transport_national_carbon_intensities   <- read_csv("../0_Data/2_IO Data/GTAP_11_MRIO/GTAP11A_2014/Transport_National_Carbon_Intensities_L.csv")
  electricity_national_carbon_intensities <- read_csv("../0_Data/2_IO Data/GTAP_11_MRIO/GTAP11A_2014/Electricity_National_Carbon_Intensities_L.csv")
  
  # national_non_carbon_intensities         <- read_csv("National_NON_Carbon_Intensities_L.csv")
  gas_national_carbon_intensities         <- read_csv("../0_Data/2_IO Data/GTAP_11_MRIO/GTAP11A_2014/Gas_National_Carbon_Intensities_L.csv")
  # gas_intensities_pure                    <- read_csv("Gas_Intensities_Pure_L.csv")
  # coal_intensities_pure                   <- read_csv("Coal_Intensities_Pure_L.csv")
  # p_c_intensities_pure                    <- read_csv("P_C_Intensities_Pure_L.csv")
  # oil_intensities_pure                    <- read_csv("OIL_Intensities_Pure_L.csv")
  
  cons_dom <- read_delim("R:/MSA/datasets/GTAP/gtap11_data/GTAP11A_GTAP_2014_65x160/VDPB.csv", ",", escape_double = FALSE, trim_ws = TRUE, col_names = FALSE)
  cons_int <- read_delim("R:/MSA/datasets/GTAP/gtap11_data/GTAP11A_GTAP_2014_65x160/VMPB.csv", ",", escape_double = FALSE, trim_ws = TRUE, col_names = FALSE)
  
  CO2_dir_imp <- read_delim("R:/MSA/datasets/GTAP/gtap11_data/GTAP11A_GTAP_2014_65x160/MMP.csv", ",", escape_double = FALSE, col_names = FALSE, trim_ws = TRUE)%>%
    bind_rows(data.frame(X2 = "ita", X1 = "coa", X3 = 0))
  CO2_dir_dom <- read_delim("R:/MSA/datasets/GTAP/gtap11_data/GTAP11A_GTAP_2014_65x160/MDP.csv", ",", escape_double = FALSE, col_names = FALSE, trim_ws = TRUE)%>%
    bind_rows(data.frame(X2 = "ita", X1 = "coa", X3 = 0))
  
  # NONCO2 <- read_csv("Non-CO2/Output_2014_NonCO2_MtCO2.csv")%>%
  #   select(-Sector_Name, - Country_Name)%>%
  #   rename(Country_A = Country, Sector_A = Sector)%>%
  #   #mutate(Country_A = as.character(Country_A),
  #   #       Sector_A  = as.character(Sector_A))%>%
  #   filter(Sector_A != 66) # to be clarified what Sector 66 is (CGDS)
}

if(year_0 == "2011"){
  L_2 <- read_csv("../0_Data/2_IO Data/GTAP_11_MRIO/GTAP11A_2011/L_2.csv")%>%
    arrange(Country_B, Sector_B)
  
  # L_2_NON <- read_csv("L_2_NON.csv")%>%
  #   arrange(Country_B, Sector_B)
  
  national_carbon_intensities             <- read_csv("../0_Data/2_IO Data/GTAP_11_MRIO/GTAP11A_2011/National_Carbon_Intensities_L.csv")
  transport_national_carbon_intensities   <- read_csv("../0_Data/2_IO Data/GTAP_11_MRIO/GTAP11A_2011/Transport_National_Carbon_Intensities_L.csv")
  electricity_national_carbon_intensities <- read_csv("../0_Data/2_IO Data/GTAP_11_MRIO/GTAP11A_2011/Electricity_National_Carbon_Intensities_L.csv")
  
  # national_non_carbon_intensities         <- read_csv("National_NON_Carbon_Intensities_L.csv")
  gas_national_carbon_intensities         <- read_csv("../0_Data/2_IO Data/GTAP_11_MRIO/GTAP11A_2011/Gas_National_Carbon_Intensities_L.csv")
  # gas_intensities_pure                    <- read_csv("Gas_Intensities_Pure_L.csv")
  # coal_intensities_pure                   <- read_csv("Coal_Intensities_Pure_L.csv")
  # p_c_intensities_pure                    <- read_csv("P_C_Intensities_Pure_L.csv")
  # oil_intensities_pure                    <- read_csv("OIL_Intensities_Pure_L.csv")
  
  cons_dom <- read_delim("R:/MSA/datasets/GTAP/gtap11_data/GTAP11A_GTAP_2011_65x160/VDPB.csv", ",", escape_double = FALSE, trim_ws = TRUE, col_names = FALSE)
  cons_int <- read_delim("R:/MSA/datasets/GTAP/gtap11_data/GTAP11A_GTAP_2011_65x160/VMPB.csv", ",", escape_double = FALSE, trim_ws = TRUE, col_names = FALSE)
  
  CO2_dir_imp <- read_delim("R:/MSA/datasets/GTAP/gtap11_data/GTAP11A_GTAP_2011_65x160/MMP.csv", ",", escape_double = FALSE, col_names = FALSE, trim_ws = TRUE)
  CO2_dir_dom <- read_delim("R:/MSA/datasets/GTAP/gtap11_data/GTAP11A_GTAP_2011_65x160/MDP.csv", ",", escape_double = FALSE, col_names = FALSE, trim_ws = TRUE)
  
  # NONCO2 <- read_csv("Non-CO2/Output_2014_NonCO2_MtCO2.csv")%>%
  #   select(-Sector_Name, - Country_Name)%>%
  #   rename(Country_A = Country, Sector_A = Sector)%>%
  #   #mutate(Country_A = as.character(Country_A),
  #   #       Sector_A  = as.character(Sector_A))%>%
  #   filter(Sector_A != 66) # to be clarified what Sector 66 is (CGDS)
}

if(year_0 == "2007"){
  L_2 <- read_csv("../0_Data/2_IO Data/GTAP_11_MRIO/GTAP11A_2007/L_2.csv")%>%
    arrange(Country_B, Sector_B)
  
  # L_2_NON <- read_csv("L_2_NON.csv")%>%
  #   arrange(Country_B, Sector_B)
  
  national_carbon_intensities             <- read_csv("../0_Data/2_IO Data/GTAP_11_MRIO/GTAP11A_2007/National_Carbon_Intensities_L.csv")
  transport_national_carbon_intensities   <- read_csv("../0_Data/2_IO Data/GTAP_11_MRIO/GTAP11A_2007/Transport_National_Carbon_Intensities_L.csv")
  electricity_national_carbon_intensities <- read_csv("../0_Data/2_IO Data/GTAP_11_MRIO/GTAP11A_2007/Electricity_National_Carbon_Intensities_L.csv")
  
  # national_non_carbon_intensities         <- read_csv("National_NON_Carbon_Intensities_L.csv")
  gas_national_carbon_intensities         <- read_csv("../0_Data/2_IO Data/GTAP_11_MRIO/GTAP11A_2007/Gas_National_Carbon_Intensities_L.csv")
  # gas_intensities_pure                    <- read_csv("Gas_Intensities_Pure_L.csv")
  # coal_intensities_pure                   <- read_csv("Coal_Intensities_Pure_L.csv")
  # p_c_intensities_pure                    <- read_csv("P_C_Intensities_Pure_L.csv")
  # oil_intensities_pure                    <- read_csv("OIL_Intensities_Pure_L.csv")
  
  cons_dom <- read_delim("R:/MSA/datasets/GTAP/gtap11_data/GTAP11A_GTAP_2007_65x160/VDPB.csv", ",", escape_double = FALSE, trim_ws = TRUE, col_names = FALSE)
  cons_int <- read_delim("R:/MSA/datasets/GTAP/gtap11_data/GTAP11A_GTAP_2007_65x160/VMPB.csv", ",", escape_double = FALSE, trim_ws = TRUE, col_names = FALSE)
  
  CO2_dir_imp <- read_delim("R:/MSA/datasets/GTAP/gtap11_data/GTAP11A_GTAP_2007_65x160/MMP.csv", ",", escape_double = FALSE, col_names = FALSE, trim_ws = TRUE)%>%
    bind_rows(data.frame(X2 = "ita", X1 = "coa", X3 = 0))
  CO2_dir_dom <- read_delim("R:/MSA/datasets/GTAP/gtap11_data/GTAP11A_GTAP_2007_65x160/MDP.csv", ",", escape_double = FALSE, col_names = FALSE, trim_ws = TRUE)%>%
    bind_rows(data.frame(X2 = "ita", X1 = "coa", X3 = 0))
  
  # NONCO2 <- read_csv("Non-CO2/Output_2014_NonCO2_MtCO2.csv")%>%
  #   select(-Sector_Name, - Country_Name)%>%
  #   rename(Country_A = Country, Sector_A = Sector)%>%
  #   #mutate(Country_A = as.character(Country_A),
  #   #       Sector_A  = as.character(Sector_A))%>%
  #   filter(Sector_A != 66) # to be clarified what Sector 66 is (CGDS)
}

if(year_0 == "2004"){
  L_2 <- read_csv("../0_Data/2_IO Data/GTAP_11_MRIO/GTAP11A_2004/L_2.csv")%>%
    arrange(Country_B, Sector_B)
  
  # L_2_NON <- read_csv("L_2_NON.csv")%>%
  #   arrange(Country_B, Sector_B)
  
  national_carbon_intensities             <- read_csv("../0_Data/2_IO Data/GTAP_11_MRIO/GTAP11A_2004/National_Carbon_Intensities_L.csv")
  transport_national_carbon_intensities   <- read_csv("../0_Data/2_IO Data/GTAP_11_MRIO/GTAP11A_2004/Transport_National_Carbon_Intensities_L.csv")
  electricity_national_carbon_intensities <- read_csv("../0_Data/2_IO Data/GTAP_11_MRIO/GTAP11A_2004/Electricity_National_Carbon_Intensities_L.csv")
  
  # national_non_carbon_intensities         <- read_csv("National_NON_Carbon_Intensities_L.csv")
  gas_national_carbon_intensities         <- read_csv("../0_Data/2_IO Data/GTAP_11_MRIO/GTAP11A_2004/Gas_National_Carbon_Intensities_L.csv")
  # gas_intensities_pure                    <- read_csv("Gas_Intensities_Pure_L.csv")
  # coal_intensities_pure                   <- read_csv("Coal_Intensities_Pure_L.csv")
  # p_c_intensities_pure                    <- read_csv("P_C_Intensities_Pure_L.csv")
  # oil_intensities_pure                    <- read_csv("OIL_Intensities_Pure_L.csv")
  
  cons_dom <- read_delim("R:/MSA/datasets/GTAP/gtap11_data/GTAP11A_GTAP_2004_65x160/VDPB.csv", ",", escape_double = FALSE, trim_ws = TRUE, col_names = FALSE)
  cons_int <- read_delim("R:/MSA/datasets/GTAP/gtap11_data/GTAP11A_GTAP_2004_65x160/VMPB.csv", ",", escape_double = FALSE, trim_ws = TRUE, col_names = FALSE)
  
  CO2_dir_imp <- read_delim("R:/MSA/datasets/GTAP/gtap11_data/GTAP11A_GTAP_2004_65x160/MMP.csv", ",", escape_double = FALSE, col_names = FALSE, trim_ws = TRUE)
  CO2_dir_dom <- read_delim("R:/MSA/datasets/GTAP/gtap11_data/GTAP11A_GTAP_2004_65x160/MDP.csv", ",", escape_double = FALSE, col_names = FALSE, trim_ws = TRUE)
  
  # NONCO2 <- read_csv("Non-CO2/Output_2014_NonCO2_MtCO2.csv")%>%
  #   select(-Sector_Name, - Country_Name)%>%
  #   rename(Country_A = Country, Sector_A = Sector)%>%
  #   #mutate(Country_A = as.character(Country_A),
  #   #       Sector_A  = as.character(Sector_A))%>%
  #   filter(Sector_A != 66) # to be clarified what Sector 66 is (CGDS)
}

list_0 <- list()

for(k_0 in c(1:160)){
 
   Country_Number <- k_0
   
   Country.Name <- Countries$Country_Name[Countries$Country == Country_Number]

Y_OI <- Y_R2 %>%
  select(Country_A, Sector, paste0("Country_", Country_Number))%>%
  rename(Country = paste0("Country_", Country_Number))%>%
  mutate(Country_A = as.character(Country_A),
         Sector    = as.character(Sector))

L_OI <- L_1 %>%
  filter(Country_A == Country_Number & Sector_A == 46)%>%
  pivot_longer(c(1:10400), names_to = c("Country_B", "Sector_B"), names_sep = "_", values_to = 'L')

ELY_0 <- left_join(L_OI, Y_OI, by = c("Country_B" = "Country_A", "Sector_B" = "Sector"))%>%
  mutate(ELY = L*Country,
         Sector_B = as.numeric(Sector_B))%>%
  group_by(Sector_B)%>%
  summarise(ELY = sum(ELY))%>%
  ungroup()

# Insert Electricity Production (?) for respective Country in 2014 here

ELY_Country_Total <- 9294 # to be adjusted 

ELY_1 <- sum(ELY_0$ELY)
OUT_1 <- filter(OUT,Sector_A == 46 & Country_A == Country_Number)$OUT[1]
ELY_2 <- ELY_1*ELY_Country_Total/OUT_1

rm(L_OI)

# 1.4   Carbon Intensity ####
# 1.4.1 Carbon Intensity Total ####

CO2_R1 <- CO2_R %>%
  pivot_longer(starts_with("Sector_"), names_to = "Sector_A", values_to = "CO2", names_prefix = "Sector_")%>%
  rename(Country_A = Country)%>%
  mutate(Sector_A = as.numeric(Sector_A))

OUT <- OUT %>%
  mutate(Sector_A = as.numeric(Sector_A),
         Country_A = as.numeric(Country_A))

CO2_OUT <-   left_join(CO2_R1, OUT, by = c("Country_A", "Sector_A"))%>%
  mutate(CO2_OUT = ifelse(OUT != 0, CO2/OUT, 0))%>%
  select(-CO2, - OUT)

# uncomment the following lines if running code for the first time

# memory.limit(99999)
#
# L_2 <- L_1 %>%
#   pivot_longer(c(1:10400), names_to = c("Country_B", "Sector_B"), names_sep = "_", values_to = 'L')%>%
#   #group_by(Country_A, Sector_A)%>%
#   #summarise(L = sum(L))%>%
#   #ungroup()%>%
#   left_join(CO2_OUT, by = c("Country_A" = "Country_A", "Sector_A" = "Sector_A"))%>%
#   mutate(L = L*CO2_OUT)%>%
#   group_by(Country_B, Sector_B)%>%
#   summarise(L = sum(L))%>%
#   ungroup()%>%
#   mutate(Sector_B  = as.numeric(Sector_B),
#          Country_B = as.numeric(Country_B))
# 
# if(year_0 != "2017"){write_csv(L_2, sprintf("../0_Data/2_IO Data/GTAP_11_MRIO/GTAP11B_%s/L_2.csv", year_0))}
# # write_csv(L_2, "../0_Data/2_IO Data/GTAP_11_MRIO/GTAP11B_2017/L_2.csv")

# L_2 <- read_csv("L_2.csv")%>%
#   arrange(Country_B, Sector_B)
#test <- data.frame(read_csv("test.csv"))

# L2_R <- L_2 %>%
#   pivot_wider(names_from = "Sector_B", values_from = "L", names_prefix = "Sector_")

rm(CO2_OUT, CO2_R1)

# 1.4.2 Carbon Intensity National ####

# uncomment the following lines if running code for the first time

#  national_carbon_intensities <- data.frame()
#  
#  CO2_OUT_2 <- left_join(CO2_R1, OUT)%>%
#   mutate(CO2_OUT = ifelse(OUT != 0, CO2/OUT,0))%>%
#   select(-CO2, -OUT)
#  
#  L_2_2 <- L_1 %>%
#   pivot_longer(c(1:10400), names_to = c("Country_B", "Sector_B"), names_sep = "_", values_to = 'L')%>%
#   left_join(CO2_OUT_2, by = c("Country_A" = "Country_A", "Sector_A" = "Sector_A"))
#  
#  for(i in c(1:160)){
#  
#  # CO2_R2 <- CO2_R1 %>%
#  #   mutate(CO2 = ifelse(Country_A == i, CO2, 0))
#  # 
#  # CO2_OUT_2 <-   left_join(CO2_R2, OUT)%>%
#  #   mutate(CO2_OUT = ifelse(OUT != 0, CO2/OUT, 0))%>%
#  #   select(-CO2, - OUT)
#  
#  L_2_2.1 <- L_2_2 %>%
#    mutate(CO2_OUT = ifelse(Country_A == i, CO2_OUT,0))%>%
#    mutate(L = L*CO2_OUT)%>%
#    group_by(Country_B, Sector_B)%>%
#    summarise(L = sum(L))%>%
#    ungroup()%>%
#    mutate(Sector_B  = as.numeric(Sector_B),
#           Country_B = as.numeric(Country_B))%>%
#    mutate(Country_Host = i)
#  
#  national_carbon_intensities <- bind_rows(national_carbon_intensities, L_2_2.1)
#  
#  print(i)
#  
#  }
#  
#  if(year_0 != "2017"){write_csv(national_carbon_intensities, sprintf("../0_Data/2_IO Data/GTAP_11_MRIO/GTAP11A_%s/National_Carbon_Intensities_L.csv", year_0))}
#  if(year_0 == "2017"){write_csv(national_carbon_intensities, "../0_Data/2_IO Data/GTAP_11_MRIO/GTAP11B_2017/National_Carbon_Intensities_L.csv")}

# national_carbon_intensities <- read_csv("National_Carbon_Intensities_L.csv")

national_carbon_intensities_2 <- national_carbon_intensities %>%
  filter(Country_Host == Country_Number)

# 1.4.3 Carbon Intensity Transport Sector Only ####

# uncomment the following lines if running code for the first time

#    CO2_R3 <- CO2_R %>%
#      pivot_longer(starts_with("Sector_"), names_to = "Sector_A", values_to = "CO2", names_prefix = "Sector_")%>%
#      rename(Country_A = Country)%>%
#      mutate(Sector_A = as.numeric(Sector_A))%>%
#      mutate(CO2 = ifelse(Sector_A != 32 & Sector_A != 52 & Sector_A != 53 & Sector_A != 54, 0, CO2))
#    
#   CO2_OUT_3 <- left_join(CO2_R3, OUT)%>%
#     mutate(CO2_OUT = ifelse(OUT != 0, CO2/OUT, 0))%>%
#     select(-CO2, - OUT)
#   
#    L_2_3 <- L_1 %>%
#     filter(Sector_A == 32 | Sector_A == 52 | Sector_A == 53 | Sector_A == 54)%>%
#     pivot_longer(c(1:10400), names_to = c("Country_B", "Sector_B"), names_sep = "_", values_to = 'L')%>%
#     left_join(CO2_OUT_3, by = c("Country_A" = "Country_A", "Sector_A" = "Sector_A"))
#   
#    transport_national_carbon_intensities <- data.frame()
#    
#    for(i in c(1:160)){
#  
#  # CO2_R3.1 <- CO2_R3 %>%
#  #  mutate(CO2 = ifelse(Country_A == i, CO2, 0))
#  # 
#  # CO2_OUT_3 <-   left_join(CO2_R3.1, OUT)%>%
#  #  mutate(CO2_OUT = ifelse(OUT != 0, CO2/OUT, 0))%>%
#  #  select(-CO2, - OUT)
#  
#   L_2_3_1 <- L_2_3 %>%
#     filter(Country_A == i)%>%
#     mutate(CO2_OUT = ifelse(Country_A == i, CO2_OUT,0))%>%
#     mutate(L = L*CO2_OUT)%>%
#     group_by(Country_B, Sector_B)%>%
#     summarise(L = sum(L))%>%
#     ungroup()%>%
#     mutate(Sector_B  = as.numeric(Sector_B),
#            Country_B = as.numeric(Country_B))%>%
#     mutate(Country_Host = i)
#   
#   transport_national_carbon_intensities <- bind_rows(transport_national_carbon_intensities, L_2_3_1)
#   
#   }
#    if(year_0 != "2017"){write_csv(transport_national_carbon_intensities, sprintf("../0_Data/2_IO Data/GTAP_11_MRIO/GTAP11A_%s/Transport_National_Carbon_Intensities_L.csv", year_0))}
#    if(year_0 == "2017"){write_csv(transport_national_carbon_intensities, "../0_Data/2_IO Data/GTAP_11_MRIO/GTAP11B_2017/Transport_National_Carbon_Intensities_L.csv")}

transport_national_carbon_intensities_2 <- transport_national_carbon_intensities%>%
  filter(Country_Host == Country_Number)

# 1.4.4 Carbon Intensity Electricity Sector Only ####

# uncomment the following lines if running code for the first time

#  CO2_R4 <- CO2_R %>%
#    pivot_longer(starts_with("Sector_"), names_to = "Sector_A", values_to = "CO2", names_prefix = "Sector_")%>%
#    rename(Country_A = Country)%>%
#    mutate(Sector_A = as.numeric(Sector_A))%>%
#    mutate(CO2 = ifelse(Sector_A != 46, 0, CO2))
# 
# CO2_OUT_4 <- left_join(CO2_R4, OUT)%>%
#   mutate(CO2_OUT = ifelse(OUT != 0, CO2/OUT, 0))%>%
#   select(-CO2, - OUT)
# 
#  electricity_national_carbon_intensities <- data.frame()
#  
#  L_2_4 <- L_1 %>%
#    filter(Sector_A == 46)%>%
#    pivot_longer(c(1:10400), names_to = c("Country_B", "Sector_B"), names_sep = "_", values_to = 'L')%>%
#    left_join(CO2_OUT_4, by = c("Country_A" = "Country_A", "Sector_A" = "Sector_A"))
#  
#  for(i in c(1:160)){
#  
#    # CO2_R4.1 <- CO2_R4 %>%
#    #   mutate(CO2 = ifelse(Country_A == i, CO2, 0))
#    # 
#    # CO2_OUT_4 <-   left_join(CO2_R4.1, OUT)%>%
#    #   mutate(CO2_OUT = ifelse(OUT != 0, CO2/OUT, 0))%>%
#    #   select(-CO2, - OUT)
#    
#    L_2_4_1 <- L_2_4 %>%
#      filter(Country_A == i)%>%
#      mutate(CO2_OUT = ifelse(Country_A == i, CO2_OUT,0))%>%
#      mutate(L = L*CO2_OUT)%>%
#      group_by(Country_B, Sector_B)%>%
#      summarise(L = sum(L))%>%
#      ungroup()%>%
#      mutate(Sector_B  = as.numeric(Sector_B),
#             Country_B = as.numeric(Country_B))%>%
#      mutate(Country_Host = i)
#    
#    electricity_national_carbon_intensities <- bind_rows(electricity_national_carbon_intensities, L_2_4_1)
#    
#  }
#  
#  if(year_0 != "2017"){write_csv(electricity_national_carbon_intensities, sprintf("../0_Data/2_IO Data/GTAP_11_MRIO/GTAP11A_%s/Electricity_National_Carbon_Intensities_L.csv", year_0))}
#  if(year_0 == "2017"){write_csv(electricity_national_carbon_intensities, "../0_Data/2_IO Data/GTAP_11_MRIO/GTAP11B_2017/Electricity_National_Carbon_Intensities_L.csv")}

electricity_national_carbon_intensities_2 <- electricity_national_carbon_intensities %>%
  filter(Country_Host == Country_Number)

# 1.4.5 Carbon Intensity Non-CO2-Emissions ####

# NONCO2 <- read_csv("Non-CO2/Output_2014_NonCO2_MtCO2.csv")%>%
#   select(-Sector_Name, - Country_Name)%>%
#   rename(Country_A = Country, Sector_A = Sector)%>%
#   #mutate(Country_A = as.character(Country_A),
#   #       Sector_A  = as.character(Sector_A))%>%
#   filter(Sector_A != 66) # to be clarified what Sector 66 is (CGDS)

### THIS IS NEEDED BELOW
# NONCO2_OUT <-   left_join(NONCO2, OUT)%>%
#   mutate(CH4_OUT  = ifelse(OUT != 0, CH4/OUT,  0),
#          FGAS_OUT = ifelse(OUT != 0, FGAS/OUT, 0),
#          N2O_OUT  = ifelse(OUT != 0, N2O/OUT,  0))%>%
#   select(-FGAS, - CH4, - N2O, - OUT)%>%
#   mutate(Sector_A  = as.numeric(Sector_A),
#          Country_A = as.numeric(Country_A))
###

# Global

# memory.limit(99999)
# 
# L_2_NON <- L_1 %>%
#  pivot_longer(c(1:9165), names_to = c("Country_B", "Sector_B"), names_sep = "_", values_to = 'L')%>%
#  left_join(NONCO2_OUT, by = c("Country_A" = "Country_A", "Sector_A" = "Sector_A"))%>%
#  mutate(L_CH4  = L*CH4_OUT,
#         L_FGAS = L*FGAS_OUT,
#         L_N2O  = L*N2O_OUT)%>%
#  group_by(Country_B, Sector_B)%>%
#  summarise(L_CH4  = sum(L_CH4),
#            L_FGAS = sum(L_FGAS),
#            L_N2O = sum(L_N2O))%>%
#  ungroup()%>%
#  mutate(Sector_B  = as.numeric(Sector_B),
#         Country_B = as.numeric(Country_B))
# 
# write_csv(L_2_NON, "L_2_NON.csv")

# L_2_NON <- read_csv("L_2_NON.csv")%>%
#   arrange(Country_B, Sector_B)

# National
# uncomment if you are running this script for the first time

# national_non_carbon_intensities <- data.frame()
#  
#  for(i in c(1:141)){
# NONCO2_1 <- NONCO2 %>%
#  mutate(CH4  = ifelse(Country_A == i, CH4, 0),
#         N2O  = ifelse(Country_A == i, N2O, 0),
#         FGAS = ifelse(Country_A == i, FGAS, 0))
# 
# NONCO2_OUT_2 <-   left_join(NONCO2_1, OUT)%>%
#  mutate(CH4_OUT  = ifelse(OUT != 0, CH4/OUT, 0),
#         N2O_OUT  = ifelse(OUT != 0, N2O/OUT, 0),
#         FGAS_OUT = ifelse(OUT != 0, FGAS/OUT, 0))%>%
#  select(-CH4, - N2O, -FGAS, - OUT)%>%
#  mutate(Sector_A  = as.numeric(Sector_A),
#         Country_A = as.numeric(Country_A))
# 
# L_2_2 <- L_1 %>%
#  pivot_longer(c(1:9165), names_to = c("Country_B", "Sector_B"), names_sep = "_", values_to = 'L')%>%
#  left_join(NONCO2_OUT_2, by = c("Country_A" = "Country_A", "Sector_A" = "Sector_A"))%>%
#  mutate(L_CH4  = L*CH4_OUT,
#         L_FGAS = L*FGAS_OUT,
#         L_N2O  = L*N2O_OUT)%>%
#  group_by(Country_B, Sector_B)%>%
#  summarise(L_CH4  = sum(L_CH4),
#            L_FGAS = sum(L_FGAS),
#            L_N2O  = sum(L_N2O))%>%
#  ungroup()%>%
#  mutate(Sector_B  = as.numeric(Sector_B),
#         Country_B = as.numeric(Country_B))%>%
#  mutate(Country_Host = i)
# 
# national_non_carbon_intensities <- bind_rows(national_non_carbon_intensities, L_2_2)
# 
# }
#  
#  write_csv(national_non_carbon_intensities, "National_NON_Carbon_Intensities_L.csv")

# national_non_carbon_intensities <- read_csv("National_NON_Carbon_Intensities_L.csv")

# national_non_carbon_intensities_2 <- national_non_carbon_intensities %>%
#   filter(Country_Host == Country_Number)

# 1.4.6 Carbon Intensity Gas Sector Only ####

# uncomment the following lines if running code for the first time
#   CO2_R6 <- CO2_R %>%
#     pivot_longer(starts_with("Sector_"), names_to = "Sector_A", values_to = "CO2", names_prefix = "Sector_")%>%
#     rename(Country_A = Country)%>%
#     mutate(Sector_A = as.numeric(Sector_A))%>%
#     # mutate(Country_A = as.numeric(Country_A))%>%
#     mutate(CO2 = ifelse(Sector_A != 17 & Sector_A != 47, 0, CO2))
#   
#   L_2_6 <- L_1 %>%
#    filter(Sector_A == 17 | Sector_A == 47)%>%
#    pivot_longer(c(1:10400), names_to = c("Country_B", "Sector_B"), names_sep = "_", values_to = 'L')
#  
#   OUT_1 <- OUT %>%
#     mutate(Country_A = as.numeric(Country_A),
#            Sector_A  = as.numeric(Sector_A))
#   
#  gas_national_carbon_intensities <- data.frame()
#  
#  for(i in c(1:160)){
#  
#  ## uncomment ONLY for national gas intensity
#      
#  CO2_R6.1 <- CO2_R6 %>%
#    mutate(CO2 = ifelse(Country_A == i, CO2, 0))
#  
#  CO2_OUT_6 <-   left_join(CO2_R6.1, OUT_1)%>%
#    mutate(CO2_OUT = ifelse(OUT != 0, CO2/OUT, 0))%>%
#    select(-CO2, - OUT)
#  
#  L_2_6_1 <- L_2_6 %>%
#   filter(Country_A == i)%>%
#   left_join(CO2_OUT_6, by = c("Country_A" = "Country_A", "Sector_A" = "Sector_A"))%>%
#   mutate(L = L*CO2_OUT)%>%
#   group_by(Country_B, Sector_B)%>%
#   summarise(L = sum(L))%>%
#   ungroup()%>%
#   mutate(Sector_B  = as.numeric(Sector_B),
#          Country_B = as.numeric(Country_B))%>%
#   mutate(Country_Host = i)
#  
#  gas_national_carbon_intensities <- bind_rows(gas_national_carbon_intensities, L_2_6_1)
#  
#  }
#  if(year_0 != "2017"){write_csv(gas_national_carbon_intensities, sprintf("../0_Data/2_IO Data/GTAP_11_MRIO/GTAP11A_%s/Gas_National_Carbon_Intensities_L.csv", year_0))}
#  if(year_0 == "2017"){write_csv(gas_national_carbon_intensities, "../0_Data/2_IO Data/GTAP_11_MRIO/GTAP11B_2017/Gas_National_Carbon_Intensities_L.csv")}

gas_national_carbon_intensities_2 <- gas_national_carbon_intensities %>%
 filter(Country_Host == Country_Number)

# 1.4.7 Gas Price Intensity ####

# uncomment the following lines if running code for the first time

# CO2_R7 <- CO2_R %>%
#   pivot_longer(starts_with("Sector_"), names_to = "Sector_A", values_to = "CO2", names_prefix = "Sector_")%>%
#   rename(Country_A = Country)%>%
#   mutate(Sector_A = as.numeric(Sector_A))%>%
#   mutate(GAS = ifelse(Sector_A != 17 & Sector_A != 47, 0, 1))
# 
# L_2_7 <- L_1 %>%
#  filter(Sector_A == 17 | Sector_A == 47)%>%
#  pivot_longer(c(1:10400), names_to = c("Country_B", "Sector_B"), names_sep = "_", values_to = 'L')
# 
# gas_intensities_pure <- data.frame()
# 
# for(i in c(1:160)){
# 
# ## uncomment ONLY for national gas intensity
#    
# ## CO2_R7.1 <- CO2_R7 %>%
# ##   mutate(GAS = ifelse(Country_A == i, GAS, 0))
# 
# CO2_OUT_7 <-   left_join(CO2_R7, OUT)%>%
#  mutate(GAS_OUT = ifelse(OUT != 0, GAS/OUT, 0))%>%
#  select(-GAS, - OUT)
# 
# L_2_7_1 <- L_2_7 %>%
# filter(Country_A == i)%>%
# left_join(CO2_OUT_7, by = c("Country_A" = "Country_A", "Sector_A" = "Sector_A"))%>%
# mutate(L = L*GAS_OUT)%>%
# group_by(Country_B, Sector_B)%>%
# summarise(L = sum(L))%>%
# ungroup()%>%
# mutate(Sector_B  = as.numeric(Sector_B),
#        Country_B = as.numeric(Country_B))%>%
# mutate(Country_Host = i)
# 
# gas_intensities_pure <- bind_rows(gas_intensities_pure, L_2_7_1)
# 
# }
# 
# write_csv(gas_intensities_pure, "Gas_Intensities_Pure_L.csv")

# gas_intensities_pure_2 <- gas_intensities_pure %>%
#   filter(Country_Host == Country_Number)

# 1.4.8 Coal Price Intensity ####

# # uncomment the following lines if running code for the first time
# 
# CO2_R8 <- CO2_R %>%
#  pivot_longer(starts_with("Sector_"), names_to = "Sector_A", values_to = "CO2", names_prefix = "Sector_")%>%
#  rename(Country_A = Country)%>%
#  mutate(Sector_A = as.numeric(Sector_A))%>%
#  mutate(COAL = ifelse(Sector_A != 15, 0, 1))
# 
# L_2_8 <- L_1 %>%
# filter(Sector_A == 15)%>%
# pivot_longer(c(1:10400), names_to = c("Country_B", "Sector_B"), names_sep = "_", values_to = 'L')
# 
# coal_intensities_pure <- data.frame()
# 
# for(i in c(1:160)){
# 
# ## uncomment ONLY for national gas intensity
#   
# ## CO2_R8.1 <- CO2_R8 %>%
# ##   mutate(COAL = ifelse(Country_A == i, COAL, 0))
# 
# CO2_OUT_8 <-   left_join(CO2_R8, OUT)%>%
# mutate(COAL_OUT = ifelse(OUT != 0, COAL/OUT, 0))%>%
# select(-COAL, - OUT)
# 
# L_2_8_1 <- L_2_8 %>%
# filter(Country_A == i)%>%
# left_join(CO2_OUT_8, by = c("Country_A" = "Country_A", "Sector_A" = "Sector_A"))%>%
# mutate(L = L*COAL_OUT)%>%
# group_by(Country_B, Sector_B)%>%
# summarise(L = sum(L))%>%
# ungroup()%>%
# mutate(Sector_B  = as.numeric(Sector_B),
#       Country_B = as.numeric(Country_B))%>%
# mutate(Country_Host = i)
# 
# coal_intensities_pure <- bind_rows(coal_intensities_pure, L_2_8_1)
# 
# }
# 
# write_csv(coal_intensities_pure, "Coal_Intensities_Pure_L.csv")

# coal_intensities_pure_2 <- coal_intensities_pure %>%
#   filter(Country_Host == Country_Number)

# 1.4.9 Petroleum & Coke Intensity ####

# uncomment the following lines if running code for the first time

# CO2_R9 <- CO2_R %>%
#   pivot_longer(starts_with("Sector_"), names_to = "Sector_A", values_to = "CO2", names_prefix = "Sector_")%>%
#   rename(Country_A = Country)%>%
#   mutate(Sector_A = as.numeric(Sector_A))%>%
#   mutate(P_C = ifelse(Sector_A != 32, 0, 1))
# 
# L_2_9 <- L_1 %>%
#   filter(Sector_A == 32)%>%
#   pivot_longer(c(1:10400), names_to = c("Country_B", "Sector_B"), names_sep = "_", values_to = 'L')
# 
# p_c_intensities_pure <- data.frame()
# 
# for(i in c(1:160)){
#   
#   ## uncomment ONLY for national gas intensity
#   
#   ## CO2_R9.1 <- CO2_R9 %>%
#   ##   mutate(P_C = ifelse(Country_A == i, P_C, 0))
#   
#   CO2_OUT_9 <-   left_join(CO2_R9, OUT)%>%
#     mutate(P_C_OUT = ifelse(OUT != 0, P_C/OUT, 0))%>%
#     select(-P_C, - OUT)
#   
#   L_2_9_1 <- L_2_9 %>%
#     filter(Country_A == i)%>%
#     left_join(CO2_OUT_9, by = c("Country_A" = "Country_A", "Sector_A" = "Sector_A"))%>%
#     mutate(L = L*P_C_OUT)%>%
#     group_by(Country_B, Sector_B)%>%
#     summarise(L = sum(L))%>%
#     ungroup()%>%
#     mutate(Sector_B  = as.numeric(Sector_B),
#            Country_B = as.numeric(Country_B))%>%
#     mutate(Country_Host = i)
#   
#   p_c_intensities_pure <- bind_rows(p_c_intensities_pure, L_2_9_1)
#   
# }
# 
# write_csv(p_c_intensities_pure, "P_C_Intensities_Pure_L.csv")

# p_c_intensities_pure_2 <- p_c_intensities_pure %>%
#   filter(Country_Host == Country_Number)

# 1.5.0 Oil Intensity ####

# CO2_R10 <- CO2_R %>%
#   pivot_longer(starts_with("Sector_"), names_to = "Sector_A", values_to = "CO2", names_prefix = "Sector_")%>%
#   rename(Country_A = Country)%>%
#   mutate(Sector_A = as.numeric(Sector_A))%>%
#   mutate(OIL = ifelse(Sector_A != 16, 0, 1))
# 
# L_2_10 <- L_1 %>%
#   filter(Sector_A == 16)%>%
#   pivot_longer(c(1:10400), names_to = c("Country_B", "Sector_B"), names_sep = "_", values_to = 'L')
# 
# oil_intensities_pure <- data.frame()
# 
# for(i in c(1:160)){
#   
#   ## uncomment ONLY for national gas intensity
#   
#   ## CO2_R10.1 <- CO2_R10 %>%
#   ##   mutate(P_C = ifelse(Country_A == i, P_C, 0))
#   
#   CO2_OUT_10 <-   left_join(CO2_R10, OUT)%>%
#     mutate(OIL_OUT = ifelse(OUT != 0, OIL/OUT, 0))%>%
#     select(-OIL, - OUT)
#   
#   L_2_10_1 <- L_2_10 %>%
#     filter(Country_A == i)%>%
#     left_join(CO2_OUT_10, by = c("Country_A" = "Country_A", "Sector_A" = "Sector_A"))%>%
#     mutate(L = L*OIL_OUT)%>%
#     group_by(Country_B, Sector_B)%>%
#     summarise(L = sum(L))%>%
#     ungroup()%>%
#     mutate(Sector_B  = as.numeric(Sector_B),
#            Country_B = as.numeric(Country_B))%>%
#     mutate(Country_Host = i)
#   
#  oil_intensities_pure <- bind_rows(oil_intensities_pure, L_2_10_1)
#   
# }
# 
# write_csv(oil_intensities_pure, "OIL_Intensities_Pure_L.csv")

# oil_intensities_pure_2 <- oil_intensities_pure %>%
#   filter(Country_Host == Country_Number)

# 1.5   Embedded Emissions ####

# 1.5.1 Global Embedded Emissions ####

EE_1 <- data.frame()

for(i in c(1:160)){
  Y_R2.2 <- Y_R2 %>%
    select(Country_A, Sector, paste0("Country_", i))%>%
    rename(Country = paste0("Country_", i))%>%
    left_join(L_2, by = c("Sector" = "Sector_B", "Country_A" = "Country_B"))%>%
    mutate(Cons_Em = Country*L)%>%
    group_by(Sector)%>%
    summarise(Y_Cons  = sum(Country),
              Cons_Em = sum(Cons_Em))%>%
    ungroup()%>%
    mutate(Country = i)
  
  EE_1 <- bind_rows(EE_1, Y_R2.2)
  
}

# EE_1.1 <- EE_1 %>%
#   group_by(Country)%>%
#   summarise(Y_Cons_total = sum(Y_Cons),
#             Cons_Em      = sum(Cons_Em))%>%
#   ungroup()

# 1.5.2 National Embedded Emissions ####

EE_2 <- data.frame()

for(i in c(1:160)){
  Y_R2.2 <- Y_R2 %>%
    select(Country_A, Sector, paste0("Country_", i))%>%
    rename(Country = paste0("Country_", i))%>%
    left_join(national_carbon_intensities_2, by = c("Sector" = "Sector_B", "Country_A" = "Country_B"))%>%
    mutate(Cons_Em = Country*L)%>%
    group_by(Sector)%>%
    summarise(Y_Cons  = sum(Country),
              Cons_Em = sum(Cons_Em))%>%
    ungroup()%>%
    mutate(Country = i)
  
  EE_2 <- bind_rows(EE_2, Y_R2.2)
  
}

# EE_2.1 <- EE_2 %>%
#   group_by(Country)%>%
#   summarise(Y_Cons_total_national = sum(Y_Cons),
#             Cons_Em_national      = sum(Cons_Em))%>%
#   ungroup()

rm(national_carbon_intensities_2, Y_R2.2)

# 1.5.3 National Transport Only Emissions ####

EE_3 <- data.frame()

for(i in c(1:160)){
  Y_R2.3 <- Y_R2 %>%
    select(Country_A, Sector, paste0("Country_", i))%>%
    rename(Country = paste0("Country_", i))%>%
    left_join(transport_national_carbon_intensities_2, by = c("Sector" = "Sector_B", "Country_A" = "Country_B"))%>%
    mutate(Cons_Em = Country*L)%>%
    group_by(Sector)%>%
    summarise(Y_Cons  = sum(Country),
              Cons_Em = sum(Cons_Em))%>%
    ungroup()%>%
    mutate(Country = i)
  
  EE_3 <- bind_rows(EE_3, Y_R2.3)
  
}

# EE_3.1 <- EE_3 %>%
#   group_by(Country)%>%
#   summarise(Y_Cons_total_national = sum(Y_Cons),
#             Cons_Em_national      = sum(Cons_Em))%>%
#   ungroup()

rm(transport_national_carbon_intensities_2, Y_R2.3)

# 1.5.4 National Electricity Only Emissions ####

EE_4 <- data.frame()

for(i in c(1:160)){
  Y_R2.4 <- Y_R2 %>%
    select(Country_A, Sector, paste0("Country_", i))%>%
    rename(Country = paste0("Country_", i))%>%
    left_join(electricity_national_carbon_intensities_2, by = c("Sector" = "Sector_B", "Country_A" = "Country_B"))%>%
    mutate(Cons_Em = Country*L)%>%
    group_by(Sector)%>%
    summarise(Y_Cons  = sum(Country),
              Cons_Em = sum(Cons_Em))%>%
    ungroup()%>%
    mutate(Country = i)
  
  EE_4 <- bind_rows(EE_4, Y_R2.4)
  
}

# EE_4.1 <- EE_4 %>%
#   group_by(Country)%>%
#   summarise(Y_Cons_total_national = sum(Y_Cons),
#             Cons_Em_national      = sum(Cons_Em))%>%
#   ungroup()

rm(electricity_national_carbon_intensities_2, Y_R2.4)

# 1.5.5 Global Embedded Non-CO2-Emissions ####

# EE_5 <- data.frame()
# 
# for(i in c(1:141)){
#   Y_R2.5 <- Y_R2 %>%
#     select(Country_A, Sector, paste0("Country_", i))%>%
#     rename(Country = paste0("Country_", i))%>%
#     left_join(L_2_NON, by = c("Sector" = "Sector_B", "Country_A" = "Country_B"))%>%
#     mutate(Cons_Em_CH4  = Country*L_CH4,
#            Cons_Em_FGAS = Country*L_FGAS,
#            Cons_Em_N2O  = Country*L_N2O)%>%
#     group_by(Sector)%>%
#     summarise(Y_Cons  = sum(Country),
#               Cons_Em_CH4  = sum(Cons_Em_CH4),
#               Cons_Em_FGAS = sum(Cons_Em_FGAS),
#               Cons_Em_N2O  = sum(Cons_Em_N2O))%>%
#     ungroup()%>%
#     mutate(Country = i)
#   
#   EE_5 <- bind_rows(EE_5, Y_R2.5)
#   
# }
# 
# rm(Y_R2.5)

# 1.5.6 National Embedded Non-CO2-Emissions ####

# EE_6 <- data.frame()
# 
# for(i in c(1:141)){
#   Y_R2.6 <- Y_R2 %>%
#     select(Country_A, Sector, paste0("Country_", i))%>%
#     rename(Country = paste0("Country_", i))%>%
#     left_join(national_non_carbon_intensities_2, by = c("Sector" = "Sector_B", "Country_A" = "Country_B"))%>%
#     mutate(Cons_Em_CH4  = Country*L_CH4,
#            Cons_Em_FGAS = Country*L_FGAS,
#            Cons_Em_N2O  = Country*L_N2O)%>%
#     group_by(Sector)%>%
#     summarise(Y_Cons  = sum(Country),
#               Cons_Em_CH4  = sum(Cons_Em_CH4),
#               Cons_Em_FGAS = sum(Cons_Em_FGAS),
#               Cons_Em_N2O  = sum(Cons_Em_N2O))%>%
#     ungroup()%>%
#     mutate(Country = i)
#   
#   EE_6 <- bind_rows(EE_6, Y_R2.6)
#   
# }
# 
# rm(national_non_carbon_intensities_2, Y_R2.6)

# 1.5.7 National Gas Emissions ####

EE_7 <- data.frame()

for(i in c(1:160)){
 Y_R2.7 <- Y_R2 %>%
   select(Country_A, Sector, paste0("Country_", i))%>%
   rename(Country = paste0("Country_", i))%>%
   left_join(gas_national_carbon_intensities_2, by = c("Sector" = "Sector_B", "Country_A" = "Country_B"))%>%
   mutate(Cons_Em = Country*L)%>%
   group_by(Sector)%>%
   summarise(Y_Cons  = sum(Country),
             Cons_Em = sum(Cons_Em))%>%
   ungroup()%>%
   mutate(Country = i)
 
 EE_7 <- bind_rows(EE_7, Y_R2.7)
 
}

# EE_7.1 <- EE_7 %>%
#   group_by(Country)%>%
#   summarise(Y_Cons_total_national = sum(Y_Cons),
#             Cons_Em_national      = sum(Cons_Em))%>%
#   ungroup()

rm(gas_national_carbon_intensities_2, Y_R2.7)

# 1.5.8 Global Gas Input or Expenditures ####

# EE_8 <- data.frame()
# 
# for(i in c(1:141)){
#   Y_R2.8 <- Y_R2 %>%
#     select(Country_A, Sector, paste0("Country_", i))%>%
#     rename(Country = paste0("Country_", i))%>%
#     left_join(gas_intensities_pure_2, by = c("Sector" = "Sector_B", "Country_A" = "Country_B"))%>%
#     mutate(Cons_Em = Country*L)%>%
#     group_by(Sector)%>%
#     summarise(Y_Cons = sum(Country),
#               Cons_Em = sum(Cons_Em))%>%
#     ungroup()%>%
#     mutate(Country = i)
#   
#   EE_8 <- bind_rows(EE_8, Y_R2.8)
# }
# 
# # EE_8.1 <- EE_8 %>%
# #   group_by(Country)%>%
# #   summarise(Y_Cons_total_national = sum(Y_Cons),
# #             Cons_Em_national      = sum(Cons_Em))%>%
# #   ungroup()
# 
# rm(gas_intensities_pure_2, Y_R2.8)

# 1.5.9 Global Coal Input or Expenditures ####

# EE_9 <- data.frame()
# 
# for(i in c(1:141)){
#   Y_R2.9 <- Y_R2 %>%
#     select(Country_A, Sector, paste0("Country_", i))%>%
#     rename(Country = paste0("Country_", i))%>%
#     left_join(coal_intensities_pure_2, by = c("Sector" = "Sector_B", "Country_A" = "Country_B"))%>%
#     mutate(Cons_Em = Country*L)%>%
#     group_by(Sector)%>%
#     summarise(Y_Cons = sum(Country),
#               Cons_Em = sum(Cons_Em))%>%
#     ungroup()%>%
#     mutate(Country = i)
#   
#   EE_9 <- bind_rows(EE_9, Y_R2.9)
# }
# 
# # EE_9.1 <- EE_9 %>%
# #   group_by(Country)%>%
# #   summarise(Y_Cons_total_national = sum(Y_Cons),
# #             Cons_Em_national      = sum(Cons_Em))%>%
# #   ungroup()
# 
# rm(coal_intensities_pure_2, Y_R2.9)

# 1.5.10 Global Petroleum Input or Expenditures ####

# EE_10 <- data.frame()
# 
# for(i in c(1:141)){
#   Y_R2.10 <- Y_R2 %>%
#     select(Country_A, Sector, paste0("Country_", i))%>%
#     rename(Country = paste0("Country_", i))%>%
#     left_join(p_c_intensities_pure_2, by = c("Sector" = "Sector_B", "Country_A" = "Country_B"))%>%
#     mutate(Cons_Em = Country*L)%>%
#     group_by(Sector)%>%
#     summarise(Y_Cons = sum(Country),
#               Cons_Em = sum(Cons_Em))%>%
#     ungroup()%>%
#     mutate(Country = i)
#   
#   EE_10 <- bind_rows(EE_10, Y_R2.10)
# }
# 
# # EE_10.1 <- EE_10 %>%
# #   group_by(Country)%>%
# #   summarise(Y_Cons_total_national = sum(Y_Cons),
# #             Cons_Em_national      = sum(Cons_Em))%>%
# #   ungroup()
# 
# rm(p_c_intensities_pure_2, Y_R2.10)

# 1.5.11 Global Oil Input or Expenditures ####

# EE_11 <- data.frame()
# 
# for(i in c(1:141)){
#   Y_R2.11 <- Y_R2 %>%
#     select(Country_A, Sector, paste0("Country_", i))%>%
#     rename(Country = paste0("Country_", i))%>%
#     left_join(oil_intensities_pure_2, by = c("Sector" = "Sector_B", "Country_A" = "Country_B"))%>%
#     mutate(Cons_Em = Country*L)%>%
#     group_by(Sector)%>%
#     summarise(Y_Cons = sum(Country),
#               Cons_Em = sum(Cons_Em))%>%
#     ungroup()%>%
#     mutate(Country = i)
#   
#   EE_11 <- bind_rows(EE_11, Y_R2.11)
# }
# 
# # EE_11.1 <- EE_11 %>%
# #   group_by(Country)%>%
# #   summarise(Y_Cons_total_national = sum(Y_Cons),
# #             Cons_Em_national      = sum(Cons_Em))%>%
# #   ungroup()
# 
# rm(oil_intensities_pure_2, Y_R2.11)

# 1.6   Consumption Data ####

library(openxlsx)

# Household Consumption Data

# cons_dom <- read_delim("VDPM.csv", ",", escape_double = FALSE, trim_ws = TRUE)
# cons_int <- read_delim("VIPM.csv", ",", escape_double = FALSE, trim_ws = TRUE)

# write.csv(cons_dom, "VDPM.csv", col.names = FALSE, row.names = FALSE)
# write.csv(cons_int, "VIPM.csv", col.names = FALSE, row.names = FALSE)

cons_dom_1 <- cons_dom %>%
  # separate(VDPM, c("Sector", "Name"), sep = " ")%>%
  rename(Cons_Dom = X3)
  # filter(Sector != "Total")%>%
  # select(-Total)%>%
  # pivot_longer(c(3:143), names_to = "Country", values_to = "Cons_Dom")%>%
  # separate(Country, c("Country", "Name"), sep = " ")

cons_int_1 <- cons_int %>%
  #separate(VIPM, c("Sector", "Name"), sep = " ")%>%
  rename(Cons_Int = X3)
  # filter(Sector != "Total")%>%
  # select(-Total)%>%
  # pivot_longer(c(3:143), names_to = "Country", values_to = "Cons_Int")%>%
  # separate(Country, c("Country", "Name"), sep = " ")

# Countries <- select(cons_int_1, Country, Name)%>%
#   distinct()
# 
# Countries_1 <- read.xlsx("Regions_Overview.xlsx")%>%
#   select('3-letter', '30-letter')%>%
#   rename(Name = '3-letter', Country_Name = '30-letter')
# 
# Countries <- left_join(Countries, Countries_1)
# 
# write_csv(Countries, "Countries_Overview.csv")

cons_tot <- left_join(cons_dom_1, cons_int_1, by = c("X1", "X2"))%>%
  mutate(Cons_Tot = Cons_Dom + Cons_Int)%>%
  rename(Sector = X1, Country = X2)

regions <- distinct(cons_tot, Country)%>%
  mutate(country_number = 1:n())

sectors <- distinct(cons_tot, Sector)%>%
  mutate(sector_number = 1:n())

cons_tot_0 <- select(cons_tot, Sector, Cons_Tot, Country)%>%
  left_join(regions, by = "Country")%>%
  left_join(sectors, by = "Sector")%>%
  # mutate(Sector = as.numeric(Sector))%>%
  filter(country_number == Country_Number)%>%
  select(-Country, -country_number)

# cons_agg <- cons_tot %>%
#   group_by(Country)%>%
#   summarise(Cons_Dom = sum(Cons_Dom),
#             Cons_Int = sum(Cons_Int),
#             Cons_Tot = sum(Cons_Tot))%>%
#   ungroup()

rm(cons_dom_1, cons_int_1)

# 1.7   Load Direct Emissions ####

# CO2_dir_imp <- read_delim("MIP_all.csv", ";", escape_double = FALSE, col_names = FALSE, trim_ws = TRUE)
# CO2_dir_dom <- read_delim("MDP_all.csv", ";", escape_double = FALSE, col_names = FALSE, trim_ws = TRUE)

# colnames(CO2_dir_imp) <- paste0("Country_", 1:ncol(CO2_dir_imp))
# colnames(CO2_dir_dom) <- paste0("Country_", 1:ncol(CO2_dir_dom))

# GTAP_0 <- data.frame("GTAP_0" = c(15,16,17,32,47))
# 
# CO2_dir_imp_1 <- CO2_dir_imp %>%
#   bind_cols(GTAP_0)
# 
# CO2_dir_dom_1 <- CO2_dir_dom %>%
#   bind_cols(GTAP_0)

CO2_dir <- bind_rows(CO2_dir_imp, CO2_dir_dom)%>%
  group_by(X1, X2)%>%
  summarise(CO2_direct = sum(X3))%>%
  ungroup()%>%
  left_join(regions, by = c(X2 ="Country"))%>%
  left_join(sectors, by = c("X1" = "Sector"))%>%
  filter(country_number == Country_Number)%>%
  select(-X2, -country_number)%>%
  arrange(sector_number)%>%
  mutate(Gas_pure_direct  = c(0,0,1,0,1),
         Coal_pure_direct = c(1,0,0,0,0),
         P_C_pure_direct  = c(0,0,0,1,0),
         Oil_pure_direct  = c(0,1,0,0,0))

# CO2_dir <- bind_rows(CO2_dir_imp_1, CO2_dir_dom_1)%>%
#   pivot_longer(Country_1:Country_141, names_to = "Country", names_prefix = "Country_", values_to = "CO2_direct")%>%
#   group_by(Country, GTAP_0)%>%
#   summarise(CO2_direct = sum(CO2_direct))%>%
#   ungroup()%>%
#   mutate(Country = as.numeric(Country))%>%
#   arrange(Country)%>%
#   filter(Country == Country_Number)%>%
#   select(-Country)%>%
#   mutate(Gas_pure_direct  = c(0,0,1,0,1),
#          Coal_pure_direct = c(1,0,0,0,0),
#          P_C_pure_direct  = c(0,0,0,1,0),
#          Oil_pure_direct  = c(0,1,0,0,0))

# rm(CO2_dir_dom_1, CO2_dir_imp_1)

# 1.7.1 Load Direct Non-CO2 Emissions ####

# NON_CO2_dir <- read_csv("Non-CO2/Household_2014_NonCO2_MtCO2.csv")%>%
#   select(-Sector_Name, -Country_Name)%>%
#   filter(Sector != 66)%>%
#   rename(GTAP_0 = Sector)%>%
#   filter(Country == Country_Number)%>%
#   select(-Country)

# 1 B   Electricity Calculations II ####

# cons_tot_country <- cons_tot %>%
#   filter(Country == Country_Number)%>%
#   select(Sector, Cons_Tot)%>%
#   group_by(Sector)%>%
#   summarise(Cons_Tot = sum(Cons_Tot))%>%
#   ungroup()
# 
# Y_OI_2 <- Y_OI %>%
#   group_by(Sector)%>%
#   summarise(Y_Tot = sum(Country))%>%
#   ungroup()
# 
# EE_1.0 <- EE_1 %>%
#   filter(Country == Country_Number)
# 
# # CON_2 <- left_join(cons_tot_country, Y_OI_2)%>%
# #   mutate(Sector = as.numeric(Sector))%>%
# #   left_join(EE_1.0)%>%
# #   mutate(CON_0 = Cons_Tot*Cons_Em/Y_Tot)%>%
# #   select(Sector, CON_0)%>%
# #   arrange(Sector)
# 
# ELY_3 <- left_join(cons_tot_country, Y_OI_2)%>%
#   mutate(Sector = as.numeric(Sector))%>%
#   left_join(ELY_0,  by = c("Sector" = "Sector_B"))%>%
#   mutate(ELY_0 = ELY*Cons_Tot/Y_Tot,
#          ELY_per_sec = ELY_0*ELY_2/ELY_1)%>%
#   select(Sector, ELY_0, ELY_per_sec)%>%
#   arrange(Sector)
# 
# rm(Y_OI, Y_OI_2, ELY_0, cons_tot, cons_tot_country)

# 2     Compiling Output #### 

# Add direct Emissions

EE_1.0 <- EE_1 %>%
  filter(Country == Country_Number)%>%
  select(Sector, Cons_Em)%>%
  rename(Indir_Emissions_Consumed = Cons_Em)

EE_2.0 <- EE_2 %>%
  filter(Country == Country_Number)%>%
  select(Sector, Cons_Em)%>%
  rename(Indir_Emissions_Consumed_National = Cons_Em)

EE_3.0 <- EE_3 %>%
  filter(Country == Country_Number)%>%
  select(Sector, Cons_Em)%>%
  rename(Indir_Emissions_Consumed_National_Transport = Cons_Em)

EE_4.0 <- EE_4 %>%
  filter(Country == Country_Number)%>%
  select(Sector, Cons_Em)%>%
  rename(Indir_Emissions_Consumed_National_Electricity = Cons_Em)

# EE_5.0 <- EE_5 %>%
#   filter(Country == Country_Number)%>%
#   select(Sector, starts_with("Cons_Em"))%>%
#   rename(Indir_Cons_Em_CH4  = Cons_Em_CH4,
#          Indir_Cons_Em_FGAS = Cons_Em_FGAS,
#          Indir_Cons_Em_N2O  = Cons_Em_N2O)
# 
# EE_6.0 <- EE_6 %>%
#   filter(Country == Country_Number)%>%
#   select(Sector, starts_with("Cons_Em"))%>%
#   rename(Indir_National_Cons_Em_CH4  = Cons_Em_CH4,
#          Indir_National_Cons_Em_FGAS = Cons_Em_FGAS,
#          Indir_National_Cons_Em_N2O  = Cons_Em_N2O)
# 
EE_7.0 <- EE_7 %>%
 filter(Country == Country_Number)%>%
 select(Sector, Cons_Em)%>%
 rename(Indir_Emissions_Consumed_Gas = Cons_Em)
# 
# EE_8.0 <- EE_8 %>%
#   filter(Country == Country_Number)%>%
#   select(Sector, Cons_Em)%>%
#   rename(Indir_Gas_Consumed = Cons_Em)
# 
# EE_9.0 <- EE_9 %>%
#   filter(Country == Country_Number)%>%
#   select(Sector, Cons_Em)%>%
#   rename(Indir_Coal_Consumed = Cons_Em)
# 
# EE_10.0 <- EE_10 %>%
#   filter(Country == Country_Number)%>%
#   select(Sector, Cons_Em)%>%
#   rename(Indir_P_C_Consumed = Cons_Em)
# 
# EE_11.0 <- EE_11 %>%
#   filter(Country == Country_Number)%>%
#   select(Sector, Cons_Em)%>%
#   rename(Indir_Oil_Consumed = Cons_Em)

rm(EE_1, EE_2, EE_3, EE_4, 
   # EE_5, EE_6, 
   EE_7
   # EE_8, EE_9, EE_10, EE_11
   )

df_final <- expand.grid(GTAP = c(1:65))%>%
  left_join(EE_1.0, by = c("GTAP" = "Sector"))%>%
  left_join(EE_2.0, by = c("GTAP" = "Sector"))%>%
  left_join(EE_3.0, by = c("GTAP" = "Sector"))%>%
  left_join(EE_4.0, by = c("GTAP" = "Sector"))%>%
  #left_join(EE_5.0, by = c("GTAP" = "Sector"))%>%
  #left_join(EE_6.0, by = c("GTAP" = "Sector"))%>%
  left_join(EE_7.0, by = c("GTAP" = "Sector"))%>%
  #left_join(EE_8.0, by = c("GTAP" = "Sector"))%>%
  #left_join(EE_9.0, by = c("GTAP" = "Sector"))%>%
  #left_join(EE_10.0, by = c("GTAP" = "Sector"))%>%
  #left_join(EE_11.0, by = c("GTAP" = "Sector"))%>%
  #left_join(ELY_3,  by = c("GTAP" = "Sector"))%>%
  left_join(cons_tot_0,  by = c("GTAP" = "sector_number"))%>%
  left_join(CO2_dir,     by = c("GTAP" = "sector_number"))%>%
  #left_join(NON_CO2_dir, by = c("GTAP" = "GTAP_0"))%>%
  mutate(CO2_direct       = ifelse(is.na(CO2_direct),0, CO2_direct),
         Gas_pure_direct  = ifelse(is.na(Gas_pure_direct),  0, Gas_pure_direct),
         Coal_pure_direct = ifelse(is.na(Coal_pure_direct), 0, Coal_pure_direct),
         P_C_pure_direct  = ifelse(is.na(P_C_pure_direct),  0, P_C_pure_direct),
         Oil_pure_direct  = ifelse(is.na(Oil_pure_direct),  0, Oil_pure_direct))%>%
  rename(Total_HH_Consumption_MUSD = Cons_Tot, 
         #Electricity_MUSD = ELY_0, 
         #Electricity_GWh = ELY_per_sec
         )%>%
  mutate(CO2_Mt               = Indir_Emissions_Consumed + CO2_direct,
         CO2_Mt_within        = Indir_Emissions_Consumed_National + CO2_direct,
         CO2_Mt_Electricity   = Indir_Emissions_Consumed_National_Electricity,
         CO2_Mt_Transport     = ifelse(GTAP == 32, Indir_Emissions_Consumed_National_Transport + CO2_direct, Indir_Emissions_Consumed_National_Transport),
         CO2_Mt_Gas           = ifelse(GTAP == 17 | GTAP == 47, Indir_Emissions_Consumed_Gas + CO2_direct, Indir_Emissions_Consumed_Gas),
         # CO2_Mt_Gas_indir     = Indir_Emissions_Consumed_Gas,
         # CO2_Mt_Gas_direct    = ifelse(GTAP == 17 | GTAP == 47, CO2_direct, 0),
         # GAS_USD_Gas          = ifelse(GTAP == 17 | GTAP == 47, Indir_Gas_Consumed + Gas_pure_direct, Indir_Gas_Consumed),
         # GAS_USD_Gas_indir    = Indir_Gas_Consumed,
         # GAS_USD_Gas_direct   = Gas_pure_direct,
         
         # COAL_USD_Coal        = ifelse(GTAP == 15, Indir_Coal_Consumed + Coal_pure_direct, Indir_Coal_Consumed),
         # COAL_USD_Coal_indir  = Indir_Coal_Consumed,
         # COAL_USD_Coal_direct = Coal_pure_direct,
         
         # P_C_USD_p_c          = ifelse(GTAP == 32, Indir_P_C_Consumed + P_C_pure_direct, Indir_P_C_Consumed),
         # P_C_USD_p_c_indir    = Indir_P_C_Consumed,
         # P_C_USD_p_c_direct   = P_C_pure_direct,
         
         # OIL_USD_oil          = ifelse(GTAP == 16, Indir_Oil_Consumed + Oil_pure_direct, Indir_Oil_Consumed),
         
         # CH4_MtCO2            = Indir_Cons_Em_CH4  + CH4,
         # FGAS_MtCO2           = Indir_Cons_Em_FGAS + FGAS,
         # N2O_MtCO2            = Indir_Cons_Em_N2O  + N2O,
         # CH4_MtCO2_within     = Indir_National_Cons_Em_CH4  + CH4,
         # FGAS_MtCO2_within    = Indir_National_Cons_Em_FGAS + FGAS,
         # N2O_MtCO2_within     = Indir_National_Cons_Em_N2O  + N2O,
         
         CO2_direct           = CO2_direct
         # CH4_direct           = CH4,
         # FGAS_direct          = FGAS,
         # N2O_direct           = N2O
         )

df_final_out <- df_final %>%
  select(GTAP, CO2_Mt, CO2_Mt_within, CO2_Mt_Electricity, CO2_Mt_Transport, CO2_Mt_Gas,
         CO2_direct,
         #Electricity_MUSD, Electricity_GWh,
         #CH4_MtCO2, FGAS_MtCO2, N2O_MtCO2, CH4_MtCO2_within, FGAS_MtCO2_within, N2O_MtCO2_within, 
         #CO2_Mt_Gas, CO2_Mt_Gas_indir, CO2_Mt_Gas_direct,
         #GAS_USD_Gas, GAS_USD_Gas_indir, GAS_USD_Gas_direct,
         #COAL_USD_Coal, COAL_USD_Coal_indir, COAL_USD_Coal_direct,
         #P_C_USD_p_c, P_C_USD_p_c_indir, P_C_USD_p_c_direct, OIL_USD_oil,
         #CO2_direct, CH4_direct, FGAS_direct, N2O_direct,
         Total_HH_Consumption_MUSD)

 list_0[[length(list_0) + 1]] <- df_final_out
 
 print(k_0)
  
 }

Countries_new <- read.xlsx("../0_Data/2_IO Data/GTAP_11_MRIO/Countries_Overview.xlsx")%>%
  mutate(Name = tolower(Code),
         Country_Name = Description,
         Country = Number)%>%
  select(Name, Country_Name, Country)%>%
  mutate(Country_Name = ifelse(Country_Name == "Hong Kong, Special Administrative Region of China", "Hong Kong", 
                               ifelse(Country_Name == "Venezuela (Bolivarian Republic of)", "Venezuela", 
                                      ifelse(Country_Name == "Rest of European Free Trade Association", "Rest of European FTA", 
                                             ifelse(Country_Name == "Palestineian Territory, Occupied", "Palestina", 
                                                    ifelse(Country_Name == "Democratic Republic of the Congo", "DR Congo", 
                                                           ifelse(Country_Name == "Rest of South and Central Africa", "Rest of SC Africa", 
                                                                  ifelse(Country_Name == "Rest of South African Customs Union", "Reso of SA CU", 
                                                                         ifelse(Country_Name == "Viet Nam", "Vietnam",
                                                                                ifelse(Country_Name == "United States of America", "USA",
                                                                                       ifelse(Country_Name == "Rest of Caribbean", "Rest of the Caribbean",
                                                                                              ifelse(Country_Name == "Slovakia", "Slovak Republic",
                                                                                                     ifelse(Country_Name == "Côte d'Ivoire", "Cote dIvoire", 
                                                                                                            ifelse(Country_Name == "Russian Federation", "Russia", Country_Name))))))))))))))

names(list_0) <- Countries_new$Country_Name[1:length(list_0)]

if(year_0 != "2017"){write.xlsx(list_0, sprintf("../0_Data/2_IO Data/GTAP_11_MRIO/GTAP11A_%s/Carbon_Intensities_Full_All_Gas_%s.xlsx", year_0, year_0), append = TRUE, colNames = TRUE)}
if(year_0 == "2017"){write.xlsx(list_0, "../0_Data/2_IO Data/GTAP_11_MRIO/GTAP11B_2017/Carbon_Intensities_Full_All_Gas.xlsx", append = TRUE, colNames = TRUE)}

# write_csv(df_final_out, sprintf("Carbon_Intensities_%s.csv", Country.Name))

# Additional transformation to carbon intensities

# GTAP_code            <- read_delim("GTAP10.csv", ";", escape_double = FALSE, trim_ws = TRUE)
# 
# carbon_intensities   <- left_join(GTAP_code, df_final_out, by = c("Number"="GTAP"))%>%
#   select(-Explanation, - Number)%>%
#   mutate(GTAP = ifelse(GTAP == "gas" | GTAP == "gdt", "gasgdt", GTAP))%>%
#   group_by(GTAP)%>%
#   summarise(across(CO2_Mt:Total_HH_Consumption_MUSD, ~ sum(.)))%>%
#   ungroup()%>%
#   mutate(CO2_t_per_dollar_global       = CO2_Mt/            Total_HH_Consumption_MUSD,
#          CO2_t_per_dollar_national     = CO2_Mt_within/     Total_HH_Consumption_MUSD,
#          CO2_t_per_dollar_electricity  = CO2_Mt_Electricity/Total_HH_Consumption_MUSD,
#          CO2_t_per_dollar_transport    = CO2_Mt_Transport/  Total_HH_Consumption_MUSD,
#          CH4_tCO2_per_dollar_global    = CH4_MtCO2/         Total_HH_Consumption_MUSD,
#          FGAS_tCO2_per_dollar_global   = FGAS_MtCO2/        Total_HH_Consumption_MUSD,
#          N2O_tCO2_per_dollar_global    = N2O_MtCO2/         Total_HH_Consumption_MUSD,
#          CH4_tCO2_per_dollar_national   = CH4_MtCO2_within/  Total_HH_Consumption_MUSD,
#          FGAS_tCO2_per_dollar_national = FGAS_MtCO2_within/ Total_HH_Consumption_MUSD,
#          N2O_tCO2_per_dollar_national  = N2O_MtCO2_within/  Total_HH_Consumption_MUSD)%>%
#   select(GTAP, starts_with("CO2_t"), ends_with("global"), ends_with("national"))
# 
# write.xlsx(carbon_intensities, sprintf("Carbon_Intensities_%s.xlsx", Country.Name))


# 3 Compare GTAP 10 and GTAP 11 ####

Countries_new <- read.xlsx("../0_Data/2_IO Data/GTAP_11_MRIO/Countries_Overview.xlsx")%>%
  mutate(Name = tolower(Code),
         Country_Name = Description,
         Country = Number)%>%
  select(Name, Country_Name, Country)%>%
  mutate(Country_Name = ifelse(Country_Name == "Hong Kong, Special Administrative Region of China", "Hong Kong", 
                               ifelse(Country_Name == "Venezuela (Bolivarian Republic of)", "Venezuela", 
                                      ifelse(Country_Name == "Rest of European Free Trade Association", "Rest of European FTA", 
                                             ifelse(Country_Name == "Palestineian Territory, Occupied", "Palestina", 
                                                    ifelse(Country_Name == "Democratic Republic of the Congo", "DR Congo", 
                                                           ifelse(Country_Name == "Rest of South and Central Africa", "Rest of SC Africa", 
                                                                  ifelse(Country_Name == "Rest of South African Customs Union", "Reso of SA CU", 
                                                                         ifelse(Country_Name == "Viet Nam", "Vietnam",
                                                                                ifelse(Country_Name == "United States of America", "USA",
                                                                                       ifelse(Country_Name == "Rest of Caribbean", "Rest of the Caribbean",
                                                                                              ifelse(Country_Name == "Slovakia", "Slovak Republic",
                                                                                                     ifelse(Country_Name == "Côte d'Ivoire", "Cote dIvoire", 
                                                                                                            ifelse(Country_Name == "Russian Federation", "Russia", Country_Name))))))))))))))


carbon_intensities_0 <- data.frame()

for (i in Countries_new$Country_Name){
  
  if(i %in% excel_sheets("../0_Data/2_IO Data/GTAP_10_MRIO/Carbon_Intensities_Full_All_incl_Gas_Coal_PC_direct.xlsx")){
    carbon_intensities_10 <- read.xlsx("../0_Data/2_IO Data/GTAP_10_MRIO/Carbon_Intensities_Full_All_incl_Gas_Coal_PC_direct.xlsx", sheet = i)%>%
      mutate(GTAP_type    = "10",
             Country = i)
  }else {carbon_intensities_10 <- data.frame()}
  
  
  
  if(i %in% excel_sheets("../0_Data/2_IO Data/GTAP_11_MRIO/Carbon_Intensities_Full_All.xlsx")){
    carbon_intensities_11 <- read.xlsx("../0_Data/2_IO Data/GTAP_11_MRIO/Carbon_Intensities_Full_All.xlsx", sheet = i)%>%
      mutate(GTAP_type = "11",
             Country = i)
  }else {carbon_intensities_11 <- data.frame()}
  
  
  carbon_intensities <- bind_rows(carbon_intensities_10, carbon_intensities_11)%>%
    select(Country, GTAP_type, GTAP:CO2_Mt_Transport, CO2_direct, Total_HH_Consumption_MUSD)
  
  carbon_intensities_0 <- bind_rows(carbon_intensities_0, carbon_intensities)
  
}

GTAP_code            <- read_delim("../0_Data/2_IO Data/GTAP_10_MRIO/GTAP10.csv", ";", escape_double = FALSE, trim_ws = TRUE, show_col_types = FALSE)

carbon_intensities_1 <- left_join(GTAP_code, carbon_intensities_0, by = c("Number"="GTAP"))%>%
  select(-Explanation, -Number)%>%
  mutate(GTAP = ifelse(GTAP == "gas" | GTAP == "gdt", "gasgdt", GTAP))%>%
  group_by(GTAP, Country, GTAP_type)%>%
  summarise(across(CO2_Mt:Total_HH_Consumption_MUSD, ~ sum(.)))%>%
  ungroup()%>%
  mutate(CO2_t_per_dollar_global      = CO2_Mt/            Total_HH_Consumption_MUSD,
         CO2_t_per_dollar_national    = CO2_Mt_within/     Total_HH_Consumption_MUSD,
         CO2_t_per_dollar_electricity = CO2_Mt_Electricity/Total_HH_Consumption_MUSD,
         CO2_t_per_dollar_transport   = CO2_Mt_Transport/  Total_HH_Consumption_MUSD)

for (sector in distinct(carbon_intensities_1, GTAP)$GTAP){
  print(sector)
  
  carbon_intensities_1.1 <- carbon_intensities_1 %>%
    filter(GTAP == sector)%>%
    mutate(CO2_t_per_dollar_national = ifelse(Total_HH_Consumption_MUSD == 0,0,CO2_t_per_dollar_national))%>%
    arrange(desc(GTAP_type), CO2_t_per_dollar_national)%>%
    mutate(number = 1:n())%>%
    group_by(Country)%>%
    mutate(order = min(number))%>%
    ungroup()
  
  P_1 <- ggplot(carbon_intensities_1.1)+
    geom_line(aes(x = reorder(Country,order), y = CO2_t_per_dollar_national), size = 0.1)+
    geom_point(aes(x = reorder(Country,order), y = CO2_t_per_dollar_national, fill = GTAP_type), shape = 21)+
    scale_fill_viridis_d()+
    #scale_y_log10()+
    theme_bw()+
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 6),
          panel.grid = element_blank())+
    coord_cartesian(ylim = c(0,min(max(carbon_intensities_1.1$CO2_t_per_dollar_national*1.05),1)))+
    ylab("Country")+
    xlab("Carbon intensity")+
    ggtitle(paste0("Sector: ", sector))
  
  jpeg(sprintf("../0_Data/2_IO Data/GTAP_11_MRIO/Comparing GTAP 11 and GTAP10/Comparison_%s_rev_scale.jpg", sector), width = 30, height = 20, unit = "cm", res = 200)
  print(P_1)
  dev.off()
  
}

# Test matching of names

names_11 <- data.frame(GTAP11 = excel_sheets("../0_Data/2_IO Data/GTAP_11_MRIO/Carbon_Intensities_Full_All.xlsx"))%>%
  mutate(Type11 = 1)
names_10 <- data.frame(GTAP10 = excel_sheets("../0_Data/2_IO Data/GTAP_10_MRIO/Carbon_Intensities_Full_All_incl_Gas_Coal_PC_direct.xlsx"))%>%
  mutate(Type10 = 1)

names <- full_join(names_11, names_10, by = c("GTAP11" = "GTAP10"))

# 3.1 Compare GTAP 10 and GTAP 11 - 2014  ####

Countries_new <- read.xlsx("../0_Data/2_IO Data/GTAP_11_MRIO/Countries_Overview.xlsx")%>%
  mutate(Name = tolower(Code),
         Country_Name = Description,
         Country = Number)%>%
  select(Name, Country_Name, Country)%>%
  mutate(Country_Name = ifelse(Country_Name == "Hong Kong, Special Administrative Region of China", "Hong Kong", 
                               ifelse(Country_Name == "Venezuela (Bolivarian Republic of)", "Venezuela", 
                                      ifelse(Country_Name == "Rest of European Free Trade Association", "Rest of European FTA", 
                                             ifelse(Country_Name == "Palestineian Territory, Occupied", "Palestina", 
                                                    ifelse(Country_Name == "Democratic Republic of the Congo", "DR Congo", 
                                                           ifelse(Country_Name == "Rest of South and Central Africa", "Rest of SC Africa", 
                                                                  ifelse(Country_Name == "Rest of South African Customs Union", "Reso of SA CU", 
                                                                         ifelse(Country_Name == "Viet Nam", "Vietnam",
                                                                                ifelse(Country_Name == "United States of America", "USA",
                                                                                       ifelse(Country_Name == "Rest of Caribbean", "Rest of the Caribbean",
                                                                                              ifelse(Country_Name == "Slovakia", "Slovak Republic",
                                                                                                     ifelse(Country_Name == "Côte d'Ivoire", "Cote dIvoire", 
                                                                                                            ifelse(Country_Name == "Russian Federation", "Russia", Country_Name))))))))))))))


carbon_intensities_0 <- data.frame()

for (i in Countries_new$Country_Name){
  
  if(i %in% excel_sheets("../0_Data/2_IO Data/GTAP_10_MRIO/Carbon_Intensities_Full_All_incl_Gas_Coal_PC_direct.xlsx")){
    carbon_intensities_10 <- read.xlsx("../0_Data/2_IO Data/GTAP_10_MRIO/Carbon_Intensities_Full_All_incl_Gas_Coal_PC_direct.xlsx", sheet = i)%>%
      mutate(GTAP_type    = "10",
             Country = i)
  }else {carbon_intensities_10 <- data.frame()}
  
  
  
  if(i %in% excel_sheets("../0_Data/2_IO Data/GTAP_11_MRIO/GTAP11A_2014/Carbon_Intensities_Full_All_Gas_2014.xlsx")){
    carbon_intensities_11 <- read.xlsx("../0_Data/2_IO Data/GTAP_11_MRIO/GTAP11A_2014/Carbon_Intensities_Full_All_Gas_2014.xlsx", sheet = i)%>%
      mutate(GTAP_type = "11",
             Country = i)
  }else {carbon_intensities_11 <- data.frame()}
  
  
  carbon_intensities <- bind_rows(carbon_intensities_10, carbon_intensities_11)%>%
    select(Country, GTAP_type, GTAP:CO2_Mt_Transport, CO2_direct, Total_HH_Consumption_MUSD)
  
  carbon_intensities_0 <- bind_rows(carbon_intensities_0, carbon_intensities)
  
}

GTAP_code            <- read_delim("../0_Data/2_IO Data/GTAP_10_MRIO/GTAP10.csv", ";", escape_double = FALSE, trim_ws = TRUE, show_col_types = FALSE)

carbon_intensities_1 <- left_join(GTAP_code, carbon_intensities_0, by = c("Number"="GTAP"))%>%
  select(-Explanation, -Number)%>%
  mutate(GTAP = ifelse(GTAP == "gas" | GTAP == "gdt", "gasgdt", GTAP))%>%
  group_by(GTAP, Country, GTAP_type)%>%
  summarise(across(CO2_Mt:Total_HH_Consumption_MUSD, ~ sum(.)))%>%
  ungroup()%>%
  mutate(CO2_t_per_dollar_global      = CO2_Mt/            Total_HH_Consumption_MUSD,
         CO2_t_per_dollar_national    = CO2_Mt_within/     Total_HH_Consumption_MUSD,
         CO2_t_per_dollar_electricity = CO2_Mt_Electricity/Total_HH_Consumption_MUSD,
         CO2_t_per_dollar_transport   = CO2_Mt_Transport/  Total_HH_Consumption_MUSD)

for (sector in distinct(carbon_intensities_1, GTAP)$GTAP){
  print(sector)
  
  carbon_intensities_1.1 <- carbon_intensities_1 %>%
    filter(GTAP == sector)%>%
    mutate(CO2_t_per_dollar_national = ifelse(Total_HH_Consumption_MUSD == 0,0,CO2_t_per_dollar_national))%>%
    arrange(desc(GTAP_type), CO2_t_per_dollar_national)%>%
    mutate(number = 1:n())%>%
    group_by(Country)%>%
    mutate(order = min(number))%>%
    ungroup()
  
  P_1 <- ggplot(carbon_intensities_1.1)+
    geom_line(aes(x = reorder(Country,order), y = CO2_t_per_dollar_national), size = 0.1)+
    geom_point(aes(x = reorder(Country,order), y = CO2_t_per_dollar_national, fill = GTAP_type), shape = 21)+
    scale_fill_viridis_d()+
    #scale_y_log10()+
    theme_bw()+
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 6),
          panel.grid = element_blank())+
    coord_cartesian(ylim = c(0,min(max(carbon_intensities_1.1$CO2_t_per_dollar_national*1.05),1)))+
    ylab("Country")+
    xlab("Carbon intensity")+
    ggtitle(paste0("Sector: ", sector))
  
  jpeg(sprintf("../0_Data/2_IO Data/GTAP_11_MRIO/Comparing GTAP 11 and GTAP10/Comparison_%s_rev_scale.jpg", sector), width = 30, height = 20, unit = "cm", res = 200)
  print(P_1)
  dev.off()
  
}

# Test matching of names

names_11 <- data.frame(GTAP11 = excel_sheets("../0_Data/2_IO Data/GTAP_11_MRIO/Carbon_Intensities_Full_All.xlsx"))%>%
  mutate(Type11 = 1)
names_10 <- data.frame(GTAP10 = excel_sheets("../0_Data/2_IO Data/GTAP_10_MRIO/Carbon_Intensities_Full_All_incl_Gas_Coal_PC_direct.xlsx"))%>%
  mutate(Type10 = 1)

names <- full_join(names_11, names_10, by = c("GTAP11" = "GTAP10"))

# 4 Evaluating GTAP11 ####

Countries_new <- read.xlsx("../0_Data/2_IO Data/GTAP_11_MRIO/Countries_Overview.xlsx")%>%
  mutate(Name = tolower(Code),
         Country_Name = Description,
         Country = Number)%>%
  select(Name, Country_Name, Country)%>%
  mutate(Country_Name = ifelse(Country_Name == "Hong Kong, Special Administrative Region of China", "Hong Kong", 
                               ifelse(Country_Name == "Venezuela (Bolivarian Republic of)", "Venezuela", 
                                      ifelse(Country_Name == "Rest of European Free Trade Association", "Rest of European FTA", 
                                             ifelse(Country_Name == "Palestineian Territory, Occupied", "Palestina", 
                                                    ifelse(Country_Name == "Democratic Republic of the Congo", "DR Congo", 
                                                           ifelse(Country_Name == "Rest of South and Central Africa", "Rest of SC Africa", 
                                                                  ifelse(Country_Name == "Rest of South African Customs Union", "Reso of SA CU", 
                                                                         ifelse(Country_Name == "Viet Nam", "Vietnam",
                                                                                ifelse(Country_Name == "United States of America", "USA",
                                                                                       ifelse(Country_Name == "Rest of Caribbean", "Rest of the Caribbean",
                                                                                              ifelse(Country_Name == "Slovakia", "Slovak Republic",
                                                                                                     ifelse(Country_Name == "Côte d'Ivoire", "Cote dIvoire", 
                                                                                                            ifelse(Country_Name == "Russian Federation", "Russia", Country_Name))))))))))))))
carbon_intensities_0 <- data.frame()

for (i in Countries_new$Country_Name){

  if(i %in% excel_sheets("../0_Data/2_IO Data/GTAP_11_MRIO/Carbon_Intensities_Full_All.xlsx")){
    carbon_intensities_11 <- read.xlsx("../0_Data/2_IO Data/GTAP_11_MRIO/Carbon_Intensities_Full_All.xlsx", sheet = i)%>%
      mutate(GTAP_type = "11",
             Country = i)
  }else {
    carbon_intensities_11 <- data.frame()
    print("FAIL", i)
  }
  
  
  carbon_intensities <- carbon_intensities_11%>%
    select(Country, GTAP_type, GTAP:CO2_Mt_Transport, CO2_direct, Total_HH_Consumption_MUSD)
  
  carbon_intensities_0 <- bind_rows(carbon_intensities_0, carbon_intensities)
  
}

GTAP_code            <- read_delim("../0_Data/2_IO Data/GTAP_10_MRIO/GTAP10.csv", ";", escape_double = FALSE, trim_ws = TRUE, show_col_types = FALSE)

carbon_intensities_1 <- left_join(GTAP_code, carbon_intensities_0, by = c("Number"="GTAP"))%>%
  select(-Explanation, -Number)%>%
  mutate(GTAP = ifelse(GTAP == "gas" | GTAP == "gdt", "gasgdt", GTAP))%>%
  group_by(GTAP, Country, GTAP_type)%>%
  summarise(across(CO2_Mt:Total_HH_Consumption_MUSD, ~ sum(.)))%>%
  ungroup()%>%
  mutate(CO2_t_per_dollar_global      = CO2_Mt/            Total_HH_Consumption_MUSD,
         CO2_t_per_dollar_national    = CO2_Mt_within/     Total_HH_Consumption_MUSD,
         CO2_t_per_dollar_electricity = CO2_Mt_Electricity/Total_HH_Consumption_MUSD,
         CO2_t_per_dollar_transport   = CO2_Mt_Transport/  Total_HH_Consumption_MUSD)%>%
  group_by(GTAP)%>%
  mutate(intensity_sd   = sd(CO2_t_per_dollar_national),
         intensity_mean = mean(CO2_t_per_dollar_national))%>%
  ungroup()%>%
  mutate(z_score_intensity = (CO2_t_per_dollar_national - intensity_mean)/intensity_sd)

outlier <- carbon_intensities_1 %>%
  filter(z_score_intensity > 3)%>%
  arrange(desc(z_score_intensity))%>%
  select(GTAP, Country, CO2_t_per_dollar_national, intensity_mean, z_score_intensity)%>%
  filter(Country %in% c("Argentina", "Austria", "Bangladesh", "Brazil", "Canada", "Chile",
                        "Colombia", "Costa Rica", "Czech Republic", "Estonia", "Finland", "Germany", "India", "Indonesia", "Ireland",
                        "Kenya", "Mexico", "Morocco", "Pakistan", "Rest of the Caribbean", "Rest of Western Africa",
                        "Russia", "South Africa", "Thailand", "Turkey", "USA", "Vietnam"))%>%
  arrange(Country, desc(z_score_intensity))


# 4.1 Direct emissions in GTAP11 ####

GTAP_code            <- read_delim("../0_Data/2_IO Data/GTAP_10_MRIO/GTAP10.csv", ";", escape_double = FALSE, trim_ws = TRUE, show_col_types = FALSE)

carbon_intensities_1 <- left_join(GTAP_code, carbon_intensities_0, by = c("Number"="GTAP"))%>%
  select(-Explanation, -Number)%>%
  mutate(GTAP = ifelse(GTAP == "gas" | GTAP == "gdt", "gasgdt", GTAP))%>%
  group_by(GTAP, Country, GTAP_type)%>%
  summarise(across(CO2_Mt:Total_HH_Consumption_MUSD, ~ sum(.)))%>%
  ungroup()%>%
  mutate(CO2_t_per_dollar_global      = CO2_Mt/            Total_HH_Consumption_MUSD,
         CO2_t_per_dollar_national    = CO2_Mt_within/     Total_HH_Consumption_MUSD,
         CO2_t_per_dollar_electricity = CO2_Mt_Electricity/Total_HH_Consumption_MUSD,
         CO2_t_per_dollar_transport   = CO2_Mt_Transport/  Total_HH_Consumption_MUSD,
         CO2_t_per_dollar_direct      = CO2_direct/Total_HH_Consumption_MUSD)%>%
  group_by(GTAP)%>%
  mutate(intensity_sd   = sd(CO2_t_per_dollar_direct),
         intensity_mean = mean(CO2_t_per_dollar_direct))%>%
  ungroup()%>%
  filter(CO2_direct != 0)%>%
  mutate(z_score_direct_intensity = (CO2_t_per_dollar_direct - intensity_mean)/intensity_sd)%>%
  mutate(GTAP_type = ifelse(Country %in% c("South Africa", "Rwanda", "Togo", "Uganda"), "African", GTAP_type))

for (sector in c("gasgdt", "p_c", "coa", "oil")){
  print(sector)
  
  carbon_intensities_1.1 <- carbon_intensities_1 %>%
    filter(GTAP == sector)%>%
    # mutate(CO2_t_per_dollar_national = ifelse(Total_HH_Consumption_MUSD == 0,0,CO2_t_per_dollar_national))%>%
    arrange(CO2_t_per_dollar_direct)%>%
    mutate(number = 1:n())%>%
    group_by(Country)%>%
    mutate(order = min(number))%>%
    ungroup()
  
  P_1 <- ggplot(carbon_intensities_1.1)+
    # geom_line(aes(x = reorder(Country,order), y = CO2_t_per_dollar_national), size = 0.1)+
    geom_point(aes(x = reorder(Country, order), y = CO2_t_per_dollar_direct, fill = GTAP_type), shape = 21)+
    scale_fill_viridis_d()+
    #scale_y_log10()+
    theme_bw()+
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 6),
          panel.grid = element_blank())+
    coord_cartesian(ylim = c(0,min(max(carbon_intensities_1.1$CO2_t_per_dollar_national*1.05),1)))+
    ylab("Country")+
    xlab("Carbon intensity")+
    ggtitle(paste0("Sector: ", sector))
  
  jpeg(sprintf("../0_Data/2_IO Data/GTAP_11_MRIO/Comparing GTAP 11 and GTAP10/Comparison_%s_direct_scale.jpg", sector), width = 30, height = 20, unit = "cm", res = 200)
  print(P_1)
  dev.off()
  
}

# 4.2 Indirect emissions in GTAP 11 ####

carbon_intensities_1 <- left_join(GTAP_code, carbon_intensities_0, by = c("Number"="GTAP"))%>%
  select(-Explanation, -Number)%>%
  mutate(GTAP = ifelse(GTAP == "gas" | GTAP == "gdt", "gasgdt", GTAP))%>%
  group_by(GTAP, Country, GTAP_type)%>%
  summarise(across(CO2_Mt:Total_HH_Consumption_MUSD, ~ sum(.)))%>%
  ungroup()%>%
  mutate(CO2_t_per_dollar_global      = CO2_Mt/            Total_HH_Consumption_MUSD,
         CO2_t_per_dollar_national    = CO2_Mt_within/     Total_HH_Consumption_MUSD,
         CO2_t_per_dollar_electricity = CO2_Mt_Electricity/Total_HH_Consumption_MUSD,
         CO2_t_per_dollar_transport   = CO2_Mt_Transport/  Total_HH_Consumption_MUSD,
         CO2_t_per_dollar_direct      = CO2_direct/Total_HH_Consumption_MUSD)%>%
  mutate(CO2_t_per_dollar_indirect    = CO2_t_per_dollar_national - CO2_t_per_dollar_direct)%>%
  group_by(GTAP)%>%
  mutate(intensity_sd   = sd(CO2_t_per_dollar_indirect),
         intensity_mean = mean(CO2_t_per_dollar_indirect))%>%
  ungroup()%>%
  filter(CO2_direct != 0)%>%
  mutate(z_score_indirect_intensity = (CO2_t_per_dollar_indirect - intensity_mean)/intensity_sd)%>%
  mutate(GTAP_type = ifelse(Country %in% c("South Africa", "Rwanda", "Togo", "Uganda"), "African", GTAP_type))

for (sector in c("gasgdt", "p_c", "coa", "oil")){
  print(sector)
  
  carbon_intensities_1.1 <- carbon_intensities_1 %>%
    filter(GTAP == sector)%>%
    # mutate(CO2_t_per_dollar_national = ifelse(Total_HH_Consumption_MUSD == 0,0,CO2_t_per_dollar_national))%>%
    arrange(CO2_t_per_dollar_indirect)%>%
    mutate(number = 1:n())%>%
    group_by(Country)%>%
    mutate(order = min(number))%>%
    ungroup()
  
  P_1 <- ggplot(carbon_intensities_1.1)+
    # geom_line(aes(x = reorder(Country,order), y = CO2_t_per_dollar_national), size = 0.1)+
    geom_point(aes(x = reorder(Country, order), y = CO2_t_per_dollar_indirect, fill = GTAP_type), shape = 21)+
    scale_fill_viridis_d()+
    #scale_y_log10()+
    theme_bw()+
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 6),
          panel.grid = element_blank())+
    coord_cartesian(ylim = c(0,min(max(carbon_intensities_1.1$CO2_t_per_dollar_indirect*1.05),1)))+
    ylab("Country")+
    xlab("Carbon intensity")+
    ggtitle(paste0("Sector: ", sector))
  
  jpeg(sprintf("../0_Data/2_IO Data/GTAP_11_MRIO/Comparing GTAP 11 and GTAP10/Comparison_%s_indirect_scale.jpg", sector), width = 30, height = 20, unit = "cm", res = 200)
  print(P_1)
  dev.off()
  
}

rm(carbon_intensities, carbon_intensities_0, carbon_intensities_1, carbon_intensities_11, Countries_new, GTAP_code, outlier)

# 4.3 Comparing GTAP11A and GTAP11B ####

Countries_new <- read.xlsx("../0_Data/2_IO Data/GTAP_11_MRIO/Countries_Overview.xlsx")%>%
  mutate(Name = tolower(Code),
         Country_Name = Description,
         Country = Number)%>%
  select(Name, Country_Name, Country)%>%
  mutate(Country_Name = ifelse(Country_Name == "Hong Kong, Special Administrative Region of China", "Hong Kong", 
                               ifelse(Country_Name == "Venezuela (Bolivarian Republic of)", "Venezuela", 
                                      ifelse(Country_Name == "Rest of European Free Trade Association", "Rest of European FTA", 
                                             ifelse(Country_Name == "Palestineian Territory, Occupied", "Palestina", 
                                                    ifelse(Country_Name == "Democratic Republic of the Congo", "DR Congo", 
                                                           ifelse(Country_Name == "Rest of South and Central Africa", "Rest of SC Africa", 
                                                                  ifelse(Country_Name == "Rest of South African Customs Union", "Reso of SA CU", 
                                                                         ifelse(Country_Name == "Viet Nam", "Vietnam",
                                                                                ifelse(Country_Name == "United States of America", "USA",
                                                                                       ifelse(Country_Name == "Rest of Caribbean", "Rest of the Caribbean",
                                                                                              ifelse(Country_Name == "Slovakia", "Slovak Republic",
                                                                                                     ifelse(Country_Name == "Côte d'Ivoire", "Cote dIvoire", 
                                                                                                            ifelse(Country_Name == "Russian Federation", "Russia", Country_Name))))))))))))))


carbon_intensities_0 <- data.frame()

for (i in Countries_new$Country_Name){
  
  if(i %in% excel_sheets("../0_Data/2_IO Data/GTAP_11_MRIO/Carbon_Intensities_Full_All_Gas.xlsx")){
    carbon_intensities_11A <- read.xlsx("../0_Data/2_IO Data/GTAP_11_MRIO/Carbon_Intensities_Full_All_Gas.xlsx", sheet = i)%>%
      mutate(GTAP_type    = "11A",
             Country = i)
  }else {carbon_intensities_11A <- data.frame()}
  
  
  
  if(i %in% excel_sheets("../0_Data/2_IO Data/GTAP_11_MRIO/GTAP11B_2017/Carbon_Intensities_Full_All_Gas.xlsx")){
    carbon_intensities_11B <- read.xlsx("../0_Data/2_IO Data/GTAP_11_MRIO/GTAP11B_2017/Carbon_Intensities_Full_All_Gas.xlsx", sheet = i)%>%
      mutate(GTAP_type = "11B",
             Country = i)
  }else {carbon_intensities_11B <- data.frame()}
  
  
  carbon_intensities <- bind_rows(carbon_intensities_11A, carbon_intensities_11B)%>%
    select(Country, GTAP_type, GTAP:CO2_Mt_Transport, CO2_direct, Total_HH_Consumption_MUSD)
  
  carbon_intensities_0 <- bind_rows(carbon_intensities_0, carbon_intensities)
  
}

GTAP_code            <- read_delim("../0_Data/2_IO Data/GTAP_10_MRIO/GTAP10.csv", ";", escape_double = FALSE, trim_ws = TRUE, show_col_types = FALSE)

carbon_intensities_1 <- left_join(GTAP_code, carbon_intensities_0, by = c("Number"="GTAP"))%>%
  select(-Explanation, -Number)%>%
  mutate(GTAP = ifelse(GTAP == "gas" | GTAP == "gdt", "gasgdt", GTAP))%>%
  group_by(GTAP, Country, GTAP_type)%>%
  summarise(across(CO2_Mt:Total_HH_Consumption_MUSD, ~ sum(.)))%>%
  ungroup()%>%
  mutate(CO2_t_per_dollar_global      = CO2_Mt/            Total_HH_Consumption_MUSD,
         CO2_t_per_dollar_national    = CO2_Mt_within/     Total_HH_Consumption_MUSD,
         CO2_t_per_dollar_electricity = CO2_Mt_Electricity/Total_HH_Consumption_MUSD,
         CO2_t_per_dollar_transport   = CO2_Mt_Transport/  Total_HH_Consumption_MUSD)

for (sector in distinct(carbon_intensities_1, GTAP)$GTAP){
  print(sector)
  
  carbon_intensities_1.1 <- carbon_intensities_1 %>%
    filter(GTAP == sector)%>%
    mutate(CO2_t_per_dollar_national = ifelse(Total_HH_Consumption_MUSD == 0,0,CO2_t_per_dollar_national))%>%
    arrange(desc(GTAP_type), CO2_t_per_dollar_national)%>%
    mutate(number = 1:n())%>%
    group_by(Country)%>%
    mutate(order = min(number))%>%
    ungroup()
  
  P_1 <- ggplot(carbon_intensities_1.1)+
    geom_line(aes(x = reorder(Country,order), y = CO2_t_per_dollar_national), size = 0.1)+
    geom_point(aes(x = reorder(Country,order), y = CO2_t_per_dollar_national, fill = GTAP_type), shape = 21)+
    scale_fill_viridis_d()+
    #scale_y_log10()+
    theme_bw()+
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 6),
          panel.grid = element_blank())+
    coord_cartesian(ylim = c(0,min(max(carbon_intensities_1.1$CO2_t_per_dollar_national*1.05),1)))+
    ylab("Country")+
    xlab("Carbon intensity")+
    ggtitle(paste0("Sector: ", sector))
  
  jpeg(sprintf("../0_Data/2_IO Data/GTAP_11_MRIO/Comparing GTAP11A and GTAP11B/Comparison_%s_rev_scale.jpg", sector), width = 30, height = 20, unit = "cm", res = 200)
  print(P_1)
  dev.off()
  
}

carbon_intensities_2 <- carbon_intensities_1 %>%
  group_by(GTAP_type, GTAP)%>%
  mutate(mean_0 = mean(CO2_t_per_dollar_national),
         sd_0   = sqrt(var(CO2_t_per_dollar_national)))%>%
  mutate(z_score = (CO2_t_per_dollar_national - mean_0)/sd_0)%>%
  ungroup()%>%
  # as an example
  filter(Country == "Czech Republic")
