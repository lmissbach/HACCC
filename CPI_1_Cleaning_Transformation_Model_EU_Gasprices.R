# 0       General ####

# Author: L. Missbach, missbach@mcc-berlin.net

carbon.price <- 377 # TBD
gas.price    <- 1.38 # relative price increase in percent: 1.38 = 138%
coal.price   <- 0.15 # relative price increase in percent: 0.15 = 15%
p_c.price    <- 0.15 # relative price increase in percent: 0.15 = 15%
oil.price    <- 0.15 # relative price increase in percent: 0.15 = 15%
  
  
# 1       Packages ####

library("haven")
library("Hmisc")
library("openxlsx")
library("rattle")
library("scales")
library("tidyverse")
options(scipen=999)

# 1.1     Setup ####

Year_0 <- 2015
Country.Name <- "Europe"
path_0                  <-list.files("../0_Data/1_Household Data/")[grep(Country.Name, list.files("../0_Data/1_Household Data/"), ignore.case = T)]

# 2       Load Household and Expenditure File ####

household_information   <- read_csv("K:/WorkInProgress/2021_Carbon_Footprint_Analysis/Data_Transformed/Household_Data_Clean.csv")
expenditure_information <- read_csv("K:/WorkInProgress/2021_Carbon_Footprint_Analysis/Data_Transformed/Expenditure_Data_Clean_Corrected.csv")


if(ncol(expenditure_information)>4){
print("Warning! Expenditure-DF is in Wide-Format.")
}

if(nrow(count(expenditure_information, hh_id)) != nrow(count(household_information, hh_id))) print("WARNING!")

# 2.1     First Data Screening #### 

data_combined <- left_join(expenditure_information, household_information)

data_2.1 <- data_combined %>%
  distinct(hh_id, COUNTRY)%>%
  group_by(COUNTRY)%>%
  summarise(number_Country = n())%>%
  ungroup()

data_2.2 <- data_combined %>%
  group_by(COUNTRY, item_code)%>%
  summarise(number = n(),
            expenditures = sum(expenditures_year))%>%
  ungroup()%>%
  left_join(data_2.1)%>%
  mutate(share = number/number_Country)

# 3       Data Cleaning ####
# 3.1     Check for Duplicates ####

household_information_1 <- household_information %>%
  group_by_at(vars(-hh_id))%>%
  mutate(number = n(),
         flag = ifelse(number > 1,1,0))%>%
  ungroup()

if(nrow(filter(household_information_1, flag != 0))>0) View(filter(household_information_1, flag != 0))

hh_duplicates_information <- household_information_1 %>%
  filter(flag != 0)%>%
  select(hh_id)

# Exact duplicates for expenditures - see below for more detailed approach
expenditure_information_1 <- expenditure_information %>%
  pivot_wider(names_from = "item_code", values_from = "expenditures_year")%>%
  group_by_at(vars(-hh_id))%>%
  mutate(number = n(),
         flag = ifelse(number > 1,1,0))%>%
  ungroup()

print(paste("There are ", nrow(count(expenditure_information, hh_id)) - nrow(filter(expenditure_information_1, flag == 0)), sprintf(" cases of exact duplicates of expenditures on the item level in %s.", Country.Name)))

hh_duplicates_expenditures_1 <- expenditure_information_1 %>%
  filter(flag != 0)%>%
  select(hh_id)

# Alternative: calculates share of duplicates on the item level
# Use this as a monitoring tool
expenditure_information_2 <- expenditure_information %>%
  filter(!is.na(expenditures_year) & expenditures_year != 0)%>%
  group_by(item_code, expenditures_year)%>%
  mutate(duplicate_flag = ifelse(n()>1,1,0))%>%
  ungroup()%>%
  group_by(hh_id)%>%
  mutate(duplicate_share = sum(duplicate_flag)/n())%>%
  ungroup()

hh_duplicates_expenditures_2 <- expenditure_information_2 %>%
  filter(duplicate_share == 1)%>%
  select(hh_id)%>%
  distinct()

# Could be sufficient to monitor

expenditure_information_3 <- expenditure_information %>%
  filter(!is.na(expenditures_year) & expenditures_year != 0)%>%
  group_by(hh_id)%>%
  summarise(hh_expenditures = sum(expenditures_year))%>%
  ungroup()%>%
  group_by(hh_expenditures)%>%
  mutate(duplicate_flag_2 = ifelse(n()>1,1,0))%>%
  ungroup()

if(nrow(filter(expenditure_information_3, duplicate_flag_2 ==1))>1) print("Warning! Two or more households spend exactly the same amount of money on all their items.")

# Probably more important than searching for duplicates at item expenditure level

hh_duplicates_expenditures_3 <- expenditure_information_3 %>%
  filter(duplicate_flag_2 == 1)%>%
  select(hh_id)

# Negative total expenditures

expenditure_information_4 <- expenditure_information_3 %>%
  mutate(flag_negative = ifelse(hh_expenditures < 0,1,0))

hh_negative_expenditures_4 <- expenditure_information_4 %>%
  filter(flag_negative == 1)%>%
  select(hh_id)

# 3.1.1   Duplicate Removal ####

# hh_duplicates_information captures all households, whose characteristics are identical with another --> needs careful consideration on whether these are actual duplicates
# hh_duplicates_expenditures_1 captures all households, who spend exactly the same amount of money on each item than any other households --> likely duplicate
# hh_duplicates_expenditures_2 captures all households, who do not report any individual amount of expenditures on any items. 
# Each level of expenditures for any item is shared with another household --> needs careful consideration on whether these are actual duplicates --> likely no duplicate
# hh_duplicates_expenditures_3 captures all households, who report the same amount of total expenditures as some other household --> likely no duplicate, but check individually for your country

# If you have identified duplicates and want to delete them, do the following:
# select the corresponding line with hh_ids


household_information <- household_information %>%
  filter(!hh_id %in% hh_duplicates_information$hh_id)%>%
  filter(!hh_id %in% hh_duplicates_expenditures_1$hh_id)%>%
  filter(!hh_id %in% hh_negative_expenditures_4$hh_id)

expenditure_information <- expenditure_information %>%
  filter(!hh_id %in% hh_duplicates_information$hh_id)%>%
  filter(!hh_id %in% hh_duplicates_expenditures_1$hh_id)%>%
  filter(!hh_id %in% hh_negative_expenditures_4$hh_id)


rm(expenditure_information_1, expenditure_information_2, expenditure_information_3, household_information_1, 
   hh_duplicates_expenditures_1, hh_duplicates_expenditures_2, hh_duplicates_expenditures_3, hh_duplicates_information,
   hh_negative_expenditures_4, expenditure_information_4)

# 3.2     Cleaning per Item_code ####

expenditure_information_4 <- expenditure_information %>%
  # pivot_longer(-hh_id, names_to = "item_code", values_to = "expenditures") %>%
  left_join(select(household_information, hh_id, hh_weights))%>%
  # here: negative values are deleted (@EU)
  filter(!is.na(expenditures_year) & expenditures_year > 0 )%>%
  group_by(item_code)%>%
  mutate(outlier_95 = wtd.quantile(expenditures_year, weights = hh_weights, probs = 0.95),
         outlier_99 = wtd.quantile(expenditures_year, weights = hh_weights, probs = 0.99),
         median_exp = wtd.quantile(expenditures_year, weights = hh_weights, probs = 0.5),
         mean_exp   = wtd.mean(    expenditures_year, weights = hh_weights))%>%
  ungroup()%>%
  mutate(flag_outlier_95 = ifelse(expenditures_year>= outlier_95,1,0),
         flag_outlier_99 = ifelse(expenditures_year>= outlier_99,1,0))%>%
  # this line replaces all expenditures which are above the 99th percentile for each item to the median
  mutate(expenditures = ifelse(flag_outlier_99 == 1, median_exp, expenditures_year))%>%
  select(hh_id, item_code, expenditures, hh_weights)

expenditure_information <- expenditure_information_4 %>%
  select(-hh_weights)

# 3.2.1   Cleaning per Total Expenditures (99%) ####

expenditure_information_4.1 <- expenditure_information_4 %>%
  group_by(hh_id) %>%
  mutate(total_expenditures = sum(expenditures)) %>%
  ungroup() %>%
  mutate(outlier_95 = wtd.quantile(total_expenditures, weights = hh_weights, probs = 0.95),
         outlier_99 = wtd.quantile(total_expenditures, weights = hh_weights, probs = 0.99))

expenditure_outlier <- expenditure_information_4.1 %>%
  filter(total_expenditures >= outlier_99)%>%
  select(hh_id)%>%
  distinct()

print("Expenditure data cleaned!")

rm(expenditure_information_4.1, expenditure_information_4, expenditure_outlier)

# 4       Summary Statistics ####
# _____   ####
# 5       Transformation and Modelling ####

# 5.1     Load Additional Data ####
# 5.1.1   Supplementary Data ####
# Exchange Rates

information.ex <- read.xlsx("../0_Data/9_Supplementary Data/Exchange_Rates_2014.xlsx") # from World Bank

exchange.rate  <- as.numeric(information.ex$exchange_rate[information.ex$Country == "Europe"]) # not ppp-adjusted

# CPI-Adjustment (Inflation/Deflation)

cpis <- read.xlsx("../0_Data/9_Supplementary Data/IMF_Consumer_Price_Index_Inflation_Average.xlsx")


countries <- distinct(household_information, COUNTRY)
countries_b <- c("Belgium", "Bulgaria", "Cyprus", "Czech Republic", "Germany", "Denmark", "Estonia", "Greece", "Spain", "Finland",
                   "France", "Croatia", "Hungary" ,"Ireland", "Italy", "Lithuania", "Luxembourg", "Latvia", "Netherlands", "Poland",
                   "Portugal", "Romania", "Sweden", "Slovak Republic")
countries <- countries %>%
 mutate(Country = countries_b)
  
cpis_0 <- cpis %>%
  select(Country, starts_with("2"))%>%
  filter(Country %in% countries$Country)
  
cpis_1 <- cpis_0 %>%
  mutate_at(vars('2010':'2019'), function(x) x = as.numeric(x))%>%
  mutate_at(vars('2010':'2019'), function(x) x = 1 + x/100)%>%
  rename_at(vars(starts_with("2")), list(~ str_replace(., "^", "Year_")))%>%
  group_by(Country)%>%
  mutate(inflation_factor = ifelse(Year_0 == 2010, Year_2011*Year_2012*Year_2013*Year_2014, 
                                   ifelse(Year_0 == 2012, Year_2013*Year_2014,
                                          ifelse(Year_0 == 2013, Year_2014,
                                                 ifelse(Year_0 == 2014, 1,
                                                        ifelse(Year_0 == 2015, 1/Year_2015,
                                                               ifelse(Year_0 == 2016, 1/(Year_2015*Year_2016),
                                                                      ifelse(Year_0 == 2017, 1/(Year_2015*Year_2016*Year_2017),
                                                                             ifelse(Year_0 == 2018, 1/(Year_2015*Year_2016*Year_2017*Year_2018), 
                                                                                    ifelse(Year_0 == 2019, 1/(Year_2015*Year_2016*Year_2017*Year_2018*Year_2019),0))))))))))%>%
  ungroup()%>%
  select(Country, inflation_factor)%>%
  left_join(countries)
  
inflation_factor_EU <- cpis_1
  
rm(cpis_1, cpis_0, information.ex, cpis)

# 5.1.2   Matching GTAP Concordance ####

matching <- read.xlsx("../0_Data/1_Household Data/4_Europe_EU27/3_Matching_Tables/Item_GTAP_Concordance_EU_incl_Artificial.xlsx")

matching <- matching %>%
  select (-Explanation) %>%
  pivot_longer(-GTAP, names_to = "drop", values_to = "item_code")%>%
  filter(!is.na(item_code))%>%
  select(GTAP, item_code)%>%
  mutate(GTAP = ifelse(GTAP == "gas" | GTAP == "gdt", "gasgdt", GTAP))

# Check if single item codes are assigned to two different GTAP categories

item_codes <- select(expenditure_information, item_code)%>%
  distinct()%>%
  left_join(matching)%>%
  filter(is.na(GTAP))

if(nrow(item_codes != 0))(paste("WARNING! Item-Codes missing in Excel-File!"))

matching.check <- count(matching, item_code)%>%
  filter(n != 1)

if(nrow(matching.check) != 0) (paste("WARNING! Item-Codes existing with two different GTAP-categories in Excel-File"))

if(nrow(item_codes != 0) | nrow(matching.check) != 0) break

rm(matching.check, item_codes)

# 5.1.3   Matching Category Concordance ####

categories <- read.xlsx(sprintf("../0_Data/1_Household Data/%s/3_Matching_Tables/Item_Categories_Concordance_%s.xlsx", path_0, Country.Name), colNames = FALSE)

categories <- categories %>%
  pivot_longer(-X1, names_to = "drop", values_to = "item_code")%>%
  filter(!is.na(item_code))%>%
  select(X1, item_code)%>%
  rename(category = X1)

item_codes <- select(expenditure_information, item_code)%>%
  distinct()%>%
  left_join(categories)%>%
  filter(is.na(category))

if(nrow(item_codes != 0))(paste("WARNING! Item-Codes missing in Category-Excel-File!"))

matching.check <- count(categories, item_code)%>%
  filter(n != 1)

if(nrow(matching.check) != 0) (paste("WARNING! Item-Codes existing with two different Categories-categories in Excel-File"))

if(nrow(item_codes != 0) | nrow(matching.check) != 0) break

rm(matching.check, item_codes)

# 5.1.4   Add Codes if necessary (TBD) ####

# 5.1.5   Matching Fuel Concordance ####

fuels <- read.xlsx(sprintf("../0_Data/1_Household Data/%s/3_Matching_Tables/Item_Fuel_Concordance_%s.xlsx", path_0, Country.Name), colNames = FALSE)

fuels <- fuels %>%
  pivot_longer(-X1, names_to = "drop", values_to = "item_code")%>%
  filter(!is.na(item_code))%>%
  rename(fuel = X1)%>%
  select(fuel, item_code)

energy <- filter(categories, category == "energy")%>%
  full_join(fuels)%>%
  filter(is.na(fuel) | is.na(category))

if(nrow(energy) >0) print("Warning. Watch out for energy item codes.")

rm(energy)

# 5.1.6   Vector with Carbon Intensities ####

GTAP_code            <- read_delim("../0_Data/2_IO Data/GTAP_10_MRIO/GTAP10.csv", ";", escape_double = FALSE, trim_ws = TRUE)

carbon_intensities_EU <- data.frame()
  
for(i in countries_b){
  carbon_intensities_0 <- read.xlsx("../0_Data/2_IO Data/GTAP_10_MRIO/Carbon_Intensities_Full_All_incl_Gas_Coal_PC.xlsx", sheet = i)
  carbon_intensities   <- left_join(GTAP_code, carbon_intensities_0, by = c("Number"="GTAP"))%>%
    select(-Explanation, - Number)%>%
    mutate(GTAP = ifelse(GTAP == "gas" | GTAP == "gdt", "gasgdt", GTAP))%>%
    group_by(GTAP)%>%
    summarise(across(CO2_Mt:Total_HH_Consumption_MUSD, ~ sum(.)))%>%
    ungroup()%>%
    mutate(CO2_t_per_dollar_global       = CO2_Mt/            Total_HH_Consumption_MUSD,
           CO2_t_per_dollar_national     = CO2_Mt_within/     Total_HH_Consumption_MUSD,
           CO2_t_per_dollar_electricity  = CO2_Mt_Electricity/Total_HH_Consumption_MUSD,
           CO2_t_per_dollar_transport    = CO2_Mt_Transport/  Total_HH_Consumption_MUSD,
           CO2_t_per_dollar_gas          = CO2_Mt_Gas/        Total_HH_Consumption_MUSD,
           CO2_t_per_dollar_gas_direct   = CO2_Mt_Gas_direct/ Total_HH_Consumption_MUSD,
           CO2_t_per_dollar_gas_indirect = CO2_Mt_Gas_indir/  Total_HH_Consumption_MUSD)%>%
    select(GTAP, starts_with("CO2_t"), starts_with("GAS_"), starts_with("COAL_"), starts_with("P_C"), starts_with("OIL_"))%>%
    mutate(Country = i)%>%
    mutate(GAS_USD_Gas = ifelse(GTAP == "gasgdt", GAS_USD_Gas -1, GAS_USD_Gas),
           GAS_USD_Gas_direct = ifelse(GTAP == "gasgdt", GAS_USD_Gas_direct - 1, GAS_USD_Gas_direct))
  
  carbon_intensities_EU <- carbon_intensities_EU %>%
    bind_rows(carbon_intensities)
}
  
rm(carbon_intensities_0, GTAP_code, carbon_intensities)

# ____    ####
# 6       Transformation of Data ####

# 6.1     Anonymising Household-ID ####

household_ids <- select(household_information, hh_id)%>%
  distinct()%>%
  mutate(hh_id_new = 1:n())

household_information <- left_join(household_information, household_ids)%>%
  select(hh_id_new, everything(), -hh_id)%>%
  rename(hh_id = hh_id_new)

expenditure_information <- left_join(expenditure_information, household_ids)%>%
  select(hh_id_new, everything(), -hh_id)%>%
  rename(hh_id = hh_id_new)

basic_household_information <- household_information %>%
  select(hh_id, hh_size, hh_weights)

basic_household_information <- household_information %>%
  select(hh_id, hh_size, hh_weights, COUNTRY)%>%
  left_join(countries)%>%
  select(-COUNTRY)


# 6.2     Merging Expenditure Data and GTAP ####

expenditure_information_1 <- left_join(expenditure_information, matching, by = "item_code")%>%
  filter(GTAP != "deleted")

rm(matching)

# 6.3     Assign Households to Expenditure Bins ####

binning_0 <- expenditure_information_1 %>%
  group_by(hh_id)%>%
  mutate(hh_expenditures = sum(expenditures))%>%
  ungroup()%>%
  left_join(basic_household_information)%>%
  mutate(hh_expenditures_pc = hh_expenditures/hh_size)%>%
  select(hh_id, hh_expenditures, hh_expenditures_pc, hh_weights, Country)%>%
  filter(!duplicated(hh_id))%>%
  group_by(Country)%>%
  mutate(Income_Group_5  = as.numeric(binning(hh_expenditures_pc, bins = 5,  method = c("wtd.quantile"), weights = hh_weights)),
         Income_Group_10 = as.numeric(binning(hh_expenditures_pc, bins = 10, method = c("wtd.quantile"), weights = hh_weights)))%>%
  ungroup()%>%
  select(hh_id, hh_expenditures, hh_expenditures_pc, starts_with("Income"))


# 6.4     Calculating Expenditure Shares on Energy/Food/Goods/Services ####

expenditures_categories_0 <- left_join(expenditure_information, categories)%>%
  filter(category != "deleted" & category != "in-kind" & category != "self-produced")%>%
  group_by(hh_id, category)%>%
  summarise(expenditures_category = sum(expenditures))%>%
  ungroup()%>%
  group_by(hh_id)%>%
  mutate(share_category = expenditures_category/sum(expenditures_category))%>%
  ungroup()%>%
  select(hh_id, category, share_category)%>%
  pivot_wider(names_from = "category", values_from = "share_category", names_prefix = "share_", values_fill = 0)

rm(categories)

# 6.5     Calculating Expenditure Shares on detailed Energy Items ####

expenditures_fuels <- left_join(expenditure_information, fuels)%>%
  filter(!is.na(fuel))%>%
  group_by(hh_id, fuel)%>%
  summarise(expenditures = sum(expenditures))%>%
  ungroup()%>%
  pivot_wider(names_from = "fuel", values_from = "expenditures", names_prefix = "exp_LCU_")

rm(expenditure_information, fuels)

# 6.6     Summarising Expenditures on the GTAP Level ####

expenditure_information_1 <- expenditure_information_1 %>%
  group_by(hh_id, GTAP)%>%
  summarise(expenditures = sum(expenditures))%>%
  ungroup()%>%
  left_join(select(basic_household_information, hh_id, Country))%>%
  left_join(select(inflation_factor_EU, - COUNTRY))%>%
  mutate(expenditures_USD_2014 = expenditures*inflation_factor*exchange.rate)%>%
  group_by(hh_id)%>%
  mutate(hh_expenditures_USD_2014 = sum(expenditures_USD_2014))%>%
  ungroup()%>%
  select(-inflation_factor)

expenditure_information_2 <- expenditure_information_1 %>%
  group_by(hh_id)%>%
  summarise(hh_expenditures_LCU = sum(expenditures))%>%
  ungroup()

rm(exchange.rate, inflation_factor_EU, basic_household_information)

# 6.7     Merging Expenditures and Carbon Intensities ####

household_carbon_footprint <- left_join(expenditure_information_1, carbon_intensities_EU, by = c("GTAP", "Country"))%>%
  filter(GTAP != "other")%>%
  mutate(CO2_t_global              = expenditures_USD_2014*CO2_t_per_dollar_global,
         CO2_t_national            = expenditures_USD_2014*CO2_t_per_dollar_national,
         CO2_t_electricity         = expenditures_USD_2014*CO2_t_per_dollar_electricity,
         CO2_t_transport           = expenditures_USD_2014*CO2_t_per_dollar_transport,
         CO2_t_gas                 = expenditures_USD_2014*CO2_t_per_dollar_gas,
         CO2_t_gas_direct          = expenditures_USD_2014*CO2_t_per_dollar_gas_direct,
         CO2_t_gas_indirect        = expenditures_USD_2014*CO2_t_per_dollar_gas_indirect,
         GAS_USD_on_gas            = expenditures_USD_2014*GAS_USD_Gas,
         GAS_USD_on_gas_direct     = expenditures_USD_2014*GAS_USD_Gas_direct,
         GAS_USD_on_gas_indirect   = expenditures_USD_2014*GAS_USD_Gas_indir,
         
         COAL_USD_on_coal          = expenditures_USD_2014*COAL_USD_Coal,
         COAL_USD_on_coal_direct   = expenditures_USD_2014*COAL_USD_Coal_direct,
         COAL_USD_on_coal_indirect = expenditures_USD_2014*COAL_USD_Coal_indir,
         
         P_C_USD_on_p_c            = expenditures_USD_2014*P_C_USD_p_c,
         P_C_USD_on_p_c_direct     = expenditures_USD_2014*P_C_USD_p_c_direct,
         P_C_USD_on_p_c_indirect   = expenditures_USD_2014*P_C_USD_p_c_indir,
         
         OIL_USD_on_oil            = expenditures_USD_2014*OIL_USD_oil)%>%
  select(-starts_with("CO2_t_per"))%>%
  group_by(hh_id)%>%
  summarise(hh_expenditures_USD_2014  = first(hh_expenditures_USD_2014),
            CO2_t_global              = sum(CO2_t_global),    
            CO2_t_national            = sum(CO2_t_national),  
            CO2_t_electricity         = sum(CO2_t_electricity),
            CO2_t_transport           = sum(CO2_t_transport),
            CO2_t_gas                 = sum(CO2_t_gas),
            CO2_t_gas_direct          = sum(CO2_t_gas_direct),
            CO2_t_gas_indirect        = sum(CO2_t_gas_indirect),
            GAS_USD_on_gas            = sum(GAS_USD_on_gas),
            GAS_USD_on_gas_direct     = sum(GAS_USD_on_gas_direct),
            GAS_USD_on_gas_indirect   = sum(GAS_USD_on_gas_indirect),
            
            COAL_USD_on_coal          = sum(COAL_USD_on_coal),
            COAL_USD_on_coal_direct   = sum(COAL_USD_on_coal_direct),
            COAL_USD_on_coal_indirect = sum(COAL_USD_on_coal_indirect),
            
            P_C_USD_on_p_c            = sum(P_C_USD_on_p_c),
            P_C_USD_on_p_c_direct     = sum(P_C_USD_on_p_c_direct),
            P_C_USD_on_p_c_indirect   = sum(P_C_USD_on_p_c_indirect),
            
            OIL_USD_on_oil            = sum(OIL_USD_on_oil))%>%
  ungroup()


rm(carbon_intensities_EU, expenditure_information_1)

# ____    ####
# 7       Model / Calculating Carbon Incidence ####
# 7.1     Analysis of Carbon Pricing Incidence ####

household_carbon_incidence <- household_carbon_footprint %>%
  mutate(#exp_CO2_global              = CO2_t_global*carbon.price,
         #exp_CO2_national            = CO2_t_national*carbon.price,
         #exp_CO2_electricity         = CO2_t_electricity*carbon.price,
         #exp_CO2_transport           = CO2_t_transport*carbon.price,
         exp_CO2_gas            = CO2_t_gas*carbon.price,
         exp_CO2_gas_direct     = CO2_t_gas_direct*carbon.price,
         exp_CO2_gas_indirect   = CO2_t_gas_indirect*carbon.price,
         exp_GAS_gas            = GAS_USD_on_gas*gas.price,
         exp_GAS_gas_direct     = GAS_USD_on_gas_direct*gas.price,
         exp_GAS_gas_indirect   = GAS_USD_on_gas_indirect*gas.price,
         exp_COAL_coal          = COAL_USD_on_coal*coal.price,
         exp_COAL_coal_direct   = COAL_USD_on_coal_direct*coal.price,
         exp_COAL_coal_indirect = COAL_USD_on_coal_indirect*coal.price,
         exp_P_C_p_c            = P_C_USD_on_p_c*p_c.price,
         exp_P_C_p_c_direct     = P_C_USD_on_p_c_direct*p_c.price,
         exp_P_C_p_c_indirect   = P_C_USD_on_p_c_indirect*p_c.price,
         exp_OIL_oil            = OIL_USD_on_oil*oil.price)%>%
  mutate(#burden_CO2_global           = exp_CO2_global/     hh_expenditures_USD_2014,
         #burden_CO2_national         = exp_CO2_national/   hh_expenditures_USD_2014,
         #burden_CO2_electricity      = exp_CO2_electricity/hh_expenditures_USD_2014,
         #burden_CO2_transport        = exp_CO2_transport/  hh_expenditures_USD_2014,
         burden_CO2_gas          = exp_CO2_gas/hh_expenditures_USD_2014,
         burden_CO2_gas_direct   = exp_CO2_gas_direct/hh_expenditures_USD_2014,
         burden_CO2_gas_indirect = exp_CO2_gas_indirect/hh_expenditures_USD_2014,
         burden_GAS_gas          = exp_GAS_gas/hh_expenditures_USD_2014,
         burden_GAS_direct       = exp_GAS_gas_direct/hh_expenditures_USD_2014,
         burden_GAS_indirect     = exp_GAS_gas_indirect/hh_expenditures_USD_2014,
         burden_COAL_coal        = exp_COAL_coal/hh_expenditures_USD_2014,
         burden_COAL_direct      = exp_COAL_coal_direct/hh_expenditures_USD_2014,
         burden_COAL_indirect    = exp_COAL_coal_indirect/hh_expenditures_USD_2014,
         burden_P_C_p_c          = exp_P_C_p_c/hh_expenditures_USD_2014,
         burden_P_C_direct       = exp_P_C_p_c_direct/hh_expenditures_USD_2014,
         burden_P_C_indirect     = exp_P_C_p_c_indirect/hh_expenditures_USD_2014,
         burden_OIL_oil          = exp_OIL_oil/hh_expenditures_USD_2014)

final_incidence_information <- household_carbon_incidence %>%
  left_join(binning_0)%>%
  left_join(expenditures_categories_0)%>%
  left_join(expenditures_fuels)

if(max(final_incidence_information$CO2_t_global) == "Inf") "Warning! Check Intensities."

if(max(final_incidence_information$CO2_t_global) == "Inf") break


NA_ids <- household_information %>%
  filter(!hh_id %in% final_incidence_information$hh_id)

household_information <- household_information %>%
  left_join(countries)%>%
  filter(!hh_id %in% NA_ids$hh_id)

write_csv(household_information, "K:/WorkInProgress/2021_Carbon_Footprint_Analysis/Data_Transformed/household_information_Europe_new.csv")
write_csv(final_incidence_information, "K:/WorkInProgress/2021_Carbon_Footprint_Analysis/Data_Transformed/Gas_Price_Incidence_Europe.csv")

rm(carbon_intensities_EU, countries, NA_ids)

