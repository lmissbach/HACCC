# 0   General ####

# Author: L. Missbach, missbach@mcc-berlin.net

# 0.1 Packages ####

library("boot")
library("broom")
library("cowplot")
library("fixest")
library("ggsci")
library("haven")
library("Hmisc")
library("openxlsx")
library("rattle")
library("scales")
library("tidyverse")
options(scipen=999)

# 1   Loading Data ####

Country.Set <- c("Argentina", "Barbados","Bolivia", "Brazil", "Chile", "Colombia",
                 "Costa Rica", "Dominican Republic", "Ecuador",
                 "El Salvador", "Guatemala", "Mexico", "Nicaragua", "Paraguay", "Peru", "Uruguay")

data_joint_0 <- data.frame()
  
for(Country.Name in c("Argentina", "Barbados","Bolivia", "Brazil", "Chile", "Colombia",
                        "Costa Rica", "Dominican Republic", "Ecuador",
                        "El Salvador", "Guatemala", "Mexico", "Nicaragua", "Paraguay", "Peru", "Uruguay")) {
    
    carbon_pricing_incidence_0 <- read_csv(sprintf("../1_Carbon_Pricing_Incidence/3_Analyses/1_LAC_2021/4_Transformed Data/Carbon_Pricing_Incidence_%s.csv", Country.Name))
    
    household_information_0    <- read_csv(sprintf("../1_Carbon_Pricing_Incidence/3_Analyses/1_LAC_2021/4_Transformed Data/household_information_%s_new.csv", Country.Name))
    
    if(Country.Name == "El_Salvador") Country.Name.2 <- "El Salvador" else Country.Name.2 <- Country.Name
    
    if(Country.Name != "Chile") appliances_0_1 <- read_csv(sprintf("../0_Data/1_Household Data/3_%s/1_Data_Clean/appliances_0_1_new_%s.csv", Country.Name.2, Country.Name.2))
    
        carbon_pricing_incidence_1 <- left_join(carbon_pricing_incidence_0, household_information_0)%>%
      mutate(Country = Country.Name.2)
    
    if("district" %in% colnames(carbon_pricing_incidence_1)){
      carbon_pricing_incidence_1 <- carbon_pricing_incidence_1 %>%
        mutate(district = as.character(district))
    }
    
    if("ethnicity" %in% colnames(carbon_pricing_incidence_1)){
      carbon_pricing_incidence_1 <- carbon_pricing_incidence_1 %>%
        mutate(ethnicity = as.character(ethnicity))
      
      Ethnicity.Code <- read_csv(sprintf("../0_Data/1_Household Data/3_%s/2_Codes/Ethnicity.Code.csv", Country.Name.2))%>%
        select(ethnicity, everything())%>%
        mutate(ethnicity = as.character(ethnicity))
      
      colnames(Ethnicity.Code) <- c("ethnicity", "Ethnicity")
      carbon_pricing_incidence_1 <- left_join(carbon_pricing_incidence_1, Ethnicity.Code)
      
    }    
        
    if("province" %in% colnames(carbon_pricing_incidence_1)){
      carbon_pricing_incidence_1 <- carbon_pricing_incidence_1 %>%
        mutate(province = as.character(province))
    }
    
    if("edu_hhh" %in% colnames(carbon_pricing_incidence_1)){
      carbon_pricing_incidence_1 <- carbon_pricing_incidence_1 %>%
        mutate(edu_hhh = as.character(edu_hhh))
      
      Education.Code <- read_csv(sprintf("../0_Data/1_Household Data/3_%s/2_Codes/Education.Code.csv", Country.Name.2))%>%
        select(edu_hhh, ISCED)%>%
        mutate(edu_hhh = as.character(edu_hhh),
               ISCED = as.character(ISCED))
      
      carbon_pricing_incidence_1 <- left_join(carbon_pricing_incidence_1, Education.Code)
      
    }
    
    if("ind_hhh" %in% colnames(carbon_pricing_incidence_1)){
      carbon_pricing_incidence_1 <- carbon_pricing_incidence_1 %>%
        mutate(ind_hhh = as.character(ind_hhh))
    }
    
    if("toilet" %in% colnames(carbon_pricing_incidence_1)){
      carbon_pricing_incidence_1 <- carbon_pricing_incidence_1 %>%
        mutate(toilet = as.character(toilet))
      
      Toilet.Code <- read_csv(sprintf("../0_Data/1_Household Data/3_%s/2_Codes/Toilet.Code.csv", Country.Name.2))%>%
        select(toilet, TLT)%>%
        mutate(toilet = as.character(toilet))
      
      carbon_pricing_incidence_1 <- left_join(carbon_pricing_incidence_1, Toilet.Code)
    }
    
    if("water" %in% colnames(carbon_pricing_incidence_1)){
      carbon_pricing_incidence_1 <- carbon_pricing_incidence_1 %>%
        mutate(water = as.character(water))
      Water.Code <- read_csv(sprintf("../0_Data/1_Household Data/3_%s/2_Codes/Water.Code.csv", Country.Name.2))%>%
        select(water, WTR)%>%
        mutate(water = as.character(water))
      carbon_pricing_incidence_1 <- left_join(carbon_pricing_incidence_1, Water.Code)
    }
    
    if("cooking_fuel" %in% colnames(carbon_pricing_incidence_1)){
      Cooking.Code <- read_csv(sprintf("../0_Data/1_Household Data/3_%s/2_Codes/Cooking.Code.csv", Country.Name.2))%>%
        select(cooking_fuel, CF)%>%
        mutate(cooking_fuel = as.character(cooking_fuel))
      
      carbon_pricing_incidence_1 <- carbon_pricing_incidence_1 %>%
        mutate(cooking_fuel = as.character(cooking_fuel))%>%
        left_join(Cooking.Code)
        
    }
    
    if("heating_fuel" %in% colnames(carbon_pricing_incidence_1)){
      Heating.Code <- read_csv(sprintf("../0_Data/1_Household Data/3_%s/2_Codes/Heating.Code.csv", Country.Name.2))%>%
        select(heating_fuel, HF)
      
      carbon_pricing_incidence_1 <- left_join(carbon_pricing_incidence_1, Heating.Code)
    }
    
    if("lighting_fuel" %in% colnames(carbon_pricing_incidence_1)){
      Lighting.Code <- read_csv(sprintf("../0_Data/1_Household Data/3_%s/2_Codes/Lighting.Code.csv", Country.Name.2))%>%
        select(lighting_fuel, LF)
      
      carbon_pricing_incidence_1 <- left_join(carbon_pricing_incidence_1, Lighting.Code)
    }
    
    if(Country.Name != "Chile") {carbon_pricing_incidence_1 <- left_join(carbon_pricing_incidence_1, appliances_0_1)}
    
    print(Country.Name)
    
    data_joint_0 <- data_joint_0 %>%
      bind_rows(carbon_pricing_incidence_1)
    
  }
  
data_joint_0 <- data_joint_0 %>%
  select(hh_id, hh_weights, hh_size, Country, hh_expenditures_USD_2014, everything())%>%
  mutate(hh_expenditures_USD_2014_pc     = hh_expenditures_USD_2014/hh_size,
         log_hh_expenditures_USD_2014    = log(hh_expenditures_USD_2014),
         log_hh_expenditures_USD_2014_pc = log(hh_expenditures_USD_2014_pc))%>%
  mutate(electricity.access = ifelse(Country == "Chile" & exp_USD_Electricity == 0,0,1))%>%
  select(hh_id, hh_weights, hh_size, Country, hh_expenditures_USD_2014,
         urban_01, province, district, village, municipality,
         adults, children, age_hhh, sex_hhh, ind_hhh, ISCED,
         ethnicity, religion, language, 
         # cooking_fuel, lighting_fuel, heating_fuel, water, toilet, edu_hhh
         CF, LF, HF, WTR, TLT, electricity.access,
         starts_with("CO2"), starts_with("exp_"), starts_with("burden_"),
         starts_with("hh_exp"), starts_with("log_hh"), starts_with("Income_Group"), starts_with("share_"),
         starts_with("exp_USD_"), starts_with("inc_"), ends_with(".01"), everything())%>%
  select(-boiler.01, -iron.01, -pump.01, -solar.heater.01, -radio.01, -cooker.01, -vacuum.01, -video.01, -bicycle.01,-sewing.machine.01,
         -sewing_machine.01, -printer.01, -vaccum.01, - mobile.01, -stove.01a,
         -lighting_fuel, -heating_fuel, -cooking_fuel, -water, -toilet, edu_hhh)%>%
  mutate(share_other_binning = ifelse(is.na(share_other_binning),0, share_other_binning))%>%
  mutate(car.01 = ifelse(Country != "Chile" & is.na(car.01),0,car.01),
         CF = ifelse(is.na(CF), "Unknown", CF),
         LF = ifelse(is.na(LF), "Unknown", LF),
         ISCED = ifelse(is.na(ISCED), 9, ISCED),
         Ethnicity = ifelse(is.na(ethnicity) & Country == "Guatemala","Ignorado",Ethnicity),
         Ethnicity = ifelse(is.na(ethnicity) & Country == "Barbados", "Other",Ethnicity))

# 1.2 Several Summary Statistics ####

# General Summary Statistics

Summary_1.2 <- data.frame()

for(i in Country.Set){
  sum_1.2 <- data_joint_0 %>%
    filter(Country == i)%>%
    summarise(number                   = n(),
              hh_size                  = wtd.mean(hh_size, weights = hh_weights),
              urban_01                 = sum(urban_01),
              electricity.access       = sum(electricity.access),
              hh_expenditures_USD_2014 = wtd.mean(hh_expenditures_USD_2014, weights = hh_weights),
              car.01                   = sum(car.01))%>%
    mutate(urban_01                    = ifelse(i == "Argentina",1,urban_01/number),
           electricity.access          = electricity.access/number,
           car.01                      = car.01/number)%>%
    mutate(Country = i)%>%
    select(Country, number, hh_size, urban_01, electricity.access, hh_expenditures_USD_2014, car.01)%>%
    mutate(hh_expenditures_USD_2014 = round(hh_expenditures_USD_2014,0),
           electricity.access = paste0(round(electricity.access*100,1),"%"),
           car.01 = paste0(round(car.01*100,0),"%"),
           urban_01 = paste0(round(urban_01*100,0),"%"),
           hh_size = round(hh_size,2))
  
  Summary_1.2 <- Summary_1.2 %>%
    bind_rows(sum_1.2)
}

colnames(Summary_1.2) <- c("Country", "Observations", "Average Household Size", "Urban Population", "Electricity Access", "Average Household Expenditures", "Car Ownership")

write.xlsx(Summary_1.2, "../1_Carbon_Pricing_Incidence/3_Analyses/1_LAC_2021/2_Tables/Table_Summary_Stats_1/1_Households_General.xlsx")

# Average Expenditure und Energy Expenditure Share Income Groups

Summary_1.3 <- data.frame()

for(i in Country.Set){
  
  sum_1.3.1 <- data_joint_0 %>%
    filter(Country == i)%>%
    summarise(hh_expenditures_USD_2014 = wtd.mean(hh_expenditures_USD_2014, weights = hh_weights),
              share_energy             = wtd.mean(share_energy, weights = hh_weights))
  
  sum_1.3.2 <- data_joint_0 %>%
    filter(Country == i)%>%
    group_by(Income_Group_5)%>%
    summarise(hh_expenditures_USD_2014 = wtd.mean(hh_expenditures_USD_2014, weights = hh_weights),
              share_energy             = wtd.mean(share_energy, weights = hh_weights))%>%
    ungroup()%>%
    pivot_wider(names_from = "Income_Group_5", values_from = c("share_energy", "hh_expenditures_USD_2014"))
  
  sum_1.3.3 <- bind_cols(sum_1.3.1, sum_1.3.2)%>%
    mutate(Country = i)%>%
    select(Country, starts_with("hh_expenditures_USD_2014"), starts_with("share_energy"))
  
  Summary_1.3 <- Summary_1.3 %>%
    bind_rows(sum_1.3.3)
    
}

Summary_1.3 <- Summary_1.3 %>%
  mutate_at(vars(starts_with("hh_expenditures_USD_2014")), list(~ round(.,0)))%>%
  mutate_at(vars(starts_with("share_energy")), list(~ paste0(round(.*100,1), "%")))

colnames(Summary_1.3) <- c("Country", rep(c("All","IG1","IG2","IG3","IG4","IG5"),2))

write.xlsx(Summary_1.3, "../1_Carbon_Pricing_Incidence/3_Analyses/1_LAC_2021/2_Tables/Table_Summary_Stats_1/2_Households_Exp_Share.xlsx")

# Footprint und Burden National

Summary_1.4 <- data.frame()

for(i in Country.Set){
  
  sum_1.4.1 <- data_joint_0 %>%
    filter(Country == i)%>%
    summarise(CO2_t_national      = wtd.mean(CO2_t_national,      weights = hh_weights),
              burden_CO2_national = wtd.mean(burden_CO2_national, weights = hh_weights))
  
  sum_1.4.2 <- data_joint_0 %>%
    filter(Country == i)%>%
    group_by(Income_Group_5)%>%
    summarise(CO2_t_national      = wtd.mean(CO2_t_national,      weights = hh_weights),
              burden_CO2_national = wtd.mean(burden_CO2_national, weights = hh_weights))%>%
    ungroup()%>%
    pivot_wider(names_from = "Income_Group_5", values_from = c("CO2_t_national", "burden_CO2_national"))
  
  sum_1.4.3 <- bind_cols(sum_1.4.1, sum_1.4.2)%>%
    mutate(Country = i)%>%
    select(Country, starts_with("CO2_t_national"), starts_with("burden_CO2_national"))
  
  Summary_1.4 <- Summary_1.4 %>%
    bind_rows(sum_1.4.3)
  
}

Summary_1.4 <- Summary_1.4 %>%
  mutate_at(vars(starts_with("CO2_t_national")), list(~ round(.,1)))%>%
  mutate_at(vars(starts_with("burden_CO2_national")), list(~ paste0(round(.*100,2), "%")))

colnames(Summary_1.4) <- c("Country", rep(c("All","IG1","IG2","IG3","IG4","IG5"),2))

write.xlsx(Summary_1.4, "../1_Carbon_Pricing_Incidence/3_Analyses/1_LAC_2021/2_Tables/Table_Summary_Stats_1/3_Households_CO2_Burden.xlsx")

# 2.0  Decomposing Horizontal Factors ####

# 2.1 Decomposing Additional Cost Burden ####

# 2.1.1 Simple OLS ####

list_2.1.1 <- list()
data_frame_2.1.1 <- data.frame()
ref_list <- data.frame()

for(i in Country.Set){
  
  household_information_0    <- read_csv(sprintf("../1_Carbon_Pricing_Incidence/3_Analyses/1_LAC_2021/4_Transformed Data/household_information_%s_new.csv", i))
  
  data_2.1.1 <- data_joint_0 %>%
    filter(Country == i)
  
  formula_0 <- "burden_CO2_national ~ log_hh_expenditures_USD_2014 + hh_size"
  
  if("urban_01" %in% colnames(household_information_0) & sum(is.na(data_2.1.1$urban_01))==0)           formula_0 <- paste0(formula_0, " + urban_01")
  #if("electricity.access" %in% colnames(household_information_0) & sum(is.na(data_2.1.1$electricity.access))==0) formula_0 <- paste0(formula_0, " + electricity.access")
  if(i != "Chile" & i != "Costa Rica" & sum(is.na(data_2.1.1$car.01))==0)                                                formula_0 <- paste0(formula_0, " + car.01")
  if(i != "Chile" & sum(is.na(data_2.1.1$refrigerator.01))==0)                                                formula_0 <- paste0(formula_0, " + refrigerator.01")
  if("cooking_fuel" %in% colnames(household_information_0) & sum(is.na(data_2.1.1$CF))==0){
    if(i != "Guatemala") formula_0 <- paste0(formula_0, ' + i(CF, ref = "Electricity")')
    if(i == "Guatemala") formula_0 <- paste0(formula_0, ' + i(CF, ref = "Kerosene")')
    }
  #if("lighting_fuel" %in% colnames(household_information_0) & sum(is.na(data_2.1.1$LF))==0)      formula_0 <- paste0(formula_0, " + LF")
  #if("heating_fuel" %in% colnames(household_information_0))      formula_0 <- paste0(formula_0, " + HF")
  if("edu_hhh" %in% colnames(household_information_0) & sum(is.na(data_2.1.1$ISCED))==0)            formula_0 <- paste0(formula_0, " + i(ISCED, ref = 1)")
  if("ethnicity" %in% colnames(household_information_0) & sum(is.na(data_2.1.1$Ethnicity))==0){
    ref_0 <- count(data_2.1.1, Ethnicity)$Ethnicity[which.max(count(data_2.1.1, Ethnicity)$n)]
    
    ref_list <- bind_rows(ref_list, data.frame(Country = i, Type = "Ethnicity", ref = ref_0))
    
    formula_0 <- paste0(formula_0, ' + i(Ethnicity, ref = "', ref_0,'")')
  }
  if("religion" %in% colnames(household_information_0) & sum(is.na(data_2.1.1$religion))==0)           formula_0 <- paste0(formula_0, " + factor(religion)")
  #if("district" %in% colnames(household_information_0) & sum(is.na(data_2.1.1$district))==0)           formula_0 <- paste0(formula_0, " + factor(district)")
  #if("province" %in% colnames(household_information_0) & sum(is.na(data_2.1.1$province))==0)           formula_0 <- paste0(formula_0, " + factor(province)")
  
  formula_1 <- as.formula(formula_0)
  
  model_2.1.1.0 <- feols(formula_1, data = data_2.1.1, weights = data_2.1.1$hh_weights)
  model_2.1.1.1 <- feols(formula_1, data = data_2.1.1, weights = data_2.1.1$hh_weights, fsplit = ~ Income_Group_5)
  
  model_2.1.1.2 <- etable(model_2.1.1.1)%>%
    as_tibble(rownames = NA)%>%
    rownames_to_column()%>%
    separate("model 1", c("model_1", "model_1_SE"), sep = " ")%>%
    separate("model 2", c("model_2", "model_2_SE"), sep = " ")%>%
    separate("model 3", c("model_3", "model_3_SE"), sep = " ")%>%
    separate("model 4", c("model_4", "model_4_SE"), sep = " ")%>%
    separate("model 5", c("model_5", "model_5_SE"), sep = " ")%>%
    separate("model 6", c("model_6", "model_6_SE"), sep = " ")%>%
    mutate(number = 1:n())
  
  model_2.1.1.3 <- model_2.1.1.2 %>%
    select(- ends_with("_SE"))
  
  model_2.1.1.4 <- model_2.1.1.2 %>%
    select(-starts_with("model"), ends_with("_SE"))%>%
    rename_at(vars(starts_with("model")), list(~ str_replace(., "_SE", "")))
  
  model_2.1.1.5 <- rbind(model_2.1.1.3, model_2.1.1.4)%>%
    arrange(number)%>%
    filter(number != 1 | model_2 == 1)%>%
    mutate(model_1 = ifelse(model_1 == "Full", "Full Sample", model_1))%>%
    filter(number != 2 | model_2 == "burden_CO2_national")%>%
    mutate_at(vars(starts_with("model")), list(~ ifelse(. == "burden_CO2_national", "Carbon Price Incidence",.)))%>%
    filter(number != 3)%>%
    filter(!(rowname != lead(rowname) & rowname %in% c("_____________________________________", "S.E. type",
                                                    "Observations", "R2", "Adj. R2")))%>%
    mutate(rowname_1 = ifelse(str_sub(rowname,1,2)  == "CF", paste0("Cooks with ", str_sub(rowname,5,-1)), rowname))%>%
    mutate(rowname_1 = ifelse(str_sub(rowname,1,5) == "ISCED", paste0("ISCED: ", str_sub(rowname,-1,-1)), rowname_1))%>%
    mutate(rowname_1 = ifelse(str_sub(rowname,1,2)  == "HF", paste0("Heats with", str_sub(rowname,3,-1)), rowname_1))%>%
    mutate(rowname_1 = ifelse(str_sub(rowname,1,2)  == "LF", paste0("Lighting Fuel:", str_sub(rowname,3,-1)), rowname_1))%>%
    mutate(rowname_1 = ifelse(str_sub(rowname,1,9) == "Ethnicity", paste0("ETH: ", str_sub(rowname,12,-1)), rowname_1))%>%
    mutate(rowname_1 = ifelse(rowname_1 == "hh_size", "HH Size", 
                             ifelse(rowname_1 == "car.01", "Car Ownership",
                                    ifelse(rowname_1 == "refrigerator.01", "Refrigerator Own.",
                                           ifelse(rowname_1 == "urban_01", "Urban Area",
                                                  ifelse(rowname_1 == "log_hh_expenditures_USD_2014", "HH Exp. (log)", 
                                                         ifelse(rowname_1 == "Sample (Income_Group_5)", "Sample:",rowname_1)))))))%>%
    select(rowname_1, starts_with("model"))%>%
    mutate(rowname_1 = ifelse(rowname_1 == lag(rowname_1) & rowname_1 != "Sample:","",rowname_1))%>%
    mutate(rowname_1 = ifelse(i == "Mexico" & rowname_1 == 'i(var=Ethnicity,ref="Non-Indigeneous")', "ETH: Non-Indigeneous", rowname_1))
  
  tidy_2.1.1.1 <- tidy(model_2.1.1.0)%>%
    mutate(Country = i)
  
  data_frame_2.1.1 <- data_frame_2.1.1 %>%
    bind_rows(tidy_2.1.1.1)
  
  list_2.1.1[[i]] <- model_2.1.1.5
  print(i)
}

write.xlsx(list_2.1.1, "../1_Carbon_Pricing_Incidence/3_Analyses/1_LAC_2021/2_Tables/Table_6_Multifactor_Burden_OLS/Table_6.2_Multifactor_Burden_OLS.xlsx", colNames = FALSE)

data_frame_2.1.1.1 <- data_frame_2.1.1 %>%
  filter(p.value <= 0.05)%>%
  filter(term != "(Intercept)")%>%
  mutate(Type_A = "Burden National",
         Type_B = "OLS")

# Alle Variablen, die signifikant mit burden_CO2_national korrelieren

ref_list_1 <- ref_list

write.xlsx(ref_list_1, "../1_Carbon_Pricing_Incidence/3_Analyses/1_LAC_2021/2_Tables/Table_6_Multifactor_Burden_OLS/Table_6.2_Reference.xlsx")

# 2.1.2 Logit ####

barrier_0 <- data_joint_0 %>%
  group_by(Country)%>%
  summarise(burden_CO2_national_80q = wtd.quantile(burden_CO2_national, probs = 0.90, weights = hh_weights))%>%
  ungroup()

data_2.1.2 <- data_joint_0 %>%
  left_join(barrier_0)%>%
  mutate(affected_more_than_80q_CO2n = ifelse(burden_CO2_national > burden_CO2_national_80q,1,0))

list_2.1.2 <- list()
data_frame_2.1.2 <- data.frame()

ref_list_2 <- data.frame()

for(i in Country.Set){
  
  household_information_0    <- read_csv(sprintf("../1_Carbon_Pricing_Incidence/3_Analyses/1_LAC_2021/4_Transformed Data/household_information_%s_new.csv", i))
  
  data_2.1.2.1 <- data_2.1.2 %>%
    filter(Country == i)
  
  formula_0 <- "affected_more_than_80q_CO2n ~ log_hh_expenditures_USD_2014 + hh_size"
  
  if("urban_01" %in% colnames(household_information_0) & sum(is.na(data_2.1.2.1$urban_01))==0)           formula_0 <- paste0(formula_0, " + urban_01")
  #if("electricity.access" %in% colnames(household_information_0) & sum(is.na(data_2.1.2.1$electricity.access))==0) formula_0 <- paste0(formula_0, " + electricity.access")
  if(i != "Chile" & i != "Costa Rica" & sum(is.na(data_2.1.2.1$car.01))==0)                                                formula_0 <- paste0(formula_0, " + car.01")
  if(i != "Chile" & sum(is.na(data_2.1.2.1$refrigerator.01))==0)                                                formula_0 <- paste0(formula_0, " + refrigerator.01")
  if("cooking_fuel" %in% colnames(household_information_0) & sum(is.na(data_2.1.2.1$CF))==0){
    if(i != "Guatemala") formula_0 <- paste0(formula_0, ' + i(CF, ref = "Electricity")')
    if(i == "Guatemala") formula_0 <- paste0(formula_0, ' + i(CF, ref = "Kerosene")')
  }
  #if("lighting_fuel" %in% colnames(household_information_0) & sum(is.na(data_2.1.2.1$LF))==0)      formula_0 <- paste0(formula_0, " + LF")
  #if("heating_fuel" %in% colnames(household_information_0))      formula_0 <- paste0(formula_0, " + HF")
  if("edu_hhh" %in% colnames(household_information_0) & sum(is.na(data_2.1.2.1$ISCED))==0)            formula_0 <- paste0(formula_0, " + i(ISCED, ref = 1)")
  if("ethnicity" %in% colnames(household_information_0) & sum(is.na(data_2.1.2.1$Ethnicity))==0){
    ref_0 <- count(data_2.1.2.1, Ethnicity)$Ethnicity[which.max(count(data_2.1.2.1, Ethnicity)$n)]
    
    ref_list_2 <- bind_rows(ref_list_2, data.frame(Country = i, Type = "Ethnicity", ref = ref_0))
    
    formula_0 <- paste0(formula_0, ' + i(Ethnicity, ref = "', ref_0,'")')
  }
  if("religion" %in% colnames(household_information_0) & sum(is.na(data_2.1.2.1$religion))==0)           formula_0 <- paste0(formula_0, " + factor(religion)")
  #if("district" %in% colnames(household_information_0) & sum(is.na(data_2.1.2.1$district))==0)           formula_0 <- paste0(formula_0, " + factor(district)")
  #if("province" %in% colnames(household_information_0) & sum(is.na(data_2.1.2.1$province))==0)           formula_0 <- paste0(formula_0, " + factor(province)")

  formula_1 <- as.formula(formula_0)
  model_2.1.2.0 <- feglm(formula_1, data = data_2.1.2.1, weights = data_2.1.2.1$hh_weights, family = quasibinomial("logit"), se = "hetero")
  model_2.1.2.1 <- feglm(formula_1, data = data_2.1.2.1, weights = data_2.1.2.1$hh_weights, family = quasibinomial("logit"), se = "hetero", fsplit = ~ Income_Group_5)
  
  model_2.1.2.2 <- etable(model_2.1.2.1)%>%
    as_tibble(rownames = NA)%>%
    rownames_to_column()%>%
    separate("model 1", c("model_1", "model_1_SE"), sep = " ")%>%
    separate("model 2", c("model_2", "model_2_SE"), sep = " ")%>%
    separate("model 3", c("model_3", "model_3_SE"), sep = " ")%>%
    separate("model 4", c("model_4", "model_4_SE"), sep = " ")%>%
    separate("model 5", c("model_5", "model_5_SE"), sep = " ")%>%
    separate("model 6", c("model_6", "model_6_SE"), sep = " ")%>%
    mutate(number = 1:n())
  
  model_2.1.2.3 <- model_2.1.2.2 %>%
    select(- ends_with("_SE"))
  
  model_2.1.2.4 <- model_2.1.2.2 %>%
    select(-starts_with("model"), ends_with("_SE"))%>%
    rename_at(vars(starts_with("model")), list(~ str_replace(., "_SE", "")))
  
  model_2.1.2.5 <- rbind(model_2.1.2.3, model_2.1.2.4)%>%
    arrange(number)%>%
    filter(number != 1 | model_2 == 1)%>%
    mutate(model_1 = ifelse(model_1 == "Full", "Full Sample", model_1))%>%
    filter(number != 2 | model_2 == "burden_CO2_national")%>%
    mutate_at(vars(starts_with("model")), list(~ ifelse(. == "burden_CO2_national", "Carbon Price Incidence",.)))%>%
    filter(number != 3)%>%
    filter(!(rowname != lead(rowname) & rowname %in% c("_____________________________________", "S.E. type",
                                                       "Observations", "R2", "Adj. R2")))%>%
    mutate(rowname_1 = ifelse(str_sub(rowname,1,2)  == "CF", paste0("Cooks with ", str_sub(rowname,5,-1)), rowname))%>%
    mutate(rowname_1 = ifelse(str_sub(rowname,1,5) == "ISCED", paste0("ISCED: ", str_sub(rowname,-1,-1)), rowname_1))%>%
    mutate(rowname_1 = ifelse(str_sub(rowname,1,2)  == "HF", paste0("Heats with", str_sub(rowname,3,-1)), rowname_1))%>%
    mutate(rowname_1 = ifelse(str_sub(rowname,1,2)  == "LF", paste0("Lighting Fuel:", str_sub(rowname,3,-1)), rowname_1))%>%
    mutate(rowname_1 = ifelse(str_sub(rowname,1,9) == "Ethnicity", paste0("ETH: ", str_sub(rowname,12,-1)), rowname_1))%>%
    mutate(rowname_1 = ifelse(rowname_1 == "hh_size", "HH Size", 
                              ifelse(rowname_1 == "car.01", "Car Ownership",
                                     ifelse(rowname_1 == "refrigerator.01", "Refrigerator Own.",
                                            ifelse(rowname_1 == "urban_01", "Urban Area",
                                                   ifelse(rowname_1 == "log_hh_expenditures_USD_2014", "HH Exp. (log)", 
                                                          ifelse(str_sub(rowname_1,1,6) == "Sample", "Sample:",rowname_1)))))))%>%
    select(rowname_1, starts_with("model"))%>%
    mutate(rowname_1 = ifelse(rowname_1 == lag(rowname_1) & rowname_1 != "Sample:","",rowname_1))%>%
    mutate(rowname_1 = ifelse(i == "Mexico" & rowname_1 == 'i(var=Ethnicity,ref="Non-Indigeneous")', "ETH: Non-Indigeneous", rowname_1))
  
  tidy_2.1.2.1 <- tidy(model_2.1.2.0)%>%
    mutate(Country = i)
  
  data_frame_2.1.2 <- data_frame_2.1.2 %>%
    bind_rows(tidy_2.1.2.1)
  
  list_2.1.2[[i]] <- model_2.1.2.5
  print(i)
}

write.xlsx(list_2.1.2, "../1_Carbon_Pricing_Incidence/3_Analyses/1_LAC_2021/2_Tables/Table_6_Multifactor_Affected_Logit/Table_6.1_Multifactor_Affected_Logit.xlsx", colNames = FALSE)

data_frame_2.1.2.1 <- data_frame_2.1.2 %>%
  filter(p.value <= 0.05)%>%
  filter(term != "(Intercept)")%>%
  mutate(Type_A = "Affected 80",
         Type_B = "Logit")

ref_list_2 <- ref_list_2

write.xlsx(ref_list_2, "../1_Carbon_Pricing_Incidence/3_Analyses/1_LAC_2021/2_Tables/Table_6_Multifactor_Affected_Logit/Table_6.1_Reference.xlsx")


# 2.1.3 Fields Decomposition ####

# From Israel / Sinem
data_2.1.3.0 <- data.frame()
decomposition_Fields <- list()

for(i in Country.Set){
  
  data_2.1.3.1 <- data_joint_0 %>%
    filter(Country == i)%>%
    mutate(Income_Group_5 = as.character(Income_Group_5))
  
  data_2.1.3.2 <- data_joint_0 %>%
    filter(Country == i)%>%
    mutate(Income_Group_5 = "Full Sample")
  
  data_2.1.3.3 <- bind_rows(data_2.1.3.1, data_2.1.3.2)
  
  df_2.1.3 <- data.frame()
  
  for(j in c(1,2,3,4,5, "Full Sample")){
    data_2.1.3.4 <- data_2.1.3.3 %>%
      filter(Income_Group_5 == j)
    
    variance_incidence <- wtd.var(data_2.1.3.4$burden_CO2_national, weights = data_2.1.3.4$hh_weights)
    
    household_information_0    <- read_csv(sprintf("../1_Carbon_Pricing_Incidence/3_Analyses/1_LAC_2021/4_Transformed Data/household_information_%s_new.csv", i))
    
    formula_0 <- "burden_CO2_national ~ log_hh_expenditures_USD_2014 + hh_size"
    
    if("urban_01" %in% colnames(household_information_0) & sum(is.na(data_2.1.3.4$urban_01))==0)           formula_0 <- paste0(formula_0, " + urban_01")
    #if("electricity.access" %in% colnames(household_information_0) & sum(is.na(data_2.1.3.4$electricity.access))==0) formula_0 <- paste0(formula_0, " + electricity.access")
    if(i != "Chile" & i != "Costa Rica" & sum(is.na(data_2.1.3.4$car.01))==0)                                                formula_0 <- paste0(formula_0, " + car.01")
    if(i != "Chile" & sum(is.na(data_2.1.3.4$refrigerator.01))==0)                                                formula_0 <- paste0(formula_0, " + refrigerator.01")
    if("cooking_fuel" %in% colnames(household_information_0) & sum(is.na(data_2.1.3.4$CF))==0)       formula_0 <- paste0(formula_0, " + CF")
    #if("lighting_fuel" %in% colnames(household_information_0) & sum(is.na(data_2.1.3.4$LF))==0 & i != "Costa Rica" & i != "Uruguay")      formula_0 <- paste0(formula_0, " + LF")
    #if("heating_fuel" %in% colnames(household_information_0))      formula_0 <- paste0(formula_0, " + HF")
    if("edu_hhh" %in% colnames(household_information_0) & sum(is.na(data_2.1.3.4$ISCED))==0)            formula_0 <- paste0(formula_0, " + factor(ISCED)")
    if("ethnicity" %in% colnames(household_information_0) & sum(is.na(data_2.1.3.4$Ethnicity))==0)          formula_0 <- paste0(formula_0, " + factor(Ethnicity)")
    #if("religion" %in% colnames(household_information_0) & sum(is.na(data_2.1.3.4$religion))==0)           formula_0 <- paste0(formula_0, " + factor(religion)")
    #if("district" %in% colnames(household_information_0) & sum(is.na(data_2.1.3.4$district))==0)           formula_0 <- paste0(formula_0, " + factor(district)")
    #if("province" %in% colnames(household_information_0) & sum(is.na(data_2.1.3.4$province))==0)           formula_0 <- paste0(formula_0, " + factor(province)")
    
    formula_1 <- as.formula(formula_0)
    
    model_2.1.3.4 <- lm(formula_1, data = data_2.1.3.4, weights = hh_weights)
    
    prediction_2.1.3.4 <- as.data.frame(predict.lm(model_2.1.3.4, data_2.1.3.4, type = "terms"))%>%
      mutate(residuals = resid(model_2.1.3.4))
    
    correlations <- sapply(prediction_2.1.3.4, function(.) corr(d = cbind(., data_2.1.3.4$burden_CO2_national), w = data_2.1.3.4$hh_weights))
    variance     <- sapply(prediction_2.1.3.4, function(x) wtd.var(x,                                             weights = data_2.1.3.4$hh_weights))
    
    joined_0 <- cbind(correlations, variance)
    joined_1 <- cbind(joined_0, rownames(joined_0))%>%
      as_tibble()%>%
      rename(factor = V3)%>%
      select(factor, everything())%>%
      mutate(var_inc      = variance_incidence,
             correlations = as.numeric(correlations),
             variance     = as.numeric(variance))%>%
      mutate(covariance   = correlations*sqrt(variance*var_inc),
             s_j       = covariance/var_inc,
             Income_Group_5 = j,
             Country        = i,
             R_squared = summary(model_2.1.3.4)$r.squared,
             p_j = ifelse(factor != "residuals", s_j/R_squared,NA))%>%
      select(factor, Income_Group_5, p_j)%>%
      rename(s_k = p_j)%>%
      mutate(factor = ifelse(factor == "hh_size", "HH Size",
                             ifelse(factor == "car.01", "Car Ownership",
                                    ifelse(factor == "refrigerator.01", "Refrigerator Own.",
                                           ifelse(factor == "urban_01", "Urban Area",
                                                  ifelse(factor == "log_hh_expenditures_USD_2014", "HH Exp. (log)", 
                                                         ifelse(factor == "CF", "Cooking Fuel",
                                                                ifelse(factor == "factor(ISCED)", "Education",
                                                                       ifelse(factor == "factor(Ethnicity)", "Ethnicity",
                                                                              ifelse(factor == "residuals", factor, factor))))))))))%>%
      rename("Sample:" = factor)

    df_2.1.3 <- df_2.1.3 %>%
      bind_rows(joined_1)
    
  }
  
  df_2.1.3 <- df_2.1.3 %>%
    pivot_wider(names_from = "Income_Group_5", values_from = "s_k")%>%
    mutate_at(vars(-factor), list(~ round(.,3)))%>%
    filter(factor != "residuals")%>%
    select(factor, 'Full Sample', everything())
  
  data_2.1.3.0 <- data_2.1.3.0 %>%
    bind_rows(df_2.1.3)
  print(i)
  decomposition_Fields[[i]] <- df_2.1.3
  
}

write.xlsx(decomposition_Fields, "../1_Carbon_Pricing_Incidence/3_Analyses/1_LAC_2021/2_Tables/Table_6_Multifactor_Fields_Decomp/Table_6.1_Multifactor_Fields_Decomp.xlsx")

data_frame_2.1.3.1 <- data_2.1.3.0 %>%
  mutate(Type_A = "Burden National",
         Type_B = "Fields")

t <- data_frame_2.1.3.1 %>%
  filter(Income_Group_5 == "Whole Sample")%>%
  arrange(Country, -p_j)%>%
  group_by(Country)%>%
  mutate(cumsum_p_j = cumsum(p_j))%>%
  ungroup()%>%
  select(Country, factor, p_j, cumsum_p_j)


# 2.2 Decomposing Loosers + No Access to Transfers ####

# 2.2.1 Simple OLS ####

# 2.2.2 Logit ####

barrier_0 <- data_joint_0 %>%
  group_by(Country)%>%
  summarise(burden_CO2_national_80q = wtd.quantile(burden_CO2_national, probs = 0.90, weights = hh_weights))%>%
  ungroup()

data_2.2.2 <- data_joint_0 %>%
  left_join(barrier_0)%>%
  mutate(affected_more_than_80q_CO2n = ifelse(burden_CO2_national > burden_CO2_national_80q,1,0),
         access_to_transfers = ifelse((!is.na(inc_gov_cash)|!is.na(inc_gov_monetary))&(inc_gov_cash > 0 | inc_gov_monetary > 0),1,0),
         affected_80_no_transfers = ifelse(affected_more_than_80q_CO2n == 1 & access_to_transfers == 0,1,0))

list_2.2.2 <- list()
data_frame_2.2.2 <- data.frame()
ref_list_3 <- data.frame()

for(i in Country.Set){

  household_information_0    <- read_csv(sprintf("../1_Carbon_Pricing_Incidence/3_Analyses/1_LAC_2021/4_Transformed Data/household_information_%s_new.csv", i))
  
  data_2.2.2.1 <- data_2.2.2 %>%
    filter(Country == i)
  
  formula_0 <- "affected_80_no_transfers ~ log_hh_expenditures_USD_2014 + hh_size"
  
  if("urban_01" %in% colnames(household_information_0) & sum(is.na(data_2.2.2.1$urban_01))==0)           formula_0 <- paste0(formula_0, " + urban_01")
  #if("electricity.access" %in% colnames(household_information_0) & sum(is.na(data_2.2.2.1$electricity.access))==0) formula_0 <- paste0(formula_0, " + electricity.access")
  if(i != "Chile" & sum(is.na(data_2.2.2.1$car.01))==0)                                                formula_0 <- paste0(formula_0, " + car.01")
  if(i != "Chile" & sum(is.na(data_2.2.2.1$refrigerator.01))==0)                                                formula_0 <- paste0(formula_0, " + refrigerator.01")
  if("cooking_fuel" %in% colnames(household_information_0) & sum(is.na(data_2.2.2.1$CF))==0){
    if(i != "Guatemala") formula_0 <- paste0(formula_0, ' + i(CF, ref = "Electricity")')
    if(i == "Guatemala") formula_0 <- paste0(formula_0, ' + i(CF, ref = "Kerosene")')
  }
  #if("lighting_fuel" %in% colnames(household_information_0) & sum(is.na(data_2.2.2.1$LF))==0)      formula_0 <- paste0(formula_0, " + LF")
  #if("heating_fuel" %in% colnames(household_information_0))      formula_0 <- paste0(formula_0, " + HF")
  if("edu_hhh" %in% colnames(household_information_0) & sum(is.na(data_2.2.2.1$ISCED))==0)            formula_0 <- paste0(formula_0, " + i(ISCED, ref = 1)")
  if("ethnicity" %in% colnames(household_information_0) & sum(is.na(data_2.2.2.1$Ethnicity))==0){
    ref_0 <- count(data_2.2.2.1, Ethnicity)$Ethnicity[which.max(count(data_2.2.2.1, Ethnicity)$n)]
    
    ref_list_3 <- bind_rows(ref_list_3, data.frame(Country = i, Type = "Ethnicity", ref = ref_0))
    
    formula_0 <- paste0(formula_0, ' + i(Ethnicity, ref = "', ref_0,'")')
  }
  if("religion" %in% colnames(household_information_0) & sum(is.na(data_2.2.2.1$religion))==0)           formula_0 <- paste0(formula_0, " + factor(religion)")
  #if("district" %in% colnames(household_information_0) & sum(is.na(data_2.2.2.1$district))==0)           formula_0 <- paste0(formula_0, " + factor(district)")
  #if("province" %in% colnames(household_information_0) & sum(is.na(data_2.2.2.1$province))==0)           formula_0 <- paste0(formula_0, " + factor(province)")
  
  formula_1 <- as.formula(formula_0)
  model_2.2.2.0 <- feglm(formula_1, data = data_2.2.2.1, weights = data_2.2.2.1$hh_weights, family = quasibinomial("logit"), se = "hetero")
  model_2.2.2.1 <- feglm(formula_1, data = data_2.2.2.1, weights = data_2.2.2.1$hh_weights, family = quasibinomial("logit"), se = "hetero", fsplit = ~ Income_Group_5)
  
  model_2.2.2.2 <- etable(model_2.2.2.1)%>%
    as_tibble(rownames = NA)%>%
    rownames_to_column()%>%
    separate("model 1", c("model_1", "model_1_SE"), sep = " ")%>%
    separate("model 2", c("model_2", "model_2_SE"), sep = " ")%>%
    separate("model 3", c("model_3", "model_3_SE"), sep = " ")%>%
    separate("model 4", c("model_4", "model_4_SE"), sep = " ")%>%
    separate("model 5", c("model_5", "model_5_SE"), sep = " ")%>%
    separate("model 6", c("model_6", "model_6_SE"), sep = " ")%>%
    mutate(number = 1:n())
  
  model_2.2.2.3 <- model_2.2.2.2 %>%
    select(- ends_with("_SE"))
  
  model_2.2.2.4 <- model_2.2.2.2 %>%
    select(-starts_with("model"), ends_with("_SE"))%>%
    rename_at(vars(starts_with("model")), list(~ str_replace(., "_SE", "")))
  
  model_2.2.2.5 <- rbind(model_2.2.2.3, model_2.2.2.4)%>%
    arrange(number)%>%
    filter(number != 1 | model_2 == 1)%>%
    mutate(model_1 = ifelse(model_1 == "Full", "Full Sample", model_1))%>%
    filter(number != 2 | model_2 == "affected_80_no_transfers")%>%
    mutate_at(vars(starts_with("model")), list(~ ifelse(. == "affected_80_no_transfers", "Theta_i",.)))%>%
    filter(number != 3)%>%
    filter(!(rowname != lead(rowname) & rowname %in% c("_____________________________________", "S.E. type",
                                                       "Observations", "R2", "Adj. R2")))%>%
    mutate(rowname_1 = ifelse(str_sub(rowname,1,2)  == "CF", paste0("Cooks with ", str_sub(rowname,5,-1)), rowname))%>%
    mutate(rowname_1 = ifelse(str_sub(rowname,1,5) == "ISCED", paste0("ISCED: ", str_sub(rowname,-1,-1)), rowname_1))%>%
    mutate(rowname_1 = ifelse(str_sub(rowname,1,2)  == "HF", paste0("Heats with", str_sub(rowname,3,-1)), rowname_1))%>%
    mutate(rowname_1 = ifelse(str_sub(rowname,1,2)  == "LF", paste0("Lighting Fuel:", str_sub(rowname,3,-1)), rowname_1))%>%
    mutate(rowname_1 = ifelse(str_sub(rowname,1,9) == "Ethnicity", paste0("ETH: ", str_sub(rowname,12,-1)), rowname_1))%>%
    mutate(rowname_1 = ifelse(rowname_1 == "hh_size", "HH Size", 
                              ifelse(rowname_1 == "car.01", "Car Ownership",
                                     ifelse(rowname_1 == "refrigerator.01", "Refrigerator Own.",
                                            ifelse(rowname_1 == "urban_01", "Urban Area",
                                                   ifelse(rowname_1 == "log_hh_expenditures_USD_2014", "HH Exp. (log)", 
                                                          ifelse(str_sub(rowname_1,1,6) == "Sample", "Sample:",rowname_1)))))))%>%
    select(rowname_1, starts_with("model"))%>%
    mutate(rowname_1 = ifelse(rowname_1 == lag(rowname_1) & rowname_1 != "Sample:","",rowname_1))%>%
    mutate(rowname_1 = ifelse(i == "Mexico" & rowname_1 == 'i(var=Ethnicity,ref="Non-Indigeneous")', "ETH: Non-Indigeneous", rowname_1))
  
  tidy_2.2.2.1 <- tidy(model_2.2.2.0)%>%
    mutate(Country = i)
  
  data_frame_2.2.2 <- data_frame_2.2.2 %>%
    bind_rows(tidy_2.2.2.1)
  
  list_2.2.2[[i]] <- model_2.2.2.5
  print(i)
  
}

write.xlsx(list_2.2.2, "../1_Carbon_Pricing_Incidence/3_Analyses/1_LAC_2021/2_Tables/Table_7_Multifactor_Affected_Logit/Table_7.1_Multifactor_Affected_Logit.xlsx", colNames = FALSE)

data_frame_2.2.2.1 <- data_frame_2.2.2 %>%
  filter(p.value <= 0.05)%>%
  filter(term != "(Intercept)")%>%
  mutate(Type_A = "Affected 80 & No Transfers",
         Type_B = "Logit")

ref_list_3 <- ref_list_3

write.xlsx(ref_list_3, "../1_Carbon_Pricing_Incidence/3_Analyses/1_LAC_2021/2_Tables/Table_7_Multifactor_Affected_Logit/Table_7.1_Reference.xlsx")


# 2.2.3 Fields Decomposition ####

  

# 2.2.4 How to characterize the households, which loose and have no access to transfers? ####

data_2.2.4.1 <- data_2.2.2 %>%
  filter(affected_80_no_transfers == 1)%>%
  mutate(LPG = ifelse(CF == "LPG",1,0),
         Firewood = ifelse(CF == "Firewood" | CF == "Firewood Charcoal",1,0),
         Gas = ifelse(CF == "Gas",1,0))%>%
  group_by(Country)%>%
  summarise(total                    = sum(hh_weights),
         car.01                   = sum(hh_weights[car.01 == 1]),
         urban_01                 = sum(hh_weights[urban_01 == 1]),
         hh_expenditures_USD_2014 = wtd.mean(hh_expenditures_USD_2014, weights = hh_weights),
         LPG = sum(hh_weights[LPG == 1]),
         Gas = sum(hh_weights[Gas == 1]),
         Firewood = sum(hh_weights[Firewood == 1]))%>%
  ungroup()%>%
  mutate(car.01   = car.01/total,
         urban_01 = urban_01/total,
         LPG = LPG/total,
         Gas = Gas/total,
         Firewood = Firewood/total)%>%
  select(Country, hh_expenditures_USD_2014, car.01, urban_01, LPG, Gas, Firewood)

data_2.2.4.2 <- data_2.2.2 %>%
    mutate(LPG    = ifelse(CF == "LPG",1,0),
         Firewood = ifelse(CF == "Firewood" | CF == "Firewood Charcoal",1,0),
         Gas      = ifelse(CF == "Gas",1,0))%>%
  group_by(Country)%>%
  summarise(total                    = sum(hh_weights),
            car.01                   = sum(hh_weights[car.01 == 1]),
            urban_01                 = sum(hh_weights[urban_01 == 1]),
            hh_expenditures_USD_2014C = wtd.mean(hh_expenditures_USD_2014, weights = hh_weights),
            LPG = sum(hh_weights[LPG == 1]),
            Gas = sum(hh_weights[Gas == 1]),
            Firewood = sum(hh_weights[Firewood == 1]))%>%
  ungroup()%>%
  mutate(car.01C   = car.01/total,
         urban_01C = urban_01/total,
         LPGC = LPG/total,
         GasC = Gas/total,
         FirewoodC = Firewood/total)%>%
  select(Country, hh_expenditures_USD_2014C, car.01C, urban_01C, LPGC, GasC, FirewoodC)

data_2.2.4.3 <- left_join(data_2.2.4.1, data_2.2.4.2)%>%
  select(Country, starts_with("hh"), starts_with("car"), starts_with("urban"), starts_with("LPG"), starts_with("Gas"), starts_with("Firewood"))%>%
  mutate_at(vars(starts_with("hh")), list(~ round(.,0)))%>%
  mutate_at(vars(car.01:FirewoodC), list(~round(.,3)))%>%
  mutate_at(vars(car.01:FirewoodC), list(~ ifelse(. == 0,NA,.)))%>%
  mutate_at(vars(setdiff(ends_with("C"), starts_with("hh"))), list(~ ifelse(!is.na(.),paste0("(",.*100,"%)"),.)))

write.xlsx(data_2.2.4.3, "../1_Carbon_Pricing_Incidence/3_Analyses/1_LAC_2021/2_Tables/Table_Summary_Stats_3/Affected_no_Transfers_vs_All.xlsx")


# 3   Cross-Sectional Analysis ####
# 3.1 Regressions across regions ####
# 3.2 Maps ####

# 9.00 Test-Analysis for Adrien 

education.code               <- read_csv(sprintf("../0_Data/1_Household Data/%s/2_Codes/Education.Code.csv", path_0))%>%
  rename(Education = Label)
urban.code                   <- read_csv(sprintf("../0_Data/1_Household Data/%s/2_Codes/Urban.Code.csv", path_0))
ethnicity.code               <- read_csv(sprintf("../0_Data/1_Household Data/%s/2_Codes/Ethnicity.Code.csv", path_0))%>%
  rename(Ethnicity = Label)
district.code                <- read_csv(sprintf("../0_Data/1_Household Data/%s/2_Codes/District.Code.csv", path_0)) 

data_0 <- left_join(data_0, education.code, by = c("edu_hhh" = "education"))%>%
  mutate(Urban = ifelse(urban_01 == 1, "Urban", "Rural"))%>%
  left_join(ethnicity.code)%>%
  #left_join(district.code)%>%
  mutate(mean_burden   = wtd.mean(burden_CO2_national, weights = hh_weights),
         dev_burden    = burden_CO2_national - mean_burden)%>%
  mutate(more_than_5   = ifelse(burden_CO2_national > 0.05,1,0),
         define_top_30 = wtd.quantile(burden_CO2_national, probs = 0.7, weights = hh_weights),
         top_30        = ifelse(burden_CO2_national > define_top_30,1,0))

# 2.01 Test-Analysis for Adrien 

model_1 <- lm(burden_CO2_national ~ factor(Income_Group_5), data = data_0, weights = hh_weights)

model_1 <- feols(c(burden_CO2_national, dev_burden) ~ factor(Income_Group_5),                          data = data_0, weights = data_0$hh_weights)
# model_2 <- feols(burden_CO2_national ~ i(Income_Group_5) + log_total_expenditures, data = data_0, weights = data_0$hh_weights)
model_3 <- feols(burden_CO2_national ~ log_total_expenditures,                     data = data_0, weights = data_0$hh_weights)
model_4 <- feols(burden_CO2_national ~ log_total_expenditures + Urban + Education +
                   hh_size + Ethnicity + car.01, data = data_0, weights = data_0$hh_weights)
model_5 <- feols(burden_CO2_national ~ factor(Income_Group_5) + Urban + Education +
                   hh_size + Ethnicity + car.01 + i(province, ref = "Guayas"), data = data_0, weights = data_0$hh_weights)

t_1 <- tidy(model_1)
t_3 <- tidy(model_3)
t_4 <- tidy(model_4)
t_5 <- tidy(model_5)
t_6 <- tidy(model_6)

t_all <- list("Income Group only" = t_1, "Log Expenditures only" = t_3, "Income Group + Covariates" = t_4, "Log Expenditures + Covariates" = t_5)

write.xlsx(t_all, sprintf("../1_Carbon_Pricing_Incidence/3_Analyses/9_Adrien_Regressions/Multiple_Regression_%s.xlsx", Country.Name))

model_6 <- glm(top_30 ~ log_total_expenditures + Urban + Education +  hh_size + car.01, data = data_0, family = quasibinomial(logit))
summary(model_6)

model_7 <- feols(share_energy ~ log_total_expenditures + Urban + Education + hh_size +  car.01, data = data_0, weights = data_0$hh_weights)
summary(model_7)


