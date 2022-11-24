if(!require("pacman")) install.packages("pacman")

p_load("countrycode", "haven", "Hmisc", "openxlsx", "rattle", "scales", "tidyverse")

options(scipen=999)

# Authors: P. Blechschmidt, L. Missbach

raw_per     <-  read_dta("../0_Data/1_Household Data/4_United_Kingdom/1_Data_Raw/2018_rawper_ukanon_final.dta")
raw_hh      <-  read_dta("../0_Data/1_Household Data/4_United_Kingdom/1_Data_Raw/2018_rawhh_ukanon.dta")
derived_hh  <-  read_dta("../0_Data/1_Household Data/4_United_Kingdom/1_Data_Raw/2018_dvhh_ukanon.dta")
derived_per <-  read_dta("../0_Data/1_Household Data/4_United_Kingdom/1_Data_Raw/2018_dvper_ukanon201819.dta")
exp         <-  read_dta("../0_Data/1_Household Data/4_United_Kingdom/1_Data_Raw/2018_dv_set89_ukanon.dta")
codes       <- read.xlsx("../0_Data/1_Household Data/4_United_Kingdom/9_Documentation/Item_Codes.xlsx")

Variable.Description <- data.frame()

for(i in c(1:1917)){
  var_0 <- data.frame("Variable" = colnames(derived_hh[i]),
                      "Label"    = attr(derived_hh[[i]], 'label'))
  Variable.Description <- bind_rows(Variable.Description, var_0)
}

# write.xlsx(Variable.Description, "../0_Data/1_Household Data/4_United_Kingdom/9_Documentation/Variable.Description_HH.xlsx")

Variable.Description_P <- data.frame()

for(i in c(1:291)){
  var_0 <- data.frame("Variable" = colnames(derived_per[i]),
                      "Label"    = attr(derived_per[[i]], 'label'))
  Variable.Description_P <- bind_rows(Variable.Description_P, var_0)
}

# write.xlsx(Variable.Description_P, "../0_Data/1_Household Data/4_United_Kingdom/9_Documentation/Variable.Description_PER.xlsx")

# Transform data ####

household_information_1 <- derived_hh %>%
  rename(hh_id = case, hh_size = A049, province = Gorx, hh_weights = weighta)%>%
  mutate_at(vars(starts_with("URGrid")), list(~ ifelse(is.na(.),3,.)))%>%
  mutate(urban_01 = ifelse(URGridEWp == 1 | URGridSCp == 1,1,
                           ifelse(URGridEWp == 2 | URGridSCp == 2,0,NA)))%>%
  mutate(children = A040 + A041 + A042, 
         adults   = A043 + A044 + A045 + A046 + A047)%>%
  select(hh_id, hh_size, adults, children, urban_01, province, hh_weights, A150:A156)%>%
  mutate(hh_weights = hh_weights*1000)%>%
  mutate(heating_fuel = ifelse(A150 == 1, 1,
                               ifelse(A151 == 1,2,
                                      ifelse(A152 == 1,3,
                                             ifelse(A153 == 1,4,
                                                    ifelse(A154 == 1,5,
                                                           ifelse(A155 == 1,6,
                                                                  ifelse(A156 == 1,7,8))))))))%>%
  select(hh_id, hh_size, hh_weights, adults, children, urban_01, province, heating_fuel)

household_information_2 <- derived_per %>%
  rename(hh_id = case, sex_hhh = A004, ethnicity = a012p)%>%
  filter(A002 == 1)%>%
  select(hh_id, sex_hhh, ethnicity)

household_information_3 <- derived_hh %>%
  rename(hh_id = case)%>%
  select(hh_id, P328p, P332, P336)%>%
  mutate(inc_gov_monetary = P328p + P332 + P336,
         inc_gov_cash     = 0)%>%
  select(hh_id, inc_gov_cash, inc_gov_monetary)

household_information <- left_join(household_information_1, household_information_2)%>%
  left_join(household_information_3)%>%
  write_csv(., "../0_Data/1_Household Data/4_United_Kingdom/1_Data_Clean/household_information_United_Kingdom.csv")
  
# Expenditures ####


#clean up item code file
codes_long <- read.xlsx("./4_United_Kingdom/9_Documentation/8686_volume_d_expenditure_codes_201819.xlsx", sheet=3)
  
codes <- codes_long%>%
  select(code=COIPLUS.Code, description = COIPLUS.Description)%>%
  distinct(.)

write.xlsx(codes,"./4_United_Kingdom/9_Documentation/Item_Codes.xlsx")

#other codes

# new_codes <- read.xlsx("./4_United_Kingdom/9_Documentation/8686_volume_d_expenditure_codes_201819.xlsx", sheet = 3)
# LCF<- new_codes %>%
#   select(code = LCF.CODE, description = LCF.Description)%>%
#   distinct(.)
# COIP <- new_codes%>%
#   select(code = COIPLUS.Code, description = COIPLUS.Description)%>%
#   distinct(.)
# codes_description <- COIP %>%
#   mutate(contained = ifelse(code %in% exp$COI_PLUS, 1, 0))#%>%
#   #filter(contained == 1)%>%
#   #select(code, description)

# der_var_desc <-read.xlsx("../0_Data/1_Household Data/4_United_Kingdom/9_Documentation/8686_volume_f_derived_variables_201819.xlsx",
#                          sheet = "Part 2", startRow = 7)
# dvdes <- der_var_desc %>%
#   filter(`In.the.UKDA.dataset?` == "Yes")%>%
#   select(Variable, Description)
# 
# dvde<-(dvdes[c(441:1893),])%>%
#   subset(!grepl("[c,C]\\d{5}[c,w, x, y, z, l, t]", Variable)) %>%
#   subset(!grepl("[c,C][a-z,A-Z]\\d{4}[c,w, x, y, z, l, t]", Variable))%>%
#   subset(!grepl("[c,C][a-z,A-Z]\\d{3}[a-z,A-Z][c,w, x, y, z, l, t]", Variable))%>%
#   subset(!grepl("[c,C]\\d{4}[a-z,A-Z][c,w, x, y, z, l, t]", Variable))%>%
#   subset(!grepl("[p,P]\\d{3}[c,w, x, y, z, l, t]", Variable))

expenditures_items_0 <- derived_hh %>%
  select(case, starts_with("C"))%>%
  #select(case, starts_with("C") & !ends_with("c") & !ends_with("w") & !ends_with("t"))%>%
  select(order(colnames(expenditures_items_0)))

expenditures_items <- derived_hh %>%
  select(case, starts_with("C"))%>%
  select(case, starts_with("C") & ends_with("c"))

# 382 Vars

var_0 <- data.frame("Col" = colnames(expenditures_items_0))%>%
  mutate(code = str_replace(Col, "C", ""))%>%
  mutate(type = "raw")

var_1 <- data.frame("Col" = colnames(expenditures_items))%>%
  mutate(code = str_replace(Col, "C",""))%>%
  mutate(code = str_replace(code, "c",""))%>%
  mutate(Type = "c")

expenditures_items_2 <- derived_hh %>%
  select(case, starts_with("C"))%>%
  select(case, starts_with("C") & ends_with("t"))

# 379 Vars

var_2 <- data.frame("Col" = colnames(expenditures_items_2))%>%
  mutate(code = str_replace(Col, "C",""))%>%
  mutate(code = str_replace(code, "t",""))%>%
  mutate(Type = "t")

vars <- full_join(var_1, var_2, by = "code")

# Expenditures are missing

# expenditures <- derived_hh %>%
#   select(case, matches("C\\d{5}") & !ends_with(c("c", "w", "x", "y", "z", "l", "t")), 
#          matches("C[A-Z]{1}\\d{4}")& !ends_with(c("c", "w", "x", "y", "z", "l", "t")))
# 
# #prepare and save expenditures
# expenditures_items_United_Kingdom <- expenditures%>%
#   rename(hh_id = case)%>%
#   pivot_longer(-c(hh_id), names_to="item_codes", values_to = "expenditures_year") %>%
#   arrange(expenditures_year, item_codes)%>%
#   #expenditures are recorded in weekly amounts, multiply by 52 to get "yearly"
#   mutate(expenditures_year= expenditures_year*52)%>%
#   write_csv(., "./4_United_Kingdom/1_Data_Clean/expenditures_items_United_Kingdom.csv")
#   
# item_codes <- expenditures%>%
#   filter(case == 1)%>%
#   pivot_longer(-c(case), names_to="Variable", values_to = "expenditures_year")%>%
#   mutate(Variable = tolower(Variable))%>%
#   merge(., dvde, by = "Variable")%>%
#   select(item_code = Variable, item_name = Description)#%>%
#   write.xlsx(., "4_United_Kingdom/3_Matching_Tables/Item_Codes_Description_United_Kingdom.xlsx")
# 
# expenditures2 <- exp%>%
#   rename(code = COI_PLUS)%>%
#   group_by(case, code)%>%
#   summarise(exp = sum(pdamount))%>%
#   ungroup()
# tmp <- filter(expenditures2, Person == 1)
# x <- distinct(tmp, case)
# expenditures2 <- expenditures2%>%
#   mutate(Person = ifelse(!(case %in% x$case), 1, Person))%>%
#   filter(Person == 1)%>%
#   select(case, code, exp)%>%
#   write_csv(., "./4_United_Kingdom/1_Data_Clean/expenditures_items2_United_Kingdom.csv")
# 
# expenditures2_codes <- distinct(expenditures2, code, .keep_all = TRUE)%>%
#   merge(., codes, by = "code")%>%
#   select(item_code = code, item_name = description)%>%
#   write.xlsx(., "4_United_Kingdom/3_Matching_Tables/Item_Codes_Description2_United_Kingdom.xlsx")

# Codes ####

Heating.Code <- data.frame(heating_fuel = c(seq(1,8,1), Heating_Fuel = c("Electric central heating",
                                                                         "Gas central heating",
                                                                         "Oil central heating",
                                                                         "Solid fuel central heating",
                                                                         "Solid fuel or oil central heating",
                                                                         "Calor gas central heating",
                                                                         "Other gas central heating", 
                                                                         "Unknown")))%>%
  write_csv(., "../0_Data/1_Household Data/4_United_Kingdom/2_Codes/Heating.Code.csv")
Province.Code <- stack(attr(derived_hh$Gorx, 'labels'))%>%
  rename(province = values, Province = ind)%>%
  write_csv(., "../0_Data/1_Household Data/4_United_Kingdom/2_Codes/Province.Code.csv")
Gender.Code <- stack(attr(derived_per$A004, 'labels'))%>%
  rename(sex_hh = values, Gender = ind)%>%
  write_csv(., "../0_Data/1_Household Data/4_United_Kingdom/2_Codes/Gender.Code.csv")
Ethnicity.Code <- stack(attr(derived_per$a012p, 'labels'))%>%
  rename(ethnicity = values, Ethnicity = ind)%>%
  write_csv(., "../0_Data/1_Household Data/4_United_Kingdom/2_Codes/Ethnicity.Code.csv")

# Appliances ####

appliance_0_1 <- derived_hh %>%
  rename(hh_id = case, washing_machine.01 = A108, car.01 = A149, refrigerator.01 = A164, computer.01 = A1661, tv.01 = A1711,
         dryer.01 = A167, microwave.01 = A168)%>%
  mutate(washing_machine.01 = ifelse(washing_machine.01 == 1,1,0),
         car.01             = ifelse(car.01 > 0,1,0),
         refrigerator.01    = ifelse(refrigerator.01 == 1,1,0))%>%
  mutate_at(vars(computer.01:microwave.01), list(~ ifelse(. == 1,1,0)))%>%
  select(hh_id, ends_with(".01"))%>%
  write_csv(., "../0_Data/1_Household Data/4_United_Kingdom/1_Data_Clean/appliances_0_1_United_Kingdom.csv")
