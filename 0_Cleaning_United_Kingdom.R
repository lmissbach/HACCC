if(!require("pacman")) install.packages("pacman")

p_load("countrycode", "haven", "Hmisc", "openxlsx", "rattle", "scales", "tidyverse")

options(scipen=999)

# Authors: P. Blechschmidt, L. Missbach

# raw_per     <-  read_dta("../0_Data/1_Household Data/4_United_Kingdom/1_Data_Raw/2018_rawper_ukanon_final.dta")
# raw_hh      <-  read_dta("../0_Data/1_Household Data/4_United_Kingdom/1_Data_Raw/2018_rawhh_ukanon.dta")
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

Variable.Description <- Variable.Description %>%
  arrange(Variable)

# write.xlsx(Variable.Description, "../0_Data/1_Household Data/4_United_Kingdom/9_Documentation/Variable.Description_HH.xlsx")

Variable.Description_P <- data.frame()

for(i in c(1:291)){
  var_0 <- data.frame("Variable" = colnames(derived_per[i]),
                      "Label"    = attr(derived_per[[i]], 'label'))
  Variable.Description_P <- bind_rows(Variable.Description_P, var_0)
}

Variable.Description_P <- Variable.Description_P %>%
  arrange(Variable)

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
  filter(A002 == 0)%>%
  select(hh_id, sex_hhh, ethnicity)

household_information_3 <- derived_hh %>%
  rename(hh_id = case)%>%
  select(hh_id, P328p, P332, P336)%>%
  mutate(inc_gov_monetary = P328p + P332 + P336,
         inc_gov_cash     = 0)%>%
  select(hh_id, inc_gov_cash, inc_gov_monetary)

household_information <- left_join(household_information_1, household_information_2)%>%
  left_join(household_information_3)%>%
  write_csv(., "../0_Data/1_Household Data/4_United Kingdom/1_Data_Clean/household_information_United Kingdom.csv")
  

# Codes ####

Heating.Code <- data.frame(heating_fuel = c(seq(1,8,1), Heating_Fuel = c("Electric central heating",
                                                                         "Gas central heating",
                                                                         "Oil central heating",
                                                                         "Solid fuel central heating",
                                                                         "Solid fuel or oil central heating",
                                                                         "Calor gas central heating",
                                                                         "Other gas central heating", 
                                                                         "Unknown")))%>%
  write_csv(., "../0_Data/1_Household Data/4_United Kingdom/2_Codes/Heating.Code.csv")
Province.Code <- stack(attr(derived_hh$Gorx, 'labels'))%>%
  rename(province = values, Province = ind)%>%
  write_csv(., "../0_Data/1_Household Data/4_United Kingdom/2_Codes/Province.Code.csv")
Gender.Code <- stack(attr(derived_per$A004, 'labels'))%>%
  rename(sex_hh = values, Gender = ind)%>%
  write_csv(., "../0_Data/1_Household Data/4_United Kingdom/2_Codes/Gender.Code.csv")
Ethnicity.Code <- stack(attr(derived_per$a012p, 'labels'))%>%
  rename(ethnicity = values, Ethnicity = ind)%>%
  write_csv(., "../0_Data/1_Household Data/4_United Kingdom/2_Codes/Ethnicity.Code.csv")

# Appliances ####

appliance_0_1 <- derived_hh %>%
  rename(hh_id = case, washing_machine.01 = A108, car.01 = A149, refrigerator.01 = A164, computer.01 = A1661, tv.01 = A1711,
         dryer.01 = A167, microwave.01 = A168)%>%
  mutate(washing_machine.01 = ifelse(washing_machine.01 == 1,1,0),
         car.01             = ifelse(car.01 > 0,1,0),
         refrigerator.01    = ifelse(refrigerator.01 == 1,1,0))%>%
  mutate_at(vars(computer.01:microwave.01), list(~ ifelse(. == 1,1,0)))%>%
  select(hh_id, ends_with(".01"))%>%
  write_csv(., "../0_Data/1_Household Data/4_United Kingdom/1_Data_Clean/appliances_0_1_United Kingdom.csv")

# Expenditures ####

#clean up item code file
codes_long <- read.xlsx("../0_Data/1_Household Data/4_United_Kingdom/9_Documentation/8686_volume_d_expenditure_codes_201819.xlsx", sheet=3)

codes <- codes_long%>%
  select(code=COIPLUS.Code, description = COIPLUS.Description)%>%
  distinct(.)

# write.xlsx(codes,"./4_United_Kingdom/9_Documentation/Item_Codes.xlsx")

# Final expenditure dataframe comprises several "B-Variables" and "CXXXXt-Variables"

# B-Variables 

expenditure_information_1 <- derived_hh %>%
  select(case, starts_with("B"))%>%
  select(case, order(colnames(.)))%>%
  rename(hh_id = case)%>%
  select(hh_id, B010, B017, B018, B050, B053p, B056p, B060, B102, B104:B110, B160:B167, B170, B175,
         B181:B195b, B199, B216:B227, B229, B2291, B231, B233, B237:B244, B245, B248:B252, B260,
         B270:B280, B480:B490)%>%
  # remove sub-categories
  select(-B160,-B164,-B270, -B480, -B481)

expenditure_information_1.1 <- expenditure_information_1 %>%
  pivot_longer(-hh_id, names_to = "item_code", values_to = "expenditures_year")%>%
  filter(expenditures_year > 0)%>%
  # Weekly basis
  mutate(expenditures_year = expenditures_year*52)

# C-Variables

expenditure_information_2 <- derived_hh %>%
  select(case, starts_with("C") & ends_with("t"))%>%
  rename(hh_id = case)%>%
  select(-Ctpcnt, -Ctrbpcnt, -Ctspcnt, -Ctwtpcnt)

expenditure_information_2.1 <- expenditure_information_2 %>%
  pivot_longer(-hh_id, names_to = "item_code", values_to = "expenditures_year")%>%
  # Weekly basis
  mutate(expenditures_year = expenditures_year*52)%>%
  mutate(item_code = str_replace(item_code, "t",""))%>%
  mutate(item_code_COICOP = str_replace(item_code, "C",""))

# Check if SET89 == derived_hh

# exp_1 <- exp %>%
#   rename(hh_id = case, item_code_COICOP = COI_PLUS, expenditures_year = pdamount)%>%
#   mutate(expenditures_year = expenditures_year*52)%>%
#   group_by(hh_id, item_code_COICOP)%>%
#   summarise(expenditures_year_exp_1 = sum(expenditures_year))%>%
#   ungroup()%>%
#   mutate(item_code_COICOP = str_replace_all(item_code_COICOP, "\\.",""))
# 
# test_expenditures <- full_join(expenditure_information_2.1, exp_1, by = c("hh_id", "item_code_COICOP"))%>%
#   mutate(expenditures_year_exp_1 = ifelse(is.na(expenditures_year_exp_1),0,expenditures_year_exp_1))%>%
#   mutate(test_0 = expenditures_year_exp_1 - expenditures_year)%>%
#   mutate(test = ifelse(expenditures_year_exp_1 != expenditures_year,1,0))%>%
#   filter(test == 1 | is.na(test))%>%
#   filter(test_0 > 0.00005 | test_0 < - 0.00005 | is.na(test_0))
# 
# item_codes <- count(test_expenditures, item_code_COICOP)%>%
#   group_by(n)%>%
#   mutate(number = n())%>%
#   ungroup()%>%
#   filter(number != 2)
# 
# test_expenditures_2 <- test_expenditures %>%
#   filter(item_code_COICOP %in% item_codes$item_code_COICOP)%>%
#   filter(!item_code_COICOP %in% c("K5221", "205221", "K5111", "205111", "B1114", "111114", "121111", "C1111", "201316", "K1316",
#                                   "B112B", "1111211", "K1411", "201411", "101113", "A1113", "123111", "C3111", "943110", "9431A",
#                                   "205113", "K5113", "205316", "K5316", "943114", "9431E", "K5213", "205213", "943113", "9431D",
#                                   "203111", "205212", "K3111", "K5212", "943111", "9431B", "201314", "K1314", "127114", "C7114",
#                                   "112114", "B2114", "125213", "C5213", "123223", "C3223", "125413", "C5413", "102113", "A2113",
#                                   "203112", "K3112", "104113", "A4113", "C6212", "126212", "K5215", "205215", "C4112", "124112",
#                                   "1111114", "B111E", "123112", "C3112", "105113", "A5113", "124111", "C4111", "204112", "205115",
#                                   "K4112", "K5115", "K5116", "205116", "C7111", "127111"))%>%
#   arrange(hh_id, item_code_COICOP)%>%
#   mutate(test_2 = ifelse(expenditures_year %in% .$expenditures_year_exp_1, 1,0))%>%
#   filter(test_2 == 1 | is.na(expenditures_year))%>%
#   group_by(hh_id)%>%
#   mutate(number = n())%>%
#   ungroup()%>%
#   filter(number != 1)

# Hardly any difference

# Will work with C-Variables for now

expenditure_information_3 <- bind_rows(expenditure_information_1.1, expenditure_information_2.1)%>%
  arrange(hh_id, desc(expenditures_year))%>%
  # apparently, expenditures from questionnaire are not actually covered by item codes
  filter(expenditures_year > 0)%>%
  select(hh_id, item_code, expenditures_year)

write_csv(expenditure_information_3, "../0_Data/1_Household Data/4_United Kingdom/1_Data_Clean/expenditures_items_United Kingdom.csv")    

# Item Code Description 

Item.Code.Description <- Variable.Description %>%
  filter(Variable %in% colnames(expenditure_information_1) | Variable %in% colnames(expenditure_information_2))%>%
  rename(item_code = "Variable", item_name = "Label")

write.xlsx(Item.Code.Description, "../0_Data/1_Household Data/4_United Kingdom/3_Matching_Tables/Item_Codes_Description_United Kingdom.xlsx")

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

