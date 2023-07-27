if(!require("pacman")) install.packages("pacman")

p_load("countrycode", "haven", "Hmisc", "openxlsx", "rattle", "scales", "tidyverse")

options(scipen=999)

# Read Data ####

# General reading and formatting, Diary first (3 file types MEMD, EXPD, FMLD), 
# then Interview (3 file types FMLI, MTBI, MEMI) 
# all details found on ( https://www.bls.gov/cex/pumd-getting-started-guide.htm )
# for highest accuracy we use data from 2018, second quarter up to 2019 last quarter. this way most households were present for four full quarters 

### DIARY (files used: MEMD - hh-member charact. + income; EXPD - detailed exp; FMLD - summary exp, hh income, hh charact. + weights)
###        files not used: dtbt -  detailed income; dtid - income imputaions iterations)

#information on household members characteristics + income - diary

memd181 <- read_dta("../0_Data/1_Household Data/3_USA/1_Data_Raw/diary18/memd181.dta")%>%
  mutate(quarter = 181)
memd182 <- read_dta("../0_Data/1_Household Data/3_USA/1_Data_Raw/diary18/memd182.dta")%>%
  mutate(quarter = 182)
memd183 <- read_dta("../0_Data/1_Household Data/3_USA/1_Data_Raw/diary18/memd183.dta")%>%
  mutate(quarter = 183)
memd184 <- read_dta("../0_Data/1_Household Data/3_USA/1_Data_Raw/diary18/memd184.dta")%>%
  mutate(quarter = 184)
memd191 <- read_dta("../0_Data/1_Household Data/3_USA/1_Data_Raw/diary19/memd191.dta")%>%
 mutate(quarter = 191)
memd192 <- read_dta("../0_Data/1_Household Data/3_USA/1_Data_Raw/diary19/memd192.dta")%>%
 mutate(quarter = 192)
memd193 <- read_dta("../0_Data/1_Household Data/3_USA/1_Data_Raw/diary19/memd193.dta")%>%
 mutate(quarter = 193)
memd194 <- read_dta("../0_Data/1_Household Data/3_USA/1_Data_Raw/diary19/memd194.dta")%>%
 mutate(quarter = 194)

# Information on diary members

household_members_diary <- bind_rows(memd181, memd182, memd183, memd184,
                                     memd191, memd192, memd193, memd194)%>%
 select(newid, everything())%>%
 rename(hh_id = newid)%>%
  mutate(id = gsub('.$', '', hh_id))

# detailed exp data - diary
expd181 <- read_dta("../0_Data/1_Household Data/3_USA/1_Data_Raw/diary18/expd181.dta")%>%
  mutate(quarter = 181)
expd182 <- read_dta("../0_Data/1_Household Data/3_USA/1_Data_Raw/diary18/expd182.dta")%>%
  mutate(quarter = 182)
expd183 <- read_dta("../0_Data/1_Household Data/3_USA/1_Data_Raw/diary18/expd183.dta")%>%
  mutate(quarter = 183)
expd184 <- read_dta("../0_Data/1_Household Data/3_USA/1_Data_Raw/diary18/expd184.dta")%>%
  mutate(quarter = 184)
expd191 <- read_dta("../0_Data/1_Household Data/3_USA/1_Data_Raw/diary19/expd191.dta")%>%
  mutate(quarter = 191)
expd192 <- read_dta("../0_Data/1_Household Data/3_USA/1_Data_Raw/diary19/expd192.dta")%>%
  mutate(quarter = 192)
expd193 <- read_dta("../0_Data/1_Household Data/3_USA/1_Data_Raw/diary19/expd193.dta")%>%
  mutate(quarter = 193)
expd194 <- read_dta("../0_Data/1_Household Data/3_USA/1_Data_Raw/diary19/expd194.dta")%>%
  mutate(quarter = 194)

# Detailed expenditure data (diary)

exp_diary <- bind_rows(expd181, expd182, expd183, expd184,
                       expd191, expd192, expd193, expd194)%>% #all are distinct hhs, put them together
  select(newid, everything())%>%
  rename(hh_id = newid)%>%
  mutate(id = gsub('.$', '', hh_id))

# summary exp, hh level income, hh characteristics + weights - diary
fmld181 <- read_dta("../0_Data/1_Household Data/3_USA/1_Data_Raw/diary18/fmld181.dta")%>%
  mutate(quarter = 181)
fmld182 <- read_dta("../0_Data/1_Household Data/3_USA/1_Data_Raw/diary18/fmld182.dta")%>%
  mutate(quarter = 182)
fmld183 <- read_dta("../0_Data/1_Household Data/3_USA/1_Data_Raw/diary18/fmld183.dta")%>%
  mutate(quarter = 183)
fmld184 <- read_dta("../0_Data/1_Household Data/3_USA/1_Data_Raw/diary18/fmld184.dta")%>%
  mutate(quarter = 184)
fmld191 <- read_dta("../0_Data/1_Household Data/3_USA/1_Data_Raw/diary19/fmld191.dta")%>%
  mutate(quarter = 191)
fmld192 <- read_dta("../0_Data/1_Household Data/3_USA/1_Data_Raw/diary19/fmld192.dta")%>%
  mutate(quarter = 192)
fmld193 <- read_dta("../0_Data/1_Household Data/3_USA/1_Data_Raw/diary19/fmld193.dta")%>%
  mutate(quarter = 193)
fmld194 <- read_dta("../0_Data/1_Household Data/3_USA/1_Data_Raw/diary19/fmld194.dta")%>%
  mutate(quarter = 194)

# Summary diary

fml_diary <- bind_rows(fmld181, fmld182, fmld183, fmld184,
                       fmld191, fmld192, fmld193, fmld194)%>% #all are distinct hhs, put them together
  select(newid, everything())%>%
  rename(hh_id = newid)%>%
  mutate(id = gsub('.$', '', hh_id))

# remove all smaller files
rm(expd181, expd182, expd183, expd184,
   expd191, expd192, expd193, expd194, 
   fmld181, fmld182, fmld183, fmld184,
   fmld191, fmld192, fmld193, fmld194, 
   memd181, memd182, memd183, memd184,
   memd191, memd192,memd193, memd194)

# INTERVIEW (files used: FMLI - hh exp., income & charact; 
#                        MTBI - monthly exp.; 
#                        MEMI - hh member income and charact.,
#            files not used: ITBI - detailed income; ITII - imputed income iterations; NTAXI - taxes estimations; 
#                            FPAR & MCHI - survey process and contact history [in /para19]; more detailes exp data [in /expn19])
# Files marked with an "x" are NOT USED, they appeared in the previous survey too and were processed under different standards, so contents slightly differ

# hh level exp., income and characteristics - interview
fmli182 <- read_dta("../0_Data/1_Household Data/3_USA/1_Data_Raw/intrvw18/intrvw18/fmli182.dta")%>%
  mutate(quarter = 182)
fmli183 <- read_dta("../0_Data/1_Household Data/3_USA/1_Data_Raw/intrvw18/intrvw18/fmli183.dta")%>%
  mutate(quarter = 183)
fmli184 <- read_dta("../0_Data/1_Household Data/3_USA/1_Data_Raw/intrvw18/intrvw18/fmli184.dta")%>%
  mutate(quarter = 184)
fmli191 <- read_dta("../0_Data/1_Household Data/3_USA/1_Data_Raw/intrvw18/intrvw18/fmli191.dta")%>%
  mutate(quarter = 191)
fmli192 <- read_dta("../0_Data/1_Household Data/3_USA/1_Data_Raw/intrvw19/intrvw19/fmli192.dta")%>%
  mutate(quarter = 192)
fmli193 <- read_dta("../0_Data/1_Household Data/3_USA/1_Data_Raw/intrvw19/intrvw19/fmli193.dta")%>%
  mutate(quarter = 193)
fmli194 <- read_dta("../0_Data/1_Household Data/3_USA/1_Data_Raw/intrvw19/intrvw19/fmli194.dta")%>%
  mutate(quarter = 194)

# Household expenditures and income and characteristics

fml_iv <- bind_rows(fmli182, fmli183, fmli184, fmli191, fmli192, fmli193, fmli194)%>% #all are distinct hhs, put them together
  select(newid, everything())%>%
  rename(hh_id = newid)%>%
  mutate(id = gsub('.$', '', hh_id))
# tmp <- unique(gsub('.$', '', fmli191$newid))
# fml_iv <- filter(fml_iv, id %in% tmp)

# tmp    <-fml_iv[1,] %in% c("A", "B", "C", "D", "E", "F", "G", "H", "T", "U", "V", "W") #all cols that are flags
# fml_iv <- fml_iv[colnames(fml_iv)[!tmp]] #subset dataframe with all cols that are not flags using tmp (line above)

# monthly expenditures - interview
mtbi182 <- read_dta("../0_Data/1_Household Data/3_USA/1_Data_Raw/intrvw18/intrvw18/mtbi182.dta")%>%
  mutate(quarter = 182)
mtbi183 <- read_dta("../0_Data/1_Household Data/3_USA/1_Data_Raw/intrvw18/intrvw18/mtbi183.dta")%>%
  mutate(quarter = 183)
mtbi184 <- read_dta("../0_Data/1_Household Data/3_USA/1_Data_Raw/intrvw18/intrvw18/mtbi184.dta")%>%
  mutate(quarter = 184)
mtbi191 <- read_dta("../0_Data/1_Household Data/3_USA/1_Data_Raw/intrvw18/intrvw18/mtbi191.dta")%>%
  mutate(quarter = 191)
mtbi192 <- read_dta("../0_Data/1_Household Data/3_USA/1_Data_Raw/intrvw19/intrvw19/mtbi192.dta")%>%
  mutate(quarter = 192)
mtbi193 <- read_dta("../0_Data/1_Household Data/3_USA/1_Data_Raw/intrvw19/intrvw19/mtbi193.dta")%>%
  mutate(quarter = 193)
mtbi194 <- read_dta("../0_Data/1_Household Data/3_USA/1_Data_Raw/intrvw19/intrvw19/mtbi194.dta")%>%
  mutate(quarter = 194)

mtb_iv <- rbind(mtbi182, mtbi183, mtbi184, mtbi191, mtbi192, mtbi193, mtbi194)%>% #all are distinct hhs, put them together
  select(newid, everything())%>%
  rename(hh_id = newid)%>%
  mutate(id = gsub('.$', '', hh_id))
# tmp <- unique(gsub('.$', '', mtbi191$newid))
# mtb_iv <- filter(mtb_iv, id %in% tmp)

# tmp <-mtb_iv[1,] %in% c("A", "B", "C", "D", "E", "F", "G", "H", "T", "U", "V", "W") #all cols that are flags
# mtb_iv <- mtb_iv[colnames(mtb_iv)[!tmp]] #subset dataframe with all cols that are not flags using tmp (line above)

# hh member income and characteristics - interview
memi182 <- read_dta("../0_Data/1_Household Data/3_USA/1_Data_Raw/intrvw18/intrvw18/memi182.dta")%>%
  mutate(quarter = 182)
memi183 <- read_dta("../0_Data/1_Household Data/3_USA/1_Data_Raw/intrvw18/intrvw18/memi183.dta")%>%
  mutate(quarter = 183)
memi184 <- read_dta("../0_Data/1_Household Data/3_USA/1_Data_Raw/intrvw18/intrvw18/memi184.dta")%>%
  mutate(quarter = 184)
memi191 <- read_dta("../0_Data/1_Household Data/3_USA/1_Data_Raw/intrvw18/intrvw18/memi191.dta")%>%
  mutate(quarter = 191)
memi192 <- read_dta("../0_Data/1_Household Data/3_USA/1_Data_Raw/intrvw19/intrvw19/memi192.dta")%>%
  mutate(quarter = 192)
memi193 <- read_dta("../0_Data/1_Household Data/3_USA/1_Data_Raw/intrvw19/intrvw19/memi193.dta")%>%
  mutate(quarter = 193)
memi194 <- read_dta("../0_Data/1_Household Data/3_USA/1_Data_Raw/intrvw19/intrvw19/memi194.dta")%>%
  mutate(quarter = 194)

mem_iv <- bind_rows(memi182, memi183, memi184, memi191, memi192, memi193, memi194)%>% #all are distinct hhs, put them together
  select(newid, everything())%>%
  rename(hh_id = newid)%>%
  mutate(id = gsub('.$', '', hh_id))
# tmp <- unique(gsub('.$', '', memi191$newid))
# mem_iv <- filter(mem_iv, id %in% tmp)

# tmp    <- mem_iv[1,] %in% c("A", "B", "C", "D", "E", "F", "G", "H", "T", "U", "V", "W") #all cols that are flags
# mem_iv <- mem_iv[colnames(mem_iv)[!tmp]] #subset dataframe with all cols that are not flags using tmp (line above)

#rm unnecessary smaller files
rm(memi182, memi183, memi184, memi191, memi192, memi193, memi194,
   mtbi182, mtbi183, mtbi184, mtbi191, mtbi192, mtbi193, mtbi194, 
   fmli182, fmli183, fmli184, fmli191, fmli192, fmli193, fmli194)

# Transform Data ####

# We proceed using the Interview data, it's representative and comprises of more participants #
# each dataset consists of 4 quarters, second of 2019 to first of 2020, we aggregate them for more comprehensive expenditure data
# household information that is not expenditure, we use the latest available dataset in each household
# we later do the same for the diary data and compare them for robustness

# collect household information, appliances and expenditure information (including item code descriptions) in this order

### HOUSEHOLD INFORMATION

# This captures all households and their ids which are present in the first quarter of 2019

households_used <- fml_iv %>%
  mutate(quarter_2019.1 = ifelse(quarter == 191,1,0))%>%
  group_by(id)%>%
  mutate(quarter_20191.1 = sum(quarter_2019.1),
         number          = n())%>%
  ungroup()%>%
  select(hh_id, id, quarter, number, quarter_20191.1)%>%
  filter(quarter_20191.1 == 1)%>%
  arrange(id, quarter)

fml_iv_2 <- fml_iv %>%
  # equivalent to filtering households_used$hh_id
  filter(hh_id %in% households_used$hh_id)%>%
  arrange(id, quarter)%>%
  mutate(urban_01 = ifelse(bls_urbn == 1,1,0))%>%
  rename(province = division, district = state, hh_weights = finlwt21)%>%
  select(hh_id_0 = id, urban_01, province, district, hh_weights, hh_id)%>%
  group_by(hh_id_0)%>%
  mutate(number_cons = 1:n(),
         number      = n(),
         hh_weights  = sum(hh_weights))%>%
  ungroup()%>%
  # Weights adjustment
  mutate(hh_weights = hh_weights/number)

fml_iv_2.1 <- fml_iv_2 %>%
  filter(province != "")%>%
  distinct(hh_id_0, province)

fml_iv_2.2 <- fml_iv_2 %>%
  filter(district != "")%>%
  distinct(hh_id_0, district)

mem_iv_1 <- mem_iv %>%
  filter(hh_id %in% households_used$hh_id)%>%
  filter(cu_code == 1)%>%
  rename(age_hhh = age, sex_hhh = sex, ind_hhh = occucode, edu_hhh = educa,
         ethnicity = membrace, ethnicity_2.1 = asian, ethnicity_2 = hispanic)%>%
  filter(quarter == "191")%>%
  select(hh_id_0 = id, sex_hhh, age_hhh, ind_hhh, edu_hhh, ethnicity, ethnicity_2.1, ethnicity_2)%>%
  mutate(ethnicity_new     = ifelse((ethnicity == 4 | ethnicity == 6) & ethnicity_2.1 != "", as.numeric(ethnicity_2.1)+10, ethnicity))%>%
  select(-ethnicity_2.1, -ethnicity_2, -ethnicity)%>%
  rename(ethnicity = ethnicity_new)

mem_iv_2 <- mem_iv %>%
  filter(hh_id %in% households_used$hh_id)%>%
  select(hh_id_0 = id, hh_id, quarter, socrrx)%>%
  mutate(socrrx = ifelse(is.na(socrrx),0, socrrx))%>%
  group_by(hh_id_0)%>%
  summarise(inc_gov_monetary = sum(socrrx),
            inc_gov_cash     = 0)%>%
  ungroup()

mem_iv_3 <- mem_iv %>%
  filter(hh_id %in% households_used$hh_id)%>%
  mutate(adults   = ifelse(age > 15,1,0),
         children = ifelse(age < 16,1,0))%>%
  filter(quarter == "191")%>%
  rename(hh_id_0 = id)%>%
  group_by(hh_id_0)%>%
  summarise(adults           = sum(adults),
            children         = sum(children),
            hh_size          = n())%>%
  ungroup()
  
household_information <- distinct(fml_iv_2, hh_id_0, urban_01, hh_weights)%>%
  left_join(fml_iv_2.1)%>%
  left_join(fml_iv_2.2)%>%
  left_join(mem_iv_1)%>%
  left_join(mem_iv_2)%>%
  left_join(mem_iv_3)%>%
  mutate_at(vars(-hh_id_0), list(~ ifelse(. == "", NA,.)))%>%
  # Only now
  rename(hh_id = hh_id_0)%>%
  mutate(district = ifelse(is.na(district),"00",district),
         province = ifelse(is.na(province), "0",province))

write_csv(household_information, "../0_Data/1_Household Data/3_USA/1_Data_Clean/household_information_USA.csv")

# Expenditures ####

# Start with interview --> add diary (for shares)

### EXPENDITURE
# not all households are in the set for all 4 quarters in which the interviews were conducted
# -> we normalize the expenditures to yearly expenses for all households, including those with missing interviews

expenditure_information <- mtb_iv %>%
  group_by(hh_id, ucc)%>%
  summarise(cost = sum(cost))%>%
  ungroup()%>%
  filter(hh_id %in% households_used$hh_id)%>%
  left_join(households_used)%>%
  # extrapolate expenditures if households do not have information for all quarters
  mutate(expenditures_year = cost*4/number)%>%
  group_by(id, ucc)%>%
  summarise(expenditures_year = sum(expenditures_year))%>%
  ungroup()%>%
  rename(hh_id_0 = id, item_code = ucc)%>%
  arrange(hh_id_0, item_code)%>%
  rename(hh_id = hh_id_0)%>%
  # Delete 790210 because it does not really help us
  filter(item_code != 790210)

expenditure_information_1 <- expenditure_information %>%
  filter(!item_code %in% c(790210, 790240, 190903,190904))

expenditure_information_2 <- expenditure_information %>%
  filter(item_code %in% c(790240, 190903,190904))%>%
  group_by(hh_id)%>%
  summarise(expenditures_year_food = sum(expenditures_year))%>%
  ungroup()%>%
  left_join(select(household_information, hh_id, urban_01, province, district, edu_hhh, hh_size, ethnicity))

# Decompose total food expenditures by expenditure shares for households based on socio-demographic characteristics

households_used_diary <- fml_diary %>%
  select(hh_id, id, quarter)%>%
  filter(quarter == 191 | quarter == 192 | quarter == 193 | quarter == 194)

fml_diary_2 <- fml_diary %>%
  filter(hh_id %in% households_used_diary$hh_id)%>%
  mutate(urban_01 = ifelse(bls_urbn == 1,1,0))%>%
  rename(province = division, district = state, hh_weights = finlwt21)%>%
  select(hh_id_0 = id, urban_01, province, district, hh_weights, hh_id)

mem_diary_1 <- household_members_diary %>%
  filter(hh_id %in% households_used_diary$hh_id)%>%
  filter(cu_code1 == 1)%>%
  rename(age_hhh = age, sex_hhh = sex, ind_hhh = occuearn, edu_hhh = educa,
         ethnicity = membrace, ethnicity_2.1 = asian, ethnicity_2 = hispanic)%>%
  select(hh_id_0 = id, hh_id, sex_hhh, age_hhh, ind_hhh, edu_hhh, ethnicity, ethnicity_2.1, ethnicity_2)%>%
  mutate(ethnicity     = ifelse(ethnicity == 4 | ethnicity == 6, as.numeric(ethnicity_2.1)+10, ethnicity))%>%
  select(-ethnicity_2.1)

mem_diary_2 <- household_members_diary %>%
  filter(hh_id %in% households_used_diary$hh_id)%>%
  mutate(adults   = ifelse(age > 15,1,0),
         children = ifelse(age < 16,1,0))%>%
  rename(hh_id_0 = id)%>%
  group_by(hh_id_0, hh_id)%>%
  summarise(adults           = sum(adults),
            children         = sum(children),
            hh_size          = n())%>%
  ungroup()

household_information_diary <- fml_diary_2 %>%
  left_join(mem_diary_1)%>%
  left_join(mem_diary_2)%>%
  mutate_at(vars(-hh_id_0), list(~ ifelse(. == "", NA,.)))

exp_diary_1 <- exp_diary %>%
  filter(quarter %in% c(191,192,193,194))%>%
  rename(item_code = ucc, expenditures_year = cost)%>%
  mutate(expenditures_year = expenditures_year*52)%>%
  select(hh_id, item_code, expenditures_year)%>%
  arrange(hh_id, item_code)%>%
  filter(item_code >= 100110 & item_code <=200534)%>%
  group_by(hh_id, item_code)%>%
  summarise(expenditures_year = sum(expenditures_year))%>%
  ungroup()%>%
  pivot_wider(names_from = "item_code", values_from = "expenditures_year", values_fill = 0)%>%
  pivot_longer(-hh_id, names_to = "item_code", values_to = "expenditures_year")%>%
  group_by(hh_id)%>%
  mutate(food_expenditures = sum(expenditures_year))%>%
  ungroup()%>%
  mutate(food_share = expenditures_year/food_expenditures)%>%
  select(hh_id, item_code, food_share)%>%
  left_join(household_information_diary)

# State / District / urban/rural / edu_hhh / ethnicity

information_diary_1.1 <- distinct(exp_diary_1, province, district, urban_01, edu_hhh, ethnicity, hh_size)%>%
  mutate(groupA = paste0("A", 1:n()))

exp_diary_1.1 <- exp_diary_1 %>%
  group_by(province, district, urban_01, edu_hhh, ethnicity, hh_size, item_code)%>%
  summarise(food_share_A = wtd.mean(food_share, hh_weights))%>%
  ungroup()%>%
  left_join(information_diary_1.1)%>%
  select(groupA, item_code, food_share_A)

information_diary_1.2 <- distinct(exp_diary_1, province, district, urban_01, edu_hhh, ethnicity)%>%
  mutate(groupB = paste0("B", 1:n()))

exp_diary_1.2 <- exp_diary_1 %>%
  group_by(province, district, urban_01, edu_hhh, ethnicity, item_code)%>%
  summarise(food_share_B = wtd.mean(food_share, hh_weights))%>%
  ungroup()%>%
  left_join(information_diary_1.2)%>%
  select(groupB, item_code, food_share_B)

information_diary_1.3 <- distinct(exp_diary_1, province, district, urban_01, edu_hhh)%>%
  mutate(groupC = paste0("C", 1:n()))

exp_diary_1.3 <- exp_diary_1 %>%
  group_by(province, district, urban_01, edu_hhh, item_code)%>%
  summarise(food_share_C = wtd.mean(food_share, hh_weights))%>%
  ungroup()%>%
  left_join(information_diary_1.3)%>%
  select(groupC, item_code, food_share_C)

information_diary_1.4 <- distinct(exp_diary_1, province, district, urban_01)%>%
  mutate(groupD = paste0("D", 1:n()))

exp_diary_1.4 <- exp_diary_1 %>%
  group_by(province, district, urban_01, item_code)%>%
  summarise(food_share_D = wtd.mean(food_share, hh_weights))%>%
  ungroup()%>%
  left_join(information_diary_1.4)%>%
  select(groupD, item_code, food_share_D)

expenditure_information_2.1 <- expenditure_information_2 %>%
  left_join(information_diary_1.1)%>%
  left_join(information_diary_1.2)%>%
  left_join(information_diary_1.3)%>%
  left_join(information_diary_1.4)

expenditure_information_2.1.1 <- expenditure_information_2.1 %>%
  filter(!is.na(groupA))%>%
  select(hh_id, expenditures_year_food, groupA)

expenditure_information_2.1.2 <- expenditure_information_2.1 %>%
  filter(is.na(groupA) & !is.na(groupB))%>%
  select(hh_id, expenditures_year_food, groupB)

expenditure_information_2.1.3 <- expenditure_information_2.1 %>%
  filter(is.na(groupA) & is.na(groupB) & !is.na(groupC))%>%
  select(hh_id, expenditures_year_food, groupC)

expenditure_information_2.1.4 <- expenditure_information_2.1 %>%
  filter(is.na(groupA) & is.na(groupB) & is.na(groupC))%>%
  select(hh_id, expenditures_year_food, groupD)

# Now: new dataframes with food expenditures

Item.Codes <- distinct(exp_diary, ucc)%>%
  rename(item_code = ucc)

expenditure_information_3.1 <- expand_grid(hh_id = expenditure_information_2.1.1$hh_id, item_code = Item.Codes$item_code)%>%
  distinct()%>%
  left_join(expenditure_information_2.1.1, by = "hh_id")%>%
  left_join(exp_diary_1.1)%>%
  filter(!is.na(food_share_A))%>%
  mutate(expenditures_year = food_share_A*expenditures_year_food)%>%
  select(hh_id, item_code, expenditures_year)%>%
  arrange(hh_id, item_code)

expenditure_information_3.2 <- expand_grid(hh_id = expenditure_information_2.1.2$hh_id, item_code = Item.Codes$item_code)%>%
  distinct()%>%
  left_join(expenditure_information_2.1.2, by = "hh_id")%>%
  left_join(exp_diary_1.2)%>%
  filter(!is.na(food_share_B))%>%
  mutate(expenditures_year = food_share_B*expenditures_year_food)%>%
  select(hh_id, item_code, expenditures_year)%>%
  arrange(hh_id, item_code)

expenditure_information_3.3 <- expand_grid(hh_id = expenditure_information_2.1.3$hh_id, item_code = Item.Codes$item_code)%>%
  distinct()%>%
  left_join(expenditure_information_2.1.3, by = "hh_id")%>%
  left_join(exp_diary_1.3)%>%
  filter(!is.na(food_share_C))%>%
  mutate(expenditures_year = food_share_C*expenditures_year_food)%>%
  select(hh_id, item_code, expenditures_year)%>%
  arrange(hh_id, item_code)

expenditure_information_3.4 <- expand_grid(hh_id = expenditure_information_2.1.4$hh_id, item_code = Item.Codes$item_code)%>%
  distinct()%>%
  left_join(expenditure_information_2.1.4, by = "hh_id")%>%
  left_join(exp_diary_1.4)%>%
  filter(!is.na(food_share_D))%>%
  mutate(expenditures_year = food_share_D*expenditures_year_food)%>%
  select(hh_id, item_code, expenditures_year)%>%
  arrange(hh_id, item_code)

expenditure_information_3.5 <- bind_rows(expenditure_information_3.1, 
                                         expenditure_information_3.2,
                                         expenditure_information_3.3,
                                         expenditure_information_3.4)%>%
  filter(expenditures_year > 0)

expenditure_information_4 <- bind_rows(expenditure_information_1, expenditure_information_3.5)%>%
  arrange(hh_id, item_code)%>%
  filter(!item_code %in% c(820101,820102,830101,830102,830201,830202,830203,8350100,860100,860200,860301,860302))

write_csv(expenditure_information_4, "../0_Data/1_Household Data/3_USA/1_Data_Clean/expenditures_items_USA.csv")

### EXPENDITURE-CODES ####
# the Hierarchical groupings for all years are found in the stubs.zip file found on the official website 
# https://www.bls.gov/cex/pumd_doc.htm
# it contains all UCC codes and their description for each year of both interviews and diaries
# load the interview-files for 2019 and 2020, extract codes from exp and then match codes to descriptions
ucc_codes_raw <- read.xlsx("../0_Data/1_Household Data/3_USA/9_Documentation/ce_source_integrate.xlsx", startRow = 4, sheet = 1)

ucc_codes <- ucc_codes_raw%>%
  filter(!is.na(y19))%>%
  select(item_name = Description, item_code = UCC)

Item.Codes <- distinct(expenditure_information, item_code)%>%
  arrange(item_code)%>%
  left_join(ucc_codes)%>%
  distinct()%>%
  mutate(Type = "Interview")

Item.Codes.Diary <- distinct(exp_diary_1, item_code)%>%
  arrange(item_code)%>%
  left_join(ucc_codes)%>%
  distinct()%>%
  mutate(Type = "Diary")

Item.Codes.Joint <- bind_rows(Item.Codes, Item.Codes.Diary)%>%
  arrange(item_code)

# write.xlsx(Item.Codes.Joint, "../0_Data/1_Household Data/3_USA/3_Matching_Tables/Item_Codes_Description_USA.xlsx")

# Matching Tables 

GTAP_0 <- read.xlsx("../0_Data/1_Household Data/3_USA/3_Matching_Tables/BUA-Seminar/Item_GTAP_Concordance_USA_Interview.xlsx")%>%
  select(GTAP, Explanation)%>%
  mutate(number = 1:n())

GTAP_1 <- read.xlsx("../0_Data/1_Household Data/3_USA/3_Matching_Tables/BUA-Seminar/Item_GTAP_Concordance_USA_Interview.xlsx")%>%
  select (-Explanation) %>%
  pivot_longer(-GTAP, names_to = "drop", values_to = "item_code")%>%
  filter(!is.na(item_code))%>%
  select(GTAP, item_code)%>%
  mutate(GTAP = ifelse(GTAP == "gas" | GTAP == "gdt", "gasgdt", GTAP))%>%
  rename(GTAP_A = GTAP)

GTAP_2 <- read.xlsx("../0_Data/1_Household Data/3_USA/3_Matching_Tables/BUA-Seminar/Item_GTAP_Concordance_USA.xlsx")%>%
  select (-Explanation) %>%
  pivot_longer(-GTAP, names_to = "drop", values_to = "item_code")%>%
  filter(!is.na(item_code))%>%
  select(GTAP, item_code)%>%
  mutate(GTAP = ifelse(GTAP == "gas" | GTAP == "gdt", "gasgdt", GTAP))%>%
  rename(GTAP_B = GTAP)

Item.Codes.Matching <- Item.Codes.Joint %>%
  left_join(GTAP_1)%>%
  left_join(GTAP_2)%>%
  mutate(GTAP = ifelse(!is.na(GTAP_A), GTAP_A, GTAP_B))%>%
  left_join(GTAP_0, by = "GTAP")%>%
  arrange(number)

# write.xlsx(Item.Codes.Matching, "../0_Data/1_Household Data/3_USA/3_Matching_Tables/BUA-Seminar/Item_GTAP_Concordance_USA_Check.xlsx")

Item.Codes.Matching.New <- read.xlsx("../0_Data/1_Household Data/3_USA/3_Matching_Tables/BUA-Seminar/Item_GTAP_Concordance_USA_Check.xlsx")

# GTAP-Matching

Item.Codes.Matching.1 <- Item.Codes.Matching.New %>%
  select(GTAP_final, item_code)%>%
  distinct()%>%
  group_by(GTAP_final)%>%
  mutate(number = 1:n())%>%
  ungroup()%>%
  pivot_wider(values_from = "item_code", names_from = "number", values_fill = NA)%>%
  full_join(GTAP_0, by = c("GTAP_final" = "GTAP"))%>%
  select(GTAP_final, Explanation, everything())%>%
  arrange(number)%>%
  select(-number)

write.xlsx(Item.Codes.Matching.1, "../0_Data/1_Household Data/3_USA/3_Matching_Tables/Item_GTAP_Concordance_USA.xlsx")

Item.Codes.Matching.2 <- Item.Codes.Matching.New %>%
  select(item_code, Categories)%>%
  distinct()%>%
  group_by(Categories)%>%
  mutate(number = 1:n())%>%
  ungroup()%>%
  pivot_wider(values_from = "item_code", names_from = "number", values_fill = NA)%>%
  select(Categories, everything())

write.xlsx(Item.Codes.Matching.2, "../0_Data/1_Household Data/3_USA/3_Matching_Tables/Item_Categories_Concordance_USA.xlsx")


# Code Intermezzo

Gender.Code <- distinct(mem_iv, sex)%>%
  rename(sex_hhh = sex)%>%
  arrange(sex_hhh)%>%
  mutate(Gender = c("Male", "Female"))%>%
  write_csv(., "../0_Data/1_Household Data/3_USA/2_Codes/Gender.Code.csv")
Industry.Code <- distinct(mem_iv, occucode)%>%
  rename(ind_hhh = occucode)%>%
  arrange(ind_hhh)%>%
  mutate(Industry = c(NA,"Manager, Professional Administrator","Teacher","Professional","Administrative support, including clerical","Sales, retail","Sales, business goods, and services","Technician Service","Protective Service","Private household service","Other Service","Machine or transportation operator, laborer","Construction workers, mechanics","Farming","Forestry, fishing, groundskeeping","Armed Forces"))%>%
  write_csv(., "../0_Data/1_Household Data/3_USA/2_Codes/Industry.Code.csv")
Education.Code <- distinct(mem_iv, educa)%>%
  rename(edu_hhh = educa)%>%
  arrange(edu_hhh)%>%
  mutate(Education = c(NA,"No schooling completed, or less than 1 year","Nursery, kindergarten, and elementary (grades 1-8)","High school (grades 9-12), no degree","High school graduate - high school diploma or equivalent (GED)","Some college, but no degree","Associates degree in college","Bachelors degree (BA, AB, BS, etc.)","Masters, professional, or doctorate degree (MA, MS, MBA, MD, JD, PhD, etc.)"))%>%
  mutate(ISCED = c(9,0,1,2,3,4,4,6,7))%>%
  write_csv(., "../0_Data/1_Household Data/3_USA/2_Codes/Education.Code.csv")
Ethnicity.Code <- distinct(mem_iv_1, ethnicity)%>%
  mutate(ethnicity = as.numeric(ethnicity))%>%
  arrange(ethnicity)%>%
  mutate(Ethnicity = c("White", "Black", "Native American", "Asian", "Pacific Islander", "Multi-race", 
                       "Asian or Multi-race - Chinese","Asian or Multi-race - Filipino", "Asian or Multi-race - Japanese",
                       "Asian or Multi-race - Korean", "Asian or Multi-race - Vietnamese", "Asian or Multi-race - Asian Indian", "Asian or Multi-race - Other"))%>%
  write_csv(., "../0_Data/1_Household Data/3_USA/2_Codes/Ethnicity.Code.csv")
Ethnicity.Code.2 <- distinct(mem_iv, hispanic)%>%
  arrange(hispanic)%>%
  rename(ethnicity_2 = hispanic)%>%
  mutate(Ethnicity_2 = c("Mexican", "Mexican-American", "Chicano", "Puerto Rican", "Cuban", "Other", NA))%>%
  write_csv(., "../0_Data/1_Household Data/3_USA/2_Codes/Ethnicity.2.Code.csv")
Province.Code <- distinct(fml_iv, division)%>%
  arrange(division)%>%
  rename(province = division)%>%
  mutate(province = ifelse(province == "","0",province))%>%
  mutate(Province = c("Unknown", "New England","Middle Atlantic","East North Central","West North Central","South Atlantic","East South Central",
  "West South Central","Mountain","Pacific"))%>%
  write_csv(., "../0_Data/1_Household Data/3_USA/2_Codes/Province.Code.csv")
District.Code <- distinct(fml_iv, state)%>%
  arrange(state)%>%
  rename(district = state)%>%
  mutate(district = ifelse(district == "","00",district))%>%
  mutate(District = c("Unknown", "Alabama","Alaska","Arizona","California","Colorado","Connecticut","Delaware","District of Columbia","Florida","Georgia",
                      "Hawaii","Illinois","Indiana","Iowa","Kansas","Kentucky","Louisiana",
                      "Maryland","Massachuse","Michigan","Minnesota","Mississippi","Missouri","Nebraska","Nevada","New Hampshire","New Jersey",
                      "New York","North Carolina","Ohio","Oklahoma","Oregon","Pennsylvania","South Carolina","Tennessee","Texas","Utah","Virginia",
                      "Washington","Wisconsin"))%>%
  write_csv(., "../0_Data/1_Household Data/3_USA/2_Codes/District.Code.csv")
