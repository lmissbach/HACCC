if(!require("pacman")) install.packages("pacman")

p_load("countrycode", "haven", "Hmisc", "openxlsx", "rattle", "scales", "tidyverse")

options(scipen=999)

# Read Data ####

# pre-existing summarized files for diary and interview for double checking (author is participants of seminar at TU Berlin))
dia_exp_pre <- read_rds("../0_Data/1_Household Data/3_USA/1_Data_Raw/diary19/expenditure_information.rds")
dia_hh_pre  <- read_rds("../0_Data/1_Household Data/3_USA/1_Data_Raw/diary19/household_description.rds")
int_exp_pre <- read_rds("../0_Data/1_Household Data/3_USA/1_Data_Raw/intrvw19/expenditure_information.rds")
int_hh_pre  <- read_rds("../0_Data/1_Household Data/3_USA/1_Data_Raw/intrvw19/household_description.rds")


##### General reading and formatting, Diary first (3 file types MEMD, EXPD, FMLD), 
##### then Interview (3 file types FMLI, MTBI, MEMI) 
##### all details found on ( https://www.bls.gov/cex/pumd-getting-started-guide.htm )
#### for highest accuracy we use data from 2018, second quarter up to 2019 last quarter. this way most households were present for four full quarters 


### DIARY (files used: MEMD - hh-member charact. + income; EXPD - detailed exp; FMLD - summary exp, hh income, hh charact. + weights)
###        files not used: dtbt -  detailed income; dtid - income imputaions iterations)

#information on household members characteristics + income - diary
memd191 <- read_dta("../0_Data/1_Household Data/3_USA/1_Data_Raw/diary19/memd191.dta")%>%
  mutate(quarter = 1)
memd192 <- read_dta("../0_Data/1_Household Data/3_USA/1_Data_Raw/diary19/memd192.dta")%>%
  mutate(quarter = 2)
memd193 <- read_dta("../0_Data/1_Household Data/3_USA/1_Data_Raw/diary19/memd193.dta")%>%
  mutate(quarter = 3)
memd194 <- read_dta("../0_Data/1_Household Data/3_USA/1_Data_Raw/diary19/memd194.dta")%>%
  mutate(quarter = 4)

household_members_diary <- bind_rows(memd191, memd192, memd193, memd194)%>%
  select(newid, everything())%>%
  rename(hh_id = newid)

# tmp                     <-household_members_diary[1,] %in% c("A", "B", "C", "D", "E", "F", "G", "H", "T", "U", "V", "W") #all cols that are flags
# household_members_diary <- household_members_diary[colnames(household_members_diary)[!tmp]] #subset dataframe with all cols that are not flags using tmp (line above)

# detailed exp data - diary
expd191 <- read_dta("../0_Data/1_Household Data/3_USA/1_Data_Raw/diary19/expd191.dta")%>%
  mutate(quarter = 1)
expd192 <- read_dta("../0_Data/1_Household Data/3_USA/1_Data_Raw/diary19/expd192.dta")%>%
  mutate(quarter = 2)
expd193 <- read_dta("../0_Data/1_Household Data/3_USA/1_Data_Raw/diary19/expd193.dta")%>%
  mutate(quarter = 3)
expd194 <- read_dta("../0_Data/1_Household Data/3_USA/1_Data_Raw/diary19/expd194.dta")%>%
  mutate(quarter = 4)

exp_diary <- bind_rows(expd191, expd192, expd193, expd194)%>% #all are distinct hhs, put them together
  select(newid, everything())%>%
  rename(hh_id = newid)

# tmp  <- exp_diary[1,] %in% c("A", "B", "C", "D", "E", "F", "G", "H", "T", "U", "V", "W") #all cols that are flags
# exp_diary <- exp_diary[colnames(exp_diary)[!tmp]] #subset dataframe with all cols that are not flags using tmp (line above)
  
# summary exp, hh level income, hh characteristics + weights - diary
fmld191 <- read_dta("../0_Data/1_Household Data/3_USA/1_Data_Raw/diary19/fmld191.dta")%>%
  mutate(quarter = 1)
fmld192 <- read_dta("../0_Data/1_Household Data/3_USA/1_Data_Raw/diary19/fmld192.dta")%>%
  mutate(quarter = 2)
fmld193 <- read_dta("../0_Data/1_Household Data/3_USA/1_Data_Raw/diary19/fmld193.dta")%>%
  mutate(quarter = 3)
fmld194 <- read_dta("../0_Data/1_Household Data/3_USA/1_Data_Raw/diary19/fmld194.dta")%>%
  mutate(quarter = 4)

fml_diary <- bind_rows(fmld191, fmld192, fmld193, fmld194)%>% #all are distinct hhs, put them together
  select(newid, everything())%>%
  rename(hh_id = newid)

# tmp       <- fml_diary[1,] %in% c("A", "B", "C", "D", "E", "F", "G", "H", "T", "U", "V", "W") #all cols that are flags
# fml_diary <- fml_diary[colnames(fml_diary)[!tmp]] #subset dataframe with all cols that are not flags using tmp (line above)

# remove all smaller files
rm(expd191, expd192, expd193, expd194, fmld191, fmld192, fmld193, fmld194, memd191, memd192,memd193, memd194)

### INTERVIEW (files used: FMLI - hh exp., income & charact; MTBI - monthly exp.; MEMI - hh member income and charact.,
###            files not used: ITBI - detailed income; ITII - imputed income iterations; NTAXI - taxes estimations; 
###                            FPAR & MCHI - survey process and contact history [in /para19]; more detailes exp data [in /expn19])
### Files marked with an "x" are NOT USED, they appeared in the previous survey too and were processed under different standards, so contents slightly differ

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


fml_iv <- bind_rows(fmli182, fmli183, fmli184, fmli191, fmli192, fmli193, fmli194)%>% #all are distinct hhs, put them together
  select(newid, everything())%>%
  rename(hh_id = newid)%>%
  mutate(id = gsub('.$', '', hh_id))
tmp <- unique(gsub('.$', '', fmli191$newid))
fml_iv <- filter(fml_iv, id %in% tmp)

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
tmp <- unique(gsub('.$', '', mtbi191$newid))
mtb_iv <- filter(mtb_iv, id %in% tmp)

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
tmp <- unique(gsub('.$', '', memi191$newid))
mem_iv <- filter(mem_iv, id %in% tmp)

# tmp    <- mem_iv[1,] %in% c("A", "B", "C", "D", "E", "F", "G", "H", "T", "U", "V", "W") #all cols that are flags
# mem_iv <- mem_iv[colnames(mem_iv)[!tmp]] #subset dataframe with all cols that are not flags using tmp (line above)

#rm unnecessary smaller files
rm(tmp, memi182, memi183, memi184, memi191, memi192, memi193, memi194,
        mtbi182, mtbi183, mtbi184, mtbi191, mtbi192, mtbi193, mtbi194, 
        fmli182, fmli183, fmli184, fmli191, fmli192, fmli193, fmli194)

# Transform Data ####

#______ We proceed using the Interview data, it's representative and comprises of more participants ______#
# each dataset consists of 4 quarters, second of 2019 to first of 2020, we aggregate them for more comprehensive expenditure data
#   -> household information that is not expenditure, we use the latest available dataset in each household
# we later do the same for the diary data and compare them for robustness

# collect household information, appliances and expenditure information (including item code descriptions) in this order

### HOUSEHOLD INFORMATION

# Household information
#collect relevant data from most convenient sources, link them through newid
mem_iv_1 <- mem_iv %>%
  filter(cu_code == 1)%>% # reference person
  filter(grepl("1$", hh_id))%>%
  rename(age_hhh = age, sex_hhh = sex, ind_hhh = occucode, edu_hhh = educa,
         ethnicity = membrace, ethnicity_2.1 = asian, ethnicity_2 = hispanic)%>%
  select(hh_id = id, age_hhh, sex_hhh, ind_hhh, edu_hhh, ethnicity, ethnicity_2.1, ethnicity_2)%>%
  mutate(ethnicity     = ifelse(ethnicity == 4 | ethnicity == 6, as.numeric(ethnicity_2.1)+10, ethnicity))%>%
  select(-ethnicity_2.1)%>%
  distinct()

mem_iv_2 <- mem_iv %>%
  mutate(adults   = ifelse(age > 15,1,0),
         children = ifelse(age < 16,1,0))%>%
  filter(grepl("1$", hh_id))%>%
  select(hh_id = id, adults, children, socrrx)%>%
  mutate(socrrx = ifelse(is.na(socrrx),0, socrrx))%>%
  group_by(hh_id)%>%
  summarise(adults           = sum(adults),
            children         = sum(children),
            hh_size          = n(),
            inc_gov_monetary = sum(socrrx),
            inc_gov_cash     = 0)%>%
  ungroup()%>%
  distinct()

fml_iv_1 <- fml_iv %>%
  filter(grepl("1$", hh_id))%>%
  mutate(urban_01 = ifelse(bls_urbn == 1,1,0))%>%
  rename(province = division, district = state, hh_weights = finlwt21)%>%
  select(hh_id = id, urban_01, province, district, hh_weights)%>%
  mutate(hh_weights = hh_weights/4)

household_information <- mem_iv_1 %>%
  left_join(mem_iv_2)%>%
  left_join(fml_iv_1)%>%
  rowwise()%>%
  mutate(pop = hh_size*hh_weights)
  
write_csv(household_information, "../0_Data/1_Household Data/3_USA/1_Data_Clean/household_information_USA.csv")

# Appliances --> maybe look at apla ? ####


### APPLIANCES_1_0 are looking a bit slim, the apl file from the documentation has stopped being included as of 2013
# toilet = bathrmq+hlfbathq > 0
# AC unit = windowac + cntralac > 0
# solar panels = solarpnl > 0
# 

# Expenditures ####

# Start with interview --> add diary (for shares)

### EXPENDITURE
# not all households are in the set for all 4 quarters in which the interviews were conducted
# -> we normalize the expenditures to yearly expenses for all households, including those with missing interviews
exp <- mtb_iv %>%
  mutate(individual_quarter = strtoi(str_sub(hh_id, -1)))%>%
  group_by(id)%>%
  mutate(completeness = ifelse(1%in%individual_quarter, 1, 0))%>%
  mutate(completeness = ifelse(2%in%individual_quarter, completeness + 1, completeness))%>%
  mutate(completeness = ifelse(3%in%individual_quarter, completeness + 1, completeness))%>%
  mutate(completeness = ifelse(4%in%individual_quarter, completeness + 1, completeness))%>%
  ungroup()

exp2 <- exp%>%
  group_by(id, ucc)%>%
  mutate(costsum = sum(cost))%>%
  summarise(cost = (costsum/completeness)*4)%>%
  ungroup()%>%
  distinct(.)%>%
  group_by(id)%>%
  mutate(exp = sum(cost))%>%
  ungroup()


### EXPENDITURE-CODES
# the Hierarchical groupings for all years are found in the stubs.zip file found on the official website 
# https://www.bls.gov/cex/pumd_doc.htm
# it contains all UCC codes and their description for each year of both interviews and diaries
# load the interview-files for 2019 and 2020, extract codes from exp and then match codes to descriptions
ucc_codes_raw <- read.xlsx("../0_Data/1_Household Data/3_USA/9_Documentation/ce_source_integrate.xlsx", startRow = 4, sheet = 1)
ucc_codes <- ucc_codes_raw%>%
  filter(!is.na(y19))%>%
  select(description = Description, ucc = UCC)
exp_codes <- exp2[,2]%>%
  distinct(.)%>%
  left_join(., ucc_codes)%>%
  distinct()

exp_codes_diary <- exp_diary[,6]%>%
  distinct(.)%>%
  left_join(., ucc_codes)%>%
  distinct()
  
apa <- read_dta("../0_Data/1_Household Data/3_USA/1_Data_Raw/intrvw19/expn19/apa19.dta")

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
  write_csv(., "../0_Data/1_Household Data/3_USA/2_Codes/Education.Code.csv")
Ethnicity.Code <- distinct(mem_iv_1, ethnicity)%>%
  mutate(ethnicity = as.numeric(ethnicity))%>%
  arrange(ethnicity)%>%
  mutate(Ethnicity = c("White", "Black", "Native American", "Pacific Islander", 
                       "Asian or Multi-race - Chinese","Asian or Multi-race - Filipino", "Asian or Multi-race - Japanese",
                       "Asian or Multi-race - Korean", "Asian or Multi-race - Vietnamese", "Asian or Multi-race - Asian Indian", "Asian or Multi-race - Other", NA))%>%
  write_csv(., "../0_Data/1_Household Data/3_USA/2_Codes/Ethnicity.Code.csv")
Ethnicity.Code.2 <- distinct(mem_iv, hispanic)%>%
  arrange(hispanic)%>%
  rename(ethnicity_2 = hispanic)%>%
  mutate(Ethnicity_2 = c("Mexican", "Mexican-American", "Chicano", "Puerto Rican", "Cuban", "Other", NA))%>%
  write_csv(., "../0_Data/1_Household Data/3_USA/2_Codes/Ethnicity.2.Code.csv")
Province.Code <- distinct(fml_iv, division)%>%
  arrange(division)%>%
  rename(province = division)%>%
  mutate(Province = c(NA, "New England","Middle Atlantic","East North Central","West North Central","South Atlantic","East South Central",
  "West South Central","Mountain","Pacific"))%>%
  write_csv(., "../0_Data/1_Household Data/3_USA/2_Codes/Province.Code.csv")
District.Code <- distinct(fml_iv, state)%>%
  arrange(state)%>%
  rename(district = state)%>%
  mutate(District = c(NA, "Alabama","Alaska","Arizona","California","Colorado","Connecticut","Delaware","District of Columbia","Florida","Georgia",
                      "Hawaii","Illinois","Indiana","Iowa","Kansas","Kentucky","Louisiana",
                      "Maryland","Massachuse","Michigan","Minnesota","Mississippi","Missouri","Nebraska","Nevada","New Hampshire","New Jersey",
                      "New York","North Carolina","Ohio","Oklahoma","Oregon","Pennsylvania","South Carolina","Tennessee","Texas","Utah","Virginia",
                      "Washington","Virginia","Wisconsin"))%>%
  write_csv(., "../0_Data/1_Household Data/3_USA/2_Codes/District.Code.csv")
