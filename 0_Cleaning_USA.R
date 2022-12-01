library(tidyverse)
library(Hmisc)
library(haven)
library(readr)
library(openxlsx)
library(dplyr)
library(stringr)  

path <- "../0_Data/1_Household Data"
#setwd(dir = path)

# pre-existing summarized files for diary and interview for double checking (author is Leonard?)
dia_exp_pre <- read_rds(paste0(path, "/3_USA/1_Data_Raw/diary19/expenditure_information.rds"))
dia_hh_pre <- read_rds(paste0(path, "/3_USA/1_Data_Raw/diary19/household_description.rds"))

int_exp_pre <- read_rds(paste0(path, "/3_USA/1_Data_Raw/intrvw19/expenditure_information.rds"))
int_hh_pre <- read_rds(paste0(path, "/3_USA/1_Data_Raw/intrvw19/household_description.rds"))


##### General reading and formatting, Diary first (3 file types MEMD, EXPD, FMLD), 
##### then Interview (3 file types FMLI, MTBI, MEMI) 
##### all details found on ( https://www.bls.gov/cex/pumd-getting-started-guide.htm )


### DIARY (files used: MEMD - hh-member charact. + income; EXPD - detailed exp; FMLD - summary exp, hh income, hh charact. + weights)
###        files not used: dtbt -  detailed income; dtid - income imputaions iterations)

#information on household members characteristics + income - diary
memd191 <- read_dta(paste0(path, "/3_USA/1_Data_Raw/diary19/memd191.dta"))
memd192 <- read_dta(paste0(path, "/3_USA/1_Data_Raw/diary19/memd192.dta"))
memd193 <- read_dta(paste0(path, "/3_USA/1_Data_Raw/diary19/memd193.dta"))
memd194 <- read_dta(paste0(path, "/3_USA/1_Data_Raw/diary19/memd194.dta"))

memd <- rbind(memd191, memd192,memd193, memd194)%>% #all are distinct hhs, put them together
  distinct %>%
  select(newid, !newid) #make sure newid is first col
tmp <-memd[1,] %in% c("A", "B", "C", "D", "E", "F", "G", "H", "T", "U", "V", "W") #all cols that are flags
memd <- memd[colnames(memd)[!tmp]] #subset dataframe with all cols that are not flags using tmp (line above)

# detailed exp data - diary
expd191 <- read_dta(paste0(path, "/3_USA/1_Data_Raw/diary19/expd191.dta"))
expd192 <- read_dta(paste0(path, "/3_USA/1_Data_Raw/diary19/expd192.dta"))
expd193 <- read_dta(paste0(path, "/3_USA/1_Data_Raw/diary19/expd193.dta"))
expd194 <- read_dta(paste0(path, "/3_USA/1_Data_Raw/diary19/expd194.dta"))

expd <- rbind(expd191, expd192, expd193, expd194)%>% #all are distinct hhs, put them together
  distinct%>% #make sure no dups
  select(newid, !newid&!pub_flag) #make sure newid is first col and rm extra flag var pub_flag
tmp <-expd[1,] %in% c("A", "B", "C", "D", "E", "F", "G", "H", "T", "U", "V", "W") #all cols that are flags
expd <- expd[colnames(expd)[!tmp]] #subset dataframe with all cols that are not flags using tmp (line above)
  
# summary exp, hh level income, hh characteristics + weights - diary
fmld191 <- read_dta(paste0(path, "/3_USA/1_Data_Raw/diary19/fmld191.dta"))
fmld192 <- read_dta(paste0(path, "/3_USA/1_Data_Raw/diary19/fmld192.dta"))
fmld193 <- read_dta(paste0(path, "/3_USA/1_Data_Raw/diary19/fmld193.dta"))
fmld194 <- read_dta(paste0(path, "/3_USA/1_Data_Raw/diary19/fmld194.dta"))

fmld <- rbind(fmld191, fmld192, fmld193, fmld194)%>% #all are distinct hhs, put them together
  select(newid, !newid) #make sure newid is first col
tmp <-fmld[1,] %in% c("A", "B", "C", "D", "E", "F", "G", "H", "T", "U", "V", "W") #all cols that are flags
fmld <- fmld[colnames(fmld)[!tmp]] #subset dataframe with all cols that are not flags using tmp (line above)

# remove all smaller files
rm(expd191, expd192, expd193, expd194, fmld191, fmld192, fmld193, fmld194, memd191, memd192,memd193, memd194)


### INTERVIEW (files used: FMLI - hh exp., income & charact; MTBI - monthly exp.; MEMI - hh member income and charact.,
###            files not used: ITBI - detailed income; ITII - imputed income iterations; NTAXI - taxes estimations; 
###                            FPAR & MCHI - survey process and contact history [in /para19]; more detailes exp data [in /expn19])
### Files marked with an "x" are NOT USED, they appeared in the previous survey too and were processed under different standards, so contents slightly differ

# hh level exp., income and characteristics - interview
fmli192 <- read_dta(paste0(path, "/3_USA/1_Data_Raw/intrvw19/intrvw19/fmli192.dta"))
fmli193 <- read_dta(paste0(path, "/3_USA/1_Data_Raw/intrvw19/intrvw19/fmli193.dta"))
fmli194 <- read_dta(paste0(path, "/3_USA/1_Data_Raw/intrvw19/intrvw19/fmli194.dta"))
fmli201 <- read_dta(paste0(path, "/3_USA/1_Data_Raw/intrvw19/intrvw19/fmli201.dta"))

fmli <- rbind(fmli192, fmli193, fmli194, fmli201)%>% #all are distinct hhs, put them together
  distinct %>%
  select(newid, !newid) #make sure newid is first col
tmp <-fmli[1,] %in% c("A", "B", "C", "D", "E", "F", "G", "H", "T", "U", "V", "W") #all cols that are flags
fmli <- fmli[colnames(fmli)[!tmp]] #subset dataframe with all cols that are not flags using tmp (line above)

# monthly expenditures - interview
mtbi192 <- read_dta(paste0(path, "/3_USA/1_Data_Raw/intrvw19/intrvw19/mtbi192.dta"))
mtbi193 <- read_dta(paste0(path, "/3_USA/1_Data_Raw/intrvw19/intrvw19/mtbi193.dta"))
mtbi194 <- read_dta(paste0(path, "/3_USA/1_Data_Raw/intrvw19/intrvw19/mtbi194.dta"))
mtbi201 <- read_dta(paste0(path, "/3_USA/1_Data_Raw/intrvw19/intrvw19/mtbi201.dta"))

mtbi <- rbind(mtbi192, mtbi193, mtbi194, mtbi201)%>% #all are distinct hhs, put them together
  distinct %>%
  select(newid, !newid&!pubflag) #make sure newid is first col and rm extra flag var pub_flag
tmp <-mtbi[1,] %in% c("A", "B", "C", "D", "E", "F", "G", "H", "T", "U", "V", "W") #all cols that are flags
mtbi <- mtbi[colnames(mtbi)[!tmp]] #subset dataframe with all cols that are not flags using tmp (line above)

# hh member income and characteristics - interview
memi192 <- read_dta(paste0(path, "/3_USA/1_Data_Raw/intrvw19/intrvw19/memi192.dta"))
memi193 <- read_dta(paste0(path, "/3_USA/1_Data_Raw/intrvw19/intrvw19/memi193.dta"))
memi194 <- read_dta(paste0(path, "/3_USA/1_Data_Raw/intrvw19/intrvw19/memi194.dta"))
memi201 <- read_dta(paste0(path, "/3_USA/1_Data_Raw/intrvw19/intrvw19/memi201.dta"))

memi <- rbind(memi192, memi193, memi194, memi201)%>% #all are distinct hhs, put them together
  distinct %>%
  select(newid, !newid) #make sure newid is first col
tmp <-memi[1,] %in% c("A", "B", "C", "D", "E", "F", "G", "H", "T", "U", "V", "W") #all cols that are flags
memi <- memi[colnames(memi)[!tmp]] #subset dataframe with all cols that are not flags using tmp (line above)

#rm unnecessary smaller files
rm(tmp, memi192, memi193, memi194, memi201, mtbi192, mtbi193, mtbi194, mtbi201, fmli192, fmli193, fmli194, fmli201)



#______ We proceed using the Interview data, it's representative and comprises of more participants ______#
# each dataset consists of 4 quarters, second of 2019 to first of 2020, we aggregate them for more comprehensive expenditure data
#   -> household information that is not expenditure, we use the latest available dataset in each household
# we later do the same for the diary data and compare them for robustness

# collect household information, appliances and expenditure information (including item code descriptions) in this order

### HOUSEHOLD INFORMATION
#collect relevant data from most convenient sources, link them through newid

hh_memi <- memi%>% #adults, children, race (with race code included)
  group_by(newid)%>%
  mutate(adults = sum(age >= 18), 
         children = sum(age <18),
         #adult equivalents as per OECD-modified scale (see: https://www.oecd.org/els/soc/OECD-Note-EquivalenceScales.pdf )
         adult_eq = ifelse(adults > 0, (1 + (adults-1)*0.5 + children * 0.3), 1 + children * 0.3),
         #rc_** variables are set to 1..7 if member reported the corresponding mixed ethnicities, so rc_natam == 3 if reported, rc_white == 1 if reported, etc.
         #we create a string concatenating all reported ethnicities of len 0 if not mixed or no concise report to (possibly) len 7 if all ethnicities reported
         mixed_eth = gsub(" ", "", ifelse(membrace == 6, paste(rc_white, rc_black, rc_natam, rc_asian, rc_pacil, rc_other, rc_dk), "")))%>%
  ungroup%>%
  #filter out only reference person, if more granular information is wanted, remove this
  filter(cu_code == 1)%>%
  select(newid, adults, children, adult_eq, membrace, mixed_eth, asian, hispanic)
  

hh_fmli <- fmli%>%
  select(newid, fam_size, finlwt21, state, region, psu, urban_01 = bls_urbn,
         sex_ref, age_ref, educ_ref, occucod1)%>%
  #region is: midwest, south, west, northeast
  #psu is larger metro area for (most) urban households
  mutate(urban_01 = ifelse(urban_01 == 2, 0, (ifelse(urban_01 == 1, 1, urban_01)))) #transform bls_urbn's 1/2 format to 0/1 format

#connect all files via newid
household_information1 <- hh_fmli%>%
  inner_join(., hh_memi)
  

# define df with variable name correspondence, old-new
names_hhinfo <- data.frame(old = colnames(household_information1),
                           new = c("hh_id", "hh_size", "hh_weights", "province", "region", "district", "urban_01", "sex_hhh", "age_hhh", "edu_hhh", 
                                   "ind_hhh", "adults", "children", "adult_eq", "ethnicity", "mixed_eth", "country_asian", "country_hispanic"))

# save under new name before renaming using "lookup-table" names_hhinfo (old names in household_information1 are used again later)
household_information <- household_information1
names(household_information) = names_hhinfo$new[match(names(household_information1), names_hhinfo$old)]

#save household information
write.csv(household_information, paste0(path, "/3_USA/1_Data_Clean/household_information_usa.csv"))

### CODES for decyphering household information
# all codes are listed in the following file, they need to be extracted and stored in separate files
codes_all <- read.xlsx(paste0(path, "/3_USA/9_Documentation/ce_pumd_interview_diary_dictionary.xlsx"), sheet = 3)

# select all codes from our files (MEMI + FMLI), which are present as variables in household_information1 (old names) 
codes <- codes_all%>%
  select(file = File, var = Variable, code = Code.value, description = Code.description)%>%
  filter(var %in% toupper(colnames(household_information1)))%>%
  filter(file == "MEMI" | file == "FMLI")

# extract all individual code lists by
#   iterating over the old variables names from household_information1
#   filtering all entries of said variable in the code file
#   check if entries exist, then create new_name using lookup table names_hhinfo
#     use this to rename the variable names stored in var
#     + rename the column "code" (as per our convention)
#     + rename the column "description" to capitalized version (with some exceptions see case_when) (as per our convention)
#   then append code dataframe to code_list 
#   and save code file, which includes only code and description 
i <-1
code_list <- list()
for (var_name in colnames(household_information1)) {
  tmp_df <- distinct(filter(codes, var == toupper(var_name)))
  if(nrow(tmp_df) != 0){
    new_name <- names_hhinfo$new[match(var_name, names_hhinfo$old)]
    tmp_df <- tmp_df%>%
      mutate(var = new_name)
    names(tmp_df)[3] = all_of(new_name)
    nn <- case_when(
      new_name == "edu_hhh" ~ "Education",
      new_name == "ind_hhh" ~ "Industry",
      new_name == "sex_hhh" ~ "Gender",
      TRUE ~ new_name
    )
    names(tmp_df)[4] = all_of(str_to_title(nn))
    code_list[[i]] <- tmp_df
    i = i+1
    tmp_df <- select(tmp_df, new_name, str_to_title(nn))
    write_csv(tmp_df, paste0(path, "/3_USA/2_Codes/", str_to_title(nn), ".Code.csv"))
  }
}
rm(tmp_df, var_name, new_name, nn, i)
# manually create extra codes for urban_01 and mixed_eth
urban_01_code <- data.frame(urban_01 = c(0,1),
                       Urban = c("Rural", "Urban"))
write_csv(urban_01_code, paste0(path, "/3_USA/2_Codes/Urban.Code.csv"))

# create code that lists all ethnicities in mixed ethnicities, translation taken from following variables contained in MEMI
# rc_white, rc_black, rc_natam, rc_asian, rc_pacil, rc_other, rc_dk
tmp <- sort(unique(household_information$mixed_eth))[-1]
tmp2 <- gsub("1", "White ", tmp)
tmp2 <- gsub("2", "Black ", tmp2)
tmp2 <- gsub("3", "Native American ", tmp2)
tmp2 <- gsub("4", "Asian ", tmp2)
tmp2 <- gsub("5", "Pacific Islander ", tmp2)
tmp2 <- gsub("6", "Other ", tmp2)
tmp2 <- gsub("7", "Don't know ", tmp2)
tmp2 <- unlist(lapply(tmp2, trimws))
mixed_eth_code <- data.frame(mixed_eth = tmp,
                             Mixed_Ethnicity = tmp2) 
write_csv(mixed_eth_code, paste0(path, "/3_USA/2_Codes/Mixed_Ethnicity.Code.csv"))
rm(tmp, tmp2)

###### THIS REMOVES ALL CODES; COMMENT LINE FOR GRANULAR INSPECTION
rm(code_list, codes, codes_all, mixed_eth_code, urban_01_code)

### APPLIANCES_1_0 are looking a bit slim, the apl file from the documentation has stopped being included as of 2013
# toilet = bathrmq+hlfbathq > 0
# AC unit = windowac + cntralac > 0
# solar panels = solarpnl > 0
# 

### EXPENDITURE
# not all households are in the set for all 4 quarters in which the interviews were conducted
# -> we normalize the expenditures to yearly expenses for all households, inclduing those with missing interviews
exp <- mtbi %>%
  mutate(quarter = strtoi(str_sub(newid, -1)))%>%
  mutate(id = gsub('.$', '', newid))%>%
  group_by(id)%>%
  mutate(completeness = ifelse(1%in%quarter, 1, 0))%>%
  mutate(completeness = ifelse(2%in%quarter, completeness + 1, completeness))%>%
  mutate(completeness = ifelse(3%in%quarter, completeness + 1, completeness))%>%
  mutate(completeness = ifelse(4%in%quarter, completeness + 1, completeness))%>%
  group_by(id, ucc)%>%
  mutate(costsum = sum(cost))%>%
  summarise(cost = (costsum/completeness)*4)%>%
  distinct(.)

### EXPENDITURE-CODES
# the Hierarchical groupings for all years are found in the stubs.zip file found on the official website 
# https://www.bls.gov/cex/pumd_doc.htm
# it contains all UCC codes and their description for each year of both interviews and diaries
# load the interview-files for 2019 and 2020, extract codes from exp and then match codes to descriptions
ucc_codes_raw <- read.xlsx(paste0(path, "/3_USA/9_Documentation/ce_source_integrate.xlsx"), startRow = 4, sheet = 1)
ucc_codes <- ucc_codes_raw%>%
  filter(!is.na(y19))%>%
  select(description = Description, ucc = UCC)
exp_codes <- exp[,2]%>%
  distinct(.)%>%
  left_join(., ucc_codes)
  
apa <- read_dta(paste0(path, "/3_USA/1_Data_Raw/intrvw19/expn19/apa19.dta"))
