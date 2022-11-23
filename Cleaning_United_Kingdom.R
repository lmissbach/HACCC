library(tidyverse)
library(Hmisc)
library(haven)
library(readr)
library(openxlsx)
library(dplyr)

path <- "T:/MSA/papers_internal/work_in_progress/Mi_Homogenized_Datainfrastructure/0_Data/1_Household Data"
setwd(dir = path)

raw_per <- read_dta("./4_United_Kingdom/1_Data_Raw/2018_rawper_ukanon_final.dta")
raw_hh <- read_dta("./4_United_Kingdom/1_Data_Raw/2018_rawhh_ukanon.dta")
derived_hh <- read_dta("./4_United_Kingdom/1_Data_Raw/2018_dvhh_ukanon.dta")
derived_per <- read_dta("./4_United_Kingdom/1_Data_Raw/2018_dvper_ukanon201819.dta")
exp <- read_dta("./4_United_Kingdom/1_Data_Raw/2018_dv_set89_ukanon.dta")
codes <- read.xlsx("./4_United_Kingdom/9_Documentation/Item_Codes.xlsx")

#extract household reference person (we'll use them as hhhfor simplicity) from derived_per
hhh_derived <- derived_per %>%
  filter(A003==1)

#now unite information on household and hhh in one table (derived_hh+hhh_derived)
data <- merge(hhh_derived, derived_hh)

#extract vars of interest for household information
household_information_United_Kingdom <- data %>%
  select(hh_id = case, hh_size = A049,  hh_weights = weighta,  
         age_hhh=a005p, sex_hhh = A004, edu_hhh_current = A007, edu_hhh_age_finished = A010, employment_hhh = A015,
         A040, A041, A042, A043, A044, A045, A046, A047, A049,ethicity=a012p, 
         province = Gorx, urb1 = URGridEWp, urb2 = URGridSCp) %>% #, age_oldest=a070p
  mutate(children= A040+A041+A042, adults=A043+A044+A045+A046+A047)%>%
  mutate(urb1 = ifelse(is.na(urb1), 2, ifelse(urb1==2, 0, 1)),
         urb2 = ifelse(is.na(urb2), 2, ifelse(urb2==2, 0, 1)),
         urban_01 = urb1+urb2-2)%>%
  mutate(urban_01 = ifelse(urban_01== 2, NA, ifelse(urban_01==0, 0, 1)))%>%
  select(hh_id, hh_size, hh_weights, age_hhh, sex_hhh, edu_hhh_current, edu_hhh_age_finished, employment_hhh, ethicity, province, urban_01) %>%
  write_csv(., "./4_United_Kingdom/1_Data_Clean/household_information_United_Kingdom.csv")

#clean up item code file
codes_long <- read.xlsx("./4_United_Kingdom/9_Documentation/8686_volume_d_expenditure_codes_201819.xlsx", sheet=3)
  
codes <- codes_long%>%
  select(code=COIPLUS.Code, description = COIPLUS.Description)%>%
  distinct(.)

write.xlsx(codes,"./4_United_Kingdom/9_Documentation/Item_Codes.xlsx")

#other codes
Province.Code <- stack(attr(data$Gorx, 'labels'))%>%
  rename(province = values, Province = ind)%>%
  write_csv(., "4_United_Kingdom/2_Codes/Province.Code.csv")
Gender.Code <- stack(attr(data$A004, 'labels'))%>%
  rename(sex_hh = values, Gender = ind)%>%
  write_csv(., "4_United_Kingdom/2_Codes/Gender.Code.csv")
Ethnicity.Code <- stack(attr(data$a012p, 'labels'))%>%
  rename(ethnicity = values, Ethnicity = ind)%>%
  write_csv(., "4_United_Kingdom/2_Codes/Ethnicity.Code.csv")
Employment.Code <- stack(attr(data$A015, 'labels'))%>%
  rename(employment_hhh = values, Employment_Status = ind)%>%
  write_csv(., "4_United_Kingdom/2_Codes/Employment.Code.csv")
Current_Education.Code <- stack(attr(data$A007, 'labels'))%>%
  rename(edu_hhh_current = values, Current_Education = ind)%>%
  write_csv(., "4_United_Kingdom/2_Codes/Current_Education.Code.csv")
Urban01.Code <- data.frame(urban_01 = c(0,1), Urban_Rural = c("Rural", "Urban"))%>%
  write_csv(., "4_United_Kingdom/2_Codes/Urban01.Code.csv")

#appliances 1/0
appliance_0_1_United_Kingdom<- data%>%
  select(PC= A1661, TV= A1711, Internet = A172, PC_internet_access = A190, 
         Mobile_Phone_internet_access = A192, other_internet_access=A194)%>%
  mutate(PC = ifelse(PC==2, 0, ifelse(PC==0, 0, 1))) %>%
  mutate(TV = ifelse(PC==2, 0, ifelse(TV==0, 0, 1))) %>%
  mutate(Internet = ifelse(PC==2, 0, ifelse(Internet==0, 0, 1))) %>%
  write_csv(., "4_United_Kingdom/1_Data_Clean/appliances_0_1_United_Kingdom.csv")



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

der_var_desc <-read.xlsx("./4_United_Kingdom/9_Documentation/8686_volume_f_derived_variables_201819.xlsx",
                         sheet = "Part 2", startRow = 7)
dvdes <- der_var_desc %>%
  filter(`In.the.UKDA.dataset?` == "Yes")%>%
  select(Variable, Description)
dvde<-(dvdes[c(441:1893),])%>%
  subset(!grepl("[c,C]\\d{5}[c,w, x, y, z, l, t]", Variable)) %>%
  subset(!grepl("[c,C][a-z,A-Z]\\d{4}[c,w, x, y, z, l, t]", Variable))%>%
  subset(!grepl("[c,C][a-z,A-Z]\\d{3}[a-z,A-Z][c,w, x, y, z, l, t]", Variable))%>%
  subset(!grepl("[c,C]\\d{4}[a-z,A-Z][c,w, x, y, z, l, t]", Variable))%>%
  subset(!grepl("[p,P]\\d{3}[c,w, x, y, z, l, t]", Variable))


expenditures <- data %>%
  select(case,matches("C\\d{5}") & !ends_with(c("c", "w", "x", "y", "z", "l", "t")), 
         matches("C[A-Z]{1}\\d{4}")& !ends_with(c("c", "w", "x", "y", "z", "l", "t")))

#prepare and save expenditures
expenditures_items_United_Kingdom <- expenditures%>%
  rename(hh_id = case)%>%
  pivot_longer(-c(hh_id), names_to="item_codes", values_to = "expenditures_year") %>%
  arrange(expenditures_year, item_codes)%>%
  #expenditures are recorded in weekly amounts, multiply by 52 to get "yearly"
  mutate(expenditures_year= expenditures_year*52)%>%
  write_csv(., "./4_United_Kingdom/1_Data_Clean/expenditures_items_United_Kingdom.csv")
  
item_codes <- expenditures%>%
  filter(case == 1)%>%
  pivot_longer(-c(case), names_to="Variable", values_to = "expenditures_year")%>%
  mutate(Variable = tolower(Variable))%>%
  merge(., dvde, by = "Variable")%>%
  select(item_code = Variable, item_name = Description)#%>%
  write.xlsx(., "4_United_Kingdom/3_Matching_Tables/Item_Codes_Description_United_Kingdom.xlsx")

expenditures2 <- exp%>%
  rename(code = COI_PLUS)%>%
  group_by(case, code)%>%
  mutate(exp = sum(pdamount))%>%
  ungroup()
tmp <- filter(expenditures2, Person == 1)
x <- distinct(tmp, case)
expenditures2 <- expenditures2%>%
  mutate(Person = ifelse(!(case %in% x$case), 1, Person))%>%
  filter(Person == 1)%>%
  select(case, code, exp)%>%
  write_csv(., "./4_United_Kingdom/1_Data_Clean/expenditures_items2_United_Kingdom.csv")

expenditures2_codes <- distinct(expenditures2, code, .keep_all = TRUE)%>%
  merge(., codes, by = "code")%>%
  select(item_code = code, item_name = description)%>%
  write.xlsx(., "4_United_Kingdom/3_Matching_Tables/Item_Codes_Description2_United_Kingdom.xlsx")

