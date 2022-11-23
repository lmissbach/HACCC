library(tidyverse)
library(Hmisc)
library(haven)
library(readr)
library(openxlsx)
library(dplyr)

path <- "T:/MSA/papers_internal/work_in_progress/Mi_Homogenized_Datainfrastructure/0_Data/1_Household Data"
setwd(dir = path)

data_raw <- read_dta("./4_Norway/1_Data_Raw/Consumer Expenditure Survey, 2012.dta")

#read in data with translated labels
data_labeled<- read_dta("4_Norway/1_Data_Raw/data_labeled.dta")
##Process of translating the first-level variable labels##

# data_labeled <- data_raw
# 
# #get var labels, save, translate externally
# columns <- colnames(data_raw)
# sink("./4_Norway/1_Data_Clean/labels.txt")
# for (n in c(1:1258)) {
#   print(attr(data_raw[[n]],'label'))
# }
# sink()
# 
# #import translated labels and overwrite old labels
# new_labels <- scan("./4_Norway/1_Data_Clean/labelsEN.txt", what = "c", sep= "\n")
# for(n in c(1:1258)){
#   attr(data_labeled[[n]], 'label') <- new_labels[n]
# }
# write_dta(data_labeled, "4_Norway/1_Data_Raw/data_labeled.dta")

#filter out invalid households and do general cleanup
data<- data_labeled %>%
  #total expenses are negative:
  filter(sfutg_c>0) %>%
  #filter exact duplicates
  distinct(lopenr, .keep_all = TRUE) %>%
  #filter negative total net income
  filter(aggi_24_2012 > 0) %>%
  #make education codes higher-level
  mutate(utdanning = as.numeric(substr(utdanning,1,1)))

#save codes
Gender.Code <- stack(attr(data$kjonn1, 'labels'))%>%
  rename(sex_hhh = values, Gender = ind)%>%
  write_csv(., "4_Norway/2_Codes/Gender.Code.csv")
Industry.Code <- stack(attr(data$knaer1, 'labels')) %>%
  rename(ind_hhh = values, Industry = ind)%>%
  write_csv(., "4_Norway/2_Codes/Industry.Code.csv")
Education.Code <- read.csv("4_Norway/2_Codes/1659.csv", header = TRUE, sep = ";") %>%
  filter(code < 10) %>%
  filter(level ==1) %>%
  select(edu_hhh = code, Education = name) %>%
  write_csv(., "4_Norway/2_Codes/Education.Code.csv")

#extract vars of interest for household information
household_information_Norway <- data %>%
  select(hh_id = lopenr, hh_size = antpersh,  hh_weights = fvekt,  
         ald1,  ald2,  ald3,  ald4,  ald5,  ald6,  ald7,  ald8,  ald9,  ald10, 
         sex_hhh = kjonn1, ind_hhh = knaer1, edu_hhh = utdanning, 
         income_year11 = aggi_24_2011, income_year12 = aggi_24_2012, expenditures_year = sfutg_c, nuts2) %>%
  mutate(age1 = ald1>17,  age2 = ald2>17,  age3 = ald3>17,  age4 = ald4>17,  age5 = ald5>17,  age6 = ald6>17,  age7 = ald7>17,  
         age8 = ald8>17,  age9 = ald9>17,  age10 = ald10>17) %>%
  rename(age_hhh = ald1) %>%
  select(!ald2:ald10) %>%
  rowwise() %>%
  mutate(adults = sum(age1, age2, age3, age4, age5, age6, age7, age8, age9, age10, na.rm = TRUE), 
         children = ifelse(hh_size> adults,hh_size - adults,0))%>%
  select(!age1:age10)%>%
  write_csv(., "4_Norway/1_Data_Clean/household_information_Norway.csv")

#appliances y/n
appliances_0_1_Norway <- data %>%
  select(hh_id = lopenr, contains("sp43eg"), spm40a) %>%
  rename(Stove = sp43eg1, Microwave = sp43eg2, Washing_Machine =sp43eg3,
  Fridge_Freezer_Combo=sp43eg4, Fridge = sp43eg5, Freezer = sp43eg6,
  Sewing_Machine= sp43eg7, Drying_Cabinet = sp43eg8,Dishwasher = sp43eg9,
  Camping_Trailer = sp43eg10, Motorcycle = sp43eg11, TV = sp43eg12, Mobile_Phone = sp43eg13,
  Video_Recorder = sp43eg14, Camera = sp43eg15, Video_Camera = sp43eg16, Boat = sp43eg17,
  Sailboat = sp43eg18, Computer = sp43eg19, Car = spm40a)
for (n in c(2:21)) {
  appliances_0_1_Norway[n] = ifelse(appliances_0_1_Norway[n] == 2, 0, 1)
}
write_csv(appliances_0_1_Norway, "4_Norway/1_Data_Clean/appliances_0_1_Norway.csv")
  
# tmp<- data%>%
#   select(lopenr,a = cvnr_04111_belop, b = cvnr_04121_mengde, impA= cvnr_04211_belop, impB =cvnr_04221_mengde)%>%
#   #filter(a != b)%>%
#   mutate(c = a/b)%>%
#   view(.)

#extract item codes and their expenditures
expenses1 <- data %>%
  select(lopenr, 222:415)

expenses <- expenses1 %>%
  rename(hh_id = lopenr)%>%
  pivot_longer(-c(hh_id), names_to="item_codes", values_to = "expenditures_year") %>%
  arrange(expenditures_year, item_codes) %>%
  write_csv(., "4_Norway/1_Data_Clean/expenditures_items_Norway.csv")

labels <- scan("./4_Norway/1_Data_Clean/labelsEN.txt", what = "c", sep= "\n")
item_codes <- expenses1%>%
  filter(lopenr == 2)%>%
  pivot_longer(-c(lopenr), names_to="item_code", values_to = "expenditures_year")%>%
  mutate(item_name = labels[222:415]) %>%
  select(item_code, item_name)%>%
  write.xlsx(., "4_Norway/3_Matching_Tables/Item_Codes_Description_Norway.xlsx")

#strange secondary variables for expenses that have the same name with a different ending 
weird_expenses <- data %>%
  select(lopenr, 416:609)

questionnaire <- data%>%
  select(lopenr, 610:1258)
