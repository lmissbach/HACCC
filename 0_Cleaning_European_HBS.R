# Packages ####

library("cowplot")
library("data.table")
library("dplyr")
library("forcats")
library("foreign")
library("ggthemes")
# library("ggExtra")
library("ggplot2")
library("ggrepel")
library("ggsci")
library("haven")
library("Hmisc")
library("janitor")
library("officer")
library("openxlsx")
library("patchwork") # patchwork requires installation process: install.packages("devtools"), devtools::install_github("thomasp85/patchwork")
library("quantreg")
library("rattle")
library("readr")
library("readxl")
library("reshape2")
library("scales") 
library("stringr")
library("tidyr")
#library("tidyverse")
library("utils")
library("wesanderson")
library("weights")
library("xlsx")
options(scipen=999)

# Load Data ####
# Household Data
BE_h <- read.xlsx("K:/HBS 2010 and 2015 full set.7z/HBS 2010 and 2015 MASTER/HBS2015/BE_MFR_hh.xlsx")
BG_h <- read.xlsx("K:/HBS 2010 and 2015 full set.7z/HBS 2010 and 2015 MASTER/HBS2015/BG_MFR_hh.xlsx")
CY_h <- read.xlsx("K:/HBS 2010 and 2015 full set.7z/HBS 2010 and 2015 MASTER/HBS2015/CY_MFR_hh.xlsx")
CZ_h <- read.xlsx("K:/HBS 2010 and 2015 full set.7z/HBS 2010 and 2015 MASTER/HBS2015/CZ_MFR_hh.xlsx")
DE_h <- read.xlsx("K:/HBS 2010 and 2015 full set.7z/HBS 2010 and 2015 MASTER/HBS2015/DE_MFR_hh.xlsx")
DK_h <- read.xlsx("K:/HBS 2010 and 2015 full set.7z/HBS 2010 and 2015 MASTER/HBS2015/DK_MFR_hh.xlsx")
EE_h <- read.xlsx("K:/HBS 2010 and 2015 full set.7z/HBS 2010 and 2015 MASTER/HBS2015/EE_MFR_hh.xlsx")
EL_h <- read.xlsx("K:/HBS 2010 and 2015 full set.7z/HBS 2010 and 2015 MASTER/HBS2015/EL_MFR_hh.xlsx")
ES_h <- read.xlsx("K:/HBS 2010 and 2015 full set.7z/HBS 2010 and 2015 MASTER/HBS2015/ES_MFR_hh.xlsx")
FI_h <- read.xlsx("K:/HBS 2010 and 2015 full set.7z/HBS 2010 and 2015 MASTER/HBS2015/FI_MFR_hh.xlsx")
FR_h <- read.xlsx("K:/HBS 2010 and 2015 full set.7z/HBS 2010 and 2015 MASTER/HBS2015/FR_MFR_hh.xlsx")
HR_h <- read.xlsx("K:/HBS 2010 and 2015 full set.7z/HBS 2010 and 2015 MASTER/HBS2015/HR_MFR_hh.xlsx")
HU_h <- read.xlsx("K:/HBS 2010 and 2015 full set.7z/HBS 2010 and 2015 MASTER/HBS2015/HU_MFR_hh.xlsx")
IE_h <- read.xlsx("K:/HBS 2010 and 2015 full set.7z/HBS 2010 and 2015 MASTER/HBS2015/IE_MFR_hh.xlsx")
IT_h <- read.xlsx("K:/HBS 2010 and 2015 full set.7z/HBS 2010 and 2015 MASTER/HBS2015/IT_MFR_hh.xlsx")
LT_h <- read.xlsx("K:/HBS 2010 and 2015 full set.7z/HBS 2010 and 2015 MASTER/HBS2015/LT_MFR_hh.xlsx")
LU_h <- read.xlsx("K:/HBS 2010 and 2015 full set.7z/HBS 2010 and 2015 MASTER/HBS2015/LU_MFR_hh.xlsx")
LV_h <- read.xlsx("K:/HBS 2010 and 2015 full set.7z/HBS 2010 and 2015 MASTER/HBS2015/LV_MFR_hh.xlsx")
NL_h <- read.xlsx("K:/HBS 2010 and 2015 full set.7z/HBS 2010 and 2015 MASTER/HBS2015/NL_MFR_hh.xlsx")
PL_h <- read.xlsx("K:/HBS 2010 and 2015 full set.7z/HBS 2010 and 2015 MASTER/HBS2015/PL_MFR_hh.xlsx")
PT_h <- read.xlsx("K:/HBS 2010 and 2015 full set.7z/HBS 2010 and 2015 MASTER/HBS2015/PT_MFR_hh.xlsx")
RO_h <- read.xlsx("K:/HBS 2010 and 2015 full set.7z/HBS 2010 and 2015 MASTER/HBS2015/RO_MFR_hh.xlsx")
SE_h <- read.xlsx("K:/HBS 2010 and 2015 full set.7z/HBS 2010 and 2015 MASTER/HBS2015/SE_MFR_hh.xlsx")
SK_h <- read.xlsx("K:/HBS 2010 and 2015 full set.7z/HBS 2010 and 2015 MASTER/HBS2015/SK_MFR_hh.xlsx")

# Household Member Data
BE_m <- read.xlsx("K:/HBS 2010 and 2015 full set.7z/HBS 2010 and 2015 MASTER/HBS2015/BE_MFR_hm.xlsx")
BG_m <- read.xlsx("K:/HBS 2010 and 2015 full set.7z/HBS 2010 and 2015 MASTER/HBS2015/BG_MFR_hm.xlsx")
CY_m <- read.xlsx("K:/HBS 2010 and 2015 full set.7z/HBS 2010 and 2015 MASTER/HBS2015/CY_MFR_hm.xlsx")
CZ_m <- read.xlsx("K:/HBS 2010 and 2015 full set.7z/HBS 2010 and 2015 MASTER/HBS2015/CZ_MFR_hm.xlsx")
DE_m <- read.xlsx("K:/HBS 2010 and 2015 full set.7z/HBS 2010 and 2015 MASTER/HBS2015/DE_MFR_hm.xlsx")
DK_m <- read.xlsx("K:/HBS 2010 and 2015 full set.7z/HBS 2010 and 2015 MASTER/HBS2015/DK_MFR_hm.xlsx")
EE_m <- read.xlsx("K:/HBS 2010 and 2015 full set.7z/HBS 2010 and 2015 MASTER/HBS2015/EE_MFR_hm.xlsx")
EL_m <- read.xlsx("K:/HBS 2010 and 2015 full set.7z/HBS 2010 and 2015 MASTER/HBS2015/EL_MFR_hm.xlsx")
ES_m <- read.xlsx("K:/HBS 2010 and 2015 full set.7z/HBS 2010 and 2015 MASTER/HBS2015/ES_MFR_hm.xlsx")
FI_m <- read.xlsx("K:/HBS 2010 and 2015 full set.7z/HBS 2010 and 2015 MASTER/HBS2015/FI_MFR_hm.xlsx")
FR_m <- read.xlsx("K:/HBS 2010 and 2015 full set.7z/HBS 2010 and 2015 MASTER/HBS2015/FR_MFR_hm.xlsx")
HR_m <- read.xlsx("K:/HBS 2010 and 2015 full set.7z/HBS 2010 and 2015 MASTER/HBS2015/HR_MFR_hm.xlsx")
HU_m <- read.xlsx("K:/HBS 2010 and 2015 full set.7z/HBS 2010 and 2015 MASTER/HBS2015/HU_MFR_hm.xlsx")
IE_m <- read.xlsx("K:/HBS 2010 and 2015 full set.7z/HBS 2010 and 2015 MASTER/HBS2015/IE_MFR_hm.xlsx")
IT_m <- read.xlsx("K:/HBS 2010 and 2015 full set.7z/HBS 2010 and 2015 MASTER/HBS2015/IT_MFR_hm.xlsx")
LT_m <- read.xlsx("K:/HBS 2010 and 2015 full set.7z/HBS 2010 and 2015 MASTER/HBS2015/LT_MFR_hm.xlsx")
LU_m <- read.xlsx("K:/HBS 2010 and 2015 full set.7z/HBS 2010 and 2015 MASTER/HBS2015/LU_MFR_hm.xlsx")
LV_m <- read.xlsx("K:/HBS 2010 and 2015 full set.7z/HBS 2010 and 2015 MASTER/HBS2015/LV_MFR_hm.xlsx")
NL_m <- read.xlsx("K:/HBS 2010 and 2015 full set.7z/HBS 2010 and 2015 MASTER/HBS2015/NL_MFR_hm.xlsx")
PL_m <- read.xlsx("K:/HBS 2010 and 2015 full set.7z/HBS 2010 and 2015 MASTER/HBS2015/PL_MFR_hm.xlsx")
PT_m <- read.xlsx("K:/HBS 2010 and 2015 full set.7z/HBS 2010 and 2015 MASTER/HBS2015/PT_MFR_hm_.xlsx") # Attention: additional _
RO_m <- read.xlsx("K:/HBS 2010 and 2015 full set.7z/HBS 2010 and 2015 MASTER/HBS2015/RO_MFR_hm.xlsx")
SE_m <- read.xlsx("K:/HBS 2010 and 2015 full set.7z/HBS 2010 and 2015 MASTER/HBS2015/SE_MFR_hm.xlsx")
SK_m <- read.xlsx("K:/HBS 2010 and 2015 full set.7z/HBS 2010 and 2015 MASTER/HBS2015/SK_MFR_hm.xlsx")

# Functions for Data Analysis ####

# Analyse Household Data
analyse_household_data <- function(data_h0, data_m0){
  
data_h1 <- data_h0 %>%
  select(-ends_with("suppressed"), -starts_with("EUR_HE"), - starts_with("EUR_HJ"), - starts_with("HQ"))%>%
  rename(hh_id = HA04, hh_weights = HA10, district = NUTS1, density = HA09,
         income_year = EUR_HH099, hh_size = HB05, hh_type = HB075, ocu_hhh = HC23)%>%
  arrange(hh_id)%>%
  mutate(children = HB051 + HB052 + HB053,
         adults   = HB054 + HB056 + HB057)%>%
  mutate(urban_01 = ifelse(density == 3,0,1))%>% # subject to debate
  select(hh_id, hh_weights, hh_size, COUNTRY, YEAR, children, adults, district, urban_01, income_year, hh_type)
  
data_m1 <- data_m0 %>%
  rename(hh_id = MA04, sex_hhh = MB02, household.head = MB05, edu_hhh = MC01, ocu_hhh = ME01, ind_hhh = ME04)%>%
  filter(household.head == 1)%>%
  select(hh_id, sex_hhh, edu_hhh, ocu_hhh, ind_hhh)

data_hm1 <- left_join(data_h1, data_m1)%>%
  unite(hh_id, c("hh_id", COUNTRY), sep = "_", remove = FALSE)

data_c1 <- data_h0 %>%
  rename(hh_id = HA04, hh_weights = HA10)%>%
  select(hh_id,  starts_with("EUR_HE"), starts_with("EUR_HJ"), COUNTRY)%>%
  rename_at(vars(-hh_id), list(~ str_replace(., "EUR_HE", "")))%>%
  rename_at(vars(-hh_id), list(~ str_replace(., "EUR_HJ", "999")))%>%
  pivot_longer(c(-hh_id,-COUNTRY), names_to = "item_code", values_to = "expenditures_year")%>%
  filter(expenditures_year != 0)%>%
  arrange(hh_id, item_code)%>%
  #mutate(item_code = as.numeric(item_code))%>%
  unite(hh_id, c("hh_id", COUNTRY), sep = "_", remove = FALSE)

list_0 <- list("HH_Data" = data_hm1, "Exp_Data" = data_c1)

return(list_0)

}

BE_h1 <- analyse_household_data(BE_h, BE_m)$HH_Data
BG_h1 <- analyse_household_data(BG_h, BG_m)$HH_Data
CY_h1 <- analyse_household_data(CY_h, CY_m)$HH_Data
CZ_h1 <- analyse_household_data(CZ_h, CZ_m)$HH_Data
DE_h1 <- analyse_household_data(DE_h, DE_m)$HH_Data
DK_h1 <- analyse_household_data(DK_h, DK_m)$HH_Data%>%
  group_by(hh_id)%>%
  mutate(number = 1:n())%>%
  ungroup()%>%
  filter(number == 1)%>%
  select(-number)
EE_h1 <- analyse_household_data(EE_h, EE_m)$HH_Data
EL_h1 <- analyse_household_data(EL_h, EL_m)$HH_Data
ES_h1 <- analyse_household_data(ES_h, ES_m)$HH_Data
FI_h1 <- analyse_household_data(FI_h, FI_m)$HH_Data
FR_h1 <- analyse_household_data(FR_h, FR_m)$HH_Data
HR_h1 <- analyse_household_data(HR_h, HR_m)$HH_Data
HU_h1 <- analyse_household_data(HU_h, HU_m)$HH_Data
IE_h1 <- analyse_household_data(IE_h, IE_m)$HH_Data
IT_h1 <- analyse_household_data(IT_h, IT_m)$HH_Data
LT_h1 <- analyse_household_data(LT_h, LT_m)$HH_Data
LU_h1 <- analyse_household_data(LU_h, LU_m)$HH_Data
LV_h1 <- analyse_household_data(LV_h, LV_m)$HH_Data
NL_h1 <- analyse_household_data(NL_h, NL_m)$HH_Data
PL_h1 <- analyse_household_data(PL_h, PL_m)$HH_Data
PT_h1 <- analyse_household_data(PT_h, PT_m)$HH_Data
RO_h1 <- analyse_household_data(RO_h, RO_m)$HH_Data
SE_h1 <- analyse_household_data(SE_h, SE_m)$HH_Data
SK_h1 <- analyse_household_data(SK_h, SK_m)$HH_Data

BE_e1 <- analyse_household_data(BE_h, BE_m)$Exp_Data
BG_e1 <- analyse_household_data(BG_h, BG_m)$Exp_Data
CY_e1 <- analyse_household_data(CY_h, CY_m)$Exp_Data
CZ_e1 <- analyse_household_data(CZ_h, CZ_m)$Exp_Data
DE_e1 <- analyse_household_data(DE_h, DE_m)$Exp_Data
DK_e1 <- analyse_household_data(DK_h, DK_m)$Exp_Data
EE_e1 <- analyse_household_data(EE_h, EE_m)$Exp_Data
EL_e1 <- analyse_household_data(EL_h, EL_m)$Exp_Data
ES_e1 <- analyse_household_data(ES_h, ES_m)$Exp_Data
FI_e1 <- analyse_household_data(FI_h, FI_m)$Exp_Data
FR_e1 <- analyse_household_data(FR_h, FR_m)$Exp_Data
HR_e1 <- analyse_household_data(HR_h, HR_m)$Exp_Data
HU_e1 <- analyse_household_data(HU_h, HU_m)$Exp_Data
IE_e1 <- analyse_household_data(IE_h, IE_m)$Exp_Data
IT_e1 <- analyse_household_data(IT_h, IT_m)$Exp_Data
LT_e1 <- analyse_household_data(LT_h, LT_m)$Exp_Data
LU_e1 <- analyse_household_data(LU_h, LU_m)$Exp_Data
LV_e1 <- analyse_household_data(LV_h, LV_m)$Exp_Data
NL_e1 <- analyse_household_data(NL_h, NL_m)$Exp_Data
PL_e1 <- analyse_household_data(PL_h, PL_m)$Exp_Data
PT_e1 <- analyse_household_data(PT_h, PT_m)$Exp_Data
RO_e1 <- analyse_household_data(RO_h, RO_m)$Exp_Data
SE_e1 <- analyse_household_data(SE_h, SE_m)$Exp_Data
SK_e1 <- analyse_household_data(SK_h, SK_m)$Exp_Data

rm(BE_h, BG_h, CY_h, CZ_h, DE_h, DK_h, EE_h,
   EL_h, ES_h, FI_h, FR_h, HR_h, HU_h, IE_h,
   IT_h, LT_h, LU_h, LV_h, NL_h, PL_h, PT_h,
   RO_h, SE_h, SK_h, BE_m, BG_m, CY_m, CZ_m, DE_m, DK_m, EE_m,
   EL_m, ES_m, FI_m, FR_m, HR_m, HU_m, IE_m,
   IT_m, LT_m, LU_m, LV_m, NL_m, PL_m, PT_m,
   RO_m, SE_m, SK_m)

# Compress 

EU_h1 <- bind_rows(BE_h1, BG_h1, CY_h1, CZ_h1, DE_h1, DK_h1, EE_h1,
                   EL_h1, ES_h1, FI_h1, FR_h1, HR_h1, HU_h1, IE_h1,
                   IT_h1, LT_h1, LU_h1, LV_h1, NL_h1, PL_h1, PT_h1,
                   RO_h1, SE_h1, SK_h1)

EU_e1 <- bind_rows(BE_e1, BG_e1, CY_e1, CZ_e1, DE_e1, DK_e1, EE_e1,
                   EL_e1, ES_e1, FI_e1, FR_e1, HR_e1, HU_e1, IE_e1,
                   IT_e1, LT_e1, LU_e1, LV_e1, NL_e1, PL_e1, PT_e1,
                   RO_e1, SE_e1, SK_e1)

rm(BE_h1, BG_h1, CY_h1, CZ_h1, DE_h1, DK_h1, EE_h1,
   EL_h1, ES_h1, FI_h1, FR_h1, HR_h1, HU_h1, IE_h1,
   IT_h1, LT_h1, LU_h1, LV_h1, NL_h1, PL_h1, PT_h1,
   RO_h1, SE_h1, SK_h1, BE_e1, BG_e1, CY_e1, CZ_e1, 
   DE_e1, DK_e1, EE_e1, EL_e1, ES_e1, FI_e1, FR_e1, 
   HR_e1, HU_e1, IE_e1, IT_e1, LT_e1, LU_e1, LV_e1, 
   NL_e1, PL_e1, PT_e1, RO_e1, SE_e1, SK_e1)

# Provide new ID

hh_id_new <- distinct(EU_e1, hh_id)%>%
  mutate(hh_id_new = 1:n())

EU_h2 <- EU_h1 %>%
  left_join(hh_id_new)%>%
  select(-YEAR, - hh_id)%>%
  rename(hh_id = hh_id_new)%>%
  select(hh_id, everything())

EU_e2 <- EU_e1 %>%
  left_join(hh_id_new)%>%
  select(hh_id_new, item_code, expenditures_year, - hh_id)%>%
  rename(hh_id = hh_id_new)

# Add artificial item codes for different expenditures at item levels ####

sep_codes <- c("99900", "99901","99902", "99903", "99904", "99905", "99906", "99907", "99908", "99909",
               "99910", "99911","99912", "99990")

EU_e3 <- EU_e2 %>%
  mutate(code_type   = nchar(item_code),
         four_digit  = str_sub(item_code,1,4),
         three_digit = str_sub(item_code,1,3),
         two_digit   = str_sub(item_code,1,2))%>%
  filter(!item_code %in% sep_codes)

# Split expenditure information into item level information 
EU_e3.9 <- EU_e2 %>%
  filter(item_code %in% sep_codes)%>%
  filter(item_code != "99900")
EU_e3.5 <- EU_e3 %>%
  filter(code_type == 5)
EU_e3.4 <- EU_e3 %>%
  filter(code_type == 4)
EU_e3.3 <- EU_e3 %>%
  filter(code_type == 3)
EU_e3.2 <- EU_e3 %>%
  filter(code_type == 2)%>%
  filter(two_digit != "00")%>%
  rename(expenditures_year_2 = expenditures_year)%>%
  select(hh_id, two_digit, expenditures_year_2)
EU_e3.0 <- EU_e3 %>%
  filter(two_digit == "00")

IDs   <- distinct(EU_e3,   hh_id)
two   <- distinct(EU_e3.2, two_digit)
three <- distinct(EU_e3.3, three_digit)
four  <- distinct(EU_e3.4, four_digit)

# Create Dataframe with artificial three-digit item codes

three_digit_new <- expand_grid(hh_id = IDs$hh_id, two_digit = two$two_digit)%>%
  mutate(item_code = str_replace(two_digit, "$", "X"))%>%
  mutate(expenditures_year = 0)%>%
  distinct()

EU_e_2.3 <- bind_rows(EU_e3.3, three_digit_new)%>%
  arrange(hh_id)%>%
  group_by(hh_id, two_digit)%>%
  mutate(sum_two_digit = sum(expenditures_year))%>%
  ungroup()%>%
  full_join(EU_e3.2, by = c("hh_id", "two_digit"))%>%
  mutate(sum_two_digit = ifelse(is.na(sum_two_digit),0, sum_two_digit),
         dif_two_three = expenditures_year_2 - sum_two_digit)%>%
  filter(!is.na(dif_two_three))%>%
  mutate(expenditures_year_3 = ifelse(expenditures_year == 0 & dif_two_three >= 10, dif_two_three, expenditures_year))%>%
  filter(expenditures_year_3 != 0)%>%
  select(hh_id, item_code, expenditures_year_3)%>%
  mutate(three_digit = ifelse(nchar(item_code) == 3, item_code, NA))%>%
  select(-item_code)

rm(EU_e3.3, three_digit_new, EU_e1, EU_e3, EU_h1, hh_id_new)

# Create Dataframe with artificial four-digit item codes

four_digit_new <- expand_grid(hh_id = IDs$hh_id, three_digit = three$three_digit)%>%
  mutate(item_code = str_replace(three_digit, "$", "X"))%>%
  mutate(expenditures_year = 0)%>%
  distinct()

memory.limit(size = 999999)

EU_e_3.4 <- bind_rows(EU_e3.4, four_digit_new)%>%
  arrange(hh_id)%>%
  group_by(hh_id, three_digit)%>%
  mutate(sum_three_digit = sum(expenditures_year))%>%
  ungroup()%>%
  full_join(EU_e_2.3, by = c("hh_id", "three_digit"))%>%
  mutate(sum_three_digit = ifelse(is.na(sum_three_digit),0, sum_three_digit),
         dif_three_four  = expenditures_year_3 - sum_three_digit)%>%
  filter(!is.na(dif_three_four))%>%
  mutate(expenditures_year_4 = ifelse(expenditures_year == 0 & dif_three_four >= 10, dif_three_four, expenditures_year))%>%
  filter(expenditures_year_4 != 0)%>%
  mutate(four_digit = ifelse(nchar(item_code) == 4, item_code, NA))%>%
  select(hh_id, four_digit, expenditures_year_4)

rm(EU_e3.4, four_digit_new)

# Create Dataframe with artificial four-digit item codes

five_digit_new <- expand_grid(hh_id = IDs$hh_id, four_digit = four$four_digit)%>%
  mutate(item_code = str_replace(four_digit, "$", "X"))%>%
  mutate(expenditures_year = 0)%>%
  distinct()

EU_e_4.5 <- bind_rows(EU_e3.5, five_digit_new)%>%
  arrange(hh_id)%>%
  group_by(hh_id, four_digit)%>%
  mutate(sum_four_digit = sum(expenditures_year))%>%
  ungroup()%>%
  full_join(EU_e_3.4, by = c("hh_id", "four_digit"))%>%
  filter(!is.na(sum_four_digit))%>%
  mutate(expenditures_year_4 = ifelse(is.na(expenditures_year_4),0, expenditures_year_4),
         dif_four_five       = expenditures_year_4 - sum_four_digit)%>%
  filter(!is.na(dif_four_five))%>%
  mutate(expenditures_year = ifelse(expenditures_year == 0 & dif_four_five >= 10, dif_four_five, expenditures_year))%>%
  filter(expenditures_year != 0)%>%
  select(hh_id, item_code, expenditures_year)

rm(five_digit_new, EU_e3.5, IDs, two, three, four, sep_codes)

EU_e_3.4.1 <- EU_e_3.4 %>%
  rename(item_code = four_digit,  expenditures_year = expenditures_year_4)
EU_e_2.3.1 <- EU_e_2.3 %>%
  rename(item_code = three_digit, expenditures_year = expenditures_year_3)
EU_e_1.2.1 <- EU_e3.2 %>%
  rename(item_code = two_digit,   expenditures_year = expenditures_year_2)

rm(EU_e_1.2, EU_e_2.3, EU_e_3.4, EU_e3.2)

EU_e_final <- bind_rows(EU_e_1.2.1,
                        EU_e_2.3.1,
                        EU_e_3.4.1,
                        EU_e3.9,
                        EU_e_4.5)%>%
  arrange(hh_id, item_code)%>%
  mutate(code_type = nchar(item_code))%>%
  group_by(hh_id, code_type)%>%
  mutate(code_type_sum = sum(expenditures_year))%>%
  ungroup()%>%
  arrange(hh_id, code_type)%>%
  filter(code_type == 5 | str_detect(item_code, "X"))%>%
  select(-code_type, - code_type_sum)

#write_csv(EU_e_final, "K:/WorkInProgress/2021_Carbon_Footprint_Analysis/Data_Transformed/Expenditure_Data_Clean.csv")
#write_csv(EU_h2,      "K:/WorkInProgress/2021_Carbon_Footprint_Analysis/Data_Transformed/Household_Data_Clean.csv")
rm(EU_e_1.2.1, EU_e_2.3.1, EU_e_3.4.1, EU_e_4.5, EU_e3.9, EU_e3.0)

EU_e_final_add <- EU_e_final %>%
  filter(str_detect(item_code, "X"))%>%
  select(hh_id, item_code, expenditures_year)

# I think it is best to save EU_e_final_add, to load it and to add it to EU_e2 instead of running this process again

# Codes ####
District.Code <- distinct(EU_h1, district)%>%
  arrange(district)%>%
  mutate(District = c())%>%
  write_csv(., "../0_Data/1_Household Data/4_Europe_EU27/2_Codes/District.Code.csv")
Occupation.Code <- distinct(EU_h1, ocu_hhh)%>%
  arrange(ocu_hhh)
Type.Code <- distinct(EU_h1, hh_type)%>%
  arrange(hh_type)%>%
  mutate(HH_TYPE = c("One Person Household", "Lone Parent with children", "Couple without children",
                     "Couple with children", "Couple with children and other people", "Other Type"))
Gender.Code <- distinct(EU_h1, sex_hhh)%>%
  arrange(sex_hhh)%>%
  mutate(Gender = c("Male", "Female", NA))%>%
  write_csv(., "../0_Data/1_Household Data/4_Europe_EU27/2_Codes/Gender.Code.csv")
Education.Code <- distinct(EU_h1, edu_hhh)%>%
  arrange(edu_hhh)%>%
  mutate(Education = c("Early Childhood Education", "Primary Education", "Lower Secondary Education",
                     "Upper Secondary Education", "Post-secondary non-tertiary Education", "Short cycle tertiary",
                     "Bachelor or equivalent", "Master or equivalent", "Not specified", NA))%>%
  write_csv(., "../0_Data/1_Household Data/4_Europe_EU27/2_Codes/Education.Code.csv")
Industry.Code <- distinct(EU_h1, ind_hhh)%>%
  arrange(ind_hhh)%>%
  mutate(Industry = c("Agriculture, Forestry, Fishing", "Mining and Quarrying", "Manufacturing", "Electricity, Gas, Steam and Air Conditioning Supply",
                     "Water Supply, Sewerage, Waste Management and Remediation Activities", "Construction", "Wholesale and Retail Trade; Repair of Motor Vehicles and Motorcycles",
                     "Transportation and Storage", "Accomodation and Food Service Activities", "Information and Communication", "Financial and Insurance Activities",
                     "Real estate activities", "Profession, scientific and technical activities", "Administrative and support service activities",
                     "Public administration and defence, compulsory social security", "Education", "Human health and social work", "Arts, entertainment and recreation",
                     "Other service activities", "Activities of households as employers", "Acitivities of extraterritorial organisations and bodies", "Not specified", NA))%>%
  write_csv(., "../0_Data/1_Household Data/4_Europe_EU27/2_Codes/Industry.Code.csv")

item_codes <- distinct(EU_e1, item_code)%>%
  mutate(item_code = as.character(item_code))%>%
  arrange(item_code)

item_codes_x <- distinct(EU_e_final_add, item_code)%>%
  mutate(item_code = as.character(item_code))%>%
  arrange(item_code)

# write.xlsx(item_codes, "T:/MSA/papers_internal/work_in_progress/Mi_Homogenized_Datainfrastructure/0_Data/1_Household Data/4_Europe_EU27/3_Matching_Tables/Item_Codes_Description_EU_new.xlsx")
# write.xlsx(item_codes_x, "T:/MSA/papers_internal/work_in_progress/Mi_Homogenized_Datainfrastructure/0_Data/1_Household Data/4_Europe_EU27/3_Matching_Tables/Item_Codes_Description_EU_Artificial.xlsx")
# write_csv(district, "T:/MSA/papers_internal/work_in_progress/Mi_Homogenized_Datainfrastructure/0_Data/1_Household Data/4_Europe_EU27/2_Codes/District.Code.csv")
# write_csv(ocu_hhh,  "T:/MSA/papers_internal/work_in_progress/Mi_Homogenized_Datainfrastructure/0_Data/1_Household Data/4_Europe_EU27/2_Codes/Occupation.Code.csv")
# write_csv(ind_hhh,  "T:/MSA/papers_internal/work_in_progress/Mi_Homogenized_Datainfrastructure/0_Data/1_Household Data/4_Europe_EU27/2_Codes/Industry.Code.csv")
# write_csv(edu_hhh,  "T:/MSA/papers_internal/work_in_progress/Mi_Homogenized_Datainfrastructure/0_Data/1_Household Data/4_Europe_EU27/2_Codes/Education.Code.csv")
# write_csv(sex_hhh,  "T:/MSA/papers_internal/work_in_progress/Mi_Homogenized_Datainfrastructure/0_Data/1_Household Data/4_Europe_EU27/2_Codes/Gender.Code.csv")
# write_csv(hh_type,  "T:/MSA/papers_internal/work_in_progress/Mi_Homogenized_Datainfrastructure/0_Data/1_Household Data/4_Europe_EU27/2_Codes/HHType.Code.csv")

rm(edu_hhh, district, hh_type, ind_hhh, ocu_hhh, sex_hhh, item_codes, hh_id_new)
