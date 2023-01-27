if(!require("pacman")) install.packages("pacman")

p_load("countrycode", "haven", "Hmisc", "openxlsx", "rattle", "scales", "tidyverse", "sjlabelled")

options(scipen=999)

# Load data

data_0 <- read_dta("../../HH Surveys Paper/Pakistan_analysis/Data/data_stata_2015-16/sec_00a.dta") # done
data_1 <- read_dta("../../HH Surveys Paper/Pakistan_analysis/Data/data_stata_2015-16/sec_1b.dta")  # done
data_2 <- read_dta("../../HH Surveys Paper/Pakistan_analysis/Data/data_stata_2015-16/sec_2a.dta")  # done
data_3 <- read_dta("../../HH Surveys Paper/Pakistan_analysis/Data/data_stata_2015-16/sec_3a.dta")  # done
data_4 <- read_dta("../../HH Surveys Paper/Pakistan_analysis/Data/data_stata_2015-16/sec_4abcde.dta")

data_5 <- read_dta("../../HH Surveys Paper/Pakistan_analysis/Data/data_stata_2015-16/sec_5a.dta")
# data_6a <- read_dta("../../HH Surveys Paper/Pakistan_analysis/Data/data_stata_2015-16/sec_6a.dta") # Buildings and land used
# data_6b <- read_dta("../../HH Surveys Paper/Pakistan_analysis/Data/data_stata_2015-16/sec_6b.dta") # Financial assets and liabilities
# data_7a <- read_dta("../../HH Surveys Paper/Pakistan_analysis/Data/data_stata_2015-16/sec_7a.dta") # Agriculture
# data_7b <- read_dta("../../HH Surveys Paper/Pakistan_analysis/Data/data_stata_2015-16/sec_7b.dta") # Agriculture

#data_8 <- read_dta("../../HH Surveys Paper/Pakistan_analysis/Data/data_stata_2015-16/sec_8ab.dta")  # Non-Agriculture establishment
#data_9a <- read_dta("../../HH Surveys Paper/Pakistan_analysis/Data/data_stata_2015-16/sec_9a.dta")
#data_9b <- read_dta("../../HH Surveys Paper/Pakistan_analysis/Data/data_stata_2015-16/sec_9b.dta")
#data_9c <- read_dta("../../HH Surveys Paper/Pakistan_analysis/Data/data_stata_2015-16/sec_9c.dta")
#data_9d <- read_dta("../../HH Surveys Paper/Pakistan_analysis/Data/data_stata_2015-16/sec_9d.dta")
#
#data_9e <- read_dta("../../HH Surveys Paper/Pakistan_analysis/Data/data_stata_2015-16/sec_9e.dta")
# data_ict <- read_dta("../../HH Surveys Paper/Pakistan_analysis/Data/data_stata_2015-16/sec_ict.dta")
data_w <- read_dta("../../HH Surveys Paper/Pakistan_analysis/Data/data_stata_2015-16/weight.dta")  # done
data_p <- read_dta("../../HH Surveys Paper/Pakistan_analysis/Data/data_stata_2015-16/plist.dta")   # done
data_r <- read_dta("../../HH Surveys Paper/Pakistan_analysis/Data/data_stata_2015-16/roster.dta")  # done

# Transformation of data 

data_w1 <- data_w %>%
  rename(hh_weights = weights)

data_0.1 <- data_0 %>%
  rename(hh_id = hhcode)%>%
  mutate(urban_01 = ifelse(region == 1,0,1))%>%
  left_join(data_w1)%>%
  select(hh_id, province, urban_01, hh_weights)

data_r1 <- data_r %>%
  rename(hh_id = hhcode, sex_hhh = s1aq04)%>%
  filter(s1aq02 == 1)%>%
  select(hh_id, sex_hhh, idc)

data_r2 <- data_r %>%
  rename(hh_id = hhcode)%>%
  filter(s1aq11 != 2)%>%
  mutate(adults   = ifelse(age > 15,1,0),
         children = ifelse(age < 16,1,0))%>%
  group_by(hh_id)%>%
  summarise(adults   = sum(adults),
            children = sum(children),
            hh_size  = n())%>%
  ungroup()

data_1.1 <- data_1 %>%
  rename(hh_id = hhcode, ind_hhh = s1bq05)%>%
  select(hh_id, idc, ind_hhh)%>%
  left_join(data_r1)%>%
  filter(!is.na(sex_hhh))%>%
  select(hh_id, ind_hhh)

data_2.1 <- data_2 %>%
  rename(hh_id = hhcode, edu_hhh = s2ac05)%>%
  select(hh_id, idc, edu_hhh)%>%
  left_join(data_r1)%>%
  filter(!is.na(sex_hhh))%>%
  mutate(edu_hhh = ifelse(is.na(edu_hhh),21,edu_hhh))%>% # has never visited any school
  select(hh_id, edu_hhh, sex_hhh)
  
data_3.1 <- data_3 %>%
  rename(hh_id = hhcode, water = s3aq06, toilet = s3aq15)%>%
  mutate(electricity.access = ifelse(s3aq05a == 3,0,1))%>%
  select(hh_id, water, toilet, electricity.access)

data_5.1 <- data_5 %>%
  rename(hh_id = hhcode)%>%
  filter(itc %in% c(810, 811, 818, 819))%>%
  group_by(hh_id)%>%
  summarise(inc_gov_monetary = sum(value),
            inc_gov_cash     = 0)%>%
  ungroup()

household_information <- data_0.1 %>%
  left_join(data_3.1)%>%
  left_join(data_2.1)%>%
  left_join(data_r2)%>%
  left_join(data_1.1)%>%
  left_join(data_5.1)%>%
  mutate(inc_gov_monetary = ifelse(is.na(inc_gov_monetary),0,inc_gov_monetary),
         inc_gov_cash     = ifelse(is.na(inc_gov_cash),    0,inc_gov_cash))%>%
  select(hh_id, hh_size, hh_weights, province, urban_01, everything())

write_csv(household_information, "../0_Data/1_Household Data/1_Pakistan/1_Data_Clean/household_information_Pakistan.csv")

# Expenditure data

data_2.2 <- data_2 %>%
  rename(hh_id = hhcode)%>%
  select(hh_id, s2ac9a:s2ac9i)%>%
  pivot_longer(-hh_id, names_to = "item_code", values_to = "expenditures_year")%>%
  filter(!is.na(expenditures_year) & expenditures_year > 0)

data_4.1 <- data_4 %>%
  rename(hh_id = hhcode, item_code = itc)%>%
  mutate_at(vars(v1,v2,v3,v4), list(~ ifelse(is.na(.),0,.)))%>%
  mutate(expenditures_year    = v1,
         expenditures_sp_year = v2 + v3 + v4)%>%
  mutate(factor = ifelse(item_code >= 11201 & item_code <= 12206, 365/14,
                       ifelse(item_code >= 111101 & item_code <= 111140, 365/14, 
                              ifelse(item_code >= 11101 & item_code <= 12104, 12,
                                     ifelse(item_code >= 56101 & item_code <= 56115, 12,
                                            ifelse(item_code >= 121101 & item_code <= 121320, 12, 
                                                   ifelse(item_code >= 22001 & item_code <= 22009, 12,
                                                          ifelse(item_code >= 45101 & item_code <= 45406, 12,
                                                                 ifelse(item_code >= 31101 & item_code <= 32202, 1, 
                                                                        ifelse(item_code >= 41101 & item_code <= 43207, 1,
                                                                               ifelse(item_code == 44101 | item_code == 44201,1,1)))))))))))%>%
  mutate(expenditures_year    = expenditures_year*factor,
         expenditures_sp_year = expenditures_sp_year*factor)%>%
  rename(Coicop_codes = item_code)%>%
  select(hh_id, Coicop_codes, expenditures_year, expenditures_sp_year)

Item.Codes.A <- stack(attr(data_4$itc, 'labels'))%>%
  rename(Coicop_codes = values, item_name = ind)

# write.xlsx(Item.Codes.A, "../0_Data/1_Household Data/1_Pakistan/3_Matching_Tables/Item_Codes_Pakistan_Raw.xlsx")

Item.Codes <- read.xlsx("../0_Data/1_Household Data/1_Pakistan/3_Matching_Tables/Item_Codes_Description_Pakistan.xlsx")

data_4.2 <- data_4.1 %>%
  left_join(select(Item.Codes, Coicop_codes, item_code))%>%
  filter(!is.na(item_code))%>%
  select(-Coicop_codes)%>%
  arrange(hh_id, item_code)

write_csv(data_4.2, "../0_Data/1_Household Data/1_Pakistan/1_Data_Clean/expenditures_items_Pakistan.csv")

# Codes

Province.Code <- stack(attr(data_0$province, 'labels'))%>%
  rename(province = values, Province = ind)%>%
  write_csv(., "../0_Data/1_Household Data/1_Pakistan/2_Codes/Province.Code.csv")

Water.Code <- stack(attr(data_3$s3aq06, 'labels'))%>%
  rename(water = values, Water = ind)%>%
  mutate(WTR = c("Basic", "Basic", "Basic", "Limited", "Limited", "Limited", "Limited", "Basic", "Basic", "Basic", "Limited"))%>%
  write_csv(., "../0_Data/1_Household Data/1_Pakistan/2_Codes/Water.Code.csv")

Toilet.Code <- stack(attr(data_3$s3aq15, 'labels'))%>%
  rename(toilet = values, Toilet = ind)%>%
  mutate(TLT = c("Basic", "Basic", "Basic", "Limited", "Limited", "No Service"))%>% 
  write_csv(., "../0_Data/1_Household Data/1_Pakistan/2_Codes/Toilet.Code.csv")

Education.Code <- stack(attr(data_2$s2ac05, 'labels'))%>%
  rename(edu_hhh = values, Education = ind)%>%
  bind_rows(data.frame(edu_hhh = 21, Education = "No schooling"))%>%
  mutate(ISCED = c(0,1,1,1,1,2,2,2,3,3,3,5,5,6,7,7,7,7,7,8,9,0))%>% 
  write_csv(., "../0_Data/1_Household Data/1_Pakistan/2_Codes/Education.Code.csv")

Industry.Code <- stack(attr(data_1$s1bq05, 'labels'))%>%
  rename(ind_hhh = values, Industry = ind)%>%
  write_csv(., "../0_Data/1_Household Data/1_Pakistan/2_Codes/Industry.Code.csv")
