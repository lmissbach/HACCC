# 0   General ####

# Author: L. Missbach, missbach@mcc-berlin.net

# 0.1 Packages ####

library("boot")
library("cowplot")
library("ggpubr")
library("ggsci")
library("haven")
library("Hmisc")
library("openxlsx")
library("rattle")
library("scales")
library("tidyverse")
options(scipen=999)

# 1. Load data ####

data_m_0 <- read_sav("../0_Data/1_Household Data/2_Morocco/1_Data_Raw/Ménage ENCDM 2014 en format SAV.sav")
data_i_0 <- read_sav("../0_Data/1_Household Data/2_Morocco/1_Data_Raw/Individus ENCDM 2014 en format SAV.sav")

# 2. Transform data ####

data_i_1 <- data_i_0 %>%
  rename(hh_id = N_ménage, province = Région_12, sex_hhh = Sexe, age_hhh = Age, edu_hhh = Niveau_scolaire_agreg,
         ocu_hhh = Profession_agreg, ind_hhh = Secteur_activité_agreg)%>%
  mutate(urban_01 = ifelse(Milieu == 1, 1, 0))%>%
  filter(Liendeparenté == 1)%>%
  select(hh_id, urban_01, province, sex_hhh, age_hhh, edu_hhh, ocu_hhh, ind_hhh)

data_i_2 <- data_i_0 %>%
  rename(hh_id = N_ménage)%>%
  mutate(children = ifelse(Age > 15,0,1),
         adults   = ifelse(Age > 15,1,0))%>%
  group_by(hh_id)%>%
  summarise(hh_size = n(),
            adults = sum(adults),
            children = sum(children))%>%
  ungroup()

data_m_1 <- data_m_0 %>%
  rename(hh_id = N_ménage, hh_weights = coef_ménage)%>%
  select(hh_id, hh_weights)

household_information_0 <- left_join(data_m_1, data_i_2)%>%
  left_join(data_i_1)

write_csv(household_information_0, "../0_Data/1_Household Data/2_Morocco/1_Data_Clean/household_information_Morocco.csv")

data_m_2 <- data_m_0 %>%
  rename(hh_id = N_ménage)%>%
  # DAP present annual expenditures per person --> drop it
  select(hh_id, starts_with("DAM"))%>%
  pivot_longer(starts_with("DAM_"), names_to = "item_code", values_to = "expenditures_year", names_prefix = "DAM_")%>%
  # Test purpose
  # mutate(item_type = nchar(item_code))%>%
  # mutate(item_type = ifelse(item_type > 4,5,item_type))%>%
  # group_by(hh_id, item_type)%>%
  # mutate(agg_exp = sum(expenditures_year))%>%
  # ungroup()%>%
  # mutate(test = agg_exp-DAM)
  filter(expenditures_year > 0)%>%
  select(-DAM)

write_csv(data_m_2, "../0_Data/1_Household Data/2_Morocco/1_Data_Clean/expenditures_items_Morocco.csv")

Labels <- select(data_m_0, starts_with("DAM_"))%>%
  map_dfc(attr, 'label')%>%
  pivot_longer(DAM_G1:DAM_G951, names_to = "item_code", values_to = "item_name", names_prefix = "DAM_")%>%
  mutate(item_name = str_remove(item_name, "Dépense annuelle du ménage du sous groupe"))

write.xlsx(Labels, "../0_Data/1_Household Data/2_Morocco/3_Matching_Tables/Item_Codes_Description_Morocco.xlsx")

# 3. Codes ####

Province.Code <- stack(attr(data_i_1$province, 'labels'))%>%
  rename(Province = ind, province = values)%>%
  write_csv(., "../0_Data/1_Household Data/2_Morocco/2_Codes/Province.Code.csv")

Gender.Code <- stack(attr(data_i_1$sex_hhh, 'labels'))%>%
  rename(sex_hhh = values, Gender = ind)%>%
  write_csv(., "../0_Data/1_Household Data/2_Morocco/2_Codes/Gender.Code.csv")

Occupation.Code <- stack(attr(data_i_1$ocu_hhh, 'labels'))%>%
  rename(ocu_hhh = values, Occupation = ind)%>%
  write_csv(., "../0_Data/1_Household Data/2_Morocco/2_Codes/Occupation.Code.csv")

Education.Code <- stack(attr(data_i_1$edu_hhh, 'labels'))%>%
  rename(edu_hhh = values, Education = ind)%>%
  write_csv(., "../0_Data/1_Household Data/2_Morocco/2_Codes/Education.Code.csv")

Industry.Code <- stack(attr(data_i_1$ind_hhh, 'labels'))%>%
  rename(ind_hhh = values, Industry = ind)%>%
  write_csv(., "../0_Data/1_Household Data/2_Morocco/2_Codes/Industry.Code.csv")
