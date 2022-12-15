if(!require("pacman")) install.packages("pacman")

p_load("countrycode", "haven", "Hmisc", "openxlsx", "rattle", "scales", "tidyverse")

options(scipen=999)

# Loading Data ####

# Confidential Microdata
data_0a <- read_dta("R:/MSA/datasets/Household Microdata/Vietnam/VHLSS 2014/VHLSS2014_Households/Ho1.dta")
data_0b <- read_dta("R:/MSA/datasets/Household Microdata/Vietnam/VHLSS 2014/VHLSS2014_Households/Ho2.dta")
data_0c <- read_dta("R:/MSA/datasets/Household Microdata/Vietnam/VHLSS 2014/VHLSS2014_Households/Ho3.dta")
data_0d <- read_dta("R:/MSA/datasets/Household Microdata/Vietnam/VHLSS 2014/VHLSS2014_Households/Ho4.dta")

data_1a <- read_dta("R:/MSA/datasets/Household Microdata/Vietnam/VHLSS 2014/VHLSS2014_Households/Muc1A.dta")
data_1b <- read_dta("R:/MSA/datasets/Household Microdata/Vietnam/VHLSS 2014/VHLSS2014_Households/Muc1B.dta")
data_1c <- read_dta("R:/MSA/datasets/Household Microdata/Vietnam/VHLSS 2014/VHLSS2014_Households/Muc1C.dta")

data_2a <- read_dta("R:/MSA/datasets/Household Microdata/Vietnam/VHLSS 2014/VHLSS2014_Households/Muc2A.dta")
data_2b <- read_dta("R:/MSA/datasets/Household Microdata/Vietnam/VHLSS 2014/VHLSS2014_Households/Muc2X.dta")

data_3a <- read_dta("R:/MSA/datasets/Household Microdata/Vietnam/VHLSS 2014/VHLSS2014_Households/Muc3A.dta")
data_3b <- read_dta("R:/MSA/datasets/Household Microdata/Vietnam/VHLSS 2014/VHLSS2014_Households/Muc3B.dta")
data_3c <- read_dta("R:/MSA/datasets/Household Microdata/Vietnam/VHLSS 2014/VHLSS2014_Households/Muc3C.dta")

data_4a <- read_dta("R:/MSA/datasets/Household Microdata/Vietnam/VHLSS 2014/VHLSS2014_Households/Muc4a.dta")
data_4b11 <- read_dta("R:/MSA/datasets/Household Microdata/Vietnam/VHLSS 2014/VHLSS2014_Households/Muc4B11.dta")
data_4b12 <- read_dta("R:/MSA/datasets/Household Microdata/Vietnam/VHLSS 2014/VHLSS2014_Households/Muc4B12.dta")
data_4b13 <- read_dta("R:/MSA/datasets/Household Microdata/Vietnam/VHLSS 2014/VHLSS2014_Households/Muc4B13.dta")
data_4b14 <- read_dta("R:/MSA/datasets/Household Microdata/Vietnam/VHLSS 2014/VHLSS2014_Households/Muc4B14.dta")

data_4b15 <- read_dta("R:/MSA/datasets/Household Microdata/Vietnam/VHLSS 2014/VHLSS2014_Households/Muc4B15.dta")
data_4b16 <- read_dta("R:/MSA/datasets/Household Microdata/Vietnam/VHLSS 2014/VHLSS2014_Households/Muc4B16.dta")
data_4b17 <- read_dta("R:/MSA/datasets/Household Microdata/Vietnam/VHLSS 2014/VHLSS2014_Households/Muc4B17.dta")
data_4b21 <- read_dta("R:/MSA/datasets/Household Microdata/Vietnam/VHLSS 2014/VHLSS2014_Households/Muc4B21.dta")
data_4b22 <- read_dta("R:/MSA/datasets/Household Microdata/Vietnam/VHLSS 2014/VHLSS2014_Households/Muc4B22.dta")

data_4b31 <- read_dta("R:/MSA/datasets/Household Microdata/Vietnam/VHLSS 2014/VHLSS2014_Households/Muc4B31.dta")
data_4b32 <- read_dta("R:/MSA/datasets/Household Microdata/Vietnam/VHLSS 2014/VHLSS2014_Households/Muc4B32.dta")
data_4b41 <- read_dta("R:/MSA/datasets/Household Microdata/Vietnam/VHLSS 2014/VHLSS2014_Households/Muc4B41.dta")
data_4b42 <- read_dta("R:/MSA/datasets/Household Microdata/Vietnam/VHLSS 2014/VHLSS2014_Households/Muc4B42A.dta")
data_4b51 <- read_dta("R:/MSA/datasets/Household Microdata/Vietnam/VHLSS 2014/VHLSS2014_Households/Muc4B51.dta")

data_4b52 <- read_dta("R:/MSA/datasets/Household Microdata/Vietnam/VHLSS 2014/VHLSS2014_Households/Muc4B52.dta")
data_4c1 <- read_dta("R:/MSA/datasets/Household Microdata/Vietnam/VHLSS 2014/VHLSS2014_Households/Muc4C1.dta")
data_4c2 <- read_dta("R:/MSA/datasets/Household Microdata/Vietnam/VHLSS 2014/VHLSS2014_Households/Muc4C2.dta")
data_4d <- read_dta("R:/MSA/datasets/Household Microdata/Vietnam/VHLSS 2014/VHLSS2014_Households/Muc4D.dta")

data_5a1 <- read_dta("R:/MSA/datasets/Household Microdata/Vietnam/VHLSS 2014/VHLSS2014_Households/Muc5a1.dta")
data_5a2 <- read_dta("R:/MSA/datasets/Household Microdata/Vietnam/VHLSS 2014/VHLSS2014_Households/Muc5a2.dta")
data_5b1 <- read_dta("R:/MSA/datasets/Household Microdata/Vietnam/VHLSS 2014/VHLSS2014_Households/Muc5b1.dta")
data_5b2 <- read_dta("R:/MSA/datasets/Household Microdata/Vietnam/VHLSS 2014/VHLSS2014_Households/Muc5b2.dta")
data_5b3 <- read_dta("R:/MSA/datasets/Household Microdata/Vietnam/VHLSS 2014/VHLSS2014_Households/Muc5b3.dta")

data_6a <- read_dta("R:/MSA/datasets/Household Microdata/Vietnam/VHLSS 2014/VHLSS2014_Households/Muc6.dta")
data_6b <- read_dta("R:/MSA/datasets/Household Microdata/Vietnam/VHLSS 2014/VHLSS2014_Households/Muc6b.dta")

data_7 <- read_dta("R:/MSA/datasets/Household Microdata/Vietnam/VHLSS 2014/VHLSS2014_Households/Muc7.dta")
data_8 <- read_dta("R:/MSA/datasets/Household Microdata/Vietnam/VHLSS 2014/VHLSS2014_Households/Muc8.dta")
data_9 <- read_dta("R:/MSA/datasets/Household Microdata/Vietnam/VHLSS 2014/VHLSS2014_Households/Muc9.dta")

data_91 <- read_dta("R:/MSA/datasets/Household Microdata/Vietnam/VHLSS 2014/VHLSS2014_Households/Muc82.dta")
data_91 <- read_dta("R:/MSA/datasets/Household Microdata/Vietnam/VHLSS 2014/VHLSS2014_Households/Muc91.dta")
data_92 <- read_dta("R:/MSA/datasets/Household Microdata/Vietnam/VHLSS 2014/VHLSS2014_Households/Muc92.dta")
data_92a <- read_dta("R:/MSA/datasets/Household Microdata/Vietnam/VHLSS 2014/VHLSS2014_Households/Muc92T.dta")
data_93 <- read_dta("R:/MSA/datasets/Household Microdata/Vietnam/VHLSS 2014/VHLSS2014_Households/Muc93.dta")
data_94 <- read_dta("R:/MSA/datasets/Household Microdata/Vietnam/VHLSS 2014/VHLSS2014_Households/Muc94.dta")
data_95 <- read_dta("R:/MSA/datasets/Household Microdata/Vietnam/VHLSS 2014/VHLSS2014_Households/Muc95.dta")
data_96 <- read_dta("R:/MSA/datasets/Household Microdata/Vietnam/VHLSS 2014/VHLSS2014_Households/Muc96.dta")
data_97 <- read_dta("R:/MSA/datasets/Household Microdata/Vietnam/VHLSS 2014/VHLSS2014_Households/Muc97.dta")
data_92004 <- read_dta("R:/MSA/datasets/Household Microdata/Vietnam/VHLSS 2014/VHLSS2014_Households/Muc92004.dta")
data_92014 <- read_dta("R:/MSA/datasets/Household Microdata/Vietnam/VHLSS 2014/VHLSS2014_Households/Muc92014.dta")

data_c_00 <- read_dta("R:/MSA/datasets/Household Microdata/Vietnam/VHLSS 2014/VHLSS2014_Commues/Bia.dta")
data_c_0 <- read_dta("R:/MSA/datasets/Household Microdata/Vietnam/VHLSS 2014/VHLSS2014_Commues/Muc0.dta")
data_c_1 <- read_dta("R:/MSA/datasets/Household Microdata/Vietnam/VHLSS 2014/VHLSS2014_Commues/Muc1.dta")
data_c_2 <- read_dta("R:/MSA/datasets/Household Microdata/Vietnam/VHLSS 2014/VHLSS2014_Commues/Muc2.dta")
data_c_3 <- read_dta("R:/MSA/datasets/Household Microdata/Vietnam/VHLSS 2014/VHLSS2014_Commues/Muc3.dta")
data_c_4a <- read_dta("R:/MSA/datasets/Household Microdata/Vietnam/VHLSS 2014/VHLSS2014_Commues/Muc4A.dta")
data_c_4b <- read_dta("R:/MSA/datasets/Household Microdata/Vietnam/VHLSS 2014/VHLSS2014_Commues/Muc4B.dta")
data_c_5a <- read_dta("R:/MSA/datasets/Household Microdata/Vietnam/VHLSS 2014/VHLSS2014_Commues/Muc5A.dta")
data_c_5b <- read_dta("R:/MSA/datasets/Household Microdata/Vietnam/VHLSS 2014/VHLSS2014_Commues/Muc5B.dta")
data_c_6a <- read_dta("R:/MSA/datasets/Household Microdata/Vietnam/VHLSS 2014/VHLSS2014_Commues/Muc6A.dta")
data_c_6b <- read_dta("R:/MSA/datasets/Household Microdata/Vietnam/VHLSS 2014/VHLSS2014_Commues/Muc6B.dta")
data_c_6c <- read_dta("R:/MSA/datasets/Household Microdata/Vietnam/VHLSS 2014/VHLSS2014_Commues/Muc6C.dta")
data_c_7 <- read_dta("R:/MSA/datasets/Household Microdata/Vietnam/VHLSS 2014/VHLSS2014_Commues/Muc7.dta")
data_c_9a <- read_dta("R:/MSA/datasets/Household Microdata/Vietnam/VHLSS 2014/VHLSS2014_Commues/Muc9A.dta")
data_c_9b <- read_dta("R:/MSA/datasets/Household Microdata/Vietnam/VHLSS 2014/VHLSS2014_Commues/Muc9B.dta")

# Shaping Data ####

muc1a_1 <- muc1a %>%
  left_join(wt2012new, by = c("tinh", "huyen", "xa", "diaban"))%>%
  unite("hh_id", tinh:hoso, sep = "", remove = FALSE)%>%
  select(hh_id, matv, m1ac3, m1ac5, wt9)%>%
  rename(household.head = m1ac3, age = m1ac5, hh_weights = wt9)%>%
  mutate(household.head = ifelse(household.head == "Ch? h?", 1, 0))%>%
  mutate(adults = ifelse(age >= 18, 1, 0))%>%
  mutate(children = ifelse(age < 18, 1, 0))

muc1a_1.1 <- muc1a_1 %>%
  select(hh_id, household.head, matv)%>%
  filter(household.head == 1)%>%
  select(-household.head)

muc1a_1 <- muc1a_1 %>%
  group_by(hh_id)%>%
  summarise(adults = sum(adults),
            children = sum(children),
            hh_size = n(),
            hh_weights = first(hh_weights))%>%
  ungroup()%>%
  mutate(household.head = 1)%>% 
  arrange(hh_id)

muc2_1 <- muc2 %>%
  unite("hh_id", tinh:hoso, sep = "", remove = TRUE)%>%
  select(hh_id, matv, m2c2a)%>% 
  filter(matv == 1)%>%
  rename(education = m2c2a)

muc12 <- left_join(muc1a_1, muc2_1, by = "hh_id")%>%
  left_join(Education.Code, by = "education")%>%
  select(-education, -translation)%>%
  rename(education = number)

ttchung_1 <- ttchung %>%
  unite("hh_id", tinh:hoso, sep = "", remove = TRUE)%>%
  select(hh_id, ttnt, dantoc)%>%
  rename(urban = ttnt, ethnicity = dantoc)

muc12t <- left_join(muc12, ttchung_1, by = "hh_id")

muc12t$hh_weights[is.na(muc12t$hh_weights)] <- min(muc12t$hh_weights, na.rm = TRUE)

# 89.255.719 people included

muc7_1 <- muc7 %>%
  unite("hh_id", tinh:hoso, sep = "", remove = TRUE)%>%
  select(hh_id, m7c18, m7c21, m7c22)%>%
  rename(drinking.water = m7c18, toilet.type = m7c21, lighting.source = m7c22)%>%
  remove_labels()

muc127t <- left_join(muc12t, muc7_1, by = "hh_id")%>%
  remove_labels()%>%
  select(-matv, - household.head)

# write_csv(muc127t, "Household.Information.Vietnam.csv")

muc7_2 <- muc7 %>%
  unite("hh_id", tinh:hoso, sep = "", remove = TRUE)%>%
  select(hh_id, m7c8, m7c14, m7c20, m7c24, m7c26)%>%
  remove_all_labels()

colnames(muc7_2) <- c("hh_id", "8", "14", "20", "24", "26")

muc7_2 <- muc7_2 %>%
  gather(key = "item_code", value = "expenditures", "8":"26", na.rm = TRUE)%>%
  mutate(expenditures_selfproduced = 0)

# Consumption 

muc5a1.1 <- muc5a1%>%
  unite("hh_id", tinh:hoso, sep = "", remove = TRUE)%>%
  rename(item_code = m5a1ma, expenditures = m5a1c2b, expenditures_selfproduced = m5a1c3b)

muc5a1.1$m5a1ma1[is.na(muc5a1.1$m5a1ma1)] <- 0
muc5a1.1 <- muc5a1.1%>%
  mutate(item_code = item_code*10+m5a1ma1)%>%
  select(hh_id, item_code, expenditures, expenditures_selfproduced)%>%
  remove_all_labels()%>%
  mutate(item_code = item_code *10)



muc5a2.1 <- muc5a2 %>%
  unite("hh_id", tinh:hoso, sep = "", remove = TRUE)%>%
  rename(item_code = m5a2ma)

muc5a2.1$m5a2ma1[is.na(muc5a2.1$m5a2ma1)] <- 0

muc5a2.1 <- muc5a2.1 %>%
  mutate(item_code = ifelse((m5a2ma1 == 1 | m5a2ma1 == 2 | m5a2ma1 == 3), (item_code*10+m5a2ma1), item_code))%>%
  select(-m5a2ma1)

muc5a2.1$m5a2c3b[is.na(muc5a2.1$m5a2c3b)] <- 0
muc5a2.1$m5a2c4b[is.na(muc5a2.1$m5a2c4b)] <- 0
muc5a2.1$m5a2c5b[is.na(muc5a2.1$m5a2c5b)] <- 0

test <- muc5a2.1 %>%
  mutate(test = m5a2c3b + m5a2c4b + m5a2c5b)%>%
  mutate(dif = test - m5a2c2b)

# IMPORTANT! We hence should use m5a2c3b and m5a2c4b

muc5a2.1 <- muc5a2.1 %>%
  rename(expenditures = m5a2c3b, expenditures_selfproduced = m5a2c4b)%>%
  select(hh_id, item_code, expenditures, expenditures_selfproduced)%>%
  remove_all_labels()%>%
  mutate(expenditures = expenditures*12)%>%
  mutate(expenditures_selfproduced = expenditures_selfproduced*12)

# Regular Consumption on a monthly basis

muc5b1.1 <- muc5b1 %>%
  unite("hh_id", tinh:hoso, sep = "", remove = TRUE)%>%
  rename(item_code = m5b1ma, expenditures = m5b1c3, expenditures_selfproduced = m5b1c4)%>%
  remove_all_labels()%>%
  mutate(expenditures = expenditures*12)%>%
  mutate(expenditures_selfproduced = expenditures_selfproduced*12)%>%
  select(hh_id, item_code, expenditures, expenditures_selfproduced)

muc5b2.1 <- muc5b2 %>%
  unite("hh_id", tinh:hoso, sep = "", remove = TRUE)%>%
  remove_all_labels()%>%
  rename(item_code = m5b2ma, expenditures = m5b2c2, expenditures_selfproduced = m5b2c3)

muc5b3.1 <- muc5b3 %>%
  unite("hh_id", tinh:hoso, sep = "", remove = TRUE)%>%
  remove_all_labels()
colnames(muc5b3.1) <- c("hh_id", c(seq(400, 408, by = 1)))

muc5b3.1 <- muc5b3.1%>%
  gather(key = "item_code", value = "expenditures", '400':'408')%>%
  mutate(expenditures_selfproduced = 0)

muc5 <- muc5a1.1 %>%
  rbind(muc5a2.1)%>%
  rbind(muc5b1.1)%>%
  rbind(muc5b2.1)%>%
  rbind(muc5b3.1)%>%
  rbind(muc7_2)%>%
  arrange(hh_id, item_code)

# write_csv(muc5, "expenditures_items_Vietnam_long.csv")

muc5_1 <- muc5 %>%
  select(-expenditures_selfproduced)%>%
  spread(key = "item_code", value = "expenditures")

# write_csv(muc5_1, "expenditures_items_Vietnam.wide.csv")

muc5_2 <- muc5 %>%
  select(-expenditures)%>%
  spread(key = "item_code", value = "expenditures_selfproduced")

#write_csv(muc5_2, "expenditures_selfproduced_Vietnam_wide.csv")

s <- as.data.frame(colnames(muc5_1))

write.xlsx(s, "Item_Names_Vietnam_1.xlsx")

# Codierungen ####

Education.Code <- count(muc2_1, education)%>%
  select(-n)%>%
  mutate(number = 1:n())
Ethnicity.Code <- data.frame("ethnicity" = c(seq(1, 56, by = 1)), "Label" = c("kinh", "tay", "thai", "chinese", "khmer", "muong", "nung", "hmong (MEO)", "dao", "jrai", "ngai", "ede", "bana", "sedang", "sanchay (Cao lan - San chi)", "co ho", "Cham ", "san diu", "hre", "mnong", "raglai", "stieng", "bru - Van Kieu", "tho", "giay", "co tu", "gie- trieng", "ma", "kho mu", "co", "ta - oi", "choro", "khang", "singmun", "hanhi", "churu", "lao", "lachi", "laha", "Phula", "lahu", "lu", "lolo", "Chut", "Mang", "pathen", "colao", "cong", "bo y", "si la", "pu peo", "brau", "odu", "romam", "foreigner", "unspecified")) # Attention - comes from coding of 2010 (should be checked if used)

Education.Code$translation <- c("No degree", "primary school", "lower secondary school", "upper secondary school", "college", "university", "master", "PHD", "Other", "NA")

Lighting.Code <- stack(attr(muc7_1$lighting.source, 'labels'))%>%
  rename(lighting.source = values)


Lighting.Code$Label <- c("Electricity", "Battery lamp, resin torch", "Gas, Oil, Kerosene Lamps", "Others (specify)")
Water.Code <- stack(attr(muc7_1$drinking.water, 'labels'))%>%
  rename(drinking.water = values)
Water.Code$Label <- c("Private Tap", "Public Tap Water", "Water pumped from deep drill wells", "Water from hand-dug and reinforced wells", "water from hand-dug and non-reinforced wells, covered", "protected spring sources", "unprotected spring sources", "buying water (bottlers, stems, small vans)", "rain water", "Other (specify)")

Toilet.Code <- stack(attr(muc7_1$toilet.type, 'labels'))%>%
  rename(toilet.type = values)
Toilet.Code$Label <- c("Flush toilet with septic tank/sewage pipes", "suilabh", "double vault compost latrine", "toilet direct over the water", "others", "no toilet")

Urban.Code <- data.frame("urban" = c(1,2), "Urban" = c("Urban", "Rural"))

#setwd()

# write_csv(Urban.Code, "Urban.Code.csv")
# write_csv(Education.Code, "Education.Code.csv")
# write_csv(Ethnicity.Code, "Ethnicity.Code.csv")
# write_csv(Lighting.Code, "Lighting.Code.csv")
# write_csv(Toilet.Code, "Toilet.Code.csv")
# write_csv(Water.Code, "Water.Code.csv")

# Recoding for Distributional Paper ####

#setwd()

hh_viet <- read_csv("household_information_Vietnam.csv")
exp_viet <- read_csv("expenditures_items_Vietnam.csv", col_types = cols(.default = col_double()))

# Expenditures on appliances 
exp_add_viet <- muc6b %>% 
  unite("hh_id", tinh:hoso, sep = "", remove = TRUE)%>%
  mutate(hh_id = as.numeric(hh_id))%>%
  select(hh_id, m6c2, m6c5)%>%
  rename(item_code = m6c2, expenditures = m6c5)%>%
  mutate(item_code = item_code*1000)%>%
  mutate(item_code = ifelse(item_code == 12000, 12001, item_code))%>%
  mutate(item_code = ifelse(item_code == 11000, 11001, item_code))%>%
  mutate(item_code = ifelse(item_code == 14000, 14001, item_code))%>%
  mutate(item_code = ifelse(item_code == 15000, 15001, item_code))%>%
  filter(!is.na(expenditures))%>%
  group_by(hh_id, item_code)%>%
  summarise(expenditures = sum(expenditures))%>%
  ungroup()%>%
  remove_all_labels()


exp_add_viet_2 <- exp_add_viet %>%
  mutate(expenditures_selfproduced = 0)

# this is for the distributional paper
viet <- rbind(muc5, exp_add_viet_2)%>%
  arrange(hh_id, item_code)%>%
  mutate(hh_id = as.numeric(hh_id))

viet_2 <- left_join(hh_viet, viet, by = "hh_id")

write_csv(viet_2, "Vietnam_Household_2020_01_20.csv")

# Appliances

muc6b_1 <- muc6b %>%
  unite("hh_id", tinh:hoso, sep = "", remove = TRUE)%>%
  select(hh_id, m6c2, m6c3)%>%
  rename(item_code = m6c2, number = m6c3)

nrow(muc6b_1)

muc6b_1.1 <- muc6b_1 %>%
  group_by(hh_id, item_code)%>%
  summarise(number = sum(number))%>% # Obviously some items were listed twice per household
  ungroup()

muc6b_1.2 <- muc6b_1.1 %>%
  mutate(posession = ifelse(number > 0, 1, 0))%>%
  select(-number)%>%
  spread(key = "item_code", value = "posession")%>%
  select(hh_id, '1', '2', '3', '7', '8', '9', '12', '13', '15', '16', '18', '20', '22', '23', '24', '25', '26', '27', '28', '33', '34')%>%
  rename(car.01 = '1', motorcycle.01 = '2', bycicle.01 = '3', waterpumps.01 = '7', generator.01 = '8', printer.01 = '9', mobile.01 = '12', sewing.machine.01 = '13', TV.01a = '15', TV.01b = '16', radio.01 = '18', computer.01 = '20', refrigerator.01 = '22', ac.01 = '23', washing_machine.01 = '24', fan.01 = '25', boiler.01 = '26', cas.cookers.01 = '27', electric.cookers.01 = '28', vacuum.01 = '33', microwave.01 = '34')

# write_csv(muc6b_1.2, "Appliances_0_1_Vietnam.csv")

muc6b_1.2a <- muc6b_1.1 %>%
  spread(key = "item_code", value = "number")%>%
  select(hh_id, '1', '2', '3', '7', '8', '9', '12', '13', '15', '16', '18', '20', '22', '23', '24', '25', '26', '27', '28', '33', '34')%>%
  rename(car.01 = '1', motorcycle.01 = '2', bycicle.01 = '3', waterpumps.01 = '7', generator.01 = '8', printer.01 = '9', mobile.01 = '12', sewing.machine.01 = '13', TV.01a = '15', TV.01b = '16', radio.01 = '18', computer.01 = '20', refrigerator.01 = '22', ac.01 = '23', washing_machine.01 = '24', fan.01 = '25', boiler.01 = '26', cas.cookers.01 = '27', electric.cookers.01 = '28', vacuum.01 = '33', microwave.01 = '34')%>%
  rename_at(vars(ends_with("01")), funs(sub("01", "no", .)))


# write_csv(muc6b_1.2a, "Appliances_no_Vietnam.csv")
