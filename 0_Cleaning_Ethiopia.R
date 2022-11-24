# Load Data ####
cover <- read_dta("C:/Users/misl/ownCloud/Fuel_Elasticities_in_LMIC/1_Ethiopia/Data/ETH_2018_ESS_v02_M_Stata/sect_cover_hh_w4.dta")
sect1_hh_w4 <- read_dta("C:/Users/misl/ownCloud/Fuel_Elasticities_in_LMIC/1_Ethiopia/Data/ETH_2018_ESS_v02_M_Stata/sect1_hh_w4.dta")
# sect1_ph_w4 <- read_dta("C:/Users/misl/ownCloud/Fuel_Elasticities_in_LMIC/1_Ethiopia/Data/ETH_2018_ESS_v02_M_Stata/sect1_ph_w4.dta")
# sect1_pp_w4 <- read_dta("C:/Users/misl/ownCloud/Fuel_Elasticities_in_LMIC/1_Ethiopia/Data/ETH_2018_ESS_v02_M_Stata/sect1_pp_w4.dta")

sect2 <- read_dta("C:/Users/misl/ownCloud/Fuel_Elasticities_in_LMIC/1_Ethiopia/Data/ETH_2018_ESS_v02_M_Stata/sect2_hh_w4.dta")
sect4 <- read_dta("C:/Users/misl/ownCloud/Fuel_Elasticities_in_LMIC/1_Ethiopia/Data/ETH_2018_ESS_v02_M_Stata/sect4_hh_w4.dta")

sect6a <- read_dta("C:/Users/misl/ownCloud/Fuel_Elasticities_in_LMIC/1_Ethiopia/Data/ETH_2018_ESS_v02_M_Stata/sect6a_hh_w4.dta")
# sect6b1 <- read_dta("C:/Users/misl/ownCloud/Fuel_Elasticities_in_LMIC/1_Ethiopia/Data/ETH_2018_ESS_v02_M_Stata/sect6b1_hh_w4.dta")
# sect6b2 <- read_dta("C:/Users/misl/ownCloud/Fuel_Elasticities_in_LMIC/1_Ethiopia/Data/ETH_2018_ESS_v02_M_Stata/sect6b2_hh_w4.dta")
# sect6b3 <- read_dta("C:/Users/misl/ownCloud/Fuel_Elasticities_in_LMIC/1_Ethiopia/Data/ETH_2018_ESS_v02_M_Stata/sect6b3_hh_w4.dta")
sect6b4 <- read_dta("C:/Users/misl/ownCloud/Fuel_Elasticities_in_LMIC/1_Ethiopia/Data/ETH_2018_ESS_v02_M_Stata/sect6b4_hh_w4.dta")

sect7a1 <- read_dta("C:/Users/misl/ownCloud/Fuel_Elasticities_in_LMIC/1_Ethiopia/Data/ETH_2018_ESS_v02_M_Stata/sect7a_hh_w4.dta")
sect7a2 <- read_dta("C:/Users/misl/ownCloud/Fuel_Elasticities_in_LMIC/1_Ethiopia/Data/ETH_2018_ESS_v02_M_Stata/sect7b_hh_w4_v2.dta")
sect10a <- read_dta("C:/Users/misl/ownCloud/Fuel_Elasticities_in_LMIC/1_Ethiopia/Data/ETH_2018_ESS_v02_M_Stata/sect10a_hh_w4.dta")


sect11 <- read_dta("C:/Users/misl/ownCloud/Fuel_Elasticities_in_LMIC/1_Ethiopia/Data/ETH_2018_ESS_v02_M_Stata/sect11_hh_w4.dta")
#sec11b1 <- read_dta("C:/Users/misl/ownCloud/Fuel_Elasticities_in_LMIC/1_Ethiopia/Data/ETH_2018_ESS_v02_M_Stata/sect11b1_hh_w4.dta")
#sec11b2 <- read_dta("C:/Users/misl/ownCloud/Fuel_Elasticities_in_LMIC/1_Ethiopia/Data/ETH_2018_ESS_v02_M_Stata/sect11b2_hh_w4.dta")

#com_03 <- read_dta("C:/Users/misl/ownCloud/Fuel_Elasticities_in_LMIC/1_Ethiopia/Data/ETH_2018_ESS_v02_M_Stata/sect03_com_w4.dta")
#com_04 <- read_dta("C:/Users/misl/ownCloud/Fuel_Elasticities_in_LMIC/1_Ethiopia/Data/ETH_2018_ESS_v02_M_Stata/sect04_com_w4.dta")


# Note: Spacial Mapping is possible
cover_0 <- cover %>%
  select(household_id, ea_id, saq14, pw_w4, saq01, saq09)%>%
  mutate(urban_01 = ifelse(saq14 == 1,0,1))%>%
  rename(hh_weights = pw_w4, hh_size = saq09, district = saq01)%>%
  select(-saq14)

sect_1.1 <- sect1_hh_w4 %>%
  select(household_id, individual_id, s1q03a, s1q04)%>%
  rename(age = s1q03a)%>%
  mutate(adults = ifelse(age > 17,1,0),
         children = ifelse(age <18,1,0))%>%
  group_by(household_id)%>%
  summarise(adults = sum(adults),
             children = sum(children))%>%
  ungroup()

sect_1.2 <- sect1_hh_w4 %>%
  filter(s1q01 == 1)%>%
  select(household_id, individual_id, s1q08, s1q02)%>%
  rename(religion = s1q08, sex_hhh = s1q02)

sect_2.1 <- sect2 %>%
  select(household_id, individual_id, s2q06)%>%
  rename(edu_hhh = s2q06)

sect_1.2.1 <- left_join(sect_1.2, sect_2.1)

cover_1 <- left_join(cover_0, sect_1.1)%>%
  left_join(sect_1.2.1)%>%
  remove_all_labels()%>%
  select(-individual_id)%>%
  rename(hh_id = household_id)

housing <- sect10a %>%
  select(household_id, s10aq12, s10aq21, s10aq34, s10aq38, s10aq41)%>%
  rename(telephone = s10aq41, cooking_fuel = s10aq38, lighting_fuel = s10aq34, toilet = s10aq12, water = s10aq21, hh_id = household_id)

cover_2 <- left_join(cover_1, housing)%>%
  remove_all_labels()

write_csv(cover_2, "../0_Data/1_Household Data/2_Ethiopia/1_Data_Clean/household_information_Ethiopia.csv")



# Expenditures

sect_6a <- sect6a %>%
  select(household_id, item_cd, s6aq04)%>%
  rename(hh_id = household_id, item_code = item_cd, expenditures = s6aq04)%>%
  mutate(expenditures_year = expenditures*52)%>%
  select(-expenditures)%>%
  filter(expenditures_year > 0 & !is.na(expenditures_year))

sect_6b4 <- sect6b4 %>%
  select(household_id, meal_id, s6bq07)%>%
  mutate(expenditures_year = s6bq07*52)%>%
  select(-s6bq07)%>%
  filter(expenditures_year > 0 & !is.na(expenditures_year))%>%
  rename(item_code = meal_id, hh_id = household_id)

sect_7a1 <- sect7a1 %>%
  select(household_id, item_cd_30day, s7q02)%>%
  mutate(item_code = item_cd_30day + 1000,
         expenditures_year = s7q02*12)%>%
  select(-item_cd_30day, -s7q02)%>%
  filter(expenditures_year > 0 & !is.na(expenditures_year))%>%
  rename(hh_id = household_id)
  
sect_7a2 <- sect7a2 %>%
  select(household_id, item_cd_12months, s7q04)%>%
  mutate(item_code = item_cd_12months + 10000,
         expenditures_year = s7q04)%>%
  select(-item_cd_12months, -s7q04)%>%
  filter(expenditures_year > 0 & !is.na(expenditures_year))%>%
  rename(hh_id = household_id)

consumption <- bind_rows(sect_6a, sect_6b4)%>%
  bind_rows(sect_7a1)%>%
  bind_rows(sect_7a2)%>%
  arrange(hh_id)

ely <- sect10a %>%
  select(household_id, s10aq35, s10aq31, s10aq05, s10aq42)%>%
  pivot_longer(-household_id, names_to = "item_code", values_to = "expenditures_month")%>%
  mutate(item_code = ifelse(item_code == "s10aq05", 98,
                            ifelse(item_code == "s10aq31", 91,
                                   ifelse(item_code == "s10aq35", 92,93))),
         expenditures_year = ifelse(item_code != 98, expenditures_month*12, expenditures_month))%>%
  rename(hh_id = household_id)%>%
  select(hh_id, item_code, expenditures_year)%>%
  filter(!is.na(expenditures_year) & expenditures_year > 0)

consumption <- bind_rows(sect_6a, sect_6b4)%>%
  bind_rows(sect_7a1)%>%
  bind_rows(sect_7a2)%>%
  bind_rows(ely)%>%
  arrange(hh_id)

write_csv(consumption, "../0_Data/1_Household Data/2_Ethiopia/1_Data_Clean/expenditures_items_Ethiopia.csv")

# Appliances



appliances <- sect11 %>%
  rename(hh_id = household_id)%>%
  select(hh_id, asset_cd, s11q00)%>%
  mutate(Appliance = ifelse(asset_cd == 1, "stove.k.01",
                            ifelse(asset_cd == 2, "stove.g.01",
                                   ifelse(asset_cd == 3, "stove.e.01",
                                          ifelse(asset_cd == 8, "radio.01",
                                                 ifelse(asset_cd == 9, "tv.01",
                                                        ifelse(asset_cd == 14, "motorcycle.01",
                                                               ifelse(asset_cd == 21, "refrigerator",
                                                                      ifelse(asset_cd == 22, "car",
                                                                             ifelse(asset_cd == 23, "other", "other"))))))))))%>%
  filter(Appliance != "other")%>%
  mutate(yn = ifelse(s11q00 == 1,1,0))%>%
  select(hh_id, Appliance, yn)%>%
  pivot_wider(names_from = "Appliance", values_from = "yn", values_fill = 0)

write_csv(appliances, "../0_Data/1_Household Data/2_Ethiopia/1_Data_Clean/appliances_0_1_Ethiopia.csv")



# Codierungen

district.code <- stack(attr(cover_0$district, 'labels'))%>%
  rename(district = values, District = ind)

religion.code <- stack(attr(sect_1.2.1$religion, 'labels'))%>%
  rename(religion = values, Religion = ind)

gender.code <- stack(attr(sect_1.2.1$sex_hhh, 'labels'))%>%
  rename(sex_hhh = values, SEX_HHH = ind)

education.code <- stack(attr(sect_1.2.1$edu_hhh, 'labels'))%>%
  rename(edu_hhh = values, EDU_HHH = ind)

lighting.code <- stack(attr(housing$lighting_fuel, 'labels'))%>%
  rename(lighting_fuel = values, Lighting = ind)

cooking.code <- stack(attr(housing$cooking_fuel, 'labels'))%>%
  rename(cooking_fuel = values, Cooking = ind)

water.code <- stack(attr(housing$water, 'labels'))%>%
  rename(water = values, Water = ind)

toilet.code <- stack(attr(housing$toilet, 'labels'))%>%
  rename(toilet = values, Toilet = ind)

Item.1 <- stack(attr(sect_6a$item_code, 'labels'))%>%
  rename(item_code = values, item_name = ind)
Item.2 <- stack(attr(sect_6b4$item_code, 'labels'))%>%
  rename(item_code = values, item_name = ind)

Item.3 <- stack(attr(sect7a1$item_cd_30day, 'labels'))%>%
  rename(item_code = values, item_name = ind)%>%
  mutate(item_code = item_code + 1000)
Item.4 <- stack(attr(sect7a2$item_cd_12months, 'labels'))%>%
  rename(item_code = values, item_name = ind)%>%
  mutate(item_code = item_code + 10000)

Items <- bind_rows(Item.1, Item.2, Item.3, Item.4)

write.xlsx(Items, "../0_Data/1_Household Data/2_Ethiopia/3_Matching_Tables/Item_Codes_Description_Ethiopia.xlsx")

write_csv(toilet.code ,"../0_Data/1_Household Data/2_Ethiopia/2_Codes/toilet.code.csv")
write_csv(water.code ,"../0_Data/1_Household Data/2_Ethiopia/2_Codes/water.code.csv")
write_csv(lighting.code ,"../0_Data/1_Household Data/2_Ethiopia/2_Codes/lighting.code.csv")
write_csv(cooking.code ,"../0_Data/1_Household Data/2_Ethiopia/2_Codes/cooking.code.csv")
write_csv(gender.code ,"../0_Data/1_Household Data/2_Ethiopia/2_Codes/gender.code.csv")
write_csv(district.code ,"../0_Data/1_Household Data/2_Ethiopia/2_Codes/district.code.csv")
write_csv(education.code ,"../0_Data/1_Household Data/2_Ethiopia/2_Codes/education.code.csv")
write_csv(religion.code ,"../0_Data/1_Household Data/2_Ethiopia/2_Codes/religion.code.csv")

household    <- read_csv("../0_Data/1_Household Data/2_Ethiopia/1_Data_Clean/household_information_Ethiopia.csv")
expenditures <- read_csv("../0_Data/1_Household Data/2_Ethiopia/1_Data_Clean/expenditures_items_Ethiopia.csv")

household <- household %>%
  filter(hh_id %in% expenditures$hh_id)

write_csv(household, "../0_Data/1_Household Data/2_Ethiopia/1_Data_Clean/household_information_Ethiopia.csv")
