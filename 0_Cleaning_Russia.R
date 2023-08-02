# Authors: C. Buder, L. Missbach (missbach@mcc-berlin.net)

if(!require("pacman")) install.packages("pacman")

p_load("countrycode", "haven", "Hmisc", "openxlsx", "rattle", "scales", "tidyverse")

data_1 <- read_dta("../0_Data/1_Household Data/4_Russia/1_Data_Raw/dataverse_files/adult2015x.dta") # former data_russia_adults
data_2 <- read_dta("../0_Data/1_Household Data/4_Russia/1_Data_Raw/dataverse_files/child2015x.dta") # formerly not included
data_3 <- read_dta("../0_Data/1_Household Data/4_Russia/1_Data_Raw/dataverse_files/hh2015x.dta")    # former data_russia_clean

# Transform data ####

data_3.1 <- data_3 %>%
  select(tid, regionx, hhwgt_x, hxnfmemo, hxnfmemn, hxrnum,
         starts_with("hxinhh"), starts_with("hxsex"), starts_with("hxbyr"),
         hxhcwatr, hxhcsewr, hxrsbfu, hxvsbfu, hxrbcl18, hxvbcl18, hxrbcg18, hxvbcl18, hxgetsub)%>%
  rename(province = regionx, hh_id = tid, hh_weights = hhwgt_x,
         water = hxhcwatr, toilet = hxhcsewr)%>%
  mutate_at(vars(hxrsbfu, hxrbcl18, hxrbcg18, hxgetsub), ~ ifelse(is.na(.),0,.))%>%
  mutate(inc_gov_monetary = ifelse(hxrsbfu == 1 | !is.na(hxvsbfu) | hxrbcl18 == 1 | !is.na(hxvbcl18) | hxrbcg18 == 1 | !is.na(hxvbcl18) | hxgetsub == 1,1,0),
         inc_gov_cash     = 0)%>%
  select(-hxrsbfu, -hxvsbfu, -hxrbcl18, -hxvbcl18, -hxrbcg18, -hxvbcl18, -hxgetsub)%>%
  mutate(hh_size = ifelse(!is.na(hxnfmemo), hxnfmemo,hxnfmemn))%>%
  mutate(sex_hhh = ifelse(hxrnum == 1, hxsex01,
                          ifelse(hxrnum == 2, hxsex02,
                                 ifelse(hxrnum == 3, hxsex03, 
                                        ifelse(hxrnum == 4, hxsex04,
                                               ifelse(hxrnum == 5, hxsex05,
                                                      ifelse(hxrnum == 6, hxsex06,
                                                             ifelse(hxrnum == 7, hxsex07,
                                                                    ifelse(hxrnum == 8, hxsex08,
                                                                           ifelse(hxrnum == 9, hxsex09,
                                                                                  ifelse(hxrnum == 10, hxsex10,
                                                                                         ifelse(hxrnum == 11, hxsex11,
                                                                                                ifelse(hxrnum == 12, hxsex12,
                                                                                                       ifelse(hxrnum == 13, hxsex13,
                                                                                                              ifelse(hxrnum == 14, hxsex14,
                                                                                                                     ifelse(hxrnum == 15, hxsex15,
                                                                                                                            ifelse(hxrnum == 16, hxsex16,
                                                                                                                                   ifelse(hxrnum == 17, hxsex17,
                                                                                                                                          NA)))))))))))))))))) %>%
  mutate(birthyear = ifelse(hxrnum == 1, hxbyr01,
                            ifelse(hxrnum == 2, hxbyr02,
                                   ifelse(hxrnum == 3, hxbyr03, 
                                          ifelse(hxrnum == 4, hxbyr04,
                                                 ifelse(hxrnum == 5, hxbyr05,
                                                        ifelse(hxrnum == 6, hxbyr06,
                                                               ifelse(hxrnum == 7, hxbyr07,
                                                                      ifelse(hxrnum == 8, hxbyr08,
                                                                             ifelse(hxrnum == 9, hxbyr09,
                                                                                    ifelse(hxrnum == 10, hxbyr10,
                                                                                           ifelse(hxrnum == 11, hxbyr11,
                                                                                                  ifelse(hxrnum == 12, hxbyr12,
                                                                                                         ifelse(hxrnum == 13, hxbyr13,
                                                                                                                ifelse(hxrnum == 14, hxbyr14,
                                                                                                                       ifelse(hxrnum == 15, hxbyr15,
                                                                                                                              ifelse(hxrnum == 16, hxbyr16,
                                                                                                                                     ifelse(hxrnum == 17, hxbyr17,
                                                                                                                                            NA))))))))))))))))))%>%
  filter(!is.na(sex_hhh))%>%
  mutate(age_hhh = 2015 - birthyear)%>%
  select(-birthyear, - starts_with("hxinhh"), - starts_with("hxsex"), - starts_with("hxbyr"), - hxnfmemo, - hxnfmemn)

data_1.1 <- data_1 %>%
  select(tid_h, personx, ixrelign, ixhiedul, ixpriind, ixampenb)%>%
  rename(hh_id = tid_h, ind_hhh = ixpriind, religion = ixrelign, edu_hhh = ixhiedul, inc_gov_monetary_2 = ixampenb)

data_1.1.1 <- data_1.1 %>%
  mutate(inc_gov_monetary_2 = ifelse(is.na(inc_gov_monetary_2),0,inc_gov_monetary_2))%>%
  group_by(hh_id)%>%
  summarise(inc_gov_monetary_2 = sum(inc_gov_monetary_2))%>%
  ungroup()

data_1.1.2 <- data_1.1 %>%
  select(-inc_gov_monetary_2)%>%
  left_join(mutate(rename(select(data_3.1, hh_id, hxrnum), personx = hxrnum), HHH = "Yes"))%>%
  arrange(HHH)%>%
  distinct(hh_id, .keep_all = TRUE)%>%
  select(-HHH, -personx)

data_1.2 <- data_1 %>%
  select(tid_h)%>%
  rename(hh_id = tid_h)%>%
  group_by(hh_id)%>%
  summarise(adults = n())%>%
  ungroup()

household_information <- left_join(data_3.1, data_1.1.2)%>%
  select(-hxrnum)%>%
  left_join(data_1.1.1)%>%
  mutate(inc_gov_monetary_2 = ifelse(is.na(inc_gov_monetary_2),0, inc_gov_monetary_2))%>%
  mutate(inc_gov_monetary = inc_gov_monetary_2 + inc_gov_monetary)%>%
  select(-inc_gov_monetary_2)%>%
  mutate(edu_hhh = ifelse(is.na(edu_hhh),97,edu_hhh),
         water = ifelse(is.na(water),7,water),
         toilet = ifelse(is.na(toilet),7,toilet),
         religion = ifelse(religion == 99999996 | is.na(religion),96, religion),
         ind_hhh = ifelse(is.na(ind_hhh),33,ind_hhh))%>%
  left_join(data_1.2)%>%
  mutate(children = hh_size - adults)

#write_csv(household_information, "../0_Data/1_Household Data/4_Russia/1_Data_Clean/household_information_Russia.csv")

# Codes Intermezzo

Industry.Code <- distinct(data_1, ixpriind)%>%
  arrange(ixpriind)%>%
  rename(ind_hhh = ixpriind)%>%
  mutate(ind_hhh = ifelse(is.na(ind_hhh),33,ind_hhh))%>%
  mutate(Industry = c("LIGHT INDUSTRY, FOOD INDUSTRY"
                      , "CIVIL MACHINE CONSTRUCTION"
                      , "MILITARY INDUSTRIAL COMPLEX"
                      , "OIL AND GAS INDUSTRY"
                      , "OTHER BRANCH OF HEAVY INDUSTRY"
                      , "CONSTRUCTION" 
                      , "TRANSPORTATION, COMMUNICATION"
                      , "AGRICULTURE" 
                      , "GOVERNMENT AND PUBLIC ADMINISTRATION"
                      , "EDUCATION" 
                      , "SCIENCE, CULTURE" 
                      , "PUBLIC HEALTH" 
                      , "ARMY, MINISTRY OF INTERNAL"
                      , "SECURITY SERVICES" 
                      , "TRADE, CONSUMER SERVICES"
                      , "FINANCES" 
                      , "ENERGY (POWER) INDUSTRY"
                      , "HOUSING AND COMMUNAL SERVICES"
                      , "REAL ESTATE OPERATIONS" 
                      , rep("OTHER",13), "No industry"))%>%
  mutate(Industry = tolower(Industry))%>%
  write_csv(., "../0_Data/1_Household Data/4_Russia/2_Codes/Industry.Code.csv")

Religion.Code <- distinct(household_information, religion)%>%
  arrange(religion)%>%
  mutate(Religion = c("Orthodoxy", "Other", "Other", "Islam", rep("Other",25)))%>%
  write_csv(., "../0_Data/1_Household Data/4_Russia/2_Codes/Religion.Code.csv")

Toilet.Code <- distinct(data_3, hxhcsewr)%>%
  arrange(hxhcsewr)%>%
  rename(toilet = hxhcsewr)%>%
  mutate(toilet = ifelse(is.na(toilet),7,toilet))%>%
  mutate(Toilet = c("Has central sewerage", "Has no central sewerage", "Does not know"))%>%
  mutate(TLT = c("Basic", "Limited", "Unknown"))%>%
  write_csv(., "../0_Data/1_Household Data/4_Russia/2_Codes/Toilet.Code.csv")

Water.Code <- distinct(data_3, hxhcwatr)%>%
  arrange(hxhcwatr)%>%
  rename(water = hxhcwatr)%>%
  mutate(water = ifelse(is.na(water),7,water))%>%
  mutate(Water = c("Has central water supply", "Has no central water supply", "Does not know"))%>%
  mutate(WTR = c("Basic", "Limited", "Unknown"))%>%
  write_csv(., "../0_Data/1_Household Data/4_Russia/2_Codes/Water.Code.csv")

Gender.Code <- distinct(data_3, hxsex01)%>%
  arrange(hxsex01)%>%
  filter(!is.na(hxsex01))%>%
  mutate(Gender = c("Male", "Female"))%>%
  rename(sex_hhh = hxsex01)%>%
  write_csv(., "../0_Data/1_Household Data/4_Russia/2_Codes/Gender.Code.csv")

Education.Code <- distinct(data_1, ixhiedul)%>%
  arrange(ixhiedul)%>%
  rename(edu_hhh = ixhiedul)%>%
  mutate(edu_hhh = ifelse(is.na(edu_hhh),97,edu_hhh))%>%
  mutate(Education = c("General or incomplete secondary school", "Complete secondary school", 
                       "Professional courses of driving, accounting, typing", "Vocational training without secondary education",
                       "Vocational training with secondary education", "Technial community college, medical, music, pedagogical",
                       "Graduate course", "Post-graduate course", "Institute, university, academy, specialist diploma",
                       "Institute, university, academy, bachelor's degree","Institute, university, academy, master's degree",
                       "PhD", "Doctoral", "Does not know"))%>%
  mutate(ISCED = c(1,2,3,4,4,4,4,5,6,6,7,8,8,9))%>%
  write_csv(., "../0_Data/1_Household Data/4_Russia/2_Codes/Education.Code.csv")
  

Province.Code <- distinct(data_3, regionx)%>%
  arrange(regionx)%>%
  rename(province = regionx)%>%
  mutate(Province = c("Leningrad Oblast: Volosovkij Rajon",
                      "Krasnodar CR",
                      "Udmurt ASSR: Glasov CR",
                      "Perm Territory: Solikamsk City & Rajon",
                      "Kaluzhskaya Oblast: Kuibyshev Rajon",
                      "Tambov Oblast: Uvarovo CR",
                      "Volgograd Oblast: Rudnjanskij Rajon",
                      "Tatarskaja ASSR: Kazan",
                      "Kurgan",
                      "Orenburg Oblast: Orsk",
                      "Chuvashskaya ASSR: Shumerlja CR",
                      "Stavropolskij Kraj: Georgievskij CR",
                      "Altaiskij Kraj: Kur`inskij Rajon",
                      "Krasnojarskij Kraij: Krasnojarsk",
                      "Kalinin Oblast: Rzhev CR",
                      "Saratov CR",
                      "Tomsk",
                      "Lipetskaya Oblast: Lipetsk CR",
                      "Krasnojarskij Kraij: Nazarovo CR",
                      "Kabardino-Balkarija, Zol`skij Rajon",
                      "Altaiskij Kraj: Biisk CR",
                      "Komi-ASSR: Usinsk CR (1994-2019)",
                      "Vladivostok",
                      "Amurskaja Oblast: Tambovskii Rajon",
                      "Saratov Oblast: Volskij Gorosovet & Rajon",
                      "Komi-ASSR: Syktyvkar",
                      "Cheliabinsk",
                      "Cheliabinsk Oblast: Krasnoarmeiskij Rajon",
                      "Nizhegorodskaja Oblast: Nizhnij Novgorod",
                      "Penzenskaya Oblast: Zemetchinskij Rajon",
                      "Krasnodarskij Kraj: Kushchevskij Rajon",
                      "Smolensk CR",
                      "Tulskaja Oblast: Tula",
                      "Rostov Oblast: Batajsk",
                      "Moscow City",
                      "Moscow City",
                      "New Moscow City",
                      "St. Petersburg City",
                      "Moscow Oblast",
                      "Novosibirskaya Oblast: Berdsk City & Raion (2003-present)"))%>%
  write_csv(., "../0_Data/1_Household Data/4_Russia/2_Codes/Province.Code.csv")

# Appliances

appliances_0_1 <- data_3 %>%
  select(tid, hxorefnf, hxofrez, hxowshra, hxomicov, hxodishw, hxocoltv, hxoflctv, hxocard, hxocarf, hxotruk, hxomcb, hxotrak, hxolawnm, hxoairco) %>% 
  rename(hh_id = tid, 
         refrigerator.01a   = hxorefnf, 
         refrigerator.01b   = hxofrez, 
         washing_machine.01 = hxowshra, 
         microwave.01       = hxomicov,
         dishwasher.01      = hxodishw, 
         tv.01a             = hxocoltv, 
         tv.01b             = hxoflctv, 
         car.01a            = hxocard, 
         car.01b            = hxocarf, 
         car.01c            = hxotruk, 
         motorcycle.01      = hxomcb, 
         ac.01              = hxoairco) %>%
  mutate_at(vars(-hh_id), list(~ ifelse(. == 1 & !is.na(.),1,0)))%>%
  mutate(refrigerator.01 = ifelse(refrigerator.01a > 0 | refrigerator.01b > 0,1,0),
         tv.01           = ifelse(tv.01a > 0 | tv.01b > 0,1,0),
         car.01          = ifelse(car.01a > 0 | car.01b > 0,1,0))%>%
  select(hh_id, ends_with("01"))

write_csv(appliances_0_1, "../0_Data/1_Household Data/4_Russia/1_Data_Clean/appliances_0_1_Russia.csv")

# Expenditures ####

#create expenditures dataframe

expenditures_items <- data_3 %>%
  select(tid, hxcbrdwi, hxcbrdbl, hxcgroat, hxcflour, hxcpasta, hxcpotat, hxcvegcn, hxccabbg, hxccucum, hxctomat, hxcrtveg, hxcongar, hxcsquas, hxcvegot, hxcmelon, hxcfrucn, 
         hxcberry, hxcfruit, hxcfrudr, hxcnuts, hxcmetcn, hxcbeef, hxcmuttn, hxcpork, hxcorgan, hxcpoult, hxclard, hxcsmeat, hxcmlkcd, hxcmilk, hxckefir, hxccream, hxcbuttr, hxccurds,
         hxcchees, hxcvgo, hxcmargn, hxcsugar, hxccandy, hxcjam, hxchoney, hxccake, hxceggs, hxcfish, hxcfshcn, hxcseafd, hxcconfd, hxcrdml, hxctea, hxccoffe, hxcbev, 
         hxcsltsp, hxcmushr, hxcvodka, hxcliqur, hxcbeer, hxctobac, hxcgum, 
         hxc8out, hxcfoodm, hxccltha, hxcclthc, 
         hxccltgs, hxcmobit, hxcfurn, hxcappl, hxcauto, hxcmcycl, hxcgarag, hxcbuild, hxcdal, hxcbooks, hxcsport, 
         hxcfuelm, hxcfcpk, hxcgasbt, hxctran, hxcshutl, hxchgdsr, hxcbldgr, hxcautor, hxclndyb, hxccommu, hxcmocom, hxcinter, hxccabtv, hxclawye, hxcritul, 
         hxcresdn, hxcchild, hxctour, hxctickt, hxchosp, hxcpolyc, hxcdentr, hxcmedcn, hxcsoap, hxchygie, hxcperfm, hxcaclub, 
         hxcdacha,
         hxcinsur, hxcalimn, hxcparkp, hxcmemf, hxcprtax, hxcgvpar, hxcgvchd, hxcgvgrp, hxcgvgrc, hxcgvoth) %>%
  rename(hh_id = tid) %>%
  mutate(hxcbrdwi = hxcbrdwi / 7 * 365, hxcbrdbl = hxcbrdbl / 7 * 365, hxcgroat = hxcgroat / 7 * 365, hxcflour = hxcflour / 7 * 365, hxcpasta = hxcpasta / 7 * 365, hxcpotat = hxcpotat / 7 * 365, hxcvegcn = hxcvegcn / 7 * 365, hxccabbg = hxccabbg / 7 * 365, hxccucum = hxccucum / 7 * 365, hxctomat = hxctomat / 7 * 365, hxcrtveg = hxcrtveg / 7 * 365, hxcongar = hxcongar / 7 * 365, hxcsquas = hxcsquas / 7 * 365, hxcvegot = hxcvegot / 7 * 365, hxcmelon = hxcmelon / 7 * 365, hxcfrucn = hxcfrucn / 7 * 365, hxcberry = hxcberry / 7 * 365, hxcfruit = hxcfruit / 7 * 365, hxcfrudr = hxcfrudr / 7 * 365, hxcnuts = hxcnuts / 7 * 365, hxcmetcn = hxcmetcn / 7 * 365, hxcbeef = hxcbeef / 7 * 365, hxcmuttn = hxcmuttn / 7 * 365, hxcpork = hxcpork / 7 * 365, hxcorgan = hxcorgan / 7 * 365, hxcpoult = hxcpoult / 7 * 365, hxclard = hxclard / 7 * 365, hxcsmeat = hxcsmeat / 7 * 365, hxcmlkcd = hxcmlkcd / 7 * 365, hxcmilk = hxcmilk / 7 * 365, hxckefir = hxckefir / 7 * 365, hxccream = hxccream / 7 * 365, hxcbuttr = hxcbuttr / 7 * 365, hxccurds = hxccurds / 7 * 365, hxcchees = hxcchees / 7 * 365, hxcvgo = hxcvgo / 7 * 365, hxcmargn = hxcmargn / 7 * 365, hxcsugar = hxcsugar / 7 * 365, hxccandy = hxccandy / 7 * 365, hxcjam = hxcjam / 7 * 365, hxchoney = hxchoney / 7 * 365, hxccake = hxccake / 7 * 365, hxceggs = hxceggs / 7 * 365, hxcfish = hxcfish / 7 * 365, hxcfshcn = hxcfshcn / 7 * 365, hxcseafd = hxcseafd / 7 * 365, hxcconfd = hxcconfd / 7 * 365, hxcrdml = hxcrdml / 7 * 365, hxctea = hxctea / 7 * 365, hxccoffe = hxccoffe / 7 * 365, hxcbev = hxcbev / 7 * 365, hxcsltsp = hxcsltsp / 7 * 365, hxcmushr = hxcmushr / 7 * 365, hxcvodka = hxcvodka / 7 * 365, hxcliqur = hxcliqur / 7 * 365, hxcbeer =  hxcbeer / 7 * 365, hxctobac = hxctobac / 7 * 365, hxcgum = hxcgum / 7 * 365, hxc8out =  hxc8out / 7 * 365, hxcfoodm = hxcfoodm / 7 * 365) %>%
  mutate(hxccltha = hxccltha * 4, hxcclthc = hxcclthc * 4, hxccltgs = hxccltgs * 4, hxcmobit = hxcmobit * 4, hxcfurn = hxcfurn * 4, hxcappl = hxcappl * 4, hxcauto = hxcauto * 4, hxcmcycl = hxcmcycl * 4, hxcgarag = hxcgarag * 4, hxcbuild = hxcbuild * 4, hxcdal = hxcdal * 4, hxcbooks = hxcbooks * 4, hxcsport = hxcsport * 4) %>% 
  mutate(hxcfuelm = hxcfuelm / 30 * 365, hxcfcpk = hxcfcpk / 30 * 365, hxcgasbt = hxcgasbt / 30 * 365, hxctran = hxctran / 30 * 365, hxcshutl = hxcshutl / 30 * 365, hxchgdsr = hxchgdsr / 30 * 365, hxcbldgr = hxcbldgr / 30 * 365, hxcautor = hxcautor / 30 * 365, hxclndyb = hxclndyb / 30 * 365, hxccommu = hxccommu / 30 * 365, hxcmocom = hxcmocom / 30 * 365, hxcinter = hxcinter / 30 * 365, hxccabtv = hxccabtv / 30 * 365, hxclawye = hxclawye / 30 * 365, hxcritul = hxcritul / 30 * 365, hxcresdn = hxcresdn / 30 * 365, hxcchild = hxcchild / 30 * 365, hxctour = hxctour / 30 * 365, hxctickt = hxctickt / 30 * 365, hxchosp = hxchosp / 30 * 365, hxcpolyc = hxcpolyc / 30 * 365, hxcdentr = hxcdentr / 30 * 365, hxcmedcn = hxcmedcn / 30 * 365, hxcsoap = hxcsoap / 30 * 365, hxchygie = hxchygie / 30 * 365, hxcperfm = hxcperfm / 30 * 365, hxcaclub = hxcaclub / 30 * 365, hxcinsur = hxcinsur / 30 * 365, hxcalimn = hxcalimn / 30 * 365, hxcdacha = hxcdacha / 30 * 365, hxcparkp = hxcparkp / 30 * 365, hxcmemf = hxcmemf / 30 * 365, hxcprtax = hxcprtax / 30 * 365, hxcgvpar = hxcgvpar / 30 * 365, hxcgvchd = hxcgvchd / 30 * 365, hxcgvgrp = hxcgvgrp / 30 * 365, hxcgvgrc = hxcgvgrc / 30 * 365  , hxcgvoth = hxcgvoth / 30 * 365)  %>%
  mutate(hxcshoes = sum(hxccltha, hxcclthc)) %>%
  select(-hxccltha, -hxcclthc) %>%
  pivot_longer(-hh_id, names_to = "item_code", values_to = "expenditures_year") %>%
  filter(!is.na(expenditures_year))%>%
  filter(hh_id %in% household_information$hh_id)

expenditures_items_health <- data_1 %>%
  select(tid_h, 
         ixpambhw, ixpamhhw, ixpamgth, # *1
         ixhmpofd, ixhmpuod, ixhmgifd, # 12
         ixexpmam, # *12
         ixpofmea, ixpunmea, ixboumea,
         ixpofhoa, ixpunhoa, ixgifhoa,
         ixpofdea, ixpundea, ixgifdea,
         ixpofcha, ixpuncha, ixgifcha)%>%
  rename(hh_id = tid_h)%>%
  mutate_at(vars(ixhmpofd, ixhmpuod, ixhmgifd, ixexpmam), list(~ .*12))%>%
  mutate_at(vars(ixpofmea, ixpunmea, ixboumea, ixpofhoa, ixpunhoa, ixgifhoa, ixpofdea, ixpundea, ixgifdea, ixpofcha, ixpuncha, ixgifcha), list(~ .*4))%>%
  mutate_at(vars(-hh_id), list(~ ifelse(is.na(.),0,.)))%>%
  pivot_longer(-hh_id, names_to = "item_code", values_to = "expenditures_year")%>%
  group_by(hh_id, item_code)%>%
  summarise(expenditures_year = sum(expenditures_year))%>%
  ungroup()%>%
  filter(expenditures_year > 0)

expenditures_items <- bind_rows(expenditures_items, expenditures_items_health)%>%
  arrange(hh_id, item_code)

# Potentially needs to be updated 

write_csv(expenditures_items, "../0_Data/1_Household Data/4_Russia/1_Data_Clean/expenditures_items_Russia.csv")

household_information <- household_information %>%
  filter(hh_id %in% expenditures_items$hh_id)

write_csv(household_information, "../0_Data/1_Household Data/4_Russia/1_Data_Clean/household_information_Russia.csv")
