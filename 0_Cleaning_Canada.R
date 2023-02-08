if(!require("pacman")) install.packages("pacman")

p_load("countrycode", "haven", "Hmisc", "openxlsx", "rattle", "scales", "tidyverse", "readr")

options(scipen=999)

# Author: L. Missbach

# Load data ####

data_interview <- read_sas("../0_Data/1_Household Data/3_Canada/1_Data_Raw/Data/Data/RY2017/DataModified/SAS/pumf_shs2017_interview.sas7bdat", NULL)

data_diary <- read_sas("../0_Data/1_Household Data/3_Canada/1_Data_Raw/Data/Data/RY2017/DataModified/SAS/pumf_shs2017_diary.sas7bdat", NULL)

# var_names_diary <- read.table("../0_Data/1_Household Data/3_Canada/1_Data_Raw/Data/Data/RY2017/DataModified/TXT/pumf_shs2017_diary_layout2.txt", 
#                               header = FALSE, fill = TRUE, skip = 3)
# data_raw_diary <- read.fwf("../0_Data/1_Household Data/3_Canada/1_Data_Raw/Data/Data/RY2017/DataModified/TXT/pumf_shs2017_diary_flatfile.txt", 
#                            widths = var_names_diary$V3, header = FALSE, col.names = var_names_diary$V2)
# 
# var_names_interview <- read.table("../0_Data/1_Household Data/3_Canada/1_Data_Raw/Data/Data/RY2017/DataModified/TXT/pumf_shs2017_interview_layout.txt", 
#                         header = FALSE, fill = TRUE, skip = 3)
# data_raw_interview <- read.fwf("../0_Data/1_Household Data/3_Canada/1_Data_Raw/Data/Data/RY2017/DataModified/TXT/pumf_shs2017_interview_flatfile.txt", 
#                                widths = var_names_interview$V3, header = FALSE, col.names = var_names_interview$V2)

# Transform data ####

# Household information

household_information <- data_interview %>%
  rename(hh_id = CaseID, hh_weights = WeightC, province = Prov, hh_size = HHSize,
         age_hhh = RP_AgeGrp, sex_hhh = RP_Sex, edu_hhh = RP_Educ)%>%
  mutate(hh_size  = as.numeric(hh_size))%>%
  mutate(children = (ifelse(P0to4YN == 1,1,0)) + ifelse(P5to17YN == 1,1,0))%>%
  mutate(adults   = (ifelse(P18to24YN == 1,1,0)) + (ifelse(P25to64YN == 1,1,0)) + (ifelse(P65to74YN == 1,1,0)) + (ifelse(P75plusYN == 1,1,0)))%>%
  mutate(adults   = ifelse(hh_size - adults - children != 0, hh_size - children, adults))%>%
  mutate(age_hhh = case_when(age_hhh == "01" ~ 29,
                             age_hhh == "02" ~ 39,
                             age_hhh == "03" ~ 54,
                             age_hhh == "04" ~ 64,
                             age_hhh == "05" ~ 74,
                             age_hhh == "06" ~ 75))%>%
  mutate(inc_gov_monetary = RP_GovInc + SP_GovInc + OTH_GovInc,
         inc_gov_cash     = 0)%>%
  select(hh_id, hh_size, adults, children, hh_weights, province, age_hhh, sex_hhh, edu_hhh, inc_gov_cash, inc_gov_monetary)

write_csv(household_information, "../0_Data/1_Household Data/3_Canada/1_Data_Clean/household_information_Canada.csv")

# Appliances

appliances_01 <- data_interview %>%
  rename(hh_id = CaseID, mobile.01 = NumCell, computer.01 = ComputerYN, 
         tv.01a = TVCon_Phone, tv.01b = TVCon_SatDish, tv.01c = TVCon_Cable,
         car.01a = VehicleYN, car.01b = RecVehYN)%>%
  mutate(car.01      = ifelse(car.01a == 1 | car.01b == 1,1,0),
         tv.01       = ifelse(tv.01a == 1  |tv.01b == 1 | tv.01c == 1,1,0),
         mobile.01   = ifelse(mobile.01 == 0,0,1),
         computer.01 = ifelse(computer.01 == 1,1,0))%>%
  select(hh_id, ends_with(".01"))

write_csv(appliances_01, "../0_Data/1_Household Data/3_Canada/1_Data_Clean/appliances_0_1_Canada.csv")

# Codes

Gender.Code <- distinct(household_information, sex_hhh)%>%
  arrange(sex_hhh)%>%
  mutate(Gender = c("Male"," Female"))%>%
  write_csv(., "../0_Data/1_Household Data/3_Canada/2_Codes/Gender.Code.csv")

Education.Code <- distinct(household_information, edu_hhh)%>%
  arrange(edu_hhh)%>%
  mutate(Education = c("Less than high school diploma or its equivalent",
                       "High school diploma, high school equivalency certificate, or not stated",
                       "Certificate of diploma from a trades school, CEGEP or other non-university educational institution",
                       "University certificate or diploma", "Masked records"))%>%
  mutate(ISCED = c(2,3,4,6,9))%>%
  write_csv(., "../0_Data/1_Household Data/3_Canada/2_Codes/Education.Code.csv")

Province.Code <- distinct(household_information, province)%>%
  arrange(province)%>%
  mutate(Province = c("Atlantic provinces", "Quebec", "Ontario", "Manitoba", "Saskatchewan", "Alberta", "British Columbia", "Territoral capitals"))%>%
  write_csv(., "../0_Data/1_Household Data/3_Canada/2_Codes/Province.Code.csv")

rm(Province.Code, Education.Code, Gender.Code, appliances_01)

  #check if all values are same in cropped diary and cropped interview
#select all households in interview file that filled out both diary and interview
cropped_interview <- data_raw_interview %>%
  filter(CaseID %in% data_raw_diary$CaseID) %>%
  select(!WeightC)
#select all variables in diary that are also in interview file
cropped_diary <- data_raw_diary %>%
  select(any_of(colnames(data_raw_interview)))
#make bool dataframe with true where there is a difference, append caseIDs
dif_full <- as.data.frame((!(cropped_diary == cropped_interview))) 
dif_full = cbind(dif_full, cropped_diary$CaseID)
#filter all rows that contain at least one difference (meaning one true value)
dif<-dif_full %>%
  filter(rowSums(dif_full[-c(172)]) >0)
  
  #remove the 7 households that have differing values in diary and interview
household_information_Canada = household_information_Canada %>% filter(!(hh_id %in% dif$`cropped_diary$CaseID`))
data_raw_interview = data_raw_interview %>% filter(!(CaseID %in% dif$`cropped_diary$CaseID`))
data_raw_diary = data_raw_diary %>% filter(!(CaseID %in% dif$`cropped_diary$CaseID`))

  #for inspecting the differences manually run this:
{#dif_diary <- filter(cropped_diary, cropped_diary$CaseID %in% dif$`cropped_diary$CaseID`)
#dif_interview <- filter(cropped_interview, cropped_interview$CaseID %in% dif$`cropped_diary$CaseID`)
#empty_columns <- sapply(dif, function(x) all(x == "FALSE"))
#dif = dif[, !empty_columns]
#dif_diary = dif_diary[, !empty_columns[-c(172)]]
#dif_interview = dif_interview[, !empty_columns[-c(172)]]
}

#merge both datasets into one 
interview_diary <- merge(x = data_raw_interview, y = data_raw_diary, all = TRUE)

  #check if all summed values add up up to a 1$ threshold for mistakes
#totals is a dataframe with all summed variables, their summands and manually calculated sums called "total[summed variable]"
{totals <- interview_diary %>%
  #select all relevant vars
  select(CaseID,
         HH_TotInc, HH_EarnInc, HH_InvInc, HH_GovInc, HH_OthInc, #total hh income
         RP_TotInc, RP_EarnInc, RP_InvInc, RP_GovInc, RP_OthInc, #income of representative (hhh)
         SP_TotInc, SP_EarnInc, SP_InvInc, SP_GovInc, SP_OthInc, #income of spouse
         OTH_TotInc,OTH_EarnInc, OTH_InvInc,OTH_GovInc, OTH_OthInc, #other income
         CC001, CC001_D, CC001_C, #childcare exp
         CL001, CL001_D, CL001_C, CF001, CM001, CI001, CT010, CL010_C, CL007, CL010_D, CL010, #clothes
         CS001, CS001_D, CS001_C, CS003, CS007, CS008, CS010, CS004, CS005, CS011, #exp for communications
         ED002, ED002_D, ED002_C, ED003, ED030_C, ED030_D, ED020, ED010, #education
         FD001, FD003, FD990, FD100, FD200, FD300, FD400, FD500, FD600, FD700, 
         FD800, FD101, FD104, FD107, FD102, FD103, FD105, FD106, FD108, FD112,
         FD201, FD204, FD208, FD202, FD203, FD205, FD206, FD207, FD209, FD212,
         FD301, FD330, FD380, FD302, FD303, FD304, FD305, FD308, FD309, FD315, FD316,
         FD331, FD350, FD381, FD382, FD401, FD440, FD470, FD402, FD403, FD404, 
         FD405, FD406, FD407, FD408, FD409, FD410, FD411, FD412, FD418, FD441, FD442, FD447,
         FD471, FD478, FD479, FD501, FD520, FD540, FD541, FD550, FD570,
         FD502, FD503, FD504, FD505, FD521, FD522, FD525, FD551, FD555, FD571, FD572,
         FD601, FD650, FD602, FD603, FD604, FD607, FD651, FD660, FD701, FD720, FD730,
         FD705, FD706, FD1003, FD721, FD722, FD723, FD724, FD731, FD732, 
         FD801, FD814, FD827, FD833, FD841, FD845, FD850, FD853, FD870, FD883,
         FD802, FD806, FD815, FD821, FD828, FD829, FD1004, 
         FD834, FD835, FD836, FD837, FD838, FD839, FD840, FD421, FD879,
         FD842, FD843, FD844, FD889, FD846, FD847, FD857, FD1001, FD851, FD852,
         FD854, FD855, FD1002, FD871, FD872, FD873, FD874, FD875, FD880, FD881, FD882,
         FD884, FD885, FD991, FD995, FD992, FD993, FD994, #food
         HC001, HC001_D, HC001_C, HC002_C, HC022, HC002_D, HC002, HC023, HC024, HC025,#healthcare
         HE001, HE001_D, HE001_C, HE002_C, HE010_C, HE002_D, HE010_D, HE002, HE020, HE016,#household equipment expenses
         HF001, HF001_D, HF001_C, HF002, HF002_D, HF002_C, HE010,#household furnishing and equipment
         HO001, HO001_D, HO001_C,  HO002, HO003, HO003_C, HO018_C, HO003_D, 
         HO010, HO014, HO018_D, HO022, HO004, HO005, HO018, HO006,#household operations (communications, pets, child care, supplies and equipment)
         ME001, ME001_D, ME001_C, ME002, ME002, ME010, ME010_C, ME010_D, ME010_D, ME010_C,#misc expenses
         PC001, PC001_C, PC001_D, PC020, PC002, #personal care
         RE001, RE001_D, RE001_C, RE002, RE002_C, RE040_C, RE060_C, RV001_C, RE002_D, RE040_D, 
         RE060_D, RV001_D, RE010_C, RE016_C, RE022, RE004, RE005, RE006, RE007, RE008, 
         RE010_D, RE016_D, RE026,RE041_C, RE041_D, RE052,RE061_C, RE070, RE074,RE061_D, 
         RE078, RE067, RE062, RE063, RE066, RE010, RE016, RE040, RE041, RE060, RE061,#recreation etc.
         RO001, RO001_D, RO001_C, RO004, RO002, RO003, RO005, RO006,#reading materials and print
         RV001, RV001_C, RV001_D, RV002, RV010_C, RV010_D, RV010,#recreational vehicles
         SH001, SH002, SH040, SH003, SH010, SH030, SH004, SH990, SH011, SH012, SH991, SH015, 
         SH016, SH019, SH992, SH031, SH032, SH033, SH034, SH041, SH047, SH050, SH042,
         SH044, SH046, SH061, SH062, SH060, #shelter incl. secondary or travel accommodations
         TA001, TA002, TA005, TA006, TA007, TA008,#tobacco and alcohol
         TC001_C, TC001_D, TC001, #total hh cost
         TE001, TE001_C, TE001_D, #total hh expenditure
         TX001, EP001, MG001, #taxes, insurance payments, charity and other gift expenses
         TR001, TR001_C, TR001_D, TR002, TR002_C, TR002_D, TR003, TR004, TR008, TR010, TR020, 
         TR020_C, TR020_D, TR021, TR022, TR030, TR030_C, TR030_D, TR031, TR032, TR033, TR034, 
         TR035, TR036, TR037, TR038, TR039, TR050,#cost of public and private transport
         GC001 #gambling expenses
         ) %>%
  #manually calculate sums
  mutate(
         totalHH_TotInc = HH_EarnInc + HH_InvInc + HH_GovInc + HH_OthInc,
         totalRP_TotInc = RP_EarnInc + RP_InvInc + RP_GovInc + RP_OthInc,
         totalSP_TotInc = SP_EarnInc + SP_InvInc + SP_GovInc + SP_OthInc,
         totalOTH_TotInc = OTH_EarnInc + OTH_InvInc + OTH_GovInc + OTH_OthInc,
         totalCC001 = CC001_C + CC001_D,
         totalCL001 = CL001_C + CL001_D,
         totalCL001_C = CF001 + CM001 + CI001 + CT010 + CL010_C,
         totalCL010 = CL010_C + CL010_D,
         totalCL001_D = CL007 + CL010_D,
         totalCS001 = CS001_D + CS001_C,
         totalCS001_C = CS003 +CS007 +CS008,
         totalCS001_D = CS010,
         totalCS003 = CS004 +CS005+ CS011,
         totalED002 = ED002_C +ED002_D,
         totalED002_C = ED003 + ED030_C,
         totalED002_D = ED030_D,
         totalED030_C = ED020,
         totalED030_D = ED010,
         totalFD001 = FD003+FD990,
         totalFD003 = FD100+FD200+FD300+FD400+FD500+FD600+FD700+FD800,
         totalFD100 = FD101+FD104+FD107,
         totalFD101 = FD102+FD103,
         totalFD104 = FD105+FD106,
         totalFD107 = FD108+FD112,
         totalFD200 = FD201+FD204+FD208,
         totalFD201 = FD202+FD203,
         totalFD204 = FD205+FD206+FD207,
         totalFD208 = FD209+FD212,
         totalFD300 = FD301+FD330+FD380,
         totalFD301 = FD302+ FD303+ FD304+ FD305+ FD308+ FD309+FD315+FD316,
         totalFD330 = FD331+FD350,
         totalFD380 = FD381+FD382,
         totalFD400 = FD401+FD440+FD470,
         totalFD401 = FD402 + FD403 + FD404 + FD405 + FD406 + FD407 + FD408 + FD409 + FD410 + FD411 + FD412 + FD418,
         totalFD440 = FD441 + FD442 + FD447,
         totalFD470 = FD471 + FD478 + FD479,
         totalFD500 = FD501 + FD520 + FD540 + FD541 + FD550 + FD570,
         totalFD501 = FD502 + FD503 + FD504 + FD505,
         totalFD520 = FD521 + FD522 + FD525,
         totalFD550 = FD551 + FD555,
         totalFD570 = FD571 + FD572,
         totalFD600 = FD601 + FD650,
         totalFD601 = FD602 + FD603 + FD604 + FD607,
         totalFD650 = FD651 + FD660,
         totalFD700 = FD701 + FD720 + FD730,
         totalFD701 = FD705 + FD706 + FD1003,
         totalFD720 = FD721 + FD722 + FD723 + FD724,
         totalFD730 = FD731 + FD732,
         totalFD800 = FD801 + FD814 + FD827 + FD833 + FD841 + FD845 + FD850 + FD853 + FD870 + FD883,
         totalFD801 = FD802 + FD806,
         totalFD814 = FD815 + FD821,
         totalFD827 = FD828 + FD829 + FD1004,
         totalFD833 = FD834 + FD835 + FD836 + FD837 + FD838 + FD839 + FD840 + FD421 + FD879,
         totalFD841 = FD842 + FD843 + FD844 + FD889,
         totalFD845 = FD846 + FD847 + FD857 + FD1001,
         totalFD850 = FD851 + FD852,
         totalFD853 = FD854 + FD855 + FD1002,
         totalFD870 = FD871 + FD872 + FD873 + FD874 + FD875 + FD880 + FD881 + FD882,
         totalFD883 = FD884 + FD885,
         totalFD990 = FD991 + FD995,
         totalFD991 = FD992 + FD993 + FD994,
         totalHC001 = HC001_D + HC001_C,
         totalHC001_C = HC002_C + HC022,
         totalHC001_D = HC002_D,
         totalHC002 = HC002_D+HC002_C,
         totalHC022 = HC023+HC024+HC025,
         totalHE001 = HE001_D + HE001_C,
         totalHE001_C = HE002_C + HE010_C,
         totalHE001_D = HE002_D + HE010_D,
         totalHE002 = HE002_D + HE002_C,
         totalHE010 = HE010_D + HE010_C,
         totalHF001 = HF001_D+ HF001_C,
         totalHF001_C = HF002_C + HE001_C + HE020,
         totalHF001_D = HF002_D + HE001_D + HE016,
         totalHF002 = HF002_D + HF002_C,
         totalHO001 = HO001_D+ HO001_C,
         totalHO001_C = CS001_C + CC001_C + HO002 + HO003_C + HO018_C,
         totalHO001_D = CS001_D + CC001_D + HO003_D + HO010 + HO014 + HO018_D + HO022,
         totalHO003 = HO003_D + HO003_C,
         totalHO003_C = HO006,
         totalHO003_D = HO004 + HO005,
         totalHO018 = HO018_D + HO018_C,
         totalME001 = ME001_C + ME001_D,
         totalME001_C = ME002 + ME010_C,
         totalME001_D = ME010_D,
         totalME010 = ME010_D + ME010_C,
         totalPC001 = PC001_D + PC001_C,
         totalPC001_C = PC020,
         totalPC001_D = PC002,
         totalRE001 = RE001_D + RE001_C,
         totalRE001_C = RE002_C + RE040_C + RE060_C + RV001_C,
         totalRE001_D = RE002_D + RE040_D + RE060_D + RV001_D,
         totalRE002 = RE002_C + RE002_D,
         totalRE002_C = RE010_C + RE016_C + RE022,
         totalRE002_D = RE004 + RE005 + RE006 + RE007 + RE008 + RE010_D + RE016_D + RE026,
         totalRE010 = RE010_D + RE010_C,
         totalRE016 = RE016_D + RE016_C,
         totalRE040 = RE040_D + RE040_C,
         totalRE040_C = RE041_C,
         totalRE040_D = RE041_D + RE052,
         totalRE041 = RE041_D + RE041_C,
         totalRE060 = RE060_D + RE060_C,
         totalRE060_C = RE061_C + RE070 + RE074,
         totalRE060_D = RE061_D + RE078,
         totalRE061 = RE061_D + RE061_C,
         totalRE061_C = RE067,
         totalRE061_D = RE062 + RE063 + RE066,
         totalRO001 = RO001_C + RO001_D,
         totalRO001_C = RO004,
         totalRO001_D = RO002 + RO003 + RO005 + RO006,
         totalRV001 = RV001_D + RV001_C,
         totalRV001_C = RV002 + RV010_C,
         totalRV001_D = RV010_D,
         totalRV010 = RV010_D + RV010_C,
         totalSH001 = SH002 + SH040,
         totalSH002 = SH003 + SH010 + SH030,
         totalSH040 = SH041 + SH047 + SH050,
         totalSH003 = SH004 +SH990,
         totalSH010 = SH011 +SH012 +SH991 +SH015 +SH016,
         totalSH030 = SH031 + SH032 + SH033 + SH034,
         totalSH041 = SH042 + SH061 + SH044 + SH062 + SH060 + SH046,
         totalSH016 = SH019 + SH992,
         totalTA001 = TA002 + TA005, 
         totalTA005 = TA006 + TA007 + TA008,
         totalTC001 = TC001_C + TC001_D,
         totalTC001_C = SH001 + HO001_C + HF001_C + CL001_C + TR001_C + HC001_C + PC001_C + RE001_C + RO001_C + ED002_C + ME001_C,
         totalTC001_D = FD001 + HO001_D + HF001_D + CL001_D + TR001_D + HC001_D + PC001_D + RE001_D + RO001_D + ED002_D + TA001 + GC001 + ME001_D,
         totalTE001 = TE001_C + TE001_D,
         totalTE001_C = TX001 + TC001_C + EP001 + MG001,
         totalTE001_D = TC001_D,
         totalTR001 = TR001_C + TR001_D,
         totalTR001_D = TR002_D,
         totalTR001_C = TR002_C + TR050,
         totalTR002 = TR002_C + TR002_D,
         totalTR002_C = TR003 + TR020_C + TR030_C,
         totalTR002_D = TR020_D + TR030_D, 
         totalTR003 = TR004 + TR008 + TR010,
         totalTR020 = TR020_C +TR020_D,
         totalTR020_C = TR021,
         totalTR020_D = TR022,
         totalTR030 = TR030_C +TR030_D,
         totalTR030_C = TR031+TR032+TR033+TR034+TR035+TR038+TR039,
         totalTR030_D = TR036+TR037)
}

#fill var_totals with the names of all summed variables
var_totals <- {c("HH_TotInc", "RP_TotInc", "SP_TotInc", "OTH_TotInc", 
                "CC001",
                "CL001", "CL001_C", "CL010", "CL001_D",
                "CS001", "CS001_C", "CS001_D", "CS003",
                "ED002", "ED002_C", "ED002_D", "ED030_C", "ED030_D",
                "FD001", "FD003", "FD100", "FD101", "FD104", "FD107", "FD200", 
                "FD201", "FD204", "FD208", "FD300", "FD301", "FD330", "FD380",
                "FD400", "FD401", "FD440", "FD470", "FD500", "FD501", "FD520",
                "FD550", "FD570", "FD600", "FD601", "FD650", "FD700", "FD701",
                "FD720", "FD730", "FD800", "FD801", "FD814", "FD827", "FD833",
                "FD841", "FD845", "FD850", "FD853", "FD870", "FD883", "FD990", "FD991",
                "HC001", "HC001_C", "HC001_D", "HC002", "HC022", "HE002", 
                "HE001", "HE001_C", "HE001_D", "HE002", "HE010",
                "HF001", "HF001_C", "HF001_D", "HF002", 
                "HO001", "HO001_C", "HO001_D", "HO003", "HO003_C", "HO003_D", "HO018",
                "ME001", "ME001_C", "ME001_D", "ME010",
                "PC001", "PC001_C", "PC001_D", 
                "RE001", "RE001_C", "RE001_D", "RE002","RE002_C", "RE002_D", 
                "RE010", "RE016", "RE040", "RE040_C", "RE040_D", "RE041", "RE060", 
                "RE060_C", "RE060_D", "RE061", "RE061_C", "RE061_D", 
                "RO001", "RO001_C", "RO001_D",
                "RV001", "RV001_C", "RV001_D", "RV010",
                "SH001", "SH002", "SH003", "SH010", "SH016", "SH030", "SH040", "SH041",
                "TA001", "TA005",
                "TC001", "TC001_C", "TC001_D",
                "TE001", "TE001_C", "TE001_D",
                "TR001", "TR001_D", "TR001_C", "TR002", "TR002_C", "TR002_D", 
                "TR003", "TR020", "TR020_C", "TR020_D", "TR030", "TR030_C"
)}

#check if all sums work, if no output, then all is good
for(i in var_totals){
  #print(i)
  tmp = paste("total",i, sep="")
  if (!(all((-1 < (totals[,tmp] - totals[,i])) && ((totals[,tmp] - totals[,i]) < 1)|| is.na(totals[,tmp] - totals[,i])))){
    print(paste("Problem with sum of ",i))
  }
}

