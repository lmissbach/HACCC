# 1        Packages ####

if(!require("pacman")) install.packages("pacman")

p_load("countrycode", "ggsci", "haven", "Hmisc", "openxlsx", "rattle", "scales", "tidyverse", "sjlabelled")

options(scipen=999)

# 2. Load data ####

expenditures <- read_dta("../0_Data/1_Household Data/4_France/1_Data_Raw/lil-1416.dta/lil-1416.dta/Stata/C05.dta",        encoding = "ISO-8859-1")
menage       <- read_dta("../0_Data/1_Household Data/4_France/1_Data_Raw/lil-1416.dta/lil-1416.dta/Stata/MENAGE.dta",     encoding = "ISO-8859-1")
depmen       <- read_dta("../0_Data/1_Household Data/4_France/1_Data_Raw/lil-1416.dta/lil-1416.dta/Stata/DEPMEN.dta",     encoding = "ISO-8859-1")
automobile   <- read_dta("../0_Data/1_Household Data/4_France/1_Data_Raw/lil-1416.dta/lil-1416.dta/Stata/AUTOMOBILE.dta", encoding = "ISO-8859-1")

# 2.1 Items list ####

labels <- data.frame(item_code = colnames(expenditures), item_name = sapply(expenditures, function(col) Hmisc::label(col)))%>%
  filter(!item_code %in% c("CTOT", "pondmen", "IDENT_MEN"))%>%
  mutate(item_code = str_replace(item_code, "C", ""))

write.xlsx(labels, "../0_Data/1_Household Data/4_France/3_Matching_Tables/Item_Codes_Description_France.xlsx")

# 3. Data transformation ####

# When deleting item codes starting with 13 and 14, it is identical with aggregate consumption expenditures (CTOT).

expenditures_1 <- expenditures %>%
  rename(hh_id = IDENT_MEN)%>%
  select(-pondmen, -CTOT)%>%
  # pivot_longer(-c("hh_id", "CTOT"), names_to = "item_code", values_to = "expenditures_year")%>%
  pivot_longer(-c("hh_id"), names_to = "item_code", values_to = "expenditures_year")%>%
  # group_by(hh_id)%>%
  # mutate(exp_year = sum(expenditures_year))%>%
  # ungroup()%>%
  # mutate(category = str_sub(item_code,2,3))%>%
  # filter(category != 13 & category != 14)%>%
  # group_by(hh_id)%>%
  # mutate(exp_year_alt = sum(expenditures_year))%>%
  # ungroup()%>%
  
  filter(expenditures_year > 0)%>%
  arrange(hh_id, item_code)%>%
  mutate(item_code = str_replace(item_code, "C",""))%>%
  # group_by(hh_id)%>%
  # summarise(expenditures_year = sum(expenditures_year))%>%
  # ungroup()%>%
  # left_join(select(expenditures, IDENT_MEN, CTOT), by = c("hh_id" = "IDENT_MEN"))%>%
  # mutate(test = CTOT - exp_year_alt)
  select(hh_id, item_code, expenditures_year)

  # Household information
  
menage_1 <- menage %>%
  rename(hh_id = IDENT_MEN, age_hhh = AGEPR, edu_hhh = dip14pr, nationality = NATIO7PR, hh_size = NPERS, children = NENFANTS, hh_weights = pondmen, sex_hhh = SEXEPR,
         province = ZEAT, occupation_hhh = CS24PR, urban_type = CATAEU, urban_type_2 = TUU, house_type = TYPLOG)%>%
  select(hh_id, hh_size, children, hh_weights, province,
         age_hhh, edu_hhh, nationality, sex_hhh, occupation_hhh, urban_type, urban_type_2, house_type)
  
depmen_1 <- depmen %>%
  rename(hh_id = IDENT_MEN, refrigerator.01 = DISPEL01, freezer.01 = DISPEL02, washing_machine.01 = DISPEL03, dryer.01 = DISPEL04, tv.01 = DISPAU1,
         dishwasher.01 = DISPEL05, ac.01 = DISPEL10, motorcycle.01 = DISP2R)%>%
  select(hh_id, ends_with(".01"))

depmen_2 <- depmen %>%
  rename(hh_id = IDENT_MEN, construction_year = Ancons, housing_type = Htl, housing_type_2 = imi, wall = NAT_MUR, floor = NAT_SOL, roof = NAT_TOIT,
         heating_fuel = Sourcp, tenant = Stalog, area = Surfhab)%>%
  select(hh_id, housing_type, housing_type_2, construction_year, wall, floor, roof, heating_fuel, tenant, area)

automobile_1 <- automobile %>%
  rename(hh_id = IDENT_MEN, number_of_cars = NBVEHIC)%>%
  select(hh_id, number_of_cars)%>%
  distinct()

household_information <- left_join(menage_1, depmen_2)

appliances <- left_join(depmen_1, automobile_1)%>%
  mutate(number_of_cars = ifelse(is.na(number_of_cars),0,number_of_cars))

write_csv(household_information, "../0_Data/1_Household Data/4_France/1_Data_Clean/household_information_France.csv")
write_csv(appliances,            "../0_Data/1_Household Data/4_France/1_Data_Clean/appliances_0_1_France.csv")
write_csv(expenditures_1,        "../0_Data/1_Household Data/4_France/1_Data_Clean/expenditures_items_France.csv")

rm(menage, menage_1, depmen, depmen_1, depmen_2, automobile, automobile_1, expenditures, expenditures_1)

# Codes ####

Urban.1.Code <- distinct(menage, CATAEU)%>%
  arrange(CATAEU)%>%
  rename(urban_type = CATAEU)%>%
  mutate(Urban_Type = c("Other",
                        "Commune appartenant à un grand pôle (10 000 emplois ou plus)",
                        "Commune appartenant à la couronne d'un grand pôle",
                        "Commune multipolarisée des grandes aires urbaines",
                        "Commune appartenant à un moyen pôle (5 000 à moins de 10 000 emplois)",
                        "Commune appartenant à la couronne d'un moyen pôle",
                        "Commune appartenant à un petit pôle (de 1 500 à moins de 5 000 emplois)",
                        "Commune appartenant à la couronne d'un petit pôle",
                        "Autre commune multipolarisée",
                        "Commune isolée hors influence des pôles"))%>%
  write_csv(., "../0_Data/1_Household Data/4_France/2_Codes/Urban_1_Code.csv")
Occupation.Code <- distinct(menage, CS24PR)%>%
  arrange(CS24PR)%>%
  rename(occupation_hhh = CS24PR)%>%
  mutate(Occupation = c("Other",
                        "Autre cas",
                        "Agriculteurs exploitants",
                        "Artisans",
                        "Commerçants et assimilés",
                        "Chefs d'entreprise de 10 salariés ou plus",
                        "Professions libérales et assimilés",
                        "Cadres de la fonction publique, professions intellectuelles et artistiques",
                        "Cadres d'entreprise",
                        "Professions intermédiaires",
                        "Professions intermédiaires de l'enseignement, de la santé, de la fonction publique et assimilés",
                        "Professions intermédiaires administratives et commerciales des entreprises",
                        "Techniciens",
                        "Contremaîtres, agents de maîtrise",
                        "Inconnu", # Synthetic
                        "Employés de la fonction publique",
                        "Employés administratifs d'entreprise",
                        "Employés de commerce",
                        "Personnels des services directs aux particuliers",
                        "Ouvriers",
                        "Ouvriers qualifiés",
                        "Ouvriers non qualifiés",
                        "Ouvriers agricoles",
                        "Anciens agriculteurs exploitants",
                        "Anciens artisans, commerçants, chefs d'entreprise",
                        "Anciens cadres et professions intermédiaires",
                        "Anciens employés et ouvriers",
                        "Chômeurs n'ayant jamais travaillé",
                        "Inactifs divers (autres que retraités)",
                        "Inconnu"))%>%
  write_csv(., "../0_Data/1_Household Data/4_France/2_Codes/Occupation.Code.csv")
Education.Code   <- distinct(menage, dip14pr)%>%
  arrange(dip14pr)%>%
  rename(edu_hhh = dip14pr)%>%
  mutate(Education = c("Other",
                       "DIPLOME DE 3E CYCLE UNIVERSITAIRE, DOCTORAT",
                       "DIPLOME D'INGENIEUR, D'UNE GRANDE ECOLE",
                       "DIPLOME DE 2E CYCLE UNIVERSITAIRE",
                       "DIPLOME DE 1ER CYCLE UNIVERSITAIRE",
                       "BTS, DUT OU EQUIVALENT",
                       "DIPLOME DES PROFESSIONS SOCIALES ET DE LA SANTE DE NIVEAU BAC+2",
                       "BACCALAUREAT GENERAL, BREVET SUPERIEUR, CAPACITE EN DROIT, DAEU...",
                       "BACCALAUREAT TECHNOLOGIQUE",
                       "BACCALAUREAT PROFESSIONNEL",
                       "BREVET PROFESSIONNEL OU DE TECHNICIEN",
                       "CAP, BEP OU DIPLOME DE MEME NIVEAU",
                       "BREVET DES COLLEGES, BEPC",
                       "CERTIFICAT D'ETUDES PRIMAIRES",
                       "AUCUN DIPLOME"
  ))%>%
  write_csv(., "../0_Data/1_Household Data/4_France/2_Codes/Education.Code.csv")
Nationality.Code <- distinct(menage, NATIO7PR)%>%
  arrange(NATIO7PR)%>%
  rename(nationality = NATIO7PR)%>%
  mutate(Nationality = c("Française de naissance",
                         "Française par naturalisation, mariage, déclaration ou option à sa majorité",
                         "Nationalité de l'Union européenne des 15 (sauf France)",
                         "Nationalité des pays entrés en 2004 dans l'Union européenne",
                         "Algérienne, marocaine ou tunisienne",
                         "Nationalité d'Afrique (sauf Maghreb)",
                         "Autre nationalité ou apatride"))%>%
  write_csv(., "../0_Data/1_Household Data/4_France/2_Codes/Nationality.Code.csv")
Gender.Code      <- distinct(menage, SEXEPR)%>%
  arrange(SEXEPR)%>%
  rename(sex_hhh = SEXEPR)%>%
  mutate(Gender = c("Male", "Female"))%>%
  write_csv(., "../0_Data/1_Household Data/4_France/2_Codes/Gender.Code.csv")
Urban.2.Code <- distinct(menage, TUU)%>%
  arrange(TUU)%>%
  rename(urban_type_2 = TUU)%>%
  mutate(Urban_Type_2 = c("Rural",
                          "Unités urbaines de 2 000 à 4 999 habitants",
                          "Unités urbaines de 5 000 à 9 999 habitants",
                          "Unités urbaines de 10 000 à 19 999 habitants",
                          "Unités urbaines de 20 000 à 49 999 habitants",
                          "Unités urbaines de 50 000 à 99 999 habitants",
                          "Unités urbaines de 100 000 à 199 999 habitants",
                          "Unités urbaines de 200 000 à 1 999 999 habitants",
                          "Agglomération de Paris"))%>%
  write_csv(., "../0_Data/1_Household Data/4_France/2_Codes/Urban_2_Code.csv")
House.Code <- distinct(menage, TYPLOG)%>%
  arrange(TYPLOG)%>%
  rename(house_type = TYPLOG)%>%
  mutate(House_Type = c("Une ferme, un pavillon ou une maison indépendante",
                          "Une maison de ville mitoyenne, jumelée, en bande, ou groupée de toute autre façon",
                          "Un appartement (y compris pièce indépendante) dans un immeuble de deux logements",
                          "Un appartement (y compris pièce indépendante) dans un immeuble de trois à neuf logements",
                          "Un appartement (y compris pièce indépendante) dans un immeuble de 10 logements ou plus",
                          "Une habitation précaire (roulotte, caravane...)",
                          "Un autre type de logement"))%>%
  write_csv(., "../0_Data/1_Household Data/4_France/2_Codes/House.Code.csv")
Province.Code <- distinct(menage, ZEAT)%>%
  arrange(ZEAT)%>%
  rename(province = ZEAT)%>%
  mutate(Province = c("Dom",
                      "Région parisienne",
                      "Bassin parisien",
                      "Nord",
                      "Est",
                      "Ouest",
                      "Sud-ouest",
                      "Centre-est",
                      "Méditerranée"))%>%
  write_csv(., "../0_Data/1_Household Data/4_France/2_Codes/Province.Code.csv")

Construction.Year.Code <- distinct(depmen, Ancons)%>%
  arrange(Ancons)%>%
  rename(construction_year = Ancons)%>%
  mutate(Construction_Year = c("Other",
                               "En 1948 ou avant",
                                "En construction",
                               "De 1949 à 1961",
                               "De 1962 à 1967",
                               "De 1968 à 1974",
                               "De 1975 à 1981",
                               "De 1982 à 1989",
                               "De 1990 à 1998",
                               "De 1999 à 2003",
                               "En 2004 et après",
                                "Refus",
                                "Ne sait pas"))%>%
  write_csv(., "../0_Data/1_Household Data/4_France/2_Codes/Construction.Year.Code.csv")

Housing.Type.1.Code <- distinct(depmen, Htl)%>%
  arrange(Htl)%>%
  rename(housing_type = Htl)%>%
  mutate(Housing_Type = c("Other",
                          "Une case traditionnelle",
                          "Une maison individuelle",
                          "Un logement dans un immeuble collectif",
                          "Une pièce indépendante (ayant sa propre entrée)",
                          "Une ferme, bâtiment d'exploitation agricole",
                          "Une construction provisoire, habitation de fortune",
                          "Un logement dans un immeuble collectif à usage autre que d'habitation (usine, bureaux, commerce, bâtiment public...)"))%>%
  write_csv(., "../0_Data/1_Household Data/4_France/2_Codes/Housing.Type.Code.csv")

Housing.Type.2.Code <- distinct(depmen, imi)%>%
  arrange(imi)%>%
  rename(housing_type_2 = imi)%>%
  mutate(Housing_Type_2 = c("Other", "Case traditionnelle ou maison dans un enclos",
                            "Case traditionnelle ou maison sans clôture",
                            "En bande ou regroupée selon toute autre configuration",
                            "Ne sait pas"))%>%
  write_csv(., "../0_Data/1_Household Data/4_France/2_Codes/Housing.Type.2.Code.csv")

Wall.Code <- distinct(depmen, NAT_MUR)%>%
  arrange(NAT_MUR)%>%
  rename(wall = NAT_MUR)%>%
  mutate(Wall = c("Other", "Végétal autre que raphia (feuilles de cocotier par exemple)",
                  "Terre (torchis) ou raphia",
                  "Tôle",
                  "Semi-dur (terre enduite, chaux)",
                  "Dur (Pierre, brique, parpaing)",
                  "Autre (matériaux de récupération)"))%>%
  write_csv(., "../0_Data/1_Household Data/4_France/2_Codes/Wall.Code.csv")

Floor.Code <- distinct(depmen, NAT_SOL)%>%
  arrange(NAT_SOL)%>%
  rename(floor = NAT_SOL)%>%
  mutate(Floor = c("Other", "Terre battue",
                   "Béton",
                   "Carrelage",
                   "Revêtement plastique (lino...)",
                   "Autres"))%>%
  write_csv(., "../0_Data/1_Household Data/4_France/2_Codes/Floor.Code.csv")

Roof.Code <- distinct(depmen, NAT_TOIT)%>%
  arrange(NAT_TOIT)%>%
  rename(roof = NAT_TOIT)%>%
  mutate(Roof = c("Other",
                  "Végétal (feuilles de cocotier, raphia)",
                   "Tôle",
                   "Béton (maison en cours d'agrandissement)",
                   "Autres"))%>%
  write_csv(., "../0_Data/1_Household Data/4_France/2_Codes/Roof.Code.csv")

Heating.Code <- distinct(depmen, Sourcp)%>%
  arrange(Sourcp)%>%
  rename(heating_fuel = Sourcp)%>%
  mutate(Heating_Fuel = c("Other", 
                          "Electricité",
                          "Autre",
                          "Gaz de ville",
                          "Butane, propane, gaz en citerne",
                          "Fuel, mazout, pétrole",
                          "Charbon, coke",
                          "Bois",
                          "Solaire",
                          "Géothermie",
                          "Aérothermie (pompe à chaleur)",
                          "Ne sait pas"))%>%
  write_csv(., "../0_Data/1_Household Data/4_France/2_Codes/Heating.Code.csv")

Tenant.Code <- distinct(depmen, Stalog)%>%
  arrange(Stalog)%>%
  rename(tenant = Stalog)%>%
  mutate(Tenant = c("Accédant à la propriété : vous avez des remboursements de prêts en cours",
                    "Propriétaire ou copropriétaire (y compris en indivision) : vous n'avez pas de remboursement de prêt sur votre habitation",
                    "Usufruitier, y compris en viager",
                    "Locataire",
                    "Sous-locataire, co-locataire",
                    "Logé gratuitement, avec paiement éventuel de charges"))%>%
  write_csv(., "../0_Data/1_Household Data/4_France/2_Codes/Tenant.Code.csv")

# Check in on matching tables 

item_codes <- read.xlsx("../0_Data/1_Household Data/4_France/3_Matching_Tables/Item_Codes_Description_France.xlsx")
item_gtap  <- read.xlsx("../0_Data/1_Household Data/4_France/3_Matching_Tables/Item_GTAP_Concordance_France.xlsx")

# Want to understand - which item codes are lacking in the existing matching for COICOP codes?

item_gtap_1 <- item_gtap %>%
  select (-Explanation) %>%
  pivot_longer(-GTAP, names_to = "drop", values_to = "item_code")%>%
  filter(!is.na(item_code))%>%
  select(GTAP, item_code)%>%
  mutate(GTAP = ifelse(GTAP == "gas" | GTAP == "gdt", "gasgdt", GTAP))

item_joint <- full_join(item_gtap_1, item_codes)%>%
  arrange(item_code)

item_joint_1 <- filter(item_joint, !is.na(item_name))%>%
  filter(is.na(GTAP))

item_joint_2 <- item_gtap_1 %>%
  full_join(select(item_joint_1, -GTAP))%>%
  arrange(item_code)%>%
  mutate(number_new = ifelse(is.na(item_name) & is.na(lag(item_name)) & is.na(lead(item_name)), NA,1))%>%
  filter(number_new == 1)

write.xlsx(item_joint_2, "../0_Data/1_Household Data/4_France/3_Matching_Tables/Item_Codes_to_match.xlsx")
