# 1        Packages ####

if(!require("pacman")) install.packages("pacman")

p_load("countrycode", "ggsci", "haven", "Hmisc", "openxlsx", "rattle", "scales", "tidyverse", "sjlabelled")

options(scipen=999)

# 2. Load data ####

data_0 <- read_dta("../0_Data/1_Household Data/4_Spain/1_Data_Raw/HBS_SPAIN_2019/EPF_2019.dta")

gastos <- read_sav("../0_Data/1_Household Data/4_Spain/1_Data_Raw/datos_2019/EPFgastos_2019/SPSS/EPFgastos_2019.sav")
hogar  <- read_dta("../0_Data/1_Household Data/4_Spain/1_Data_Raw/datos_2019/EPFhogar_2019/STATA/EPFhogar_2019.dta")
mhogar <- read_dta("../0_Data/1_Household Data/4_Spain/1_Data_Raw/datos_2019/EPFmhogar_2019/STATA/EPFmhogar_2019.dta")

# 3. Transform data ####

# 20,817 households

hogar_1 <- hogar %>%
  rename(hh_id = NUMERO, district = CCAA, province = NUTS1, hh_weights = FACTOR, hh_size = NMIEMB, urban_identif = TAMAMU, urban_identif_2 = DENSIDAD, children = NMIEM3, adults = NMIEM4,
         age_hhh = EDADSP, sex_hhh = SEXOSP, nationality = PAISNACSP, edu_hhh = ESTUDIOSSP, occupation_hhh = SITUACTSP, industry_hhh = ACTESTB,
         tenant = REGTEN, housing_type = TIPOCASA, area = SUPERF, water_energy = FUENAGUA, heating_fuel = FUENCALE, house_age = ANNOCON)%>%
  mutate(urban_01 = ifelse(ZONARES %in% c(1,2,3,4),1,0))%>%
  select(hh_id, hh_size, hh_weights, district, province, urban_identif, urban_identif_2, urban_01,
         adults, children, age_hhh, sex_hhh, nationality, edu_hhh, occupation_hhh, industry_hhh,
         tenant, housing_type, area, water_energy, heating_fuel, house_age)%>%
  remove_all_labels()

write_csv(hogar_1, "../0_Data/1_Household Data/4_Spain/1_Data_Clean/household_information_Spain.csv")

gastos_1 <- gastos %>%
  rename(hh_id = NUMERO, item_code = CODIGO, expenditures_year = GASTOMON)%>%
  select(hh_id, item_code, expenditures_year, FACTOR)%>%
  mutate(expenditures_year = expenditures_year/FACTOR)

write_csv(gastos_1, "../0_Data/1_Household Data/4_Spain/1_Data_Clean/expenditures_items_Spain.csv")

# 4. Codes ####

District.Code <- distinct(hogar_1, district)%>% # CCAA
  arrange(district)%>%
  mutate(District = c("Andalucía","Aragón","Asturias, Principado d","Balears, Illes","Canarias","Cantabria","Castilla y León","Castilla – La Mancha",
                      "Cataluña","Comunitat Valenciana","Extremadura","Galicia","Madrid, Comunidad de","Murcia, Región de","Navarra, Comunidad Foral de",
                      "País Vasco","Rioja, La","Ceuta","Melilla"))%>%
  write_csv(., "T:/MSA/papers_internal/work_in_progress/Mi_Homogenized_Datainfrastructure/0_Data/1_Household Data/4_Spain/2_Codes/District.Code.csv")
Province.Code <- distinct(hogar_1, province)%>% # NUTS1
  arrange(province)%>%
  mutate(Province = c("Noroeste", "Noreste", "Comunidad de Madrid", "Central", "Este", "Sur", "Canarias"))%>%
  write_csv(., "T:/MSA/papers_internal/work_in_progress/Mi_Homogenized_Datainfrastructure/0_Data/1_Household Data/4_Spain/2_Codes/Province.Code.csv")
Urban.1.Code <- distinct(hogar_1, urban_identif)%>% # TAMAMU
  arrange(urban_identif)%>%
  mutate(Urban_Identif = c("Municipio de 100.000 habitantes o más",  
                           "Municipio con 50.000 o más y menos 100.000 habitantes", 
                           "Municipio con 20.000 o más y menos de 50.000 habitantes",
                           "Municipio con 10.000 o más y menos de 20.000 habitantes",
                           "Municipio con menos de 10.000 habitantes"))%>%
  write_csv(., "T:/MSA/papers_internal/work_in_progress/Mi_Homogenized_Datainfrastructure/0_Data/1_Household Data/4_Spain/2_Codes/Urban.Code.1.csv")
Urban.2.Code <- distinct(hogar_1, urban_identif_2)%>% # DENSIDAD
  arrange(urban_identif_2)%>%
  mutate(Urban_Identif_2 = c("Zona densamente poblada", "Zona intermedia", "Zona diseminada"))%>%
  write_csv(., "T:/MSA/papers_internal/work_in_progress/Mi_Homogenized_Datainfrastructure/0_Data/1_Household Data/4_Spain/2_Codes/Urban.Code.2.csv")
Gender.Code <- distinct(hogar_1, sex_hhh)%>% # SEXOSP
  arrange(sex_hhh)%>%
  mutate(Gender = c("Hombre", "Mujer"))%>%
  write_csv(., "T:/MSA/papers_internal/work_in_progress/Mi_Homogenized_Datainfrastructure/0_Data/1_Household Data/4_Spain/2_Codes/Gender.Code.csv")
Nationality.Code <- distinct(hogar_1, nationality)%>% # PAISNACSP
  arrange(nationality)%>%
  mutate(Nationality = c("España", "Resto de la Unión Europea (27 países)", "Resto de Europa", "Resto del mundo"))%>%
  write_csv(., "T:/MSA/papers_internal/work_in_progress/Mi_Homogenized_Datainfrastructure/0_Data/1_Household Data/4_Spain/2_Codes/Nationality.Code.csv")
Education.Code <- distinct(hogar_1, edu_hhh)%>% # ESTUDIOSSP
  arrange(edu_hhh)%>%
  mutate(Education = c("No sabe leer o escribir o fue menos de 5 años a la escuela",
                       "Educación primaria completa o fue a la escuela al menos 5 años",
                       "ESO, EGB o Bachiller Elemental (con titulo o cursados, al menos, 3º, 8º o 4º respectivamente) certificados de Estudios Primarios, Escolaridad (anterior a 1999), o Profesionalidad (niveles 1 o 2) y similares",
                       "Bachiller, BUP, COU, Bachiller Superior, FP de Grado Medio, FP Básica y otros estudios de grado medio (Certificado de Profesionalidad de nivel 3, etc…)",
                       "FP de Grado Superior, FPII y equivalentes",
                       "Grado de 240 ECTS, Diplomatura, Arquitectura e Ingeniería Técnicas y equivalentes.",
                       "Grado de más de 240 ECTS, Licenciatura, Arquitectura, Ingeniería, másteres, especialidad en Ciencias de la Salud y equivalentes.",
                       "Doctorado universitario."))%>%
  write_csv(., "T:/MSA/papers_internal/work_in_progress/Mi_Homogenized_Datainfrastructure/0_Data/1_Household Data/4_Spain/2_Codes/Education.Code.csv")
Occupation.Code <- distinct(hogar_1, occupation_hhh)%>% # SITUACTSP
  arrange(occupation_hhh)%>%
  mutate(Occupation = c("Trabajando al menos una hora",
                        "Con trabajo del que está ausente (por enfermedad, vacaciones, maternidad,...) y al que espera volver a incorporarse",
                        "Parado/a", 
                        "Jubilado/a, retirado/a anticipadamente",
                        "Estudiante", 
                        "Dedicado/a a las labores del hogar", 
                        "Con incapacidad laboral permanente",
                        "Otra situación de inactividad económica"))%>%
  write_csv(., "T:/MSA/papers_internal/work_in_progress/Mi_Homogenized_Datainfrastructure/0_Data/1_Household Data/4_Spain/2_Codes/Occupation.Code.csv")
Industry.Code <- distinct(hogar_1, industry_hhh)%>% # ACTESTB
  arrange(industry_hhh)%>%
  mutate(Industry = c("No aplicable  (si TRABAJO=6)",
                      "No consta (incluye actividades de organizaciones y organismos extraterritoriales)",
                      "Agricultura, ganadería, silvicultura y pesca",
                      "Industrias extractivas",
                      "Industria manufacturera",
                      "Suministro de energía eléctrica, gas, vapor y aire acondicionado",
                      "Suministro de agua, actividades de saneamiento, gestión de residuos y descontaminación",
                      "Construcción",
                      "Comercio al por mayor y al por menor; reparación  de vehículos de motor y motocicletas",
                      "Transporte y almacenamiento",
                      "Hostelería",
                      "Información y comunicaciones",
                      "Actividades financieras y de seguros",
                      "Actividades inmobiliarias",
                      "Actividades profesionales, científicas y técnicas",
                      "Actividades administrativas y servicios auxiliares",
                      "Administración pública y defensa; seguridad social obligatoria",
                      "Educación",
                      "Actividades sanitarias y de servicios sociales",
                      "Actividades artísticas, recreativas y de entretenimiento",
                      "Otros servicios",
                      "Actividades de los hogares como empleadores de personal doméstico"))%>%
  write_csv(., "T:/MSA/papers_internal/work_in_progress/Mi_Homogenized_Datainfrastructure/0_Data/1_Household Data/4_Spain/2_Codes/Industry.Code.csv")
Tenant.Code <- distinct(hogar_1, tenant)%>% # REGTEN
  arrange(tenant)%>%
  mutate(Tenant = c("Propiedad sin préstamo o hipoteca en curso",
                    "Propiedad con préstamo o hipoteca en curso", 
                    "Alquiler", 
                    "Alquiler reducido (renta antigua)", 
                    "Cesión semigratuita", 
                    "Cesión gratuita"))%>%
  write_csv(., "T:/MSA/papers_internal/work_in_progress/Mi_Homogenized_Datainfrastructure/0_Data/1_Household Data/4_Spain/2_Codes/Tenant.Code.csv")
Housing.Code <- distinct(hogar_1, housing_type)%>% # TIPOCASA
  arrange(housing_type)%>%
  mutate(Housing_Type = c("Chalé o casa grande", "Casa media", "Casa económica o alojamiento"))%>%
  write_csv(., "T:/MSA/papers_internal/work_in_progress/Mi_Homogenized_Datainfrastructure/0_Data/1_Household Data/4_Spain/2_Codes/Housing.Code.csv")
Water.Code <- distinct(hogar_1, water_energy)%>% # FUENAGUA
  arrange(water_energy)%>%
  mutate(Water_Energy = c("No aplicable  (si AGUACALI=6)", "No consta", "Electricidad", "Gas natural", "Gas licuado", "Otros combustibles líquidos", 
                          "Combustibles sólidos", "Otras"))%>%
  write_csv(., "T:/MSA/papers_internal/work_in_progress/Mi_Homogenized_Datainfrastructure/0_Data/1_Household Data/4_Spain/2_Codes/Water.Code.csv")
Heating.Code <- distinct(hogar_1, heating_fuel)%>% # FUENCALE
  arrange(heating_fuel)%>%
  mutate(Heating_Fuel = c("No aplicable  (si CALEF=6)", "No consta", "Electricidad", "Gas natural", "Gas licuado", "Otros combustibles líquidos", 
                          "Combustibles sólidos", "Otras"))%>%
  write_csv(., "T:/MSA/papers_internal/work_in_progress/Mi_Homogenized_Datainfrastructure/0_Data/1_Household Data/4_Spain/2_Codes/Heating.Code.csv")

# 5. Item codes ####

gastos_spss <- read_sav("../0_Data/1_Household Data/4_Spain/1_Data_Raw/datos_2019/EPFgastos_2019/SPSS/EPFgastos_2019.sav")

item_code <- stack(attr(gastos_spss$CODIGO, 'labels'))%>%
  rename(item_code = values, item_name = ind)

write.xlsx(item_code, "C:/Users/misl/OwnCloud/Distributional_Map/3_Data/4_Spain/3_Matching_Tables/Item_Codes_Description_Spain_Extract.xlsx")

rm(item_code)
