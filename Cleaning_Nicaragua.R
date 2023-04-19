if(!require("pacman")) install.packages("pacman")

p_load("haven", "Hmisc", "openxlsx", "rattle", "scales", "tidyverse", "stringi")

# Load Data ####

vivienda_hogar    <- read_sav("../0_Data/1_Household Data/3_Nicaragua/1_Data_Raw/EMNV14-02 DATOS DE LA VIVIENDA Y EL HOGAR.SAV")
programs_sociales <- read_sav("../0_Data/1_Household Data/3_Nicaragua/1_Data_Raw/EMNV14-03 SECCION 1 - PROGRAMAS SOCIALES.sav")
poblacion         <- read_sav("../0_Data/1_Household Data/3_Nicaragua/1_Data_Raw/EMNV14-04 POBLACION.sav")
negocios          <- read_sav("../0_Data/1_Household Data/3_Nicaragua/1_Data_Raw/EMNV14-05 NEGOCIOS.sav")
migracion         <- read_sav("../0_Data/1_Household Data/3_Nicaragua/1_Data_Raw/EMNV14-06 MIGRACION INTERNACIONAL.sav")

seccion7          <- read_sav("../0_Data/1_Household Data/3_Nicaragua/1_Data_Raw/EMNV14-07 VARIABLES SIMPLES DE LA SECCION 7.sav")
seccion7A         <- read_sav("../0_Data/1_Household Data/3_Nicaragua/1_Data_Raw/EMNV14-08 PARTE A DE LA SECCION 7.SAV")  # Especially relevant: food expenditures
seccion7B1        <- read_sav("../0_Data/1_Household Data/3_Nicaragua/1_Data_Raw/EMNV14-09 PARTE B1 DE LA SECCION 7.SAV") # Especially relevant: non-food items week
seccion7B2        <- read_sav("../0_Data/1_Household Data/3_Nicaragua/1_Data_Raw/EMNV14-10 PARTE B2 DE LA SECCION 7.SAV") # Especially relevant: non-food items month
seccion7B3        <- read_sav("../0_Data/1_Household Data/3_Nicaragua/1_Data_Raw/EMNV14-11 PARTE B3 DE LA SECCION 7.SAV") # Especially relevant: infrequent, 6 months
seccion7B4        <- read_sav("../0_Data/1_Household Data/3_Nicaragua/1_Data_Raw/EMNV14-12 PARTE B4 DE LA SECCION 7.SAV") # Especially relevant: infrequent, annual
seccion7C1        <- read_sav("../0_Data/1_Household Data/3_Nicaragua/1_Data_Raw/EMNV14-13 PARTE C1 DE LA SECCION 7.SAV")
seccion7C2        <- read_sav("../0_Data/1_Household Data/3_Nicaragua/1_Data_Raw/EMNV14-14 PARTE C2 DE LA SECCION 7.SAV")
seccion7C3        <- read_sav("../0_Data/1_Household Data/3_Nicaragua/1_Data_Raw/EMNV14-15 PARTE C3 DE LA SECCION 7.SAV")
seccion7C4        <- read_sav("../0_Data/1_Household Data/3_Nicaragua/1_Data_Raw/EMNV14-16 PARTE C4 DE LA SECCION 7.SAV")
seccion7C5        <- read_sav("../0_Data/1_Household Data/3_Nicaragua/1_Data_Raw/EMNV14-17 PARTE C5 DE LA SECCION 7.SAV")
seccion7D         <- read_sav("../0_Data/1_Household Data/3_Nicaragua/1_Data_Raw/EMNV14-18 PARTE D DE LA SECCION 7.SAV")  # Appliances
seccion7DA        <- read_sav("../0_Data/1_Household Data/3_Nicaragua/1_Data_Raw/EMNV14-19 PARTE DA DE LA SECCION 7.SAV") # Additional Appliances

agropecuarias     <- read_sav("../0_Data/1_Household Data/3_Nicaragua/1_Data_Raw/EMNV14-20 VARIABLES SIMPLES AGROPECUARIAS.sav")
fincas_proprias   <- read_sav("../0_Data/1_Household Data/3_Nicaragua/1_Data_Raw/EMNV14-21 FINCAS PROPIAS.sav")
fincas_alquiladas <- read_sav("../0_Data/1_Household Data/3_Nicaragua/1_Data_Raw/EMNV14-22 FINCAS ALQUILADAS.sav")

seccion8B1        <- read_sav("../0_Data/1_Household Data/3_Nicaragua/1_Data_Raw/EMNV14-23 PARTE B1 DE LA SECCION 8.sav")
seccion8C1C2      <- read_sav("../0_Data/1_Household Data/3_Nicaragua/1_Data_Raw/EMNV14-24 PARTES C1 Y C2 DE LA SECCION 8.sav")
seccion8_productos<- read_sav("../0_Data/1_Household Data/3_Nicaragua/1_Data_Raw/EMNV14-25 PARTE D PRODUCTOS DE LA SECCION 8.sav")
seccion8_subprod  <- read_sav("../0_Data/1_Household Data/3_Nicaragua/1_Data_Raw/EMNV14-26 PARTE D SUBPRODUCTOS DE LA SECCION 8.sav")

fuerza_sin        <- read_sav("../0_Data/1_Household Data/3_Nicaragua/1_Data_Raw/EMNV14-27 PARTE E FUERZA SIN ALIMENTACION.sav")
fuerza_trabajo_con<- read_sav("../0_Data/1_Household Data/3_Nicaragua/1_Data_Raw/EMNV14-28 PARTE E FUERZA DE TRABAJO CON ALIMENTACION.sav")
fuerza_trabajo_por<- read_sav("../0_Data/1_Household Data/3_Nicaragua/1_Data_Raw/EMNV14-29 PARTE E FUERZA DE TRABAJO POR SUELDOS Y SALARIOS.sav")
gastos_agrop      <- read_sav("../0_Data/1_Household Data/3_Nicaragua/1_Data_Raw/EMNV14-30 PARTE F GASTOS AGROP.sav")
equipos_agrop     <- read_sav("../0_Data/1_Household Data/3_Nicaragua/1_Data_Raw/EMNV14-31 PARTE F EQUIPOS AGROP.sav")
instalacion_agrop <- read_sav("../0_Data/1_Household Data/3_Nicaragua/1_Data_Raw/EMNV14-32 PARTE F INSTALACIONES AGROP.sav")
patio_agric       <- read_sav("../0_Data/1_Household Data/3_Nicaragua/1_Data_Raw/EMNV14-33 PARTE H1 PROD DE PATIO AGRIC.sav")
patio_pecuaria    <- read_sav("../0_Data/1_Household Data/3_Nicaragua/1_Data_Raw/EMNV14-34 PARTE I1 Y I2 PROD DE PATIO PECUARIA.sav")
productos_patio   <- read_sav("../0_Data/1_Household Data/3_Nicaragua/1_Data_Raw/EMNV14-35 PARTE J PRODUCTOS DE PATIO.sav")
subproductos_patio<- read_sav("../0_Data/1_Household Data/3_Nicaragua/1_Data_Raw/EMNV14-36 PARTE J SUBPRODUCTOS DE PATIO.sav")
probreza          <- read_sav("../0_Data/1_Household Data/3_Nicaragua/1_Data_Raw/Pobreza.sav")

# Transform Data ####

# Household Information

vivienda_hogar_1 <- vivienda_hogar %>%
  rename(hh_id = I00, province = DOMINIO4, urban = I06, water = S1P15, toilet = S1P18, lighting_fuel = S1P21, cooking_fuel = S1P25)%>%
  mutate(urban_01 = ifelse(urban == 1,1,0))%>%
  select(hh_id, province, urban_01, water, toilet, lighting_fuel, cooking_fuel)%>%
  mutate(electricity.access = ifelse(lighting_fuel == 1 | lighting_fuel == 2 | lighting_fuel == 3,1,0))

poblacion_1 <- poblacion %>%
  filter(S2P4 == 1)%>%
  rename(hh_id = I00, ind_hhh = S5P14, age_hhh = S2P2A, sex_hhh = S2P5, ethnicity.1 = S2P8, ethnicity.2 = S2P9, edu_hhh = S4P12A)%>%
  select(hh_id, age_hhh, sex_hhh, ind_hhh, edu_hhh, ethnicity.1, ethnicity.2)%>%
  mutate(ethnicity.2 = ifelse(is.na(ethnicity.2),"",ethnicity.2))

poblacion_2 <- poblacion %>%
  rename(hh_id = I00)%>%
  #filter(MIEMBRO == 1)%>%
  mutate(adults   = ifelse(S2P2A > 15,1,0),
         children = ifelse(S2P2A < 16,1,0))%>%
  group_by(hh_id)%>%
  summarise(adults   = sum(adults),
            children = sum(children),
            hh_size  = n(),
            hh_weights  = first(Peso2))%>%
  ungroup()

income_1 <- programs_sociales %>%
  rename(hh_id = I00)%>%
  select(hh_id, LINPROG, S1P34B, S1P34C, S1P35)%>%
  mutate(inc_gov_cash = S1P34C*S1P35)%>%
  mutate(inc_gov_cash = ifelse(is.na(inc_gov_cash),0, inc_gov_cash))%>%
  group_by(hh_id)%>%
  summarise(inc_gov_cash = sum(inc_gov_cash),
            inc_gov_monetary = 0)%>%
  ungroup()

exchange.rate <- 0.038522433

income_2 <- seccion7C3 %>%
  rename(hh_id = I00)%>%
  mutate(S7P35A = ifelse(S7P35B != 0, S7P35B*exchange.rate,S7P35A))%>%
  filter(S7C3COD != 1 & S7C3COD != 2)%>%
  mutate(type = ifelse(S7C3COD == 3,"inc_gov_cash",
                       ifelse(S7C3COD == 4, "inc_gov_monetary",
                              ifelse(S7C3COD == 5, "inc_gov_monetary",
                                     ifelse(S7C3COD == 6, "inc_gov_cash", "FAIL")))))%>%
  mutate(S7P35A = ifelse(is.na(S7P35A),0, S7P35A))%>%
  group_by(hh_id, type)%>%
  summarise(value = sum(S7P35A))%>%
  ungroup()%>%
  pivot_wider(names_from = "type", values_from = "value")

income_3 <- bind_rows(income_1, income_2)%>%
  group_by(hh_id)%>%
  summarise(inc_gov_cash = sum(inc_gov_cash),
            inc_gov_monetary = sum(inc_gov_monetary))%>%
  ungroup()

household_information <- left_join(vivienda_hogar_1, poblacion_1)%>%
  left_join(poblacion_2)%>%
  left_join(income_3)%>%
  unite(ethnicity, c(ethnicity.1, ethnicity.2), sep = "")%>%
  mutate(ethnicity = ifelse(ethnicity == "NANA",3,as.numeric(ethnicity)))%>%
  mutate(age_hhh = ifelse(is.na(age_hhh), 16, age_hhh),
         sex_hhh = ifelse(is.na(sex_hhh),  1, sex_hhh),
         edu_hhh = ifelse(is.na(edu_hhh),  0, edu_hhh))

# Ethnicity.Code

Ethnicity.1.Code <- stack(attr(poblacion_1$ethnicity.1, 'labels'))%>%
  rename(ethnicity.1 = values, Ethnicity.1 = ind)
Ethnicity.2.Code <- stack(attr(poblacion$S2P9, 'labels'))%>%
  rename(ethnicity.2 = values, Ethnicity.2 = ind)
Ethnicity.Code <- expand_grid(Ethnicity.1.Code, Ethnicity.2.Code)%>%
  unite(ethnicity, c(ethnicity.1, ethnicity.2), sep = "", remove = FALSE)%>%
  unite(Ethnicity_0, c(Ethnicity.1, Ethnicity.2), sep = ", ")%>%
  mutate(Ethnicity = ifelse(ethnicity.1 == 1, "Indigeneous",
                            ifelse(ethnicity.1 == 2, "Non-Indigeneous", "No Sabe")))%>%
  select(-ethnicity.1, -ethnicity.2)%>%
  rbind(c("2", "Non-Indigeneous", "Non-Indigeneous"), c("3", "No Sabe", "No Sabe"))%>%
  write_csv(., "../0_Data/1_Household Data/3_Nicaragua/2_Codes/Ethnicity.Code.csv")

write_csv(household_information, "../0_Data/1_Household Data/3_Nicaragua/1_Data_Clean/household_information_Nicaragua.csv")

# Codes

Province.Code <- stack(attr(household_information$province, 'labels'))%>%
  rename(province = values, Province = ind)%>%
  write_csv(., "../0_Data/1_Household Data/3_Nicaragua/2_Codes/Province.Code.csv")
Toilet.Code <- stack(attr(household_information$toilet, 'labels'))%>%
  rename(toilet = values, Toilet = ind)%>%
  write_csv(., "../0_Data/1_Household Data/3_Nicaragua/2_Codes/Toilet.Code.csv")
Water.Code <- stack(attr(household_information$water, 'labels'))%>%
  rename(water = values, Water = ind)%>%
  write_csv(., "../0_Data/1_Household Data/3_Nicaragua/2_Codes/Water.Code.csv")
Lighting.Code <- stack(attr(household_information$lighting_fuel, 'labels'))%>%
  rename(lighting_fuel = values, Lighting_Fuel = ind)%>%
  write_csv(., "../0_Data/1_Household Data/3_Nicaragua/2_Codes/Lighting.Code.csv")
Cooking.Code <- stack(attr(household_information$cooking_fuel, 'labels'))%>%
  rename(cooking_fuel = values, Cooking_Fuel = ind)%>%
  write_csv(., "../0_Data/1_Household Data/3_Nicaragua/2_Codes/Cooking.Code.csv")
Education.Code <- stack(attr(household_information$edu_hhh, 'labels'))%>%
  rename(edu_hhh = values, Education = ind)%>%
  write_csv(., "../0_Data/1_Household Data/3_Nicaragua/2_Codes/Education.Code.csv")
Gender.Code <- stack(attr(poblacion$S2P5, 'labels'))%>%
  rename(sex_hhh = values, Gender = ind)%>%
  write_csv(., "../0_Data/1_Household Data/3_Nicaragua/2_Codes/Gender.Code.csv")
Industry.Code <- stack(attr(household_information$ind_hhh, 'labels'))%>%
  rename(ind_hhh = values, Industry = ind)%>%
  write_csv(., "../0_Data/1_Household Data/3_Nicaragua/2_Codes/Industry.Code.csv")
Education.Code <- stack(attr(poblacion$S4P12A, 'labels'))%>%
  rename(edu_hhh = values, Education = ind)%>%
  write_csv(., "../0_Data/1_Household Data/3_Nicaragua/2_Codes/Education.Code.csv")

# Expenditures

exp_1 <- seccion7A %>%
  rename(hh_id = I00, item_code = S7PROD)%>%
  select(hh_id, item_code, S7P4, S7P6,
         S7P7, S7P8, S7P10)%>%
  filter(!is.na(S7P6)|!is.na(S7P10))%>%
  filter(S7P6 < 99990 | is.na(S7P6))%>%
  mutate(factor = ifelse(S7P4 == 1,365,
                         ifelse(S7P4 == 2,52,
                                ifelse(S7P4 == 3,26,
                                       ifelse(S7P4 == 4,12,
                                              ifelse(S7P4 == 5,4,
                                                     ifelse(S7P4 == 6,2,
                                                            ifelse(S7P4 == 7,1,9999))))))))%>%
  mutate(factor_sp = ifelse(S7P8 == 1,365,
                         ifelse(S7P8 == 2,52,
                                ifelse(S7P8 == 3,26,
                                       ifelse(S7P8 == 4,12,
                                              ifelse(S7P8 == 5,4,
                                                     ifelse(S7P8 == 6,2,
                                                            ifelse(S7P8 == 7,1,9999))))))))%>%
  select(-S7P4, -S7P8)%>%
  mutate(expenditures_year    = ifelse(!is.na(factor), S7P6*factor,0),
         expenditures_sp_year = ifelse(!is.na(factor_sp), S7P10*factor_sp,0))%>%
  select(hh_id, item_code, expenditures_year, expenditures_sp_year)%>%
  mutate(item_code = as.character(item_code))

exp_2 <- seccion7B1 %>%
  rename(hh_id = I00, item_code = S7B1COD, expenditures = S7P18)%>%
  filter(!is.na(expenditures))%>%
  select(hh_id, item_code, expenditures)%>%
  mutate(item_code = paste0("B", item_code),
         expenditures_year = expenditures*52,
         expenditures_sp_year = 0)%>%
  select(hh_id, item_code, expenditures_year, expenditures_sp_year)

exp_3 <- seccion7B2 %>%
  rename(hh_id = I00, item_code = S7B2COD, expenditures = S7P20)%>%
  filter(!is.na(expenditures))%>%
  mutate(item_code = paste0("C", item_code),
         expenditures_year = expenditures*12,
         expenditures_sp_year = 0)%>%
  select(hh_id, item_code, expenditures_year, expenditures_sp_year)

exp_4 <- seccion7B3 %>%
  rename(hh_id = I00, item_code = S7B3COD, expenditures = S7P22)%>%
  filter(!is.na(expenditures))%>%
  mutate(item_code = paste0("D", item_code),
         expenditures_year = expenditures*2,
         expenditures_sp_year = 0)%>%
  select(hh_id, item_code, expenditures_year, expenditures_sp_year)

exp_5 <- seccion7B4 %>%
  rename(hh_id = I00, item_code = S7B4COD, expenditures_year = S7P25)%>%
  filter(!is.na(expenditures_year))%>%
  mutate(item_code = paste0("E", item_code),
         expenditures_sp_year = 0)%>%
  select(hh_id, item_code, expenditures_year, expenditures_sp_year)

exchange.rate <- 0.038522433

exp_6 <- vivienda_hogar %>%
  rename(hh_id = I00)%>%
  select(hh_id,
         S1P12A, S1P12B, # Monthly
         S1P13A, S1P13B, # Monthly
         S1P17,          # Monthly
         S1P20,          # Monthly
         S1P23,          # Monthly
         S1P24,          # Monthly
         S1P27,          # Monthly
         S1P30A, S1P30B, S1P30C, S1P30D, S1P30E, S1P30F)%>% # Monthly
  pivot_longer(-hh_id, names_to = "item_code", values_to = "expenditures")%>%
  filter(!is.na(expenditures))%>%
  mutate(expenditures = ifelse(item_code == "S1P12B" | item_code == "S1P13B", expenditures/exchange.rate, expenditures))%>%
  mutate(expenditures_year = expenditures*12,
         expenditures_sp_year = 0)%>%
  select(hh_id, item_code, expenditures_year, expenditures_sp_year)

exp_6.1 <- vivienda_hogar %>%
  rename(hh_id = I00, lighting = S1P21, cooking = S1P25)%>%
  select(hh_id, lighting, cooking)

exp_6.2 <- exp_6 %>%
  left_join(exp_6.1, by = "hh_id")%>%
  mutate(item_code = ifelse(item_code == "S1P24", paste0(item_code, "_", lighting), item_code))%>%
  mutate(item_code = ifelse(item_code == "S1P27", paste0(item_code, "_", cooking), item_code))%>%
  select(-lighting, -cooking)

exp_7 <- poblacion%>%
  rename(hh_id = I00)%>%
  select(hh_id, 
         S3P4B, # Monthly
         S3P5B, # Monthly
         S3P6B, # Monthly
         S3P7B, # Monthly
         S3P9B, # Monthly
         S3P10B,# Monthly
         S3P13B,# Annual
         S3P15B,# Annual
         S3P16B,# Annual
         S4P6B, S4P6C, # Multiply
         S4P7B, S4P7C, # Multiply
         S4P8B, S4P8C, # Monthly
         S4P9B, S4P9C, # Annual
         S4P10B, S4P10C, S4P10D, S4P10E, # Annual
         S4P21B, S4P21C, # Multiply
         S4P22B, S4P22C, # Multiply
         S4P23B, S4P23C, # Multiply
         S4P24B, S4P24C, # Monthly
         S4P25B, S4P25C, # Annual
         S4P26B, S4P26C, S4P26D, # Annual
         S4P30)%>% # Annual
  mutate(S4P6B = S4P6B*S4P6C,
         S4P7B = S4P7B*S4P7C,
         S4P21B = S4P21B*S4P21C,
         S4P22B = S4P22B*S4P22C,
         S4P23B = S4P23B*S4P23C)%>%
  select(-S4P6C, -S4P7C, -S4P21C, -S4P22C, -S4P23C)%>%
  pivot_longer(-hh_id, names_to = "item_code", values_to = "expenditures")%>%
  filter(!is.na(expenditures))%>%
  mutate(expenditures_year = ifelse(item_code %in% c("S3P4B", "S3P5B", "S3P6B", "S3P7B", "S3P9B", "S3P10B", "S4P8B", "S4P8C", "S4P21B", "S4P22B", "S4P23B", "S4P24B", "S4P24C"),
                                    expenditures*12,expenditures),
         expenditures_sp_year = 0)%>%
  select(hh_id, item_code, expenditures_year, expenditures_sp_year)%>%
  group_by(hh_id, item_code)%>%
  summarise(expenditures_year = sum(expenditures_year),
            expenditures_sp_year = sum(expenditures_sp_year))%>%
  ungroup()%>%
  filter(expenditures_year > 0)

# Fuels for Lighting and Cooking can be derived from answers

exp_all <- bind_rows(exp_1, exp_2, exp_3, exp_4, exp_5, exp_6.2, exp_7)%>%
  arrange(hh_id, item_code)

write_csv(exp_all, "../0_Data/1_Household Data/3_Nicaragua/1_Data_Clean/expenditures_items_Nicaragua.csv")
  
# Item-Codes

Item.Codes.1 <- stack(attr(seccion7A$S7PROD, 'labels'))%>%
  rename(item_code = values, Item_name = ind)%>%
  mutate(item_code = as.character(item_code))
Item.Codes.2 <- stack(attr(seccion7B1$S7B1COD, 'labels'))%>%
  mutate(item_code = paste0("B", values))%>%
  rename(Item_name = ind)
Item.Codes.3 <- stack(attr(seccion7B2$S7B2COD, 'labels'))%>%
  mutate(item_code = paste0("C", values))%>%
  rename(Item_name = ind)
Item.Codes.4 <- stack(attr(seccion7B3$S7B3COD, 'labels'))%>%
  mutate(item_code = paste0("D", values))%>%
  rename(Item_name = ind)
Item.Codes.5 <- stack(attr(seccion7B4$S7B4COD, 'labels'))%>%
  mutate(item_code = paste0("E", values))%>%
  rename(Item_name = ind)
Item.Codes.6 <- distinct(exp_6.2, item_code)%>%
  arrange(item_code)
Item.Codes.7 <- distinct(exp_7, item_code)%>%
  arrange(item_code)

Item.Codes.all <- bind_rows(Item.Codes.1, Item.Codes.2, Item.Codes.3, Item.Codes.4, Item.Codes.5, Item.Codes.6, Item.Codes.7)

# write.xlsx(Item.Codes.all, "../0_Data/1_Household Data/3_Nicaragua/3_Matching_Tables/Item_Code_Description_Nicaragua.xlsx")

# Appliances

appliances_01.1 <- seccion7D %>%
  rename(hh_id = I00)%>%
  select(hh_id, S7DCOD, S7P47)

Appliances <- stack(attr(appliances_01.1$S7DCOD, 'labels'))

# write.xlsx(Appliances, "../0_Data/1_Household Data/3_Nicaragua/2_Codes/Appliance.Codes.xlsx")

Appliances <- read.xlsx("../0_Data/1_Household Data/3_Nicaragua/2_Codes/Appliance.Codes.xlsx")%>%
  rename(S7DCOD = values)

appliances_01.2 <- appliances_01.1 %>%
  left_join(Appliances)%>%
  filter(!is.na(Var))%>%
  mutate(posession = ifelse(S7P47 == 1,1,0))%>%
  select(hh_id, Var, posession)%>%
  pivot_wider(names_from = "Var", values_from = "posession")%>%
  mutate(tv.01 = ifelse(tv.01a == 1 | tv.01b == 1,1,0))%>%
  select(hh_id, ends_with("01"))

write_csv(appliances_01.2, "../0_Data/1_Household Data/3_Nicaragua/1_Data_Clean/appliances_0_1_Nicaragua.csv")

#### #### ####

# Transform Data
#select and rename variables of interest 

data_1_personas <- data_0_personas %>%
  select(ZONA, FOLIO, FE, NPERSONAS, JHOGAR, PARENTESCO, SEXO, ECIVIL, EDAD, EDUNIVEL, EDUDEPENDENCIA, CAE, AECIUO88, TRANSFERENCIAS_RECIBIDAS)%>%
  rename(area = ZONA, hh_id = FOLIO, hh_weights = FE, hh_size = NPERSONAS, 
         head = JHOGAR, family_status = PARENTESCO, sex_hhh = SEXO, 
         age_hhh=EDAD, marital_status = ECIVIL, edu_hhh = EDUNIVEL, 
         edu_institution = EDUDEPENDENCIA, empl_hhh = CAE,
         ind_hhh = AECIUO88, cash_transfers_year = TRANSFERENCIAS_RECIBIDAS)%>%
  mutate(children = ifelse(age_hhh<18, 1, 0),
         adults = ifelse(age_hhh>=18,1,0))%>%
  group_by(hh_id)%>%
  mutate(children = sum(children),
         adults = sum(adults))%>%
  ungroup()
  
child <- NULL
#remove all entries except for heads of household
data_2_personas <- data_1_personas %>%
  filter(head==1)%>%
  select(hh_id, hh_size, hh_weights, area, sex_hhh, marital_status, adults, children, age_hhh, edu_hhh, edu_institution, ind_hhh)

#vp05a - vp05j do you have to pay for..? (gas, rent, electricity, security..)
#TA09 net salary last month
#TA11 average income over 12 months self reported
#JU01 retirement income? y/n JU02 how much? gross per month
#TR01 pension -||- TR02 -||-
#TR08 last month money from state (1) from non-profit (2) both(3) no(4) TR09 how much
#TR11 did you receive non-money donations? y/n TR13 from whom? homes (1) non-profits(2) state(3) TR14 how much worth 

code_marital <- data.frame(marital_status = c(seq(1,5,1),-88,-99), marital_Status = c("Married", "Civil Union", "Divorced", "Widowed", "Unmarried", "Unknown", "Unknown"))
code_area <- data.frame(area = c(1,2), area = c("Gran Santiago", "Other"))
code_edu <- data.frame(edu_hhh = c(seq(1,17,1), -88, -99), edu_hhh = c("None", "Just starting formal education", "Special Education", "Kindergarten", "Kindergarten", "Kindergarten", "Primary Education", "Primary Education", "Secondary Education", "Secondary Education", "Bachelor's Equivalent", "Bachelor's Equivalent", "Master's Equivalent", "Professional", "Postgrad Equivalent", "Master's", "PhD", "Unknown", "Unknown"))
code_eduinst <- data.frame(edu_institution = c(seq(1,12,1),-77,-88,-99), edu_institution = c("Municipal School", "Subsidized Private School", "State-Owned Technical School", "Private School", "Workplace Kindergarten", "Other Kindergarten/Elementary-/Middle School", "Technical Training Center (higher education)", "Professional Institute (higher education)", "Public University", "Private University", "Armed Forces", "Other higher education", "Unknown/None", "Unknown/None", "Unknown/None"))
code_jobind <- data.frame(ind_hhh = c(seq(1,10,1),-77,-88), Industry = c("Government and Public Administration", "Science", "Technicians and Professionals", "Office Workers", "Service Workers and Vendors", "Agriculture and Fishing", "Trades", "Heavy Machine Assembly and Operation", "Unskilled Labor", "Other", "Unknown/None", "Unknown/None"))

data_personas <- data_2_personas %>%
  left_join(code_marital, by = "marital_status")%>%
  left_join(code_area, by = "area")%>%
  left_join(code_edu, by = "edu_hhh")%>%
  left_join(code_eduinst, by = "edu_institution")%>%
  left_join(code_jobind, by = "ind_hhh")


#reduce gastos to the necessary size
#reduce item code to 6 digit code if necessary

data_gastos <- data_0_gastos %>%
  select(FOLIO, CCIF, GASTO) %>%
  rename(hh_id= FOLIO, item_code = CCIF, expenditures_year = GASTO)%>%
  mutate(item_code = ifelse(stri_length(item_code) == 7, paste("0",item_code,sep = ""), item_code))%>%
  mutate(smallCode = substring(item_code,1,6),
         expenditures_year = expenditures_year*12)
gtap_codes1 <- gtap_codes  %>%
  select (-Explanation) %>%
  pivot_longer(-GTAP, names_to = "drop", values_to = "item_code")%>%
  filter(!is.na(item_code))%>%
  select(GTAP, item_code)%>%
  mutate(GTAP = ifelse(GTAP == "gas" | GTAP == "gdt", "gasgdt", GTAP))

data_1_gastos <- data_gastos %>%
  left_join(gtap_codes1, by = c("item_code"))%>%
  left_join(rename(gtap_codes1, GTAPB = GTAP), by = c("smallCode" = "item_code"))%>%
  mutate(item_code_new = ifelse(is.na(GTAP), smallCode,
                                ifelse(is.na(GTAPB), item_code, "FAIL")))

data_gastos <- data_1_gastos%>%
  select(hh_id, item_code_new, expenditures_year)%>%
  rename(item_code = item_code_new)%>%
  group_by(hh_id, item_code)%>%
  summarise(expenditures_year= sum(expenditures_year))%>%
  ungroup()
#write data into new files 
write_csv(data_personas, "../0_Data/1_Household Data/3_Nicaragua/1_Data_Clean/household_information_Nicaragua.csv")
write_csv(data_gastos, "../0_Data/1_Household Data/3_Nicaragua/1_Data_Clean/expenditures_items_Nicaragua.csv")
