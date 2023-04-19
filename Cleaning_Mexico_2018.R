# This script aims at cleaning household data for the Mexcian 2018 ENIGH data

if(!require("pacman")) install.packages("pacman")

p_load("haven", "Hmisc", "openxlsx", "rattle", "scales", "tidyverse")

# Load Data

data_0_hogares     <- read_dta("../0_Data/1_Household Data/3_Mexico/1_Data_Raw/ENIGH_2018/hogares.dta")
data_0_gastosh     <- read_dta("../0_Data/1_Household Data/3_Mexico/1_Data_Raw/ENIGH_2018/gastoshogar.dta")
data_0_poblacion   <- read_dta("../0_Data/1_Household Data/3_Mexico/1_Data_Raw/ENIGH_2018/poblacion.dta")
data_0_viviendas   <- read_dta("../0_Data/1_Household Data/3_Mexico/1_Data_Raw/ENIGH_2018/viviendas.dta")
data_0_ingresos    <- read_dta("../0_Data/1_Household Data/3_Mexico/1_Data_Raw/ENIGH_2018/ingresos.dta")
data_0_gastosp     <- read_dta("../0_Data/1_Household Data/3_Mexico/1_Data_Raw/ENIGH_2018/gastospersona.dta")
data_0_trabajos    <- read_dta("../0_Data/1_Household Data/3_Mexico/1_Data_Raw/ENIGH_2018/trabajos.dta")
data_0_concentrado <- read_dta("../0_Data/1_Household Data/3_Mexico/1_Data_Raw/ENIGH_2018/concentradohogar.dta")

# Transform Data - Expenditures

data_1_gastosh <- data_0_gastosh %>%
  mutate(Type = "Gasto")

data_1_gastosp <- data_0_gastosp %>%
  mutate(Type = "Persona")

data_1_gasto <- bind_rows(data_1_gastosh, data_1_gastosp)

item_codes <- count(data_1_gasto, clave)%>%
  select(-n)

#write.xlsx(item_codes, "../0_Data/1_Household Data/3_Mexico/3_Matching_Tables/Item_Code_Description_Mexico_1.xlsx")

data_2_gasto <- data_1_gasto %>%
  unite(hh_id, c(folioviv, foliohog))%>%
  rename(item_code = clave, expenditure_type = tipo_gasto)%>%
  mutate(expenditure_type_aux = ifelse(expenditure_type %in% c("G1","G2"),1,2),
         expenditures_month   = gasto_tri/3)%>%
  select(hh_id, item_code, expenditure_type_aux, expenditures_month)%>%
  filter(!is.na(expenditures_month))%>%
  mutate(expenditures_year    = ifelse(expenditure_type_aux == 1, expenditures_month*12,0),
         expenditures_sp_year = ifelse(expenditure_type_aux == 2, expenditures_month*12,0))%>%
  group_by(hh_id, item_code)%>%
  summarise(expenditures_year    = sum(expenditures_year),
            expenditures_sp_year = sum(expenditures_sp_year))%>%
  ungroup()

write_csv(data_2_gasto, "../0_Data/1_Household Data/3_Mexico/1_Data_Clean/expenditures_items_Mexico.csv")  

# Transform Data - Household

household_1 <- data_0_concentrado %>%
  unite(hh_id, c(folioviv, foliohog), remove = FALSE)%>%
  rename(hh_weights = factor, district = ubica_geo, density = tam_loc,
         sex_hhh = sexo_jefe, edu_hhh = educa_jefe, hh_size = tot_integ,
         adults = mayores, children = menores)%>%
  mutate(urban = substr(folioviv,3,3),
         urban_01 = ifelse(urban == 6, 0,1))%>%
  select(hh_id, hh_weights, hh_size, district, density, sex_hhh, edu_hhh, hh_size, adults, children, urban_01, folioviv)%>%
  mutate(province = str_sub(district,1,2))

household_2 <- data_0_viviendas %>%
  rename(water = disp_agua, toilet = excusado, toilet_1 = uso_compar, toilet_2 = sanit_agua, electricity = disp_elect, cooking_fuel = combustible)%>%
  select(folioviv, water, toilet, toilet_1, toilet_2, electricity, cooking_fuel)%>%
  mutate(electricity.access = ifelse(electricity == 5,0,1))%>%
  select(-electricity)%>%
  unite(toilet, c(toilet, toilet_1, toilet_2), sep = "")%>%
  mutate(cooking_fuel = ifelse(cooking_fuel == "&",6,cooking_fuel))

household_3 <- data_0_poblacion %>%
  unite(hh_id, c(folioviv, foliohog), remove = FALSE)%>%
  filter(numren == "01")%>%
  rename(language_0 = hablaind, language_2 = lenguaind, language_3 = hablaesp, indigeneous = etnia, alphabetism = alfabetism)%>%
  mutate(ethnicity = ifelse(indigeneous == 1,1,2),
         language  = ifelse(language_0 == 1 & language_3 == 1, 1,
                            ifelse(language_0 == 1 & language_3 == 2, 2, 3)))%>%
  select(hh_id, ethnicity, language, alphabetism)

household_4 <- data_0_trabajos %>%
  unite(hh_id, c(folioviv, foliohog), remove = FALSE)%>%
  filter(numren == "01")%>%
  filter(id_trabajo == 1)%>%
  select(hh_id, scian)%>%
  rename(ind_hhh = scian)

household_final <- household_1%>%
  left_join(household_2, by = "folioviv")%>%
  left_join(household_3)%>%
  left_join(household_4)%>%
  select(-folioviv)

# Information on Income / Transfers

data_1_ingresos <- data_0_ingresos %>%
  unite(hh_id, c(folioviv, foliohog), remove = FALSE)

income_codes <- distinct(data_1_ingresos, clave)%>%
  arrange(clave)

# write.xlsx(income_codes, "../0_Data/1_Household Data/3_Mexico/2_Codes/Income_Type_Codes.xlsx")

income_codes_1 <- read.xlsx("../0_Data/1_Household Data/3_Mexico/2_Codes/Income_Type_Codes.xlsx", sheet = 2)

data_2_ingresos <- data_1_ingresos %>%
  left_join(income_codes_1)%>%
  mutate(income_month = ing_tri/3)%>%
  select(hh_id, numren, clave, income_month, income_class)%>%
  group_by(hh_id, income_class)%>%
  summarise(income_month = sum(income_month))%>%
  pivot_wider(names_from = "income_class", values_from = "income_month", values_fill = 0, names_prefix = "inc_")

household_final <- left_join(household_final, data_2_ingresos)%>%
  # Adjustment for 16 households
  mutate(inc_gov_cash     = ifelse(is.na(inc_gov_cash),     0, inc_gov_cash),
         inc_gov_monetary = ifelse(is.na(inc_gov_monetary), 0, inc_gov_monetary))%>%
  filter(hh_id %in% data_2_gasto$hh_id)

write_csv(household_final, "../0_Data/1_Household Data/3_Mexico/1_Data_Clean/household_information_Mexico.csv")

# Information on Appliances

appliances_1 <- data_0_viviendas %>%
  rename(ac.01 = aire_acond)%>%
  select(folioviv, ac.01)
  
appliances_2 <- data_0_hogares %>%
  unite(hh_id, c(folioviv, foliohog), remove = FALSE)%>%
  rename(mobile.01 = celular, car.01 = num_auto, car.01.1 = num_van, car.01.2 = num_pickup, motorcycle.01 = num_moto,
         radio.01 = num_radio, tv.01.1 = num_tva, tv.01.2 = num_tvd, microwave.01 = num_micro, refrigerator.01 = num_refri,
         stove.01 = num_estuf, washing_machine.01 = num_lavad, fan.01 = num_venti, vacuum.01 = num_aspir, computer.01 = num_compu)%>%
  select(hh_id, folioviv, ends_with("01"), ends_with("01.1"), ends_with("01.2"))%>%
  mutate(car.01 = ifelse(car.01 == 1 | car.01.2 == 1 | car.01.1 == 1,1,0),
         tv.01  = ifelse(tv.01.1 == 1 | tv.01.2 == 1,1,0))%>%
  select(-ends_with("01.1"), - ends_with("01.2"))

appliances_3 <- left_join(appliances_2, appliances_1, by = "folioviv")%>%
  select(-folioviv)%>%
  mutate_at(vars(-hh_id), list(~ ifelse(. > 0,1,0)))

write_csv(appliances_3, "../0_Data/1_Household Data/3_Mexico/1_Data_Clean/appliances_0_1_Mexico.csv")

# Codes
Mex_District <- read_csv("../0_Data/1_Household Data/3_Mexico/2_Codes/Mexico_District_Codes.csv")%>%
  select(Nom_Ent, Cve_Ent, Nom_Mun, Cve_Mun)%>%
  distinct()

District.Code <- distinct(household_1, district)%>%
  separate(district, c("Entidad", "Municipo"), sep = 2, remove = FALSE)%>%
  left_join(Mex_District, by = c("Entidad" = "Cve_Ent", "Municipo" = "Cve_Mun"))
Province.Code.1 <- District.Code %>%
  select(Entidad, Nom_Ent)%>%
  rename(province = Entidad, Province = Nom_Ent)%>%
  distinct()
District.Code.1 <- District.Code %>%
  select(-Entidad, - Municipo)%>%
  mutate(District = paste0(Nom_Mun, " (", Nom_Ent,")"))%>%
  select(district, District)
Density.Code <- distinct(household_1, density)%>%
  bind_cols(Density = c("More than 100,000 citizens", "15-100,000 citizens", "2.5-15,000 citizens", "Less than 2,500 citizens"))
Gender.Code <- distinct(household_1, sex_hhh)%>%
  arrange(sex_hhh)%>%
  bind_cols(Gender = c("Male", "Female"))
Education.Code <- distinct(household_1, edu_hhh)%>%
  arrange(edu_hhh)%>%
  bind_cols(Education = c("No schooling", "Preschool", "Incomplete Primary", "Primary", "Incomplete Secondary", "Secondary", "Incomplete High School",
                          "High School", "Incomplete Professional", "Professional", "Postgraduate"))
Water.Code <- distinct(household_2, water)%>%
  arrange(water)%>%
  bind_cols(Water = c("Piped Water (Inside)", "Piped Water (Outside)", "Piped Water (Public)", "Rain Water", "Piped Water (Other Home)", "Pipe Water ?", "Well, River, Lake, Stream"))
Toilet.Code.0      <- distinct(data_0_viviendas, excusado)%>%
  bind_cols(Toilet = c("Yes, has sanitation facilities inside", "No, does not have sanitation facilities inside"))
Toilet.Code.1    <- distinct(data_0_viviendas, uso_compar)%>%
  arrange(uso_compar)%>%
  bind_cols(Toilet.1 = c("", "Shared with other household", "Private"))
Toilet.Code.2    <- distinct(data_0_viviendas, sanit_agua)%>%
  arrange(sanit_agua)%>%
  bind_cols(Toilet.2 = c("", "Direct Discharge", "Water Discharge from Bucket", "No Water Discharge"))
Toilet.Code <- expand.grid(excusado   = Toilet.Code.0$excusado,
                           uso_compar = Toilet.Code.1$uso_compar,
                           sanit_agua = Toilet.Code.2$sanit_agua)%>%
  distinct()%>%
  left_join(Toilet.Code.0)%>%
  left_join(Toilet.Code.1)%>%
  left_join(Toilet.Code.2)%>%
  unite(toilet, c(excusado, uso_compar, sanit_agua), sep = "")%>%
  filter(toilet %in% household_2$toilet)%>%
  unite(TLT, c(Toilet, Toilet.1, Toilet.2), sep = " - ")
Cooking.Code     <- distinct(household_2, cooking_fuel)%>%
  arrange(cooking_fuel)%>%
  bind_cols(Cooking_Fuel = c("Firewood", "Coal", "LPG", "Natural Gas", "Electricity", "Other Fuel"))%>%
  write_csv(.,     "../0_Data/1_Household Data/3_Mexico/2_Codes/Cooking.Code.csv")
Electricity.Code <- distinct(household_2, electricity)%>%
  bind_cols(Electricity = c("Public Grid", "Plant", "Solar Panel", "Another Source", "No electric light"))
Ethnicitiy.Code  <- distinct(household_3, ethnicity)%>%
  arrange(ethnicity)%>%
  bind_cols(Ethnicity = c("Indigeneous", "Non-Indigeneous"))
Language.Code    <- distinct(household_3, language)%>%
  arrange(language)%>%
  bind_cols(Language = c("Indigeneous and Spanish", "Indigeneous", "Spanish"))
Alphabetism.Code <- distinct(household_3, alphabetism)%>%
  bind_cols(Alphabetism = c("Alphabet", "Analphabet"))
Industry.Code    <- distinct(household_4, ind_hhh)%>%
  arrange(ind_hhh)
# Filled Manually

#write_csv(District.Code,    "../0_Data/1_Household Data/3_Mexico/2_Codes/District.Code.csv")
write_csv(District.Code.1,    "../0_Data/1_Household Data/3_Mexico/2_Codes/District.Code.csv")
write_csv(Province.Code.1,    "../0_Data/1_Household Data/3_Mexico/2_Codes/Province.Code.csv")
write_csv(Density.Code,     "../0_Data/1_Household Data/3_Mexico/2_Codes/Density.Code.csv")
write_csv(Gender.Code,      "../0_Data/1_Household Data/3_Mexico/2_Codes/Gender.Code.csv")
write_csv(Education.Code,   "../0_Data/1_Household Data/3_Mexico/2_Codes/Education.Code.csv")
write_csv(Water.Code,       "../0_Data/1_Household Data/3_Mexico/2_Codes/Water.Code.csv")
write_csv(Toilet.Code,      "../0_Data/1_Household Data/3_Mexico/2_Codes/Toilet.Code.csv")
write_csv(Cooking.Code,     "../0_Data/1_Household Data/3_Mexico/2_Codes/Cooking.Code.csv")
write_csv(Electricity.Code, "../0_Data/1_Household Data/3_Mexico/2_Codes/Electricity.Code.csv")
write_csv(Ethnicitiy.Code,  "../0_Data/1_Household Data/3_Mexico/2_Codes/Ethncitiy.Code.csv")
write_csv(Language.Code,    "../0_Data/1_Household Data/3_Mexico/2_Codes/Language.Code.csv")
write_csv(Alphabetism.Code, "../0_Data/1_Household Data/3_Mexico/2_Codes/Alphabetism.Code.csv")


