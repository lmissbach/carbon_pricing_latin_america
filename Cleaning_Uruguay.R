if(!require("pacman")) install.packages("pacman")

p_load("haven", "Hmisc", "openxlsx", "rattle", "scales", "tidyverse")

# This script cleans the ENGIH household data for Uruguay

# Import Data

hogares_0  <- read_sav("../0_Data/1_Household Data/3_Uruguay/1_Data_Raw/ENGIH 2016 Base de Datos Hogares.sav")
personas_0 <- read_sav("../0_Data/1_Household Data/3_Uruguay/1_Data_Raw/ENGIH 2016 Base de Datos Personas.sav")
gastos_0   <- read_sav("../0_Data/1_Household Data/3_Uruguay/1_Data_Raw/ENGIH 2016 Base de Datos Gastos.sav")

hogares_0  <- read_sav("C:/Users/misl/ownCloud/A_Seminar_Verteilungsanalyse/3_Daten_Code/1_Uruguay_Data/Data_Uruguay/ENGIH 2016 Base de Datos Hogares.sav")
personas_0 <- read_sav("C:/Users/misl/ownCloud/A_Seminar_Verteilungsanalyse/3_Daten_Code/1_Uruguay_Data/Data_Uruguay/ENGIH 2016 Base de Datos Personas.sav")
gastos_0   <- read_sav("C:/Users/misl/ownCloud/A_Seminar_Verteilungsanalyse/3_Daten_Code/1_Uruguay_Data/Data_Uruguay/ENGIH 2016 Base de Datos Gastos.sav")
#estratos_0   <- read_sav("C:/Users/misl/ownCloud/A_Seminar_Verteilungsanalyse/3_Daten_Code/1_Uruguay_Data/Data_Uruguay/ENGIH 2016 UPM y Estratos.sav")

# Household Information

hogares_1 <- hogares_0 %>%
  select(NUMERO, everything())%>%
  rename(hh_id = NUMERO, province = REGION, water = D11, toilet.a = D15, toilet.b = D16,
         lighting_fuel = D18, heating_fuel = D260, cooking_fuel = D20, adults.a = D23, children.a = D24,
         hh_size.a = D25, hh_weights = peso)%>%
  mutate(electricity.access = ifelse(lighting_fuel == 1,1,0))%>%
  unite(toilet, c(toilet.a, toilet.b), sep = "")%>%
  select(hh_id, hh_weights, province, water, toilet, lighting_fuel, heating_fuel, cooking_fuel, electricity.access,
         ends_with(".a"))

# Codes Household Information
Province.Code <- distinct(hogares_1, province)%>%
  arrange(province)%>%
  bind_cols(Province = c("Montevideo", "Interior, mas que 5000 inhabitantes", "Interior, menos que 5000 inhabitantes"))%>%
  write_csv(., "../0_Data/1_Household Data/3_Uruguay/2_Codes/Province.Code.csv")
Lighting.Code <- stack(attr(hogares_1$lighting_fuel, 'labels'))%>%
  rename(lighting_fuel = values, Lighting.Fuel = ind)%>%
  write_csv(., "../0_Data/1_Household Data/3_Uruguay/2_Codes/Lighting.Code.csv")
Cooking.Code <- stack(attr(hogares_1$cooking_fuel, 'labels'))%>%
  rename(cooking_fuel = values, Cooking.Fuel = ind)%>%
  write_csv(., "../0_Data/1_Household Data/3_Uruguay/2_Codes/Cooking.Code.csv")
Heating.Code <- stack(attr(hogares_1$heating_fuel, 'labels'))%>%
  rename(heating_fuel = values, Heating.Fuel = ind)%>%
  write_csv(., "../0_Data/1_Household Data/3_Uruguay/2_Codes/Heating.Code.csv")
Water.Code <- stack(attr(hogares_1$water, 'labels'))%>%
  rename(water = values, Water = ind)%>%
  write_csv(., "../0_Data/1_Household Data/3_Uruguay/2_Codes/Water.Code.csv")
Toilet.A.Code <- stack(attr(hogares_1$toilet.a, 'labels'))%>%
  rename(A1 = values, A2 = ind)
Toilet.B.Code <- stack(attr(hogares_1$toilet.b, 'labels'))%>%
  rename(B1 = values, B2 = ind)
Toilet.Code   <- expand_grid(Toilet.A.Code, Toilet.B.Code)%>%
  unite(toilet, c(A1,B1), sep = "")%>%
  unite(Toilet, c(A2,B2), sep = "")%>%
  write_csv(., "../0_Data/1_Household Data/3_Uruguay/2_Codes/Toilet.Code.csv")

hogares_2 <- hogares_1 %>%
  zap_formats()%>%
  remove_all_labels()%>%
  mutate(urban_01 = ifelse(province == "03",0,1))

personas_1 <- personas_0 %>%
  select(NUMERO, everything())%>%
  filter(E30 == 1)%>%
  rename(sex_hhh = E26, age_hhh = E27, ethnicity = E29_6, ind_hhh = F76_2, hh_id = NUMERO)%>%
  remove_all_labels()%>%
  zap_formats()%>%
  select(hh_id, sex_hhh, age_hhh, ethnicity, ind_hhh, E224_1, E221_1, E218_1, E215_1, E212_1, E201_1, E197_1)%>%
  mutate(edu_hhh = ifelse(E224_1 == 1, "Postgrado",
                          ifelse(E221_1 == 1, "Terciario no universitario",
                                 ifelse(E218_1 == 1, "Universidad o similar", NA))))%>%
  mutate(edu_hhh = ifelse(is.na(edu_hhh) & E215_1 == 1, "Magisterio o Profesorado", edu_hhh))%>%
  mutate(edu_hhh = ifelse(is.na(edu_hhh) & E212_1 == 1, "Educacion Technica", edu_hhh))%>%
  mutate(edu_hhh = ifelse(is.na(edu_hhh) & E201_1 == 1, "Educacion Media", edu_hhh))%>%
  mutate(edu_hhh = ifelse(is.na(edu_hhh) & E197_1 == 1, "Educacion Primaria", edu_hhh))%>%
  mutate(edu_hhh = ifelse(is.na(edu_hhh), "Unknown", edu_hhh))%>%
  select(hh_id, sex_hhh, ethnicity, ind_hhh, age_hhh, edu_hhh)

personas_2 <- personas_0 %>%
  select(NUMERO, everything())%>%
  mutate(adults   = ifelse(E27 > 15,1,0),
         children = ifelse(E27 < 16,1,0))%>%
  group_by(NUMERO)%>%
  summarise(hh_size = n(),
            adults = sum(adults),
            children = sum(children))%>%
  ungroup()%>%
  remove_all_labels()%>%
  zap_formats()%>%
  rename(hh_id = NUMERO)

household_information <- left_join(hogares_2, personas_2)%>%
  left_join(personas_1)%>%
  select(-adults.a, -children.a, -hh_size.a)%>%
  select(hh_id, hh_size, hh_weights, everything())%>%
  arrange(hh_id)
  
Education.Code <- distinct(household_information, edu_hhh)%>%
  rename(Education = edu_hhh)%>%
  mutate(edu_hhh = 1:n())%>%
  write_csv(., "../0_Data/1_Household Data/3_Uruguay/2_Codes/Education.Code.csv")
Gender.Code <- stack(attr(personas_1$sex_hhh, 'labels'))%>%
  rename(sex_hhh = values, Gender = ind)%>%
  write_csv(., "../0_Data/1_Household Data/3_Uruguay/2_Codes/Gender.Code.csv")
Ethnicity.Code <- stack(attr(personas_1$ethnicity, 'labels'))%>%
  rename(ethnicitiy = values, Ethnicity = ind)%>%
  write_csv(., "../0_Data/1_Household Data/3_Uruguay/2_Codes/Ethnicity.Code.csv")
# Industry.Code <- count(personas_1, ind_hhh)%>%
#   arrange(ind_hhh)%>%
#   select(-n)%>%
#   write_csv(., "../0_Data/1_Household Data/3_Uruguay/2_Codes/Industry.Code.csv")

household_information <- household_information %>%
  rename(Education = edu_hhh)%>%
  left_join(Education.Code)%>%
  select(-Education)

# Income Information

inc_1 <- personas_0 %>%
  rename(hh_id = NUMERO)%>%
  select(hh_id, starts_with("G"), E560_1_1, E560_2_1)%>%
  mutate(G257 = ifelse(G152 == 2, G257/2,G257))%>%
  select(hh_id, G257, E560_1_1, E560_2_1, starts_with("G148"))%>%
  mutate_at(vars(-hh_id), list(~ ifelse(is.na(.),0,.)))%>%
  mutate(inc_gov_cash = G148_5_1 + G257,
         inc_gov_monetary = E560_1_1 + E560_2_1 + G148_1_1 + G148_1_2 + G148_1_3 + G148_1_4 + G148_1_5 + G148_1_6 + G148_1_7 + G148_1_8 + G148_1_9 + G148_1_10 + G148_1_12 +
           G148_2_1 + G148_2_2 + G148_2_3 + G148_2_4 + G148_2_5 + G148_2_6 + G148_2_7 + G148_2_8 + G148_2_9 + G148_2_10 + G148_2_12 + G148_3)%>%
  mutate(inc_gov_cash = inc_gov_cash*12,
         inc_gov_monetary = inc_gov_monetary*12)%>%
  group_by(hh_id)%>%
  summarise(inc_gov_cash = sum(inc_gov_cash),
            inc_gov_monetary = sum(inc_gov_monetary))%>%
  ungroup()

household_information <- household_information %>%
  left_join(inc_1)

write_csv(household_information, "../0_Data/1_Household Data/3_Uruguay/1_Data_Clean/household_information_Uruguay.csv")

# Focus on Expenditures

gastos_1 <- gastos_0 %>%
  select(NUMERO, ARTICULOCODIGO, COMOCODIGO, VALORCONTM, VALORCONT_IMPUTADO)%>%
  filter(VALORCONT_IMPUTADO == 0)%>%
  select(-VALORCONT_IMPUTADO)%>%
  rename(hh_id = NUMERO, item_code = ARTICULOCODIGO, expenditures_month = VALORCONTM, expenditure_type = COMOCODIGO)

# Item-Codes

item_codes_original <- read.xlsx("../0_Data/1_Household Data/3_Uruguay/9_Documentation/ENGIH 2016 Codificador CCIF.xlsx")

item_codes <- count(gastos_1, item_code)%>%
  arrange(item_code)%>%
  mutate(item_code_new = 1:n())

item_codes_1 <- item_codes %>%
  left_join(item_codes_original, by = c("item_code" = "CCIF"))%>%
  select(item_code_new, item_code, DESCRIPCION, 'DIVISIÓN.-.NOMBRE')%>%
  rename(item_code_old = item_code, item_code = item_code_new)

#write.xlsx(item_codes_1, "../0_Data/1_Household Data/3_Uruguay/3_Matching_Tables/Item_Code_Description_Uruguay_1.xlsx")

gastos_2 <- gastos_1 %>%
  left_join(item_codes)%>%
  select(hh_id, item_code_new, expenditures_month, expenditure_type)%>%
  arrange(hh_id, item_code_new)%>%
  rename(item_code = item_code_new)%>%
  mutate(expenditure_type_aux = ifelse(expenditure_type %in% c(0,1,2,3,4,5,6,7),1,2))%>%
  mutate(expenditures_year_0  = expenditures_month*12)%>%
  mutate(expenditures_year    = ifelse(expenditure_type_aux == 1 & expenditures_year_0 > 0, expenditures_year_0,0),
         expenditures_sp_year = ifelse(expenditure_type_aux == 2, expenditures_year_0, 0))%>%
  group_by(hh_id, item_code)%>%
  summarise(expenditures_year    = sum(expenditures_year),
            expenditures_sp_year = sum(expenditures_sp_year))%>%
  ungroup()%>%
  filter(expenditures_year > 0 | expenditures_sp_year > 0)

write_csv(gastos_2, "../0_Data/1_Household Data/3_Uruguay/1_Data_Clean/expenditures_items_Uruguay.csv")  

# Appliances

appliances_1 <- hogares_0 %>%
  select(NUMERO, starts_with("D21"))%>%
  rename(heater.01 = D21_1, refrigerator.01 = D21_3, tv.01a = D21_4, tv.01b = D21_5, radio.01 = D21_6,
         washing_machine.01 = D21_10, dryer.01 = D21_11, dishwasher.01 = D21_12,
         microwave.01 = D21_13, ac.01 = D21_14, computer.01 = D21_15,
         car.01 = D21_18, motorcycle.01 = D21_19, hh_id = NUMERO)%>%
  select(hh_id, ends_with("01"), tv.01a, tv.01b)%>%
  mutate_at(vars(-hh_id), list(~ as.numeric(str_replace(., "2","0"))))%>%
  mutate(tv.01 = ifelse(tv.01a == 1 | tv.01b == 1,1,0))%>%
  select(-tv.01a, -tv.01b)

write_csv(appliances_1, "../0_Data/1_Household Data/3_Uruguay/1_Data_Clean/appliances_0_1_Uruguay.csv")
