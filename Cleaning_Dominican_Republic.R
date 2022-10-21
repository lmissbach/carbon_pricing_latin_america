if(!require("pacman")) install.packages("pacman")

p_load("tidyverse", "Hmisc", "haven", "readr", "openxlsx")

# Raw data

data_0_socio <- read.xlsx("../0_Data/1_Household Data/3_Dominican Republic/1_Data_Raw/data_orig/Sociodemograficas_e_ingresos.xlsx")
data_gastos  <- read.xlsx("../0_Data/1_Household Data/3_Dominican Republic/1_Data_Raw/data_orig/Gasto_consumo_final_mensual.xlsx") 

# Household Data

data_1_socio <- data_0_socio %>%
  unite(hh_id, c(VIVIENDA, HOGAR), sep = "_", remove = FALSE)%>%
  rename(hh_weights = FACTOR_EXPANSION,
         province = ID_PROVINCIA, municipality = ID_MUNICIPIO, district = ID_DISTRITO_MUNICIPAL, village = ID_BARRIO_PARAJE,
         water = A213, lighting_fuel = A219, toilet = A221, cooking_fuel = A301)%>%
  mutate(urban_01 = ifelse(ESTRATO == 11 | ESTRATO == 21 | ESTRATO == 31 | ESTRATO == 41,1,0))%>%
  mutate(adults = ifelse(A403 > 15,1,0),
         children = ifelse(A403 < 16,1,0))%>%
  group_by(hh_id)%>%
  mutate(hh_size = n(),
         adults = sum(adults),
         children = sum(children))%>%
  ungroup()%>%
  select(hh_id, hh_weights, hh_size, adults, children, province, municipality, district, village, water, lighting_fuel, toilet, cooking_fuel, urban_01)%>%
  distinct()%>%
  mutate(electricity.access = ifelse(lighting_fuel == 1 | lighting_fuel == 2 | lighting_fuel == 3,1,0))

data_2_socio <- data_0_socio %>%
  unite(hh_id, c(VIVIENDA, HOGAR), sep = "_", remove = FALSE)%>%
  rename(sex_hhh = A402, edu_hhh = A410, age_hhh = A403, ind_hhh = ORDEN_RAMA)%>%
  filter(A404 == 1)%>%
  select(hh_id, sex_hhh, edu_hhh, age_hhh, ind_hhh)

data_3_socio <- data_0_socio %>%
  unite(hh_id, c(VIVIENDA, HOGAR), sep = "_", remove = FALSE)%>%
  select(hh_id, PENSION, REGALIA_PENSION, starts_with("D701"))%>%
  select(hh_id, PENSION, REGALIA_PENSION, ends_with("_MONTO"))%>%
  mutate_at(vars(-hh_id), list(~ ifelse(is.na(.),0,.)))%>%
  mutate(inc_gov_cash     = D701F_MONTO + D701K1_MONTO + D701K2_MONTO + D701K3_MONTO+
           D701K4_MONTO + D701K5_MONTO + D701K6_MONTO + D701K7_MONTO + D701K8_MONTO + D701K9_MONTO + D701K10_MONTO,
         inc_gov_monetary = D701H_MONTO + D701J_MONTO)%>%
  group_by(hh_id)%>%
  summarise(inc_gov_cash     = sum(inc_gov_cash),
            inc_gov_monetary = sum(inc_gov_monetary))%>%
  ungroup()

data_12_socio <- left_join(data_1_socio, data_2_socio)%>%
  left_join(data_3_socio)

write_csv(data_12_socio, "../0_Data/1_Household Data/3_Dominican Republic/1_Data_Clean/household_information_Dominican Republic.csv")

# Expenditure Data

data_1_gastos <- data_gastos %>%
  unite(hh_id, c(VIVIENDA, HOGAR), sep = "_", remove = FALSE)%>%
  select(hh_id, ARTICULO, ID_VARIEDAD, GASTOMENSUAL_TOTAL)%>%
  mutate(expenditures_year = GASTOMENSUAL_TOTAL*12)%>%
  filter(expenditures_year > 0)%>%
  arrange(hh_id, ARTICULO, ID_VARIEDAD)%>%
  separate(ARTICULO, c("item_code", "item_name"), sep = 7)%>%
  mutate(item_code = as.numeric(item_code))%>%
  select(-item_name)

matching <- read.xlsx("../0_Data/1_Household Data/3_Dominican Republic/3_Matching_Tables/Item_GTAP_Concordance_Dominican Republic.xlsx")

matching <- matching %>%
  select (-Explanation) %>%
  pivot_longer(-GTAP, names_to = "drop", values_to = "item_code")%>%
  filter(!is.na(item_code))%>%
  select(GTAP, item_code)%>%
  mutate(GTAP = ifelse(GTAP == "gas" | GTAP == "gdt", "gasgdt", GTAP))

items_gastos <- distinct(data_1_gastos, item_code, ID_VARIEDAD)%>%
  left_join(matching, by = "item_code")%>%
  left_join(rename(matching, GTAP_1 = GTAP), by = c("ID_VARIEDAD" = "item_code"))

data_2_gastos <- data_1_gastos %>%
  left_join(matching, by = "item_code")%>%
  left_join(rename(matching, GTAP_1 = GTAP), by = c("ID_VARIEDAD" = "item_code"))%>%
  mutate(item_code = ifelse(!is.na(GTAP_1), ID_VARIEDAD, item_code))%>%
  select(hh_id, item_code, expenditures_year)%>%
  group_by(hh_id, item_code)%>%
  summarise(expenditures_year = sum(expenditures_year))%>%
  ungroup()

write_csv(data_2_gastos, "../0_Data/1_Household Data/3_Dominican Republic/1_Data_Clean/expenditures_items_Dominican Republic.csv")

# Appliances

data_1_appliances <- data_0_socio %>%
  unite(hh_id, c(VIVIENDA, HOGAR), sep = "_", remove = FALSE)%>%
  select(hh_id, starts_with("A309"), starts_with("A310"))%>%
  distinct()

data_1_appliances[is.na(data_1_appliances)] <- 0

data_2_appliances <- data_1_appliances %>%
  mutate(car.01 = ifelse(A309_AUTOS_EXC + A309_AUTOS_NEG + A309_AUTOS_COMP + A309_AUTOS_ASIG > 0,1,0),
         motorcycle.01 = ifelse(A309_MOTOS_ASIG + A309_MOTOS_COMP + A309_MOTOS_EXC + A309_MOTOS_NEG > 0,1,0))%>%
  select(-starts_with("A309"))%>%
  select(hh_id, car.01, motorcycle.01, everything())%>%
  rename(tv.01 = A310B, stove.01 = A310D, microwave.01 = A310E, refrigerator.01 = A310F,
         washing_machine.01 = A310G, ac.01 = A310H, fan.01 = A310I, heater.01 = A310J,
         radio.01 = A310L, computer.01a = A310R, computer.01b = A310S, vacuum.01 = A310M)%>%
  mutate(tv.01              = ifelse(tv.01 == 2,0,tv.01),
         stove.01           = ifelse(stove.01 == 2,0,stove.01),
         microwave.01       = ifelse(microwave.01 == 2,0,microwave.01),
         refrigerator.01    = ifelse(refrigerator.01 == 2,0,refrigerator.01),
         washing_machine.01 = ifelse(washing_machine.01 == 2,0, washing_machine.01),
         ac.01              = ifelse(ac.01 == 2,0, ac.01),
         fan.01             = ifelse(fan.01 == 2,0, fan.01),
         heater.01          = ifelse(heater.01 == 2,0, heater.01),
         radio.01           = ifelse(radio.01 == 2,0, radio.01),
         computer.01        = ifelse(computer.01a == 1 | computer.01b == 1,1,0),
         vacuum.01          = ifelse(vacuum.01 == 2,0, vacuum.01))%>%
  select(hh_id, ends_with(".01"))

write_csv(data_2_appliances, "../0_Data/1_Household Data/3_Dominican_Republic/1_Data_Clean/appliances_0_1_Dominican_Republic.csv")


#Codes
Province.Code <- distinct(data_0_socio, ID_PROVINCIA, DES_PROVINCIA)%>%
  arrange(ID_PROVINCIA)%>%
  rename(province = ID_PROVINCIA)

Municipality.Code <- distinct(data_0_socio, ID_MUNICIPIO, DES_MUNICIPIO)%>%
  arrange(ID_MUNICIPIO)%>%
  rename(municipality = ID_MUNICIPIO)

District.Code <- distinct(data_0_socio, ID_DISTRITO_MUNICIPAL, DES_DISTRITO_MUNICIPAL)%>%
  arrange(ID_DISTRITO_MUNICIPAL)%>%
  rename(district = ID_DISTRITO_MUNICIPAL)

Village.Code <- distinct(data_0_socio, ID_BARRIO_PARAJE, DES_BARRIO_PARAJE)%>%
  arrange(ID_BARRIO_PARAJE)%>%
  rename(village = ID_BARRIO_PARAJE)

Gender.Code <- distinct(data_2_socio, sex_hhh)%>%
  arrange(sex_hhh)%>%
  bind_cols(Gender = c("Male", "Female"))

Education.Code <- distinct(data_2_socio, edu_hhh)%>%
  arrange(edu_hhh)%>%
  bind_cols(Education = c("Pre-escolar", "Primario", "Secundario", "Secundario - technico", "Universitario", "Post-Grado", "Maesteria", "Ninguno"))

Industry.Code <- distinct(data_2_socio, ind_hhh)%>%
  arrange(ind_hhh)%>%
  bind_cols(Industry = c("Agriculture and Livestock", "Manufacturing", "Electricity and Water", "Construction", "Commerce",
                         "Hotels and Restaurants", "Transport and Communication", "Financial Intermediaries", "Public administration and defense",
                         "Education", "Health and Social Assistance", "Other services", "None"))

Toilet.Code <- distinct(data_12_socio, toilet)%>%
  arrange(toilet)%>%
  bind_cols(Toilet = c("Inside and private", "Inside and shared", "Private latrine", "Shared latrine", "No toilet"))

Water.Code <- distinct(data_12_socio, water)%>%
  arrange(water)%>%
  bind_cols(Water = c("Aqueduct", "Tanker truck", "Water truck", "Bottle", "River or canal", "Tubular well", "Rain", "Other"))

Lighting.Code <- distinct(data_12_socio, lighting_fuel)%>%
  arrange(lighting_fuel)%>%
  bind_cols(Lighting = c("Electricity (public)", "Electricity (private)", "Power Plant", "Kerosene", "Gas (Propan)", "Solar Panel", "Candle", "Others"))

Cooking.Code <- distinct(data_12_socio, cooking_fuel)%>%
  arrange(cooking_fuel)%>%
  bind_cols(Cooking = c("LPG", "Kerosene", "Electricity", "Firewood", "Coal", "Does not cook", "Other"))

write_csv(Province.Code, "../0_Data/1_Household Data/3_Dominican_Republic/2_Codes/Province.Code.csv")
write_csv(Municipality.Code, "../0_Data/1_Household Data/3_Dominican_Republic/2_Codes/Municipality.Code.csv")
write_csv(District.Code, "../0_Data/1_Household Data/3_Dominican_Republic/2_Codes/District.Code.csv")
write_csv(Village.Code, "../0_Data/1_Household Data/3_Dominican_Republic/2_Codes/Village.Code.csv")
write_csv(Gender.Code, "../0_Data/1_Household Data/3_Dominican_Republic/2_Codes/Gender.Code.csv")
write_csv(Education.Code, "../0_Data/1_Household Data/3_Dominican_Republic/2_Codes/Education.Code.csv")
write_csv(Industry.Code, "../0_Data/1_Household Data/3_Dominican_Republic/2_Codes/Industry.Code.csv")
write_csv(Toilet.Code, "../0_Data/1_Household Data/3_Dominican_Republic/2_Codes/Toilet.Code.csv")
write_csv(Water.Code, "../0_Data/1_Household Data/3_Dominican_Republic/2_Codes/Water.Code.csv")
write_csv(Lighting.Code, "../0_Data/1_Household Data/3_Dominican_Republic/2_Codes/Lighting.Code.csv")
write_csv(Cooking.Code, "../0_Data/1_Household Data/3_Dominican_Republic/2_Codes/Cooking.Code.csv")

# Item Codes

item_codes <- distinct(data_gastos, ARTICULO)%>%
  separate(ARTICULO, c("item_code", "item_name"), sep = 7)%>%
  mutate(item_code = as.numeric(item_code))%>%
  arrange(item_code)

item_codes_0 <- read.xlsx("../0_Data/1_Household Data/3_Dominican_Republic/1_Data_Raw/data_orig/Gasto_periodicidad/Registros_Cuestionario_E.xlsx", sheet = "Catálogo variedades")

item_codes_1 <- item_codes_0 %>%
  select(ID_ARTICULO, DES_ARTICULO)%>%
  distinct()

item_codes_2 <- left_join(item_codes, item_codes_1, by = c("item_code" = "ID_ARTICULO"))

item_codes_3 <- item_codes_0 %>%
  select(ID_VARIEDAD, DES_VARIEDAD, COD_ARTICULO, DES_ARTICULO)%>%
  distinct()%>%
  arrange(COD_ARTICULO, ID_VARIEDAD)
write.xlsx(item_codes_2, "../0_Data/1_Household Data/3_Dominican_Republic/2_Codes/Item_Codes_Dominican_Republic.xlsx")
write.xlsx(item_codes_3, "../0_Data/1_Household Data/3_Dominican_Republic/2_Codes/Item_Codes_Additional_Dominican_Republic.xlsx")
