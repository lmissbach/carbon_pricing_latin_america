if(!require("pacman")) install.packages("pacman")

p_load("haven", "Hmisc", "openxlsx", "rattle", "scales", "tidyverse")

# Load Data ####

alimentos_detalle      <- read_sav("../0_Data/1_Household Data/3_Guatemala/1_Data_Raw/Data Guatemala/Alimentos Detalle.sav")
personas               <- read_sav("../0_Data/1_Household Data/3_Guatemala/1_Data_Raw/Data Guatemala/Personas.sav")
equipamiento           <- read_sav("../0_Data/1_Household Data/3_Guatemala/1_Data_Raw/Data Guatemala/Equipamiento.sav")
otros_gastos_12        <- read_sav("../0_Data/1_Household Data/3_Guatemala/1_Data_Raw/Data Guatemala/Otros Gastos 12 Meses.sav")
otros_gastos_mes       <- read_sav("../0_Data/1_Household Data/3_Guatemala/1_Data_Raw/Data Guatemala/Otros Gastos Mes Pasado.sav")
hogares                <- read_sav("../0_Data/1_Household Data/3_Guatemala/1_Data_Raw/Data Guatemala/Hogares.sav")
programas_sociales     <- read_sav("../0_Data/1_Household Data/3_Guatemala/1_Data_Raw/Data Guatemala/Programas Sociales.sav")
seguridad_ciudana      <- read_sav("../0_Data/1_Household Data/3_Guatemala/1_Data_Raw/Data Guatemala/Seguridad Ciudana.sav")
otros_gastos_sema      <- read_sav("../0_Data/1_Household Data/3_Guatemala/1_Data_Raw/Data Guatemala/Otros Gastos Semana Pasada.sav")
equipo_agro            <- read_sav("../0_Data/1_Household Data/3_Guatemala/1_Data_Raw/Data Guatemala/Equipo Agro.sav")
instalaciones          <- read_sav("../0_Data/1_Household Data/3_Guatemala/1_Data_Raw/Data Guatemala/Instalaciones.sav")
donaciones             <- read_sav("../0_Data/1_Household Data/3_Guatemala/1_Data_Raw/Data Guatemala/Donaciones.sav")
instrumentos           <- read_sav("../0_Data/1_Household Data/3_Guatemala/1_Data_Raw/Data Guatemala/Instrumentos.sav")
agrega_consumo         <- read_sav("../0_Data/1_Household Data/3_Guatemala/1_Data_Raw/Data Guatemala/Agrega de Consumo.sav")
asistencia_technica    <- read_sav("../0_Data/1_Household Data/3_Guatemala/1_Data_Raw/Data Guatemala/Asistencia Technica.sav")
produccion_detalle     <- read_sav("../0_Data/1_Household Data/3_Guatemala/1_Data_Raw/Data Guatemala/Produccion Detalle.sav")
existencia_pecua_d     <- read_sav("../0_Data/1_Household Data/3_Guatemala/1_Data_Raw/Data Guatemala/Existencia pecuarios Detalle.sav")
productos_animal_d     <- read_sav("../0_Data/1_Household Data/3_Guatemala/1_Data_Raw/Data Guatemala/Productos animales Detalle.sav")
Datos_unidad_prod      <- read_sav("../0_Data/1_Household Data/3_Guatemala/1_Data_Raw/Data Guatemala/Datos de la unidad de la produccion.sav")
compras_credito        <- read_sav("../0_Data/1_Household Data/3_Guatemala/1_Data_Raw/Data Guatemala/Compras al Credito.sav")
prestamos              <- read_sav("../0_Data/1_Household Data/3_Guatemala/1_Data_Raw/Data Guatemala/Prestamos.sav")
negocios_detalle       <- read_sav("../0_Data/1_Household Data/3_Guatemala/1_Data_Raw/Data Guatemala/Negocios Detalle.sav")
negocios               <- read_sav("../0_Data/1_Household Data/3_Guatemala/1_Data_Raw/Data Guatemala/Negocios.sav")
prestamos_detalle      <- read_sav("../0_Data/1_Household Data/3_Guatemala/1_Data_Raw/Data Guatemala/Prestamos Detalle.sav")
existencia_pecuarios   <- read_sav("../0_Data/1_Household Data/3_Guatemala/1_Data_Raw/Data Guatemala/Existencia pecuarios.sav")
produccion_forestal    <- read_sav("../0_Data/1_Household Data/3_Guatemala/1_Data_Raw/Data Guatemala/Produccion forestal.sav")
produccion_cultivos    <- read_sav("../0_Data/1_Household Data/3_Guatemala/1_Data_Raw/Data Guatemala/Produccion de cultivos.sav")
compras_credito_d      <- read_sav("../0_Data/1_Household Data/3_Guatemala/1_Data_Raw/Data Guatemala/Compras al Credito Detalle.sav")
productos_animal       <- read_sav("../0_Data/1_Household Data/3_Guatemala/1_Data_Raw/Data Guatemala/Productos animales.sav")
produccion_forestal_d  <- read_sav("../0_Data/1_Household Data/3_Guatemala/1_Data_Raw/Data Guatemala/Produccion forestal detalle.sav")
alimentos_supermercado <- read_sav("../0_Data/1_Household Data/3_Guatemala/1_Data_Raw/Data Guatemala/Alimentos Supermercado.sav")

# Transform Data ####
# Household Data ####

hogares_0 <- hogares %>%
  rename(hh_id = NUMHOG, province = REGION, district = DEPTO, Urban = AREA, hh_size = THOGAR, hh_weights = FACTOR,
         language = PPD06, electricity = P01A05C, water = P01D06, toilet = P01D17)%>%
  mutate(urban_01 = ifelse(Urban == 1,1,0),
         electricity.access = ifelse(electricity == 1,1,0))%>%
  select(hh_id, province, district, urban_01, hh_size, hh_weights, language, electricity.access, water, toilet)

energy <- hogares %>%
  rename(hh_id = NUMHOG, firewood = P01D27)%>%
  select(hh_id, firewood, starts_with("P01E01"))%>%
  rename(candles = P01E01_1, Kerosene = P01E01_2, LPG = P01E01_3, Coal = P01E01_4, Battery = P01E01_5, 
         Electricity = P01E01_6, Solar = P01E01_7, Firewood = P01E01_8, Other = P01E01_9)

energy_01 <- energy %>%
  select(hh_id, candles, Battery, Electricity, Solar, Other)%>%
  pivot_longer(-hh_id, names_to = "Source", values_to = "Value")%>%
  mutate(Value = ifelse(Value == 2,0,Value))%>%
  pivot_wider(names_from = "Source", values_from = "Value")%>%
  mutate(Lighting_Fuel = ifelse(Electricity == 1, "Electricity", 
                                ifelse(Solar == 1, "Solar",
                                       ifelse(Battery == 1, "Battery",
                                              ifelse(candles == 1, "Candles",
                                                     ifelse(Other == 1, "Other", NA))))))

Lighting.Code <- distinct(energy_01, Lighting_Fuel)%>%
  mutate(lighting_fuel = 1:n())

energy_01 <- energy_01 %>%
  left_join(Lighting.Code)%>%
  select(hh_id, lighting_fuel)

write_csv(Lighting.Code, "../0_Data/1_Household Data/3_Guatemala/2_Codes/Lighting.Code.csv")

energy_02 <- energy %>%
  select(hh_id, Kerosene, LPG, Coal, Firewood, firewood)%>%
  pivot_longer(-hh_id, names_to = "Source", values_to = "Value")%>%
  mutate(Value = ifelse(Value == 2,0,Value))%>%
  pivot_wider(names_from = "Source", values_from = "Value")%>%
  mutate(firewood = ifelse(is.na(firewood),0, firewood))%>%
  mutate(firewood = ifelse(firewood == 1 | Firewood == 1, 1, firewood),
         Firewood = ifelse(Firewood == 1 | firewood == 1, 1, Firewood))%>%
  mutate(Cooking_Fuel = ifelse(LPG == 1 & Firewood == 0 & firewood == 0 & Coal == 0 & Kerosene == 0, "LPG",
                               ifelse(LPG == 0 & (Firewood == 1 | firewood == 1) & Coal == 0 & Kerosene == 0, "Firewood",
                                      ifelse(LPG == 0 & Firewood == 0 & firewood == 0 & Coal == 1 & Kerosene == 0, "Coal",
                                             ifelse(LPG == 0 & Firewood == 0 & firewood == 0 & Coal == 0 & Kerosene == 1, "Kerosene",
                                                    ifelse(LPG == 1 & (Firewood == 1 | firewood == 1) & Coal == 0 & Kerosene == 0, "Firewood & LPG", 
                                                           ifelse((Firewood == 1 | firewood == 1) & Kerosene == 1 & Coal == 0 & LPG == 0, "Firewood & Kerosene", 
                                                                  ifelse((Firewood == 1 | firewood == 1) & Coal == 1 & Kerosene == 0 & LPG == 0, "Firewood & Coal",
                                                                         ifelse(Firewood == 0 & firewood == 0 & Coal == 0 & Kerosene == 0 & LPG == 0, "No Fuel",
                                                                                ifelse(Firewood == 0 & firewood == 0 & LPG == 1 & Coal == 1 & Kerosene == 0, "LPG & Coal",
                                                                                       ifelse(Firewood == 1 & firewood == 1 & LPG == 1 & Coal == 1, "Firewood, LPG, Coal","Other")))))))))))


Cooking.Code <- distinct(energy_02, Cooking_Fuel)%>%
  mutate(cooking_fuel = 1:n())

energy_02 <- energy_02 %>%
  left_join(Cooking.Code)%>%
  select(hh_id, cooking_fuel)

write_csv(Cooking.Code, "../0_Data/1_Household Data/3_Guatemala/2_Codes/Cooking.Code.csv")

personas_0 <- personas %>%
  rename(hh_id = NUMHOG, sex_hhh = PPA02, age_hhh = PPA03, language.b = P04A07A, ethnicity = P04A11A,
         edu_hhh = P06B25A, ind_hhh = P10B03B)%>%
  filter(PPA05 == 1)%>%
  select(hh_id, sex_hhh, age_hhh, language.b, ethnicity, edu_hhh, ind_hhh)

personas_1 <- personas %>%
  rename(hh_id = NUMHOG, age_hhh = PPA03)%>%
  mutate(adults   = ifelse(age_hhh > 15,1,0),
         children = ifelse(age_hhh < 16,1,0))%>%
  group_by(hh_id)%>%
  summarise(adults   = sum(adults),
            children = sum(children))%>%
  ungroup()

# Income Information

inc_1 <- programas_sociales %>%
  rename(hh_id = NUMHOG)%>%
  select(hh_id, 
         P03C03B, P03C04,
         P03C08B, P03C09,
         P03C13B, P03C14)%>%
  mutate(P03C03 = P03C03B*P03C04,
         P03C08 = P03C08B*P03C09,
         P03C13 = P03C13B*P03C14)%>%
  select(hh_id, P03C03, P03C08, P03C13)%>%
  mutate_at(vars(-hh_id), list(~ ifelse(is.na(.),0,.)))%>%
  mutate(inc_gov_cash = P03C03 + P03C08 + P03C13,
         inc_gov_monetary = 0)%>%
  group_by(hh_id)%>%
  summarise(inc_gov_cash = sum(inc_gov_cash),
            inc_gov_monetary = sum(inc_gov_monetary))%>%
  ungroup()

inc_2 <- personas %>%
  rename(hh_id = NUMHOG)%>%
  select(hh_id, starts_with("P11"))%>%
  select(hh_id, ends_with("B"))%>%
  mutate_at(vars(-hh_id), list(~ ifelse(is.na(.),0,.)))%>%
  mutate(inc_gov_cash     = P11A07B,
         inc_gov_monetary = P11A04B + P11A08B + P11B04B)%>%
  mutate(inc_gov_cash = inc_gov_cash*4,
         inc_gov_monetary = inc_gov_monetary*4)%>%
  group_by(hh_id)%>%
  summarise(inc_gov_cash     = sum(inc_gov_cash),
            inc_gov_monetary = sum(inc_gov_monetary))%>%
  ungroup()

inc_3 <- bind_rows(inc_1, inc_2)%>%
  group_by(hh_id)%>%
  summarise(inc_gov_cash = sum(inc_gov_cash),
            inc_gov_monetary = sum(inc_gov_monetary))%>%
  ungroup()

hogares_1 <- hogares_0 %>%
  left_join(energy_02)%>%
  left_join(energy_01)%>%
  left_join(personas_0)%>%
  left_join(personas_1)%>%
  left_join(inc_3)

write_csv(hogares_1, "../0_Data/1_Household Data/3_Guatemala/1_Data_Clean/household_information_Guatemala.csv")

# Expenditure Data ####

# Some expenditure data might be listed in hogares questionnaire / also on energy expenditures

alimentos_detalle_1 <- alimentos_detalle %>%
  rename(hh_id = NUMHOG, item_code = P12A03A, expenditures = P12A05, expenditure_type = P12A07)%>%
  select(hh_id, item_code, expenditures, P12A04, expenditure_type)%>%
  filter(!is.na(expenditures) & expenditures > 0)%>%
  mutate(expenditures_year    = ifelse(expenditure_type == 2, expenditures*P12A04,0),
         expenditures_sp_year = ifelse(expenditure_type == 1, expenditures*P12A04,0))%>%
  select(hh_id, item_code, expenditures_year, expenditures_sp_year)%>%
  left_join(select(Item.Codes.1, item_code, item_code_new), by = c("item_code"))%>%
  select(-item_code)%>%
  rename(item_code = item_code_new)

otros_gastos_12_1 <- otros_gastos_12 %>%
  rename(hh_id = NUMHOG, item_code = ID_GASTOS12M, expenditures_year = P12B08)%>%
  select(hh_id, item_code, expenditures_year)%>%
  filter(!is.na(expenditures_year))%>%
  left_join(select(Item.Codes.2, item_code, item_code_new), by = c("item_code"))%>%
  select(-item_code)%>%
  rename(item_code = item_code_new)

otros_gastos_mes_1 <- otros_gastos_mes %>%
  rename(hh_id = NUMHOG, item_code = ID_GASTOSMP, expenditures = P12B06)%>%
  mutate(expenditures_year = expenditures*12)%>%
  filter(!is.na(expenditures_year))%>%
  select(hh_id, item_code, expenditures_year)%>%
  left_join(select(Item.Codes.3, item_code, item_code_new), by = c("item_code"))%>%
  select(-item_code)%>%
  rename(item_code = item_code_new)

otros_gastos_sema_1 <- otros_gastos_sema %>%
  rename(hh_id = NUMHOG, item_code = ID_GASTOSP, expenditures = P12B04)%>%
  mutate(expenditures_year = expenditures*52)%>%
  filter(!is.na(expenditures_year))%>%
  select(hh_id, item_code, expenditures_year)%>%
  left_join(select(Item.Codes.4, item_code, item_code_new), by = c("item_code"))%>%
  select(-item_code)%>%
  rename(item_code = item_code_new)

exp_energy <- hogares %>%
  rename(hh_id = NUMHOG)%>%
  select(hh_id, starts_with("P01E03"), P01D09, P01D15B, P01D22B, P01B03)%>%
  pivot_longer(-hh_id, names_to = "item_code", values_to = "expenditures_year")%>%
  filter(!is.na(expenditures_year))%>%
  mutate(expenditures_year = expenditures_year*12)%>%
  arrange(hh_id, item_code)

exp_health <- personas %>%
  rename(hh_id = NUMHOG)%>%
  select(hh_id, starts_with("P05D01"), starts_with("P05C09"), P05D11B, starts_with("P05D18"),
         P05D11B, P05D12B, P05D13B, P05D14B, P05D15B, P05D16B, P05E21B,
         P06A03B, P06A04B, P06A05B, P06A06B, P06A07B, P06A08B, P06A09B,
         P06B12B, P06B13B, P06B14B, P06B16B, P06B18B, P06B20B,
         P06B21B)%>%
  pivot_longer(-hh_id, names_to = "item_code", values_to = "expenditures_year")%>%
  filter(!is.na(expenditures_year))%>%
  mutate(expenditures_year = ifelse(!item_code %in% c("P06A03B", "P06A04B", "P06A05B", "P06A06B", "P06A07B", "P06A08B", "P06A09B",
                                                      "P06B12B", "P06B13B", "P06B14B", "P06B16B", "P06B18B", "P06B20B",
                                                      "P06B21B"), expenditures_year*12, expenditures_year))%>%
  arrange(hh_id, item_code)

expenditures_whole <- bind_rows(alimentos_detalle_1, otros_gastos_12_1, otros_gastos_mes_1, otros_gastos_sema_1)%>%
  bind_rows(exp_energy)%>%
  bind_rows(exp_health)%>%
  arrange(hh_id, item_code)

write_csv(expenditures_whole, "../0_Data/1_Household Data/3_Guatemala/1_Data_Clean/expenditures_items_Guatemala.csv")

expenditures_whole <- read_csv("../0_Data/1_Household Data/3_Guatemala/1_Data_Clean/expenditures_items_Guatemala.csv")%>%
  group_by(hh_id, item_code)%>%
  summarise(expenditures_year    = sum(expenditures_year),
            expenditures_sp_year = sum(expenditures_sp_year))%>%
  ungroup()

write_csv(expenditures_whole, "../0_Data/1_Household Data/3_Guatemala/1_Data_Clean/expenditures_items_Guatemala.csv")


# Expenditures on Health and Education from Personas?

# Appliance Data ####

equipamiento_1 <- equipamiento %>%
  rename(hh_id = NUMHOG)%>%
  select(hh_id, ID_EQUIPA, P01I01A)%>%
  mutate(P01I01A = ifelse(P01I01A == 9, NA, 
                          ifelse(P01I01A == 2,0,P01I01A)))%>%
  pivot_wider(names_from = ID_EQUIPA, values_from = P01I01A)%>%
  rename(stove.01 = '1', microwave.01 = '2', refrigerator.01 = '3', computer.a.01 = '13', computer.b.01 = '14',
         radio.01 = '18', tv.01 = '22', iron.01 = '31', washing_machine.01 = '33', dryer.01 = '34', fan.01 = '35',
         vacuum.01 = '36', heater.a.01 = '40', heater.b.01 = '41', car.a.01 = '43', car.b.01 = '44', car.c.01 = '45',
         motorcycle.01 = '46')%>%
  select(hh_id, ends_with(".01"))%>%
  mutate(heater.01   = ifelse(heater.a.01 == 1| heater.b.01 == 1,1,0),
         car.01      = ifelse(car.a.01 == 1 | car.b.01 == 1 | car.c.01 == 1,1,0),
         computer.01 = ifelse(computer.a.01 == 1 | computer.b.01 == 1,1,0))%>%
  select(-heater.a.01, -heater.b.01, -car.a.01, -car.b.01, -car.c.01, -computer.a.01, -computer.b.01)

write_csv(equipamiento_1, "../0_Data/1_Household Data/3_Guatemala/1_Data_Clean/appliances_0_1_Guatemala.csv")

# Codes ####

Gender.Code <- stack(attr(hogares_1$sex_hhh, 'labels'))%>%
  rename(sex_hhh = values, Gender = ind)%>%
  write_csv(., "../0_Data/1_Household Data/3_Guatemala/2_Codes/Gender.Code.csv")
Province.Code <- stack(attr(hogares_1$province, 'labels'))%>%
  rename(province = values, Province = ind)%>%
  write_csv(., "../0_Data/1_Household Data/3_Guatemala/2_Codes/Province.Code.csv")
District.Code <- stack(attr(hogares_1$district, 'labels'))%>%
  rename(district = values, District = ind)%>%
  write_csv(., "../0_Data/1_Household Data/3_Guatemala/2_Codes/District.Code.csv")
Education.Code <- stack(attr(hogares_1$edu_hhh, 'labels'))%>%
  rename(edu_hhh = values, Education = ind)%>%
  write_csv(., "../0_Data/1_Household Data/3_Guatemala/2_Codes/Education.Code.csv")
Industry.Code <- stack(attr(hogares_1$ind_hhh, 'labels'))%>%
  rename(ind_hhh = values, Industry = ind)%>%
  write_csv(., "../0_Data/1_Household Data/3_Guatemala/2_Codes/Industry.Code.csv")
Ethnicity.Code <- stack(attr(hogares_1$ethnicity, 'labels'))%>%
  rename(ethnicity = values, Ethnicity = ind)%>%
  write_csv(., "../0_Data/1_Household Data/3_Guatemala/2_Codes/Ethnicity.Code.csv")
Water.Code <- stack(attr(hogares_1$water, 'labels'))%>%
  rename(water = values, Water = ind)%>%
  write_csv(., "../0_Data/1_Household Data/3_Guatemala/2_Codes/Water.Code.csv")
Toilet.Code <- stack(attr(hogares_1$toilet, 'labels'))%>%
  rename(toilet = values, Toilet = ind)%>%
  write_csv(., "../0_Data/1_Household Data/3_Guatemala/2_Codes/Toilet.Code.csv")
Language.Code <- stack(attr(hogares_1$language, 'labels'))%>%
  rename(language = values, Language = ind)%>%
  write_csv(., "../0_Data/1_Household Data/3_Guatemala/2_Codes/Language.Code.csv")
Language.B.Code <- stack(attr(hogares_1$language.b, 'labels'))%>%
  rename(language.b = values, Language.B = ind)%>%
  write_csv(., "../0_Data/1_Household Data/3_Guatemala/2_Codes/Language.B.Code.csv")

# Item-Codes ####

Item.Codes.1 <- stack(attr(alimentos_detalle_1$item_code, 'labels'))%>%
  rename(item_code = values, item_name = ind)%>%
  mutate(item_code_new = paste0(10,item_code))
Item.Codes.2 <- stack(attr(otros_gastos_12_1$item_code, 'labels'))%>%
  rename(item_code = values, item_name = ind)%>%
  mutate(item_code_new = paste0(20,item_code))
Item.Codes.3 <- stack(attr(otros_gastos_mes_1$item_code, 'labels'))%>%
  rename(item_code = values, item_name = ind)%>%
  mutate(item_code_new = paste0(30,item_code))
Item.Codes.4 <- stack(attr(otros_gastos_sema_1$item_code, 'labels'))%>%
  rename(item_code = values, item_name = ind)%>%
  mutate(item_code_new = paste0(40,item_code))
Item.Codes.5 <- distinct(exp_energy, item_code)%>%
  arrange(item_code)
Item.Codes.6 <- distinct(exp_health, item_code)%>%
  arrange(item_code)
Item.Codes.all <- bind_rows(Item.Codes.1, Item.Codes.2, Item.Codes.3, Item.Codes.4)%>%
  mutate(item_code = as.character(item_code))%>%
  bind_rows(Item.Codes.5)%>%
  bind_rows(Item.Codes.6)%>%
  mutate(item_code = ifelse(!is.na(item_name), item_code_new, item_code))

write.xlsx(select(Item.Codes.all, item_code, item_name), "../0_Data/1_Household Data/3_Guatemala/3_Matching_Tables/Item_Code_Description_Guatemala.xlsx")
