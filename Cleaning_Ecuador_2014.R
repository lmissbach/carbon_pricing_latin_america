if(!require("pacman")) install.packages("pacman")

p_load("haven", "Hmisc", "openxlsx", "rattle", "scales", "tidyverse")

# Load Data ####

path_0 <- "R:/MSA/datasets/Household Microdata/Ecuador/Dataset_2014/DATOS_ABIERTOS/BASES/" # path with raw data

data_01 <- read.csv(paste0(path_0, "01_ecv6r_vivienda.csv"),           encoding = "UTF-8", stringsAsFactors = FALSE)
data_02 <- read.csv(paste0(path_0, "02_ecv6r_personas.csv"),           encoding = "UTF-8", stringsAsFactors = FALSE)
data_04 <- read.csv(paste0(path_0, "04_ecv6r_gastos_alimentos.csv"),   encoding = "UTF-8", stringsAsFactors = FALSE)
data_05 <- read.csv(paste0(path_0, "05_ecv6r_otros_gastos.csv"),       encoding = "UTF-8", stringsAsFactors = FALSE)
data_06 <- read.csv(paste0(path_0, "06_ecv6r_otros_ingresos.csv"),     encoding = "UTF-8", stringsAsFactors = FALSE)
data_11 <- read.csv(paste0(path_0, "11_ecv6r_equipamiento.csv"),       encoding = "UTF-8", stringsAsFactors = FALSE)
data_25 <- read.csv(paste0(path_0, "25_base_de_trabajo_hogares.csv"),  encoding = "UTF-8", stringsAsFactors = FALSE)
data_26 <- read.csv(paste0(path_0, "26_base_de_trabajo_personas.csv"), encoding = "UTF-8", stringsAsFactors = FALSE)

# Transform data ####

data_01.1 <- data_01 %>%
  rename(hh_id = IDENTIF_HOG, Cooking_Fuel = VI13, Toilet = VI14, Water = VI17, Lighting_Fuel = VI26, hh_weights = FEXP, District = PROVINCIA,
         Province = CIUDAD_AUTO, Village = CIUDAD)%>%
  mutate(electricity.access = ifelse(VI27 == "Si, de uso común para varias viviendas" | VI27 == "Si, de uso exclusivo para la vivienda",1,0))%>%
  mutate(urban_01 = ifelse(AREA_5000 == "Urbano",1,0))%>%
  select(hh_id, hh_weights, urban_01, Province, District, Village, electricity.access,
         Cooking_Fuel, Lighting_Fuel, Water, Toilet)%>%
  mutate(Cooking_Fuel = ifelse(Cooking_Fuel == "", "Otro, cual", Cooking_Fuel))

data_02.1 <- data_02 %>%
  rename(hh_id = IDENTIF_HOG, Edu_hhh = PE47, Sex_hhh = SEXO, ind_hhh = PA16, Ethnicity = PD18)%>%
  filter(PD04 == "Jefe")%>%
  select(hh_id, Edu_hhh, Sex_hhh, ind_hhh, Ethnicity)
    
data_02.2 <- data_02 %>%
  rename(hh_id = IDENTIF_HOG)%>%
  mutate(age = as.numeric(EDAD))%>%
  mutate(adults   = ifelse(age > 15 | is.na(age) ,1,0),
         children = ifelse(age < 16 & !is.na(age),1,0))%>%
  group_by(hh_id)%>%
  summarise(hh_size  = n(),
            adults   = sum(adults),
            children = sum(children))%>%
  ungroup()

data_02.3 <- data_02 %>%
  rename(hh_id = IDENTIF_HOG)%>%
  select(hh_id, PA92, starts_with("PA85"))%>%
  mutate(PA85B = ifelse(PA85A == "Instituciones Nacionales", PA85B,0),
         PA92 = ifelse(is.na(PA92),0,PA92))%>%
  select(hh_id, PA92, PA85B)%>%
  mutate(cash = PA92 + PA85B)%>%
  group_by(hh_id)%>%
  summarise(inc_gov_cash     = sum(cash),
            inc_gov_monetary = 0)%>%
  ungroup()

data_06.1 <- data_06 %>%
  rename(hh_id = IDENTIF_HOG)%>%
  select(hh_id, starts_with("IB"))%>%
  select(hh_id, ends_with("02"), IB0104)%>%
  mutate(IB0102 = as.numeric(IB0102))%>%
  mutate_at(vars(-hh_id), list(~ ifelse(is.na(.)|. == "",0,.)))%>%
  mutate(inc_gov_cash     = IB0302 + IB0104,
         inc_gov_monetary = IB0102 + IB0202)%>%
  select(hh_id, inc_gov_cash, inc_gov_monetary)

data_02.3_06.1 <- bind_rows(data_02.3, data_06.1)%>%
  group_by(hh_id)%>%
  summarise(inc_gov_cash     = sum(inc_gov_cash),
            inc_gov_monetary = sum(inc_gov_monetary))%>%
  ungroup()

# Codes ####

Province.Code <- distinct(data_01, CIUDAD_AUTO)%>%
  arrange(CIUDAD_AUTO)%>%
  rename(Province = CIUDAD_AUTO)%>%
  mutate(province = 1:n())%>%
  write_csv(., "carbon_pricing_latin_america/0_Data/1_Household Data/3_Ecuador/2_Codes/Province.Code.csv")

District.Code <- distinct(data_01, PROVINCIA)%>%
  arrange(PROVINCIA)%>%
  rename(District = PROVINCIA)%>%
  mutate(district = 1:n())%>%
  write_csv(., "carbon_pricing_latin_america/0_Data/1_Household Data/3_Ecuador/2_Codes/District.Code.csv")

Village.Code <- distinct(data_01, CIUDAD)%>%
  arrange(CIUDAD)%>%
  rename(Village = CIUDAD)%>%
  mutate(village = 1:n())%>%
  write_csv(., "carbon_pricing_latin_america/0_Data/1_Household Data/3_Ecuador/2_Codes/Village.Code.csv")

Cooking.Code <- distinct(data_01, VI13)%>%
  arrange(VI13)%>%
  rename(Cooking_Fuel = VI13)%>%
  mutate(Cooking_Fuel = ifelse(Cooking_Fuel == "Leña / carbón", "Lena / carbon",Cooking_Fuel))%>%
  mutate(cooking_fuel = 0:(n()-1))%>%
  mutate(CF = c(NA, "Electricity", "LPG", "Firewood", "Other"))%>%
  write_csv(., "carbon_pricing_latin_america/0_Data/1_Household Data/3_Ecuador/2_Codes/Cooking.Code.csv")

Lighting.Code <- distinct(data_01, VI26)%>%
  arrange(VI26)%>%
  rename(Lighting_Fuel = VI26)%>%
  mutate(lighting_fuel = 1:n())%>%
  mutate(LF = c("Electricity", "No Lighting", "Electricity", "Electricity", "Other"))%>%
  write_csv(., "carbon_pricing_latin_america/0_Data/1_Household Data/3_Ecuador/2_Codes/Lighting.Code.csv")

Water.Code <- distinct(data_01, VI17)%>%
  arrange(VI17)%>%
  rename(Water = VI17)%>%
  mutate(water = 1:n())%>%
  mutate(WTR = c("Basic", "Basic", "Limited", "Basic", "Basic", "Limited"))%>%
  write_csv(., "carbon_pricing_latin_america/0_Data/1_Household Data/3_Ecuador/2_Codes/Water.Code.csv")

Toilet.Code <- distinct(data_01, VI14)%>%
  arrange(VI14)%>%
  rename(Toilet = VI14)%>%
  mutate(toilet = 1:n())%>%
  mutate(TLT = c("Basic", "Basic", "Basic", "Basic", "No Service"))%>%
  write_csv(., "carbon_pricing_latin_america/0_Data/1_Household Data/3_Ecuador/2_Codes/Toilet.Code.csv")

Ethnicity.Code <- distinct(data_02, PD18)%>%
  arrange(PD18)%>%
  rename(Ethnicity = PD18)%>%
  mutate(ethnicity = 1:n())%>%
  write_csv(., "carbon_pricing_latin_america/0_Data/1_Household Data/3_Ecuador/2_Codes/Ethnicity.Code.csv")

Gender.Code <- distinct(data_02, SEXO)%>%
  arrange(SEXO)%>%
  rename(Sex_hhh = SEXO)%>%
  mutate(sex_hhh = 1:n())%>%
  write_csv(., "carbon_pricing_latin_america/0_Data/1_Household Data/3_Ecuador/2_Codes/Gender.Code.csv")

Education.Code <- distinct(data_02, PE47)%>%
  arrange(PE47)%>%
  rename(Education = PE47)%>%
  mutate(edu_hhh = 0:(n()-1))%>%
  mutate(ISCED = c(0,1,1,2,5,0,6,7,1,3,6))%>%
  write_csv(., "carbon_pricing_latin_america/0_Data/1_Household Data/3_Ecuador/2_Codes/Education.Code.csv")


# Output ####

data_01.2 <- data_01.1 %>%
  left_join(Province.Code)%>%
  left_join(District.Code)%>%
  left_join(Village.Code)%>%
  left_join(select(Cooking.Code, Cooking_Fuel, cooking_fuel))%>%
  left_join(select(Lighting.Code, Lighting_Fuel, lighting_fuel))%>%
  left_join(select(Water.Code, Water, water))%>%
  left_join(select(Toilet.Code, Toilet, toilet))%>%
  select(hh_id, hh_weights, urban_01, province, district, village, cooking_fuel, lighting_fuel, water, toilet, electricity.access)

data_02.4 <- data_02.1 %>%
  left_join(Ethnicity.Code)%>%
  left_join(Gender.Code)%>%
  left_join(Education.Code)%>%
  select(hh_id, ethnicity, sex_hhh, ind_hhh, edu_hhh)

household_information <- data_01.2 %>%
  left_join(data_02.2)%>%
  left_join(data_02.4)%>%
  left_join(data_02.3_06.1)

write_csv(household_information, "../0_Data/1_Household Data/3_Ecuador/1_Data_Clean/household_information_Ecuador.csv")

# Appliances ####

data_11.1 <- data_11 %>%
  rename(hh_id = IDENTIF_HOG)%>%
  select(hh_id, EQ00, EQ01)%>%
  mutate(ownership = ifelse(EQ01 == "No",0,1))%>%
  mutate(appliance = ifelse(EQ00 == "Aire acondicionado", "ac.01", 
                            ifelse(EQ00 == "Bicicleta", "bicycle.01",
                                   ifelse(EQ00 == "Carro para uso exclusivo del hogar", "car.01",
                                          ifelse(EQ00 == "Computador, laptop, tablet, ..." | EQ00 == "Computadora de escritorio", "computer.01",
                                                 ifelse(EQ00 == "Cocina de inducción" | EQ00 == "Hornos de toda clase" | EQ00 == "Cocineta" | EQ00 == "Cocina con o sin horno", "stove.01",
                                                        ifelse(EQ00 == "Lavadora de platos", "dishwasher.01",
                                                               ifelse(EQ00 == "Lavadora de ropa" | EQ00 == "Lavadora y secadora de ropa", "washing_machine.01",
                                                                      ifelse(EQ00 == "Microondas", "microwave.01",
                                                                             ifelse(EQ00 == "Plancha", "iron.01",
                                                                                    ifelse(EQ00 == "Motocicleta", "motorcycle.01",
                                                                                           ifelse(EQ00 == "Ventilador", "fan.01",
                                                                                                  ifelse(EQ00 == "Televisor a color" | EQ00 == "Televisor blanco y negro" | EQ00 == "Televisor plasma / LCD / LED", "tv.01",
                                                                                                         ifelse(EQ00 == "Refrigeradora", "refrigerator.01",
                                                                                                                ifelse(EQ00 == "Radio grabadora", "radio.01",
                                                                                                                       ifelse(EQ00 == "Secadora de ropa", "dryer.01",
                                                                                                                              ifelse(EQ00 == "Máquina de coser", "sewing_machine.01", "None")))))))))))))))))%>%
  filter(appliance != "None")%>%
  group_by(hh_id, appliance)%>%
  summarise(ownership = max(ownership))%>%
  ungroup()%>%
  pivot_wider(names_from = "appliance", values_from = "ownership")

write_csv(data_11.1, "../0_Data/1_Household Data/3_Ecuador/1_Data_Clean/appliances_0_1_Ecuador.csv")

# Expenditures ####

data_01.3 <- data_01 %>%
  select(IDENTIF_HOG, VI28A, VI30A, VI32A, VI34A, VI37A, VI38A, VI39, VI40, VI45B)%>%
  rename(hh_id = IDENTIF_HOG)%>%
  mutate_at(vars(-hh_id), list(~ as.numeric(.)))%>%
  pivot_longer(-hh_id, names_to = "item_code", values_to = "expenditures_year")%>%
  filter(expenditures_year > 0 & !is.na(expenditures_year))%>%
  mutate(expenditures_year = expenditures_year*12)%>%
  mutate(item_code = str_replace(item_code, "VI",""))%>%
  mutate(item_code = str_replace(item_code, "A", ""))%>%
  mutate(item_code = str_replace(item_code, "B", ""))%>%
  mutate(item_code = as.numeric(item_code))%>%
  mutate(item_code = 400 + item_code)%>%
  group_by(hh_id, item_code)%>%
  summarise(expenditures_year = sum(expenditures_year))%>%
  ungroup()

data_04.1 <- data_04 %>%
  rename(hh_id = IDENTIF_HOG)%>%
  select(hh_id, GA00, GA05, GA07)%>%
  filter(GA07 != " ")%>%
  mutate(GA07 = as.numeric(GA07))%>%
  filter(!is.na(GA07))%>%
  mutate(expenditures_year = ifelse(GA05 == "Anual", GA07, 
                                    ifelse(GA05 == "Diario", GA07*365,
                                           ifelse(GA05 == "Mensual", GA07*12,
                                                  ifelse(GA05 == "Quincenal", GA07*365/14,
                                                         ifelse(GA05 == "Semanal", GA07*365/7,
                                                                ifelse(GA05 == "Semestral", GA07*2, 
                                                                       ifelse(GA05 == "Trimestral", GA07*3, 9999999999))))))))%>%
  select(hh_id, GA00, expenditures_year)

Item.Code.Food <- data_04.1 %>%
  distinct(GA00)%>%
  arrange(GA00)%>%
  mutate(item_code = 1:n())
  
data_04.2 <- data_04.1 %>%
  left_join(Item.Code.Food)%>%
  select(-GA00)

data_05.1 <- data_05 %>%
  rename(hh_id = IDENTIF_HOG)%>%
  select(hh_id, starts_with("GA") & ends_with("2"))%>%
  pivot_longer(-hh_id, names_to = "item_code_GX", values_to = "expenditures_year")%>%
  filter(expenditures_year > 0)%>%
  mutate(expenditures_year = expenditures_year*365/7) # Weekly expenditures

data_05.2 <- data_05 %>%
  rename(hh_id = IDENTIF_HOG)%>%
  select(hh_id, starts_with("GB") & ends_with ("2"))%>%
  mutate_at(vars(GB0102, GB0402, GB0702, GB1002, GB1102, GB2102, GB2302, GB3602, GB3702, GB3902), list(~ as.numeric(.)))%>%
  pivot_longer(-hh_id, names_to = "item_code_GX", values_to = "expenditures_year")%>%
  filter(expenditures_year > 0)%>%
  mutate(expenditures_year = expenditures_year*12) # monthly expenditures
  
data_05.3 <- data_05 %>%
  rename(hh_id = IDENTIF_HOG)%>%
  select(hh_id, starts_with("GC") & ends_with ("2"))%>%
  mutate_at(vars(GC0902, GC1002, GC2002, GC2102, GC2302, GC2402, GC2502, GC2602), list(~ as.numeric(.)))%>%
  pivot_longer(-hh_id, names_to = "item_code_GX", values_to = "expenditures_year")%>%
  filter(expenditures_year > 0)%>%
  mutate(expenditures_year = expenditures_year *3) # trimestral expenditures

data_05.4 <- data_05 %>%
  rename(hh_id = IDENTIF_HOG)%>%
  select(hh_id, starts_with("GD") & ends_with("2"))%>%
  mutate_at(vars(GD0302, GD2202, GD2302, GD2502), list(~ as.numeric(.)))%>%
  pivot_longer(-hh_id, names_to = "item_code_GX", values_to = "expenditures_year")%>%
  filter(expenditures_year > 0)%>%
  mutate(expenditures_year = expenditures_year) # annual expenditures

data_05.5 <- bind_rows(data_05.1, data_05.2, data_05.3, data_05.4)

Item.Code.Nonfood <- distinct(data_05.5, item_code_GX)%>%
  arrange(item_code_GX)%>%
  mutate(item_code = 200:307)

data_05.6 <- data_05.5 %>%
  left_join(Item.Code.Nonfood)%>%
  select(-item_code_GX)

expenditures_items <- bind_rows(data_04.2, data_05.6)%>%
  bind_rows(data_01.3)%>%
  arrange(hh_id, item_code)

write_csv(expenditures_items, "../0_Data/1_Household Data/3_Ecuador/1_Data_Clean/expenditures_items_Ecuador.csv")
