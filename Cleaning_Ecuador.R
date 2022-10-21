# Load Data ####

ec_01 <- read.csv("Dataset_2014/DATOS_ABIERTOS/BASES/01_ecv6r_vivienda.csv", encoding = "UTF-8", stringsAsFactors = FALSE)
ec_02 <- read_csv("Dataset_2014/DATOS_ABIERTOS/BASES/02_ecv6r_personas.csv", encoding = "UTF-8", stringsAsFactors = FALSE)

# ec_03 <- read.csv("Dataset_2014/DATOS_ABIERTOS/BASES/03_ecv6r_gastos_alimentos1.csv", encoding = "UTF-8", stringsAsFactors = FALSE)
# Information on who was interviewed on expenditures

ec_04 <- read.csv("Dataset_2014/DATOS_ABIERTOS/BASES/04_ecv6r_gastos_alimentos.csv", encoding = "UTF-8", stringsAsFactors = FALSE)
ec_05 <- read.csv("Dataset_2014/DATOS_ABIERTOS/BASES/05_ecv6r_otros_gastos.csv", encoding = "UTF-8", stringsAsFactors = FALSE)
ec_11 <- read.csv("Dataset_2014/DATOS_ABIERTOS/BASES/11_ecv6r_equipamiento.csv", encoding = "UTF-8", stringsAsFactors = FALSE)

#ec_12 <- read.csv("Dataset_2014/DATOS_ABIERTOS/BASES/12_ecv6r_capital_social.csv", encoding = "UTF-8", stringsAsFactors = FALSE)
# Happiness, how you percieve your life

# ec_24 <- read.csv("Dataset_2014/DATOS_ABIERTOS/BASES/24_factor_fin.csv", encoding = "UTF-8", stringsAsFactors = FALSE)%>%
#   rename(IDENTIF_SECT = idsector)
# ec_25 <- read.csv("Dataset_2014/DATOS_ABIERTOS/BASES/25_base_de_trabajo_hogares.csv", encoding = "UTF-8", stringsAsFactors = FALSE)
# condensed data on households
# ec_26 <- read.csv("Dataset_2014/DATOS_ABIERTOS/BASES/26_base_de_trabajo_personas.csv", encoding = "UTF-8", stringsAsFactors = FALSE)
# condensed data on persons within households

# there exists way more in ARCHIVOS de Variables but this should suffice

# Transform Data ####

ec_011 <- ec_01%>%
  select(IDENTIF_SECT, IDENTIF_HOG, AREA_5000, AREA_2000, VI13, VI14, VI17, VI26, VI27, VI28A, VI30A, VI31, VI32A, VI34A, AM10A, AM10B, FEXP, CIUDAD_AUTO, PROVINCIA)%>%
  rename(hh_id = IDENTIF_HOG, cooking.fuel = VI13, toilet.type = VI14, water.source = VI17, lighting.fuel = VI26, electricity.access = VI27,
         internet.access = VI31, light_bulb.noa = AM10A, light_bulb.nob = AM10B, hh_weights = FEXP,
         district = CIUDAD_AUTO, province = PROVINCIA)

ec_011bulb <- ec_011 %>%
  select(hh_id, light_bulb.noa, light_bulb.nob)%>%
  mutate(light_bulba.01 = ifelse(light_bulb.noa > 0, 1, 0))%>%
  mutate(light_bulbb.01 = ifelse(light_bulb.nob > 0, 1, 0))%>%
  rename(light_bulba.no = light_bulb.noa, light_bulbb.no = light_bulb.nob)

ec_011bulba <- ec_011bulb %>%
  select(-light_bulba.01, -light_bulbb.01)

ec_011bulbb <- ec_011bulb %>%
  select(-light_bulba.no, -light_bulbb.no)

ec_0112 <- ec_01 %>%
  select(IDENTIF_HOG, VI28A, VI30A, VI32A, VI34A, VI37A, VI38A, VI39, VI40, VI45B)%>%
  rename(hh_id = IDENTIF_HOG)%>%
  mutate(VI28A = as.numeric(VI28A))%>%
  mutate(VI30A = as.numeric(VI30A))%>%
  mutate(VI32A = as.numeric(VI32A))%>%
  mutate(VI34A = as.numeric(VI34A))%>%
  mutate(VI37A = as.numeric(VI37A))%>%
  mutate(VI38A = as.numeric(VI38A))%>%
  mutate(VI39  = as.numeric(VI39))%>%
  mutate(VI40  = as.numeric(VI40))%>%
  mutate(VI45B = as.numeric(VI45B))%>%
  mutate_at(vars(c(VI28A, VI30A, VI32A, VI34A, VI37A, VI38A, VI39, VI40)), function(x){x = x*12})

ec_0112 <- ec_0112%>%
  rename_at(vars(starts_with("VI")), funs(str_replace(., "VI", "")))%>%
  rename_at(vars(ends_with("A")), funs(str_replace(., "A", "")))%>%
  rename_at(vars(ends_with("B")), funs(str_replace(., "B", "")))

ec_0112.1 <- ec_0112 %>%
  gather(key = "item_code", value = "expenditures", - hh_id)%>%
  mutate(item_code = as.numeric(item_code))%>%
  mutate(item_code = item_code + 400)%>%
  spread(key = "item_code", value = "expenditures")

# write_csv(ec_0112.1, "exp_dwellings_Ecuador.csv")            

ec_0111 <- ec_011 %>%
  select(hh_id, AREA_5000, AREA_2000, hh_weights, cooking.fuel, toilet.type, water.source, lighting.fuel, electricity.access, internet.access)%>%
  mutate(internet.access = ifelse(internet.access == "No", 0, 1))

# Attention: VI27 refers to Stromz?hler, not necessarily electricity access

ec_021 <- ec_02%>%
  select(IDENTIF_HOG, PERSONA, EDAD, PD03B, PD04, PD18, PE47)%>%
  rename(hh_id = IDENTIF_HOG, education = PE47, ethnicity = PD18)%>%
  mutate(age = as.numeric(EDAD))

ec_021$age[is.na(ec_021$age)] <- 0

ec_022 <- ec_021 %>%
  mutate(adults = ifelse(age >= 18, 1, 0))%>%
  mutate(children = ifelse(age < 18, 1, 0))%>%
  group_by(hh_id)%>%
  mutate(hh_size = n())%>%
  mutate(adults = sum(adults))%>%
  mutate(children = sum(children))%>%
  ungroup()%>%
  filter(PD04 == "Jefe")%>%
  select(hh_id, hh_size, adults, children, education, ethnicity)

ec_0 <- left_join(ec_0111, ec_022, by = "hh_id")%>%
  select(hh_id, hh_size, hh_weights, adults, children, everything())%>%
  mutate(pop = hh_weights*hh_size)

# Population 15.952.442

# ec_01 <- ec_0 %>%
#   group_by(AREA_5000)%>%
#   summarise(pop = sum(pop))
# 
# ec_02 <- ec_0 %>%
#   group_by(AREA_2000)%>%
#   summarise(pop = sum(pop))

# around 63,67 % should live in cities (--> urban)

# AREA_5000 works better --> decision here at this point

ec_0 <- ec_0 %>%
  select(-pop, - AREA_2000)%>%
  rename(Urban = AREA_5000)%>%
  mutate(Urban = ifelse(Urban == "Urbano", "urban", "rural"))%>%
  mutate(urban = ifelse(Urban == "urban", 2, 1))

# write_csv(ec_0, "household_information_Ecuador.csv")

ec_041 <- ec_04%>%
  select(IDENTIF_HOG, GA00, GA05, GA07)%>%
  filter(GA07 != " ")%>%
  mutate(GA07 = as.numeric(GA07))

ec_041$GA07[is.na(ec_041$GA07)] <- 0

ec_0411 <- ec_041%>%
  mutate(expenditures = ifelse(GA05 == "Anual", GA07, 
                               ifelse(GA05 == "Diario", GA07*365,
                                      ifelse(GA05 == "Mensual", GA07*12,
                                             ifelse(GA05 == "Quincenal", GA07*365/14,
                                                    ifelse(GA05 == "Semanal", GA07*365/7,
                                                           ifelse(GA05 == "Semestral", GA07*2, 
                                                                  ifelse(GA05 == "Trimestral", GA07*3, 9999999999))))))))
ec_0411 <- ec_0411 %>%
  rename(hh_id = IDENTIF_HOG, item_code = GA00)

average_item <- ec_0411%>%
  group_by(item_code)%>%
  summarise(
    average = mean(expenditures),
    n = n()
  )
# Outlier Correction

ec_0411$expenditures[ec_0411$hh_id == 20250999004081 & ec_0411$item_code == "Otros productos 1"] <- average_item$average[average_item$item_code == "Otros productos 1"]
ec_0411$expenditures[ec_0411$hh_id == 170150095003041 & ec_0411$item_code == "Otros productos 5"] <- average_item$average[average_item$item_code == "Otros productos 5"]

ec_0411 <- ec_0411 %>%
  select(hh_id, item_code, expenditures)%>%
  spread(key = item_code, value = expenditures)

# missing households obviously did not buy anything

ec_042 <- ec_04 %>%
  select(IDENTIF_HOG)%>%
  rename(hh_id = IDENTIF_HOG)%>%
  filter(!duplicated(hh_id))%>%
  left_join(ec_0411, by = "hh_id")

# write.csv(ec_042, "expenditures_food_Ecuador.csv")

ec_051 <- ec_05%>%
  rename(hh_id = IDENTIF_HOG)

ec_0511 <- ec_051 %>%
  select(hh_id, GA0102, GA0202, GA0302, GA0402, GA0502, GA0602, GA0702, GA0802)%>%
  mutate_at(vars(c(GA0102:GA0802)), function(x){x= x*365/7}) # weekly

ec_052 <- ec_051 %>%
  select(hh_id, starts_with("GB"))%>%
  select(hh_id, ends_with("2"))%>%
  mutate_at(vars(c(GB0102:GB3902)), function(x){x= as.numeric(x)})%>%
  mutate_at(vars(c(GB0102:GB3902)), function(x){x= x*12}) # monthly

ec_053 <- ec_051 %>%
  select(hh_id, starts_with("GC"))%>%
  select(hh_id, ends_with("2"))%>%
  mutate_at(vars(c(GC0102:GC3002)), function(x){x= as.numeric(x)})%>%
  mutate_at(vars(c(GC0102:GC3002)), function(x){x= x*3}) # trimestral

ec_054 <- ec_051 %>%
  select(hh_id, starts_with("GD"))%>%
  select(hh_id, ends_with("2"))%>%
  mutate_at(vars(c(GD0102:GD3102)), function(x){x= as.numeric(x)}) # annual

ec_050 <- ec_0511 %>%
  left_join(ec_052, by = "hh_id")%>%
  left_join(ec_053, by = "hh_id")%>%
  left_join(ec_054, by = "hh_id")

# write.csv(ec_050, "expenditures_non_food_Ecuador.csv")

food_1 <- ec_042 %>%
  select(-X1)%>%
  gather(key = "item_code", value = "expenditures", -hh_id)

food_items <- count(food_1, item_code)%>%
  select(-n)%>%
  mutate(item_code_new = 1:n())%>%
  rename(item_name = item_code)%>%
  mutate(item_code = "")

food_2 <- food_1 %>%
  left_join(food_items, by = "item_code")%>%
  select(hh_id, item_code_new, expenditures)%>%
  rename(item_code = item_code_new)

# Leave it here for now

non_food_1 <- ec_050 %>%
  select(-X1)%>%
  gather(key = "item_code", value = "expenditures", -hh_id)

non_food_items <- count(non_food_1, item_code)%>%
  select(-n)%>%
  mutate(item_code_new = 200:307)%>%
  mutate(item_name = "")

non_food_2 <- non_food_1 %>%
  left_join(non_food_items, by = "item_code")%>%
  select(hh_id, item_code_new, expenditures)%>%
  rename(item_code = item_code_new)

expenditures <- rbind(food_2, non_food_2)%>%
  arrange(hh_id)

expenditures_1 <- expenditures %>%
  spread(key = "item_code", value = "expenditures")

write_csv(expenditures_1, "expenditures_items_Ecuador.csv")

ec_111 <- ec_11 %>%
  select(IDENTIF_HOG, EQ00, EQ01, EQ02)%>%
  rename(hh_id = IDENTIF_HOG, item_code = EQ00)

ec_112 <- ec_111 %>%
  select(hh_id, item_code, EQ01)%>%
  mutate(YN = ifelse(EQ01 == "No", 0, 1))%>%
  select(-EQ01)%>%
  spread(key = item_code, value = YN)

ec_1121 <- ec_112 %>%
  select(hh_id, `Aire acondicionado`, `Betamax, VHS, VCD, DVD, Blue Ray, ...`, Bicicleta, `Carro para uso exclusivo del hogar`, `Cocina de inducci?n`, Cocineta, `Computador, laptop, tablet, ...`, `Computadora de escritorio`, `Hornos de toda clase`, `Lavadora de platos`, `Lavadora de ropa`, `Lavadora y secadora de ropa`, `M?quina de coser`, Microondas, Motocicleta, Plancha, `Radio grabadora`, Refrigeradora, `Secadora de ropa`, `Televisor a color`, `Televisor blanco y negro`, `Televisor plasma / LCD / LED`, Ventilador)%>%
  rename(ac.01 = `Aire acondicionado`, video.01 = `Betamax, VHS, VCD, DVD, Blue Ray, ...`, bycicle.01 = Bicicleta, car.01 = `Carro para uso exclusivo del hogar`, stove.e.01 = `Cocina de inducci?n`, stove.01 = Cocineta, computer.01a = `Computador, laptop, tablet, ...`, computer.01b = `Computadora de escritorio`, stove.01a = `Hornos de toda clase`, dishwasher.01 = `Lavadora de platos`, washing_machine.01a = `Lavadora de ropa`, washing_machine.01b = `Lavadora y secadora de ropa`, sewing.machine.01 = `M?quina de coser`, microwave.01 = Microondas, motorcycle.01 = Motocicleta, iron.01 = Plancha, radio.01 = `Radio grabadora`, refrigerator.01 = Refrigeradora, dryer.01 = `Secadora de ropa`, TV.01a = `Televisor a color`, TV.01b = `Televisor blanco y negro`, TV.01c = `Televisor plasma / LCD / LED`, fan.01 = Ventilador)%>%
  mutate(computer.01 = ifelse((computer.01a == 0 & computer.01b == 0), 0, 1))%>%
  mutate(washing_machine.01 = ifelse((washing_machine.01a == 0 & washing_machine.01b == 0), 0, 1))%>%
  mutate(TV.01 = ifelse((TV.01a == 0 & TV.01b == 0 & TV.01c == 0), 0, 1))%>%
  select(-c(computer.01a, computer.01b, washing_machine.01a, washing_machine.01b, TV.01a, TV.01b, TV.01c))%>%
  left_join(ec_011bulbb, by = "hh_id")


ec_113 <- ec_111 %>%
  select(hh_id, item_code, EQ02)%>%
  spread(key = item_code, value = EQ02)%>%
  select(hh_id, `Aire acondicionado`, `Betamax, VHS, VCD, DVD, Blue Ray, ...`, Bicicleta, `Carro para uso exclusivo del hogar`, `Cocina de inducci?n`, Cocineta, `Computador, laptop, tablet, ...`, `Computadora de escritorio`, `Hornos de toda clase`, `Lavadora de platos`, `Lavadora de ropa`, `Lavadora y secadora de ropa`, `M?quina de coser`, Microondas, Motocicleta, Plancha, `Radio grabadora`, Refrigeradora, `Secadora de ropa`, `Televisor a color`, `Televisor blanco y negro`, `Televisor plasma / LCD / LED`, Ventilador)%>%
  rename(ac.01 = `Aire acondicionado`, video.01 = `Betamax, VHS, VCD, DVD, Blue Ray, ...`, bycicle.01 = Bicicleta, car.01 = `Carro para uso exclusivo del hogar`, stove.e.01 = `Cocina de inducci?n`, stove.01 = Cocineta, computer.01a = `Computador, laptop, tablet, ...`, computer.01b = `Computadora de escritorio`, stove.a.01 = `Hornos de toda clase`, dishwasher.01 = `Lavadora de platos`, washing_machine.01a = `Lavadora de ropa`, washing_machine.01b = `Lavadora y secadora de ropa`, sewing.machine.01 = `M?quina de coser`, microwave.01 = Microondas, motorcycle.01 = Motocicleta, iron.01 = Plancha, radio.01 = `Radio grabadora`, refrigerator.01 = Refrigeradora, dryer.01 = `Secadora de ropa`, TV.01a = `Televisor a color`, TV.01b = `Televisor blanco y negro`, TV.01c = `Televisor plasma / LCD / LED`, fan.01 = Ventilador)%>%
  mutate_at(vars(c(ac.01:fan.01)), function(x){x = as.numeric(x)})%>%
  mutate(computer.01 = computer.01a + computer.01b)%>%
  mutate(washing_machine.01 = washing_machine.01a + washing_machine.01b)%>%
  mutate(TV.01 = TV.01a + TV.01b + TV.01c)%>%
  select(-c(computer.01a, computer.01b, washing_machine.01a, washing_machine.01b, TV.01a, TV.01b, TV.01c))

ec_1131 <- ec_113 %>%
  rename_at(vars(ends_with("01")), funs(str_replace(., "01", "no")))%>%
  left_join(ec_011bulba, by = "hh_id")

# write.csv(ec_1121, "appliances_0_1_Ecuador.csv")

# write.csv(ec_1131, "appliances_no_Ecuador.csv")

# Codierungen ####

Cooking.Code <- count(ec_0111, cooking.fuel)%>%
  select(-n)%>%
  mutate(cooking.ID = 1:n())

Ethnicity.Code <- count(ec_0, ethnicity)%>%
  select(-n)%>%
  mutate(ethnicity.ID = 1:n())

Toilet.Code <- count(ec_0111, toilet.type)%>%
  select(-n)%>%
  mutate(toilet.ID = 1:n())

Water.Code <- count(ec_0111, water.source)%>%
  select(-n)%>%
  mutate(water.ID = 1:n())

Lighting.Code <- count(ec_0111, lighting.fuel)%>%
  select(-n)%>%
  mutate(lighting.ID = 1:n())

Electricity.Code <- count(ec_0111, electricity.access)%>%
  select(-n)%>%
  mutate(electricity.ID = 1:n())

Education.Code <- count(ec_0, education)%>%
  select(-n)%>%
  mutate(education.ID = 1:n())

Urban.Code <- count(ec_0, Urban, urban)%>%
  select(-n)

# write_csv(Ethnicity.Code, "Ethnicity.Code.csv")
# write.csv(Cooking.Code, "Cooking.Code.csv")
# write.csv(Toilet.Code, "Toilet.Code.csv")
# write.csv(Water.Code, "Water.Code.csv")
# write.csv(Lighting.Code, "Lighting.Code.csv")
# write.csv(Electricity.Code, "Electricity.Code.csv")
# write.csv(Education.Code, "Education.Code.csv")
# write.csv(Urban.Code, "Urban.Code.csv")


household    <- read_csv("../0_Data/1_Household Data/3_Ecuador/1_Data_Clean/Old/household_information_Ecuador.csv")
expenditures <- read_csv("../0_Data/1_Household Data/3_Ecuador/1_Data_Clean/Old/expenditures_items_Ecuador.csv", col_types = cols(.default = col_double()))
appliances   <- read_csv("../0_Data/1_Household Data/3_Ecuador/1_Data_Clean/Old/appliances_0_1_Ecuador.csv")


household_1 <- household %>%
  rename(edu_hhh = education, water = drinking.water, electricity = electricity.access, cooking_fuel = cooking.fuel, toilet = toilet.type, internet = internet.access, lighting_fuel = lighting.fuel)%>%
  mutate(urban_01 = ifelse(urban == 2,1,0))%>%
  select(-urban)

expenditures_1 <- expenditures %>%
  pivot_longer(-hh_id, names_to = "item_code", values_to = "expenditures_year")%>%
  filter(!is.na(expenditures_year) & expenditures_year > 0)

write_csv(household_1,    "../0_Data/1_Household Data/3_Ecuador/1_Data_Clean/household_information_Ecuador.csv")
write_csv(expenditures_1, "../0_Data/1_Household Data/3_Ecuador/1_Data_Clean/expenditures_items_Ecuador.csv")
write_csv(appliances,     "../0_Data/1_Household Data/3_Ecuador/1_Data_Clean/appliances_0_1_Ecuador.csv")

# Income
household    <- read_csv("../0_Data/1_Household Data/3_Ecuador/1_Data_Clean/household_information_Ecuador.csv")

ec_02 <- read.csv("R:/MSA/datasets/Household Microdata/Ecuador/Dataset_2014/DATOS_ABIERTOS/BASES/02_ecv6r_personas.csv", encoding = "UTF-8", stringsAsFactors = FALSE)
ec_06 <- read.csv("R:/MSA/datasets/Household Microdata/Ecuador/Dataset_2014/DATOS_ABIERTOS/BASES/06_ecv6r_otros_ingresos.csv", encoding = "UTF-8", stringsAsFactors = FALSE)
ec_26 <- read.csv("R:/MSA/datasets/Household Microdata/Ecuador/Dataset_2014/DATOS_ABIERTOS/BASES/26_base_de_trabajo_personas.csv", encoding = "UTF-8", stringsAsFactors = FALSE)

inc_1 <- ec_02 %>%
  rename(hh_id = IDENTIF_HOG)%>%
  select(hh_id, PA92, starts_with("PA85"))%>%
  mutate(PA85B = ifelse(PA85A == "Instituciones Nacionales", PA85B,0),
         PA92 = ifelse(is.na(PA92),0,PA92))%>%
  select(hh_id, PA92, PA85B)%>%
  mutate(sum_a = PA92 + PA85B)%>%
  group_by(hh_id)%>%
  summarise(inc_gov_cash = sum(sum_a),
            inc_gov_monetary = 0)%>%
  ungroup()

inc_2 <- ec_06 %>%
  rename(hh_id = IDENTIF_HOG)%>%
  select(hh_id, starts_with("IB"))%>%
  select(hh_id, ends_with("02"), IB0104)%>%
  mutate(IB0102 = as.numeric(IB0102))%>%
  mutate_at(vars(-hh_id), list(~ ifelse(is.na(.)|. == "",0,.)))%>%
  mutate(inc_gov_cash     = IB0302 + IB0104,
         inc_gov_monetary = IB0102 + IB0202)%>%
  select(hh_id, inc_gov_cash, inc_gov_monetary)

inc_3 <- bind_rows(inc_1, inc_2)%>%
  group_by(hh_id)%>%
  summarise(inc_gov_cash = sum(inc_gov_cash),
            inc_gov_monetary = sum(inc_gov_monetary))%>%
  ungroup()

household_1 <- left_join(select(household, -inc_gov_cash, -inc_gov_monetary), inc_3)           

write_csv(household_1,    "../0_Data/1_Household Data/3_Ecuador/1_Data_Clean/household_information_Ecuador.csv")

household_1 <- read_csv("../0_Data/1_Household Data/3_Ecuador/1_Data_Clean/household_information_Ecuador.csv")

household_1 <- household_1 %>%
  mutate(electricity.access = ifelse(electricity == 1 | electricity == 2,0,1))%>%
  select(-electricity)

write_csv(household_1,    "../0_Data/1_Household Data/3_Ecuador/1_Data_Clean/household_information_Ecuador.csv")

appliances <- read_csv("../0_Data/1_Household Data/3_Ecuador/1_Data_Clean/appliances_0_1_Ecuador.csv")

appliances <- appliances %>%
  rename(bicycle.01 = bycicle.01, tv.01 = TV.01)%>%
  select(-starts_with("light_bulb"))

write_csv(appliances, "../0_Data/1_Household Data/3_Ecuador/1_Data_Clean/appliances_0_1_Ecuador.csv")

# Add Household- and Person-level information to Ecuador
ec_01 <- read.csv("R:/MSA/datasets/Household Microdata/Ecuador/Dataset_2014/DATOS_ABIERTOS/BASES/01_ecv6r_vivienda.csv", encoding = "UTF-8", stringsAsFactors = FALSE)
ec_02 <- read.csv("R:/MSA/datasets/Household Microdata/Ecuador/Dataset_2014/DATOS_ABIERTOS/BASES/02_ecv6r_personas.csv", encoding = "UTF-8", stringsAsFactors = FALSE)
ec_25 <- read.csv("R:/MSA/datasets/Household Microdata/Ecuador/Dataset_2014/DATOS_ABIERTOS/BASES/25_base_de_trabajo_hogares.csv", encoding = "UTF-8", stringsAsFactors = FALSE)
ec_26 <- read.csv("R:/MSA/datasets/Household Microdata/Ecuador/Dataset_2014/DATOS_ABIERTOS/BASES/26_base_de_trabajo_personas.csv", encoding = "UTF-8", stringsAsFactors = FALSE)

ec_01.1 <- ec_01 %>%
  rename(hh_id = IDENTIF_HOG, Province = CIUDAD_AUTO, District = PROVINCIA, village = CIUDAD)%>%
  select(hh_id, Province, District, village)

Province.Code <- distinct(ec_01.1, Province)%>%
  arrange(Province)%>%
  mutate(province = 1:n())

District.Code <- distinct(ec_01.1, District)%>%
  arrange(District)%>%
  mutate(district = 1:n())

write_csv(Province.Code, "../0_Data/1_Household Data/3_Ecuador/2_Codes/Province.Code.csv")
write_csv(District.Code, "../0_Data/1_Household Data/3_Ecuador/2_Codes/District.Code.csv")

ec_01.2 <- ec_01.1 %>%
  left_join(Province.Code)%>%
  left_join(District.Code)%>%
  select(-Province, -District)

ec_02.1 <- ec_02 %>%
  filter(PD04 == "Jefe")%>%
  rename(hh_id = IDENTIF_HOG, age_hhh = EDAD, sex_hhh = SEXO, ind_hhh = PA16)%>%
  select(hh_id, age_hhh, sex_hhh, ind_hhh)%>%
  mutate(sex_hhh = ifelse(sex_hhh == "Hombre",1,2))

Gender.Code <- data.frame(sex_hhh = c(1,2), Gender = c("Male", "Female"))
write_csv(Gender.Code, "../0_Data/1_Household Data/3_Ecuador/2_Codes/Gender.Code.csv")

Industry.Code <- distinct(ec_02.1, ind_hhh)%>%
  arrange(ind_hhh)


household_2 <- read_csv("../0_Data/1_Household Data/3_Ecuador/1_Data_Clean/household_information_Ecuador.csv")

household_2.1 <- household_2 %>%
  left_join(ec_01.2)%>%
  left_join(ec_02.1)

write_csv(household_2.1,"../0_Data/1_Household Data/3_Ecuador/1_Data_Clean/household_information_Ecuador.csv")

household_3 <- read_csv("../0_Data/1_Household Data/3_Ecuador/1_Data_Clean/household_information_Ecuador.csv")

household_3.1 <- household_3 %>%
  mutate(age_hhh = ifelse(is.na(age_hhh),98,age_hhh))%>%
  mutate(age_hhh = as.numeric(age_hhh))

write_csv(household_3.1,"../0_Data/1_Household Data/3_Ecuador/1_Data_Clean/household_information_Ecuador.csv")

household_4 <- read_csv("../0_Data/1_Household Data/3_Ecuador/1_Data_Clean/household_information_Ecuador.csv")
ec_01 <- read.csv("R:/MSA/datasets/Household Microdata/Ecuador/Dataset_2014/DATOS_ABIERTOS/BASES/01_ecv6r_vivienda.csv", encoding = "UTF-8", stringsAsFactors = FALSE)

ec_0.1.1 <- ec_01 %>%
  rename(hh_id = IDENTIF_HOG)%>%
  select(hh_id, VI27)%>%
  mutate(electricity.access = ifelse(VI27 == "Si, de uso común para varias viviendas" | VI27 == "Si, de uso exclusivo para la vivienda",1,0))%>%
  select(hh_id, electricity.access)

household_4.1 <- household_4 %>%
  select(-electricity.access)%>%
  left_join(ec_0.1.1)
 
write_csv(household_4.1,"../0_Data/1_Household Data/3_Ecuador/1_Data_Clean/household_information_Ecuador.csv")
