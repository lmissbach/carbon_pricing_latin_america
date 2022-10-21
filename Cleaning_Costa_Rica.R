if(!require("pacman")) install.packages("pacman")

p_load("tidyverse", "Hmisc", "haven", "readr", "openxlsx", "sjlabelled")

# Household information

personas_0  <- read_sav("C:/Users/misl/OwnCloud/A_Seminar_Verteilungsanalyse/3_Daten_Code/1_Costa_Rica_Data/Enigh2018_CreaVar_ Personas_PUBLICA.sav")
hogar_0     <- read_sav("C:/Users/misl/OwnCloud/A_Seminar_Verteilungsanalyse/3_Daten_Code/1_Costa_Rica_Data/Enigh2018_CreaVar_ Hogar_PUBLICA.sav")
gastos_0    <- read_sav("C:/Users/misl/OwnCloud/A_Seminar_Verteilungsanalyse/3_Daten_Code/1_Costa_Rica_Data/Enigh2018_Gasto_Publica.sav")

# Household information

household_1 <- hogar_0 %>%
  rename(hh_id = LLAVE_HOGAR, water = H016_PROVENIENCIA_AGUA,
         toilet = H017_SERVICIO_SANITARIO, lighting_fuel = H021_PROVENIENCIA_LUZ,
         cooking_fuel = H032_ENERGIA_COCINAR, urban = ID_ZONA, province = ID_REGION, hh_weights = FACTOR)%>%
  select(hh_id, everything())%>%
  mutate(urban_01 = ifelse(urban == "1",1,0))%>%
  select(hh_id, hh_weights, urban_01, province, cooking_fuel, lighting_fuel, toilet, water)%>%
  mutate(electricity.access = ifelse(lighting_fuel == 0 | lighting_fuel == 5,0,1))%>%
  remove_all_labels()%>%
  zap_formats()

# Persons information

personas_1 <- personas_0 %>%
  rename(hh_id = LLAVE_HOGAR)%>%
  select(hh_id, everything())%>%
  mutate(adults   = ifelse(P003_EDAD > 15,1,0),
         children = ifelse(P003_EDAD < 16,1,0))%>%
  group_by(hh_id)%>%
  summarise(hh_size  = n(),
            adults   = sum(adults),
            children = sum(children))%>%
  ungroup()

personas_2 <- personas_0 %>%
  rename(hh_id = LLAVE_HOGAR)%>%
  filter(P001_PARENTESCO == 1)%>%
  rename(sex_hhh = P002_SEXO, age_hhh = P003_EDAD,
         ethnicity = PS01_ETNIA, edu_hhh = P008_ULTIMO_GRADO,
         ind_hhh = P011_CARRERA)%>%
  select(hh_id, sex_hhh, age_hhh, ethnicity, edu_hhh, ind_hhh)%>%
  zap_formats()%>%
  remove_all_labels()

# Income

personas_3 <- personas_0 %>%
  rename(hh_id = LLAVE_HOGAR)%>%
  select(hh_id, P023_MONTO_BECA, PS10_BECA_CURSOS_TECNICOS,
         P200_TRANSF_PENSION_IVME:P210_TRANSF_AYUDA_HOG_NAC)%>%
  select(-PS30_TRANSF_PENSION_IVMN_BRUTA, -PS31_TRANSF_PENSION_IVMN_DEDUCC)%>%
  mutate_at(vars(-hh_id), list(~ ifelse(is.na(.),0,.)))%>%
  mutate(inc_gov_cash = P023_MONTO_BECA + PS10_BECA_CURSOS_TECNICOS + P204_TRANSF_BECAS_EMPRIV + P205_TRANF_BECA_SUP_TEC_PUB + P206_TRANSF_BECA_SUP_TEC_ISFL +
           P207_TRANSF_BECA_PUBL_1Y2 + P208_TRANSF_AYUDA_PUB + P209_TRANSF_AYUDA_ISFL,
         inc_gov_monetary = P200_TRANSF_PENSION_IVME + P201_TRANSF_PENSION_IVMN_NET + P202_TRANSF_PENSION_ALIMEN + P203_TRANSF_PENSION_RNC)%>%
  group_by(hh_id)%>%
  summarise(inc_gov_cash = sum(inc_gov_cash),
            inc_gov_monetary = sum(inc_gov_monetary))%>%
  ungroup()

household_2 <- left_join(household_1, personas_1, by = "hh_id")%>%
  left_join(personas_2, by = "hh_id")%>%
  left_join(personas_3, by = "hh_id")

write_csv(household_2, "../0_Data/1_Household Data/3_Costa Rica/1_Data_Clean/household_information_Costa Rica.csv")

# Expenditures

gastos_1 <- gastos_0 %>%
  rename(hh_id = LLAVE_HOGAR)%>%
  select(hh_id, ARTICULO, CCIF, GASTO_MENS, TIPO_GASTO)%>%
  arrange(hh_id, ARTICULO)%>%
  mutate(expenditures_year = GASTO_MENS*12)%>%
  filter(TIPO_GASTO != "5")%>%
  select(-TIPO_GASTO, -GASTO_MENS)%>%
  zap_formats()%>%
  remove_all_labels()%>%
  mutate(CCIF     = ifelse(is.na(CCIF) | CCIF == "" | CCIF == "?", "0", CCIF),
         ARTICULO = ifelse(is.na(ARTICULO) | ARTICULO == "", "0", ARTICULO))%>%
  unite(item_code, c(ARTICULO, CCIF), sep = "_", remove = FALSE)
  

Item.Codes.0 <- distinct(gastos_1, ARTICULO, CCIF)%>%
  left_join(rename(Item.Code.1, Name_Articulo = Name))%>%
  left_join(rename(Item.Code.2, Name_CCIF = Name),  by = "CCIF")%>%
  unite(item_code, c(ARTICULO, CCIF), sep = "_", remove = FALSE)%>%
  arrange(item_code)%>%
  mutate(item_code_new = 1:n())%>%
  mutate(Name_Articulo = tolower(Name_Articulo),
         Name_CCIF     = tolower(Name_CCIF))%>%
  select(item_code_new, item_code, ARTICULO, CCIF, Name_Articulo, Name_CCIF)

gastos_2 <- gastos_1 %>%
  left_join(select(Item.Codes.0, item_code, item_code_new) , by = c("item_code"))%>%
  select(hh_id, item_code_new, expenditures_year)%>%
  distinct()%>%
  group_by(hh_id, item_code_new)%>%
  summarise(expenditures_year = sum(expenditures_year))%>%
  ungroup()%>%
  rename(item_code = item_code_new)

write_csv(gastos_2, "../0_Data/1_Household Data/3_Costa Rica/1_Data_Clean/expenditures_items_Costa Rica.csv")

# Appliances

appliances <- hogar_0 %>%
  rename(hh_id = LLAVE_HOGAR)%>%
  select(hh_id, 1:100)%>%
  rename(mobile.01 = H033_LINEA_CELULAR, refrigerator.01 = H037_REFRIGERADORA,
         computer.01a = H040_COMPUTAD_PORTATIL, computer.01b = H042_COMPUTAD_ESCRIT,
         radio.01 = H046_RADIO_EQUIPO, tv.01a = H048_TV_PLASMA, tv.01b = H050_TV_CONVENCIONAL,
        tv.01c = H052_TV_CABLE, car.01a = H058_AUTOS_HOGAR, car.01b = H061_AUTOS_NEGOCIO,
         car.01c = H064_AUTOS_COMPARTIDO,
         motorcycle.01a = H059_MOTOS_HOGAR, motorcycle.01b = H062_MOTOS_NEGOCIO,
         motorcycle.01c = H065_MOTOS_COMPARTIDO)%>%
  select(hh_id, ends_with("01"), ends_with("01a"), ends_with("01b"), ends_with("01c"))%>%
  remove_all_labels()%>%
  zap_formats()

appliances[is.na(appliances)] <- 0

appliances <- appliances %>%
  mutate(tv.01         = ifelse(tv.01a > 0 | tv.01b > 0 | tv.01c > 0,1,0),
         car.01        = ifelse(car.01a > 0 | car.01b > 0 | car.01c > 0,1,0),
         motorcycle.01 = ifelse(motorcycle.01a > 0 | motorcycle.01b > 0 | motorcycle.01c > 0,1,0),
         computer.01   = ifelse(computer.01a > 0 | computer.01b > 0,1,0))%>%
  select(hh_id, ends_with("01"))

write_csv(appliances, "../0_Data/1_Household Data/3_Costa Rica/1_Data_Clean/appliances_0_1_Costa_Rica.csv")

# Codes

Gender.Code <- stack(attr(personas_2$sex_hhh, 'labels'))%>%
  rename(sex_hhh = values, Gender = ind)%>%
  write_csv(., "../0_Data/1_Household Data/3_Costa_Rica/2_Codes/Gender.Code.csv")
Ethnicity.Code <- stack(attr(personas_2$ethnicity, 'labels'))%>%
  rename(ethnicity = values, Ethnicity = ind)%>%
  write_csv(., "../0_Data/1_Household Data/3_Costa_Rica/2_Codes/Ethnicity.Code.csv")
Education.Code <- stack(attr(personas_2$edu_hhh, 'labels'))%>%
  rename(edu_hhh = values, Education = ind)%>%
  write_csv(., "../0_Data/1_Household Data/3_Costa_Rica/2_Codes/Education.Code.csv")
Industry.Code <- stack(attr(personas_2$ind_hhh, 'labels'))%>%
  rename(ind_hhh = values, Industry = ind)%>%
  write_csv(., "../0_Data/1_Household Data/3_Costa_Rica/2_Codes/Industry.Code.csv")
Cooking.Code <- stack(attr(household_1$cooking_fuel, 'labels'))%>%
  rename(cooking_fuel = values, Cooking_Fuel = ind)%>%
  write_csv(., "../0_Data/1_Household Data/3_Costa_Rica/2_Codes/Cooking.Code.csv")
Lighting.Code <- stack(attr(household_1$lighting_fuel, 'labels'))%>%
  rename(lighting_fuel = values, Lighting_Fuel = ind)%>%
  write_csv(., "../0_Data/1_Household Data/3_Costa_Rica/2_Codes/Lighting.Code.csv")
Province.Code <- stack(attr(household_1$province, 'labels'))%>%
  rename(province = values, Province = ind)%>%
  write_csv(., "../0_Data/1_Household Data/3_Costa_Rica/2_Codes/Province.Code.csv")
Toilet.Code <- stack(attr(household_1$toilet, 'labels'))%>%
  rename(toilet = values, Toilet = ind)%>%
  write_csv(., "../0_Data/1_Household Data/3_Costa_Rica/2_Codes/Toilet.Code.csv")
Water.Code <- stack(attr(household_1$water, 'labels'))%>%
  rename(water = values, Water = ind)%>%
  write_csv(., "../0_Data/1_Household Data/3_Costa_Rica/2_Codes/Water.Code.csv")

Item.Code.1 <- stack(attr(gastos_0$ARTICULO, 'labels'))%>%
  rename(ARTICULO = values, Name = ind)%>%
  arrange(ARTICULO)%>%
  write.xlsx(., "../0_Data/1_Household Data/3_Costa_Rica/2_Codes/Item.Code.Articulo.xlsx")
Item.Code.2 <- stack(attr(gastos_0$CCIF, 'labels'))%>%
  rename(CCIF = values, Name =  ind)%>%
  arrange(CCIF)%>%
  write.xlsx(., "../0_Data/1_Household Data/3_Costa_Rica/2_Codes/Item.Code.CCIF.xlsx")

