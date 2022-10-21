if(!require("pacman")) install.packages("pacman")

p_load("haven", "Hmisc", "openxlsx", "rattle", "scales", "tidyverse")

options(scipen=999)

# Load Data

Vivienda  <- read_dta("../0_Data/1_Household Data/3_Paraguay/1_Data_Raw/Vivienda.dta")
Poblacion <- read_dta("../0_Data/1_Household Data/3_Paraguay/1_Data_Raw/Poblacion.dta")
#Gastos_S <- read_dta("../0_Data/1_Household Data/3_Paraguay/1_Data_Raw/Gastos_Sum.dta")
Gastos_D <- read_dta("../0_Data/1_Household Data/3_Paraguay/1_Data_Raw/Agregado.dta")

# Reg01U might be needed
# Reg02ED might be needed
# Reg02SA might be needed

reg01u <- read_dta("../0_Data/1_Household Data/3_Paraguay/1_Data_Raw/reg01u.dta")
reg02e <- read_dta("../0_Data/1_Household Data/3_Paraguay/1_Data_Raw/reg02ed.dta")
reg02s <- read_dta("../0_Data/1_Household Data/3_Paraguay/1_Data_Raw/reg02sa.dta")

# Appliances in reg20

reg20 <- read_dta("../0_Data/1_Household Data/3_Paraguay/1_Data_Raw/reg20.dta")

# Transform Data

vivienda_1 <- Vivienda %>%
  unite(hh_id, c(upm, nvivi, nhoga), sep = "0", remove = FALSE)%>%
  mutate(urban_01 = ifelse(dominio == 1,1,0),
         electricity.access = ifelse(v06 == 1,1,0))%>%
  rename(province = dptorep, hh_weights = fex, water = v07, toilet = v13,
         cooking_fuel = v15)%>%
  select(hh_id, hh_weights, water, toilet, cooking_fuel, electricity.access, urban_01, province)

# Vivienda has some expenditure information, but not many

personas_1 <- Poblacion %>%
  unite(hh_id, c(upm, nvivi, nhoga), sep = "0", remove = FALSE)%>%
  filter(p02 == 1)%>%
  rename(age_hhh = edad, sex_hhh = p05, language = ed01, ind_hhh = b02crec1)%>%
  mutate(edu_hhh = ifelse(nchar(ed04)==3,str_sub(ed04,1,1),
                          ifelse(nchar(ed04)==4,str_sub(ed04,1,2),NA)))%>%
  select(hh_id, age_hhh, sex_hhh, language, ind_hhh, edu_hhh)

personas_2 <- Poblacion %>%
  unite(hh_id, c(upm, nvivi, nhoga), sep = "0", remove = FALSE)%>%
  mutate(adults   = ifelse(edad > 15,1,0),
         children = ifelse(edad < 16,1,0))%>%
  group_by(hh_id)%>%
  summarise(adults   = sum(adults),
            children = sum(children),
            hh_size = n())%>%
  ungroup()

personas_3 <- Poblacion %>%
  unite(hh_id, c(upm, nvivi, nhoga), sep = "0", remove = FALSE)%>%
  select(hh_id, dd0108, dd0109)%>%
  mutate_at(vars(-hh_id), list(~ ifelse(. > 99999999990 | is.na(.),0,.)))%>%
  mutate(inc_gov_monetary = (dd0108 + dd0109)*12,
         inc_gov_cash = 0)%>%
  group_by(hh_id)%>%
  summarise(inc_gov_monetary = sum(inc_gov_monetary),
            inc_gov_cash     = sum(inc_gov_cash))%>%
  ungroup()

household_information <- left_join(vivienda_1, personas_1)%>%
  left_join(personas_2)%>%
  left_join(personas_3)

write_csv(household_information, "../0_Data/1_Household Data/3_Paraguay/1_Data_Clean/household_information_Paraguay.csv")

# Codes

Education.Code <- distinct(household_information, edu_hhh)%>%
  mutate(edu_hhh = as.numeric(edu_hhh))%>%
  arrange(edu_hhh)%>%
  bind_cols(Education = c("Sin Instruccion", "Educ. Especial 1 a 9", "Educ. Escolar Basica 1 a 6",
                          "Educ. Escolar Basica 7 a 9",
                          "Secundaria Ciclo Basico", "Bachillerato Humanistico/Cientifico", "Bachillerato Technico / Comercial",
                          "Bachillerato a Distancia", "Educ. Media Cientifica", "Educ. MEdia Tecnica", "Educ. Basica Bilingue de Jovenes y Adultos",
                          "Educ. Media a Distancia para Jovenes y Adultos", "Educ. Basica Alternativa de Jovenes y Adultos",
                          "Educ. Madia Alternativa de Jovenes y Adultos", "Programas de ALfabetizacion",
                          "Tecnica Superior", "Formacion Docente", "Profesionalizacion Docente",
                          "Form. Militar/Policial", "Superior Universitario", "Not known", NA))%>%
  write_csv(., "../0_Data/1_Household Data/3_Paraguay/2_Codes/Education.Code.csv")

Industry.Code <- stack(attr(household_information$ind_hhh, 'labels'))%>%
  rename(ind_hhh = values, Industry = ind)%>%
  write_csv(., "../0_Data/1_Household Data/3_Paraguay/2_Codes/Industry.Code.csv")

Gender.Code <- stack(attr(household_information$sex_hhh, 'labels'))%>%
  rename(sex_hhh = values, Gender = ind)%>%
  write_csv(., "../0_Data/1_Household Data/3_Paraguay/2_Codes/Gender.Code.csv")

Language.Code <- stack(attr(household_information$language, 'labels'))%>%
  rename(language = values, Language = ind)%>%
  write_csv(., "../0_Data/1_Household Data/3_Paraguay/2_Codes/Language.Code.csv")

Province.Code <- stack(attr(household_information$province, 'labels'))%>%
  rename(province = values, Province = ind)%>%
  write_csv(., "../0_Data/1_Household Data/3_Paraguay/2_Codes/Province.Code.csv")

Water.Code <- stack(attr(household_information$water, 'labels'))%>%
  rename(water = values, Water = ind)%>%
  write_csv(., "../0_Data/1_Household Data/3_Paraguay/2_Codes/Water.Code.csv")

Toilet.Code <- stack(attr(household_information$toilet, 'labels'))%>%
  rename(toilet = values, Toilet = ind)%>%
  write_csv(., "../0_Data/1_Household Data/3_Paraguay/2_Codes/Toilet.Code.csv")

Cooking.Code <- stack(attr(household_information$cooking_fuel, 'labels'))%>%
  rename(cooking_fuel = values, Cooking_Fuel = ind)%>%
  write_csv(., "../0_Data/1_Household Data/3_Paraguay/2_Codes/Cooking.Code.csv")

# Expenditures

exp_1 <- Gastos_D %>%
  unite(hh_id, c(upm, nvivi, nhoga), sep = "0", remove = FALSE)%>%
  filter(ga01 == 1)%>%
  filter(ga02a != 88)%>%
  mutate(sp = ifelse(ga02a != 1,1,0))%>%
  rename(frequency = ga03f, expenditures = dga04, expenditures_sp = dga05g, item_code = ga01c)%>%
  mutate(expenditures_year = expenditures*12,
         expenditures_sp_year = expenditures_sp*12)%>%
  select(hh_id, item_code, expenditures_year, expenditures_sp_year)%>%
  mutate_at(vars(starts_with("exp")), list(~ ifelse(is.na(.),0,.)))%>%
  filter(expenditures_year > 0 | expenditures_sp_year >0)%>%
  filter(expenditures_year < 1199999999980)%>%
  mutate(item_code_6 = str_sub(item_code,1,6),
         item_code_3 = str_sub(item_code,1,3))%>%
  mutate(item_code = ifelse(item_code_3 == 306 | item_code_6 == 502201, item_code, item_code_6))%>%
  select(hh_id, item_code, expenditures_year, expenditures_sp_year)%>%
  group_by(hh_id, item_code)%>%
  summarise(expenditures_year = sum(expenditures_year),
            expenditures_sp_year = sum(expenditures_sp_year))%>%
  ungroup()

exp_2 <- reg01u %>%
  unite(hh_id, c(upm, nvivi, nhoga), sep = "0", remove = FALSE)%>%
  select(hh_id, lv28, v28m1, v28m2, v28m3)%>%
  mutate_at(vars(-hh_id), list(~ ifelse(is.na(.),0,.)))%>%
  rename(item_code = lv28)%>%
  mutate(expenditures_year = v28m1*12,
         expenditures_sp_year = (v28m2+v28m3)*12)%>%
  group_by(hh_id, item_code)%>%
  summarise(expenditures_year    = sum(expenditures_year),
            expenditures_sp_year = sum(expenditures_sp_year))%>%
  ungroup()%>%
  filter(expenditures_year >0 | expenditures_sp_year > 0)%>%
  mutate(item_code = paste0("B", item_code))

exp_3 <- reg02e %>%
  unite(hh_id, c(upm, nvivi, nhoga), sep = "0", remove = FALSE)%>%
  select(hh_id, l15, ded11) %>% # annualized
  rename(item_code = l15, expenditures_year = ded11)%>%
  filter(!is.na(expenditures_year))%>%
  mutate(item_code = paste0("C", item_code))%>%
  group_by(hh_id, item_code)%>%
  summarise(expenditures_year = sum(expenditures_year),
            expenditures_sp_year = 0)%>%
  ungroup()

exp_4 <- reg02s %>%
  unite(hh_id, c(upm, nvivi, nhoga), sep = "0", remove = FALSE)%>%
  select(hh_id, l16, s12)%>%
  rename(item_code = l16)%>%
  mutate(expenditures_year = s12*4)%>%
  filter(!is.na(expenditures_year))%>%
  mutate(item_code = paste0("D", item_code))%>%
  group_by(hh_id, item_code)%>%
  summarise(expenditures_year = sum(expenditures_year),
            expenditures_sp_year = 0)%>%
  ungroup()

exp_5 <- Vivienda %>%
  unite(hh_id, c(upm, nvivi, nhoga), sep = "0", remove = FALSE)%>%
  select(hh_id, v19, v20, v23g, v24g, v25g)%>%
  pivot_longer(-hh_id, names_to = "item_code", values_to = "expenditures_year")%>%
  mutate(expenditures_year = ifelse(item_code == "v19" | item_code == "v20", expenditures_year*12, expenditures_year))%>%
  filter(!is.na(expenditures_year) & expenditures_year < 99999999998)

exp_total <- bind_rows(exp_1, exp_2, exp_3, exp_4, exp_5)%>%
  arrange(hh_id, item_code)%>%
  filter(expenditures_year < 280000001)%>%
  mutate(expenditures_sp_year = ifelse(expenditures_sp_year > 1199999999981,0, expenditures_sp_year))

write_csv(exp_total, "../0_Data/1_Household Data/3_Paraguay/1_Data_Clean/expenditures_items_Paraguay.csv")

Item.Codes <- distinct(Gastos_D, ga01c, ga01e)%>%
  #filter(ga01c %in% exp_1$item_code)%>%
  arrange(ga01c)%>%
  mutate(characters = nchar(ga01c),
         item_code_6 = str_sub(ga01c,1,6),
         item_code_4 = str_sub(ga01c,1,4),
         item_code_3 = str_sub(ga01c,1,3))%>%
  mutate(item_code = ifelse(item_code_3 == 306 | item_code_6 == 502201, ga01c,item_code_6))

Item.Codes.Raw <- read.xlsx("../0_Data/1_Household Data/3_Paraguay/9_Documentation/Item_Codes_Raw.xlsx")%>%
  select(Código, Descripción)%>%
  mutate(item_name = tolower(Descripción))%>%
  mutate(item_code = Código)%>%
  mutate(item_code = as.character(Código))%>%
  select(item_code, item_name)

Item.Codes.2 <- left_join(Item.Codes, Item.Codes.Raw)%>%
  select(item_code, item_name)%>%
  distinct()%>%
  arrange(item_code)


Item.Codes.2a <- distinct(reg01u, lv28)%>%
  arrange(lv28)%>%
  rename(item_code = lv28)%>%
  mutate(item_code = paste0("B", item_code))
Item.Codes.2b <- stack(attr(reg02e$l15, 'labels'))%>%
  rename(item_code = values, item_name = ind)%>%
  mutate(item_code = paste0("C", item_code))
Item.Codes.2c <- stack(attr(reg02s$l16, 'labels'))%>%
  rename(item_code = values, item_name = ind)%>%
  mutate(item_code = paste0("D", item_code))
Item.Codes.2d <- distinct(exp_5, item_code)%>%
  arrange(item_code)

Item.Codes.final <- bind_rows(Item.Codes.2, Item.Codes.2a, Item.Codes.2b,
                              Item.Codes.2c, Item.Codes.2d)

# write.xlsx(Item.Codes.final, "../0_Data/1_Household Data/3_Paraguay/3_Matching_Tables/Item_Code_Description_Paraguay_new.xlsx")


# appliances

appliances_01 <- reg20 %>%
  unite(hh_id, c(upm, nvivi, nhoga), sep = "0", remove = FALSE)%>%
  select(hh_id, codigo, eq01)%>%
  mutate(tv.01              = ifelse(eq01 == 1 & (codigo == 3 | codigo == 54),1,0),
         ac.01              = ifelse(eq01 == 1 & (codigo == 7),1,0),
         car.01             = ifelse(eq01 == 1 & (codigo == 22 | codigo == 23 | codigo == 24),1,0),
         motorcycle.01      = ifelse(eq01 == 1 & codigo == 21,1,0),
         refrigerator.01    = ifelse(eq01 == 1 & (codigo == 28 | codigo == 4 | codigo == 29 | codigo == 39 | codigo == 40 | codigo == 41),1,0),
         radio.01           = ifelse(eq01 == 1 & codigo == 19,1,0),
         microwave.01       = ifelse(eq01 == 1 & codigo == 16,1,0),
         dryer.01           = ifelse(eq01 == 1 & codigo == 15,1,0),
         computer.01        = ifelse(eq01 == 1 & (codigo == 9 | codigo == 10),1,0),
         stove.g.01         = ifelse(eq01 == 1 & codigo == 1,1,0),
         stove.e.10         = ifelse(eq01 == 1 & (codigo == 2 | codigo == 11),1,0),
         vacuum.01          = ifelse(eq01 == 1 & codigo == 14,1,0),
         washing_machine.01 = ifelse(eq01 == 1 & (codigo == 5 | codigo == 6),1,0))%>%
  select(hh_id, ends_with(".01"))%>%
  group_by(hh_id)%>%
  summarise_all(list(~ sum(.)))%>%
  ungroup()%>%
  mutate_at(vars(-hh_id), list(~ ifelse(. > 1,1,.)))

write_csv(appliances_01, "../0_Data/1_Household Data/3_Paraguay/1_Data_Clean/appliances_0_1_Paraguay.csv")

# Test

expenditures_items <- read_csv("../0_Data/1_Household Data/3_Paraguay/1_Data_Clean/expenditures_items_Paraguay.csv")

exp_0 <- expenditures_items %>%
  filter(item_code == "D11" | item_code == "401109")%>%
  group_by(hh_id)%>%
  mutate(number = n())%>%
  ungroup()
