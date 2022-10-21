# Author: L. Missbach (missbach@mcc-berlin.net)

# This script aims at cleaning Bolivian household data (Encuesta Hogares 2019)

# Packages ####

if(!require("pacman")) install.packages("pacman")

p_load("haven", "Hmisc", "openxlsx", "rattle", "scales", "sjlabelled", "tidyverse")

options(scipen=999)

# Load Data ####

Vivienda_0     <- read_sav("../0_Data/1_Household Data/3_Bolivia/1_Data_Raw/Data/Base_datos-EH-2019/BD_EH19_spss/EH2019_Vivienda.sav")
Personas_0     <- read_sav("../0_Data/1_Household Data/3_Bolivia/1_Data_Raw/Data/Base_datos-EH-2019/BD_EH19_spss/EH2019_Persona.sav")
Gastos_0.1     <- read_sav("../0_Data/1_Household Data/3_Bolivia/1_Data_Raw/Data/Base_datos-EH-2019/BD_EH19_spss/EH2019_GastosAlimentarios.sav")
Gastos_0.2     <- read_sav("../0_Data/1_Household Data/3_Bolivia/1_Data_Raw/Data/Base_datos-EH-2019/BD_EH19_spss/EH2019_GastosNoAlimentarios.sav")
Equipamiento_0 <- read_sav("../0_Data/1_Household Data/3_Bolivia/1_Data_Raw/Data/Base_datos-EH-2019/BD_EH19_spss/EH2019_Equipamiento.sav")

Discriminacion <- read_sav("../0_Data/1_Household Data/3_Bolivia/1_Data_Raw/Data/Base_datos-EH-2019/BD_EH19_spss/EH2019_Discriminacion.sav")
Seguridad      <- read_sav("../0_Data/1_Household Data/3_Bolivia/1_Data_Raw/Data/Base_datos-EH-2019/BD_EH19_spss/EH2019_SeguridadAlimentaria.sav")
Turismo        <- read_sav("../0_Data/1_Household Data/3_Bolivia/1_Data_Raw/Data/Base_datos-EH-2019/BD_EH19_spss/EH2019_TurismoInterno.sav")

# Transform Data ####

# Household Information

hh_no <- Vivienda_0 %>%
  select(folio)%>%
  arrange(folio)%>%
  mutate(hh_id = 1:n())%>%
  remove_all_labels()

Vivienda_1 <- Vivienda_0 %>%
  select(folio, area, depto, s01a_10, s01a_15, s01a_16, s01a_19, s01a_25, s01a_31, factor )%>%
  rename(urban              = area, 
         district           = depto,
         water              = s01a_10,
         toilet.type.a      = s01a_15, toilet.type.b = s01a_16,
         electricity.access = s01a_19,
         cooking_fuel       = s01a_25, 
         internet.access    = s01a_31, 
         hh_weights         = factor)%>%
  left_join(hh_no, by = "folio")%>%
  select(hh_id, everything())%>%
  mutate(urban_01 = ifelse(urban == 1,1,0),
         electricity.access = ifelse(electricity.access == 1,1,0))%>%
  unite(toilet, c(toilet.type.a, toilet.type.b))%>%
  select(-urban)

# Persons

Personas_1 <- Personas_0 %>%
  select(folio, s02a_02, s02a_03, s02a_05, s02a_07_1, s03a_04, s03a_04npioc,
         s05a_02a, s06b_12a_cod)%>%
  rename(sex_hhh = s02a_02, age_hhh = s02a_03, hhh = s02a_05, language = s02a_07_1, ethnicity.2 = s03a_04, ethnicity.3 = s03a_04npioc,
         edu_hhh = s05a_02a, ind_hhh = s06b_12a_cod)

Personas_1.1 <- Personas_1 %>%
  select(folio, age_hhh)%>%
  mutate(adults   = ifelse(age_hhh > 15,1,0),
         children = ifelse(age_hhh < 16,1,0))%>%
  group_by(folio)%>%
  summarise(hh_size  = n(),
            adults   = sum(adults),
            children = sum(children))%>%
  ungroup()

Personas_1.2 <- Personas_1 %>%
  filter(hhh == 1)%>%
  select(-hhh)%>%
  unite(ethnicity, c(ethnicity.2, ethnicity.3), sep = "_")

# Equipamiento ####

Equipamiento_1 <- Equipamiento_0 %>%
  select(folio, item, s10c_14)%>%
  mutate(value = ifelse(s10c_14 == 1,1,0))%>%
  mutate(appliance = ifelse(item == 3, "stove.01",
                            ifelse(item == 4, "microwaeve.01",
                                   ifelse(item == 5, "refrigerator.01",
                                          ifelse(item == 6, "ac.01",
                                                 ifelse(item == 7, "computer.01",
                                                        ifelse(item == 8, "radio.01",
                                                               ifelse(item == 10 | item == 11, "tv.01",
                                                                      ifelse(item == 12, "washing_machine.01",
                                                                             ifelse(item == 14, "motorcycle.01",
                                                                                    ifelse(item == 15, "car.01", "different")))))))))))%>%
  filter(appliance != "different")%>%
  group_by(folio, appliance)%>%
  summarise(value = max(value))%>%
  ungroup()%>%
  pivot_wider(names_from = "appliance", values_from = "value")

# Transfers

Personas_Transfers_1 <- Personas_0 %>%
  select(folio, nro, starts_with("s07"), s04a_09a)%>%
  select(folio, nro, s07a_01a, s07a_01b, s07a_01c, s07a_01d, s07a_01e0,
         s07b_05da, s07b_05db,
         s07b_05ea, s07b_05eb,
         s04a_09a)%>%
  mutate(s04a_09a = as.numeric(s04a_09a))%>%
  mutate_at(vars(-folio), ~replace_na(.,0))%>%
  mutate(s07b_05d = ifelse(s07b_05db == 4, s07b_05da*2,
                           ifelse(s07b_05db == 8, s07b_05da,0)),
         s07b_05e = ifelse(s07b_05eb == 4, s07b_05ea*2,
                           ifelse(s07b_05eb == 8, s07b_05ea,0)))%>%
  mutate(inc_gov_cash = 0,
         inc_gov_monetary = s07b_05d + s07b_05e + s07a_01a + s07a_01b + s07a_01c + s07a_01d + s07a_01e0 + s04a_09a)%>%
  group_by(folio)%>%
  summarise(inc_gov_cash = sum(inc_gov_cash),
            inc_gov_monetary = sum(inc_gov_monetary))%>%
  ungroup()

# Expenditures ####

Vivienda_2 <- Vivienda_0 %>%
  select(folio, s01a_04, s01a_12, s01a_20, 
         s01a_22_1b, s01a_22_2b, s01a_22_3b, s01a_22_4b,
         s01a_23_1b, s01a_23_2b, s01a_23_3b, s01a_23_4b, s01a_23_5b,
         s01a_25, s01a_26,
         s01a_30)

Vivienda_2.1 <- Vivienda_2 %>%
  select(folio, s01a_25, s01a_26)%>%
  mutate(item_code = paste0("cook_",s01a_25))%>%
  mutate(expenditures_year = s01a_26*12)%>%
  select(folio, item_code, expenditures_year)

Vivienda_2.2 <- Vivienda_2 %>%
  select(- s01a_25, - s01a_26)%>%
  pivot_longer(-folio, names_to = "item_code", values_to = "expenditures_year")%>%
  filter(!is.na(expenditures_year))%>%
  select(folio, item_code, expenditures_year)%>%
  mutate(expenditures_year = ifelse(item_code %in% c("s01a_04", "s01a_12", "s01a_20", "s01a_30"),expenditures_year*12,expenditures_year))

Personas_2.1 <- Personas_0 %>%
  select(folio, starts_with("s04a_05"))%>%
  pivot_longer(-folio, names_to = "item_code", values_to = "expenditures_year")%>%
  filter(expenditures_year > 0)

Expenditures_0.1.1 <- Gastos_0.1 %>%
  select(folio, producto, s10a_02, s10a_04)%>%
  filter(!is.na(s10a_04))%>%
  mutate(expenditures_year = ifelse(s10a_02 == 1, s10a_04*365,
                                    ifelse(s10a_02 == 2, s10a_04*365/3,
                                           ifelse(s10a_02 == 3, s10a_04*52/2,
                                                  ifelse(s10a_02 == 4, s10a_04*52,
                                                         ifelse(s10a_02 == 5, s10a_04*26,
                                                                ifelse(s10a_02 == 6, s10a_04*12,
                                                                       ifelse(s10a_02 == 7, s10a_04*4,
                                                                              ifelse(s10a_02 == 8, s10a_04*2,
                                                                                     ifelse(s10a_02 == 9, s10a_04,0))))))))))%>%
  select(folio, producto, expenditures_year)%>%
  rename(item_code = producto)

Expenditures_0.1.2 <- Gastos_0.1 %>%
  select(folio, producto, s10a_05, s10a_07, s10a_09)%>%
  filter(!is.na(s10a_07) | !is.na(s10a_09))%>%
  mutate(expenditures_sp_year = ifelse(s10a_05 == 1, s10a_07*365,
                                    ifelse(s10a_05 == 2, s10a_07*365/3,
                                           ifelse(s10a_05 == 3, s10a_07*52/2,
                                                  ifelse(s10a_05 == 4, s10a_07*52,
                                                         ifelse(s10a_05 == 5, s10a_07*26,
                                                                ifelse(s10a_05 == 6, s10a_07*12,
                                                                       ifelse(s10a_05 == 7, s10a_07*4,
                                                                              ifelse(s10a_05 == 8, s10a_07*2,
                                                                                     ifelse(s10a_05 == 9, s10a_07,
                                                                                            ifelse(s10a_05 == 0, s10a_09*12,0)))))))))))%>%
  select(folio, producto, expenditures_sp_year)%>%
  rename(item_code = producto)

Expenditures_0.2.1 <- Gastos_0.2 %>%
  select(folio, starts_with("s"))

Expenditures_0.2.1.1 <- Expenditures_0.2.1 %>%
  select(folio, starts_with("s10b_10_"))%>%
  mutate_at(.vars = vars(s10b_10_01:s10b_10_32), .funs = list(y = ~.*12))%>%
  select(-(s10b_10_01:s10b_10_32))

colnames(Expenditures_0.2.1.1) <- c("folio", seq(1001,1032, by = 1))

Expenditures_0.2.1.2 <- Expenditures_0.2.1.1 %>%
  pivot_longer(-folio, names_to = "item_code", values_to = "expenditures_year")%>%
  filter(expenditures_year > 0)

Expenditures_0.2.1.3 <- Expenditures_0.2.1 %>%
  select(folio, starts_with("s10b_11_"))%>%
  mutate_at(.vars = vars(s10b_11_01:s10b_11_08), .funs = list(y = ~.*4))%>%
  select(-(s10b_11_01:s10b_11_08))

colnames(Expenditures_0.2.1.3) <- c("folio", seq(1101,1108, by = 1))

Expenditures_0.2.1.4 <- Expenditures_0.2.1.3 %>%
  pivot_longer(-folio, names_to = "item_code", values_to = "expenditures_year")%>%
  filter(expenditures_year > 0)

Expenditures_0.2.1.5 <- Expenditures_0.2.1 %>%
  select(folio, starts_with("s10b_12_"))%>%
  mutate_at(.vars = vars(s10b_12_01:s10b_12_03), .funs = list(y = ~.*2))%>%
  select(-(s10b_12_01:s10b_12_03))

colnames(Expenditures_0.2.1.5) <- c("folio", seq(1201,1203, by = 1))

Expenditures_0.2.1.6 <- Expenditures_0.2.1.5 %>%
  pivot_longer(-folio, names_to = "item_code", values_to = "expenditures_year")%>%
  filter(expenditures_year > 0)

Expenditures_0.2.1.7 <- Expenditures_0.2.1 %>%
  select(folio, starts_with("s10b_13_"))

colnames(Expenditures_0.2.1.7) <- c("folio", seq(1301,1314,by = 1))

Expenditures_0.2.1.8 <- Expenditures_0.2.1.7 %>%
  pivot_longer(-folio, names_to = "item_code", values_to = "expenditures_year")%>%
  filter(expenditures_year > 0)

Expenditures_joint <- Expenditures_0.1.1 %>%
  full_join(Expenditures_0.1.2)%>%
  remove_all_labels()%>%
  mutate(item_code = as.character(item_code))%>%
  bind_rows(Expenditures_0.2.1.2)%>%
  bind_rows(Expenditures_0.2.1.4)%>%
  bind_rows(Expenditures_0.2.1.6)%>%
  bind_rows(Expenditures_0.2.1.8)%>%
  bind_rows(Vivienda_2.1)%>%
  bind_rows(Vivienda_2.2)%>%
  bind_rows(Personas_2.1)%>%
  mutate(expenditures_year    = ifelse(is.na(expenditures_year),    0, expenditures_year),
         expenditures_sp_year = ifelse(is.na(expenditures_sp_year), 0, expenditures_sp_year))%>%
  left_join(hh_no)%>%
  select(hh_id, item_code, everything())%>%
  select(-folio)%>%
  group_by(hh_id, item_code)%>%
  summarise(expenditures_year = sum(expenditures_year),
            expenditures_sp_year = sum(expenditures_sp_year))%>%
  ungroup()

# later: remove All labels 

household_information <- left_join(Vivienda_1, Personas_1.1)%>%
  left_join(Personas_1.2)%>%
  left_join(Personas_Transfers_1)%>%
  select(-folio)%>%
  remove_all_labels()

appliances_0 <- Equipamiento_1 %>%
  left_join(hh_no)%>%
  select(-folio)%>%
  select(hh_id, everything())%>%
  remove_all_labels()

expenditure_information <- Expenditures_joint %>%
  remove_all_labels()

write_csv(expenditure_information, "../0_Data/1_Household Data/3_Bolivia/1_Data_Clean/expenditures_items_Bolivia.csv")
write_csv(household_information,   "../0_Data/1_Household Data/3_Bolivia/1_Data_Clean/household_information_Bolivia.csv")
write_csv(appliances_0,            "../0_Data/1_Household Data/3_Bolivia/1_Data_Clean/appliances_0_1_Bolivia.csv")

# Codes

Item.Codes <- distinct(expenditure_information, item_code)%>%
  arrange(item_code)
Item.Codes.A <- stack(attr(Gastos_0.1$producto, 'labels'))%>%
  rename(item_code = values, item_name = ind)%>%
  mutate(Type = "A")%>%
  mutate(item_code = as.character(item_code),
         number = as.numeric(item_code))

Item.Codes.0 <- Item.Codes %>%
  left_join(Item.Codes.A)%>%
  arrange(number, item_code)%>%
  select(-number, -Type)

#write.xlsx(Item.Codes.0, "../0_Data/1_Household Data/3_Bolivia/3_Matching_Tables/Item_Code_Description_Bolivia.xlsx")

District.Code <- stack(attr(Vivienda_1$district, 'labels'))%>%
  rename(district = values, District = ind)%>%
  write_csv("../0_Data/1_Household Data/3_Bolivia/2_Codes/District.Code.csv")
Cooking.Code <- stack(attr(Vivienda_1$cooking_fuel, 'labels'))%>%
  rename(cooking_fuel = values, Cooking_Fuel = ind)%>%
  write_csv("../0_Data/1_Household Data/3_Bolivia/2_Codes/Cooking.Code.csv")
Water.Code <- stack(attr(Vivienda_1$water, 'labels'))%>%
  rename(water = values, Water = ind)%>%
  write_csv("../0_Data/1_Household Data/3_Bolivia/2_Codes/Water.Code.csv")
Toilet.Code.A <- stack(attr(Vivienda_0$s01a_15, 'labels'))%>%
  mutate(values = as.character(values))
Toilet.Code.B <- stack(attr(Vivienda_0$s01a_16, 'labels'))%>%
  mutate(values = as.character(values))
Toilet.Code <- distinct(Vivienda_1, toilet)%>%
  arrange(toilet)%>%
  separate(toilet, c("s01a_15", "s01a_16"), sep = "_", remove = FALSE)%>%
  left_join(Toilet.Code.A, by = c("s01a_15" = "values"))%>%
  left_join(Toilet.Code.B, by = c("s01a_16" = "values"))%>%
  unite(Toilet, c("ind.x", "ind.y"), sep = " ")%>%
  select(toilet, Toilet)%>%
  write_csv("../0_Data/1_Household Data/3_Bolivia/2_Codes/Toilet.Code.csv")
Education.Code <- stack(attr(Personas_1$edu_hhh, 'labels'))%>%
  rename(edu_hhh = values, Education = ind)%>%
  write_csv("../0_Data/1_Household Data/3_Bolivia/2_Codes/Education.Code.csv")
Industry.Code <- stack(attr(Personas_1$ind_hhh, 'labels'))%>%
  rename(ind_hhh = values, Industry = ind)%>%
  write_csv("../0_Data/1_Household Data/3_Bolivia/2_Codes/Industry.Code.csv")
Gender.Code <- stack(attr(Personas_1$sex_hhh, 'labels'))%>%
  rename(sex_hhh = values, Gender = ind)%>%
  write_csv("../0_Data/1_Household Data/3_Bolivia/2_Codes/Gender.Code.csv")
Language.Code <- stack(attr(Personas_1$language, 'labels'))%>%
  rename(language = values, Language = ind)%>%
  write_csv("../0_Data/1_Household Data/3_Bolivia/2_Codes/Language.Code.csv")
Ethnicity.Code.A <- stack(attr(Personas_0$s03a_04, 'labels'))%>%
  mutate(values = as.character(values))
Ethnicity.Code.B <- stack(attr(Personas_0$s03a_04npioc, 'labels'))%>%
  mutate(values = as.character(values))
Ethnicity.Code <- distinct(Personas_1.2, ethnicity)%>%
  arrange(ethnicity)%>%
  separate(ethnicity, c("s03a_04", "s03a_04npioc"), sep = "_", remove = FALSE)%>%
  left_join(Ethnicity.Code.A, by = c("s03a_04" = "values"))%>%
  left_join(Ethnicity.Code.B, by = c("s03a_04npioc" = "values"))%>%
  unite(Ethnicity, c("ind.x", "ind.y"), sep = " ")%>%
  select(ethnicity, Ethnicity)%>%
  write_csv("../0_Data/1_Household Data/3_Bolivia/2_Codes/Ethnicity.Code.csv")

Ethnicity.Code <- read_csv("../0_Data/1_Household Data/3_Bolivia/2_Codes/Ethnicity.Code.csv")%>%
  mutate(Ethnicity.Code = 1:n())

household_information <- read_csv("../0_Data/1_Household Data/3_Bolivia/1_Data_Clean/household_information_Bolivia.csv")%>%
  left_join(select(Ethnicity.Code, Ethnicity.Code, ethnicity))%>%
  select(-ethnicity)%>%
  rename(ethnicity = Ethnicity.Code)

write_csv(household_information, "../0_Data/1_Household Data/3_Bolivia/1_Data_Clean/household_information_Bolivia.csv")
Ethnicity.Code <- rename(Ethnicity.Code, Ethnicity_0 = ethnicity, ethnicity = Ethnicity.Code)
write_csv(Ethnicity.Code, "../0_Data/1_Household Data/3_Bolivia/2_Codes/Ethnicity.Code.csv")
