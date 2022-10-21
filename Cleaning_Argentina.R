if(!require("pacman")) install.packages("pacman")

p_load("haven", "Hmisc", "openxlsx", "rattle", "scales", "tidyverse")


equipamiento <- read_delim(sprintf("%s/engho2018_equipamiento/engho2018_equipamiento.txt", path_0), "|", escape_double = FALSE, trim_ws = TRUE)
gastos       <- read_delim(sprintf("%s/engho2018_gastos/engho2018_gastos.txt", path_0), "|", escape_double = FALSE, trim_ws = TRUE)
# habitos      <- read_delim(sprintf("%s/engho2018_habitos/engho2018_habitos.txt", path_0), "|", escape_double = FALSE, trim_ws = TRUE)
hogares      <- read_delim(sprintf("%s/engho2018_hogares/engho2018_hogares.txt", path_0), "|", escape_double = FALSE, trim_ws = TRUE)
# personas     <- read_delim(sprintf("%s/engho2018_personas/engho2018_personas.txt", path_0), "|", escape_double = FALSE, trim_ws = TRUE, col_types = cols(cp23_99 = col_character()))
# replicas     <- read_delim(sprintf("%s/engho2018_replicas/engho2018_replicas.txt", path_0), "|", escape_double = FALSE, trim_ws = TRUE)

# Data Transformation ####

hogares_1 <- hogares %>%
  rename(hh_id = id, province = provincia, district = subregion, hh_weights = pondera)%>%
  select(hh_id, province, district, hh_weights, ch07, ch09, ch11, ch15, ch20, jniveled,  jsexo, jocupengh, cantmiem, menor18, ingtoth)%>%
  rename(income_year = ingtoth, children = menor18, hh_size = cantmiem, sex_hhh = jsexo, ind_hhh = jocupengh, edu_hhh = jniveled, heating_fuel = ch20, 
         cooking_fuel = ch15, toilet = ch11, water = ch09, electricity = ch07)%>%
  mutate(adults = hh_size - children)

write_csv(hogares_1, "../0_Data/1_Household Data/3_Argentina/1_Data_Clean/household_information_Argentina.csv")


appliances_0 <- hogares %>%
  select(id, propauto, ch21)%>%
  mutate(ac.01  = ifelse(ch21     == 1,1,0),
         car.01 = ifelse(propauto == 1,0,1))%>%
  rename(hh_id = id)%>%
  select(hh_id, ac.01, car.01)%>%
  pivot_longer(ac.01:car.01, names_to = "article", values_to ="ownership")

# articulos_eq <- read_delim("../0_Data/1_Household Data/3_Argentina/9_Documentation/engho2018_articulos_equipamiento.txt", "|", escape_double = FALSE, trim_ws = FALSE)
# 
# equipamiento_1 <- equipamiento %>%
#   select(id, articulo, cg44)%>%
#   left_join(articulos_eq)
# 
# eq_1 <- equipamiento_1 %>%
#   distinct(articulo, articulo_desc)

# write.xlsx(eq_1, "../0_Data/1_Household Data/3_Argentina/9_Documentation/Appliances_Info.xlsx")

appliances_info <- read.xlsx("../0_Data/1_Household Data/3_Argentina/9_Documentation/Appliances_Info.xlsx")%>%
  filter(!is.na(article))%>%
  select(articulo, article)

equipamiento_1 <- equipamiento %>%
  select(id, articulo, cg44)%>%
  left_join(appliances_info)%>%
  rename(hh_id = id)%>%
  mutate(ownership = ifelse(cg44 == 3 | cg44 == 99 | is.na(cg44), 0, 1))%>%
  select(hh_id, article, ownership)%>%
  filter(!is.na(article))%>%
  bind_rows(appliances_0)%>%
  group_by(hh_id, article)%>%
  summarise(ownership = max(ownership))%>%
  ungroup()%>%
  pivot_wider(names_from = article, values_from = ownership, values_fill = 0)

write_csv(equipamiento_1, "../0_Data/1_Household Data/3_Argentina/1_Data_Clean/appliances_0_1_Argentina.csv")

# Consumption ####

gastos_1 <- gastos %>%
  select(id, r_imputado, monto, forma_pago, articulo:division)%>% # monto is anualised
  filter(r_imputado != 1)%>% # delete the imputed
  mutate(self_produced = ifelse(forma_pago == 5 | forma_pago == 8 | forma_pago == 9,1,0))%>%
  filter(self_produced == 0)%>%
  select(id, articulo, monto)%>%
  rename(hh_id = id, expenditures_year = monto, item_code = articulo)%>%
  group_by(hh_id, item_code)%>%
  summarise(expenditures_year = sum(expenditures_year))%>%
  ungroup()

articulos <- read_delim("../0_Data/1_Household Data/3_Argentina/9_Documentation/engho2018_articulos.txt", "|", escape_double = FALSE, trim_ws = FALSE)%>%
  select(articulo, articulo_desc)

gastos_2 <- distinct(gastos_1, item_code)%>%
  left_join(articulos, by = c("item_code" = "articulo"))%>%
  arrange(item_code)

write.xlsx(gastos_2, "../0_Data/1_Household Data/3_Argentina/9_Documentation/Item_Codes.xlsx")  
write.xlsx(articulos, "../0_Data/1_Household Data/3_Argentina/9_Documentation/Articulos_Raw_Description.xlsx")

write_csv(gastos_1, "../0_Data/1_Household Data/3_Argentina/1_Data_Clean/expenditures_items_Argentina.csv")

# Updates to Codes
  
item_codes <- read.xlsx("../0_Data/1_Household Data/3_Argentina/9_Documentation/Item_Codes.xlsx")%>%
  mutate(item_code_new = 1:n())

write.xlsx(item_codes, "../0_Data/1_Household Data/3_Argentina/9_Documentation/Item_Codes_new.xlsx")

expenditures_items <- read_csv("../0_Data/1_Household Data/3_Argentina/1_Data_Clean/Old/expenditures_items_Argentina.csv")%>%
  left_join(select(item_codes, item_code, item_code_new))%>%
  select(-item_code)%>%
  rename(item_code = item_code_new)

write_csv(expenditures_items, "../0_Data/1_Household Data/3_Argentina/1_Data_Clean/expenditures_items_Argentina.csv")

# Updates

expenditures_items    <- read_csv("../0_Data/1_Household Data/3_Argentina/1_Data_Clean/expenditures_items_Argentina.csv")
household_information <- read_csv("../0_Data/1_Household Data/3_Argentina/1_Data_Clean/household_information_Argentina.csv")

hh_ids <- distinct(expenditures_items, hh_id)

household_information <- household_information %>%
  filter(hh_id %in% hh_ids$hh_id)

write_csv(household_information, "../0_Data/1_Household Data/3_Argentina/1_Data_Clean/household_information_Argentina.csv")

data_clean <- read_dta("../0_Data/1_Household Data/3_Argentina/1_Data_Clean/ARG_ENGHO_2017-2018.dta")

household_information <- read_csv("../0_Data/1_Household Data/3_Argentina/1_Data_Clean/household_information_Argentina.csv")

household_information <- household_information %>%
  mutate(electricity.access = ifelse(electricity == 1 | electricity == 2 | electricity == 3,1,0))%>%
  select(-electricity)

write_csv(household_information, "../0_Data/1_Household Data/3_Argentina/1_Data_Clean/household_information_Argentina.csv")


# Codes

water.code   <- distinct(hogares_1, water)%>%
  arrange(water)%>%
  mutate(Water= c("Public network (running water)?","Drilling with a motor pump?","Hand pump drilling?","Cistern or well?","Tanker transport?","Rainwater, river, canal, stream or ditch?","other sources?" ))
cooking.code <- distinct(hogares_1, cooking_fuel)%>%
  arrange(cooking_fuel)%>%
  mutate(Cooking.Fuel = c("Mains gas?", "Bulk gas (zeppelin)?", "Gas in tube?", "Gas in a carafe?", "electricity?", "Kerosene / firewood / charcoal?", "other?" ))
heating.code <- distinct(hogares_1, heating_fuel)%>%
  arrange(heating_fuel)%>%
  mutate(Heating.Fuel = c("Mains gas?", "Bulk gas (zeppelin)?", "Gas in tube?", "Gas in a carafe?", "electricity?", "Kerosene / firewood / charcoal?", "other?" ))
education.code <- distinct(hogares_1, edu_hhh)%>%
  arrange(edu_hhh)%>%
  mutate(Education = c("Incomplete elementary (includes special education)", "Complete primary", "Incomplete secondary", "Complete secondary", "Incomplete university superior", "Complete university superior", "No instruction", "Ns / nr"))
occupation.code <- distinct(hogares_1, ind_hhh)%>%
  arrange(ind_hhh)%>%
  mutate(IND_HHH = c("Salaried","Own legal company account","Own account of non-legal company","Employer / employer / partner of legal society","Employer / partner partner of a non-legal company","Unpaid family employee", NA))
toilet.code <- distinct(hogares_1, toilet)%>%
  arrange(toilet)%>%
  mutate(Toilet = c("Toilet with button / backpack / chain and water drag?", "Toilet without button / chain and with water flush? (in vain)", "latrine? (without water carry-over)", "Ns / nr", NA))
electricity.code <- distinct(hogares_1, electricity)%>%
  arrange(electricity)%>%
  mutate(Electricity = c("Grid", "Generator Motor", "Other Generator", "No Electricity"))
province.code <- distinct(hogares_1, province)%>%
  arrange(province)%>%
  mutate(Province = c("Ciudad Autónoma de Buenos Aires", "Buenos Aires", "Catamarca", "Córdoba", "Corrientes", "Chaco", "Chubut", "Entre Ríos", "Formosa", "Jujuy", "La Pampa", "La Rioja", "Mendoza", "Misiones", "Neuquén", "Río Negro", "Salta", "San Juan", "San Luis", "Santa Cruz", "Santa Fe", "Santiago del Estero", "Tucumán", "Tierra del Fuego"))
district.code <- distinct(hogares_1, district)%>%
  arrange(district)%>%
  mutate(District = c("Ciudad Autónoma de Buenos Aires","Partidos del Gran Buenos Aires","Córdoba y La Pampa","Santa Fe y Entre Ríos","Buenos Aires", "Jujuy, Salta y Tucumán","La Rioja, Catamarca y Santiago del Estero", "Misiones y Corrientes", "Chaco y Formosa", "San Juan, Mendoza y San Luis", "Neuquén y Río Negro", "Chubut, Santa Cruz y Tierra del Fuego"))

write_csv(water.code,"../0_Data/1_Household Data/3_Argentina/2_Codes/Water.Code.csv")
write_csv(cooking.code,"../0_Data/1_Household Data/3_Argentina/2_Codes/Cooking.Code.csv")
write_csv(heating.code,"../0_Data/1_Household Data/3_Argentina/2_Codes/Heating.Code.csv")
write_csv(education.code,"../0_Data/1_Household Data/3_Argentina/2_Codes/Education.Code.csv")
write_csv(occupation.code,"../0_Data/1_Household Data/3_Argentina/2_Codes/Occupation.Code.csv")
write_csv(toilet.code,"../0_Data/1_Household Data/3_Argentina/2_Codes/Toilet.Code.csv")
write_csv(electricity.code,"../0_Data/1_Household Data/3_Argentina/2_Codes/Electricity.Code.csv")
write_csv(province.code,"../0_Data/1_Household Data/3_Argentina/2_Codes/Province.Code.csv")
write_csv(district.code,"../0_Data/1_Household Data/3_Argentina/2_Codes/District.Code.csv")
