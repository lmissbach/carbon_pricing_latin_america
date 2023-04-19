# Author: L. Missbach (missbach@mcc-berlin.net)

# Packages ####

if(!require("pacman")) install.packages("pacman")

p_load("haven", "Hmisc", "openxlsx", "rattle", "scales", "tidyverse")
options(scipen=999)

# Load Data ####

Enaho_01_0 <- read_dta("../0_Data/1_Household Data/3_Peru/1_Data_Raw/2019/Stata/687-Modulo01/enaho01-2019-100.dta")
Enaho_02_0 <- read_dta("../0_Data/1_Household Data/3_Peru/1_Data_Raw/2019/Stata/687-Modulo02/enaho01-2019-200.dta")
Enaho_03_0 <- read_dta("../0_Data/1_Household Data/3_Peru/1_Data_Raw/2019/Stata/687-Modulo03/enaho01a-2019-300.dta")
Enaho_04_0 <- read_dta("../0_Data/1_Household Data/3_Peru/1_Data_Raw/2019/Stata/687-Modulo04/enaho01a-2019-400.dta")
Enaho_05_0 <- read_dta("../0_Data/1_Household Data/3_Peru/1_Data_Raw/2019/Stata/687-Modulo05/enaho01a-2019-500.dta")
Enaho_07_0 <- read_dta("../0_Data/1_Household Data/3_Peru/1_Data_Raw/2019/Stata/687-Modulo07/enaho01-2019-601.dta")
Enaho_08_0 <- read_dta("../0_Data/1_Household Data/3_Peru/1_Data_Raw/2019/Stata/687-Modulo08/enaho01-2019-602.dta")
Enaho_09_0 <- read_dta("../0_Data/1_Household Data/3_Peru/1_Data_Raw/2019/Stata/687-Modulo09/enaho01-2019-603.dta")
Enaho_10_0 <- read_dta("../0_Data/1_Household Data/3_Peru/1_Data_Raw/2019/Stata/687-Modulo10/enaho01-2019-604.dta")
Enaho_11_0 <- read_dta("../0_Data/1_Household Data/3_Peru/1_Data_Raw/2019/Stata/687-Modulo11/enaho01-2019-605.dta")
Enaho_12_0 <- read_dta("../0_Data/1_Household Data/3_Peru/1_Data_Raw/2019/Stata/687-Modulo12/enaho01-2019-606.dta")
Enaho_13_0 <- read_dta("../0_Data/1_Household Data/3_Peru/1_Data_Raw/2019/Stata/687-Modulo13/enaho01-2019-607.dta")

Enaho_15_0 <- read_dta("../0_Data/1_Household Data/3_Peru/1_Data_Raw/2019/Stata/687-Modulo15/enaho01-2019-609.dta")
Enaho_16_0 <- read_dta("../0_Data/1_Household Data/3_Peru/1_Data_Raw/2019/Stata/687-Modulo16/enaho01-2019-610.dta")
Enaho_17_0 <- read_dta("../0_Data/1_Household Data/3_Peru/1_Data_Raw/2019/Stata/687-Modulo17/enaho01-2019-611.dta")
Enaho_18_0 <- read_dta("../0_Data/1_Household Data/3_Peru/1_Data_Raw/2019/Stata/687-Modulo18/enaho01-2019-612.dta")
Enaho_78_0 <- read_dta("../0_Data/1_Household Data/3_Peru/1_Data_Raw/2019/Stata/687-Modulo78/enaho01-2019-606d.dta")

Enaho_37.1_0 <- read_dta("../0_Data/1_Household Data/3_Peru/1_Data_Raw/2019/Stata/687-Modulo37/enaho01-2019-700.dta")
Enaho_37.2_0 <- read_dta("../0_Data/1_Household Data/3_Peru/1_Data_Raw/2019/Stata/687-Modulo37/enaho01-2019-700a.dta")
Enaho_37.3_0 <- read_dta("../0_Data/1_Household Data/3_Peru/1_Data_Raw/2019/Stata/687-Modulo37/enaho01-2019-700b.dta")

Village.Code <- distinct(Enaho_01_0, nomccpp, longitud, latitud)%>%
  arrange(nomccpp, longitud, latitud)%>%
  mutate(village = 1:n())
Lighting.Code <- data.frame(lighting_fuel = c(1,3,4,5,6,7),
                            Lighting_Fuel = c("Electricity", "Kerosene", "Candles", "Generator", "Other", "No Lighting"))
Cooking.Code <- data.frame(cooking_fuel = c(1,2,3,5,6,7,8,9),
                           Cooking_Fuel = c("Electricity", "LPG", "Natural Gas", "Coal", "Firewood", "Other", "No Cooking", "Animal Waste"))

# Transform Data ####

# Household Information

Enaho_01_1 <- Enaho_01_0 %>%
  unite("hh_id", c("nconglome", "conglome", "vivienda", "hogar"), sep = "")%>%
  select(hh_id, ubigeo, dominio, estrato, p110, p111a, factor07, nomccpp, longitud, latitud)%>%
  rename(water = p110, toilet = p111a, hh_weights = factor07, province = dominio, district = ubigeo)%>%
  mutate(urban_01 = ifelse(estrato <= 5,1,0))%>%
  left_join(Village.Code)%>%
  select(hh_id, hh_weights, province, district, village, urban_01, water, toilet)

Enaho_01_1.1 <- Enaho_01_0 %>%
  unite("hh_id", c("nconglome", "conglome", "vivienda", "hogar"), sep = "")%>%
  select(hh_id, p1121:p1127)%>%
  mutate(Lighting_Fuel = ifelse(p1121 == 1, "Electricity", 
                                ifelse(p1123 == 1, "Kerosene",
                                       ifelse(p1124 == 1, "Candles",
                                              ifelse(p1125 == 1, "Generator",
                                                     ifelse(p1126 == 1, "Other", "No Lighting"))))))%>%
  left_join(Lighting.Code)%>%
  mutate(electricity.access = ifelse(lighting_fuel == 1,1,0))%>%
  select(hh_id, lighting_fuel, electricity.access)
  

Enaho_01_1.2 <- Enaho_01_0 %>%
  unite("hh_id", c("nconglome", "conglome", "vivienda", "hogar"), sep = "")%>%
  select(hh_id, p1131:p1138)%>%
  mutate(Cooking_Fuel = ifelse(p1131 == 1, "Electricity",
                               ifelse(p1132 == 1, "LPG",
                                      ifelse(p1133 == 1, "Natural Gas",
                                             ifelse(p1135 == 1, "Coal",
                                                    ifelse(p1136 == 1, "Firewood",
                                                           ifelse(p1139 == 1, "Animal Waste",
                                                                  ifelse(p1137 == 1, "Other",
                                                                         ifelse(p1138 == 1, "No Cooking", NA)))))))))%>%
  left_join(Cooking.Code)%>%
  select(hh_id, cooking_fuel)

Enaho_01_1.CF <- Enaho_01_0 %>%
  unite("hh_id", c("nconglome", "conglome", "vivienda", "hogar"), sep = "")%>%
  select(hh_id, p1131:p1138)%>%
  mutate(CF_Electricity  = ifelse(p1131 == 1,1,0),
         CF_LPG          = ifelse(p1132 == 1,1,0),
         CF_Coal         = ifelse(p1135 == 1,1,0),
         CF_Natural_Gas  = ifelse(p1133 == 1,1,0),
         CF_Firewood     = ifelse(p1136 == 1,1,0),
         CF_Animal_Waste = ifelse(p1139 == 1,1,0),
         CF_Other        = ifelse(p1137 == 1,1,0),
         CF_No_Cooking   = ifelse(p1138 == 1,1,0))%>%
  select(hh_id, starts_with("CF"))

Enaho_02_1 <- Enaho_02_0 %>%
  unite("hh_id", c("nconglome", "conglome", "vivienda", "hogar"), sep = "")%>%
  filter(p203 == 1)%>%
  rename(sex_hhh = p207, age_hhh = p208a)%>%
  select(hh_id, sex_hhh, age_hhh)

Enaho_02_2 <- Enaho_02_0 %>%
  unite("hh_id", c("nconglome", "conglome", "vivienda", "hogar"), sep = "")%>%
  filter(p204 == 1)%>%
  mutate(adults   = ifelse(p208a > 15,1,0),
         children = ifelse(p208a < 16,1,0))%>%
  group_by(hh_id)%>%
  summarise(hh_size  = n(),
            adults   = sum(adults),
            children = sum(children))%>%
  ungroup()

Enaho_03_1 <- Enaho_03_0 %>%
  unite("hh_id", c("nconglome", "conglome", "vivienda", "hogar"), sep = "")%>%
  filter(p300n == 1)%>%
  select(hh_id, p300a, p301a)%>%
  rename(language = p300a, edu_hhh = p301a)%>%
  # affects two households
  mutate(language = ifelse(is.na(language),4, language),
         edu_hhh  = ifelse(is.na(edu_hhh), 1, edu_hhh))

Enaho_05_1 <- Enaho_05_0 %>%
  unite("hh_id", c("nconglome", "conglome", "vivienda", "hogar"), sep = "")%>%
  rename(ind_hhh = p506r4, ethnicity = p558c)%>%
  filter(p203 == 1)%>%
  select(hh_id, ind_hhh, ethnicity)%>%
  mutate(ethnicity = ifelse(is.na(ethnicity),8,ethnicity))

Enaho_05_2 <- Enaho_05_0 %>%
  unite("hh_id", c("nconglome", "conglome", "vivienda", "hogar"), sep = "")%>%
  select(hh_id, starts_with("p556"))%>%
  select(hh_id, ends_with("b"), ends_with("c"))%>%
  mutate_at(vars(ends_with("b")), list(~ ifelse(. == 1,365,
                                                ifelse(. == 2,52,
                                                       ifelse(. == 3, 26,
                                                              ifelse(. == 4,12,
                                                                     ifelse(. == 5,6,
                                                                            ifelse(. == 6,4,
                                                                                   ifelse(. == 7,2,
                                                                                          ifelse(. == 8,1,
                                                                                                 ifelse(is.na(.),0,0)))))))))))%>%
  mutate_at(vars(ends_with("b")), list(~ ifelse(is.na(.),0,.)))%>%
  mutate(P5561 = p5561c*p5561b,
         P5562 = p5562c*p5562b,
         P5563 = p5563c*p5563b,
         P5564 = p5564c*p5564b,
         P5565 = p5565c*p5565b,
         P5566 = p5566c*p5566b,
         P5567 = p5567c*p5567b,
         P5568 = p5568c*p5568b,
         P5569 = p5569c*p5569b)%>%
  mutate_at(vars(-hh_id), list(~ ifelse(is.na(.),0,.)))%>%
  mutate(inc_gov_cash     = P5566 + P5567 + P5568,
         inc_gov_monetary = P5561 + P5562 + P5564 + P5565)%>%
  group_by(hh_id)%>%
  summarise(inc_gov_cash     = sum(inc_gov_cash),
            inc_gov_monetary = sum(inc_gov_monetary))%>%
  ungroup()

household_information <- Enaho_01_1 %>%
  left_join(Enaho_01_1.1)%>%
  left_join(Enaho_01_1.2)%>%
  left_join(Enaho_02_1)%>%
  left_join(Enaho_02_2)%>%
  left_join(Enaho_03_1)%>%
  left_join(Enaho_05_1)%>%
  left_join(Enaho_05_2)%>%
  remove_all_labels()

# Should not forget to delete the meaningless

# Expenditures ####

Enaho_01_2 <- Enaho_01_0 %>%
  unite("hh_id", c("nconglome", "conglome", "vivienda", "hogar"), sep = "")%>%
  select(hh_id, p105b, p1172_01:p1172_16)%>%
  pivot_longer(-hh_id, names_to = "item_code", values_to = "expenditures")%>%
  filter(!is.na(expenditures) & expenditures>0)%>%
  mutate(expenditures_year = expenditures*12)%>%
  mutate(item_code = ifelse(item_code == "p105b", "14000",
                            ifelse(item_code != "p105b", str_sub(item_code,2,-1), "NA")))%>%
  mutate(item_code = str_remove(item_code, "_"))%>%
  select(hh_id, item_code, expenditures_year)

Enaho_03_2 <- Enaho_03_0 %>%
  unite("hh_id", c("nconglome", "conglome", "vivienda", "hogar"), sep = "")%>%
  select(hh_id, p311a1_1:p311a1_8, p311b_1:p311b_8, p3121a1, p3122a1, p3121b, p3122b, p315a)%>%
  mutate(p31101 = ifelse(p311a1_1 == 1, p311b_1,0),
         p31102 = ifelse(p311a1_2 == 1, p311b_2,0),
         p31103 = ifelse(p311a1_3 == 1, p311b_3,0),
         p31104 = ifelse(p311a1_4 == 1, p311b_4,0),
         p31105 = ifelse(p311a1_5 == 1, p311b_5,0),
         p31106 = ifelse(p311a1_6 == 1, p311b_6,0),
         p31107 = ifelse(p311a1_7 == 1, p311b_7,0),
         p31108 = ifelse(p311a1_8 == 1, p311b_8,0),
         p31201 = ifelse(p3121a1  == 1, p3121b, 0),
         p31202 = ifelse(p3122a1  == 1, p3122b, 0),
         p31500 = p315a*12)%>%
  select(hh_id, starts_with("p3110"), p31201, p31202, p31500)%>%
  pivot_longer(-hh_id, names_to = "item_code", values_to = "expenditures_year")%>%
  filter(!is.na(expenditures_year) & expenditures_year > 0)%>%
  mutate(item_code = str_sub(item_code,2,-1))
  
Enaho_04_2 <- Enaho_04_0 %>%
  unite("hh_id", c("nconglome", "conglome", "vivienda", "hogar"), sep = "")%>%
  select(hh_id, p4151_01:p4151_16, p41601:p41616)%>%
  mutate(p415101 = ifelse(p4151_01 == 1, p41601*12,0),
         p415102 = ifelse(p4151_02 == 1, p41602*12,0),
         p415103 = ifelse(p4151_03 == 1, p41603*12,0),
         p415104 = ifelse(p4151_04 == 1, p41604*12,0),
         p415105 = ifelse(p4151_05 == 1, p41605*12,0),
         p415106 = ifelse(p4151_06 == 1, p41606*4,0),
         p415107 = ifelse(p4151_07 == 1, p41607*4,0),
         p415108 = ifelse(p4151_08 == 1, p41608*4,0),
         p415109 = ifelse(p4151_09 == 1, p41609*4,0),
         p415110 = ifelse(p4151_10 == 1, p41610*4,0),
         p415111 = ifelse(p4151_11 == 1, p41611*4,0),
         p415112 = ifelse(p4151_12 == 1, p41612*4,0),
         p415113 = ifelse(p4151_13 == 1, p41613,0),
         p415114 = ifelse(p4151_14 == 1, p41614,0),
         p415115 = ifelse(p4151_15 == 1, p41615,0),
         p415116 = ifelse(p4151_16 == 1, p41616,0))%>%
  select(hh_id, starts_with("p41510") | starts_with("p41511"))%>%
  pivot_longer(-hh_id, names_to = "item_code", values_to = "expenditures_year")%>%
  filter(!is.na(expenditures_year) & expenditures_year > 0)%>%
  mutate(item_code = str_sub(item_code,2,-1))

Enaho_07_1 <- Enaho_07_0 %>%
  unite("hh_id", c("nconglome", "conglome", "vivienda", "hogar"), sep = "")%>%
  select(hh_id, p601a, d601c)%>%
  rename(item_code = p601a, expenditures_year = d601c)%>%
  filter(!is.na(expenditures_year))

Enaho_08_1 <- Enaho_08_0 %>%
  unite("hh_id", c("nconglome", "conglome", "vivienda", "hogar"), sep = "")%>%
  select(hh_id, p602n, d602e1)%>%
  rename(item_code = p602n, expenditures_year = d602e1)%>%
  filter(!is.na(expenditures_year))%>%
  mutate(item_code = item_code+5100)

Enaho_09_1 <- Enaho_09_0 %>%
  unite("hh_id", c("nconglome", "conglome", "vivienda", "hogar"), sep = "")%>%
  select(hh_id, d603b, p603n)%>%
  rename(item_code = p603n, expenditures_year = d603b)%>%
  filter(!is.na(expenditures_year))%>%
  mutate(item_code = item_code+6000)

Enaho_10_1 <- Enaho_10_0 %>%
  unite("hh_id", c("nconglome", "conglome", "vivienda", "hogar"), sep = "")%>%
  select(hh_id, p604n, d604b)%>%
  rename(item_code = p604n, expenditures_year = d604b)%>%
  filter(!is.na(expenditures_year))%>%
  mutate(item_code = item_code+7000)

Enaho_11_1 <- Enaho_11_0 %>%
  unite("hh_id", c("nconglome", "conglome", "vivienda", "hogar"), sep = "")%>%
  select(hh_id, p605n, d605b)%>%
  rename(item_code = p605n, expenditures_year = d605b)%>%
  filter(!is.na(expenditures_year))%>%
  mutate(item_code = item_code+8000)

Enaho_12_1 <- Enaho_12_0 %>%
  unite("hh_id", c("nconglome", "conglome", "vivienda", "hogar"), sep = "")%>%
  select(hh_id, p606n, d606b)%>%
  rename(item_code = p606n, expenditures_year = d606b)%>%
  filter(!is.na(expenditures_year))%>%
  mutate(item_code = item_code+9000)

Enaho_13_1 <- Enaho_13_0 %>%
  unite("hh_id", c("nconglome", "conglome", "vivienda", "hogar"), sep = "")%>%
  select(hh_id, p607n, d607b)%>%
  rename(item_code = p607n, expenditures_year = d607b)%>%
  filter(!is.na(expenditures_year))%>%
  mutate(item_code = item_code+10000)

Enaho_15_1 <- Enaho_15_0 %>%
  unite("hh_id", c("nconglome", "conglome", "vivienda", "hogar"), sep = "")%>%
  select(hh_id, p609n, d609a)%>%
  rename(item_code = p609n, expenditures_year = d609a)%>%
  filter(!is.na(expenditures_year))%>%
  mutate(item_code = item_code+11000)

Enaho_16_1 <- Enaho_16_0 %>%
  unite("hh_id", c("nconglome", "conglome", "vivienda", "hogar"), sep = "")%>%
  select(hh_id, p610n, d610b)%>%
  rename(item_code = p610n, expenditures_year = d610b)%>%
  filter(!is.na(expenditures_year))%>%
  mutate(item_code = item_code+12000)

Enaho_17_1 <- Enaho_17_0 %>%
  unite("hh_id", c("nconglome", "conglome", "vivienda", "hogar"), sep = "")%>%
  select(hh_id, p611n, d611b)%>%
  rename(item_code = p611n, expenditures_year = d611b)%>%
  filter(!is.na(expenditures_year))%>%
  mutate(item_code = item_code+13000)

Enaho_18_1.1 <- Enaho_18_0 %>%
  unite("hh_id", c("nconglome", "conglome", "vivienda", "hogar"), sep = "")%>%
  select(hh_id, p612n, p612c, p612g)%>%
  mutate(p612g = ifelse(p612c == 2019, p612g, 0))%>% 
  select(hh_id, p612n, p612g)%>%
  rename(item_code = p612n, expenditures_year = p612g)%>%
  filter(expenditures_year>0)%>%
  mutate(item_code = item_code+15000)

expenditures_items_Peru <- Enaho_07_1 %>%
  bind_rows(Enaho_01_2)%>%
  bind_rows(Enaho_03_2)%>%
  bind_rows(Enaho_04_2)%>%
  mutate(item_code = as.numeric(item_code))%>%
  bind_rows(Enaho_08_1)%>%
  bind_rows(Enaho_09_1)%>%
  bind_rows(Enaho_10_1)%>%
  bind_rows(Enaho_11_1)%>%
  bind_rows(Enaho_12_1)%>%
  bind_rows(Enaho_13_1)%>%
  bind_rows(Enaho_15_1)%>%
  bind_rows(Enaho_16_1)%>%
  bind_rows(Enaho_17_1)%>%
  bind_rows(Enaho_18_1.1)%>%
  arrange(hh_id, item_code)%>%
  remove_all_labels()%>%
  group_by(hh_id, item_code)%>%
  summarise(expenditures_year = sum(expenditures_year))%>%
  ungroup()

# Appliances ####

Enaho_18_1 <- Enaho_18_0 %>%
  unite("hh_id", c("nconglome", "conglome", "vivienda", "hogar"), sep = "")%>%
  select(hh_id, p612n, p612)%>%
  mutate(value = ifelse(p612 == 1 & !is.na(p612),1,0))%>%
  mutate(appliance = ifelse(p612n == 1, "radio.01",
                            ifelse(p612n == 2 | p612n == 3, "tv.01",
                                   ifelse(p612n == 7, "computer.01",
                                          ifelse(p612n == 10, "stove.g.01",
                                                 ifelse(p612n == 11, "stove.k.01",
                                                        ifelse(p612n == 12, "refrigerator.01",
                                                               ifelse(p612n == 13, "washing_machine.01",
                                                                      ifelse(p612n == 14, "microwave.01",
                                                                             ifelse(p612n == 17 | p612n == 21, "car.01",
                                                                                    ifelse(p612n == 18, "motorcycle.01","Appliance")))))))))))%>%
  filter(appliance != "Appliance")%>%
  group_by(hh_id, appliance)%>%
  summarise(value = max(value))%>%
  ungroup()%>%
  pivot_wider(names_from = "appliance", values_from = "value")%>%
  remove_all_labels()

# Save Output ####

household_information_0 <- household_information %>%
  filter(hh_id %in% expenditures_items_Peru$hh_id)

Cooking_Fuels_Information <- Enaho_01_1.CF %>%
  filter(hh_id %in% expenditures_items_Peru$hh_id)

Enaho_18_1.1 <- Enaho_18_1 %>%
  filter(hh_id %in% household_information_0$hh_id)

write_csv(Enaho_18_1,              "../0_Data/1_Household Data/3_Peru/1_Data_Clean/appliances_0_1_Peru.csv")
write_csv(household_information_0,   "../0_Data/1_Household Data/3_Peru/1_Data_Clean/household_information_Peru.csv")
write_csv(expenditures_items_Peru, "../0_Data/1_Household Data/3_Peru/1_Data_Clean/expenditures_items_Peru.csv")
write_csv(Cooking_Fuels_Information, "../0_Data/1_Household Data/3_Peru/1_Data_Clean/hi_Peru_CF.csv")

# Codes ####
Item.Codes.0 <- Enaho_07_0 %>%
  distinct(p601a, p601x)%>%
  rename(item_code = p601a, item_name = p601x)%>%
  group_by(item_code)%>%
  summarise(item_name = first(item_name))%>%
  ungroup()%>%
  mutate(item_code = as.numeric(item_code))
Item.Codes <- distinct(expenditures_items_Peru, item_code)%>%
  arrange(item_code)%>%
  remove_all_labels()%>%
  left_join(Item.Codes.0)

#write.xlsx(Item.Codes, "../0_Data/1_Household Data/3_Peru/3_Matching_Tables/Item_Code_Description_Peru.xlsx")

Ethnicity.Code <- stack(attr(Enaho_05_0$p558c, 'labels'))%>%
  rename(ethnicity = values, Ethnicity = ind)%>%
  write_csv(., "../0_Data/1_Household Data/3_Peru/2_Codes/Ethnicity.Code.csv")
Industry.Code <- stack(attr(Enaho_05_0$p506r4, 'labels'))%>%
  rename(ind_hhh = values, Industry = ind)%>%
  write_csv(., "../0_Data/1_Household Data/3_Peru/2_Codes/Industry.Code.csv")
Language.Code <- stack(attr(Enaho_03_0$p300a, 'labels'))%>%
  rename(language = values, Language = ind)%>%
  write_csv(., "../0_Data/1_Household Data/3_Peru/2_Codes/Language.Code.csv")
Education.Code <- stack(attr(Enaho_03_0$p301a, 'labels'))%>%
  rename(edu_hhh = values, Education = ind)%>%
  write_csv(., "../0_Data/1_Household Data/3_Peru/2_Codes/Education.Code.csv")
Gender.Code <- stack(attr(Enaho_02_0$p207, 'labels'))%>%
  rename(sex_hhh = values, Gender = ind)%>%
  write_csv(., "../0_Data/1_Household Data/3_Peru/2_Codes/Gender.Code.csv")
Lighting.Code <- data.frame(lighting_fuel = c(1,3,4,5,6,7),
                            Lighting_Fuel = c("Electricity", "Kerosene", "Candles", "Generator", "Other", "No Lighting"))%>%
  write_csv(., "../0_Data/1_Household Data/3_Peru/2_Codes/Lighting.Code.csv")
Cooking.Code <- data.frame(cooking_fuel = c(1,2,3,5,6,7,8,9),
                           Cooking_Fuel = c("Electricity", "LPG", "Natural Gas", "Coal", "Firewood", "Other", "No Cooking", "Animal Waste"))%>%
  write_csv(., "../0_Data/1_Household Data/3_Peru/2_Codes/Cooking.Code.csv")
Province.Code <- stack(attr(Enaho_01_0$dominio, 'labels'))%>%
  rename(province = values, Province = ind)%>%
  write_csv(., "../0_Data/1_Household Data/3_Peru/2_Codes/Province.Code.csv")
Water.Code <- stack(attr(Enaho_01_0$p110, 'labels'))%>%
  rename(water = values, Water = ind)%>%
  write_csv(., "../0_Data/1_Household Data/3_Peru/2_Codes/Water.Code.csv")
Toilet.Code <- stack(attr(Enaho_01_0$p111a, 'labels'))%>%
  rename(toilet = values, Toilet = ind)%>%
  write_csv(., "../0_Data/1_Household Data/3_Peru/2_Codes/Toilet.Code.csv")
District.Code <- distinct(Enaho_01_0, ubigeo)%>%
  arrange(ubigeo)%>%
  rename(district = ubigeo)%>%
  write_csv(., "../0_Data/1_Household Data/3_Peru/2_Codes/District.Code.csv")
Village.Code <- distinct(Enaho_01_0, nomccpp, longitud, latitud)%>%
  arrange(nomccpp, longitud, latitud)%>%
  mutate(village = 1:n())%>%
  write_csv(., "../0_Data/1_Household Data/3_Peru/2_Codes/Village.Code.csv")
