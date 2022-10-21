if(!require("pacman")) install.packages("pacman")

p_load("haven", "Hmisc", "openxlsx", "rattle", "scales", "tidyverse")

# Load Data ####

data_0 <- read_dta("../0_Data/1_Household Data/3_El Salvador/1_Data_Raw/Data 2015/SLV_2015_EHPM_v01_M_STATA8/EHPM_2015.dta")
data_81 <- read_dta("../0_Data/1_Household Data/3_El Salvador/1_Data_Raw/Data 2015/sec08a.dta")
data_82 <- read_dta("../0_Data/1_Household Data/3_El Salvador/1_Data_Raw/Data 2015/sec08b.dta")
data_83 <- read_dta("../0_Data/1_Household Data/3_El Salvador/1_Data_Raw/Data 2015/sec08c.dta")
data_84 <- read_dta("../0_Data/1_Household Data/3_El Salvador/1_Data_Raw/Data 2015/sec08d.dta")

# Transform Data ####

data_01 <- data_0 %>%
  rename(hh_id = idboleta, urban_01 = area, province = r004, district = r005, hh_weights = fac00, hh_size = r021a,
         hhh = r103, sex_hhh = r104, age_hhh = r106, alfabetism = r202a, edu_hhh = r217a, edu_hhh_b = r219,
         lighting_fuel = r311, water = r312, toilet = r319, gas_subsidy = r325a1, ely_subsidy = r325a2, cooking_fuel = r326, ind_hhh_b = r414,
         ind_hhh_a = r416)%>% # ind_hhha = CIIU Rev. 4
  select(hh_id, urban_01, region, province, district, hh_weights, hh_size, hhh, sex_hhh, age_hhh, alfabetism, edu_hhh, edu_hhh_b,
         lighting_fuel, water, toilet, cooking_fuel, ind_hhh_a, ind_hhh_b, gas_subsidy, ely_subsidy)

data_011 <- data_01 %>%
  select(hh_id, hh_weights, urban_01, province, district, lighting_fuel, water, toilet, gas_subsidy, ely_subsidy, cooking_fuel)%>%
  distinct()%>%# discard hh size
  mutate(electricity.access = ifelse(lighting_fuel == 1 | lighting_fuel == 2 | lighting_fuel == 5 | lighting_fuel == 6,1,0))

data_012 <- data_01 %>%
  select(hh_id, hh_size, age_hhh)%>%
  mutate(adults   = age_hhh > 15,1,0,
         children = age_hhh < 16,1,0)%>%
  group_by(hh_id)%>%
  summarise(adults = sum(adults),
            children = sum(children))%>%
  ungroup()%>%
  mutate(hh_size = adults + children)

data_013 <- data_01 %>%
  select(hh_id, hhh, sex_hhh, age_hhh, edu_hhh, edu_hhh_b, ind_hhh_a, ind_hhh_b)%>%
  rename(ind_hhh = ind_hhh_a)%>%
  filter(hhh == 1)%>%
  select(-hhh)%>%
  unite(edu_hhh, c(edu_hhh, edu_hhh_b), sep = "00")

# Codes

Gender.Code <- stack(attr(data_013$sex_hhh, 'labels'))%>%
  rename(Gender = ind, sex_hhh = values)%>%
  write_csv(., "../0_Data/1_Household Data/3_El_Salvador/2_Codes/Gender.Code.csv")
Education.Code.A <- stack(attr(data_01$edu_hhh, 'labels'))%>%
  rename(Education = ind, edu_hhh_a = values)
Education.Code.B <- stack(attr(data_01$edu_hhh_b, 'labels'))%>%
  rename(Education.B = ind, edu_hhh_b = values)
Education.Code <- expand_grid(Education.Code.A, Education.Code.B)%>%
  unite(edu_hhh, c(edu_hhh_a, edu_hhh_b), sep = "00")%>%
  filter(edu_hhh %in% data_013$edu_hhh)%>%
  unite(Education, c(Education, Education.B), sep =", ")%>%
  write_csv(., "../0_Data/1_Household Data/3_El_Salvador/2_Codes/Education.Code.csv")
Industry.Code <- distinct(data_013, ind_hhh)%>%
  arrange(ind_hhh)%>%
  mutate(ind_hhh = as.character(ind_hhh))
CIIU <- read.xlsx("../0_Data/1_Household Data/3_El_Salvador/9_Documentation/Industry_Codes_CIIU_Rev_4.xlsx")
CIIU.2 <- CIIU %>%
  filter(nchar(Código) == 4)%>%
  mutate(ind_hhh = as.numeric(Código))%>%
  filter(nchar(ind_hhh) == 3)%>%
  mutate(ind_hhh = as.character(ind_hhh))
Industry.Code.0 <- left_join(Industry.Code, CIIU, by = c("ind_hhh" = "Código"))%>%
  left_join(CIIU.2, by = "ind_hhh")%>%
  mutate(Industry = ifelse(!is.na(Código), Denominación.y, Denominación.x))%>%
  select(ind_hhh, Industry)%>%
  write_csv(., "../0_Data/1_Household Data/3_El_Salvador/2_Codes/Industry.Code.csv")

Province.Code <- stack(attr(data_01$province, 'labels'))%>%
  rename(Province = ind, province = values)%>%
  write_csv(., "../0_Data/1_Household Data/3_El_Salvador/2_Codes/Province.Code.csv")
District.Code <- stack(attr(data_01$district, 'labels'))%>%
  rename(District = ind, district = values)%>%
  write_csv(., "../0_Data/1_Household Data/3_El_Salvador/2_Codes/District.Code.csv")
Lighting.Code <- stack(attr(data_01$lighting_fuel, 'labels'))%>%
  rename(Lighting_Fuel = ind, lighting_fuel = values)%>%
  write_csv(., "../0_Data/1_Household Data/3_El_Salvador/2_Codes/Lighting.Code.csv")
Cooking.Code <- stack(attr(data_01$cooking_fuel, 'labels'))%>%
  rename(Cooking_Fuel = ind, cooking_fuel = values)%>%
  write_csv(., "../0_Data/1_Household Data/3_El_Salvador/2_Codes/Cooking.Code.csv")
Water.Code <- count(data_01, water)%>%
  arrange(water)%>%
  select(-n)%>%
  bind_cols(Water = c("Dentro de la vivienda von abastecimiento publico", "Dentro de la vivienda con otro tipo de abastecimiento",
                      "Fuera de la vivienda pero dentra de la propiedad con abastecimiento publico", "Fuera de la vivienda pero dentro de la propoedad con otro tipo de abastecimiento",
                      "Tuberia por poliducto (buen estado)", "No tiene", "Tiene pero no le cae (por mas de un mes)"))%>%
  write_csv(., "../0_Data/1_Household Data/3_El_Salvador/2_Codes/Water.Code.csv")
Toilet.Code <- stack(attr(data_01$toilet, 'labels'))%>%
  rename(Toilet = ind, toilet = values)%>%
  write_csv(., "../0_Data/1_Household Data/3_El_Salvador/2_Codes/Toilet.Code.csv")

# Income 

data_02 <- data_0 %>%
  rename(hh_id = idboleta)%>%
  select(hh_id, r44506, r44407a, r44407b, r44409a, r44409b,
         r904,r905, r915, 
         r917, r918, 
         r920, r921,
         r923, r924)%>%
  mutate(r905 = ifelse(r904 == 1, r905*12,
                       ifelse(r904 == 2, r905*6,
                              ifelse(r904 == 2, r905*4,
                                     r905))))%>%
  mutate(r918 = ifelse(r917 == 1, r918*12,
                       ifelse(r917 == 2, r918*6,r918)))%>%
  mutate(r921 = ifelse(r920 == 1,r921*12,
                       ifelse(r920 == 2, r921*6,
                              ifelse(r920 == 3, r921*4,r921))))%>%
  mutate(r924 = ifelse(r923 == 1, r924*12,
                       ifelse(r923 == 2, r924*6,
                              ifelse(r923 == 3, r924*4, r924))))%>%
  mutate(r44407 = r44407a*r44407b,
         r44409 = r44409a*r44409b)%>%
  mutate_at(vars(-hh_id), list(~ ifelse(is.na(.),0,.)))%>%
  mutate(inc_gov_cash = r44506 + r905 + r918 + r921 + r924,
         inc_gov_monetary = r44407 + r44409)%>%
  group_by(hh_id)%>%
  summarise(inc_gov_cash = sum(inc_gov_cash),
              inc_gov_monetary = sum(inc_gov_monetary))%>%
  ungroup()
  

data_01X <- left_join(data_011, data_012)%>%
  left_join(data_013)%>%
  left_join(data_02)%>%
  remove_all_labels()%>%
  zap_formats()

write_csv(data_01X, "../0_Data/1_Household Data/3_El Salvador/1_Data_Clean/household_information_El Salvador.csv")

data_811 <- data_81 %>%
  rename(hh_id = idboleta)%>%
  mutate(item_code = paste0("A",r802a))%>%
  filter(r802 != 3)%>%
  mutate(sp = ifelse(r803a == 1, 0,1))%>%
  filter(!is.na(sp))%>%
  select(hh_id, item_code, sp, r804, r806, gastomensual)%>%
  mutate(factor = gastomensual/r806)%>%
  mutate(expenditures_week = ifelse(r804 == 1, r806,
                                    ifelse(r804 == 2, r806*2,
                                           ifelse(r804 == 3, r806*3,
                                                  ifelse(r804 == 4, r806*4,
                                                         ifelse(r804 == 5, r806*5,
                                                                ifelse(r804 == 6, r806*6,
                                                                       ifelse(r804 == 7, r806*7, 
                                                                              ifelse(is.na(r804), gastomensual*12/52,gastomensual*12/52)))))))))%>%
  mutate(expenditures_year    = ifelse(sp == 0, expenditures_week*52,0),
         expenditures_sp_year = ifelse(sp == 1, expenditures_week*52,0))%>%
  select(hh_id, item_code, expenditures_year, expenditures_sp_year)%>%
  remove_all_labels()%>%
  zap_formats()


data_821 <- data_82 %>%
  rename(hh_id = idboleta)%>%
  mutate(item_code = paste0("B",r808a))%>%
  filter(r808 != 2)%>%
  mutate(r810a = ifelse(is.na(r810a),0,r810a))%>%
  mutate(sp = ifelse(r810a == 1,0,1))%>%
  select(hh_id, item_code, sp, r809, r811a, r811b, r811c)%>%
  mutate(r811a = ifelse(is.na(r811a),0, r811a),
         r811b = ifelse(is.na(r811b),0, r811b),
         r811c = ifelse(is.na(r811c),0, r811c))%>%
  mutate(factor = ifelse(r809 == 1, 365,
                         ifelse(r809 == 2, 52,
                                ifelse(r809 == 3, 26,
                                       ifelse(r809 == 4,12,
                                              ifelse(r809 == 5,6,
                                                     ifelse(r809 == 6,4,
                                                            ifelse(r809 == 7,2,
                                                                   ifelse(r809 == 8,1,0)))))))))%>%
  mutate(expenditures_year = r811a*factor,
         expenditures_sp_year = (r811b+r811c)*factor)%>%
  select(hh_id, item_code, expenditures_year, expenditures_sp_year)%>%
  arrange(hh_id, item_code)%>%
  remove_all_labels()%>%
  zap_formats()

data_831 <- data_83 %>%
  rename(hh_id = idboleta)%>%
  mutate(item_code = paste0("C",r808a))%>%
  filter(r808 == 1)%>%
  mutate(r810a = ifelse(is.na(r810a),0, r810a))%>%
  mutate(sp = ifelse(r810a == 1,0,1))%>%
  select(hh_id, item_code, sp, r809, r811a, r811b, r811c)%>%
  mutate(r811a = ifelse(is.na(r811a),0, r811a),
         r811b = ifelse(is.na(r811b),0, r811b),
         r811c = ifelse(is.na(r811c),0, r811c))%>%
  mutate(factor = ifelse(r809 == 1, 365,
                         ifelse(r809 == 2, 52,
                                ifelse(r809 == 3, 26,
                                       ifelse(r809 == 4,12,
                                              ifelse(r809 == 5,6,
                                                     ifelse(r809 == 6,4,
                                                            ifelse(r809 == 7,2,
                                                                   ifelse(r809 == 8,1,0)))))))))%>%
  mutate(expenditures_year = r811a*factor,
         expenditures_sp_year = (r811b+r811c)*factor)%>%
  select(hh_id, item_code, expenditures_year, expenditures_sp_year)%>%
  arrange(hh_id, item_code)%>%
  remove_all_labels()%>%
  zap_formats()

data_841 <- data_84 %>%
  rename(hh_id = idboleta)%>%
  mutate(item_code = paste0("D",r808a))%>%
  filter(r808 == 1)%>%
  select(hh_id, item_code, r809, r811)%>%
  mutate(r811a = ifelse(is.na(r811),0, r811))%>%
  mutate(factor = ifelse(r809 == 1, 365,
                         ifelse(r809 == 2, 52,
                                ifelse(r809 == 3, 26,
                                       ifelse(r809 == 4,12,
                                              ifelse(r809 == 5,6,
                                                     ifelse(r809 == 6,4,
                                                            ifelse(r809 == 7,2,
                                                                   ifelse(r809 == 8,1,0)))))))))%>%
  mutate(expenditures_year = r811*factor,
         expenditures_sp_year = 0)%>%
  select(hh_id, item_code, expenditures_year, expenditures_sp_year)%>%
  arrange(hh_id, item_code)%>%
  remove_all_labels()%>%
  zap_formats()

expenditures_0 <- bind_rows(data_811, data_821, data_831, data_841)

# Item Codes

# Item.Codes.1 <- distinct(data_811, item_code, descripcion)
# Item.Codes.2 <- distinct(data_821, item_code, descripcion)
# Item.Codes.3 <- distinct(data_831, item_code, descripcion)
# Item.Codes.4 <- distinct(data_841, item_code, descripcion)
Item.Codes.all <- bind_rows(Item.Codes.1, Item.Codes.2, Item.Codes.3, Item.Codes.4)%>%
  arrange(item_code)%>%
  mutate(Digit = str_sub(item_code,1,1),
         Digit2 = as.numeric(str_sub(item_code,2,4)))%>%
  arrange(Digit, Digit2)


data_01b <- data_0 %>%
  select(idboleta, 
         # Education
         r2212,  r2213,  r2214,  # Annual
         r221a2, r221a3, r221a4, # Annual
         r221b2, r221b3, r221b4, # Annual
         r221c2, r221c3, r221c4, # Annual
         r221d2, r221d3, r221d4, # Annual
         r221e2, r221e3, r221e4, # Annual
         r2222,  r2223,  r2224,  # Mensual
         r222a2, r222a3, r222a4, # Mensual  
         r222b2, r222b3, r222b4, # Mensual
         r222c2, r222c3, r222c4, # Mensual
         r308c, # Monthly
         r3141, # Weekly 
         r327b, # Monthly
         r33101b, r33101b1, r33101b2, r33102b, r33103b, r33104b, r33105b, r33106b, r33107b, r33108b, 
         r33109b, r33110b, r33111b, r33112b, r33113b, r33115b, r33116b, r33117b, # Monthly
         r44701:r44705, r44801:r44805, r45001:r45004, r45101:r45104,
         # Health - all monthly
         r605a, r605b, r605c, 
         r606a, r606b, r606c, 
         r607a, r607b, r607c,
         r608a, r608b, r608c, 
         r612a, r612b, r612c, 
         r613a, r613b, r613c,
         r614a, r614b, r614c, 
         r615a, r615b, r615c)%>%
  # Weekly expenditures
  mutate(r44601 = r44701*r44801,
         r44602 = r44702*r44802,
         r44603 = r44703*r44803,
         r44604 = r44704*r44804,
         r44605 = r44705*r44805)%>%
  mutate(r44901 = r45001*r45101,
         r44902 = r45002*r45102,
         r44903 = r45003*r45103,
         r44904 = r45004*r45104)%>%
  select(-starts_with("r447"), - starts_with("r448"), - starts_with("r450"), - starts_with("r451"))%>%
  rename(hh_id = idboleta)%>%
  pivot_longer(-hh_id, names_to = "item_code", values_to = "expenditures")%>%
  filter(!is.na(expenditures))%>%
  mutate(expenditures_sp_year = 0,
         factor = ifelse(item_code %in% c("r3141", "r44601", "r44602", "r44603", "r44604", "r44605", "r44901", "r44902", "r44903", "r44904"),52,
                                    ifelse(item_code %in% c("r2212",   "r2213", "r2214",
                                                            "r221a2", "r221a3", "r221a4",
                                                            "r221b2", "r221b3", "r221b4",
                                                            "r221c2", "r221c3", "r221c4",
                                                            "r221d2", "r221d3", "r221d4",
                                                            "r221e2", "r221e3", "r221e4"), 1,12)))%>%
  mutate(expenditures_year = expenditures*factor)%>%
  group_by(hh_id, item_code)%>%
  summarise(expenditures_year = sum(expenditures_year),
            expenditures_sp_year = sum(expenditures_sp_year))%>%
  ungroup()

expenditures_0.1 <- bind_rows(expenditures_0, data_01b)%>%
  arrange(hh_id, item_code)

write_csv(expenditures_0.1, "../0_Data/1_Household Data/3_El_Salvador/1_Data_Clean/expenditures_items_El_Salvador.csv")

Item.Codes.5 <- distinct(data_01b, item_code)%>%
  arrange(item_code)

Item.Codes.all <- bind_rows(Item.Codes.all, Item.Codes.5)

write.xlsx(Item.Codes.all, "../0_Data/1_Household Data/3_El_Salvador/3_Matching_Tables/Item_Codes_Description_El_Salvador.xlsx")

appliances_01 <- data_0 %>%
  select(idboleta, r33001a:r33019b)%>%
  select(-ends_with("b"))%>%
  rename(hh_id = idboleta, radio.01 = r33001a, tv.01 = r33003a, refrigerator.01 = r33005a, washing_machine.01 = r33006a,
         fan.01 = r33008a, computer.01 = r33009a, dryer.01 = r33010a, car.01 = r33012a, iron.01 = r33013a,
         microwave.01 = r33014a, generator.01 = r33016a, ac.01 = r33017a, motorcycle.01 = r33019a)%>%
  select(hh_id, ends_with("01"))%>%
  distinct()%>%
  remove_all_labels()%>%
  zap_formats()%>%
  mutate_at(vars(ends_with("01")), list(~ ifelse(. == 2,0,.)))%>%
  arrange(hh_id)

write_csv(appliances_01, "../0_Data/1_Household Data/3_El_Salvador/1_Data_Clean/appliances_0_1_El_Salvador.csv")

