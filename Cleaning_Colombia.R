if(!require("pacman")) install.packages("pacman")

p_load("tidyverse", "Hmisc", "haven", "readr", "openxlsx", "sjlabelled")

# Read Data

data_personas <- read_dta("../0_Data/1_Household Data/3_Colombia/1_Data_Raw/Data/Data/Caracteristicas generales personas/Caracteristicas generales personas.dta")
data_hogares  <- read_dta("../0_Data/1_Household Data/3_Colombia/1_Data_Raw/Data/Data/Viviendas y hogares/Viviendas y hogares.dta")

# Gastos
data_u_comidas   <- read_dta("../0_Data/1_Household Data/3_Colombia/1_Data_Raw/Data/Data/Gastos_u_fuera/Gastos_u_fuera.dta")
data_r_comidas   <- read_dta("../0_Data/1_Household Data/3_Colombia/1_Data_Raw/Data/Data/Gastos_s_r_fuera/Gastos_s_r_fuera.dta")

data_u_diarios   <- read_dta("../0_Data/1_Household Data/3_Colombia/1_Data_Raw/Data/Data/Gastos_d_u_p/Gastos_d_u_p.dta")
data_u_diarios_0 <- read_dta("../0_Data/1_Household Data/3_Colombia/1_Data_Raw/Data/Data/Gastos diarios Urbanos/Gastos diarios Urbanos.dta")
data_r_diarios   <- read_dta("../0_Data/1_Household Data/3_Colombia/1_Data_Raw/Data/Data/Gastos_r_p/Gastos_r_p.dta")
data_r_diarios_0 <- read_dta("../0_Data/1_Household Data/3_Colombia/1_Data_Raw/Data/Data/Gastos_s_r/Gastos_s_r.dta")

data_u_C         <- read_dta("../0_Data/1_Household Data/3_Colombia/1_Data_Raw/Data/Data/Gastos_d_u_C/Gastos_d_u_C.dta")
data_r_C         <- read_dta("../0_Data/1_Household Data/3_Colombia/1_Data_Raw/Data/Data/Gastos_s_r_C/Gastos_s_r_C.dta")

data_u_m         <- read_dta("../0_Data/1_Household Data/3_Colombia/1_Data_Raw/Data/Data/Gastos_d_u_m/Gastos_d_u_m.dta")
data_r_m         <- read_dta("../0_Data/1_Household Data/3_Colombia/1_Data_Raw/Data/Data/Gastos_s_r_m/Gastos_s_r_m.dta")

data_r_p_fuera   <- read_dta("../0_Data/1_Household Data/3_Colombia/1_Data_Raw/Data/Data/Gastos_p_r_fuera/Gastos_p_r_fuera.dta")
data_u_p_fuera   <- read_dta("../0_Data/1_Household Data/3_Colombia/1_Data_Raw/Data/Data/Gastos_p_u_fuera/Gastos_p_u_fuera.dta")

data_menos_art   <- read_dta("../0_Data/1_Household Data/3_Colombia/1_Data_Raw/Data/Data/Gastos_m_A/Gastos_m_A.dta")
data_menos_med   <- read_dta("../0_Data/1_Household Data/3_Colombia/1_Data_Raw/Data/Data/Gastos_m_M/Gastos_m_M.dta") 

# Expenditures ####

#4.28
data_u_diarios_1 <- data_u_diarios %>%
  rename(hh_id = DIRECTORIO)%>%
  select(hh_id, NC4_CC_P1_1, NC4_CC_P3, NC4_CC_P5, NC4_CC_P6)%>%
  rename(item_code        = NC4_CC_P1_1,
         expenditure_type = NC4_CC_P3,
         expenditures     = NC4_CC_P5,
         frequency        = NC4_CC_P6)%>%
  mutate(Type = "Gastos diarios personales Urbano")%>%
  mutate(month_factor = ifelse(frequency %in% c(1,2,3), 4.28,
                               ifelse(frequency == 4,2,
                                      ifelse(frequency == 5,1,
                                             ifelse(frequency == 6,1/2,
                                                    ifelse(frequency == 7,1/3,
                                                           ifelse(frequency == 9,1,1)))))))%>%
  mutate(sp = ifelse(expenditure_type %in% c(2,3,4,5,6,7),1,0))%>%
  mutate(expenditures_year    = ifelse(sp == 0, expenditures*month_factor*12,0),
         expenditures_sp_year = ifelse(sp == 1, expenditures*month_factor*12,0))%>%
  select(hh_id, item_code, expenditures_year, expenditures_sp_year)

#4.28
data_r_diarios_1 <- data_r_diarios %>%
  rename(hh_id = DIRECTORIO)%>%
  select(hh_id, NC2R_CE_P2, NC2R_CE_P7, NC2R_CE_P8)%>%
  filter(!is.na(NC2R_CE_P7))%>%
  rename(item_code    = NC2R_CE_P2,
         expenditures = NC2R_CE_P7,
         frequency    = NC2R_CE_P8)%>%
  mutate(Type = "Gastos personales Rural")%>%
  mutate(month_factor = ifelse(frequency %in% c(1,2,3), 4.28,
                               ifelse(frequency == 4,2,
                                      ifelse(frequency == 5,1,
                                             ifelse(frequency == 6,1/2,
                                                    ifelse(frequency == 7,1/3,
                                                           ifelse(frequency == 9,1,1)))))))%>%
  mutate(expenditures_year    = expenditures*month_factor)%>%
  select(hh_id, item_code, expenditures_year)

# 2.14
data_u_diarios_0.1 <- data_u_diarios_0 %>%
  rename(hh_id = DIRECTORIO)%>%
  select(hh_id, NH_CGDU_P1, NH_CGDU_P5, NH_CGDU_P8, NH_CGDU_P9)%>%
  rename(item_code        = NH_CGDU_P1,
         expenditure_type = NH_CGDU_P5,
         expenditures     = NH_CGDU_P8,
         frequency        = NH_CGDU_P9)%>%
  mutate(Type = "Gastos diarios Urbanos")%>%
  mutate(month_factor = ifelse(frequency %in% c(1,2,3),2.14,
                               ifelse(frequency == 4,2,
                                      ifelse(frequency == 5,1,
                                             ifelse(frequency == 6,1/2,
                                                    ifelse(frequency == 7,1/3,1))))))%>%
  mutate(sp = ifelse(expenditure_type %in% c(2,3,4,5,6,7),1,0))%>%
  mutate(expenditures_year    = ifelse(sp == 0, expenditures*month_factor*12,0),
         expenditures_sp_year = ifelse(sp == 1, expenditures*month_factor*12,0))%>%
  select(hh_id, item_code, expenditures_year, expenditures_sp_year)

#4.28
data_r_diarios_0.1 <- data_r_diarios_0 %>%
  rename(hh_id = DIRECTORIO)%>%
  select(hh_id, NC2R_CA_P3, NC2R_CA_P5_S1, NC2R_CA_P7_S1, NC2R_CA_P8_S1)%>%
  filter(!is.na(NC2R_CA_P7_S1))%>%
  rename(item_code        = NC2R_CA_P3,
         expenditure_type = NC2R_CA_P5_S1,
         expenditures     = NC2R_CA_P7_S1,
         frequency        = NC2R_CA_P8_S1)%>%
  mutate(expenditure_type = as.numeric(expenditure_type))%>%
  mutate(Type = "Gastos semanales Rurales")%>%
  mutate(month_factor = ifelse(frequency %in% c(1,2,3), 4.28,
                               ifelse(frequency == 4,2,
                                      ifelse(frequency == 5,1,
                                             ifelse(frequency == 6,1/2,
                                                    ifelse(frequency == 7,1/3,
                                                           ifelse(frequency == 9,1,1)))))))%>%
  mutate(sp = ifelse(expenditure_type %in% c(2,3,4,5,6,7),1,0))%>%
  mutate(expenditures_year    = ifelse(sp == 0, expenditures*month_factor*12,0),
         expenditures_sp_year = ifelse(sp == 1, expenditures*month_factor*12,0))%>%
  select(hh_id, item_code, expenditures_year, expenditures_sp_year)

# 2.14
data_r_C_1 <- data_r_C %>%
  rename(hh_id = DIRECTORIO)%>%
  select(hh_id, NC2_CC_P1, NC2_CC_P2, NC2_CC_P3_S1)%>%
  rename(item_code    = NC2_CC_P1,
         frequency    = NC2_CC_P2,
         expenditures = NC2_CC_P3_S1)%>%
  filter(!is.na(expenditures))%>%
  mutate(Type = "Gastos semanales Rural - Capitulo C")%>%
  mutate(month_factor = ifelse(frequency %in% c(1,2,3), 4.28,
                               ifelse(frequency == 4,2,
                                      ifelse(frequency == 5,1,
                                             ifelse(frequency == 6,1/2,
                                                    ifelse(frequency == 7,1/3,
                                                           ifelse(frequency == 9,1,1)))))))%>%
  mutate(expenditures_year = expenditures*month_factor)%>%
  select(hh_id, item_code, expenditures_year)%>%
  mutate(item_code = as.character(item_code))

#4.28
data_u_C_1 <- data_u_C %>%
  rename(hh_id = DIRECTORIO)%>%
  select(hh_id, NC2_CC_P1, NC2_CC_P2, NC2_CC_P3_S1)%>%
  rename(item_code    = NC2_CC_P1,
         frequency    = NC2_CC_P2,
         expenditures = NC2_CC_P3_S1)%>%
  filter(!is.na(expenditures))%>%
  mutate(Type = "Gastos diarios Urbanos - Capitulo C")%>%
  mutate(month_factor = ifelse(frequency %in% c(1,2,3),2.14,
                               ifelse(frequency == 4,2,
                                      ifelse(frequency == 5,1,
                                             ifelse(frequency == 6,1/2,
                                                    ifelse(frequency == 7,1/3,1))))))%>%
  mutate(expenditures_year = expenditures*month_factor)%>%
  select(hh_id, item_code, expenditures_year)%>%
  mutate(item_code = as.character(item_code))

#2.14
data_u_comidas_1 <- data_u_comidas %>%
  rename(hh_id = DIRECTORIO)%>%
  select(hh_id, NH_CGDUCFH_P1_1, NH_CGDUCFH_P3, NH_CGDUCFH_P5, NH_CGDUCFH_P6)%>%
  rename(item_code        = NH_CGDUCFH_P1_1,
         expenditure_type = NH_CGDUCFH_P3,
         expenditures     = NH_CGDUCFH_P5,
         frequency        = NH_CGDUCFH_P6)%>%
  mutate(Type = "Gastos diarios del hogar Urbano - Comidas preparadas fuera del hogar")%>%
  mutate(month_factor = ifelse(frequency %in% c(1,2,3),2.14,
                               ifelse(frequency == 4,2,
                                      ifelse(frequency == 5,1,
                                             ifelse(frequency == 6,1/2,
                                                    ifelse(frequency == 7,1/3,1))))))%>%
  mutate(sp = ifelse(expenditure_type %in% c(2,3,4,5,6,7),1,0))%>%
  mutate(expenditures_year    = ifelse(sp == 0, expenditures*month_factor*12,0),
         expenditures_sp_year = ifelse(sp == 1, expenditures*month_factor*12,0))%>%
  select(hh_id, item_code, expenditures_year, expenditures_sp_year)

#2.14
data_r_comidas_1 <- data_r_comidas %>%
  rename(hh_id = DIRECTORIO)%>%
  select(hh_id, NH_CGPRCFH_P1S1, NH_CGPRCFH_P5, NH_CGPRCFH_P6)%>%
  filter(!is.na(NH_CGPRCFH_P5))%>%
  rename(item_code    = NH_CGPRCFH_P1S1,
         expenditures = NH_CGPRCFH_P5,
         frequency    = NH_CGPRCFH_P6)%>%
  mutate(Type = "Gastos semanales Rural - Comidas preparadas fuera del hogar")%>%
  mutate(month_factor = ifelse(frequency %in% c(1,2,3),2.14,
                               ifelse(frequency == 4,2,
                                      ifelse(frequency == 5,1,
                                             ifelse(frequency == 6,1/2,
                                                    ifelse(frequency == 7,1/3,1))))))%>%
  mutate(expenditures_year = expenditures*month_factor)%>%
  select(hh_id, item_code, expenditures_year)

data_u_m_1 <- data_u_m %>%
  rename(hh_id = DIRECTORIO)%>%
  select(hh_id, NC2_CC_P4S1)%>%
  rename(expenditures    = NC2_CC_P4S1)%>%
  mutate(frequency = NA)%>%
  mutate(Type = "Gastos diarios Urbanos - Mercados")%>%
  mutate(item_code = "Mercado Urbanos")%>%
  mutate(expenditures_year = expenditures*12)%>%
  select(hh_id, item_code, expenditures_year)
data_r_m_1 <- data_r_m %>%
  rename(hh_id = DIRECTORIO)%>%
  select(hh_id, NC2_CC_P4S1)%>%
  rename(expenditures = NC2_CC_P4S1)%>%
  mutate(frequency = NA)%>%
  mutate(Type = "Gastos semanales Rurales - Mercados")%>%
  mutate(item_code = "Mercado Rural")%>%
  mutate(expenditures_year = expenditures*12)%>%
  select(hh_id, item_code, expenditures_year)

#4.28
data_r_p_fuera_1 <- data_r_p_fuera %>%
  rename(hh_id = DIRECTORIO)%>%
  select(hh_id, NC2R_CA_P3, NC2R_CA_P5_S1, NC2R_CA_P7_S1, NC2R_CA_P8_S1)%>%
  rename(item_code        = NC2R_CA_P3,
         expenditure_type = NC2R_CA_P5_S1,
         expenditures     = NC2R_CA_P7_S1,
         frequency        = NC2R_CA_P8_S1)%>%
  filter(!is.na(expenditures))%>%
  mutate(Type = "Gastos personales Rural - Comidas preparadas fuera del Hogar")%>%
  mutate(month_factor = ifelse(frequency %in% c(1,2,3), 4.28,
                               ifelse(frequency == 4,2,
                                      ifelse(frequency == 5,1,
                                             ifelse(frequency == 6,1/2,
                                                    ifelse(frequency == 7,1/3,
                                                           ifelse(frequency == 9,1,1)))))))%>%
  mutate(sp = ifelse(expenditure_type %in% c(2,3,4,5,6,7),1,0))%>%
  mutate(expenditures_year    = ifelse(sp == 0, expenditures*month_factor*12,0),
         expenditures_sp_year = ifelse(sp == 1, expenditures*month_factor*12,0))%>%
  select(hh_id, item_code, expenditures_year, expenditures_sp_year)

#4.28
data_u_p_fuera_1 <- data_u_p_fuera %>%
  rename(hh_id = DIRECTORIO)%>%
  select(hh_id, NH_CGPUCFH_P1_S1, NH_CGPUCFH_P3, NH_CGPUCFH_P5, NH_CGPUCFH_P6)%>%
  rename(item_code        = NH_CGPUCFH_P1_S1,
         expenditure_type = NH_CGPUCFH_P3,
         expenditures     = NH_CGPUCFH_P5,
         frequency        = NH_CGPUCFH_P6)%>%
  mutate(Type = "Gastos personales Urbano - Comidas preparadas fuera del Hogar")%>%
  mutate(month_factor = ifelse(frequency %in% c(1,2,3), 4.28,
                               ifelse(frequency == 4,2,
                                      ifelse(frequency == 5,1,
                                             ifelse(frequency == 6,1/2,
                                                    ifelse(frequency == 7,1/3,
                                                           ifelse(frequency == 9,1,1)))))))%>%
  mutate(sp = ifelse(expenditure_type %in% c(2,3,4,5,6,7),1,0))%>%
  mutate(expenditures_year    = ifelse(sp == 0, expenditures*month_factor*12,0),
         expenditures_sp_year = ifelse(sp == 1, expenditures*month_factor*12,0))%>%
  select(hh_id, item_code, expenditures_year, expenditures_sp_year)

data_menos_art_1 <- data_menos_art %>%
  rename(hh_id = DIRECTORIO)%>%
  select(hh_id, P10270, FORMA, VALOR, P10270S3, CAP)%>%
  rename(item_code          = P10270,
         expenditure_type   = FORMA,
         expenditures       = VALOR,
         frequency          = P10270S3,
         expenditure_type_2 = CAP)%>%
  mutate(frequency = as.numeric(frequency))%>%
  mutate(Type = "Gastos menos frecuentes")%>%
  mutate(month_factor = ifelse(expenditure_type_2 %in% c("A11", "B11", "C11", "E11", "F11", "H11", "J11", "K11", "L11") & (frequency %in% c(3,4,5,9) | is.na(frequency)),1,
                               ifelse(expenditure_type_2 %in% c("A11", "B11", "C11", "E11", "F11", "H11", "J11", "K11", "L11") & frequency == 6,1/2,
                                      ifelse(expenditure_type_2 %in% c("A11", "B11", "C11", "E11", "F11", "H11", "J11", "K11", "L11") & frequency == 7,1/3,
                                             ifelse(expenditure_type_2 %in% c("A11", "B11", "C11", "E11", "F11", "H11", "J11", "K11", "L11") & frequency == 8,1/12,
                                                    ifelse(expenditure_type_2 %in% c("A11", "B11", "C11", "E11", "F11", "H11", "J11", "K11", "L11") & frequency == 10,1/6,NA))))))%>%
  mutate(month_factor = ifelse(expenditure_type_2 %in% c("D11", "D12", "D13", "D14", "D15", "D16", "F12", "G11", "I11", "J12") & (frequency %in% c(3,4,5,6) | is.na(frequency)),1/3,
                               ifelse(expenditure_type_2 %in% c("D11", "D12", "D13", "D14", "D15", "D16", "F12", "G11", "I11", "J12") & frequency %in% c(7,9),1,
                                      ifelse(expenditure_type_2 %in% c("D11", "D12", "D13", "D14", "D15", "D16", "F12", "G11", "I11", "J12") & frequency == 8,1/4,
                                             ifelse(expenditure_type_2 %in% c("D11", "D12", "D13", "D14", "D15", "D16", "F12", "G11", "I11", "J12") & frequency == 10,1/2, month_factor)))))%>%
  mutate(month_factor = ifelse(expenditure_type_2 %in% c("B12", "C12", "E12", "F13", "G12", "G121", "G122", "H12", "I12", "J13", "J131", "J132", "L12", "G13", "G14", "J14") & (frequency != 9 | is.na(frequency)), 1/12, 
                               ifelse(expenditure_type_2 %in% c("B12", "C12", "E12", "F13", "G12", "G121", "G122", "H12", "I12", "J13", "J131", "J132", "L12", "G13", "G14", "J14") & frequency == 9,1,month_factor)))%>%
  mutate(expenditures_month = expenditures*month_factor)%>%
  mutate(sp = ifelse(expenditure_type %in% c(2,3,4,5,6,7),1,0))%>%
  mutate(expenditures_year = ifelse(sp == 0, expenditures_month*12,0),
         expenditures_sp_year = ifelse(sp == 1, expenditures_month*12,0))%>%
  select(hh_id, item_code, expenditures_year, expenditures_sp_year)

data_menos_med_1 <- data_menos_med %>%
  rename(hh_id = DIRECTORIO)%>%
  select(hh_id, 
         P10272S1A1, P10272S1A2,# <- how many months : Aqua
         P10272S2A1, P10272S2A2,# <- how many months : Waste
         P10272S3A1, P10272S3A2,# <- how many months : Sewerage
         P10272S4A1, P10272S4A2,# <- how many months : Electricity
         P10272S6A1, P10272S6A2,# <- how many months : Natural Gas
         P10272S7A1, P10272S7A2,# <- how many months : Telephone
         P10272S8A1, P10272S1A2)# <- how many months # Television

data_menos_med_1.1 <- data_menos_med_1 %>%
  select(hh_id, ends_with("A1"))%>%
  pivot_longer(-hh_id, names_to = "item_code", values_to = "expenditures")%>%
  mutate(item_join = str_sub(item_code, 1,8))

data_menos_med_1.2 <- data_menos_med_1 %>%
  select(hh_id, ends_with("A2"))%>%
  pivot_longer(-hh_id, names_to = "item_code_1", values_to = "frequency")%>%
  mutate(item_join = str_sub(item_code_1,1,8))

data_menos_med_2 <- left_join(data_menos_med_1.1, data_menos_med_1.2, by = c("hh_id", "item_join"))%>%
  filter(frequency != 97)%>%
  filter(expenditures > 0)%>%
  mutate(expenditures_year = expenditures*12/frequency)%>%
  select(hh_id, item_code, expenditures_year)

expenditures_final <- data_menos_med_2 %>%
  bind_rows(data_menos_art_1)%>%
  bind_rows(data_u_diarios_1)%>%
  bind_rows(data_r_diarios_1)%>%
  bind_rows(data_u_diarios_0.1)%>%
  bind_rows(data_r_diarios_0.1)%>%
  bind_rows(data_u_C_1)%>%
  bind_rows(data_r_C_1)%>%
  bind_rows(data_u_comidas_1)%>%
  bind_rows(data_r_comidas_1)%>%
  bind_rows(data_u_m_1)%>%
  bind_rows(data_r_m_1)%>%
  bind_rows(data_u_p_fuera_1)%>%
  bind_rows(data_r_p_fuera_1)%>%
  arrange(hh_id, item_code)%>%
  filter((!is.na(expenditures_year)|!is.na(expenditures_sp_year)))%>%
  filter(expenditures_year > 0 | expenditures_sp_year > 0)%>%
  # For now, we will assume expenditure data is unique
  group_by(hh_id, item_code)%>%
  summarise(expenditures_year = sum(expenditures_year),
            expenditures_sp_year = sum(expenditures_sp_year))%>%
  ungroup()

#write_csv(expenditures_final, "../0_Data/1_Household Data/3_Colombia/1_Data_Clean/expenditures_items_Colombia.csv")

expenditures_final_1 <- expenditures_final %>%
  mutate(number = nchar(item_code))%>%
  mutate(item_code_6 = ifelse(nchar(item_code)==8, str_sub(item_code,1,6), item_code))%>%
  select(-item_code)%>%
  rename(item_code = item_code_6)%>%
  group_by(hh_id, item_code)%>%
  summarise(expenditures_year    = sum(expenditures_year),
            expenditures_sp_year = sum(expenditures_year))%>%
  ungroup()

write_csv(expenditures_final_1, "../0_Data/1_Household Data/3_Colombia/1_Data_Clean/expenditures_items_Colombia.csv")

# Household Information ####

data_hogares_1 <- data_hogares %>%
  rename(hh_id = DIRECTORIO, district_0 = DOMINIO, province_0 = REGION, URBAN = P3, toilet = P814, water = P5050,
         cooking_fuel = P5080, electricity.access = P8520S1, hh_weights = FEX_C)%>%
  select(hh_id, district_0, province_0, URBAN, toilet, water, cooking_fuel, electricity.access, hh_weights)%>%
  mutate(electricity.access = ifelse(electricity.access == 2,0,electricity.access))%>%
  mutate(urban_01 = ifelse(URBAN == 1,1,0))%>%
  left_join(Province.Code, by = c("province_0" = "Province"))%>%
  left_join(District.Code, by = c("district_0" = "District"))%>%
  select(-province_0, - district_0, -URBAN)%>%
  remove_all_labels()%>%
  zap_formats()

data_personas_1 <- data_personas %>%
  rename(hh_id = DIRECTORIO)%>%
  filter(P6050 == 1)%>%
  rename(sex_hhh = P6020, age_hhh = P6040, ethnicity = P6080, edu_hhh_0 = P6210, edu_hhh_1 = P6210S1, edu_hhh_2 = P6210S2,
         ind_hhh = P6390S1)%>%
  unite(edu_hhh, c(edu_hhh_0, edu_hhh_1, edu_hhh_2), remove = FALSE, sep = "_")%>%
  select(hh_id, sex_hhh, age_hhh, edu_hhh, ind_hhh, ethnicity)

data_personas_2 <- data_personas %>%
  rename(hh_id = DIRECTORIO)%>%
  mutate(adults   = ifelse(P6040 > 15,1,0),
         children = ifelse(P6040 < 16,1,0))%>%
  group_by(hh_id)%>%
  summarise(hh_size = n(),
            adults = sum(adults),
            children = sum(children))%>%
  ungroup()

# Income

data_personas_3 <- data_personas %>%
  rename(hh_id = DIRECTORIO)%>%
  select(hh_id, P8610S1, P8612S1, starts_with("P6207"), starts_with("P6236"), # beca en dinero, subsidios en dinero
         P6585S1A1, P6585S2A1, P6585S3A1, # Auxilio o subsidio de alimentacion, Auxilio o subsidio de transporte, subsidio familiar
         P1668S1A1, # Mas familias en accion 
         P1668S2A2, # Programa de adultos mayores
         P1668S3A2, # Familias en su tierra
         P1668S4A2, # Jovenes en accion
         P1668S5A2, # Transferencias por victimizacion
         P1668S6A3, # Otro subsidio
         P9460S1, # Subsidio de desempleo
         P7500S2A1 # Pensiones o jubilaciones
         )%>%
  mutate(P8610S1 = ifelse(P6207M1 == 1 | P6207M2 == 1 | P6207M3 == 1 | P6207M4 == 1 | P6207M5 == 1,P8610S1,0), 
         P8612S1 = ifelse(P6236M1 == 1 | P6236M2 == 1 | P6236M3 == 1 | P6236M4 == 1 | P6236M5 == 1,P8612S1,0))%>%
  select(-starts_with("P6207"), - starts_with("P6236"))%>%
  mutate_at(vars(-hh_id), list(~ ifelse(is.na(.),0,.)))%>%
  mutate(inc_gov_cash = P8610S1 + P8612S1 + P1668S1A1 + P1668S2A2 + P1668S3A2 + P1668S4A2 + P1668S5A2 + P7500S2A1,
         inc_gov_monetary = P6585S1A1 + P6585S2A1 + P6585S3A1 + P1668S6A3 + P9460S1)%>%
  group_by(hh_id)%>%
  summarise(inc_gov_cash = sum(inc_gov_cash),
            inc_gov_monetary = sum(inc_gov_monetary))%>%
  ungroup()%>%
  remove_all_labels()%>%
  zap_formats()

household_information <- left_join(data_hogares_1, data_personas_1)%>%
  left_join(data_personas_2)%>%
  left_join(data_personas_3)%>%
  remove_all_labels()%>%
  zap_formats()

write_csv(household_information, "../0_Data/1_Household Data/3_Colombia/1_Data_Clean/household_information_Colombia.csv")

appliances <- data_hogares %>%
  rename(hh_id = DIRECTORIO)%>%
  select(hh_id, starts_with("P1646"))%>%
  rename_at(vars(starts_with("P1646")), list(~ str_replace(., "P1646", "")))%>%
  rename(washing_machine.01 = S4, refrigerator.01 = S5, stove.e.01 = S7, stove.g.01 = S8, microwave.01 = S9, 
         heater.01 = S10, ac.01 = S12, fan.01 = S13, vaccum.01 = S11, tv.01a = S15, tv.01b = S16, computer.01a = S25,
         computer.01b = S26, motorcycle.01 = S28, car.01 = S29)%>%
    select(hh_id, ends_with("01"), ends_with(".01a"), ends_with("01b"))%>%
  mutate_at(vars(-hh_id), list(~ str_replace(., "2", "0")))%>%
  remove_all_labels()%>%
  zap_formats()%>%
  mutate(tv.01       = ifelse(tv.01a == 1 | tv.01b == 1,1,0),
         computer.01 = ifelse(computer.01a == 1 | computer.01b == 1,1,0))%>%
  select(-computer.01a, -computer.01b, -tv.01a, -tv.01b)

write_csv(appliances, "../0_Data/1_Household Data/3_Colombia/1_Data_Clean/appliances_0_1_Colombia.csv")

# Codes ####
Education.Code.A <- stack(attr(data_personas$P6210, 'labels'))%>%
  rename(edu_A = values, Education_A = ind)%>%
  mutate(edu_A = as.character(edu_A))
Education.Code.B <- stack(attr(data_personas$P6210S1, 'labels'))%>%
  rename(edu_B = values, Education_B = ind)%>%
  mutate(edu_B = as.character(edu_B))
Education.Code.C <- stack(attr(data_personas$P6210S2, 'labels'))%>%
  rename(edu_C = values, Education_C = ind)%>%
  mutate(edu_C = as.character(edu_C))
Education.Code <- distinct(data_personas_1, edu_hhh)%>%
  arrange(edu_hhh)%>%
  separate(edu_hhh, into = c("edu_A", "edu_B", "edu_C"), sep = "_", remove = FALSE)%>%
  left_join(Education.Code.A)%>%
  left_join(Education.Code.B)%>%
  left_join(Education.Code.C)%>%
  unite(Education, c(Education_A, Education_B, Education_C), sep = ", ", remove = FALSE, na.rm = TRUE)%>%
  select(edu_hhh, Education)%>%
  write_csv(., "../0_Data/1_Household Data/3_Colombia/2_Codes/Education.Code.csv")

Industry.Code.A <- read.xlsx("T:/MSA/papers_internal/work_in_progress/Mi_Homogenized_Datainfrastructure/0_Data/1_Household Data/3_Colombia/9_Documentation/Industry_Codes_CIIU_Rev_4.xlsx")%>%
  rename(Industry = "Denominación")

Industry.Code <- distinct(data_personas_1, ind_hhh)%>%
  arrange(ind_hhh)%>%
  mutate(code_3 = str_sub(ind_hhh,1,3),
         code_2 = str_sub(ind_hhh,1,2))%>%
  left_join(Industry.Code.A, by = c("ind_hhh" = "Código"))%>%
  left_join(Industry.Code.A, by = c("code_3"  = "Código"))%>%
  left_join(Industry.Code.A, by = c("code_2"  = "Código"))%>%
  mutate(Industry_0 = ifelse(!is.na(Industry.x), Industry.x,
                             ifelse(!is.na(Industry.y), paste(Industry.y, ", Other"), paste(Industry, ", Other"))))%>%
  select(ind_hhh, Industry_0)%>%
  rename(Industry = Industry_0)%>%
  write_csv(., "../0_Data/1_Household Data/3_Colombia/2_Codes/Industry.Code.csv")
  
Ethnicity.Code <- stack(attr(data_personas_1$ethnicity, 'labels'))%>%
  rename(ethnicity = values, Ethnicity = ind)%>%
  write_csv(., "../0_Data/1_Household Data/3_Colombia/2_Codes/Ethnicity.csv")

Gender.Code <- distinct(data_personas_1, sex_hhh)%>%
  mutate(Gender = c("Male", "Female"))%>%
  write_csv(., "../0_Data/1_Household Data/3_Colombia/2_Codes/Gender.Code.csv")

Toilet.Code <- distinct(data_hogares_1, toilet)%>%
  arrange(toilet)%>%
  bind_cols(Toilet = c("Lavatroy connected to sewer", "Toilet connected to septic tank", "Toilet without connection",
                       "Latrine", "Bajamar", "No sanitary service"))%>%
  write_csv(., "../0_Data/1_Household Data/3_Colombia/2_Codes/Toilet.Code.csv")

Water.Code <- distinct(data_hogares_1, water)%>%
  mutate(water = as.numeric(water))%>%
  arrange(water)%>%
  bind_cols(Water = c("Aqueduct by pipe", "Other source by pipe", "Well with pump", "Well without pump, cistern, jagüey or auger",
                      "Rain water", "River, stream, source or spring", "Public stack", "Tank truck", "Water carrier", "Bottler or bagged water"))%>%
  write_csv(., "../0_Data/1_Household Data/3_Colombia/2_Codes/Water.Code.csv")

Cooking.Code <- distinct(data_hogares_1, cooking_fuel)%>%
  arrange(cooking_fuel)%>%
  bind_cols(Cooking_Fuel = c("Unknown", "Electricity", "Petrol, Gasolina, Kerosene",
                             "Natural gas", "LPG", "Firewood or Charcoal", "Coal", "Waste"))%>%
  write_csv(., "../0_Data/1_Household Data/3_Colombia/2_Codes/Cooking.Code.csv")

Province.Code <- count(data_hogares_1, province_0)%>%
  mutate(province.code = 1:n())%>%
  rename(Province = province_0, province = province.code)%>%
  select(-n)%>%
  write_csv(., "../0_Data/1_Household Data/3_Colombia/2_Codes/Province.Code.csv")

District.Code <- count(data_hogares_1, district_0)%>%
  mutate(district = 1:n())%>%
  select(-n)%>%
  rename(District = district_0)%>%
  write_csv(., "../0_Data/1_Household Data/3_Colombia/2_Codes/District.Code.csv")

Education.Code <- stack(attr(data_personas_1$edu_hhh, 'labels'))

# Item-Codes ####

Item.Codes.1 <- distinct(data_u_p_fuera, NH_CGPUCFH_P1_S1)%>%
  rename(item_code = NH_CGPUCFH_P1_S1)
Item.Codes.2 <- distinct(data_u_diarios_0, NH_CGDU_P1)%>%
  rename(item_code = NH_CGDU_P1)
Item.Codes.3 <- distinct(data_u_diarios, NC4_CC_P1_1)%>%
  rename(item_code = NC4_CC_P1_1)
Item.Codes.4 <- distinct(data_u_comidas, NH_CGDUCFH_P1_1)%>%
  rename(item_code = NH_CGDUCFH_P1_1)
Item.Codes.5 <- distinct(data_r_comidas, NH_CGPRCFH_P1S1)%>%
  rename(item_code = NH_CGPRCFH_P1S1)
Item.Codes.6 <- distinct(data_r_diarios,  NC2R_CE_P2)%>%
  rename(item_code = NC2R_CE_P2)
Item.Codes.7 <- distinct(data_r_diarios_0,NC2R_CA_P3)%>%
  rename(item_code = NC2R_CA_P3)
Item.Codes.8 <- distinct(data_r_p_fuera , NC2R_CA_P3)%>%
  rename(item_code = NC2R_CA_P3)
Item.Codes.9 <- distinct(data_menos_art,  P10270)%>%
  rename(item_code = P10270)

Item.Codes.agg <- bind_rows(Item.Codes.1, Item.Codes.2, Item.Codes.3, Item.Codes.4, Item.Codes.5,
                            Item.Codes.6, Item.Codes.7, Item.Codes.8, Item.Codes.9)%>%
  arrange(item_code)%>%
  distinct()%>%
  filter(item_code != "")%>%
  #mutate(char = nchar(item_code))%>%
  mutate(item_code_6 = str_sub(item_code,1,6))

Item.Codes.All <- select(Item.Codes.agg, item_code)
Item.Codes.Red <- select(Item.Codes.agg, item_code_6)

Item.Names.Red <- read_excel("T:/MSA/papers_internal/work_in_progress/Mi_Homogenized_Datainfrastructure/0_Data/1_Household Data/3_Colombia/9_Documentation/Clasificador del Gasto COICOP.xlsx", sheet = "Divisiones 01_12", skip = 2)%>%
  filter(!is.na(SUBCLASE))%>%
  rename(item_name = "Nombre del Artículo")%>%
  unite(item_code_6, c(DIVISIÓN, GRUPO, CLASE, SUBCLASE), sep = "", remove = FALSE)%>%
  filter(is.na(ARTÍCULO))%>%
  select(item_code_6, item_name)

Item.Names.All <- read_excel("T:/MSA/papers_internal/work_in_progress/Mi_Homogenized_Datainfrastructure/0_Data/1_Household Data/3_Colombia/9_Documentation/Clasificador del Gasto COICOP.xlsx", sheet = "Divisiones 01_12", skip = 2)%>%
  filter(!is.na(ARTÍCULO))%>%
  rename(item_name = "Nombre del Artículo")%>%
  unite(item_code, c(DIVISIÓN, GRUPO, CLASE, SUBCLASE, ARTÍCULO), sep = "")%>%
  select(item_code, item_name)

Item.Codes.Names.All <- left_join(Item.Codes.All, Item.Names.All)%>%
  arrange(item_code)
Item.Codes.Names.Red <- left_join(Item.Codes.Red, Item.Names.Red)%>%
  distinct()%>%
  arrange(item_code_6)

write.xlsx(Item.Codes.Names.All, "../0_Data/1_Household Data/3_Colombia/3_Matching_Tables/Item_Code_Description_Colombia.xlsx")
write.xlsx(Item.Codes.Names.Red, "../0_Data/1_Household Data/3_Colombia/3_Matching_Tables/Item_Code_Description_Colombia_6.xlsx")
