if(!require("pacman")) install.packages("pacman")

p_load("tidyverse", "Hmisc", "haven", "readr", "openxlsx", "sjlabelled")

# Load Data ####

#data_01A  <- read_dta("../0_Data/1_Household Data/3_Barbados/1_Data_Raw/Data/Data_Portal/BSLC 2016 a/BSLC 2016/Data/RT001_Anonymized.dta") # Adds Anonymous Information
data_01P  <- read_dta("../0_Data/1_Household Data/3_Barbados/1_Data_Raw/Data/Data_Portal/BSLC 2016 a/BSLC 2016/Data/RT001_Public.dta")
#data_02A  <- read_dta("../0_Data/1_Household Data/3_Barbados/1_Data_Raw/Data/Data_Portal/BSLC 2016 a/BSLC 2016/Data/RT002_Anonymized.dta")
data_02P  <- read_dta("../0_Data/1_Household Data/3_Barbados/1_Data_Raw/Data/Data_Portal/BSLC 2016 a/BSLC 2016/Data/RT002_Public.dta")
# data_03P  <- read_dta("../0_Data/1_Household Data/3_Barbados/1_Data_Raw/Data/Data_Portal/BSLC 2016 a/BSLC 2016/Data/RT003_Public.dta")
#data_11A  <- read_dta("../0_Data/1_Household Data/3_Barbados/1_Data_Raw/Data/Data_Portal/BSLC 2016 a/BSLC 2016/Data/RT011_Anonymized.dta")
data_11P  <- read_dta("../0_Data/1_Household Data/3_Barbados/1_Data_Raw/Data/Data_Portal/BSLC 2016 a/BSLC 2016/Data/RT011_Public.dta")   # Personal expenses
# data_121  <- read_dta("../0_Data/1_Household Data/3_Barbados/1_Data_Raw/Data/Data_Portal/BSLC 2016 a/BSLC 2016/Data/RT121_Public.dta") # Crops
# data_122  <- read_dta("../0_Data/1_Household Data/3_Barbados/1_Data_Raw/Data/Data_Portal/BSLC 2016 a/BSLC 2016/Data/RT122_Public.dta") # Animals
# data_123  <- read_dta("../0_Data/1_Household Data/3_Barbados/1_Data_Raw/Data/Data_Portal/BSLC 2016 a/BSLC 2016/Data/RT123_Public.dta") # Agricultural by-products
# data_124  <- read_dta("../0_Data/1_Household Data/3_Barbados/1_Data_Raw/Data/Data_Portal/BSLC 2016 a/BSLC 2016/Data/RT124_Public.dta") # Fishing
# data_125  <- read_dta("../0_Data/1_Household Data/3_Barbados/1_Data_Raw/Data/Data_Portal/BSLC 2016 a/BSLC 2016/Data/RT125_Public.dta") # Agricultural inputs
# data_126  <- read_dta("../0_Data/1_Household Data/3_Barbados/1_Data_Raw/Data/Data_Portal/BSLC 2016 a/BSLC 2016/Data/RT126_Public.dta") # Agricultural assets
#data_140A <- read_dta("../0_Data/1_Household Data/3_Barbados/1_Data_Raw/Data/Data_Portal/BSLC 2016 a/BSLC 2016/Data/RT140_Anonymized.dta")
data_140P <- read_dta("../0_Data/1_Household Data/3_Barbados/1_Data_Raw/Data/Data_Portal/BSLC 2016 a/BSLC 2016/Data/RT140_Public.dta")   # Food Consumption
#data_141A <- read_dta("../0_Data/1_Household Data/3_Barbados/1_Data_Raw/Data/Data_Portal/BSLC 2016 a/BSLC 2016/Data/RT141_Anonymized.dta")
data_141P <- read_dta("../0_Data/1_Household Data/3_Barbados/1_Data_Raw/Data/Data_Portal/BSLC 2016 a/BSLC 2016/Data/RT141_Public.dta")   # Non-food expenses

# Transform Data ####

data_01.1 <- data_01P %>%
  rename(hh_id = hhid, hh_weights = weight, province = par, district = psu, cooking_fuel = q13_13, toilet = q13_14, water = q13_15, lighting_fuel = q13_16)%>%
  select(hh_id, hh_weights, province, district, cooking_fuel, toilet, water, lighting_fuel)%>%
  mutate(electricity.access = ifelse(is.na(lighting_fuel),0,
                                     ifelse(lighting_fuel == 3,1,0)))

data_02.1 <- data_02P %>%
  rename(hh_id = hhid)%>%
  filter(q1_02 == 1)%>%
  rename(sex_hhh = q1_03, age_hhh = q1_04, ethnicity = q1_06, religion = q1_07, country_of_birth = q2_01, edu_hhh = q3_27, ind_hhh = q9_19, ind_hhh_b = q9_17)%>%
  group_by(hh_id)%>%
  mutate(number = n())%>%
  ungroup()%>%
  select(hh_id, sex_hhh, age_hhh, ethnicity, religion, country_of_birth, edu_hhh, ind_hhh, ind_hhh_b, number)%>%
  filter(number == 1 | !is.na(age_hhh))%>%
  select(-number)

data_02.2 <- data_02P %>%
  rename(hh_id = hhid)%>%
  mutate(adults   = ifelse(is.na(q1_04) | q1_04 > 15,1,0))%>%
  mutate(children = ifelse(adults != 1,1,0))%>%
  group_by(hh_id)%>%
  summarise(hh_size  = n(),
            adults   = sum(adults),
            children = sum(children))%>%
  ungroup()

# Income

data_02.3 <- data_02P %>%
  rename(hh_id = hhid)%>%
  mutate(q4_02_1c = q4_02_1c*4,
         q4_02_2c = q4_02_2c*4,
         q4_02_3c = q4_02_3c*4,
         q10_07   = q10_07*12,
         q10_10   = q10_10*12,
         q10_11   = q10_11*12,
         q10_12   = q10_12*12)%>%
  select(hh_id, q4_02_1c, q4_02_2c, q4_02_3c, q10_07, q10_10, q10_11, q10_12)%>%
  mutate(inc_gov_cash     = q10_10 + q10_11,
         inc_gov_monetary = q4_02_1c + q4_02_2c + q4_02_3c + q10_07 + q10_12)%>%
  mutate(inc_gov_cash     = ifelse(is.na(inc_gov_cash),0, inc_gov_cash),
         inc_gov_monetary = ifelse(is.na(inc_gov_monetary),0,inc_gov_monetary))%>%
  group_by(hh_id)%>%
  summarise(inc_gov_cash     = sum(inc_gov_cash),
            inc_gov_monetary = sum(inc_gov_monetary))%>%
  ungroup()

data_021 <- left_join(data_01.1, data_02.1)%>%
  left_join(data_02.2)%>%
  left_join(data_02.3)

write_csv(data_021, "../0_Data/1_Household Data/3_Barbados/1_Data_Clean/household_information_Barbados.csv")

appliances_01 <- data_01P %>%
  select(hhid, starts_with("q13_22"))%>%
  rename(hh_id = hhid, stove.01 = q13_22a, freezer.01 = q13_22b, microwave.01 = q13_22c, refrigerator.01 = q13_22d, washing_machine.01 = q13_22e,
         dryer.01 = q13_22f, dishwasher.01 = q13_22g, heater.01 = q13_22i, tv.01 = q13_22j, computer.01 = q13_22m, ac.01 = q13_22p, car.01 = q13_22q)%>%
  select(hh_id, ends_with("01"))%>%
  mutate_at(vars(-hh_id), ~ ifelse(!is.na(.)&.>0,1,0))

write_csv(appliances_01, "../0_Data/1_Household Data/3_Barbados/1_Data_Clean/appliances_0_1_Barbados.csv")

# Expenditures 

exp_2 <- data_02P %>%
  select(hhid, starts_with("q3_21"), q5_21, q5_23, q5_25, q5_28)%>%
  rename(hh_id = hhid)%>%
  pivot_longer(-hh_id, names_to = "item_code", values_to = "expenditures")%>%
  filter(expenditures > 0)%>%
  mutate(expenditures_year = ifelse(item_code %in% c("q5_21","q5_23", "q5_25", "q5_28"), expenditures*12, expenditures))%>%
  mutate(expenditures_sp_year = 0)%>%
  select(hh_id, item_code, expenditures_year, expenditures_sp_year)%>%
  group_by(hh_id, item_code)%>%
  summarise(expenditures_year = sum(expenditures_year),
            expenditures_sp_year = sum(expenditures_year))%>%
  ungroup()

data_11.1 <- data_11P %>%
  rename(hh_id = hhid, item_code = perexpco, expenditures = q11_02)%>%
  select(hh_id, item_code, expenditures)%>%
  mutate(expenditures_year    = expenditures*52,
         expenditures_sp_year = 0)%>%
  select(hh_id, item_code, expenditures_year, expenditures_sp_year)%>%
  filter(!is.na(expenditures_year))%>%
  group_by(hh_id, item_code)%>%
  summarise(expenditures_year = sum(expenditures_year),
            expenditures_sp_year = sum(expenditures_sp_year))%>%
  ungroup()

data_140.1 <- data_140P %>%
  rename(hh_id = hhid, item_code = foodcode, expenditures = amount)%>%
  filter(!is.na(expenditures) & !is.na(anncoeff))%>%
  mutate(expenditures_year    = expenditures*anncoeff,
         expenditures_sp_year = 0)%>%
  group_by(hh_id, item_code)%>%
  summarise(expenditures_year    = sum(expenditures_year),
            expenditures_sp_year = sum(expenditures_sp_year))%>%
  ungroup()

data_141.1 <- data_141P %>%
  rename(hh_id = hhid, item_code = nonfoodc, expenditures = amount)%>%
  mutate(expenditures_year = expenditures*anncoeff,
         expenditures_sp_year = 0)%>%
  filter(!is.na(expenditures_year))%>%
  group_by(hh_id, item_code)%>%
  summarise(expenditures_year = sum(expenditures_year),
            expenditures_sp_year = sum(expenditures_sp_year))%>%
  ungroup()

expenditures_joint <- bind_rows(data_11.1, data_140.1, data_141.1)%>%
  arrange(hh_id, item_code)%>%
  mutate(item_code = as.character(item_code))%>%
  bind_rows(exp_2)%>%
  arrange(hh_id)

write_csv(expenditures_joint, "../0_Data/1_Household Data/3_Barbados/1_Data_Clean/expenditures_items_Barbados.csv")

Item.Codes.1 <- stack(attr(data_11.1$item_code, 'labels'))%>%
  rename(item_code = values, item_name = ind)
Item.Codes.2 <- stack(attr(data_140.1$item_code, 'labels'))%>%
  rename(item_code = values, item_name = ind)
Item.Codes.3 <- stack(attr(data_141.1$item_code, 'labels'))%>%
  rename(item_code = values, item_name = ind)
Item.Codes.new <- distinct(exp_2, item_code)%>%
  arrange(item_code)
Item.Codes.all <- bind_rows(Item.Codes.2, Item.Codes.3, Item.Codes.1)%>%
  mutate(item_code = as.character(item_code))%>%
  bind_rows(Item.Codes.new)

write.xlsx(Item.Codes.all, "../0_Data/1_Household Data/3_Barbados/3_Matching_Tables/Item_Code_Description_Barbados.xlsx")

# Codes ####
Ethnicity.Code <- stack(attr(data_02.1$ethnicity, 'labels'))%>%
  rename(ethnicity = values, Ethnicity = ind)%>%
  write_csv(., "../0_Data/1_Household Data/3_Barbados/2_Codes/Ethnicity.Code.csv")
Religion.Code <- stack(attr(data_02.1$religion, 'labels'))%>%
  rename(religion = values, Religion = ind)%>%
  write_csv(., "../0_Data/1_Household Data/3_Barbados/2_Codes/Religion.Code.csv")
Country.Code <- stack(attr(data_02.1$country_of_birth, 'labels'))%>%
  rename(country_of_birth = values, Country = ind)%>%
  write_csv(., "../0_Data/1_Household Data/3_Barbados/2_Codes/Country.Code.csv")
Education.Code <- stack(attr(data_02.1$edu_hhh, 'labels'))%>%
  rename(edu_hhh = values, Education = ind)%>%
  write_csv(., "../0_Data/1_Household Data/3_Barbados/2_Codes/Education.Code.csv")
Industry.Code <- stack(attr(data_02.1$ind_hhh, 'labels'))%>%
  rename(ind_hhh = values, Industry = ind)%>%
  write_csv(., "../0_Data/1_Household Data/3_Barbados/2_Codes/Industry.Code.csv")
Industry.B.Code <- stack(attr(data_02.1$ind_hhh_b, 'labels'))%>%
  rename(ind_hhh_b = values, Industry_B = ind)%>%
  write_csv(., "../0_Data/1_Household Data/3_Barbados/2_Codes/Industry.Code.B.csv")

Province.Code <- stack(attr(data_01.1$province, 'labels'))%>%
  rename(province = values, Province = ind)%>%
  write_csv(., "../0_Data/1_Household Data/3_Barbados/2_Codes/Province.Code.csv")
District.Code <- distinct(data_01P, psu, lat_cen, long_cen)%>%
  rename(district = psu)%>%
  write_csv(., "../0_Data/1_Household Data/3_Barbados/2_Codes/District.Code.csv")
Water.Code <- stack(attr(data_01.1$water, 'labels'))%>%
  rename(water = values, Water = ind)%>%
  write_csv(., "../0_Data/1_Household Data/3_Barbados/2_Codes/Water.Code.csv")
Toilet.Code <- stack(attr(data_01.1$toilet, 'labels'))%>%
  rename(toilet = values, Toilet = ind)%>%
  write_csv(., "../0_Data/1_Household Data/3_Barbados/2_Codes/Toilet.Code.csv")
Lighting.Code <- stack(attr(data_01.1$lighting_fuel, 'labels'))%>%
  rename(lighting_fuel = values, Lighting_Fuel = ind)%>%
  write_csv(., "../0_Data/1_Household Data/3_Barbados/2_Codes/Lighting.Code.csv")
Cooking.Code <- stack(attr(data_01.1$cooking_fuel, 'labels'))%>%
  rename(cooking_fuel = values, Cooking_Fuel = ind)%>%
  write_csv(., "../0_Data/1_Household Data/3_Barbados/2_Codes/Cooking.Code.csv")
