if(!require("pacman")) install.packages("pacman")

p_load("tidyverse", "Hmisc", "haven", "readr", "openxlsx", "stringi")

# Load Data

data_0_gastos   <- read_csv2("../0_Data/1_Household Data/3_Chile/1_Data_Raw/base-gastos-viii-epf-(formato-csv).csv")
data_0_personas <- read_csv2("../0_Data/1_Household Data/3_Chile/1_Data_Raw/base-personas-viii-epf-(formato-csv).csv")
gtap_codes      <- read.xlsx("../0_Data/1_Household Data/3_Chile/3_Matching_Tables/Item_GTAP_Concordance_Chile.xlsx")

# Transform Data
# select and rename variables of interest 

data_1_personas <- data_0_personas %>%
  select(ZONA, FOLIO, FE, NPERSONAS, JHOGAR, PARENTESCO, SEXO, ECIVIL, EDAD, EDUNIVEL, EDUDEPENDENCIA, CAE, AECIUO88, TRANSFERENCIAS_RECIBIDAS, VP05D)%>%
  rename(province = ZONA, hh_id = FOLIO, hh_weights = FE, hh_size = NPERSONAS, 
         head = JHOGAR, family_status = PARENTESCO, sex_hhh = SEXO, 
         age_hhh = EDAD, marital_status = ECIVIL, edu_hhh = EDUNIVEL, 
         edu_institution = EDUDEPENDENCIA, empl_hhh = CAE,
         ind_hhh = AECIUO88, cash_transfers_year = TRANSFERENCIAS_RECIBIDAS)%>%
  mutate(children = ifelse(age_hhh <18, 1, 0),
         adults   = ifelse(age_hhh >17,1,0))%>%
  group_by(hh_id)%>%
  mutate(children = sum(children),
         adults   = sum(adults))%>%
  ungroup()%>%
  mutate(electricity.access = ifelse(VP05D == 1,1,NA))%>%
  select(-VP05D)
  
income <- data_0_personas %>%
  rename(hh_id = FOLIO)%>%
  select(hh_id, PERSONA,  TR02, TR04, TR10, INGJ, INGJ_HD)%>%
  pivot_longer(c(-hh_id,-PERSONA), names_to = "names", values_to = "values")%>%
  filter(values >0)%>%
  pivot_wider(names_from = "names", values_from = "values", values_fill = 0)%>%
  mutate(INGJ_HD = ifelse(INGJ_HD > INGJ,0, INGJ_HD))%>%
  mutate(inc_gov_cash     = TR10,
         inc_gov_monetary = TR04 + TR02 + INGJ - INGJ_HD)%>%
  group_by(hh_id)%>%
  summarise(inc_gov_cash = sum(inc_gov_cash),
            inc_gov_monetary = sum(inc_gov_monetary))%>%
  ungroup()

child <- NULL
# remove all entries except for heads of household
data_2_personas <- data_1_personas %>%
  filter(head==1)%>%
  select(hh_id, hh_size, hh_weights, province, sex_hhh, marital_status, adults, children, age_hhh, edu_hhh, edu_institution, ind_hhh)%>%
  left_join(income)%>%
  mutate(inc_gov_cash     = ifelse(is.na(inc_gov_cash),0, inc_gov_cash),
         inc_gov_monetary = ifelse(is.na(inc_gov_monetary),0, inc_gov_monetary))

# vp05a - vp05j do you have to pay for..? (gas, rent, electricity, security..)
# TA09 net salary last month
# TA11 average income over 12 months self reported
# JU01 retirement income? y/n JU02 how much? gross per month
# TR01 pension -||- TR02 -||-
# TR08 last month money from state (1) from non-profit (2) both(3) no(4) TR09 how much
# TR11 did you receive non-money donations? y/n TR13 from whom? homes (1) non-profits(2) state(3) TR14 how much worth 

code_marital <- data.frame(marital_status = c(seq(1,5,1),-88,-99),       marital_Status  = c("Married", "Civil Union", "Divorced", "Widowed", "Unmarried", "Unknown", "Unknown"))%>%
  write_csv(., "../0_Data/1_Household Data/3_Chile/2_Codes/Marital.Code.csv")
code_area    <- data.frame(province = c(1,2),                            Province        = c("Gran Santiago", "Other"))%>%
  write_csv(., "../0_Data/1_Household Data/3_Chile/2_Codes/Province.Code.csv")
code_edu     <- data.frame(edu_hhh = c(seq(1,17,1), -88, -99),           Education       = c("None", "Just starting formal education", "Special Education", "Kindergarten", "Kindergarten", "Kindergarten", "Primary Education", "Primary Education", "Secondary Education", "Secondary Education", "Bachelor's Equivalent", "Bachelor's Equivalent", "Master's Equivalent", "Professional", "Postgrad Equivalent", "Master's", "PhD", "Unknown", "Unknown"))%>%
  write_csv(., "../0_Data/1_Household Data/3_Chile/2_Codes/Education.Code.csv")
code_eduinst <- data.frame(edu_institution = c(seq(1,12,1),-77,-88,-99), edu_institution = c("Municipal School", "Subsidized Private School", "State-Owned Technical School", "Private School", "Workplace Kindergarten", "Other Kindergarten/Elementary-/Middle School", "Technical Training Center (higher education)", "Professional Institute (higher education)", "Public University", "Private University", "Armed Forces", "Other higher education", "Unknown/None", "Unknown/None", "Unknown/None"))%>%
  write_csv(., "../0_Data/1_Household Data/3_Chile/2_Codes/Education.Code.B.csv")
code_jobind  <- data.frame(ind_hhh = c(seq(1,10,1),-77,-88),             Industry = c("Government and Public Administration", "Science", "Technicians and Professionals", "Office Workers", "Service Workers and Vendors", "Agriculture and Fishing", "Trades", "Heavy Machine Assembly and Operation", "Unskilled Labor", "Other", "Unknown/None", "Unknown/None"))%>%
  write_csv(., "../0_Data/1_Household Data/3_Chile/2_Codes/Industry.Code.csv")
code_gender  <- data.frame(sex_hhh = c(1,2), Gender = c("Male", "Female"))%>%
  write_csv(., "../0_Data/1_Household Data/3_Chile/2_Codes/Gender.Code.csv")


data_personas <- data_2_personas %>%
  left_join(code_marital, by = "marital_status")%>%
  left_join(code_area, by = "province")%>%
  left_join(code_edu, by = "edu_hhh")%>%
  left_join(code_eduinst, by = "edu_institution")%>%
  left_join(code_jobind, by = "ind_hhh")

# reduce gastos to the necessary size
# reduce item code to 6 digit code if necessary

data_gastos <- data_0_gastos %>%
  unite(item_code, c(D, G, C, SC, P), sep = "", remove = FALSE)%>%
  filter(!is.na(D))%>%
  select(FOLIO, item_code, GASTO) %>%
  rename(hh_id = FOLIO, expenditures_year = GASTO)%>%
  mutate(item_code = ifelse(stri_length(item_code) == 7, paste("0",item_code,sep = ""), item_code))%>%
  mutate(smallCode = substring(item_code,1,6),
         expenditures_year = expenditures_year*12)

gtap_codes1 <- gtap_codes  %>%
  select (-Explanation) %>%
  pivot_longer(-GTAP, names_to = "drop", values_to = "item_code")%>%
  filter(!is.na(item_code))%>%
  select(GTAP, item_code)%>%
  mutate(GTAP = ifelse(GTAP == "gas" | GTAP == "gdt", "gasgdt", GTAP))

data_1_gastos <- data_gastos %>%
  left_join(gtap_codes1, by = c("item_code"))%>%
  left_join(rename(gtap_codes1, GTAPB = GTAP), by = c("smallCode" = "item_code"))%>%
  mutate(item_code_new = ifelse(is.na(GTAP), smallCode,
                                ifelse(is.na(GTAPB), item_code, "FAIL")))

data_gastos <- data_1_gastos%>%
  select(hh_id, item_code_new, expenditures_year)%>%
  rename(item_code = item_code_new)%>%
  group_by(hh_id, item_code)%>%
  summarise(expenditures_year = sum(expenditures_year))%>%
  ungroup()%>%
  filter(hh_id %in% data_personas$hh_id)

# write data into new files 
write_csv(data_2_personas, "../0_Data/1_Household Data/3_Chile/1_Data_Clean/household_information_Chile.csv")
write_csv(data_gastos,     "../0_Data/1_Household Data/3_Chile/1_Data_Clean/expenditures_items_Chile.csv")
