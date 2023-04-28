# 0       General ####

# Author: L. Missbach, missbach@mcc-berlin.net - test

carbon.price <- 40 # in USD/tCO2

# 1       Packages ####

if(!require("pacman")) install.packages("pacman")

p_load("haven", "Hmisc", "openxlsx", "rattle", "scales", "tidyverse")

options(scipen=999)

# 1.1     Setup ####

for(Country.Name in c("Argentina", "Barbados", "Bolivia", "Brazil", "Chile", "Colombia", "Costa Rica", "Dominican Republic", "Ecuador", "El Salvador", "Guatemala", "Mexico", "Nicaragua", "Paraguay" ,"Peru", "Uruguay")) {

Country_Year <- data.frame(Country = c("Argentina", "Barbados", "Bolivia", "Brazil", "Chile", "Colombia", "Costa Rica", "Dominican Republic", "Ecuador", "El Salvador", "Guatemala", "Mexico","Nicaragua", "Paraguay", "Peru", "Uruguay"), 
                           Year =    c("2017",     "2016"  ,    "2019",    "2017"  , "2018",  "2016",     "2018",       "2018",               "2013",    "2015",        "2014",     "2018",   "2014",    "2011",       "2019", "2016"))

Year_0 <- Country_Year$Year[Country_Year$Country == Country.Name]

print(paste0(Country.Name, " Start"))

# 2       Load Household and Expenditure File ####

path_0                  <-list.files("../0_Data/1_Household Data/")[grep(Country.Name, list.files("../0_Data/1_Household Data/"), ignore.case = T)]

household_information   <- read_csv(sprintf("../0_Data/1_Household Data/%s/1_Data_Clean/household_information_%s.csv", path_0, Country.Name), col_types = cols(hh_id = col_character()))
expenditure_information <- read_csv(sprintf("../0_Data/1_Household Data/%s/1_Data_Clean/expenditures_items_%s.csv", path_0, Country.Name), col_types = cols(hh_id = col_character()))
if(Country.Name == "Bolivia"){expenditure_information <- read_csv(sprintf("../0_Data/1_Household Data/%s/1_Data_Clean/expenditures_items_%s.csv", path_0, Country.Name), col_types = cols(hh_id = col_character(),item_code = col_character()))}
if(Country.Name != "Chile")  {appliances_0            <- read_csv(sprintf("../0_Data/1_Household Data/%s/1_Data_Clean/appliances_0_1_%s.csv", path_0, Country.Name), col_types = cols(hh_id = col_character()))}

if(ncol(expenditure_information)>4){
print("Warning! Expenditure-DF is in Wide-Format.")
}

if(nrow(count(expenditure_information, hh_id)) != nrow(count(household_information, hh_id))) print("WARNING!")

if(sum(is.na(household_information$inc_gov_cash))==nrow(household_information)){break}

# 3       Data Cleaning ####
# 3.1     Check for Duplicates ####

# Multiple checks for duplicates

# 1: Household information

household_information_1 <- household_information %>%
  group_by_at(vars(-hh_id))%>%
  mutate(number = n(),
         flag = ifelse(number > 1,1,0))%>%
  ungroup()

if(nrow(filter(household_information_1, flag != 0))>0) View(filter(household_information_1, flag != 0))

hh_duplicates_information <- household_information_1 %>%
  filter(flag != 0)%>%
  select(hh_id)

print(paste0(sprintf("For %s, ", Country.Name), nrow(hh_duplicates_information), " households have duplicate information."))

if("expenditures_sp_year" %in% colnames(expenditure_information)){
expenditure_information <- expenditure_information %>%
  select(-expenditures_sp_year)
}

# 2: Expenditure information. Exact duplicates for expenditures - see below for more detailed approach
expenditure_information_1 <- expenditure_information %>%
  pivot_wider(names_from = "item_code", values_from = "expenditures_year")%>%
  group_by_at(vars(-hh_id))%>%
  mutate(number = n(),
         flag = ifelse(number > 1,1,0))%>%
  ungroup()

print(paste("There are ", nrow(count(expenditure_information, hh_id)) - nrow(filter(expenditure_information_1, flag == 0)), sprintf(" cases of exact duplicates of expenditures on the item level in %s.", Country.Name)))

hh_duplicates_expenditures_1 <- expenditure_information_1 %>%
  filter(flag != 0)%>%
  select(hh_id)

# 3: Expenditure information. Alternative: calculates share of duplicates on the item level

expenditure_information_2 <- expenditure_information %>%
  filter(!is.na(expenditures_year) & expenditures_year != 0)%>%
  group_by(item_code, expenditures_year)%>%
  mutate(duplicate_flag = ifelse(n()>1,1,0))%>%
  ungroup()%>%
  group_by(hh_id)%>%
  mutate(duplicate_share = sum(duplicate_flag)/n())%>%
  ungroup()

hh_duplicates_expenditures_2 <- expenditure_information_2 %>%
  filter(duplicate_share == 1)%>%
  select(hh_id)%>%
  distinct()

# 4: Expenditure information. Total amount of expenditures on all items.

expenditure_information_3 <- expenditure_information %>%
  filter(!is.na(expenditures_year) & expenditures_year != 0)%>%
  group_by(hh_id)%>%
  summarise(hh_expenditures = sum(expenditures_year))%>%
  ungroup()%>%
  group_by(hh_expenditures)%>%
  mutate(duplicate_flag_2 = ifelse(n()>1,1,0))%>%
  ungroup()

if(nrow(filter(expenditure_information_3, duplicate_flag_2 ==1))>1) print("Warning! Two or more households spend exactly the same amount of money on all their items.")
print(paste0(nrow(filter(expenditure_information_3, duplicate_flag_2 == 1)), sprintf(" households report the same amount of expenditures on all their items in %s.", Country.Name)))

hh_duplicates_expenditures_3 <- expenditure_information_3 %>%
  filter(duplicate_flag_2 == 1)%>%
  select(hh_id)

# 5: Expenditure information. Negative total expenditures

expenditure_information_4 <- expenditure_information_3 %>%
  mutate(flag_negative = ifelse(hh_expenditures < 0,1,0))

hh_negative_expenditures_4 <- expenditure_information_4 %>%
  filter(flag_negative == 1)%>%
  select(hh_id)

# 3.1.1   Duplicate Removal ####

# hh_duplicates_information captures all households, whose characteristics are identical with another --> needs careful consideration on whether these are actual duplicates
# hh_duplicates_expenditures_1 captures all households, who spend exactly the same amount of money on each item than any other households --> likely duplicate
# hh_duplicates_expenditures_2 captures all households, who do not report any individual amount of expenditures on any items. 
# Each level of expenditures for any item is shared with another household --> needs careful consideration on whether these are actual duplicates --> likely no duplicate
# hh_duplicates_expenditures_3 captures all households, who report the same amount of total expenditures as some other household --> likely no duplicate, but check individually for your country

# If you have identified duplicates and want to delete them, do the following:
# select the corresponding line with hh_ids

if(Country.Name == "Costa Rica" | Country.Name == "Colombia"){

household_information <- household_information %>%
   filter(!hh_id %in% hh_duplicates_expenditures_1$hh_id)%>%
  filter(!hh_id %in% hh_duplicates_expenditures_1$hh_id)


expenditure_information <- expenditure_information %>%
   filter(!hh_id %in% hh_duplicates_expenditures_1$hh_id)%>%
  filter(!hh_id %in% hh_duplicates_expenditures_1$hh_id)

}

if(Country.Name == "Dominican Republic" | Country.Name == "Bolivia" | Country.Name == "Peru"){
  household_information <- household_information %>%
    filter(!hh_id %in% hh_duplicates_information$hh_id)
  
  expenditure_information <- expenditure_information %>%
    filter(!hh_id %in% hh_duplicates_information$hh_id)
}

if(Country.Name == "El Salvador" | Country.Name == "Mexico"){
  household_information <- household_information %>%
    filter(!hh_id %in% hh_duplicates_information$hh_id)%>%
    filter(!hh_id %in% hh_duplicates_expenditures_1$hh_id)
  
  expenditure_information <- expenditure_information %>%
    filter(!hh_id %in% hh_duplicates_information$hh_id)%>%
    filter(!hh_id %in% hh_duplicates_expenditures_1$hh_id)
  
}

if(Country.Name == "Barbados"){
  expenditure_information <- expenditure_information %>%
    filter(!hh_id %in% hh_duplicates_expenditures_1$hh_id)
  
  household_information <- household_information %>%
    filter(hh_id %in% expenditure_information$hh_id)
  
  appliances_0 <- appliances_0 %>%
    filter(hh_id %in% expenditure_information$hh_id)
}

rm(expenditure_information_1, expenditure_information_2, expenditure_information_3, household_information_1, 
   hh_duplicates_expenditures_1, hh_duplicates_expenditures_2, hh_duplicates_expenditures_3, hh_duplicates_information,
   hh_negative_expenditures_4, expenditure_information_4)

# 3.2     Cleaning per Item_code ####

expenditure_information_4 <- expenditure_information %>%
  # pivot_longer(-hh_id, names_to = "item_code", values_to = "expenditures") %>%
  left_join(select(household_information, hh_id, hh_weights))%>%
  # here: negative values are deleted
  filter(!is.na(expenditures_year) & expenditures_year > 0 )%>%
  group_by(item_code)%>%
  mutate(outlier_95 = wtd.quantile(expenditures_year, weights = hh_weights, probs = 0.95),
         outlier_99 = wtd.quantile(expenditures_year, weights = hh_weights, probs = 0.99),
         median_exp = wtd.quantile(expenditures_year, weights = hh_weights, probs = 0.5),
         mean_exp   = wtd.mean(    expenditures_year, weights = hh_weights))%>%
  ungroup()%>%
  mutate(flag_outlier_95 = ifelse(expenditures_year>= outlier_95,1,0),
         flag_outlier_99 = ifelse(expenditures_year>= outlier_99,1,0))%>%
  # this line replaces all expenditures which are above the 99th percentile for each item to the median
  mutate(expenditures = ifelse(flag_outlier_99 == 1, median_exp, expenditures_year))%>%
  select(hh_id, item_code, expenditures, hh_weights)

expenditure_information <- expenditure_information_4 %>%
  select(-hh_weights)

# 3.2.1   Cleaning per Total Expenditures (99%) ####

expenditure_information_4.1 <- expenditure_information_4 %>%
  group_by(hh_id) %>%
  mutate(total_expenditures = sum(expenditures)) %>%
  ungroup() %>%
  mutate(outlier_95 = wtd.quantile(total_expenditures, weights = hh_weights, probs = 0.95),
         outlier_99 = wtd.quantile(total_expenditures, weights = hh_weights, probs = 0.99))

expenditure_outlier <- expenditure_information_4.1 %>%
  filter(total_expenditures >= outlier_99)%>%
  select(hh_id)%>%
  distinct()

if(Country.Name == ""){
# if you would like to delete those outliers, please do the following

household_information <- household_information %>%
  filter(!hh_id %in% expenditure_outlier$hh_id)

expenditure_information <- expenditure_information%>%
  filter(!hh_id %in% expenditure_outlier$hh_id)
}

print("Expenditure data cleaned!")

rm(expenditure_information_4.1, expenditure_information_4, expenditure_outlier)

# 4       Summary Statistics ####

# Instead, see CPI_3_Econometric_Analysis_LCA.R

# _____   ####
# 5       Transformation and Modelling ####

# 5.1     Load Additional Data ####
# 5.1.1   Supplementary Data ####
# Exchange Rates

information.ex <- read.xlsx("../0_Data/9_Supplementary Data/Exchange_Rates_2014.xlsx") # from World Bank

exchange.rate  <- as.numeric(information.ex$exchange_rate[information.ex$Country == Country.Name]) # not ppp-adjusted

# CPI-Adjustment (Inflation/Deflation)

cpis <- read.xlsx("../0_Data/9_Supplementary Data/IMF_Consumer_Price_Index_Inflation_Average.xlsx")

cpis_0 <- cpis %>%
  select(Country, starts_with("2"))%>%
  filter(Country == Country.Name)

cpis_1 <- cpis_0 %>%
  mutate_at(vars('2010':'2019'), function(x) x = as.numeric(x))%>%
  mutate_at(vars('2010':'2019'), function(x) x = 1 + x/100)%>%
  rename_at(vars(starts_with("2")), list(~ str_replace(., "^", "Year_")))%>%
  mutate(inflation_factor = ifelse(Year_0 == 2010, Year_2011*Year_2012*Year_2013*Year_2014,
                                   ifelse(Year_0 == 2011, Year_2012*Year_2013*Year_2014,
                                          ifelse(Year_0 == 2012, Year_2013*Year_2014,
                                                 ifelse(Year_0 == 2013, Year_2014,
                                                        ifelse(Year_0 == 2014, 1,
                                                               ifelse(Year_0 == 2015, 1/Year_2015,
                                                                      ifelse(Year_0 == 2016, 1/(Year_2015*Year_2016),
                                                                             ifelse(Year_0 == 2017, 1/(Year_2015*Year_2016*Year_2017),
                                                                                    ifelse(Year_0 == 2018, 1/(Year_2015*Year_2016*Year_2017*Year_2018),
                                                                                           ifelse(Year_0 == 2019, 1/(Year_2015*Year_2016*Year_2017*Year_2018*Year_2019),0)))))))))))


inflation_factor <- cpis_1$inflation_factor[cpis_1$Country == Country.Name]

rm(cpis_1, cpis_0, information.ex, cpis)

# 5.1.2   Matching GTAP Concordance ####

matching <- read.xlsx(sprintf("../0_Data/1_Household Data/%s/3_Matching_Tables/Item_GTAP_Concordance_%s.xlsx", path_0, Country.Name))

if(Country.Name == "Colombia"){
  matching <- matching %>%
    mutate(X41 = as.character(X41),
           X42 = as.character(X42),
           X43 = as.character(X43))}

if(Country.Name == "Bolivia"){
  matching <- matching %>%
    mutate_at(.vars = vars(-GTAP), .funs = list(~ as.character(.)))
}

matching <- matching %>%
  select (-Explanation) %>%
  pivot_longer(-GTAP, names_to = "drop", values_to = "item_code")%>%
  filter(!is.na(item_code))%>%
  select(GTAP, item_code)%>%
  mutate(GTAP = ifelse(GTAP == "gas" | GTAP == "gdt", "gasgdt", GTAP))

# Check if single item codes are assigned to two different GTAP categories

item_codes <- select(expenditure_information, item_code)%>%
  distinct()%>%
  left_join(matching)%>%
  filter(is.na(GTAP))

if(nrow(item_codes != 0))(paste("WARNING! Item-Codes missing in Excel-File!"))

matching.check <- count(matching, item_code)%>%
  filter(n != 1)

if(nrow(matching.check) != 0) (paste("WARNING! Item-Codes existing with two different GTAP-categories in Excel-File"))

if(nrow(item_codes != 0) | nrow(matching.check) != 0) break

rm(matching.check, item_codes)

# 5.1.3   Matching Category Concordance ####

categories <- read.xlsx(sprintf("../0_Data/1_Household Data/%s/3_Matching_Tables/Item_Categories_Concordance_%s.xlsx", path_0, Country.Name), colNames = FALSE)

if(Country.Name == "Bolivia"){
  categories <- categories %>%
    mutate_at(.vars = vars(-X1), .funs = list(~ as.character(.)))
}

categories <- categories %>%
  pivot_longer(-X1, names_to = "drop", values_to = "item_code")%>%
  filter(!is.na(item_code))%>%
  select(X1, item_code)%>%
  rename(category = X1)

item_codes <- select(expenditure_information, item_code)%>%
  distinct()%>%
  left_join(categories)%>%
  filter(is.na(category))

if(nrow(item_codes != 0))(paste("WARNING! Item-Codes missing in Category-Excel-File!"))

matching.check <- count(categories, item_code)%>%
  filter(n != 1)

if(nrow(matching.check) != 0) (paste("WARNING! Item-Codes existing with two different Categories-categories in Excel-File"))

if(nrow(item_codes != 0) | nrow(matching.check) != 0) break

rm(matching.check, item_codes)

# 5.1.4   Matching Fuel Concordance ####

fuels <- read.xlsx(sprintf("../0_Data/1_Household Data/%s/3_Matching_Tables/Item_Fuel_Concordance_%s.xlsx", path_0, Country.Name), colNames = FALSE)

fuels <- fuels %>%
  pivot_longer(-X1, names_to = "drop", values_to = "item_code")%>%
  filter(!is.na(item_code))%>%
  rename(fuel = X1)%>%
  select(fuel, item_code)

if(Country.Name == "Paraguay"){fuels$item_code <- as.character(fuels$item_code)}

energy <- filter(categories, category == "energy")%>%
  full_join(fuels)%>%
  filter(is.na(fuel) | is.na(category))

if(nrow(energy) >0) print("Warning. Watch out for energy item codes.")

rm(energy)

# 5.1.5   Vector with Carbon Intensities ####

if(Country.Name != "Barbados"){carbon_intensities_0 <- read.xlsx("../0_Data/2_IO Data/GTAP_10_MRIO/Carbon_Intensities_Full_0.xlsx", sheet = Country.Name)}
if(Country.Name == "Barbados"){carbon_intensities_0 <- read.xlsx("../0_Data/2_IO Data/GTAP_10_MRIO/Carbon_Intensities_Full_0.xlsx", sheet = "Rest_of_the_Caribbean")}

GTAP_code            <- read_delim("../0_Data/2_IO Data/GTAP_10_MRIO/GTAP10.csv", ";", escape_double = FALSE, trim_ws = TRUE)

carbon_intensities   <- left_join(GTAP_code, carbon_intensities_0, by = c("Number"="GTAP"))%>%
  select(-Explanation, - Number)%>%
  mutate(GTAP = ifelse(GTAP == "gas" | GTAP == "gdt", "gasgdt", GTAP))%>%
  group_by(GTAP)%>%
  summarise(across(CO2_Mt:Total_HH_Consumption_MUSD, ~ sum(.)))%>%
  ungroup()%>%
  mutate(CO2_t_per_dollar_global      = CO2_Mt/            Total_HH_Consumption_MUSD,
         CO2_t_per_dollar_national    = CO2_Mt_within/     Total_HH_Consumption_MUSD,
         CO2_t_per_dollar_electricity = CO2_Mt_Electricity/Total_HH_Consumption_MUSD,
         CO2_t_per_dollar_transport   = CO2_Mt_Transport/  Total_HH_Consumption_MUSD)%>%
  select(GTAP, starts_with("CO2_t"))

rm(carbon_intensities_0, GTAP_code)

# ____    ####
# 6       Transformation of Data ####

# 6.1     Anonymising Household-ID ####

household_ids <- select(household_information, hh_id)%>%
  distinct()%>%
  mutate(hh_id_new = 1:n())

household_information <- left_join(household_information, household_ids)%>%
  select(hh_id_new, everything(), -hh_id)%>%
  rename(hh_id = hh_id_new)

expenditure_information <- left_join(expenditure_information, household_ids)%>%
  select(hh_id_new, everything(), -hh_id)%>%
  rename(hh_id = hh_id_new)

if(Country.Name == "Peru"){
  CF_1 <- read_csv("../0_Data/1_Household Data/3_Peru/1_Data_Clean/hi_Peru_CF.csv")%>%
    left_join(household_ids)%>%
    select(hh_id_new, everything(), -hh_id)%>%
    rename(hh_id = hh_id_new)%>%
    write_csv(., "../1_Carbon_Pricing_Incidence/3_Analyses/1_LAC_2021/4_Transformed Data/CF_Peru.csv")
}

if(Country.Name != "Chile"){appliances_1 <- left_join(appliances_0, household_ids)%>%
  select(hh_id_new, everything(), - hh_id)%>%
  rename(hh_id = hh_id_new)

write_csv(appliances_1, sprintf("../1_Carbon_Pricing_Incidence/3_Analyses/1_LAC_2021/4_Transformed Data/appliances_0_1_new_%s.csv", Country.Name))
}
rm(household_ids, appliances_1, appliances_0)

basic_household_information <- household_information %>%
  select(hh_id, hh_size, hh_weights)

# 6.2     Merging Expenditure Data and GTAP ####

expenditure_information_1 <- left_join(expenditure_information, matching, by = "item_code")%>%
  filter(GTAP != "deleted")

# 6.3     Assign Households to Expenditure Bins ####

binning_0 <- expenditure_information_1 %>%
  group_by(hh_id)%>%
  mutate(hh_expenditures = sum(expenditures))%>%
  ungroup()%>%
  left_join(basic_household_information)%>%
  mutate(hh_expenditures_pc = hh_expenditures/hh_size)%>%
  select(hh_id, hh_expenditures, hh_expenditures_pc, hh_weights)%>%
  filter(!duplicated(hh_id))%>%
  mutate(Income_Group_5  = as.numeric(binning(hh_expenditures_pc, bins = 5,  method = c("wtd.quantile"), weights = hh_weights)),
         Income_Group_10 = as.numeric(binning(hh_expenditures_pc, bins = 10, method = c("wtd.quantile"), weights = hh_weights)))%>%
  select(hh_id, hh_expenditures, hh_expenditures_pc, starts_with("Income"))

# 6.4     Calculating Expenditure Shares on Energy/Food/Goods/Services ####

expenditures_categories_0 <- left_join(expenditure_information, categories)%>%
  filter(category != "deleted" & category != "in-kind" & category != "self-produced")%>%
  group_by(hh_id, category)%>%
  summarise(expenditures_category = sum(expenditures))%>%
  ungroup()%>%
  group_by(hh_id)%>%
  mutate(share_category = expenditures_category/sum(expenditures_category))%>%
  ungroup()%>%
  select(hh_id, category, share_category)%>%
  pivot_wider(names_from = "category", values_from = "share_category", names_prefix = "share_", values_fill = 0)

# 6.5     Calculating Expenditure Shares on detailed Energy Items ####

expenditures_fuels <- left_join(expenditure_information, fuels)%>%
  filter(!is.na(fuel))%>%
  group_by(hh_id, fuel)%>%
  summarise(expenditures = sum(expenditures))%>%
  ungroup()%>%
  mutate(expenditures = expenditures*inflation_factor*exchange.rate)%>%
  pivot_wider(names_from = "fuel", values_from = "expenditures", names_prefix = "exp_USD_", values_fill = 0)

expenditures_fuels <- distinct(household_information, hh_id)%>%
  left_join(expenditures_fuels)%>%
  mutate_at(vars(-hh_id), list(~ ifelse(is.na(.),0,.)))

# 6.6     Summarising Expenditures on the GTAP Level ####

expenditure_information_1 <- expenditure_information_1 %>%
  group_by(hh_id, GTAP)%>%
  summarise(expenditures = sum(expenditures))%>%
  ungroup()%>%
  # We inflate/deflate expenditures to 2014 and convert 2014 expenditures to USD (no PPP-adjustment)
  mutate(expenditures_USD_2014 = expenditures*inflation_factor*exchange.rate)%>%
  group_by(hh_id)%>%
  mutate(hh_expenditures_USD_2014 = sum(expenditures_USD_2014))%>%
  ungroup()

expenditure_information_2 <- expenditure_information_1 %>%
  group_by(hh_id)%>%
  summarise(hh_expenditures_LCU = sum(expenditures))%>%
  ungroup()

rm(basic_household_information)

# 6.7     Merging Expenditures and Carbon Intensities ####

household_carbon_footprint <- left_join(expenditure_information_1, carbon_intensities, by = "GTAP")%>%
  filter(GTAP != "other")%>%
  mutate(CO2_t_global      = expenditures_USD_2014*CO2_t_per_dollar_global,
         CO2_t_national    = expenditures_USD_2014*CO2_t_per_dollar_national,
         CO2_t_electricity = expenditures_USD_2014*CO2_t_per_dollar_electricity,
         CO2_t_transport   = expenditures_USD_2014*CO2_t_per_dollar_transport)%>%
  select(-starts_with("CO2_t_per"))%>%
  group_by(hh_id)%>%
  summarise(hh_expenditures_USD_2014 = first(hh_expenditures_USD_2014),
            CO2_t_global      = sum(CO2_t_global),    
            CO2_t_national    = sum(CO2_t_national),  
            CO2_t_electricity = sum(CO2_t_electricity),
            CO2_t_transport   = sum(CO2_t_transport))%>%
  ungroup()

rm(expenditure_information_1)

# 6.8     Add-On: Sectoral Emissions and additional expenditures ####

household_sectoral_carbon_footprint <- left_join(expenditure_information, matching, by = "item_code")%>%
  left_join(categories)%>%
  left_join(fuels)%>%
  filter(GTAP != "deleted")%>%
  filter(category != "deleted" & category != "in-kind" & category != "self-produced" & category != "other_binning" & GTAP != "other")%>%
  mutate(expenditures_USD_2014 = expenditures*inflation_factor*exchange.rate)%>%
  mutate(aggregate_category = ifelse(category == "food" | category == "goods" | category == "services", category, 
                                     ifelse(is.na(category), "NA_1", 
                                            ifelse(category == "energy" & (is.na(fuel)| fuel == "Biomass" | fuel == "Firewood"), "other_energy",
                                                   ifelse(category == "energy" & (fuel == "Diesel" | fuel == "Petrol"), "transport_fuels",
                                                          ifelse(category == "energy" & fuel == "Electricity", "Electricity",
                                                                 ifelse(category == "energy" & (fuel == "Gas" | fuel == "LPG" | fuel == "Kerosene" | fuel == "Coal"), "cooking_fuels", "NA_2")))))))%>%
  left_join(carbon_intensities, by = "GTAP")%>%
  mutate(CO2_s_t_national    = expenditures_USD_2014*CO2_t_per_dollar_national)%>%
  select(-starts_with("CO2_t_per"))%>%
  group_by(hh_id, aggregate_category)%>%
  summarise(CO2_s_t_national      = sum(CO2_s_t_national))%>%
  ungroup()%>%
  mutate(exp_s_CO2_national    = CO2_s_t_national*carbon.price)%>%
  select(-CO2_s_t_national)%>%
  pivot_wider(names_from = "aggregate_category", values_from = "exp_s_CO2_national", values_fill = 0, names_prefix = "exp_s_")%>%
  rename(exp_s_Goods = exp_s_goods, exp_s_Services = exp_s_services, exp_s_Food = exp_s_food)

write_csv(household_sectoral_carbon_footprint, 
          sprintf("../1_Carbon_Pricing_Incidence/3_Analyses/1_LAC_2021/4_Transformed Data/Sectoral_Burden_%s.csv",  Country.Name))

rm(expenditure_information, matching, exchange.rate, inflation_factor, fuels, categories, household_sectoral_carbon_footprint)

# ____    ####
# 7       Model / Calculating Carbon Incidence ####
# 7.1     Analysis of Carbon Pricing Incidence ####

household_carbon_incidence <- household_carbon_footprint %>%
  mutate(exp_CO2_global              = CO2_t_global*carbon.price,
         exp_CO2_national            = CO2_t_national*carbon.price,
         exp_CO2_electricity         = CO2_t_electricity*carbon.price,
         exp_CO2_transport           = CO2_t_transport*carbon.price)%>%
  mutate(burden_CO2_global           = exp_CO2_global/     hh_expenditures_USD_2014,
         burden_CO2_national         = exp_CO2_national/   hh_expenditures_USD_2014,
         burden_CO2_electricity      = exp_CO2_electricity/hh_expenditures_USD_2014,
         burden_CO2_transport        = exp_CO2_transport/  hh_expenditures_USD_2014)

final_incidence_information <- household_carbon_incidence %>%
  left_join(binning_0)%>%
  left_join(expenditures_categories_0)%>%
  left_join(expenditures_fuels)

if(max(final_incidence_information$CO2_t_global) == "Inf") "Warning! Check Intensities."

if(max(final_incidence_information$CO2_t_global) == "Inf") break

write_csv(final_incidence_information, sprintf("../1_Carbon_Pricing_Incidence/3_Analyses/1_LAC_2021/4_Transformed Data/Carbon_Pricing_Incidence_%s.csv",  Country.Name))
write_csv(household_information,       sprintf("../1_Carbon_Pricing_Incidence/3_Analyses/1_LAC_2021/4_Transformed Data/household_information_%s_new.csv", Country.Name))
write_csv(left_join(expenditures_fuels, expenditure_information_2), sprintf("../1_Carbon_Pricing_Incidence/3_Analyses/1_LAC_2021/4_Transformed Data/fuel_expenditures_%s.csv", Country.Name))

rm(final_incidence_information, household_carbon_incidence, household_carbon_footprint, binning_0, expenditures_categories_0, household_information, expenditure_information_2, expenditures_fuels)

print(paste0("End ", Country.Name))
}
