# 0     General ####

# Author: L. Missbach, missbach@mcc-berlin.net

# 0.1   Packages ####

if(!require("pacman")) install.packages("pacman")

p_load("boot", "broom", "cowplot", "fixest", "ggsci", 
       "haven", "Hmisc", "kableExtra", "marginaleffects", "margins", "Metrics", "openxlsx", "rattle", 
       "readxl", "scales", "tidyverse", "xtable")

options(scipen=999)

# 1     Loading Data ####

Country.Set <- c("Argentina", "Barbados","Bolivia", "Brazil", "Chile", "Colombia",
                 "Costa Rica", "Dominican Republic", "Ecuador",
                 "El Salvador", "Guatemala", "Mexico", "Nicaragua", "Paraguay", "Peru", "Uruguay")

data_joint_0 <- data.frame()
  
for(Country.Name in c("Argentina", "Barbados","Bolivia", "Brazil", "Chile", "Colombia",
                        "Costa Rica", "Dominican Republic", "Ecuador",
                        "El Salvador", "Guatemala", "Mexico", "Nicaragua", "Paraguay", "Peru", "Uruguay")) {
    
    carbon_pricing_incidence_0 <- read_csv(sprintf("../1_Carbon_Pricing_Incidence/3_Analyses/1_LAC_2021/4_Transformed Data/Carbon_Pricing_Incidence_%s.csv", Country.Name), show_col_types = FALSE)
    
    household_information_0    <- read_csv(sprintf("../1_Carbon_Pricing_Incidence/3_Analyses/1_LAC_2021/4_Transformed Data/household_information_%s_new.csv", Country.Name), show_col_types = FALSE)
    
    burden_decomposition_0     <- read_csv(sprintf("../1_Carbon_Pricing_Incidence/3_Analyses/1_LAC_2021/4_Transformed Data/Sectoral_Burden_%s.csv", Country.Name), show_col_types = FALSE)
    
    if(!"exp_s_other_energy" %in% colnames(burden_decomposition_0)){
      burden_decomposition_0 <- burden_decomposition_0 %>%
        mutate(exp_s_other_energy = 0)
    }
    
    if(Country.Name == "El_Salvador") Country.Name.2 <- "El Salvador" else Country.Name.2 <- Country.Name
    
    if(Country.Name != "Chile") appliances_0_1 <- read_csv(sprintf("../1_Carbon_Pricing_Incidence/3_Analyses/1_LAC_2021/4_Transformed Data/appliances_0_1_new_%s.csv", Country.Name.2), show_col_types = FALSE)
    
        carbon_pricing_incidence_1 <- left_join(carbon_pricing_incidence_0, household_information_0, by = "hh_id")%>%
      mutate(Country = Country.Name.2)
    
    if("district" %in% colnames(carbon_pricing_incidence_1)){
      carbon_pricing_incidence_1 <- carbon_pricing_incidence_1 %>%
        mutate(district = as.character(district))
    }
    
    if("ethnicity" %in% colnames(carbon_pricing_incidence_1)){
      carbon_pricing_incidence_1 <- carbon_pricing_incidence_1 %>%
        mutate(ethnicity = as.character(ethnicity))
      
      Ethnicity.Code <- read_csv(sprintf("0_Data/1_Household Data/3_%s/2_Codes/Ethnicity.Code.csv", Country.Name.2), show_col_types = FALSE)%>%
        select(ethnicity, everything())%>%
        mutate(ethnicity = as.character(ethnicity))
      
      if(Country.Name == "Guatemala" | Country.Name == "Nicaragua") Ethnicity.Code <- Ethnicity.Code %>%
        select(-Ethnicity_0)
      
      if(Country.Name == "Bolivia") Ethnicity.Code <- Ethnicity.Code %>%
        select(-ethnicity,-Ethnicity_1)%>%
        rename(ethnicity = Ethnicity_0)
      
      colnames(Ethnicity.Code) <- c("ethnicity", "Ethnicity")
      carbon_pricing_incidence_1 <- left_join(carbon_pricing_incidence_1, Ethnicity.Code, by = "ethnicity")
      
    }    
        
    if("province" %in% colnames(carbon_pricing_incidence_1)){
      carbon_pricing_incidence_1 <- carbon_pricing_incidence_1 %>%
        mutate(province = as.character(province))
    }
    
    if("edu_hhh" %in% colnames(carbon_pricing_incidence_1)){
      carbon_pricing_incidence_1 <- carbon_pricing_incidence_1 %>%
        mutate(edu_hhh = as.character(edu_hhh))
      
      Education.Code <- read_csv(sprintf("0_Data/1_Household Data/3_%s/2_Codes/Education.Code.csv", Country.Name.2), show_col_types = FALSE)%>%
        select(edu_hhh, ISCED)%>%
        mutate(edu_hhh = as.character(edu_hhh),
               ISCED = as.character(ISCED))
      
      carbon_pricing_incidence_1 <- left_join(carbon_pricing_incidence_1, Education.Code, by = "edu_hhh")
      
    }
    
    if("ind_hhh" %in% colnames(carbon_pricing_incidence_1)){
      carbon_pricing_incidence_1 <- carbon_pricing_incidence_1 %>%
        mutate(ind_hhh = as.character(ind_hhh))
    }
    
    if("toilet" %in% colnames(carbon_pricing_incidence_1)){
      carbon_pricing_incidence_1 <- carbon_pricing_incidence_1 %>%
        mutate(toilet = as.character(toilet))
      
      Toilet.Code <- read_csv(sprintf("0_Data/1_Household Data/3_%s/2_Codes/Toilet.Code.csv", Country.Name.2), show_col_types = FALSE)%>%
        select(toilet, TLT)%>%
        mutate(toilet = as.character(toilet))
      
      carbon_pricing_incidence_1 <- left_join(carbon_pricing_incidence_1, Toilet.Code, by = "toilet")
    }
    
    if("water" %in% colnames(carbon_pricing_incidence_1)){
      carbon_pricing_incidence_1 <- carbon_pricing_incidence_1 %>%
        mutate(water = as.character(water))
      Water.Code <- read_csv(sprintf("0_Data/1_Household Data/3_%s/2_Codes/Water.Code.csv", Country.Name.2), show_col_types = FALSE)%>%
        select(water, WTR)%>%
        mutate(water = as.character(water))
      carbon_pricing_incidence_1 <- left_join(carbon_pricing_incidence_1, Water.Code, by = "water")
    }
    
    if("cooking_fuel" %in% colnames(carbon_pricing_incidence_1)){
      Cooking.Code <- read_csv(sprintf("0_Data/1_Household Data/3_%s/2_Codes/Cooking.Code.csv", Country.Name.2), show_col_types = FALSE)%>%
        select(cooking_fuel, CF)%>%
        mutate(cooking_fuel = as.character(cooking_fuel))
      
      carbon_pricing_incidence_1 <- carbon_pricing_incidence_1 %>%
        mutate(cooking_fuel = as.character(cooking_fuel))%>%
        left_join(Cooking.Code, by = "cooking_fuel")
        
    }
    
    if("heating_fuel" %in% colnames(carbon_pricing_incidence_1)){
      Heating.Code <- read_csv(sprintf("0_Data/1_Household Data/3_%s/2_Codes/Heating.Code.csv", Country.Name.2), show_col_types = FALSE)%>%
        select(heating_fuel, HF)
      
      carbon_pricing_incidence_1 <- left_join(carbon_pricing_incidence_1, Heating.Code, by = "heating_fuel")
    }
    
    if("lighting_fuel" %in% colnames(carbon_pricing_incidence_1)){
      Lighting.Code <- read_csv(sprintf("0_Data/1_Household Data/3_%s/2_Codes/Lighting.Code.csv", Country.Name.2), show_col_types = FALSE)%>%
        select(lighting_fuel, LF)
      
      carbon_pricing_incidence_1 <- left_join(carbon_pricing_incidence_1, Lighting.Code, by = "lighting_fuel")
    }
    
    if(Country.Name != "Chile") {carbon_pricing_incidence_1 <- left_join(carbon_pricing_incidence_1, appliances_0_1, by = "hh_id")}
        
    carbon_pricing_incidence_1 <- left_join(carbon_pricing_incidence_1, burden_decomposition_0, by = "hh_id")%>%
      mutate(burden_s_cooking_fuels   = exp_s_cooking_fuels/hh_expenditures_USD_2014,
             burden_s_transport_fuels = exp_s_transport_fuels/hh_expenditures_USD_2014,
             burden_s_Goods           = exp_s_Goods/hh_expenditures_USD_2014,
             burden_s_Services        = exp_s_Services/hh_expenditures_USD_2014,
             burden_s_Food            = exp_s_Food/hh_expenditures_USD_2014,
             burden_s_Electricity     = exp_s_Electricity/hh_expenditures_USD_2014,
             burden_s_other_energy    = exp_s_other_energy/hh_expenditures_USD_2014
      )%>%
      select(-starts_with("exp_s_"))
    
    print(Country.Name)
    
    data_joint_0 <- data_joint_0 %>%
      bind_rows(carbon_pricing_incidence_1)
    
  }
  
data_joint_0 <- data_joint_0 %>%
  select(hh_id, hh_weights, hh_size, Country, hh_expenditures_USD_2014, everything())%>%
  mutate(hh_expenditures_USD_2014_pc     = hh_expenditures_USD_2014/hh_size,
         log_hh_expenditures_USD_2014    = log(hh_expenditures_USD_2014),
         log_hh_expenditures_USD_2014_pc = log(hh_expenditures_USD_2014_pc))%>%
  mutate(electricity.access = ifelse(Country == "Chile" & exp_USD_Electricity == 0,0,
                                     ifelse(Country == "Chile" & exp_USD_Electricity > 0,1,electricity.access)))%>%
  select(hh_id, hh_weights, hh_size, Country, hh_expenditures_USD_2014,
         urban_01, province, district, village, municipality,
         adults, children, age_hhh, sex_hhh, ind_hhh, ISCED,
         ethnicity, religion, language, 
         # cooking_fuel, lighting_fuel, heating_fuel, water, toilet, edu_hhh
         CF, LF, HF, WTR, TLT, electricity.access,
         starts_with("CO2"), starts_with("exp_"), starts_with("burden_"),
         starts_with("hh_exp"), starts_with("log_hh"), starts_with("Income_Group"), starts_with("share_"),
         starts_with("exp_USD_"), starts_with("inc_"), ends_with(".01"), everything())%>%
  select(-boiler.01, -iron.01, -pump.01, -solar.heater.01, -radio.01, -cooker.01, -vacuum.01, -bicycle.01,
         -sewing_machine.01, -printer.01, - mobile.01, 
         -lighting_fuel, -heating_fuel, -cooking_fuel, -water, -toilet, edu_hhh)%>%
  mutate(share_other_binning = ifelse(is.na(share_other_binning),0, share_other_binning))%>%
  mutate(car.01          = ifelse(Country != "Chile" & is.na(car.01),0,car.01),
         refrigerator.01 = ifelse(Country != "Chile" & is.na(refrigerator.01),0,refrigerator.01),
         CF = ifelse(is.na(CF), "Unknown", CF),
         LF = ifelse(is.na(LF), "Unknown", LF),
         ISCED = ifelse(is.na(ISCED), 9, ISCED),
         Ethnicity = ifelse(is.na(ethnicity) & Country == "Guatemala","No Indica",Ethnicity),
         Ethnicity = ifelse(is.na(ethnicity) & Country == "Barbados", "Other",Ethnicity),
         Ethnicity = ifelse(is.na(ethnicity) & Country == "Costa Rica", "Otro(a)", Ethnicity),
         Ethnicity = ifelse(is.na(ethnicity) & Country == "Peru", "no sabe/no responde", Ethnicity))

# 1.1   Screen Data ####

# Ethnicity    <- count(data_joint_0, Country, Ethnicity)       # Okay
# Refrigerator <- count(data_joint_0, Country, refrigerator.01) # Okay
# Car          <- count(data_joint_0, Country, car.01)          # Okay
# Urban        <- count(data_joint_0, Country, urban_01)        # Okay
# Cooking      <- count(data_joint_0, Country, CF)              # Okay

# 1.2   Several Summary Statistics ####

# General Summary Statistics

Summary_1.2 <- data.frame()

for(i in Country.Set){
  
  sum_1.2 <- data_joint_0 %>%
    filter(Country == i)%>%
    summarise(number                   = n(),
              weights                  = sum(hh_weights),
              hh_size                  = wtd.mean(hh_size, weights = hh_weights),
              urban_01                 = wtd.mean(urban_01, weights = hh_weights),
              electricity.access       = wtd.mean(electricity.access, weights = hh_weights),
              hh_expenditures_USD_2014 = wtd.mean(hh_expenditures_USD_2014, weights = hh_weights),
              car.01                   = wtd.mean(car.01, weights = hh_weights))%>%
    mutate(urban_01                    = ifelse(i == "Argentina",1,urban_01))%>%
    mutate(Country = i)%>%
    select(Country, number, hh_size, urban_01, electricity.access, hh_expenditures_USD_2014, car.01)%>%
    mutate(hh_expenditures_USD_2014 = round(hh_expenditures_USD_2014,0),
           electricity.access = paste0(round(electricity.access*100,1),"%"),
           car.01   = ifelse(!is.na(car.01), paste0(round(car.01*100,0),"%"),""),
           urban_01 = ifelse(!is.na(urban_01), paste0(round(urban_01*100,0),"%"), ""),
           hh_size = round(hh_size,2))
  
  sum_1.2.1 <- data_joint_0 %>%
    filter(Country == i)%>%
    mutate(Firewood_Biomass_Consumption = ifelse((!is.na(exp_USD_Firewood) & exp_USD_Firewood > 0 ) | (!is.na(exp_USD_Biomass) & exp_USD_Biomass > 0) | 
                                                   CF == "Biomass" | CF == "Charcoal" | CF == "Firewood" | CF == "Firewood Charcoal" | CF == "Firewood Kerosene" |
                                                   CF == "Firewood LPG" | CF == "Firewood LPG Charcoal" | CF == "Other Biomass",hh_weights,0))%>%
    mutate(sum_hh_weights = sum(hh_weights))%>%
    summarise(Firewood_Biomass_Consumption = sum(Firewood_Biomass_Consumption),
              sum_hh_weights               = sum(hh_weights))%>%
    mutate(share_Firewood = paste0(round((Firewood_Biomass_Consumption/sum_hh_weights)*100,0),"%"))%>%
    select(share_Firewood)
  
  sum_1.2 <- bind_cols(sum_1.2, sum_1.2.1)
    
  
  Summary_1.2 <- Summary_1.2 %>%
    bind_rows(sum_1.2)
}

colnames(Summary_1.2) <- c("Country", "Observations", "Average \nHousehold Size", "Urban \nPopulation", "Electricity \nAccess", "Average \nHousehold \nExpenditures [USD]", "Car \nOwnership", "Share of \nFirewood Cons.")

kbl(mutate_all(Summary_1.2,linebreak), format = "latex", linesep = "", booktabs = T,
    caption = "Summary Statistics", format.args = list(big.mark = ",", scientific = FALSE), align = "lrcccrcc")%>%
  kable_styling(position = "center", latex_options = c("HOLD_position", "scale_down"))%>%
  footnote(general = "This table provides summary statistics for households in our sample. All values (except observations) are household-weighted averages. The Argentinian sample comprises urban households only.", threeparttable = T)%>%
  column_spec(column = 1,    width = "3.5 cm", border_right = T)%>%
  column_spec(column =  2:8, width = "2.3 cm")%>%
  save_kable(., "../1_Carbon_Pricing_Incidence/3_Analyses/1_LAC_2021/6_App/Latin-America-Paper/Tables/Table_Summary_A1/Table_Summary_A1.tex")

# Average Expenditure und Energy Expenditure Share Income Groups

Summary_1.3 <- data.frame()

for(i in Country.Set){
  
  sum_1.3.1 <- data_joint_0 %>%
    filter(Country == i)%>%
    summarise(hh_expenditures_USD_2014 = wtd.mean(hh_expenditures_USD_2014, weights = hh_weights),
              share_energy             = wtd.mean(share_energy, weights = hh_weights))
  
  sum_1.3.2 <- data_joint_0 %>%
    filter(Country == i)%>%
    group_by(Income_Group_5)%>%
    summarise(hh_expenditures_USD_2014 = wtd.mean(hh_expenditures_USD_2014, weights = hh_weights),
              share_energy             = wtd.mean(share_energy, weights = hh_weights))%>%
    ungroup()%>%
    pivot_wider(names_from = "Income_Group_5", values_from = c("share_energy", "hh_expenditures_USD_2014"))
  
  sum_1.3.3 <- bind_cols(sum_1.3.1, sum_1.3.2)%>%
    mutate(Country = i)%>%
    select(Country, starts_with("hh_expenditures_USD_2014"), starts_with("share_energy"))
  
  Summary_1.3 <- Summary_1.3 %>%
    bind_rows(sum_1.3.3)
    
}

Summary_1.3 <- Summary_1.3 %>%
  mutate_at(vars(starts_with("hh_expenditures_USD_2014")), list(~ round(.,0)))%>%
  mutate_at(vars(starts_with("share_energy")), list(~ paste0(round(.*100,1), "%")))

colnames(Summary_1.3) <- c("Country", rep(c("All","EQ1","EQ2","EQ3","EQ4","EQ5"),2))

kbl(Summary_1.3, format = "latex", caption = "Average Expenditures and Average Expenditure Shares per Expenditure Quintile", booktabs = T, align = "l|rrrrrr|rrrrrr", vline = "", format.args = list(big.mark = ",", scientific = FALSE), linesep = "")%>%
  kable_styling(position = "center", latex_options = c("HOLD_position", "scale_down"))%>%
  column_spec(1, width = "3.15 cm")%>%
  column_spec(2:7, width = "1.13 cm")%>%
  column_spec(8:13, width = "1.04 cm")%>%
  add_header_above(c(" " = 2, "Expenditure quintile" = 5, " " = 1, "Expenditure quintile" = 5))%>%
  add_header_above(c(" " = 1, "Average household expenditures [USD]" = 6, "Average energy expenditure shares" = 6))%>%
  footnote(general = "This table shows average household expenditures and average energy expenditure shares for households in 16 countries of Latin America and the Caribbean. We estimate household-weighted averages for the whole population and per expenditure quintile.", threeparttable = T)%>%
  save_kable(., "../1_Carbon_Pricing_Incidence/3_Analyses/1_LAC_2021/6_App/Latin-America-Paper/Tables/Table_Summary_A2/Table_Summary_A2.tex")

# Footprint und Burden National

Summary_1.4 <- data.frame()

for(i in Country.Set){
  
  sum_1.4.1 <- data_joint_0 %>%
    filter(Country == i)%>%
    summarise(CO2_t_national      = wtd.mean(CO2_t_national,      weights = hh_weights),
              burden_CO2_national = wtd.mean(burden_CO2_national, weights = hh_weights))
  
  sum_1.4.2 <- data_joint_0 %>%
    filter(Country == i)%>%
    group_by(Income_Group_5)%>%
    summarise(CO2_t_national      = wtd.mean(CO2_t_national,      weights = hh_weights),
              burden_CO2_national = wtd.mean(burden_CO2_national, weights = hh_weights))%>%
    ungroup()%>%
    pivot_wider(names_from = "Income_Group_5", values_from = c("CO2_t_national", "burden_CO2_national"))
  
  sum_1.4.3 <- bind_cols(sum_1.4.1, sum_1.4.2)%>%
    mutate(Country = i)%>%
    select(Country, starts_with("CO2_t_national"), starts_with("burden_CO2_national"))
  
  Summary_1.4 <- Summary_1.4 %>%
    bind_rows(sum_1.4.3)
  
}

Summary_1.4 <- Summary_1.4 %>%
  mutate_at(vars(starts_with("CO2_t_national")), list(~ round(.,1)))%>%
  mutate_at(vars(starts_with("burden_CO2_national")), list(~ paste0(round(.*100,2), "%")))

colnames(Summary_1.4) <- c("Country", rep(c("All","EQ1","EQG2","EQ3","EQ4","EQ5"),2))

kbl(Summary_1.4, format = "latex", caption = "Average Carbon Footprint and Average USD/tCO$_{2}$ Carbon Price Incidence per Expenditure Quintile", booktabs = T, align = "l|rrrrrr|rrrrrr", vline = "", linesep = "")%>%
  kable_styling(position = "center", latex_options = c("HOLD_position", "scale_down"))%>%
  column_spec(1, width = "3.15 cm")%>%
  column_spec(2:7, width = "1.05 cm")%>%
  column_spec(8:13, width = "1.15 cm")%>%
  add_header_above(c(" " = 2, "Expenditure quintile" = 5, " " = 1, "Expenditure quintile" = 5))%>%
  add_header_above(c(" " = 1, "Average carbon footprint [tCO$_{2}$]" = 6, "Average incidence from USD 40/tCO$_{2}$ carbon price" = 6), escape = FALSE)%>%
  footnote(general = "This table shows average carbon footprints in tCO$_{2}$ and average levels of carbon price incidence for households in 16 countries of Latin America and the Caribbean. We estimate household-weighted averages for the whole population and per expenditure quintile.", threeparttable = T, escape = FALSE)%>%
  save_kable(., "../1_Carbon_Pricing_Incidence/3_Analyses/1_LAC_2021/6_App/Latin-America-Paper/Tables/Table_Summary_A3/Table_Summary_A3.tex")

# Electricity-Table

LAC_Electricity <- read_excel("../0_Data/9_Supplementary Data/LAC_Electricity.xlsx")

LAC_Electricity_2 <- LAC_Electricity %>%
  mutate_at(vars("Coal":"Other"),list(~ paste0(round(.*100,1), "\\%")))%>%
  rename("Cons. [TWh]" = "total Electricity Consumption in TWh (2020)", "Cons. pc. [MWh]" = "Electricity Consumption MWh / per capita (2020)")%>%
  rename_at(vars("Coal":"Cons. pc. [MWh]"), list(~ str_replace(.,"^", "\\\\rotatebox{90}{")))%>%
  rename_at(vars(2:13), list(~str_replace(., "$","}")))

kbl(mutate_all(LAC_Electricity_2, linebreak), format = "latex", caption = "Electricity Generation in 16 Countries of Latin America and the Caribbean", 
    booktabs = T, align = "l|rrrrrrrrrr|r|r", vline = "", format.args = list(big.mark = ",", scientific = FALSE), linesep = "", escape = FALSE)%>%
  kable_styling(position = "center", latex_options = c("HOLD_position", "scale_down"))%>%
  column_spec(1,     width = "3.15 cm")%>%
  column_spec(2:10,  width = "1.3 cm")%>%
  column_spec(11:12, width = "1.4 cm")%>%
  add_header_above(c(" " = 1, "Share of Electricity Generation by Source in Percent (2020)" = 10, " " = 1, " " = 1))%>%
  footnote(general = "This table provides summary statistics for electricity generation in 16 different countries of Latin America and the Caribbean. It reports the share of electricity generated by each source in each country in 2020 [\\\\%] as well as the total annual electricity consumption [TWh] and per capita [Mwh]. Source: \\\\textcite{IEA.2021} and Our World in Data \\\\autocite{HannahRitchie.2020} for Barbados. Annual electricity consumption for Peru refers to 2019. ", threeparttable = T, escape = FALSE)%>%
  save_kable(., "../1_Carbon_Pricing_Incidence/3_Analyses/1_LAC_2021/6_App/Latin-America-Paper/Tables/Table_A7/Table_A7.tex")

# Supplement for Uruguay, Paraguay, Nicaragua and Guatemala

Summary_1.5 <- data_joint_0 %>%
  filter(Country == "Uruguay" | Country == "Paraguay" | Country == "Nicaragua" | Country == "Guatemala")%>%
  mutate(exp_USD_energy = hh_expenditures_USD_2014*share_energy)%>%
  filter(exp_USD_energy > 0)%>%
  mutate(share_Firewood = exp_USD_Firewood/exp_USD_energy,
         share_Electricity = exp_USD_Electricity/exp_USD_energy)%>%
  group_by(Country)%>%
  summarise(mean_share_Firewood    = mean(share_Firewood),
            mean_share_Electricity = mean(share_Electricity))%>%
  ungroup()

# 2.0   Decomposing Horizontal Factors ####

# 2.1   Decomposing Additional Cost Burden ####

tex.style <- style.tex(model.title = "", fixef.title = "\\midrule",
                       stats.title = "\\midrule", model.format = "",
                       fontsize = "tiny")

dict_latex <- c(Income_Group_5 = "Expenditure Quintile", log_hh_expenditures_USD_2014 = "HH Exp. (log)", car.01 = "Car Ownership",
                hh_size = "HH Size", refrigerator.01 = "Refrigerator Own.", urban_01 = "Urban Area", burden_CO2_national = "Carbon Price Incidence",
                affected_more_than_80q_CO2n ="Log-Odds of Expecting Higher Additional Costs than 80% of Population",
                affected_80_no_transfers = "Log-Odds of Higher Incidence than 80% of Pop. and No Access to Transfers",
                electricity.access = "Electricity Acc.")

reference.list <- data.frame(Country = Country.Set,
                             REF = c(
                               "Reference group for education (\\textit{ISCED}) is ISCED-level 1 and \\textit{Electricity} for cooking fuel (\\textit{CF}).",
                               "Reference group for education (\\textit{ISCED}) is ISCED-level 1, \\textit{Electricity} for cooking fuel (\\textit{CF}) and \\textit{Black} for ethnicity (\\textit{ETH}).",
                               "Reference group for education (\\textit{ISCED}) is ISCED-level 1, \\textit{LPG} for cooking fuel (\\textit{CF}) and \\textit{Non-indigeneous} for ethnicity (\\textit{ETH}).",
                               "Reference group for education (\\textit{ISCED}) is ISCED-level 1, \\textit{Electricity} for cooking fuel (\\textit{CF}) and \\textit{Parda} for ethnicity (\\textit{ETH}).",
                               "Reference group for education (\\textit{ISCED}) is ISCED-level 1 and \\textit{Electricity} for cooking fuel (\\textit{CF}).",
                               "Reference group for education (\\textit{ISCED}) is ISCED-level 1, \\textit{Electricity} for cooking fuel (\\textit{CF}) and \\textit{Mestizo o blanco} for ethnicity (\\textit{ETH}).",
                               "Reference group for education (\\textit{ISCED}) is ISCED-level 1 and \\textit{Electricity} for cooking fuel (\\textit{CF}).",
                               "Reference group for education (\\textit{ISCED}) is ISCED-level 1 and \\textit{Electricity} for cooking fuel (\\textit{CF}).",
                               "Reference group for education (\\textit{ISCED}) is ISCED-level 1, \\textit{Electricity} for cooking fuel (\\textit{CF}) and \\textit{Mestizo} for ethnicity (\\textit{ETH}).",
                               "Reference group for education (\\textit{ISCED}) is ISCED-level 1 and \\textit{Electricity} for cooking fuel (\\textit{CF}).",
                               "Reference group for education (\\textit{ISCED}) is ISCED-level 1, \\textit{LPG} for cooking fuel (\\textit{CF}) and \\textit{Ladino} for ethnicity (\\textit{ETH}).",
                               "Reference group for education (\\textit{ISCED}) is ISCED-level 1, \\textit{LPG} for cooking fuel (\\textit{CF}) and \\textit{Non-Indigeneous} for ethnicity (\\textit{ETH}).",
                               "Reference group for education (\\textit{ISCED}) is ISCED-level 1 and \\textit{Electricity} for cooking fuel (\\textit{CF}).",
                               "Reference group for education (\\textit{ISCED}) is ISCED-level 1 and \\textit{Electricity} for cooking fuel (\\textit{CF}).",
                               "Reference group for education (\\textit{ISCED}) is ISCED-level 1, \\textit{Electricity} for cooking fuel (\\textit{CF}) and \\textit{Mestizo} for ethnicity (\\textit{ETH}).",
                               "Reference group for education (\\textit{ISCED}) is ISCED-level 1, \\textit{Electricity} for cooking fuel (\\textit{CF}) and \\textit{Blanca} for ethnicity (\\textit{ETH})."))

# 2.1.1 Simple OLS ####

list_2.1.1 <- list()
data_frame_2.1.1 <- data.frame()
ref_list <- data.frame()

for(i in Country.Set){
  
  household_information_0    <- read_csv(sprintf("../1_Carbon_Pricing_Incidence/3_Analyses/1_LAC_2021/4_Transformed Data/household_information_%s_new.csv", i), show_col_types = FALSE)
  
  data_2.1.1 <- data_joint_0 %>%
    filter(Country == i)
  
  formula_0 <- "burden_CO2_national ~ log_hh_expenditures_USD_2014 + hh_size"
  
  if("urban_01" %in% colnames(household_information_0) & sum(is.na(data_2.1.1$urban_01))==0)           formula_0 <- paste0(formula_0, " + urban_01")
  if("electricity.access" %in% colnames(household_information_0) & sum(is.na(data_2.1.1$electricity.access))==0) formula_0 <- paste0(formula_0, " + electricity.access")
  if(i != "Chile" & sum(is.na(data_2.1.1$car.01))==0)                                                formula_0 <- paste0(formula_0, " + car.01")
  #if(i != "Chile" & sum(is.na(data_2.1.1$refrigerator.01))==0)                                                formula_0 <- paste0(formula_0, " + refrigerator.01")
  if("cooking_fuel" %in% colnames(household_information_0) & sum(is.na(data_2.1.1$CF))==0){
    if(i != "Guatemala" & i != "Dominican Republic" & i != "Bolivia" & i != "Mexico") formula_0 <- paste0(formula_0, ' + i(CF, ref = "Electricity")')
    if(i == "Guatemala" | i == "Dominican Republic" | i == "Bolivia" | i == "Mexico") formula_0 <- paste0(formula_0, ' + i(CF, ref = "LPG")')
    }
  #if("lighting_fuel" %in% colnames(household_information_0) & sum(is.na(data_2.1.1$LF))==0)      formula_0 <- paste0(formula_0, " + LF")
  #if("heating_fuel" %in% colnames(household_information_0))      formula_0 <- paste0(formula_0, " + HF")
  if("edu_hhh" %in% colnames(household_information_0) & sum(is.na(data_2.1.1$ISCED))==0)            formula_0 <- paste0(formula_0, " + i(ISCED, ref = 1)")
  if("ethnicity" %in% colnames(household_information_0) & sum(is.na(data_2.1.1$Ethnicity))==0){
    ref_0 <- count(data_2.1.1, Ethnicity)$Ethnicity[which.max(count(data_2.1.1, Ethnicity)$n)]
    
    ref_list <- bind_rows(ref_list, data.frame(Country = i, Type = "Ethnicity", ref = ref_0))
    
    formula_0 <- paste0(formula_0, ' + i(Ethnicity, ref = "', ref_0,'")')
  }
  #if("religion" %in% colnames(household_information_0) & sum(is.na(data_2.1.1$religion))==0)           formula_0 <- paste0(formula_0, " + factor(religion)")
  #if("district" %in% colnames(household_information_0) & sum(is.na(data_2.1.1$district))==0)           formula_0 <- paste0(formula_0, " + factor(district)")
  #if("province" %in% colnames(household_information_0) & sum(is.na(data_2.1.1$province))==0)           formula_0 <- paste0(formula_0, " + factor(province)")
  
  formula_1 <- as.formula(formula_0)
  
  model_2.1.1.0 <- feols(formula_1, data = data_2.1.1, weights = data_2.1.1$hh_weights, vcov = "hetero")
  model_2.1.1.1 <- feols(formula_1, data = data_2.1.1, weights = data_2.1.1$hh_weights, fsplit = ~ Income_Group_5, vcov = "hetero")
  
  notes_0 <- sprintf("This table displays regression results from equation (8) in the carbon price incidence of any household in %s. 
                     Coefficients are estimates on regressions on the full sample and separated by expenditure quintile.", i)
  
  tex.style <- style.tex(model.title = "", fixef.title = "\\midrule",
                         stats.title = "\\midrule", model.format = "",
                         fontsize = "small")
  
  REF_0 <- reference.list$REF[reference.list$Country == i]
  
  etable(model_2.1.1.1, dict = dict_latex, tex = TRUE, file = sprintf("../1_Carbon_Pricing_Incidence/3_Analyses/1_LAC_2021/6_App/Latin-America-Paper/Tables/Table_2_OLS/Table_OLS_%s.tex", i),
         digits = 3, replace = TRUE, fitstat = c("n", "r2"), style.tex = tex.style, se.row = TRUE, tpt = TRUE,
         title = sprintf("OLS-Regression Coefficients for %s", i),  label = sprintf("tab:OLS_%s",i), adjustbox = "width = 1\\textwidth, max height = 0.95\\textheight, center", placement = "htbp!",
         notes = c("\\medskip \\textit{Note:}",
                   paste0(sprintf("This table displays regression results from equation (8) on the carbon price incidence of any household in %s. Coefficients are estimates on regressions on the full sample and separated by expenditure quintile. ",i), REF_0)))
  
  tidy_2.1.1.1 <- tidy(model_2.1.1.0)%>%
    mutate(Country = i)
  
  data_frame_2.1.1 <- data_frame_2.1.1 %>%
    bind_rows(tidy_2.1.1.1)
  
  print(i)
}

data_frame_2.1.1.1 <- data_frame_2.1.1 %>%
  filter(term != "(Intercept)")%>%
  mutate(Type_A = "Burden National",
         Type_B = "OLS")

ref_list_1 <- ref_list

# 2.1.2 Logit ####

barrier_0 <- data_joint_0 %>%
  group_by(Country)%>%
  summarise(burden_CO2_national_80q = wtd.quantile(burden_CO2_national, probs = 0.80, weights = hh_weights))%>%
  ungroup()

data_2.1.2 <- data_joint_0 %>%
  left_join(barrier_0)%>%
  mutate(affected_more_than_80q_CO2n = ifelse(burden_CO2_national > burden_CO2_national_80q,1,0))

list_2.1.2 <- list()
data_frame_2.1.2 <- data.frame()

ref_list_2 <- data.frame()

for(i in Country.Set){
  
  household_information_0    <- read_csv(sprintf("../1_Carbon_Pricing_Incidence/3_Analyses/1_LAC_2021/4_Transformed Data/household_information_%s_new.csv", i), show_col_types = FALSE)
  
  data_2.1.2.1 <- data_2.1.2 %>%
    filter(Country == i)%>%
    mutate(CF = ifelse(CF == "Electricity" & !i %in% c("Guatemala", "Dominican Republic", "Bolivia", "Mexico"), "A_Electricity",
                       ifelse(CF == "LPG" & i %in% c("Guatemala", "Dominican Republic", "Bolivia", "Mexico"), "B_LPG", CF)))%>%
    mutate(ISCED              = factor(ISCED))
  
  formula_0 <- "affected_more_than_80q_CO2n ~ log_hh_expenditures_USD_2014 + hh_size"
  
  if("urban_01" %in% colnames(household_information_0) & sum(is.na(data_2.1.2.1$urban_01))==0)           formula_0 <- paste0(formula_0, " + urban_01")
  if("electricity.access" %in% colnames(household_information_0) & sum(is.na(data_2.1.2.1$electricity.access))==0 & !i %in% c("Argentina", "Costa Rica")) formula_0 <- paste0(formula_0, " + electricity.access")
  if(i != "Chile" & sum(is.na(data_2.1.2.1$car.01))==0)                                                formula_0 <- paste0(formula_0, " + car.01")
  # if(i != "Chile" & sum(is.na(data_2.1.2.1$refrigerator.01))==0)                                                formula_0 <- paste0(formula_0, " + refrigerator.01")
  if("cooking_fuel" %in% colnames(household_information_0) & sum(is.na(data_2.1.2.1$CF))==0){
    if(i != "Guatemala" & i != "Dominican Republic" & i != "Bolivia" & i != "Mexico") formula_0 <- paste0(formula_0, ' + i(CF, ref = "A_Electricity")')
    if(i == "Guatemala" | i == "Dominican Republic" | i == "Bolivia" | i == "Mexico") formula_0 <- paste0(formula_0, ' + i(CF, ref = "B_LPG")')
  }
  #if("lighting_fuel" %in% colnames(household_information_0) & sum(is.na(data_2.1.2.1$LF))==0)      formula_0 <- paste0(formula_0, " + LF")
  #if("heating_fuel" %in% colnames(household_information_0))      formula_0 <- paste0(formula_0, " + HF")
  if("edu_hhh" %in% colnames(household_information_0) & sum(is.na(data_2.1.2.1$ISCED))==0)            formula_0 <- paste0(formula_0, " + i(ISCED, ref = 1)")
  if("ethnicity" %in% colnames(household_information_0) & sum(is.na(data_2.1.2.1$Ethnicity))==0){
    ref_0 <- count(data_2.1.2.1, Ethnicity)$Ethnicity[which.max(count(data_2.1.2.1, Ethnicity)$n)]
    
    ref_list_2 <- bind_rows(ref_list_2, data.frame(Country = i, Type = "Ethnicity", ref = ref_0))
    
    formula_0 <- paste0(formula_0, ' + i(Ethnicity, ref = "', ref_0,'")')
  }
  # if("religion" %in% colnames(household_information_0) & sum(is.na(data_2.1.2.1$religion))==0 & i != "Barbados" & i != "Guatemala")           formula_0 <- paste0(formula_0, " + factor(religion)")
  #if("district" %in% colnames(household_information_0) & sum(is.na(data_2.1.2.1$district))==0)           formula_0 <- paste0(formula_0, " + factor(district)")
  #if("province" %in% colnames(household_information_0) & sum(is.na(data_2.1.2.1$province))==0)           formula_0 <- paste0(formula_0, " + factor(province)")

  formula_1 <- as.formula(formula_0)
  model_2.1.2.0 <- feglm(formula_1, data = data_2.1.2.1, weights = data_2.1.2.1$hh_weights, family = quasibinomial("logit"), se = "hetero")
  model_2.1.2.1 <- feglm(formula_1, data = data_2.1.2.1, weights = data_2.1.2.1$hh_weights, family = quasibinomial("logit"), se = "hetero", fsplit = ~ Income_Group_5)
  
    tex.style <- style.tex(model.title = "", fixef.title = "\\midrule",
                         stats.title = "\\midrule", model.format = "",
                         fontsize = "small")
  
  REF_0 <- reference.list$REF[reference.list$Country == i]
    
    
  etable(model_2.1.2.1, dict = dict_latex, tex = TRUE, file = sprintf("../1_Carbon_Pricing_Incidence/3_Analyses/1_LAC_2021/6_App/Latin-America-Paper/Tables/Table_4_Logit_Burden/Table_Logit_Burden_%s.tex", i),
         digits = 3, replace = TRUE, fitstat = c("n", "cor2"), style.tex = tex.style, se.row = TRUE, tpt = TRUE,
         title = sprintf("Logit-Model Coefficients Hardship Cases in %s", i),  label = sprintf("tab:Logit_1_%s",i), adjustbox = "width = 1\\textwidth, max height = 0.95\\textheight, center", placement = "htbp!",
         notes = c("\\medskip \\textit{Note:}",
                   paste0("This table displays regression results from equation (13) on the log-odds transformed probability of higher additional costs than 80\\% of the population ", sprintf("in %s",i), " as the dependent variable. We show model coefficients separately for the full sample and separated by expenditure quintile. ", REF_0)))
  
  print(i)
}

data_frame_2.1.2.1 <- data_frame_2.1.2 %>%
  filter(term != "(Intercept)")%>%
  mutate(Type_A = "Affected 80",
         Type_B = "Logit")

ref_list_2 <- ref_list_2

# 2.1.3 Fields Decomposition ####

data_2.1.3.0 <- data.frame()
decomposition_Fields <- list()

# options(warn = 1) # Set iotion warn = 2, to detect warning --> Income_Group_4 in Guatemala / Ethnicity is collinear

for(i in Country.Set){
  
  data_2.1.3.1 <- data_joint_0 %>%
    filter(Country == i)%>%
    mutate(Income_Group_5 = as.character(Income_Group_5))
  
  data_2.1.3.2 <- data_joint_0 %>%
    filter(Country == i)%>%
    mutate(Income_Group_5 = "Full Sample")
  
  data_2.1.3.3 <- bind_rows(data_2.1.3.1, data_2.1.3.2)
  
  df_2.1.3 <- data.frame()
  
  for(j in c(1,2,3,4,5, "Full Sample")){
    data_2.1.3.4 <- data_2.1.3.3 %>%
      filter(Income_Group_5 == j)
    
    variance_incidence <- wtd.var(data_2.1.3.4$burden_CO2_national, weights = data_2.1.3.4$hh_weights)
    
    household_information_0    <- read_csv(sprintf("../1_Carbon_Pricing_Incidence/3_Analyses/1_LAC_2021/4_Transformed Data/household_information_%s_new.csv", i), show_col_types = FALSE)
    
    formula_0 <- "burden_CO2_national ~ log_hh_expenditures_USD_2014 + hh_size"
    
    if("urban_01" %in% colnames(household_information_0) & sum(is.na(data_2.1.3.4$urban_01))==0)           formula_0 <- paste0(formula_0, " + urban_01")
    if("electricity.access" %in% colnames(household_information_0) & sum(is.na(data_2.1.3.4$electricity.access))==0) formula_0 <- paste0(formula_0, " + electricity.access")
    if(i != "Chile" & sum(is.na(data_2.1.3.4$car.01))==0)                                                formula_0 <- paste0(formula_0, " + car.01")
    # if(i != "Chile" & sum(is.na(data_2.1.3.4$refrigerator.01))==0)                                                formula_0 <- paste0(formula_0, " + refrigerator.01")
    if("cooking_fuel" %in% colnames(household_information_0) & sum(is.na(data_2.1.3.4$CF))==0)       formula_0 <- paste0(formula_0, " + CF")
    #if("lighting_fuel" %in% colnames(household_information_0) & sum(is.na(data_2.1.3.4$LF))==0 & i != "Costa Rica" & i != "Uruguay")      formula_0 <- paste0(formula_0, " + LF")
    #if("heating_fuel" %in% colnames(household_information_0))      formula_0 <- paste0(formula_0, " + HF")
    if("edu_hhh" %in% colnames(household_information_0) & sum(is.na(data_2.1.3.4$ISCED))==0)            formula_0 <- paste0(formula_0, " + factor(ISCED)")
    if("ethnicity" %in% colnames(household_information_0) & sum(is.na(data_2.1.3.4$Ethnicity))==0)          formula_0 <- paste0(formula_0, " + factor(Ethnicity)")
    #if("religion" %in% colnames(household_information_0) & sum(is.na(data_2.1.3.4$religion))==0)           formula_0 <- paste0(formula_0, " + factor(religion)")
    #if("district" %in% colnames(household_information_0) & sum(is.na(data_2.1.3.4$district))==0)           formula_0 <- paste0(formula_0, " + factor(district)")
    #if("province" %in% colnames(household_information_0) & sum(is.na(data_2.1.3.4$province))==0)           formula_0 <- paste0(formula_0, " + factor(province)")
    
    formula_1 <- as.formula(formula_0)
    
    model_2.1.3.4 <- lm(formula_1, data = data_2.1.3.4, weights = hh_weights)
    
    prediction_2.1.3.4 <- as.data.frame(predict.lm(model_2.1.3.4, data_2.1.3.4, type = "terms"))%>%
      mutate(residuals = resid(model_2.1.3.4))
    
    correlations <- sapply(prediction_2.1.3.4, function(.) corr(d = cbind(., data_2.1.3.4$burden_CO2_national), w = data_2.1.3.4$hh_weights))
    variance     <- sapply(prediction_2.1.3.4, function(x) wtd.var(x,                                             weights = data_2.1.3.4$hh_weights))
    
    joined_0 <- cbind(correlations, variance)
    joined_1 <- cbind(joined_0, rownames(joined_0))%>%
      as_tibble()%>%
      rename(factor = V3)%>%
      select(factor, everything())%>%
      mutate(var_inc      = variance_incidence,
             correlations = as.numeric(correlations),
             variance     = as.numeric(variance))%>%
      mutate(covariance   = correlations*sqrt(variance*var_inc),
             s_j       = covariance/var_inc,
             Income_Group_5 = j,
             Country        = i,
             R_squared = summary(model_2.1.3.4)$r.squared,
             p_j = ifelse(factor != "residuals", s_j/R_squared,NA))%>%
      select(factor, Income_Group_5, p_j)%>%
      rename(s_k = p_j)%>%
      mutate(factor = ifelse(factor == "hh_size", "HH Size",
                             ifelse(factor == "car.01", "Car Ownership",
                                    ifelse(factor == "refrigerator.01", "Refrigerator Own.",
                                           ifelse(factor == "urban_01", "Urban Area",
                                                  ifelse(factor == "log_hh_expenditures_USD_2014", "HH Exp. (log)", 
                                                         ifelse(factor == "CF", "Cooking Fuel",
                                                                ifelse(factor == "factor(ISCED)", "Education",
                                                                       ifelse(factor == "factor(Ethnicity)", "Ethnicity",
                                                                              ifelse(factor == "residuals", factor, 
                                                                                     ifelse(factor == "electricity.access", "Electricity Acc.", factor)))))))))))

    df_2.1.3 <- df_2.1.3 %>%
      bind_rows(joined_1)
    
  }
  
  df_2.1.3 <- df_2.1.3 %>%
    pivot_wider(names_from = "Income_Group_5", values_from = "s_k")%>%
    mutate_at(vars(-factor), list(~ round(.,3)))%>%
    filter(factor != "residuals")%>%
    select(factor, 'Full Sample', everything())%>%
    rename("Sample:" = factor)
  
  kbl(df_2.1.3, format = "latex", caption = sprintf("Contribution of each variable to explaining variance in carbon pricing incidence in %s",i), booktabs = T)%>%
    kable_styling(position = "center", latex_options = c("HOLD_position", "scale_down"))%>%
    add_header_above(c("","","Expenditure Quintile" = 5))%>%
    footnote(general = sprintf("This table shows the contribution of each independent variable to explain variance in carbon pricing incidence across the full sample and each expenditure quintile in %s. These are estimates from equation (11) based on OLS-regression according to equation (8).", i), threeparttable = T)%>%
    save_kable(., sprintf("../1_Carbon_Pricing_Incidence/3_Analyses/1_LAC_2021/6_App/Latin-America-Paper/Tables/Table_3_Decomposition/Table_3_Decomp_%s.tex",i))
    
  data_2.1.3.0 <- data_2.1.3.0 %>%
    bind_rows(mutate(df_2.1.3, Country = i))
  print(i)
  decomposition_Fields[[i]] <- df_2.1.3
  
}

data_frame_2.1.3.1 <- data_2.1.3.0 %>%
  mutate(Type_A = "Burden National",
         Type_B = "Fields")

data_frame_2.1.3.2 <- data_frame_2.1.3.1 %>%
  select("Sample:", "Full Sample", Country)%>%
  rename(p_j = "Full Sample", variable = "Sample:")%>%
  arrange(Country, -p_j)%>%
  group_by(Country)%>%
  mutate(cumsum_p_j = cumsum(p_j))%>%
  ungroup()%>%
  select(Country, variable, p_j, cumsum_p_j)%>%
  mutate(more_than_10 = ifelse(p_j > 0.1,"Yes","No"),
         cumulative_95 = ifelse(cumsum_p_j < 0.95, "Yes", "No"))

# 2.1.4 Supplementary Analysis for Peru ####
 
household_information_0    <- read_csv(sprintf("../1_Carbon_Pricing_Incidence/3_Analyses/1_LAC_2021/4_Transformed Data/household_information_%s_new.csv", "Peru"), show_col_types = FALSE)
CF_Peru                    <- read_csv("../1_Carbon_Pricing_Incidence/3_Analyses/1_LAC_2021/4_Transformed Data/CF_Peru.csv", show_col_types = FALSE)

data_2.1.4 <- data_joint_0 %>%
  filter(Country == "Peru")%>%
  left_join(CF_Peru)

formula_0 <- "burden_CO2_national ~ log_hh_expenditures_USD_2014 + hh_size + urban_01 + electricity.access + car.01 + i(ISCED, ref = 1) + i(Ethnicity, ref = 'Mestizo') + CF_Electricity + CF_LPG + CF_Coal + CF_Natural_Gas + CF_Firewood + CF_Animal_Waste + CF_Other + CF_No_Cooking"

formula_1 <- as.formula(formula_0)

model_2.1.4.0 <- feols(formula_1, data = data_2.1.4, weights = data_2.1.4$hh_weights, vcov = "hetero")
model_2.1.4.1 <- feols(formula_1, data = data_2.1.4, weights = data_2.1.4$hh_weights, fsplit = ~ Income_Group_5, vcov = "hetero")

notes_0 <- sprintf("This table displays regression results from equation (8) in the carbon price incidence of any household in %s. 
                     Coefficients are estimates on regressions on the full sample and separated by expenditure quintile. Households may report the use of multiple fuels for cooking.", "Peru")

tex.style <- style.tex(model.title = "", fixef.title = "\\midrule",
                       stats.title = "\\midrule", model.format = "",
                       fontsize = "small")

REF_0 <- reference.list$REF[reference.list$Country == "Peru"]

etable(model_2.1.4.1, dict = dict_latex, tex = TRUE, file = sprintf("../1_Carbon_Pricing_Incidence/3_Analyses/1_LAC_2021/6_App/Latin-America-Paper/Tables/Table_2_OLS/Table_OLS_%s_CF_Add.tex", "Peru"),
       digits = 3, replace = TRUE, fitstat = c("n", "r2"), style.tex = tex.style, se.row = TRUE, tpt = TRUE,
       title = sprintf("OLS-Regression Coefficients for %s","Peru"),  label = sprintf("tab:OLS_%s_Add","Peru"), adjustbox = "width = 1\\textwidth, max height = 0.95\\textheight, center", placement = "htbp!",
       notes = c("\\medskip \\textit{Note:}",
                 paste0(sprintf("This table displays regression results from equation (8) on the carbon price incidence of any household in %s. Coefficients are estimates on regressions on the full sample and separated by expenditure quintile. Households may report the use of multiple fuels for cooking.","Peru"), REF_0)))

# Logit

barrier_0 <- data_joint_0 %>%
  group_by(Country)%>%
  summarise(burden_CO2_national_80q = wtd.quantile(burden_CO2_national, probs = 0.80, weights = hh_weights))%>%
  ungroup()

data_2.1.2 <- data_joint_0 %>%
  left_join(barrier_0)%>%
  mutate(affected_more_than_80q_CO2n = ifelse(burden_CO2_national > burden_CO2_national_80q,1,0))

ref_list_2 <- data.frame()

data_2.1.2.1 <- data_2.1.2 %>%
  filter(Country == "Peru")%>%
  left_join(CF_Peru)
  
formula_0 <- "affected_more_than_80q_CO2n ~ log_hh_expenditures_USD_2014 + hh_size + urban_01 + electricity.access + car.01 + i(ISCED, ref = 1) + i(Ethnicity, ref = 'Mestizo') + CF_Electricity + CF_LPG + CF_Coal + CF_Natural_Gas + CF_Firewood + CF_Animal_Waste + CF_Other + CF_No_Cooking"
  
formula_1 <- as.formula(formula_0)
model_2.1.5.0 <- feglm(formula_1, data = data_2.1.2.1, weights = data_2.1.2.1$hh_weights, family = quasibinomial("logit"), se = "hetero")
model_2.1.5.1 <- feglm(formula_1, data = data_2.1.2.1, weights = data_2.1.2.1$hh_weights, family = quasibinomial("logit"), se = "hetero", fsplit = ~ Income_Group_5)
  
tex.style <- style.tex(model.title = "", fixef.title = "\\midrule",
                       stats.title = "\\midrule", model.format = "",
                       fontsize = "small")
  
REF_0 <- reference.list$REF[reference.list$Country == "Peru"]
  
  
etable(model_2.1.5.1, dict = dict_latex, tex = TRUE, file = sprintf("../1_Carbon_Pricing_Incidence/3_Analyses/1_LAC_2021/6_App/Latin-America-Paper/Tables/Table_4_Logit_Burden/Table_Logit_Burden_%s_CF_Add.tex", "Peru"),
       digits = 3, replace = TRUE, fitstat = c("n", "cor2"), style.tex = tex.style, se.row = TRUE, tpt = TRUE,
       title = sprintf("Logit-Model Coefficients Hardship Cases in %s", "Peru"),  label = sprintf("tab:Logit_1_%s_Add","Peru"), adjustbox = "width = 1\\textwidth, max height = 0.95\\textheight, center", placement = "htbp!",
       notes = c("\\medskip \\textit{Note:}",
                 paste0("This table displays regression results from equation (13) on the log-odds transformed probability of higher additional costs than 80\\% of the population ", sprintf("in %s","Peru"), " as the dependent variable. We show model coefficients separately for the full sample and separated by expenditure quintile. Households may report the use of multiple fuels for cooking.", REF_0)))
  
# 2.2   Decomposing Most affected households + No Access to Transfers ####

# 2.2.2 Logit ####

barrier_0 <- data_joint_0 %>%
  group_by(Country)%>%
  summarise(burden_CO2_national_80q = wtd.quantile(burden_CO2_national, probs = 0.80, weights = hh_weights))%>%
  ungroup()

data_2.2.2 <- data_joint_0 %>%
  left_join(barrier_0)%>%
  mutate(affected_more_than_80q_CO2n = ifelse(burden_CO2_national > burden_CO2_national_80q,1,0),
         access_to_transfers = ifelse((!is.na(inc_gov_cash)|!is.na(inc_gov_monetary))&(inc_gov_cash > 0 | inc_gov_monetary > 0),1,0),
         affected_80_no_transfers = ifelse(affected_more_than_80q_CO2n == 1 & access_to_transfers == 0,1,0))

list_2.2.2 <- list()
data_frame_2.2.2 <- data.frame()
ref_list_3 <- data.frame()

for(i in Country.Set){

  household_information_0    <- read_csv(sprintf("../1_Carbon_Pricing_Incidence/3_Analyses/1_LAC_2021/4_Transformed Data/household_information_%s_new.csv", i), show_col_types = FALSE)
  
  data_2.2.2.1 <- data_2.2.2 %>%
    filter(Country == i)
  
  formula_0 <- "affected_80_no_transfers ~ log_hh_expenditures_USD_2014 + hh_size"
  
  if("urban_01" %in% colnames(household_information_0) & sum(is.na(data_2.2.2.1$urban_01))==0)           formula_0 <- paste0(formula_0, " + urban_01")
  if("electricity.access" %in% colnames(household_information_0) & sum(is.na(data_2.2.2.1$electricity.access))==0) formula_0 <- paste0(formula_0, " + electricity.access")
  if(i != "Chile" & sum(is.na(data_2.2.2.1$car.01))==0)                                                formula_0 <- paste0(formula_0, " + car.01")
  #if(i != "Chile" & sum(is.na(data_2.2.2.1$refrigerator.01))==0)                                                formula_0 <- paste0(formula_0, " + refrigerator.01")
  if("cooking_fuel" %in% colnames(household_information_0) & sum(is.na(data_2.2.2.1$CF))==0){
    if(i != "Guatemala" & i != "Dominican Republic" & i != "Bolivia" & i != "Mexico") formula_0 <- paste0(formula_0, ' + i(CF, ref = "Electricity")')
    if(i == "Guatemala" | i == "Dominican Republic" | i == "Bolivia" | i == "Mexico") formula_0 <- paste0(formula_0, ' + i(CF, ref = "LPG")')
  }
  #if("lighting_fuel" %in% colnames(household_information_0) & sum(is.na(data_2.2.2.1$LF))==0)      formula_0 <- paste0(formula_0, " + LF")
  #if("heating_fuel" %in% colnames(household_information_0))      formula_0 <- paste0(formula_0, " + HF")
  if("edu_hhh" %in% colnames(household_information_0) & sum(is.na(data_2.2.2.1$ISCED))==0)            formula_0 <- paste0(formula_0, " + i(ISCED, ref = 1)")
  if("ethnicity" %in% colnames(household_information_0) & sum(is.na(data_2.2.2.1$Ethnicity))==0){
    ref_0 <- count(data_2.2.2.1, Ethnicity)$Ethnicity[which.max(count(data_2.2.2.1, Ethnicity)$n)]
    
    ref_list_3 <- bind_rows(ref_list_3, data.frame(Country = i, Type = "Ethnicity", ref = ref_0))
    
    formula_0 <- paste0(formula_0, ' + i(Ethnicity, ref = "', ref_0,'")')
  }
  #if("religion" %in% colnames(household_information_0) & sum(is.na(data_2.2.2.1$religion))==0)           formula_0 <- paste0(formula_0, " + factor(religion)")
  #if("district" %in% colnames(household_information_0) & sum(is.na(data_2.2.2.1$district))==0)           formula_0 <- paste0(formula_0, " + factor(district)")
  #if("province" %in% colnames(household_information_0) & sum(is.na(data_2.2.2.1$province))==0)           formula_0 <- paste0(formula_0, " + factor(province)")
  
  formula_1 <- as.formula(formula_0)
  model_2.2.2.0 <- feglm(formula_1, data = data_2.2.2.1, weights = data_2.2.2.1$hh_weights, family = quasibinomial("logit"), se = "hetero")
  model_2.2.2.1 <- feglm(formula_1, data = data_2.2.2.1, weights = data_2.2.2.1$hh_weights, family = quasibinomial("logit"), se = "hetero", fsplit = ~ Income_Group_5)
  
  tex.style <- style.tex(model.title = "", fixef.title = "\\midrule",
                         stats.title = "\\midrule", model.format = "",
                         fontsize = "small")
  
  REF_0 <- reference.list$REF[reference.list$Country == i]
  
  etable(model_2.2.2.1, dict = dict_latex, tex = TRUE, file = sprintf("../1_Carbon_Pricing_Incidence/3_Analyses/1_LAC_2021/6_App/Latin-America-Paper/Tables/Table_5/Table_Logit_Transfers_Burden_%s_1.tex", i),
         digits = 3, replace = TRUE, fitstat = c("n", "cor2"), style.tex = tex.style, se.row = TRUE, tpt = TRUE,
         title = sprintf("Logit-Model Coefficients Hardship Cases and no Access to Transfers in %s", i),  label = sprintf("tab:Logit_2_%s",i), adjustbox = "width = 1\\textwidth, max height = 0.95\\textheight, center", placement = "htbp!",
         notes = c("\\medskip \\textit{Note:}",
                   paste0("This table displays regression results from equation (15) on the log-odds transformed probability of higher additional costs than 80\\% of the population ", sprintf("in %s",i), " and having no access to governmental transfer programs as the dependent variable. We show model coefficients separately for the full sample and separated by expenditure quintile. ", REF_0)))
  
  print(i)
  
}

# write.xlsx(list_2.2.2, "../1_Carbon_Pricing_Incidence/3_Analyses/1_LAC_2021/2_Tables/Table_7_Multifactor_Affected_Logit/Table_7.1_Multifactor_Affected_Logit.xlsx", colNames = FALSE)

data_frame_2.2.2.1 <- data_frame_2.2.2 %>%
  filter(term != "(Intercept)")%>%
  mutate(Type_A = "Affected 80 & No Transfers",
         Type_B = "Logit")

ref_list_3 <- ref_list_3

# 2.2.3 How to characterize the households, which loose and have no access to transfers? ####

data_2.2.4.1 <- data_2.2.2 %>%
  filter(affected_80_no_transfers == 1)%>%
  mutate(LPG = ifelse(CF == "LPG",1,0),
         Firewood = ifelse(CF == "Firewood" | CF == "Firewood Charcoal",1,0),
         Gas = ifelse(CF == "Gas",1,0))%>%
  group_by(Country)%>%
  summarise(total                    = sum(hh_weights),
         car.01                   = sum(hh_weights[car.01 == 1]),
         urban_01                 = sum(hh_weights[urban_01 == 1]),
         hh_expenditures_USD_2014 = wtd.mean(hh_expenditures_USD_2014, weights = hh_weights),
         LPG = sum(hh_weights[LPG == 1]),
         Gas = sum(hh_weights[Gas == 1]),
         Firewood = sum(hh_weights[Firewood == 1]))%>%
  ungroup()%>%
  mutate(car.01   = car.01/total,
         urban_01 = urban_01/total,
         LPG = LPG/total,
         Gas = Gas/total,
         Firewood = Firewood/total)%>%
  select(Country, hh_expenditures_USD_2014, car.01, urban_01, LPG, Gas, Firewood)

data_2.2.4.2 <- data_2.2.2 %>%
    mutate(LPG    = ifelse(CF == "LPG",1,0),
         Firewood = ifelse(CF == "Firewood" | CF == "Firewood Charcoal",1,0),
         Gas      = ifelse(CF == "Gas",1,0))%>%
  group_by(Country)%>%
  summarise(total                    = sum(hh_weights),
            car.01                   = sum(hh_weights[car.01 == 1]),
            urban_01                 = sum(hh_weights[urban_01 == 1]),
            hh_expenditures_USD_2014C = wtd.mean(hh_expenditures_USD_2014, weights = hh_weights),
            LPG = sum(hh_weights[LPG == 1]),
            Gas = sum(hh_weights[Gas == 1]),
            Firewood = sum(hh_weights[Firewood == 1]))%>%
  ungroup()%>%
  mutate(car.01C   = car.01/total,
         urban_01C = urban_01/total,
         LPGC = LPG/total,
         GasC = Gas/total,
         FirewoodC = Firewood/total)%>%
  select(Country, hh_expenditures_USD_2014C, car.01C, urban_01C, LPGC, GasC, FirewoodC)

data_2.2.4.3 <- left_join(data_2.2.4.1, data_2.2.4.2)%>%
  select(Country, starts_with("hh"), starts_with("car"), starts_with("urban"), starts_with("LPG"), starts_with("Gas"), starts_with("Firewood"))%>%
  mutate_at(vars(starts_with("hh")), list(~ round(.,0)))%>%
  mutate_at(vars(car.01:FirewoodC), list(~round(.,3)))%>%
  mutate_at(vars(car.01:FirewoodC), list(~ ifelse(. == 0,NA,.)))%>%
  mutate_at(vars(setdiff(ends_with("C"), starts_with("hh"))), list(~ ifelse(!is.na(.),paste0("(",.*100,"%)"),.)))%>%
  mutate_at(vars(starts_with("hh")), list(~format(., big.mark = ",")))%>%
  mutate(hh_expenditures_USD_2014C = paste0("(",hh_expenditures_USD_2014C, ")"))%>%
  mutate_all(~ ifelse(is.na(.),"",.))

kbl(data_2.2.4.3, format = "latex", caption = "Comparison of households with high carbon price incidence and no access to transfers compared to the total population (in parentheses).",
    booktabs = T, align = "l|rr|cc|cc|cc|cc|cc", vline = "", format.args = list(big.mark = ",", scientific = FALSE), linesep = "", col.names = NULL, label = "tab:A9")%>%
  kable_styling(position = "center", latex_options = c("HOLD_position", "scale_down"))%>%
  column_spec(column = 1,    width = "2.0 cm")%>%
  column_spec(column = 2:3,  width = "1.20 cm")%>%
  column_spec(column = 4:13, width = "1.0 cm")%>%
  add_header_above(c("Country" = 1, "Av. HH Exp. [USD]" = 2, "Car owners" = 2,
                     "Urban" = 2, "LPG users" = 2, "Gas users" = 2, "Firewood users" = 2), escape = FALSE)%>%
  footnote(general = "This table compares summary statistics for households with higher carbon pricing incidence than 80% of the population and whichhave no access to governmental transfer programs to all households for 16 countries of Latin America and the Caribbean.", threeparttable = T)%>%
  save_kable(., "../1_Carbon_Pricing_Incidence/3_Analyses/1_LAC_2021/6_App/Latin-America-Paper/Tables/Table_A9/Table_A9.tex")

# 2.3   Decomposing burden ####

data_2.3.00 <- tibble()

for (i in Country.Set){

data_2.3.0 <- data_joint_0 %>%
  filter(Country == i)

var_0 <- var(data_2.3.0$burden_CO2_national)

data_frame_2.3.0 <- tibble()

for (j in c("burden_s_cooking_fuels", "burden_s_transport_fuels", "burden_s_Electricity", "burden_s_Food", "burden_s_Services",
            "burden_s_other_energy", "burden_s_Goods")){
  
  data_2.3.1 <- data_2.3.0 %>%
    rename(var_0 = j)%>%
    select(burden_CO2_national, var_0)
  
  data_frame_2.3.0 <- bind_rows(data_frame_2.3.0, c("Variable" = j, "Covariance" = as.numeric(cov(data_2.3.1$burden_CO2_national, data_2.3.1$var_0))))
  
  rm(data_2.3.1)
}

data_frame_2.3.0 <- data_frame_2.3.0 %>%
  mutate(Covariance = as.numeric(Covariance))%>%
  mutate(Country = i)%>%
  mutate(variance = var_0)%>%
  mutate(s_j = Covariance/variance)%>%
  mutate(sum_s_j = sum(s_j))

data_2.3.00 <- bind_rows(data_2.3.00, data_frame_2.3.0)
}  

# Export as figure

data_2.3.01 <- data_2.3.00 %>%
  mutate(Variable = ifelse(Variable == "burden_s_cooking_fuels", "Cooking fuels",
                           ifelse(Variable == "burden_s_Electricity", "Electricity",
                                  ifelse(Variable == "burden_s_Food", "Food",
                                         ifelse(Variable == "burden_s_Goods", "Goods",
                                                ifelse(Variable == "burden_s_other_energy", "Other energy",
                                                       ifelse(Variable == "burden_s_Services", "Services",
                                                              ifelse(Variable == "burden_s_transport_fuels", "Transport fuels", "FAIL"))))))))%>%
  arrange(Country, -s_j)

P_2.3.00 <- ggplot(data_2.3.01)+
  geom_col(aes(y = s_j, fill = Variable, x = 1, group = "Variable"), position = "stack", colour = "black", width = 0.75)+
  facet_wrap(. ~ Country)+
  theme_bw()+
  scale_fill_npg()+
  scale_x_discrete()+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = c(0,0.5,1))+
  guides(fill = guide_legend(nrow = 1, title = "Expenditure type"))+
  labs(x = "", y = "Variance in carbon pricing incidence explained by expenditure type")+
  theme(axis.text.y = element_text(size = 7), 
        axis.text.x = element_text(size = 7),
        axis.title  = element_text(size = 7),
        plot.title = element_text(size = 7),
        legend.position = "bottom",
        strip.text = element_text(size = 7),
        strip.text.y = element_text(angle = 180),
        panel.grid.major = element_line(size = 0.3),
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(size = 0.2),
        legend.text = element_text(size = 6),
        legend.title = element_text(size = 6),
        plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"),
        panel.border = element_rect(size = 0.3))

jpeg("../1_Carbon_Pricing_Incidence/3_Analyses/1_LAC_2021/1_Figures/Figures/Figure_3_Appendix.jpg", width = 15.5, height = 15, unit = "cm", res = 400)
print(P_2.3.00)
dev.off()
