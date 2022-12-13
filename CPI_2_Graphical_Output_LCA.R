# 0     General ####

# Author: L. Missbach, missbach@mcc-berlin.net

# 0.1   Packages ####

if(!require("pacman")) install.packages("pacman")

p_load("cowplot", "eulerr", "ggpubr", "ggrepel",
       "ggsci", "haven", "Hmisc", "knitr", "kableExtra", "openxlsx", "rattle", "scales", "tidyverse", "VennDiagram")

options(scipen=999)

# 1     Loading Data ####

data_joint_0 <- data.frame()

for(Country.Name in c("Argentina", "Barbados","Bolivia", "Brazil", "Chile", "Colombia",
                      "Costa Rica", "Dominican Republic", "Ecuador",
                      "El Salvador", "Guatemala", "Mexico", "Nicaragua", "Paraguay", "Peru", "Uruguay")) {
  
  carbon_pricing_incidence_0 <- read_csv(sprintf("../1_Carbon_Pricing_Incidence/3_Analyses/1_LAC_2021/4_Transformed Data/Carbon_Pricing_Incidence_%s.csv", Country.Name))
  
  household_information_0    <- read_csv(sprintf("../1_Carbon_Pricing_Incidence/3_Analyses/1_LAC_2021/4_Transformed Data/household_information_%s_new.csv", Country.Name))
  
  if(Country.Name == "El_Salvador") Country.Name.2 <- "El Salvador" else Country.Name.2 <- Country.Name
  
  if(Country.Name == "Bolivia") {
    ETH <- read_csv("0_Data/1_Household Data/3_Bolivia/2_Codes/Ethnicity.Code.csv")%>%
      rename(ETH = ethnicity, ethnicity = Ethnicity_0)%>%
      select(ETH, ethnicity)
    
    household_information_0 <- household_information_0 %>%
      left_join(ETH)%>%
      select(-ethnicity)%>%
      rename(ethnicity = ETH)
    
  }
  carbon_pricing_incidence_1 <- left_join(carbon_pricing_incidence_0, household_information_0)%>%
    mutate(Country = Country.Name.2)
  
  if("district" %in% colnames(carbon_pricing_incidence_1)){
    carbon_pricing_incidence_1 <- carbon_pricing_incidence_1 %>%
      mutate(district = as.character(district))
  }
  
  if("province" %in% colnames(carbon_pricing_incidence_1)){
    carbon_pricing_incidence_1 <- carbon_pricing_incidence_1 %>%
      mutate(province = as.character(province))
  }
  
  if("edu_hhh" %in% colnames(carbon_pricing_incidence_1)){
    carbon_pricing_incidence_1 <- carbon_pricing_incidence_1 %>%
      mutate(edu_hhh = as.character(edu_hhh))
  }
  
  if("ind_hhh" %in% colnames(carbon_pricing_incidence_1)){
    carbon_pricing_incidence_1 <- carbon_pricing_incidence_1 %>%
      mutate(ind_hhh = as.character(ind_hhh))
  }
  
  if("toilet" %in% colnames(carbon_pricing_incidence_1)){
    carbon_pricing_incidence_1 <- carbon_pricing_incidence_1 %>%
      mutate(toilet = as.character(toilet))
  }
  
  if("water" %in% colnames(carbon_pricing_incidence_1)){
    carbon_pricing_incidence_1 <- carbon_pricing_incidence_1 %>%
      mutate(water = as.character(water))
  }
  
  print(Country.Name)
  
  data_joint_0 <- data_joint_0 %>%
    bind_rows(carbon_pricing_incidence_1)

}

data_joint_0 <- data_joint_0 %>%
  select(hh_id, hh_weights, hh_size, Country, hh_expenditures_USD_2014, everything())

# 2.1   Boxplots ####
# 2.1.1 National Carbon Price ####

carbon_pricing_incidence_2.1.1 <- data_joint_0 %>%
  group_by(Income_Group_5, Country)%>%
  summarise(
    y5  = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.05),
    y25 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.25),
    y50 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.5),
    y75 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.75),
    y95 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.95),
    mean = wtd.mean(   burden_CO2_national, weights = hh_weights))%>%
  ungroup()%>%
  group_by(Country)%>%
  mutate(min_median = min(y50),
         max_median = max(y50))%>%
  ungroup()

# Default Y-Axis
ylim0 <- 0.085

plot_figure_1 <- function(ATT  = element_text(size = 7), ATX = element_text(size = 7), ATY = element_text(size = 7),
                          XLAB = "Expenditure Quintiles",
                          YLAB = "Carbon Price Incidence", 
                          fill0 = "none",
                          accuracy_0 = 1,
                          data_0 = carbon_pricing_incidence_2.1.1,
                          title_0 = ""){
  
  P_1 <- ggplot(data_0, aes(x = as.character(Income_Group_5)))+
    geom_rect(aes(ymin = min_median, ymax = max_median), xmin = 0, xmax = 6, alpha = 0.2, fill = "lightblue", inherit.aes = FALSE)+
    geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95), stat = "identity", position = position_dodge(0.5), outlier.shape = NA, width = 0.5, size = 0.3) +
    theme_bw()+
    facet_wrap(.~Country, nrow = 4)+
    xlab(XLAB)+ ylab(YLAB)+
    geom_point(aes(y = mean), shape = 23, size = 1.1, stroke = 0.4, fill = "white")+
    scale_y_continuous(labels = scales::percent_format(accuracy = accuracy_0), expand = c(0,0))+
    scale_x_discrete(labels = c("1", "2", "3", "4", "5"))+
    coord_flip(ylim = c(0,ylim0))+
    ggtitle(title_0)+
    theme(axis.text.y = ATY, 
          axis.text.x = ATX,
          axis.title  = ATT,
          plot.title = element_text(size = 7),
          legend.position = "bottom",
          strip.text = element_text(size = 7),
          strip.text.y = element_text(angle = 180),
          panel.grid.major = element_line(size = 0.3),
          panel.grid.minor = element_blank(),
          axis.ticks = element_line(size = 0.2),
          legend.text = element_text(size = 7),
          legend.title = element_text(size = 7),
          plot.margin = unit(c(0.1,0.1,0,0), "cm"),
          panel.border = element_rect(size = 0.3))
  
  return(P_1)
}

P_2.1.1 <- plot_figure_1()

jpeg("../../1_Carbon_Pricing_Incidence/3_Analyses/1_LAC_2021/1_Figures/Figures/Figure_1.jpg", width = 15.5, height = 15, unit = "cm", res = 400)
print(P_2.1.1)
dev.off()

# 2.1.2 Fossil Fuel Subsidy Reform ####

carbon_pricing_incidence_2.1.2 <- data_joint_0 %>%
  group_by(Income_Group_5, Country)%>%
  summarise(
    y5  = wtd.quantile(burden_CO2_transport, weights = hh_weights, probs = 0.05),
    y25 = wtd.quantile(burden_CO2_transport, weights = hh_weights, probs = 0.25),
    y50 = wtd.quantile(burden_CO2_transport, weights = hh_weights, probs = 0.5),
    y75 = wtd.quantile(burden_CO2_transport, weights = hh_weights, probs = 0.75),
    y95 = wtd.quantile(burden_CO2_transport, weights = hh_weights, probs = 0.95),
    mean = wtd.mean(   burden_CO2_transport, weights = hh_weights))%>%
  ungroup()

# Default Y-Axis
ylim0 <- 0.055

plot_figure_1.1 <- function(ATT  = element_text(size = 7), ATX = element_text(size = 7), ATY = element_text(size = 7),
                          XLAB = "Expenditure Quintiles",
                          YLAB = "Fossil Fuel Subsidy Reform Incidence", 
                          fill0 = "none",
                          accuracy_0 = 1,
                          data_0 = carbon_pricing_incidence_2.1.2,
                          title_0 = "Fossil Fuel Subsidy Reform*"){
  
  P_1.1 <- ggplot(data_0, aes(x = factor(Income_Group_5)))+
    geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95), stat = "identity", position = position_dodge(0.5), outlier.shape = NA, width = 0.5, size = 0.3) +
    theme_bw()+
    facet_wrap(.~Country, nrow = )+
    xlab(XLAB)+ ylab(YLAB)+
    geom_point(aes(y = mean), shape = 23, size = 1.1, stroke = 0.4, fill = "white")+
    scale_y_continuous(labels = scales::percent_format(accuracy = accuracy_0), expand = c(0,0))+
    scale_x_discrete(labels = c("1", "2", "3", "4", "5"))+
    coord_flip(ylim = c(0,ylim0))+
    #ggtitle(title_0)+
    theme(axis.text.y = ATY, 
          axis.text.x = ATX,
          axis.title  = ATT,
          plot.title = element_text(size = 7),
          legend.position = "bottom",
          strip.text = element_text(size = 7),
          strip.text.y = element_text(angle = 180),
          panel.grid.major = element_line(size = 0.3),
          panel.grid.minor = element_blank(),
          axis.ticks = element_line(size = 0.2),
          legend.text = element_text(size = 7),
          legend.title = element_text(size = 7),
          plot.margin = unit(c(0.1,0.1,0,0), "cm"),
          panel.border = element_rect(size = 0.3))
  
  return(P_1.1)
}

P_2.1.2 <- plot_figure_1.1()

#jpeg("../1_Carbon_Pricing_Incidence/3_Analyses/1_LAC_2021/1_Figures/Figures/Figure_1_Appendix", width = 15.5, height = 15, unit = "cm", res = 400)
#print(P_2.1.2)
#dev.off()

# 2.2   Carbon Footprints over Household Expenditures ####

P_2.2 <- ggplot(data_joint_0)+
  geom_smooth(formula = y ~ x, aes(y = CO2_t_national, x = hh_expenditures_USD_2014), method = "lm", se = FALSE, colour = "black", size = 0.6, fullrange = TRUE)+
  geom_point(aes(y = CO2_t_national, x = hh_expenditures_USD_2014, fill = Country), colour = "black", shape = 21, alpha = 0.2, size = 0.8)+
  facet_wrap(.~Country, nrow = 4)+
  coord_cartesian(xlim = c(0,99000), ylim = c(0,50))+
  guides(fill = "none", colour = "none")+
  theme_bw()+
  ylab(bquote('Carbon Footprint (in ' ~tCO[2]~ ')'))+
  xlab("Total Household Expenditures (USD)")+
  scale_x_continuous(labels = scales::unit_format(unit = "", scale = 1/1000), expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))+
  #ggtitle("Carbon Footprint of Consumption")+
  theme(axis.text.y = element_text(size = 7), 
        axis.text.x = element_text(size = 7),
        axis.title  = element_text(size = 7),
        plot.title  = element_text(size = 7),
        legend.position = "bottom",
        strip.text = element_text(size = 7),
        #strip.text.y = element_text(angle = 180),
        panel.grid.major = element_line(size = 0.3),
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(size = 0.2),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"),
        panel.border = element_rect(size = 0.3))

jpeg("../1_Carbon_Pricing_Incidence/3_Analyses/1_LAC_2021/1_Figures/Figures/Figure_1_Appendix.jpg", width = 15.5, height = 15, unit = "cm", res = 400)
print(P_2.2)
dev.off()

# 3.1   Vertical vs. horizontal effects ####

data_3.1 <- data_joint_0 %>%
  group_by(Country, Income_Group_5)%>%
  summarise(median_burden_CO2_national = wtd.quantile(burden_CO2_national, probs = 0.5, weights = hh_weights),
            q95_burden_CO2_national    = wtd.quantile(burden_CO2_national, probs = 0.95, weights = hh_weights),
            q05_burden_CO2_national    = wtd.quantile(burden_CO2_national, probs = 0.05, weights = hh_weights),
            q20_burden_CO2_national    = wtd.quantile(burden_CO2_national, probs = 0.20, weights = hh_weights),
            q80_burden_CO2_national    = wtd.quantile(burden_CO2_national, probs = 0.80, weights = hh_weights))%>%
  ungroup()%>%
  filter(Income_Group_5 == 1 | Income_Group_5 == 5)%>%
  mutate(dif_q95_q05_burden_CO2_national = q95_burden_CO2_national - q05_burden_CO2_national,
         dif_q80_q20_burden_CO2_national = q80_burden_CO2_national - q20_burden_CO2_national,)%>%
  select(Country, Income_Group_5, dif_q95_q05_burden_CO2_national, dif_q80_q20_burden_CO2_national, median_burden_CO2_national)%>%
  pivot_wider(names_from = Income_Group_5, values_from = c(median_burden_CO2_national, dif_q95_q05_burden_CO2_national, dif_q80_q20_burden_CO2_national))%>%
  mutate(median_1_5    = median_burden_CO2_national_1/median_burden_CO2_national_5,
         dif_95_05_1_5 = dif_q95_q05_burden_CO2_national_1/dif_q95_q05_burden_CO2_national_5,
         dif_80_20_1_5 = dif_q80_q20_burden_CO2_national_1/dif_q80_q20_burden_CO2_national_5)

# Table Output

data_3.1.1 <- data_3.1 %>%
  mutate_at(vars(median_burden_CO2_national_1:dif_q80_q20_burden_CO2_national_5), list(~ label_percent(accuracy = 0.01)(.)))%>%
  mutate_at(vars(median_1_5:dif_80_20_1_5), list(~ round(.,2)))

colnames(data_3.1.1) <- c("Country", "$\\overline{AC}_{r}^{1}$", "MAC5", "H1", "H5", "H1A", "H5A", "MAC 1/5", "H 1/5", "H 1/5 A")

kbl(data_3.1.1, format = "latex", caption = "Comparing Median Additional Costs (AC) and Horizontal Spread between first and fifth Expenditure Quintile", 
    booktabs = F, align = "l|cc|cccc|ccc", vline = "", linesep = "",
    col.names = NULL)%>%
  kable_styling(position = "center", latex_options = c("HOLD_position", "scale_down"))%>%
  add_header_above(c("Country" = 1, 
                     "$\\\\overline{AC}_{r}^{1}$" = 1, 
                     "$\\\\overline{AC}_{r}^{5}$" = 1, 
                     "$\\\\overline{H}_{r}^{1}$" = 1, 
                     "$\\\\overline{H}_{r}^{5}$" = 1,
                     "$\\\\overline{H}_{r}^{1*}$" = 1,
                     "$\\\\overline{H}_{r}^{5*}$" = 1,
                     "$\\\\widehat{AC}_{r}^{1}$" = 1,
                     "$\\\\widehat{H}_{r}^{1}$" = 1,
                     "$\\\\widehat{H}_{r}^{1*}$" = 1), escape = FALSE, align = "c")%>%
  column_spec(1, width = "2.88 cm")%>%
  column_spec(2:10, width = "1.46 cm")%>%
  footnote(general = "This table shows the median additional costs from carbon pricing in the first expenditure quintile ($\\\\overline{AC}_{r}^{1}$) and in the fifth quintile ($\\\\overline{AC}_{r}^{5}$). It displays the difference between the 5$^{th}$ (20$^{th}$) and 95$^{th}$ (80$^{th}$) within quintile percentile incidence for the first ($\\\\overline{H}_{r}^{1}$ and $\\\\overline{H}_{r}^{1*}$) and the fifth quintile ($\\\\overline{H}_{r}^{5}$ and $\\\\overline{H}_{r}^{5*}$). It also compares median additional costs from carbon pricing in the first income quintile to that in the fifth quintile ($\\\\hat{AC}$$_{r}^{1}$). Lastly it displays our comparison index faciltiating the comparison of within quintile variation between the first and fifth quintile ($\\\\hat{H}_{r}^{1}$ and $\\\\hat{H}_{r}^{1*}$ respectively).",
           threeparttable = T, escape = FALSE)%>%
  save_kable(., "../1_Carbon_Pricing_Incidence/3_Analyses/1_LAC_2021/6_App/Latin-America-Paper/Tables/Table_A4/Table_A4.tex")



poly <- data.frame(g = c(1,1,1,2,2,2,2,3,3,3,4,4,4,5,5,5,5,6,6,6), x = c(0.05,0.05,0.95,
                                                                         0.05,0.05,0.95,0.95,
                                                                         1.05,1.05,2.95,
                                                                         2.96,2.96,1.06,
                                                                         2.95,1.05,1.05,2.95,
                                                                         0.06,0.96,0.96), 
                   y = c(0.06,0.96,0.96,
                         1.05,2.95,2.95,1.05,
                         1.06,2.96,2.96,
                         2.95,1.05,1.05,
                         0.95,0.95,0.05,0.05,
                         0.05,0.95,0.05))%>%
  mutate(x_1 = ifelse(g == 1,0.25,
                      ifelse(g == 2,0.5,
                             ifelse(g == 3,1.75,
                                    ifelse(g == 4,2.25,
                                           ifelse(g == 5,2,
                                                  ifelse(g == 6,0.75,0)))))))%>%
  mutate(y_1 = ifelse(g == 1,0.75,
                      ifelse(g == 2,2,
                             ifelse(g == 3,2.25,
                                    ifelse(g == 4,1.75,
                                           ifelse(g == 5,0.5,
                                                  ifelse(g == 6,0.25,0)))))))%>%
  mutate(z_1 = ifelse(g == 6 & x_1 == lag(x_1), NA,x_1),
         z_2 = ifelse(x_1 == lead(x_1), NA, x_1))%>%
  mutate(z_3 = ifelse(g == 6, z_1, z_2))%>%
  mutate(z_1 = ifelse(g == 6 & y_1 == lag(y_1), NA,y_1),
         z_2 = ifelse(y_1 == lead(y_1), NA, y_1))%>%
  mutate(z_4 = ifelse(g == 6, z_1, z_2))%>%
  mutate(label = ifelse(g == 1, "Regressive and homogeneous (Horizontal)",
                        ifelse(g == 2, "Regressive and heterogeneous", 
                               ifelse(g == 3, "Progressive and heterogeneous (Horizontal)",
                                      ifelse(g == 4, "Progressive and heterogeneous (Vertical)",
                                             ifelse(g == 5, "Progressive and homogeneous",
                                                    ifelse(g == 6, "Regressive and heterogeneous (Vertical)", "FAIL")))))))

poly_2 <- data.frame(g = c(1,1,1,1,
                           2,2,2,2,
                           3,3,3,3,
                           4,4,4,4),
                     y = c(0.01,0.99,0.99,0.01,
                           1.01,3.19,3.19,1.01,
                           1.01,3.19,3.19,1.01,
                           0.01,0.99,0.99,0.01),
                     x = c(0.01,0.01,0.99,0.99,
                           0.01,0.01,0.99,0.99,
                           1.01,1.01,3.19,3.19,
                           1.01,1.01,3.19,3.19),
                     label = c(rep("Progressive and more heterogeneous in IQ5",4),
                               rep("Regressive and more heterogeneous in IQ5",4),
                               rep("Regressive and more heterogeneous in IQ1",4),
                               rep("Progressive and more heterogeneous in IQ1",4)))

poly_3 <- data.frame(g = c(1,1,1,
                           2,2,2),
                     y = c(0.03,3.18,3.18,
                           0.02,3.17,0.02),
                     x = c(0.02,0.02,3.17,
                           0.03,3.18,3.18))

poly_4 <- data.frame(text = c("Horizontal Differences > Vertical Differences",
                              "Vertical Differences > Horizontal Differences"),
                     x = c(2,1),
                     y = c(0.5,2.5))

data_3.1.2 <- data_3.1 %>%
  mutate(Country_Code = ifelse(Country == "Argentina", "ARG",
                               ifelse(Country == "Barbados", "BRB",
                                      ifelse(Country == "Bolivia", "BOL",
                                             ifelse(Country == "Brazil", "BRA",
                                                    ifelse(Country == "Chile", "CHL", 
                                                           ifelse(Country == "Colombia", "COL",
                                                                  ifelse(Country == "Costa Rica", "CRI",
                                                                         ifelse(Country == "Dominican Republic", "DOM",
                                                                                ifelse(Country == "El Salvador", "SLV",
                                                                                       ifelse(Country == "Ecuador", "ECU",
                                                                                              ifelse(Country == "Guatemala", "GTM",
                                                                                                     ifelse(Country == "Mexico", "MEX",
                                                                                                            ifelse(Country == "Nicaragua", "NIC",
                                                                                                                   ifelse(Country == "Paraguay", "PRY",
                                                                                                                          ifelse(Country == "Peru", "PER",
                                                                                                                                 ifelse(Country == "Uruguay", "URY", "Fail")))))))))))))))))

P.3.1 <- ggplot()+
  #geom_polygon(data = poly, aes(x = y, y = x, group = g), colour = "black", alpha = 0.5, fill = NA)+
  geom_polygon(data = poly_3, aes(x = x, y = y, group = g), colour = "black", fill = NA)+
  geom_polygon(data = poly_2, aes(x = x, y = y, group = g, fill = label), alpha = 0.5)+
  geom_text(data = poly_4, aes(label = text, x = x, y = y))+
  #geom_text(data = poly, aes(x = z_4, y = z_3, group = g, label = label))+
  theme_bw()+
  geom_point(data = data_3.1.2, aes(y = median_1_5, x = dif_95_05_1_5), shape = 17, colour = "black", size = 2)+
  geom_text_repel(data = data_3.1.2, aes(label = Country_Code, y = median_1_5, x = dif_95_05_1_5),
                  direction = "x", size = 2.5)+
  coord_cartesian(xlim = c(0,3.2), ylim = c(0,3.2))+
  #geom_abline(intercept = 0, slope = 1)+
  scale_fill_npg()+
  scale_shape_manual(values = c(15,15,15,15,17,17,17,17,18,18,18,18,19,19,19,19))+
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))+
  ylab("Vertical Distribution Coefficient")+
  xlab("Horizontal Distribution Coefficient")+
  labs(fill = "")+
  guides(fill = guide_legend(nrow = 2))+
  theme(axis.text.y = element_text(size = 7), 
        axis.text.x = element_text(size = 7),
        axis.title  = element_text(size = 7),
        plot.title  = element_text(size = 7),
        legend.position = "bottom",
        strip.text = element_text(size = 7),
        #strip.text.y = element_text(angle = 180),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(size = 0.2),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.1,0.1,0,0), "cm"),
        panel.border = element_rect(size = 0.3))

jpeg("../1_Carbon_Pricing_Incidence/3_Analyses/1_LAC_2021/1_Figures/Figures/Figure_2.jpg", width = 14, height = 15, unit = "cm", res = 400)
print(P.3.1)
dev.off()

# 3.2   Robustness Check on Indicator for Horizontal Effects ####

data_3.2 <- data.frame()

for (i in seq(0.01, 1,0.01)){
  
  lower_0 <- 0.5 - i/2
  upper_0 <- 0.5 + i/2
  
  data_3.2.1 <- data_joint_0 %>%
    filter(Income_Group_5 == 1 | Income_Group_5 == 5)%>%
    group_by(Country, Income_Group_5)%>%
    summarise(q_lower = wtd.quantile(burden_CO2_national, probs = lower_0, weights = hh_weights),
              q_upper = wtd.quantile(burden_CO2_national, probs = upper_0, weights = hh_weights))%>%
    ungroup()%>%
    mutate(dif = q_upper - q_lower)%>%
    select(Country, Income_Group_5, dif)%>%
    pivot_wider(names_from = Income_Group_5, values_from = dif, names_prefix = "dif_")%>%
    mutate(H1 = dif_1/dif_5)%>%
    mutate(span = i,
           lower = lower_0,
           upper = upper_0)
  
  data_3.2 <- data_3.2 %>%
    bind_rows(data_3.2.1)
}

data_3.2.2 <- data_joint_0 %>%
  filter(Income_Group_5 == 1 | Income_Group_5 == 5)%>%
  group_by(Country, Income_Group_5)%>%
  summarise(median = wtd.quantile(burden_CO2_national, probs = 0.5, weights = hh_weights))%>%
  ungroup()%>%
  pivot_wider(names_from = "Income_Group_5", values_from = median, names_prefix = "median_")%>%
  mutate(AC1 = median_1/median_5)%>%
  select(Country, AC1)

data_3.2.3 <- data_3.2 %>%
  bind_rows(expand_grid(Country = distinct(data_3.2.2, Country)$Country, span = c(-0.1,1.1)))%>%
  left_join(data_3.2.2)%>%
  arrange(Country)%>%
  mutate(interest = ifelse(span == 0.9,"90%",""))

P_3.2 <- ggplot(data = data_3.2.3)+
  facet_wrap(. ~ Country)+
  geom_hline(yintercept = 1)+
  geom_vline(xintercept = 0.9, colour = "#4DBBD5FF")+
  geom_line(aes(x = span, y = AC1), colour = "#00A087FF")+
  geom_line(aes(x = span, y = H1), size = 0.01)+
  geom_point(aes(x = span, y = H1, fill = factor(interest), size = factor(interest)), shape = 21, stroke = 0.2, size = 0.4)+
  theme_bw()+
  ylab(bquote('Measure of Horizontal Inequality ' ~hat(H)[r]^1~ ''))+
  scale_colour_npg()+
  scale_size_manual(values = c(0.3,0.5))+
  xlab("Percentage covered to compute horizontal spread")+
  scale_x_continuous(labels = scales::percent_format(), expand = c(0,0), breaks = c(0,0.5,0.9))+
  coord_cartesian(xlim = c(0,1), ylim = c(0,4))+
  guides(fill = "none", size = "none", colour = "none")+
  theme(axis.text.y = element_text(size = 7), 
        axis.text.x = element_text(size = 7),
        axis.title  = element_text(size = 7),
        plot.title  = element_text(size = 7),
        legend.position = "bottom",
        strip.text = element_text(size = 7),
        #strip.text.y = element_text(angle = 180),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(size = 0.2),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.1,0.1,0,0), "cm"),
        panel.border = element_rect(size = 0.3))

jpeg("../1_Carbon_Pricing_Incidence/3_Analyses/1_LAC_2021/1_Figures/Figures/Figure_2_Appendix.jpg", width = 14, height = 15, unit = "cm", res = 400)
print(P_3.2)
dev.off()

# 4     Correlation Analyses ####
# 4.1   Correlation with Expenditure Shares ####

data_4.1 <- data.frame()

for(i in c("Argentina", "Barbados", "Bolivia", "Brazil" ,
           "Chile", "Colombia", "Costa Rica", "Dominican Republic", "Ecuador",
           "El Salvador", "Guatemala", "Mexico", "Nicaragua","Paraguay","Peru", "Uruguay")){
  data_4.1.1 <- data_joint_0 %>%
    filter(Country == i)
  
  cor_1 <- cor(data_4.1.1$share_energy,   data_4.1.1$burden_CO2_national, method = "pearson")
  cor_2 <- cor(data_4.1.1$share_goods,    data_4.1.1$burden_CO2_national, method = "pearson")
  cor_3 <- cor(data_4.1.1$share_services, data_4.1.1$burden_CO2_national, method = "pearson")
  cor_4 <- cor(data_4.1.1$share_food,     data_4.1.1$burden_CO2_national, method = "pearson")
  
  data_4.1.2 <- data.frame(Country = rep(i,4), Category= c("Energy", "Goods", "Services", "Food"), Correlation = c(cor_1, cor_2, cor_3, cor_4), Help = c(1,1,1,1))
  data_4.1 <- bind_rows(data_4.1, data_4.1.2)
  
}  

data_4.1.3 <- data_4.1 %>%
  mutate(Correlation = round(Correlation,2))%>%
  pivot_wider(names_from = "Category", values_from = "Correlation")%>%
  select(-Help)

kbl(mutate_all(data_4.1.3, linebreak), format = "latex", caption = "Correlation Coefficients for Carbon Pricing Incidence and Expenditure Shares on different Consumption Categories", 
    booktabs = T, align = "l|cccc", format.args = list(big.mark = ",", scientific = FALSE), linesep = "", vline = "")%>%
  kable_styling(position = "center", latex_options = c("HOLD_position", "scale_down"))%>%
  column_spec(1, width = "3.5 cm")%>%
  column_spec(2:5, width = "2 cm")%>%
  footnote(general = "This table displays correlation coefficients for carbon pricing incidence and expenditure shares on different consumption categories.", threeparttable = T)%>%
  save_kable(., "../1_Carbon_Pricing_Incidence/3_Analyses/1_LAC_2021/6_App/Latin-America-Paper/Tables/Table_A5/Table_A5.tex")

P.4.1 <- ggplot(data_4.1, aes(x = Correlation, group = Country))+
  geom_vline(aes(xintercept = 0))+
  facet_wrap(Category ~ ., strip.position = "left", nrow = 4)+
  theme_bw()+
  geom_point(aes(y = factor(Help), shape = Country, fill = Country), colour = "black", size = 2, position = position_jitter(height = 0.4, seed = 2022))+
  coord_cartesian(xlim = c(-1,1))+
  scale_x_continuous(expand = c(0,0), labels = c("-1","0.5","0","0.5",1))+
  scale_shape_manual(values = c(rep(c(21,22,24,25),4)))+
  scale_fill_manual(values = c(rep("#BC3C29FF",4),
                               rep("#0072B5FF",4),
                               rep("#E18727FF",4),
                               rep("#20854EFF",4)))+
  xlab("Correlation Coefficient for Expenditure Share and Relative Additional Costs")+
  ylab("")+
  theme(axis.text.y = element_blank(), 
        axis.text.x = element_text(size = 6),
        axis.ticks.y = element_blank(),
        axis.title  = element_text(size = 6),
        plot.title  = element_text(size = 10),
        legend.position = "bottom",
        strip.text = element_text(size = 7),
        #strip.text.y = element_text(angle = 180),
        panel.grid.major = element_line(size = 0.3),
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(size = 0.2),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.1,0.1,0,0), "cm"),
        panel.border = element_rect(size = 0.3))+
  ggtitle("A)")

jpeg("../1_Carbon_Pricing_Incidence/3_Analyses/1_LAC_2021/1_Figures/Figures/Figure_3A.jpg", width = 15.5, height = 15, unit = "cm", res = 400)
print(P.4.1)
dev.off()

# 4.2   Correlation with Energy Expenditure Shares ####

data_4.2 <- data.frame()

for(i in c("Argentina", "Barbados", "Bolivia", "Brazil" ,
           "Chile", "Colombia", "Costa Rica", "Dominican Republic", "Ecuador",
           "El Salvador", "Guatemala", "Mexico", "Nicaragua","Paraguay","Peru", "Uruguay")){
  data_4.2.1 <- data_joint_0 %>%
    filter(Country == i)%>%
    mutate_at(vars(starts_with("exp_USD_")), list(~ ./hh_expenditures_USD_2014))%>%
    rename_at(vars(starts_with("exp_USD_")), list(~ str_replace(., "exp_USD_", "share_E_")))
  
  cor_1 <- cor(data_4.2.1$share_E_Electricity, data_4.2.1$burden_CO2_national, method = "pearson")
  cor_2 <- cor(data_4.2.1$share_E_Kerosene,    data_4.2.1$burden_CO2_national, method = "pearson")
  cor_3 <- cor(data_4.2.1$share_E_LPG,         data_4.2.1$burden_CO2_national, method = "pearson")
  cor_4 <- cor(data_4.2.1$share_E_Biomass,     data_4.2.1$burden_CO2_national, method = "pearson")
  cor_5 <- cor(data_4.2.1$share_E_Firewood,    data_4.2.1$burden_CO2_national, method = "pearson")
  cor_6 <- cor(data_4.2.1$share_E_Gas,         data_4.2.1$burden_CO2_national, method = "pearson")
  cor_7 <- cor(data_4.2.1$share_E_Petrol,      data_4.2.1$burden_CO2_national, method = "pearson")
  cor_8 <- cor(data_4.2.1$share_E_Diesel,      data_4.2.1$burden_CO2_national, method = "pearson")
  
  data_4.2.2 <- data.frame(Country = rep(i,8), Category = c("Electricity", "Kerosene", "LPG", "Biomass",
                                                            "Firewood", "Gas", "Petrol", "Diesel"),
                           Correlation = c(cor_1, cor_2, cor_3, cor_4,
                                           cor_5, cor_6, cor_7, cor_8),
                           Help = c(rep(1,8)))
  data_4.2 <- bind_rows(data_4.2, data_4.2.2)
  
}  

data_4.2.3 <- data_4.2 %>%
  mutate(Correlation = round(Correlation,2))%>%
  pivot_wider(names_from = "Category", values_from = "Correlation", values_fill = NA)%>%
  select(-Help)%>%
  mutate_all(~ ifelse(is.na(.),"",.))

kbl(mutate_all(data_4.2.3, linebreak), format = "latex", caption = "Correlation Coefficients for Carbon Pricing Incidence and Expenditure Shares on different Energy Consumption Categories", 
    booktabs = T, align = "l|cccccccc", format.args = list(big.mark = ",", scientific = FALSE), linesep = "", vline = "")%>%
  kable_styling(position = "center", latex_options = c("HOLD_position", "scale_down"))%>%
  column_spec(1, width = "3.5 cm")%>%
  column_spec(2:8, width = "1.5 cm")%>%
  footnote(general = "This table displays correlation coefficients for carbon pricing incidence and expenditure shares on different energy items.", threeparttable = T)%>%
  save_kable(., "../1_Carbon_Pricing_Incidence/3_Analyses/1_LAC_2021/6_App/Latin-America-Paper/Tables/Table_A6/Table_A6.tex")

P.4.2 <- ggplot(data_4.2, aes(x = Correlation, group = Country))+
  geom_vline(aes(xintercept = 0))+
  facet_wrap(Category ~ ., strip.position = "left", nrow = 4)+
  theme_bw()+
  geom_point(aes(y = factor(Help), shape = Country, fill = Country), colour = "black", size = 2, position = position_jitter(height = 0.4, seed = 2022))+
  coord_cartesian(xlim = c(-1,1))+
  scale_x_continuous(expand = c(0,0), labels = c("-1","0.5","0","0.5",1))+
  scale_shape_manual(values = c(rep(c(21,22,24,25),4)))+
  scale_fill_manual(values = c(rep("#BC3C29FF",4),
                               rep("#0072B5FF",4),
                               rep("#E18727FF",4),
                               rep("#20854EFF",4)))+
  xlab("Correlation Coefficient for Expenditure Share and Relative Additional Costs")+
  ylab("")+
  theme(axis.text.y = element_blank(), 
        axis.text.x = element_text(size = 6),
        axis.ticks.y = element_blank(),
        axis.title  = element_text(size = 6),
        plot.title  = element_text(size = 10),
        legend.position = "bottom",
        strip.text = element_text(size = 7),
        #strip.text.y = element_text(angle = 180),
        panel.grid.major = element_line(size = 0.3),
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(size = 0.2),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.1,0.1,0,0), "cm"),
        panel.border = element_rect(size = 0.3))+
  ggtitle("B)")

jpeg("../1_Carbon_Pricing_Incidence/3_Analyses/1_LAC_2021/1_Figures/Figures/Figure_3B.jpg", width = 15.5, height = 12, unit = "cm", res = 400)
print(P.4.2)
dev.off()

P_4 <- ggarrange(P.4.1, P.4.2, common.legend = TRUE, legend = "bottom")

jpeg("../1_Carbon_Pricing_Incidence/3_Analyses/1_LAC_2021/1_Figures/Figures/Figure_3.jpg", width = 15.5, height = 12, unit = "cm", res = 400)
print(P_4)
dev.off()

# 5.1   Venn-Diagramm on the Targeted, the Poor, the Affected ####
# 5.1.1 Joint ####

data_5.1.1 <- data_joint_0 %>%
  group_by(Country)%>%
  mutate(barrier_0 = wtd.quantile(burden_CO2_national, probs = 0.8, weights = hh_weights))%>%
  ungroup()%>%
  mutate(poorest_20_percent  = ifelse(Income_Group_5 == 1,1,0),
         access_to_transfers = ifelse((!is.na(inc_gov_cash)|!is.na(inc_gov_monetary))&(inc_gov_cash > 0 | inc_gov_monetary > 0),1,0),
         most_affected       = ifelse(burden_CO2_national > barrier_0,1,0))%>%
  filter(poorest_20_percent == 1 | most_affected == 1 | access_to_transfers == 1)

data_5.1.2 <- data_5.1.1 %>%
  select(Country, access_to_transfers, poorest_20_percent, most_affected, hh_weights) %>%
  mutate(A = ifelse(most_affected == 1 & access_to_transfers == 0 & poorest_20_percent == 0, hh_weights,0),
         B = ifelse(most_affected == 0 & access_to_transfers == 0 & poorest_20_percent == 1, hh_weights,0),
         C = ifelse(most_affected == 0 & access_to_transfers == 1 & poorest_20_percent == 0, hh_weights,0),
         
         D = ifelse(most_affected == 1 & access_to_transfers == 0 & poorest_20_percent == 1, hh_weights,0),
         E = ifelse(most_affected == 1 & access_to_transfers == 1 & poorest_20_percent == 0, hh_weights,0),
         G = ifelse(most_affected == 0 & access_to_transfers == 1 & poorest_20_percent == 1, hh_weights,0),
         
         H = ifelse(most_affected == 1 & access_to_transfers == 1 & poorest_20_percent == 1, hh_weights,0))%>%
  group_by(Country)%>%
  summarise("Most Affected"       = sum(A),
            "The Poorest"         = sum(B),
            "Access to Transfers" = sum(C),
            "Most Affected&The Poorest"         = sum(D),
            "Most Affected&Access to Transfers" = sum(E),
            "The Poorest&Access to Transfers"   = sum(G),
            "Most Affected&The Poorest&Access to Transfers" = sum(H))%>%
  ungroup()

create_venn_diagram <- function(Country.0){
  data_5.1.3 <- data_5.1.2 %>%
    filter(Country == Country.0)
  data_5.1.4 <- c(
    "Most Affected"                                 = data_5.1.3$'Most Affected',
    "The Poorest"                                   = data_5.1.3$'The Poorest',
    "Access to Transfers"                           = data_5.1.3$'Access to Transfers',
    "Most Affected&The Poorest"                     = data_5.1.3$'Most Affected&The Poorest',
    "Most Affected&Access to Transfers"             = data_5.1.3$'Most Affected&Access to Transfers',
    "The Poorest&Access to Transfers"               = data_5.1.3$'The Poorest&Access to Transfers',
    "The Poorest&Access to Transfers&Most Affected" = data_5.1.3$'Most Affected&The Poorest&Access to Transfers'
  )
  
  P.venn <- plot(euler(data_5.1.4, shape = "ellipse"), labels = FALSE,
                 quantities = list(type = "percent", fontsize = 7), fills = list(fill = c("#BC3C29FF", "#FFDC91FF", "#6F99ADFF"), alpha = 0.8),
                 main = list(label = Country.0, fontsize = 7),
                 #legend = list(side = "bottom", nrow = 1, ncol = 3)
                 )

  data_5.1.5 <- data_joint_0 %>%
    filter(Country == Country.0)
  
  pop <- sum(data_5.1.5$hh_weights)
  
  data_5.1.6 <- rownames_to_column(as.data.frame(data_5.1.4))%>%
    rename(Type = rowname, value = data_5.1.4)%>%
    mutate(total = pop)%>%
    mutate(percent = round(value/total,2))%>%
    mutate(label = paste0(percent*100, "%"))
    
P.venn$children$canvas.grob$children$diagram.grob.1$children$tags$children$tag.number.1$children$tag.quantity.1$label <- data_5.1.6$label[data_5.1.6$Type == "Most Affected"]
P.venn$children$canvas.grob$children$diagram.grob.1$children$tags$children$tag.number.2$children$tag.quantity.2$label <- data_5.1.6$label[data_5.1.6$Type == "The Poorest"]
P.venn$children$canvas.grob$children$diagram.grob.1$children$tags$children$tag.number.3$children$tag.quantity.3$label <- data_5.1.6$label[data_5.1.6$Type == "Access to Transfers"]
P.venn$children$canvas.grob$children$diagram.grob.1$children$tags$children$tag.number.4$children$tag.quantity.4$label <- data_5.1.6$label[data_5.1.6$Type == "Most Affected&The Poorest"]
P.venn$children$canvas.grob$children$diagram.grob.1$children$tags$children$tag.number.5$children$tag.quantity.5$label <- data_5.1.6$label[data_5.1.6$Type == "Most Affected&Access to Transfers"]
P.venn$children$canvas.grob$children$diagram.grob.1$children$tags$children$tag.number.6$children$tag.quantity.6$label <- data_5.1.6$label[data_5.1.6$Type == "The Poorest&Access to Transfers"]
P.venn$children$canvas.grob$children$diagram.grob.1$children$tags$children$tag.number.7$children$tag.quantity.7$label <- data_5.1.6$label[data_5.1.6$Type == "The Poorest&Access to Transfers&Most Affected"]

return(P.venn)
  
}

P.List <- list()

for(Country in c("Argentina", "Barbados", "Brazil", "Bolivia", "Chile",
                 "Colombia", "Costa Rica", "Dominican Republic", "Ecuador", "El Salvador",
                 "Guatemala", "Mexico", "Nicaragua", "Paraguay", "Peru", "Uruguay")){
  P.V <- create_venn_diagram(Country)
  P.List[[Country]] <- P.V
}

data.0 <- data.frame(A = c("20% most affected", "Access to transfers", "Poorest 20%"),
                     B = c(1,2,3),
                     C = c(1,2,3))
data.0$A <- factor(data.0$A, levels = c("20% most affected", "Poorest 20%", "Access to transfers"))

Legend <- ggplot(data.0, aes(x = B, y = C, fill = A))+
  geom_point(shape = 21, alpha = 0.8, size = 2)+
  scale_fill_manual(values = c("#BC3C29FF", "#FFDC91FF", "#6F99ADFF"))+
  guides(fill = guide_legend(nrow = 1))+
  theme_bw()+
  labs(fill = "")

Legend.2 <- ggdraw(get_legend(Legend))

P.5 <- ggarrange(P.List$Argentina, P.List$Barbados, P.List$Brazil, P.List$Bolivia, P.List$Chile,
                 P.List$Colombia, P.List$`Costa Rica`, P.List$`Dominican Republic`, P.List$Ecuador, P.List$`El Salvador`,
                 P.List$Guatemala, P.List$Mexico, P.List$Nicaragua, P.List$Paraguay, P.List$Peru, P.List$Uruguay, ncol = 4, nrow = 4)


jpeg("../1_Carbon_Pricing_Incidence/3_Analyses/1_LAC_2021/1_Figures/Figures/Figure_4.jpg", width = 15.5, height = 15.5, unit = "cm", res = 400)
print(P.5)
dev.off()

jpeg("../1_Carbon_Pricing_Incidence/3_Analyses/1_LAC_2021/1_Figures/Figures/Figure_4_Legend.jpg", width = 15.5, height = 1, unit = "cm", res = 400)
print(Legend.2)
dev.off()

# 5.1.2 Tables ####

data_5.1.2 <- data_joint_0 %>%
  group_by(Country)%>%
  mutate(barrier_0 = wtd.quantile(burden_CO2_national, probs = 0.8, weights = hh_weights))%>%
  ungroup()%>%
  mutate(poorest_20_percent  = ifelse(Income_Group_5 == 1,1,0),
         access_to_transfers = ifelse((!is.na(inc_gov_cash)|!is.na(inc_gov_monetary))&(inc_gov_cash > 0 | inc_gov_monetary > 0),1,0),
         most_affected       = ifelse(burden_CO2_national > barrier_0,1,0))

data_5.1.2.1 <- data_5.1.2 %>%
  group_by(Country)%>%
  summarise(total_hh = sum(hh_weights))%>%
  ungroup()

# How many households have access to transfers, overall?

data_5.1.2.2 <- data_5.1.2 %>%
  filter(access_to_transfers == 1)%>%
  group_by(Country)%>%
  summarise(access_to_transfers = sum(hh_weights))%>%
  ungroup()

data_5.1.2.3 <- data_5.1.2.2 %>%
  left_join(data_5.1.2.1)%>%
  mutate(share_access = access_to_transfers/total_hh)

# How many of the most affected 20 percent are poor?
# How many of the most affected 20 percent have access to transfers?

data_5.1.2.4 <- data_5.1.2 %>%
  filter(most_affected == 1)%>%
  mutate(poor_no_access = ifelse(poorest_20_percent == 1 & access_to_transfers == 0,1,0))%>%
  group_by(Country)%>%
  summarise(most_affected       = sum(hh_weights),
            poorest_20_percent  = sum(hh_weights[poorest_20_percent == 1]),
            access_to_transfers = sum(hh_weights[access_to_transfers == 1]),
            poor_no_access      = sum(hh_weights[poor_no_access == 1]))%>%
  ungroup()%>%
  mutate(poorest_20_percent_1  = poorest_20_percent/most_affected,
         access_to_transfers_1 = access_to_transfers/most_affected,
         poor_no_access_1      = poor_no_access/most_affected)

# Final Table for Paper

data_5.1.2.5 <- data_5.1.2.3 %>%
  select(Country, share_access)%>%
  left_join(select(data_5.1.2.4, Country,poorest_20_percent_1, access_to_transfers_1, poor_no_access_1), by = "Country")%>%
  mutate_at(vars(-Country), list(~ paste0(round(., 3)*100, "%")))

colnames(data_5.1.2.5) <- c("Country", 
                            #"\\rotatebox{90}{Households with access to transfer programs}",
                            #"\\rotatebox{90}{...are poorer than 80\\% of the population?}",
                            #"\\rotatebox{90}{...have access to governmental transfer programs?}",
                            #"\\rotatebox{90}{...are poorer than 80\\% of the population and have no access to governmental transfer programs?}"
                            "Households with access to transfer programs",
                            "...are poorer than 80 % of  the population?",
                            "...have access to governmental transfer programs?",
                            "...are poorer than 80 % of the population and have no access to governmental transfer programs?"
)

kbl(data_5.1.2.5, format = "latex", caption = "Summary Statistics on Access to Transfer Programmes", label = "tab:transfer",
    booktabs = T, linesep = "", align = "lcccc")%>%

  kable_styling(position = "center", latex_options = c("HOLD_position", "scale_down"))%>%
  column_spec(1, width = "4 cm", border_right = T)%>%
  column_spec(2, width = "3 cm", border_right = T)%>%
  column_spec(3:5, width = "3 cm")%>%
  add_header_above(c(" " = 2, "Of the most affected 20\\\\% of households, how many..." = 3), escape = FALSE)%>%
  footnote(general = "This table reports shares of total population and shares of the 20% of population with highest carbon pricing incidence adhering to different criteria for 16 countries in Latin America and the Caribbean.", threeparttable = T)%>%
  save_kable(., "../1_Carbon_Pricing_Incidence/3_Analyses/1_LAC_2021/6_App/Latin-America-Paper/Tables/Table_A8/Table_A8.tex")

# 6     Spanish ####

Country.Translation <- data.frame(Country = c("Argentina", "Barbados", "Bolivia", "Brazil", "Chile", "Colombia", "Costa Rica", "Dominican Republic", "Ecuador", "El Salvador", "Guatemala", "Mexico", "Nicaragua", "Paraguay", "Peru", "Uruguay"),
                         Pais    = c("Argentina", "Barbados", "Bolivia", "Brasil", "Chile", "Colombia", "Costa Rica", "República Dominicana", "Ecuador", "El Salvador", "Guatemala", "México", "Nicaragua", "Paraguay", "Perú", "Uruguay"))

# 6.1   Figure 1 ####

carbon_pricing_incidence_6.1 <- carbon_pricing_incidence_2.1.1 %>%
  left_join(Country.Translation, by = "Country")
  
ylim0 <- 0.085

plot_figure_1 <- function(ATT  = element_text(size = 7), ATX = element_text(size = 7), ATY = element_text(size = 7),
                          XLAB = "Quintiles de gasto",
                          YLAB = "Incidencia del precio del carbono", 
                          fill0 = "none",
                          accuracy_0 = 1,
                          data_0 = carbon_pricing_incidence_6.1,
                          title_0 = ""){
  
  P_1 <- ggplot(data_0, aes(x = as.character(Income_Group_5)))+
    geom_rect(aes(ymin = min_median, ymax = max_median), xmin = 0, xmax = 6, alpha = 0.2, fill = "lightblue", inherit.aes = FALSE)+
    geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95), stat = "identity", position = position_dodge(0.5), outlier.shape = NA, width = 0.5, size = 0.3) +
    theme_bw()+
    facet_wrap(.~Pais, nrow = 4)+
    xlab(XLAB)+ ylab(YLAB)+
    geom_point(aes(y = mean), shape = 23, size = 1.1, stroke = 0.4, fill = "white")+
    scale_y_continuous(labels = scales::percent_format(accuracy = accuracy_0), expand = c(0,0))+
    scale_x_discrete(labels = c("1", "2", "3", "4", "5"))+
    coord_flip(ylim = c(0,ylim0))+
    ggtitle(title_0)+
    theme(axis.text.y = ATY, 
          axis.text.x = ATX,
          axis.title  = ATT,
          plot.title = element_text(size = 7),
          legend.position = "bottom",
          strip.text = element_text(size = 7),
          strip.text.y = element_text(angle = 180),
          panel.grid.major = element_line(size = 0.3),
          panel.grid.minor = element_blank(),
          axis.ticks = element_line(size = 0.2),
          legend.text = element_text(size = 7),
          legend.title = element_text(size = 7),
          plot.margin = unit(c(0.1,0.1,0,0), "cm"),
          panel.border = element_rect(size = 0.3))
  
  return(P_1)
}

P_6.1 <- plot_figure_1()

jpeg("../1_Carbon_Pricing_Incidence/3_Analyses/1_LAC_2021/1_Figures/Figures_ESP/Figure_1_ESP.jpg", width = 15.5, height = 15, unit = "cm", res = 400)
print(P_6.1)
dev.off()

# 6.2   Figure 2 ####

poly <- data.frame(g = c(1,1,1,2,2,2,2,3,3,3,4,4,4,5,5,5,5,6,6,6), x = c(0.05,0.05,0.95,
                                                                         0.05,0.05,0.95,0.95,
                                                                         1.05,1.05,2.95,
                                                                         2.96,2.96,1.06,
                                                                         2.95,1.05,1.05,2.95,
                                                                         0.06,0.96,0.96), 
                   y = c(0.06,0.96,0.96,
                         1.05,2.95,2.95,1.05,
                         1.06,2.96,2.96,
                         2.95,1.05,1.05,
                         0.95,0.95,0.05,0.05,
                         0.05,0.95,0.05))%>%
  mutate(x_1 = ifelse(g == 1,0.25,
                      ifelse(g == 2,0.5,
                             ifelse(g == 3,1.75,
                                    ifelse(g == 4,2.25,
                                           ifelse(g == 5,2,
                                                  ifelse(g == 6,0.75,0)))))))%>%
  mutate(y_1 = ifelse(g == 1,0.75,
                      ifelse(g == 2,2,
                             ifelse(g == 3,2.25,
                                    ifelse(g == 4,1.75,
                                           ifelse(g == 5,0.5,
                                                  ifelse(g == 6,0.25,0)))))))%>%
  mutate(z_1 = ifelse(g == 6 & x_1 == lag(x_1), NA,x_1),
         z_2 = ifelse(x_1 == lead(x_1), NA, x_1))%>%
  mutate(z_3 = ifelse(g == 6, z_1, z_2))%>%
  mutate(z_1 = ifelse(g == 6 & y_1 == lag(y_1), NA,y_1),
         z_2 = ifelse(y_1 == lead(y_1), NA, y_1))%>%
  mutate(z_4 = ifelse(g == 6, z_1, z_2))%>%
  mutate(label = ifelse(g == 1, "Regressive and homogeneous (Horizontal)",
                        ifelse(g == 2, "Regressive and heterogeneous", 
                               ifelse(g == 3, "Progressive and heterogeneous (Horizontal)",
                                      ifelse(g == 4, "Progressive and heterogeneous (Vertical)",
                                             ifelse(g == 5, "Progressive and homogeneous",
                                                    ifelse(g == 6, "Regressive and heterogeneous (Vertical)", "FAIL")))))))

poly_2 <- data.frame(g = c(1,1,1,1,
                           2,2,2,2,
                           3,3,3,3,
                           4,4,4,4),
                     y = c(0.01,0.99,0.99,0.01,
                           1.01,3.19,3.19,1.01,
                           1.01,3.19,3.19,1.01,
                           0.01,0.99,0.99,0.01),
                     x = c(0.01,0.01,0.99,0.99,
                           0.01,0.01,0.99,0.99,
                           1.01,1.01,3.19,3.19,
                           1.01,1.01,3.19,3.19),
                     label = c(rep("Progresivos y más heterogéneos en Q5",4),
                               rep("Regresivos y más heterogéneos en Q5",4),
                               rep("Regresivos y más heterogéneos en Q1",4),
                               rep("Progresivos y más heterogéneos en Q1",4)))

poly_3 <- data.frame(g = c(1,1,1,
                           2,2,2),
                     y = c(0.03,3.18,3.18,
                           0.02,3.17,0.02),
                     x = c(0.02,0.02,3.17,
                           0.03,3.18,3.18))

poly_4 <- data.frame(text = c("Diferencias horizontales > Diferencias verticales",
                              "Diferencias verticales > Diferencias horizontales"),
                     x = c(2,1.1),
                     y = c(0.5,2.5))

P.6.2 <- ggplot()+
  #geom_polygon(data = poly, aes(x = y, y = x, group = g), colour = "black", alpha = 0.5, fill = NA)+
  geom_polygon(data = poly_3, aes(x = x, y = y, group = g), colour = "black", fill = NA)+
  geom_polygon(data = poly_2, aes(x = x, y = y, group = g, fill = label), alpha = 0.5)+
  geom_text(data = poly_4, aes(label = text, x = x, y = y))+
  #geom_text(data = poly, aes(x = z_4, y = z_3, group = g, label = label))+
  theme_bw()+
  geom_point(data = data_3.1.2, aes(y = median_1_5, x = dif_95_05_1_5), shape = 17, colour = "black", size = 2)+
  geom_text_repel(data = data_3.1.2, aes(label = Country_Code, y = median_1_5, x = dif_95_05_1_5),
                  direction = "x", size = 2.5)+
  coord_cartesian(xlim = c(0,3.2), ylim = c(0,3.2))+
  #geom_abline(intercept = 0, slope = 1)+
  scale_fill_npg()+
  scale_shape_manual(values = c(15,15,15,15,17,17,17,17,18,18,18,18,19,19,19,19))+
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))+
  ylab("Coeficiente de distribución vertical")+
  xlab("Coeficiente de distribución horizontal")+
  labs(fill = "")+
  guides(fill = guide_legend(nrow = 2))+
  theme(axis.text.y = element_text(size = 7), 
        axis.text.x = element_text(size = 7),
        axis.title  = element_text(size = 7),
        plot.title  = element_text(size = 7),
        legend.position = "bottom",
        strip.text = element_text(size = 7),
        #strip.text.y = element_text(angle = 180),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(size = 0.2),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.1,0.1,0,0), "cm"),
        panel.border = element_rect(size = 0.3))

jpeg("../1_Carbon_Pricing_Incidence/3_Analyses/1_LAC_2021/1_Figures/Figures_ESP/Figure_2_ESP.jpg", width = 14, height = 15, unit = "cm", res = 400)
print(P.6.2)
dev.off()

# 6.3   Figure 3 ####

data_6.3.1 <- data_4.1%>%
  left_join(Country.Translation)%>%
  mutate(Categoria = ifelse(Category == "Energy", "Energía",
                            ifelse(Category == "Food", "Alimentos",
                                   ifelse(Category == "Goods", "Bienes",
                                          ifelse(Category == "Services", "Servicios", "FAIL")))))

P.6.3.1 <- ggplot(data_6.3.1, aes(x = Correlation, group = Pais))+
  geom_vline(aes(xintercept = 0))+
  facet_wrap(Categoria ~ ., strip.position = "left", nrow = 4)+
  theme_bw()+
  geom_point(aes(y = factor(Help), shape = Pais, fill = Pais), colour = "black", size = 2, position = position_jitter(height = 0.4, seed = 2022))+
  coord_cartesian(xlim = c(-1,1))+
  scale_x_continuous(expand = c(0,0), labels = c("-1","0.5","0","0.5",1))+
  scale_shape_manual(values = c(rep(c(21,22,24,25),4)))+
  scale_fill_manual(values = c(rep("#BC3C29FF",4),
                               rep("#0072B5FF",4),
                               rep("#E18727FF",4),
                               rep("#20854EFF",4)))+
  xlab("Coeficiente de correlación para la cuota de gasto y los costos adicionales relativos")+
  ylab("")+
  labs(fill = "", shape = "")+
  theme(axis.text.y = element_blank(), 
        axis.text.x = element_text(size = 6),
        axis.ticks.y = element_blank(),
        axis.title  = element_text(size = 5),
        plot.title  = element_text(size = 10),
        legend.position = "bottom",
        strip.text = element_text(size = 7),
        #strip.text.y = element_text(angle = 180),
        panel.grid.major = element_line(size = 0.3),
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(size = 0.2),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.2,0.2,0.2,0.2), "cm"),
        panel.border = element_rect(size = 0.3))+
  ggtitle("A)")

data_6.3.2 <- data_4.2 %>%
  left_join(Country.Translation)%>%
  mutate(Categoria = ifelse(Category == "Biomass", "Biomasa",
                            ifelse(Category == "Electricity", "Electricidad",
                                   ifelse(Category == "Gas", "Gas",
                                          ifelse(Category == "LPG", "GLP",
                                                 ifelse(Category == "Diesel", "Diésel",
                                                        ifelse(Category == "Firewood", "Leña",
                                                               ifelse(Category == "Petrol", "Petróleo",
                                                                      ifelse(Category == "Kerosene", "Queroseno", "FAIL")))))))))

P.6.3.2 <- ggplot(data_6.3.2, aes(x = Correlation, group = Pais))+
  geom_vline(aes(xintercept = 0))+
  facet_wrap(Categoria ~ ., strip.position = "left", nrow = 4)+
  theme_bw()+
  geom_point(aes(y = factor(Help), shape = Pais, fill = Pais), colour = "black", size = 2, position = position_jitter(height = 0.4, seed = 2022))+
  coord_cartesian(xlim = c(-1,1))+
  scale_x_continuous(expand = c(0,0), labels = c("-1","0.5","0","0.5",1))+
  scale_shape_manual(values = c(rep(c(21,22,24,25),4)))+
  scale_fill_manual(values = c(rep("#BC3C29FF",4),
                               rep("#0072B5FF",4),
                               rep("#E18727FF",4),
                               rep("#20854EFF",4)))+
  xlab("Coeficiente de correlación para la cuota de gasto y los costos adicionales relativos")+
  ylab("")+
  labs(fill = "", shape = "")+
  theme(axis.text.y = element_blank(), 
        axis.text.x = element_text(size = 6),
        axis.ticks.y = element_blank(),
        axis.title  = element_text(size = 5),
        plot.title  = element_text(size = 10),
        legend.position = "bottom",
        strip.text = element_text(size = 7),
        #strip.text.y = element_text(angle = 180),
        panel.grid.major = element_line(size = 0.3),
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(size = 0.2),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.2,0.2,0.2,0.2), "cm"),
        panel.border = element_rect(size = 0.3))+
  ggtitle("B)")

P_6.3 <- ggarrange(P.6.3.1, P.6.3.2, common.legend = TRUE, legend = "bottom")

jpeg("../1_Carbon_Pricing_Incidence/3_Analyses/1_LAC_2021/1_Figures/Figures_ESP/Figure_3_ESP.jpg", width = 15.5, height = 12, unit = "cm", res = 400)
print(P_6.3)
dev.off()

# 6.4   Figure 4 ####

data_6.4.1 <- data_5.1.1 %>%
  select(Country, access_to_transfers, poorest_20_percent, most_affected, hh_weights) %>%
  mutate(A = ifelse(most_affected == 1 & access_to_transfers == 0 & poorest_20_percent == 0, hh_weights,0),
         B = ifelse(most_affected == 0 & access_to_transfers == 0 & poorest_20_percent == 1, hh_weights,0),
         C = ifelse(most_affected == 0 & access_to_transfers == 1 & poorest_20_percent == 0, hh_weights,0),
         
         D = ifelse(most_affected == 1 & access_to_transfers == 0 & poorest_20_percent == 1, hh_weights,0),
         E = ifelse(most_affected == 1 & access_to_transfers == 1 & poorest_20_percent == 0, hh_weights,0),
         G = ifelse(most_affected == 0 & access_to_transfers == 1 & poorest_20_percent == 1, hh_weights,0),
         
         H = ifelse(most_affected == 1 & access_to_transfers == 1 & poorest_20_percent == 1, hh_weights,0))%>%
  group_by(Country)%>%
  summarise("Most Affected"       = sum(A),
            "The Poorest"         = sum(B),
            "Access to Transfers" = sum(C),
            "Most Affected&The Poorest"         = sum(D),
            "Most Affected&Access to Transfers" = sum(E),
            "The Poorest&Access to Transfers"   = sum(G),
            "Most Affected&The Poorest&Access to Transfers" = sum(H))%>%
  ungroup()%>%
  left_join(Country.Translation)

create_venn_diagram <- function(Country.0){
  data_6.4.2 <- data_6.4.1 %>%
    filter(Pais == Country.0)
  data_6.4.3 <- c(
    "Most Affected"                                 = data_6.4.2$'Most Affected',
    "The Poorest"                                   = data_6.4.2$'The Poorest',
    "Access to Transfers"                           = data_6.4.2$'Access to Transfers',
    "Most Affected&The Poorest"                     = data_6.4.2$'Most Affected&The Poorest',
    "Most Affected&Access to Transfers"             = data_6.4.2$'Most Affected&Access to Transfers',
    "The Poorest&Access to Transfers"               = data_6.4.2$'The Poorest&Access to Transfers',
    "The Poorest&Access to Transfers&Most Affected" = data_6.4.2$'Most Affected&The Poorest&Access to Transfers'
  )
  
  P.venn <- plot(euler(data_6.4.3, shape = "ellipse"), labels = FALSE,
                 quantities = list(type = "percent", fontsize = 7), fills = list(fill = c("#BC3C29FF", "#FFDC91FF", "#6F99ADFF"), alpha = 0.8),
                 main = list(label = Country.0, fontsize = 7),
                 #legend = list(side = "bottom", nrow = 1, ncol = 3)
  )
  
  data_6.4.4 <- data_joint_0 %>%
    left_join(Country.Translation)%>%
    filter(Pais == Country.0)
  
  pop <- sum(data_6.4.4$hh_weights)
  
  data_6.4.5 <- rownames_to_column(as.data.frame(data_6.4.3))%>%
    rename(Type = rowname, value = data_6.4.3)%>%
    mutate(total = pop)%>%
    mutate(percent = round(value/total,2))%>%
    mutate(label = paste0(percent*100, "%"))
  
  P.venn$children$canvas.grob$children$diagram.grob.1$children$tags$children$tag.number.1$children$tag.quantity.1$label <- data_6.4.5$label[data_6.4.5$Type == "Most Affected"]
  P.venn$children$canvas.grob$children$diagram.grob.1$children$tags$children$tag.number.2$children$tag.quantity.2$label <- data_6.4.5$label[data_6.4.5$Type == "The Poorest"]
  P.venn$children$canvas.grob$children$diagram.grob.1$children$tags$children$tag.number.3$children$tag.quantity.3$label <- data_6.4.5$label[data_6.4.5$Type == "Access to Transfers"]
  P.venn$children$canvas.grob$children$diagram.grob.1$children$tags$children$tag.number.4$children$tag.quantity.4$label <- data_6.4.5$label[data_6.4.5$Type == "Most Affected&The Poorest"]
  P.venn$children$canvas.grob$children$diagram.grob.1$children$tags$children$tag.number.5$children$tag.quantity.5$label <- data_6.4.5$label[data_6.4.5$Type == "Most Affected&Access to Transfers"]
  P.venn$children$canvas.grob$children$diagram.grob.1$children$tags$children$tag.number.6$children$tag.quantity.6$label <- data_6.4.5$label[data_6.4.5$Type == "The Poorest&Access to Transfers"]
  P.venn$children$canvas.grob$children$diagram.grob.1$children$tags$children$tag.number.7$children$tag.quantity.7$label <- data_6.4.5$label[data_6.4.5$Type == "The Poorest&Access to Transfers&Most Affected"]
  
  return(P.venn)
  
}

P.List <- list()

for(Country in c("Argentina", "Barbados", "Bolivia", "Brasil", "Chile",
                 "Colombia", "Costa Rica", "República Dominicana", "Ecuador", "El Salvador",
                 "Guatemala", "México", "Nicaragua", "Paraguay", "Perú", "Uruguay")){
  P.V <- create_venn_diagram(Country)
  P.List[[Country]] <- P.V
}

P.Nic <- create_venn_diagram("Nicaragua")
P.Nic
P.List[["Nicaragua"]] <- P.Nic

P.6.4.1 <- ggarrange(P.List$Argentina, P.List$Barbados, P.List$Bolivia, P.List$Brasil,  P.List$Chile,
                     P.List$Colombia, P.List$`Costa Rica`, P.List$`República Dominicana`, P.List$Ecuador, P.List$`El Salvador`,
                     P.List$Guatemala, P.List$México, P.List$Nicaragua, P.List$Paraguay, P.List$Perú, P.List$Uruguay, ncol = 4, nrow = 4)

jpeg("../1_Carbon_Pricing_Incidence/3_Analyses/1_LAC_2021/1_Figures/Figures_ESP/Figure_4_ESP.jpg", width = 15.5, height = 15.5, unit = "cm", res = 400)
print(P.6.4.1)
dev.off()

data.0 <- data.frame(A = c("20% más afectados", "Acceso a transferencias", "20% más pobres"),
                     B = c(1,2,3),
                     C = c(1,2,3))
data.0$A <- factor(data.0$A, levels = c("20% más afectados", "20% más pobres", "Acceso a transferencias"))

Legend <- ggplot(data.0, aes(x = B, y = C, fill = A))+
  geom_point(shape = 21, alpha = 0.8, size = 2)+
  scale_fill_manual(values = c("#BC3C29FF", "#FFDC91FF", "#6F99ADFF"))+
  guides(fill = guide_legend(nrow = 1))+
  theme_bw()+
  labs(fill = "")

Legend.6.4 <- ggdraw(get_legend(Legend))

jpeg("../1_Carbon_Pricing_Incidence/3_Analyses/1_LAC_2021/1_Figures/Figures_ESP/Figure_4_Legend_ESP.jpg", width = 15.5, height = 1, unit = "cm", res = 400)
print(Legend.6.4)
dev.off()
