# 0     General ####

# Author: L. Missbach, missbach@mcc-berlin.net

# 0.1   Packages ####

library("cowplot")
library("eulerr")
library("ggpubr")
library("ggrepel")
library("ggsci")
library("haven")
library("Hmisc")
library("knitr")
library("kableExtra")
library("openxlsx")
library("rattle")
library("scales")
library("tidyverse")
library("VennDiagram")
options(scipen=999)

# 1     Loading Data ####

for(Country.Name in c("Argentina", "Barbados", "Bolivia", "Brazil" ,
                      "Chile", "Colombia", "Costa Rica", "Dominican Republic", "Ecuador",
                      "El_Salvador", "Guatemala", "Mexico", "Nicaragua","Peru", "Uruguay")) {

carbon_pricing_incidence_0 <- read_csv(sprintf("../1_Carbon_Pricing_Incidence/3_Analyses/1_LAC_2021/4_Transformed Data/Carbon_Pricing_Incidence_%s.csv", Country.Name))

household_information_0    <- read_csv(sprintf("../1_Carbon_Pricing_Incidence/3_Analyses/1_LAC_2021/4_Transformed Data/household_information_%s_new.csv", Country.Name))

#fuel_expenditures_0       <- read_csv(sprintf("../1_Carbon_Pricing_Incidence/1_Data_Incidence_Analysis/2_Fuel_Expenditure_Data/fuel_expenditures_%s.csv", Country.Name))

# 2   Graphics Individual ####

carbon_pricing_incidence_1 <- left_join(carbon_pricing_incidence_0, household_information_0)

# 2.1 Cumulative Curves ####

adjust_0 <- 0.3

add_on_df <- expand_grid(Income_Group_5 = c(1,2,3,4,5), burden_CO2_national = c(seq(0,0.1,0.001)))%>%
  mutate(hh_weights = 0)

add_on_weights <- carbon_pricing_incidence_1 %>%
  group_by(Income_Group_5)%>%
  summarise(IG_weights = sum(hh_weights))%>%
  ungroup()

carbon_pricing_incidence_2.1 <- carbon_pricing_incidence_1 %>%
  mutate(burden_CO2_national = round(burden_CO2_national,3))%>%
  filter(!is.na(burden_CO2_national))%>%
  bind_rows(add_on_df)%>%
  group_by(Income_Group_5, burden_CO2_national)%>%
  summarise(hh_weights = sum(hh_weights))%>%
  ungroup()%>%
  left_join(add_on_weights, by = "Income_Group_5")%>%
  mutate(share = hh_weights/IG_weights)

add_on_median <- carbon_pricing_incidence_2.1 %>%
  group_by(Income_Group_5)%>%
  mutate(cumsum_shares = cumsum(share))%>%
  filter(cumsum_shares >= 0.5)%>%
  slice(which.min(cumsum_shares))%>%
  ungroup()%>%
  rename(median = burden_CO2_national)%>%
  select(Income_Group_5, median)

add_on_median <- ggplot_build(ggplot(carbon_pricing_incidence_2.1, aes(y = share, x = burden_CO2_national, group = factor(Income_Group_5)))+
               geom_smooth(method = "loess", span = adjust_0, se = FALSE))$data[[1]]%>%
  select(x,y,group)%>%
  left_join(add_on_median, by = c("group" = "Income_Group_5"))%>%
  mutate(help = median - x)%>%
  mutate(help_0 = ifelse(help <0, help*-1, help))%>%
  group_by(group)%>%
  filter(help_0 == min(help_0))%>%
  ungroup()%>%
  rename(Income_Group_5 = group, median.x = x, median.y = y)%>%
  select(-median, -help, -help_0)%>%
  select(median.x, median.y, Income_Group_5)

carbon_pricing_incidence_2.1 <- left_join(carbon_pricing_incidence_2.1, add_on_median)

plot_figure_1 <- function(ATT  = element_text(size = 7), ATX = element_text(size = 7), ATY = element_text(size = 7),
                          XLAB = "Carbon Price Incidence",
                          YLAB = "Share of Households per Quintile", 
                          fill0 = "none"){

P_X <- ggplot(carbon_pricing_incidence_2.1, aes(group = factor(Income_Group_5), colour = factor(Income_Group_5), linetype = factor(Income_Group_5)))+
  theme_bw()+
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
        panel.border = element_rect(size = 0.3))+
  #annotate("rect", xmin = min_median, xmax = max_median, ymin = 0, ymax = 0.11, alpha = 0.5, fill = "grey")+
  #annotate("segment", x = min_median, xend = max_median, y = 0.078, yend = 0.078, arrow = arrow(ends = "both", angle = 90, length = unit (.05, "cm")), size = 0.2)+
  #annotate("text", x = (min_median + max_median)/2, y = 0.081, label = "paste(Delta, V)", parse = TRUE, size = 1.5)+
  geom_smooth(aes(x = burden_CO2_national, y = share), size = 0.3, method = "loess", n = 160, span = adjust_0, se = FALSE, fullrange = TRUE)+
  geom_point(aes(x = median.x, y = median.y, group = factor(Income_Group_5), fill = factor(Income_Group_5)), shape = 21, size = 1.3, stroke = 0.2, colour = "black")+
  xlab(XLAB) +
  ylab(YLAB) +
  labs(colour = "", linetype = "", fill = "")+
  scale_y_continuous(breaks = c(0,0.025,0.05,0.075), expand = c(0,0), labels = scales::percent_format(accuracy = 0.1))+
  scale_x_continuous(expand = c(0,0), labels = scales::percent_format(accuracy = 1), breaks = seq(0,0.08, 0.02))+
  coord_cartesian(xlim = c(0,0.085), ylim = c(0,0.085))+
  #geom_segment(aes(x = median, xend = median, y = 0, yend = 100, colour = factor(Income_Group_5), linetype = factor(Income_Group_5)), size = 1)+
  scale_colour_manual(  values = c("#BC3C29FF","#00A087FF","#000000","#E18727FF",   "#0072B5FF"))+
  scale_fill_manual(    values = c("#BC3C29FF","#00A087FF","#000000","#E18727FF",   "#0072B5FF"))+
  scale_linetype_manual(values = c("solid", "longdash", "dotdash", "solid", "solid"))+
  ggtitle(Country.Name)+
  #guides(fill = guide_legend("Expenditure Quintile"), colour = guide_legend("Expenditure Quintile"), linetype = guide_legend("Expenditure Quintile"))
  guides(fill = fill0, colour = fill0, linetype = fill0)

return(P_X)
}

P_1 <- plot_figure_1()

jpeg(sprintf("../1_Carbon_Pricing_Incidence/3_Analyses/1_LAC_2021/1_Figures/Figure_1_Distribution/National_Carbon_Price_Figure_1_%s.jpg", Country.Name), width = 6, height = 6, unit = "cm", res = 400)
print(P_1)
dev.off()

# L_1 <- ggdraw(get_legend(P_1))
# jpeg("../1_Carbon_Pricing_Incidence/2_Figures/Figure_1_Distribution_National_Carbon_Price/Legend_1.jpg", width = 8*400, height = 2*400, res = 400)
# L_1
# dev.off()

# 2.2 Boxplots ####

carbon_pricing_incidence_2.2 <- carbon_pricing_incidence_1 %>%
  group_by(Income_Group_5)%>%
  summarise(
    y5  = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.05),
    y25 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.25),
    y50 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.5),
    y75 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.75),
    y95 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.95),
    mean = wtd.mean(   burden_CO2_national, weights = hh_weights))%>%
  ungroup()

# Default Y-Axis
ylim0 <- 0.085

if(Country.Name == "Argentina" | Country.Name == "Turkey" | Country.Name == "South Africa") ylim0 <- 0.205

plot_figure_2 <- function(ATT  = element_text(size = 7), ATX = element_text(size = 7), ATY = element_text(size = 7),
                          XLAB = "Expenditure Quintiles",
                          YLAB = "Carbon Price Incidence", 
                          fill0 = "none",
                          accuracy_0 = 1,
                          data_0 = carbon_pricing_incidence_2.2,
                          title_0 = Country.Name){

P_2 <- ggplot(data_0, aes(x = factor(Income_Group_5)))+
  geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95), stat = "identity", position = position_dodge(0.5), outlier.shape = NA, width = 0.5, size = 0.3) +
  theme_bw()+
  xlab(XLAB)+ ylab(YLAB)+
  geom_point(aes(y = mean), shape = 23, size = 1.3, stroke = 0.2, fill = "white")+
  scale_y_continuous(labels = scales::percent_format(accuracy = accuracy_0), expand = c(0,0))+
  scale_x_discrete(labels = c("1 \n Poorest \n 20 Percent", "2", "3", "4", "5 \n Richest \n 20 Percent"))+
  coord_cartesian(ylim = c(0,ylim0))+
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

return(P_2)
}

P_2 <- plot_figure_2()

jpeg(sprintf("../1_Carbon_Pricing_Incidence/3_Analyses/1_LAC_2021/1_Figures/Figure_2_Boxplot/National_Carbon_Price_Figure_2_%s.jpg", Country.Name), width = 6, height = 6, unit = "cm", res = 400)
print(P_2)
dev.off()

# 2.3 Vertical Distribution across Instruments ####

carbon_pricing_incidence_2.3 <- carbon_pricing_incidence_1 %>%
  group_by(Income_Group_5)%>%
  summarise(
    wtd.median_CO2_global   = wtd.quantile(burden_CO2_global,   weight = hh_weights, probs = 0.5),
    wtd.median_CO2_national = wtd.quantile(burden_CO2_national, weight = hh_weights, probs = 0.5),
    wtd.median_transport    = wtd.quantile(burden_CO2_transport,    weight = hh_weights, probs = 0.5),
    wtd.median_electricity  = wtd.quantile(burden_CO2_electricity,  weight = hh_weights, probs = 0.5)
  )%>%
  ungroup()

carbon_pricing_incidence_2.3 <- carbon_pricing_incidence_2.3 %>%
  mutate(CO2_global       = wtd.median_CO2_global  /carbon_pricing_incidence_2.3$wtd.median_CO2_global[1],
         CO2_national     = wtd.median_CO2_national/carbon_pricing_incidence_2.3$wtd.median_CO2_national[1],
         transport        = wtd.median_transport   /carbon_pricing_incidence_2.3$wtd.median_transport[1],
         electricity      = wtd.median_electricity /carbon_pricing_incidence_2.3$wtd.median_electricity[1])%>%
  select(-starts_with("wtd."))%>%
  pivot_longer(-Income_Group_5, names_to = "type", values_to = "Value")%>%
  unite(help, c("type", "Income_Group_5"), sep = "_", remove = FALSE)

plot_figure_3 <- function(ATT  = element_text(size = 7), ATX = element_text(size = 7), ATY = element_text(size = 7),
                          XLAB = "Expenditure Quintiles",
                          YLAB = "Carbon Price Incidence", 
                          fill0 = "none",
                          data_0 = carbon_pricing_incidence_2.3,
                          title_0 = Country.Name){
  P_3 <- ggplot(data_0, aes(x = factor(Income_Group_5)))+
    geom_hline(yintercept = 1, colour = "black", size = 0.3)+
    #geom_ribbon(aes(ymin = low, ymax = upper, group = type, fill = type), alpha = 0.2)+
    #geom_label_repel(aes(y = 1,    group = type,  label = label),   size = 1.6, segment.linetype = 1, segment.size = 0.1, box.padding = 0.00, label.padding = 0.10, label.r = 0.05, direction = "y", min.segment.length = 0, nudge_y = nudge_0)+
    #geom_label_repel(aes(y = 3,    group = type,  label = Label_2), size = 1.6, segment.linetype = 1, segment.size = 0.1, box.padding = 0.00, label.padding = 0.10, label.r = 0.05, direction = "y", min.segment.length = 0, nudge_y = -0.6)+
    #geom_label_repel(aes(y = pure, group = type, segment.linetype = 1, label = label_emissions_coverage, segment.size = 1, size = 15), min.segment.length = 0, hjust = 1, force_pull = 0, nudge_x = 1)+
    geom_line(aes( y = Value, group = type, colour = type, alpha = type), size = 0.4, position = position_dodge(0.2))+
    geom_point(aes(y = Value, group = type, fill = type, shape = type, alpha = type), size = 1.5, colour = "black", position = position_dodge(0.2), stroke = 0.2)+
    scale_colour_npg(  labels = c("International Carbon Price","National Carbon Price", "Electricity Sector Carbon Price", "Liquid Fuel Carbon Price")) +
    scale_fill_npg  (  labels = c("International Carbon Price","National Carbon Price", "Electricity Sector Carbon Price", "Liquid Fuel Carbon Price"))+
    scale_shape_manual(labels = c("International Carbon Price","National Carbon Price", "Electricity Sector Carbon Price", "Liquid Fuel Carbon Price"), values = c(21,22,23,24,25))+
    scale_alpha_manual(labels = c("International Carbon Price","National Carbon Price", "Electricity Sector Carbon Price", "Liquid Fuel Carbon Price"), values = c(1,1,1,1,1))+
    labs(fill = "", colour = "", shape = "", alpha = "", linetype = "")+
    theme_bw() + 
    scale_x_discrete(labels = c("1","2","3","4","5"))+
    #scale_y_continuous(breaks = seq(limit_low, limit_up, step_0))+
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
          panel.border = element_rect(size = 0.3))+
    coord_cartesian(ylim = c(0.5,2.5))+
    #guides(fill = guide_legend(nrow = 2, order = 1), colour = guide_legend(nrow = 2, order = 1), shape = guide_legend(nrow = 2, order = 1), alpha = FALSE, size = FALSE)+
    guides(fill = fill0, colour = fill0, shape = fill0, size = fill0, alpha = fill0)+
    xlab(XLAB)+
    ylab(YLAB)+ 
    ggtitle(title_0)
  
  return(P_3)
  
}

P_3 <- plot_figure_3()

jpeg(sprintf("../1_Carbon_Pricing_Incidence/3_Analyses/1_LAC_2021/1_Figures/Figure_3_Vertical_Effects/Figure_3_%s.jpg", Country.Name), width = 6, height = 6, unit = "cm", res = 400)
print(P_3)
dev.off()

# 2.4 Correlations with Incidence - Energy, Fuels etc. ####
# 2.5 Tax Policies ####
# 2.6 Maps ####
# 2.7 
# 3.X ####

print(paste0("End ", Country.Name))

rm(list = ls())
}

# 4     Joint Figures ####

data_joint_0 <- data.frame()

for(Country.Name in c("Argentina", "Barbados","Bolivia", "Brazil", "Chile", "Colombia",
                      "Costa Rica", "Dominican Republic", "Ecuador",
                      "El Salvador", "Guatemala", "Mexico", "Nicaragua", "Paraguay", "Peru", "Uruguay")) {
  
  carbon_pricing_incidence_0 <- read_csv(sprintf("../1_Carbon_Pricing_Incidence/3_Analyses/1_LAC_2021/4_Transformed Data/Carbon_Pricing_Incidence_%s.csv", Country.Name))
  
  household_information_0    <- read_csv(sprintf("../1_Carbon_Pricing_Incidence/3_Analyses/1_LAC_2021/4_Transformed Data/household_information_%s_new.csv", Country.Name))
  
  if(Country.Name == "El_Salvador") Country.Name.2 <- "El Salvador" else Country.Name.2 <- Country.Name
  
  if(Country.Name == "Bolivia") {
    ETH <- read_csv("../0_Data/1_Household Data/3_Bolivia/2_Codes/Ethnicity.Code.csv")%>%
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

# 4.1   Boxplots ####
# 4.1.1 National Carbon Price ####

carbon_pricing_incidence_4.1.1 <- data_joint_0 %>%
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

plot_figure_2 <- function(ATT  = element_text(size = 7), ATX = element_text(size = 7), ATY = element_text(size = 7),
                          XLAB = "Expenditure Quintiles",
                          YLAB = "Carbon Price Incidence", 
                          fill0 = "none",
                          accuracy_0 = 1,
                          data_0 = carbon_pricing_incidence_4.1.1,
                          title_0 = "National Carbon Prices"){
  
  P_2 <- ggplot(data_0, aes(x = as.character(Income_Group_5)))+
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
  
  return(P_2)
}

P_4.1.1 <- plot_figure_2()

jpeg("../1_Carbon_Pricing_Incidence/3_Analyses/1_LAC_2021/1_Figures/Figure_2_Boxplot/National_Carbon_Price_Figure_2_Joint_1_Flipped_Ext.jpg", width = 15.5, height = 15, unit = "cm", res = 400)
print(P_4.1.1)
dev.off()

# 4.1.2 Fossil Fuel Subsidy Reform ####

carbon_pricing_incidence_4.1.2 <- data_joint_0 %>%
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

plot_figure_3 <- function(ATT  = element_text(size = 7), ATX = element_text(size = 7), ATY = element_text(size = 7),
                          XLAB = "Expenditure Quintiles",
                          YLAB = "Fossil Fuel Subsidy Reform Incidence", 
                          fill0 = "none",
                          accuracy_0 = 1,
                          data_0 = carbon_pricing_incidence_4.1.2,
                          title_0 = "Fossil Fuel Subsidy Reform*"){
  
  P_3 <- ggplot(data_0, aes(x = factor(Income_Group_5)))+
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
  
  return(P_3)
}

P_4.1.2 <- plot_figure_3()

jpeg("../1_Carbon_Pricing_Incidence/3_Analyses/1_LAC_2021/6_App/Latin-America-Paper/Figures/Figure_2_Boxplot_FFF_Joint_Flipped.jpg", width = 15.5, height = 15, unit = "cm", res = 400)
print(P_4.1.2)
dev.off()

# 4.2   Carbon Footprints over Household Expenditures ####

P_4.2 <- ggplot(data_joint_0)+
  geom_smooth(formula = y ~ x, aes(y = CO2_t_national, x = hh_expenditures_USD_2014), method = "lm", se = FALSE, colour = "black", size = 0.6, fullrange = TRUE)+
  geom_point(aes(y = CO2_t_national, x = hh_expenditures_USD_2014, fill = Country), colour = "black", shape = 21, alpha = 0.2, size = 0.8)+
  facet_wrap(.~Country, nrow = 4)+
  coord_cartesian(xlim = c(0,99000), ylim = c(0,50))+
  guides(fill = "none", colour = "none")+
  theme_bw()+
  ylab("Carbon Footprint in tCO2")+
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

jpeg("../1_Carbon_Pricing_Incidence/3_Analyses/1_LAC_2021/6_App/Latin-America-Paper/Figures/Figure_1_Carbon_Footprints.jpg", width = 15.5, height = 15, unit = "cm", res = 400)
print(P_4.2)
dev.off()

# 4.3   Venn-Diagramm on the Targeted, the Poor, the Affected ####

# 4.3.1 Mexico ####

barrier_0 <- wtd.quantile(data_Mexico$burden_CO2_national, probs = 0.8, weights = data_Mexico$hh_weights)

data_Mexico <- data_joint_0 %>%
  filter(Country == "Mexico")%>%
  mutate(poorest_20_percent  = ifelse(Income_Group_5 == 1,1,0),
         access_to_transfers = ifelse(!is.na(inc_gov_cash) & (inc_gov_cash > 0 | inc_gov_monetary > 0),1,0),
         most_affected       = ifelse(burden_CO2_national>barrier_0,1,0))%>%
  filter(poorest_20_percent == 1 | most_affected == 1 | access_to_transfers == 1)

data_Mexico_1 <- select(data_Mexico, poorest_20_percent, most_affected, access_to_transfers)
weigths_Mexico <- data_Mexico$hh_weights
data_Mexico_2 <- data_Mexico_1 %>%
  mutate(access_poor     = ifelse(poorest_20_percent == 1 & access_to_transfers == 1,1,0),
         access_affected = ifelse(most_affected == 1      & access_to_transfers == 1,1,0),
         poor_affected   = ifelse(most_affected == 1      & poorest_20_percent == 1,1,0),
         poor_affected_access = ifelse(most_affected == 1 & poorest_20_percent == 1 & access_to_transfers == 1,1,0))
data_Mexico_2.1 <- select(data_Mexico, access_to_transfers, poorest_20_percent, most_affected, hh_weights) %>%
  #mutate("Most Affected"       = ifelse(most_affected == 1 & access_to_transfers == 0 & poorest_20_percent == 0, hh_weights,0),
  #       "The Poorest"         = ifelse(most_affected == 0 & access_to_transfers == 0 & poorest_20_percent == 1, hh_weights,0),
  #       "Access to Transfers" = ifelse(most_affected == 0 & access_to_transfers == 1 & poorest_20_percent == 0, hh_weights,0),
  #       
  #       "Most Affected&The Poorest"         = ifelse(most_affected == 1 & access_to_transfers == 0 & poorest_20_percent == 1, hh_weights,0),
  #       "Most Affected&Access to Transfers" = ifelse(most_affected == 1 & access_to_transfers == 1 & poorest_20_percent == 0, hh_weights,0),
  #       "The Poorest&Access to Transfers"   = ifelse(most_affected == 0 & access_to_transfers == 1 & poorest_20_percent == 1, hh_weights,0),
  #       
  #       "Most Affected&The Poorest&Access to Transfers" = ifelse(most_affected == 1 & access_to_transfers == 1 & poorest_20_percent == 1, hh_weights,0))%>%
  mutate(A = ifelse(most_affected == 1 & access_to_transfers == 0 & poorest_20_percent == 0, hh_weights,0),
         B = ifelse(most_affected == 0 & access_to_transfers == 0 & poorest_20_percent == 1, hh_weights,0),
         C = ifelse(most_affected == 0 & access_to_transfers == 1 & poorest_20_percent == 0, hh_weights,0),
         
         D = ifelse(most_affected == 1 & access_to_transfers == 0 & poorest_20_percent == 1, hh_weights,0),
         E = ifelse(most_affected == 1 & access_to_transfers == 1 & poorest_20_percent == 0, hh_weights,0),
         G = ifelse(most_affected == 0 & access_to_transfers == 1 & poorest_20_percent == 1, hh_weights,0),
         
         H = ifelse(most_affected == 1 & access_to_transfers == 1 & poorest_20_percent == 1, hh_weights,0))%>%
  summarise("Most Affected"       = sum(A),
            "The Poorest"         = sum(B),
            "Access to Transfers" = sum(C),
            "Most Affected&The Poorest"         = sum(D),
            "Most Affected&Access to Transfers" = sum(E),
            "The Poorest&Access to Transfers"   = sum(G),
            "Most Affected&The Poorest&Access to Transfers" = sum(H))

# data_Mexico_3.1 <- c("Most Affected" = 4181882,
#                    "The Poorest"   = 3030246,
#                    "Access to Transfers" = 7322681,
#                    "Most Affected&The Poorest" = 759395,
#                    "Most Affected&Access to Transfers" = 1659255,
#                    "The Poorest&Access to Transfers"   = 2811277,
#                    "The Poorest&Access to Transfers&Most Affected" = 542196)

P.4.3 <- plot(euler(data_Mexico_3.1, shape = "ellipse"), quantities = TRUE, fill = c("#BC3C29FF", "#FFDC91FF", "#6F99ADFF"),
     main = "Typology of Mexican Households")

jpeg("../1_Carbon_Pricing_Incidence/3_Analyses/1_LAC_2021/1_Figures/Figure_4_Euler_Diagrams/Figure_4_Mexico.jpg", width = 15.5, height = 12, unit = "cm", res = 400)
print(P.4.3)
dev.off()

plot <- draw.triple.venn(area1 = sum(data_Mexico_2$most_affected),
             sum(data_Mexico_2$poorest_20_percent),
             sum(data_Mexico_2$access_to_transfers),
             sum(data_Mexico_2$poor_affected),
             sum(data_Mexico_2$access_poor),
             sum(data_Mexico_2$access_affected),
             sum(data_Mexico_2$poor_affected_access), euler.d = TRUE, scaled = TRUE,
             category = c("Most affected", "Poor", "Access to transfers"))

ggVennDiagram((data_Mexico_1$poorest_20_percent, data_Mexico_1$access_to_transfers, data_Mexico_1$most_affected))

# 4.3.2 Brazil ####

barrier_0 <- wtd.quantile(data_Brazil$burden_CO2_national, probs = 0.8, weights = data_Brazil$hh_weights)

data_Brazil <- data_joint_0 %>%
  filter(Country == "Brazil")%>%
  mutate(poorest_20_percent  = ifelse(Income_Group_5 == 1,1,0),
         access_to_transfers = ifelse(!is.na(inc_gov_cash) & (inc_gov_cash > 0 | inc_gov_monetary > 0),1,0),
         most_affected       = ifelse(burden_CO2_national>barrier_0,1,0))%>%
  filter(poorest_20_percent == 1 | most_affected == 1 | access_to_transfers == 1)

data_Brazil_1 <- select(data_Brazil, poorest_20_percent, most_affected, access_to_transfers)
weigths_Brazil <- data_Brazil$hh_weights
data_Brazil_2 <- data_Brazil_1 %>%
  mutate(access_poor     = ifelse(poorest_20_percent == 1 & access_to_transfers == 1,1,0),
         access_affected = ifelse(most_affected == 1      & access_to_transfers == 1,1,0),
         poor_affected   = ifelse(most_affected == 1      & poorest_20_percent == 1,1,0),
         poor_affected_access = ifelse(most_affected == 1 & poorest_20_percent == 1 & access_to_transfers == 1,1,0))
data_Brazil_2.1 <- select(data_Brazil, access_to_transfers, poorest_20_percent, most_affected, hh_weights) %>%
  #mutate("Most Affected"       = ifelse(most_affected == 1 & access_to_transfers == 0 & poorest_20_percent == 0, hh_weights,0),
  #       "The Poorest"         = ifelse(most_affected == 0 & access_to_transfers == 0 & poorest_20_percent == 1, hh_weights,0),
  #       "Access to Transfers" = ifelse(most_affected == 0 & access_to_transfers == 1 & poorest_20_percent == 0, hh_weights,0),
  #       
  #       "Most Affected&The Poorest"         = ifelse(most_affected == 1 & access_to_transfers == 0 & poorest_20_percent == 1, hh_weights,0),
  #       "Most Affected&Access to Transfers" = ifelse(most_affected == 1 & access_to_transfers == 1 & poorest_20_percent == 0, hh_weights,0),
  #       "The Poorest&Access to Transfers"   = ifelse(most_affected == 0 & access_to_transfers == 1 & poorest_20_percent == 1, hh_weights,0),
  #       
  #       "Most Affected&The Poorest&Access to Transfers" = ifelse(most_affected == 1 & access_to_transfers == 1 & poorest_20_percent == 1, hh_weights,0))%>%
  mutate(A = ifelse(most_affected == 1 & access_to_transfers == 0 & poorest_20_percent == 0, hh_weights,0),
         B = ifelse(most_affected == 0 & access_to_transfers == 0 & poorest_20_percent == 1, hh_weights,0),
         C = ifelse(most_affected == 0 & access_to_transfers == 1 & poorest_20_percent == 0, hh_weights,0),
         
         D = ifelse(most_affected == 1 & access_to_transfers == 0 & poorest_20_percent == 1, hh_weights,0),
         E = ifelse(most_affected == 1 & access_to_transfers == 1 & poorest_20_percent == 0, hh_weights,0),
         G = ifelse(most_affected == 0 & access_to_transfers == 1 & poorest_20_percent == 1, hh_weights,0),
         
         H = ifelse(most_affected == 1 & access_to_transfers == 1 & poorest_20_percent == 1, hh_weights,0))%>%
  summarise("Most Affected"       = sum(A),
            "The Poorest"         = sum(B),
            "Access to Transfers" = sum(C),
            "Most Affected&The Poorest"         = sum(D),
            "Most Affected&Access to Transfers" = sum(E),
            "The Poorest&Access to Transfers"   = sum(G),
            "Most Affected&The Poorest&Access to Transfers" = sum(H))

#data_Brazil_3.1 <- c("Most Affected" = 7193562,
#                     "The Poorest"   = 4874763,
#                     "Access to Transfers" = 12089309,
#                     "Most Affected&The Poorest" = 2664433,
#                     "Most Affected&Access to Transfers" = 1916138,
#                     "The Poorest&Access to Transfers"   = 4229206,
#                     "The Poorest&Access to Transfers&Most Affected" = 1994806)

P.4.3 <- plot(euler(data_Brazil_3.1, shape = "ellipse"), quantities = TRUE, fill = c("#BC3C29FF", "#FFDC91FF", "#6F99ADFF"),
              main = "Typology of Brazilian Households")

jpeg("../1_Carbon_Pricing_Incidence/3_Analyses/1_LAC_2021/1_Figures/Figure_4_Euler_Diagrams/Figure_4_Brazil.jpg", width = 15.5, height = 12, unit = "cm", res = 400)
print(P.4.3)
dev.off()

# 4.X Summary Statistics

data_4.X <- data_joint_0 %>%
  mutate(population = hh_size*hh_weights)%>%
  group_by(Country)%>%
  summarise(number = n(),
            population = sum(population))%>%
  ungroup()

t <- read_dta("../0_Data/1_Household Data/3_Chile/1_Data_Clean/LAC_Clean/CHL_EPF_2016-2017.dta")

# 4.3.3 Joint ####

data_4.3.3.1 <- data_joint_0 %>%
  group_by(Country)%>%
  mutate(barrier_0 = wtd.quantile(burden_CO2_national, probs = 0.8, weights = hh_weights))%>%
  ungroup()%>%
  mutate(poorest_20_percent  = ifelse(Income_Group_5 == 1,1,0),
         access_to_transfers = ifelse((!is.na(inc_gov_cash)|!is.na(inc_gov_monetary))&(inc_gov_cash > 0 | inc_gov_monetary > 0),1,0),
         most_affected       = ifelse(burden_CO2_national > barrier_0,1,0))%>%
  filter(poorest_20_percent == 1 | most_affected == 1 | access_to_transfers == 1)

data_4.3.3.2 <- data_4.3.3.1 %>%
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
  data_4.3.3.3 <- data_4.3.3.2 %>%
    filter(Country == Country.0)
  data_4.3.3.4 <- c(
    "Most Affected"                                 = data_4.3.3.3$'Most Affected',
    "The Poorest"                                   = data_4.3.3.3$'The Poorest',
    "Access to Transfers"                           = data_4.3.3.3$'Access to Transfers',
    "Most Affected&The Poorest"                     = data_4.3.3.3$'Most Affected&The Poorest',
    "Most Affected&Access to Transfers"             = data_4.3.3.3$'Most Affected&Access to Transfers',
    "The Poorest&Access to Transfers"               = data_4.3.3.3$'The Poorest&Access to Transfers',
    "The Poorest&Access to Transfers&Most Affected" = data_4.3.3.3$'Most Affected&The Poorest&Access to Transfers'
  )
  
  P.venn <- plot(euler(data_4.3.3.4, shape = "ellipse"), labels = FALSE,
                 quantities = list(type = "percent", fontsize = 7), fills = list(fill = c("#BC3C29FF", "#FFDC91FF", "#6F99ADFF"), alpha = 0.8),
                 main = list(label = Country.0, fontsize = 7),
                 #legend = list(side = "bottom", nrow = 1, ncol = 3)
                 )

  data_4.3.3.5 <- data_joint_0 %>%
    filter(Country == Country.0)
  
  pop <- sum(data_4.3.3.5$hh_weights)
  
  data_4.3.3.6 <- rownames_to_column(as.data.frame(data_4.3.3.4))%>%
    rename(Type = rowname, value = data_4.3.3.4)%>%
    mutate(total = pop)%>%
    mutate(percent = round(value/total,2))%>%
    mutate(label = paste0(percent*100, "%"))
    
P.venn$children$canvas.grob$children$diagram.grob.1$children$tags$children$tag.number.1$children$tag.quantity.1$label <- data_4.3.3.6$label[data_4.3.3.6$Type == "Most Affected"]
P.venn$children$canvas.grob$children$diagram.grob.1$children$tags$children$tag.number.2$children$tag.quantity.2$label <- data_4.3.3.6$label[data_4.3.3.6$Type == "The Poorest"]
P.venn$children$canvas.grob$children$diagram.grob.1$children$tags$children$tag.number.3$children$tag.quantity.3$label <- data_4.3.3.6$label[data_4.3.3.6$Type == "Access to Transfers"]
P.venn$children$canvas.grob$children$diagram.grob.1$children$tags$children$tag.number.4$children$tag.quantity.4$label <- data_4.3.3.6$label[data_4.3.3.6$Type == "Most Affected&The Poorest"]
P.venn$children$canvas.grob$children$diagram.grob.1$children$tags$children$tag.number.5$children$tag.quantity.5$label <- data_4.3.3.6$label[data_4.3.3.6$Type == "Most Affected&Access to Transfers"]
P.venn$children$canvas.grob$children$diagram.grob.1$children$tags$children$tag.number.6$children$tag.quantity.6$label <- data_4.3.3.6$label[data_4.3.3.6$Type == "The Poorest&Access to Transfers"]
P.venn$children$canvas.grob$children$diagram.grob.1$children$tags$children$tag.number.7$children$tag.quantity.7$label <- data_4.3.3.6$label[data_4.3.3.6$Type == "The Poorest&Access to Transfers&Most Affected"]

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

P.V <- ggarrange(P.List$Argentina, P.List$Barbados, P.List$Brazil, P.List$Bolivia, P.List$Chile,
                 P.List$Colombia, P.List$`Costa Rica`, P.List$`Dominican Republic`, P.List$Ecuador, P.List$`El Salvador`,
                 P.List$Guatemala, P.List$Mexico, P.List$Nicaragua, P.List$Paraguay, P.List$Peru, P.List$Uruguay, ncol = 4, nrow = 4)


jpeg("../1_Carbon_Pricing_Incidence/3_Analyses/1_LAC_2021/1_Figures/Figure_4_Euler_Diagrams/Figure_4_joint_1.jpg", width = 15.5, height = 15.5, unit = "cm", res = 400)
print(P.V)
dev.off()

jpeg("../1_Carbon_Pricing_Incidence/3_Analyses/1_LAC_2021/1_Figures/Figure_4_Euler_Diagrams/Legend_Figure_4_joint_1.jpg", width = 15.5, height = 1, unit = "cm", res = 400)
print(Legend.2)
dev.off()


# 4.3.4 Tables ####

data_4.3.4.0 <- data_joint_0 %>%
  group_by(Country)%>%
  mutate(barrier_0 = wtd.quantile(burden_CO2_national, probs = 0.8, weights = hh_weights))%>%
  ungroup()%>%
  mutate(poorest_20_percent  = ifelse(Income_Group_5 == 1,1,0),
         access_to_transfers = ifelse((!is.na(inc_gov_cash)|!is.na(inc_gov_monetary))&(inc_gov_cash > 0 | inc_gov_monetary > 0),1,0),
         most_affected       = ifelse(burden_CO2_national > barrier_0,1,0))

data_4.3.4.1 <- data_4.3.4.0 %>%
  group_by(Country)%>%
  summarise(total_hh = sum(hh_weights))%>%
  ungroup()

# How many households have access to transfers, overall?

data_4.3.4.2 <- data_4.3.4.0 %>%
  filter(access_to_transfers == 1)%>%
  group_by(Country)%>%
  summarise(access_to_transfers = sum(hh_weights))%>%
  ungroup()

data_4.3.4.3 <- data_4.3.4.2 %>%
  left_join(data_4.3.4.1)%>%
  mutate(share_access = access_to_transfers/total_hh)

# How many of the most affected 20 percent are poor?
# How many of the most affected 20 percent have access to transfers?

data_4.3.4.4 <- data_4.3.4.0 %>%
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

data_4.3.4.5 <- data_4.3.4.3 %>%
  select(Country, share_access)%>%
  left_join(select(data_4.3.4.4, Country,poorest_20_percent_1, access_to_transfers_1, poor_no_access_1), by = "Country")%>%
  mutate_at(vars(-Country), list(~ paste0(round(., 3)*100, "%")))

colnames(data_4.3.4.5) <- c("Country", 
                            #"\\rotatebox{90}{Households with access to transfer programs}",
                            #"\\rotatebox{90}{...are poorer than 80\\% of the population?}",
                            #"\\rotatebox{90}{...have access to governmental transfer programs?}",
                            #"\\rotatebox{90}{...are poorer than 80\\% of the population and have no access to governmental transfer programs?}"
                            "Households with access to transfer programs",
                            "...are poorer than 80 % of  the population?",
                            "...have access to governmental transfer programs?",
                            "...are poorer than 80 % of the population and have no access to governmental transfer programs?"
)

kbl(data_4.3.4.5, format = "latex", caption = "Summary Statistics on Access to Transfer Programmes", label = "tab:transfer",
    booktabs = T, linesep = "", align = "lcccc")%>%

  kable_styling(position = "center", latex_options = c("HOLD_position", "scale_down"))%>%
  column_spec(1, width = "4 cm", border_right = T)%>%
  column_spec(2, width = "3 cm", border_right = T)%>%
  column_spec(3:5, width = "3 cm")%>%
  add_header_above(c(" " = 2, "Of the most affected 20\\\\% of households, how many..." = 3), escape = FALSE)%>%
  footnote(general = "This table reports shares of total population and shares of the 20% of population with highest carbon pricing incidence adhering to different criteria for 16 countries in Latin America and the Caribbean.", threeparttable = T)%>%
  save_kable(., "../1_Carbon_Pricing_Incidence/3_Analyses/1_LAC_2021/6_App/Latin-America-Paper/Tables/Table_A8/Table_A8.tex")

#write.xlsx(data_4.3.4.5, "../1_Carbon_Pricing_Incidence/3_Analyses/1_LAC_2021/2_Tables/Table_Summary_Stats_2/Transfers_Stats.xlsx")

# 5     Joint Analyses ####
# 5.1   Vertical vs. horizontal effects ####

data_5.1 <- data_joint_0 %>%
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

data_5.1.1 <- data_5.1 %>%
  mutate_at(vars(median_burden_CO2_national_1:dif_q80_q20_burden_CO2_national_5), list(~ label_percent(accuracy = 0.01)(.)))%>%
  mutate_at(vars(median_1_5:dif_80_20_1_5), list(~ round(.,2)))

colnames(data_5.1.1) <- c("Country", "$\\overline{AC}_{r}^{1}$", "MAC5", "H1", "H5", "H1A", "H5A", "MAC 1/5", "H 1/5", "H 1/5 A")

write.xlsx(data_5.1.1, "../1_Carbon_Pricing_Incidence/3_Analyses/1_LAC_2021/2_Tables/Table_5_Horizontal_vs_Vertical_Effects/Table_5.xlsx")

kbl(data_5.1.1, format = "latex", caption = "Comparing Median Additional Costs (AC) and Horizontal Spread between first and fifth Expenditure Quintile", 
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

data_5.1.2 <- data_5.1 %>%
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

P.5.1 <- ggplot()+
  #geom_polygon(data = poly, aes(x = y, y = x, group = g), colour = "black", alpha = 0.5, fill = NA)+
  geom_polygon(data = poly_3, aes(x = x, y = y, group = g), colour = "black", fill = NA)+
  geom_polygon(data = poly_2, aes(x = x, y = y, group = g, fill = label), alpha = 0.5)+
  geom_text(data = poly_4, aes(label = text, x = x, y = y))+
  #geom_text(data = poly, aes(x = z_4, y = z_3, group = g, label = label))+
  theme_bw()+
  geom_point(data = data_5.1.2, aes(y = median_1_5, x = dif_95_05_1_5), shape = 17, colour = "black", size = 2)+
  geom_text_repel(data = data_5.1.2, aes(label = Country_Code, y = median_1_5, x = dif_95_05_1_5),
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

jpeg("../1_Carbon_Pricing_Incidence/3_Analyses/1_LAC_2021/1_Figures/Figure_5_Vertical_vs_Horizontal/Figure_5.jpg", width = 14, height = 15, unit = "cm", res = 400)
print(P.5.1)
dev.off()

# 5.2   Correlation with Expenditure Shares ####

data_5.2 <- data.frame()

for(i in c("Argentina", "Barbados", "Bolivia", "Brazil" ,
           "Chile", "Colombia", "Costa Rica", "Dominican Republic", "Ecuador",
           "El Salvador", "Guatemala", "Mexico", "Nicaragua","Paraguay","Peru", "Uruguay")){
  data_5.2.1 <- data_joint_0 %>%
    filter(Country == i)
  
  cor_1 <- cor(data_5.2.1$share_energy,   data_5.2.1$burden_CO2_national, method = "pearson")
  cor_2 <- cor(data_5.2.1$share_goods,    data_5.2.1$burden_CO2_national, method = "pearson")
  cor_3 <- cor(data_5.2.1$share_services, data_5.2.1$burden_CO2_national, method = "pearson")
  cor_4 <- cor(data_5.2.1$share_food,     data_5.2.1$burden_CO2_national, method = "pearson")
  
  data_5.2.2 <- data.frame(Country = rep(i,4), Category= c("Energy", "Goods", "Services", "Food"), Correlation = c(cor_1, cor_2, cor_3, cor_4), Help = c(1,1,1,1))
  data_5.2 <- bind_rows(data_5.2, data_5.2.2)
  
}  

data_5.2.1 <- data_5.2 %>%
  mutate(Correlation = round(Correlation,2))%>%
  pivot_wider(names_from = "Category", values_from = "Correlation")%>%
  select(-Help)

kbl(mutate_all(data_5.2.1, linebreak), format = "latex", caption = "Correlation Coefficients for Carbon Pricing Incidence and Expenditure Shares on different Consumption Categories", 
    booktabs = T, align = "l|cccc", format.args = list(big.mark = ",", scientific = FALSE), linesep = "", vline = "")%>%
  kable_styling(position = "center", latex_options = c("HOLD_position", "scale_down"))%>%
  column_spec(1, width = "3.5 cm")%>%
  column_spec(2:5, width = "2 cm")%>%
  footnote(general = "This table displays correlation coefficients for carbon pricing incidence and expenditure shares on different consumption categories.", threeparttable = T)%>%
  save_kable(., "../1_Carbon_Pricing_Incidence/3_Analyses/1_LAC_2021/6_App/Latin-America-Paper/Tables/Table_A5/Table_A5.tex")

write.xlsx(data_5.2.1, "../1_Carbon_Pricing_Incidence/3_Analyses/1_LAC_2021/2_Tables/Correlation_Consumption.xlsx")

P.5.2 <- ggplot(data_5.2, aes(x = Correlation, group = Country))+
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

jpeg("../1_Carbon_Pricing_Incidence/3_Analyses/1_LAC_2021/1_Figures/Figure_6_Correlation_Coefficients/Figure_6.jpg", width = 15.5, height = 15, unit = "cm", res = 400)
print(P.5.2)
dev.off()

# 5.3   Correlation with Energy Expenditure Shares ####

data_5.3 <- data.frame()

for(i in c("Argentina", "Barbados", "Bolivia", "Brazil" ,
    "Chile", "Colombia", "Costa Rica", "Dominican Republic", "Ecuador",
    "El Salvador", "Guatemala", "Mexico", "Nicaragua","Paraguay","Peru", "Uruguay")){
  data_5.3.1 <- data_joint_0 %>%
    filter(Country == i)%>%
    mutate_at(vars(starts_with("exp_USD_")), list(~ ./hh_expenditures_USD_2014))%>%
    rename_at(vars(starts_with("exp_USD_")), list(~ str_replace(., "exp_USD_", "share_E_")))
  
  cor_1 <- cor(data_5.3.1$share_E_Electricity, data_5.3.1$burden_CO2_national, method = "pearson")
  cor_2 <- cor(data_5.3.1$share_E_Kerosene,    data_5.3.1$burden_CO2_national, method = "pearson")
  cor_3 <- cor(data_5.3.1$share_E_LPG,         data_5.3.1$burden_CO2_national, method = "pearson")
  cor_4 <- cor(data_5.3.1$share_E_Biomass,     data_5.3.1$burden_CO2_national, method = "pearson")
  cor_5 <- cor(data_5.3.1$share_E_Firewood,    data_5.3.1$burden_CO2_national, method = "pearson")
  cor_6 <- cor(data_5.3.1$share_E_Gas,         data_5.3.1$burden_CO2_national, method = "pearson")
  cor_7 <- cor(data_5.3.1$share_E_Petrol,      data_5.3.1$burden_CO2_national, method = "pearson")
  cor_8 <- cor(data_5.3.1$share_E_Diesel,      data_5.3.1$burden_CO2_national, method = "pearson")
  
  data_5.3.2 <- data.frame(Country = rep(i,8), Category = c("Electricity", "Kerosene", "LPG", "Biomass",
                                                            "Firewood", "Gas", "Petrol", "Diesel"),
                           Correlation = c(cor_1, cor_2, cor_3, cor_4,
                                           cor_5, cor_6, cor_7, cor_8),
                           Help = c(rep(1,8)))
  data_5.3 <- bind_rows(data_5.3, data_5.3.2)
  
}  

data_5.3.1 <- data_5.3 %>%
  mutate(Correlation = round(Correlation,2))%>%
  pivot_wider(names_from = "Category", values_from = "Correlation", values_fill = NA)%>%
  select(-Help)%>%
  mutate_all(~ ifelse(is.na(.),"",.))

kbl(mutate_all(data_5.3.1, linebreak), format = "latex", caption = "Correlation Coefficients for Carbon Pricing Incidence and Expenditure Shares on different Energy Consumption Categories", 
    booktabs = T, align = "l|cccccccc", format.args = list(big.mark = ",", scientific = FALSE), linesep = "", vline = "")%>%
  kable_styling(position = "center", latex_options = c("HOLD_position", "scale_down"))%>%
  column_spec(1, width = "3.5 cm")%>%
  column_spec(2:8, width = "1.5 cm")%>%
  footnote(general = "This table displays correlation coefficients for carbon pricing incidence and expenditure shares on different energy items.", threeparttable = T)%>%
  save_kable(., "../1_Carbon_Pricing_Incidence/3_Analyses/1_LAC_2021/6_App/Latin-America-Paper/Tables/Table_A6/Table_A6.tex")


write.xlsx(data_5.3.1, "../1_Carbon_Pricing_Incidence/3_Analyses/1_LAC_2021/2_Tables/Table_Correlation_Coefficients/Correlation_Energy.xlsx")


P.5.3 <- ggplot(data_5.3, aes(x = Correlation, group = Country))+
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

jpeg("../1_Carbon_Pricing_Incidence/3_Analyses/1_LAC_2021/1_Figures/Figure_7_Correlation_Coefficients_Energy/Figure_7_1.jpg", width = 15.5, height = 12, unit = "cm", res = 400)
print(P.5.3)
dev.off()

t <- ggarrange(P.5.2, P.5.3, common.legend = TRUE, legend = "bottom")

jpeg("../1_Carbon_Pricing_Incidence/3_Analyses/1_LAC_2021/1_Figures/Figure_7_Correlation_Coefficients_Energy/Figure_7_2.jpg", width = 15.5, height = 12, unit = "cm", res = 400)
print(t)
dev.off()


# 5.4   Robustness Check on Indicator for Horizontal Effects ####

data_5.4 <- data.frame()

for (i in seq(0.01, 1,0.01)){
  
  lower_0 <- 0.5 - i/2
  upper_0 <- 0.5 + i/2
  
  data_5.4.1 <- data_joint_0 %>%
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
  
  data_5.4 <- data_5.4 %>%
    bind_rows(data_5.4.1)
}

data_5.4.2 <- data_joint_0 %>%
  filter(Income_Group_5 == 1 | Income_Group_5 == 5)%>%
  group_by(Country, Income_Group_5)%>%
  summarise(median = wtd.quantile(burden_CO2_national, probs = 0.5, weights = hh_weights))%>%
  ungroup()%>%
  pivot_wider(names_from = "Income_Group_5", values_from = median, names_prefix = "median_")%>%
  mutate(AC1 = median_1/median_5)%>%
  select(Country, AC1)

data_5.4.3 <- data_5.4 %>%
  bind_rows(expand_grid(Country = distinct(data_5.4.2, Country)$Country, span = c(-0.1,1.1)))%>%
  left_join(data_5.4.2)%>%
  arrange(Country)%>%
  mutate(interest = ifelse(span == 0.9,"90%",""))

PH1 <- ggplot(data = data_5.4.3)+
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

jpeg("../1_Carbon_Pricing_Incidence/3_Analyses/1_LAC_2021/6_App/Latin-America-Paper/Figures/Figure_A3.jpg", width = 14, height = 15, unit = "cm", res = 400)
print(PH1)
dev.off()

# 6     For Presentation Purposes ####
# 6.1   Figure 2 Boxplot_Flipped ####

Country.Name.6 <- "Brazil"

carbon_pricing_incidence_6 <- read_csv(sprintf("../1_Carbon_Pricing_Incidence/3_Analyses/1_LAC_2021/4_Transformed Data/Carbon_Pricing_Incidence_%s.csv", Country.Name.6))

household_information_6    <- read_csv(sprintf("../1_Carbon_Pricing_Incidence/3_Analyses/1_LAC_2021/4_Transformed Data/household_information_%s_new.csv", Country.Name.6))

carbon_pricing_incidence_6 <- left_join(carbon_pricing_incidence_6, household_information_6)

carbon_pricing_incidence_6.1 <- carbon_pricing_incidence_6 %>%
  group_by(Income_Group_5)%>%
  summarise(
    y5  = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.05),
    y25 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.25),
    y50 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.5),
    y75 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.75),
    y95 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.95),
    mean = wtd.mean(   burden_CO2_national, weights = hh_weights))%>%
  ungroup()%>%
  mutate(Status = ifelse(Income_Group_5 == 1 | Income_Group_5 == 5,1,0))

# Default Y-Axis
ylim0 <- 0.085

P_6.1.1 <- ggplot(carbon_pricing_incidence_6.1, aes(x = factor(Income_Group_5)))+
  geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95), stat = "identity", position = position_dodge(0.5), outlier.shape = NA, width = 0.5, size = 0.3) +
  theme_bw()+
  xlab("Expenditure Quintiles")+ ylab("Carbon Price Incidence")+
  geom_point(aes(y = mean), shape = 23, size = 1.1, stroke = 0.4, fill = "white")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0))+
  scale_x_discrete(labels = c("1", "2", "3", "4", "5"))+
  coord_flip(ylim = c(0,0.062))+
  ggtitle(expression(paste("Incidence of a National Carbon Price (USD 40/t", CO[2], ") in Brazil", sep = "")))+
  theme(axis.text.y = element_text(size = 8), 
        axis.text.x = element_text(size = 8),
        axis.title  = element_text(size = 9),
        plot.title  = element_text(size = 9),
        legend.position = "bottom",
        strip.text = element_text(size = 7),
        strip.text.y = element_text(angle = 180),
        panel.grid.major = element_line(size = 0.3),
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(size = 0.2),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.3,0.3,0.3,0.3), "cm"),
        panel.border = element_rect(size = 0.3))

P_6.1.2 <- ggplot(carbon_pricing_incidence_6.1, aes(x = factor(Income_Group_5)))+
  geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95, fill = factor(Status)), stat = "identity", position = position_dodge(0.5), outlier.shape = NA, width = 0.5, size = 0.3) +
  theme_bw()+
  xlab("Expenditure Quintiles")+ ylab("Carbon Price Incidence")+
  geom_point(aes(y = mean, shape = factor(Status)), size = 1.1, stroke = 0.4, fill = "white")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0))+
  scale_x_discrete(labels = c("1", "2", "3", "4", "5"))+
  coord_flip(ylim = c(0,0.062))+
  ggtitle(expression(paste("Incidence of a National Carbon Price (USD 40/t", CO[2], ") in Brazil", sep = "")))+
  scale_fill_manual(values = c("white", "lightgrey"))+
  guides(fill = "none", shape = "none")+
  scale_shape_manual(values = c(23,22))+
  theme(axis.text.y = element_text(size = 8), 
        axis.text.x = element_text(size = 8),
        axis.title  = element_text(size = 9),
        plot.title  = element_text(size = 9),
        legend.position = "bottom",
        strip.text = element_text(size = 7),
        strip.text.y = element_text(angle = 180),
        panel.grid.major = element_line(size = 0.3),
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(size = 0.2),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.3,0.3,0.3,0.3), "cm"),
        panel.border = element_rect(size = 0.3))

P_6.1.3 <- ggplot(carbon_pricing_incidence_6.1, aes(x = factor(Income_Group_5)))+
  geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95, fill = factor(Status), colour = factor(Status), size = factor(Status)), stat = "identity", position = position_dodge(0.5), outlier.shape = NA, width = 0.5, alpha = 0.5) +
  theme_bw()+
  xlab("Expenditure Quintiles")+ ylab("Carbon Price Incidence")+
  geom_point(aes(y = mean, shape = factor(Status), size = factor(Status), colour = factor(Status)), size = 1.1, stroke = 0.4, fill = "white")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0))+
  scale_x_discrete(labels = c("1", "2", "3", "4", "5"))+
  coord_flip(ylim = c(0,0.062))+
  ggtitle(expression(paste("Incidence of a National Carbon Price (USD 40/t", CO[2], ") in Brazil", sep = "")))+
  scale_fill_manual(values = c("white", "lightgrey"))+
  scale_colour_manual(values = c("black", "#BC3C29FF"))+
  scale_size_manual(values = c(0.3, 0.7))+
  guides(fill = "none", shape = "none", colour = "none", size = "none")+
  scale_shape_manual(values = c(23,22))+
  theme(axis.text.y = element_text(size = 8), 
        axis.text.x = element_text(size = 8),
        axis.title  = element_text(size = 9),
        plot.title  = element_text(size = 9),
        legend.position = "bottom",
        strip.text = element_text(size = 7),
        strip.text.y = element_text(angle = 180),
        panel.grid.major = element_line(size = 0.3),
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(size = 0.2),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.3,0.3,0.3,0.3), "cm"),
        panel.border = element_rect(size = 0.3))


jpeg("../1_Carbon_Pricing_Incidence/3_Analyses/1_LAC_2021/01_Drafts/0_EAERE_2022/Graphics/Distribution_Brazil_%d.jpg", width = 11, height = 10, unit = "cm", res = 400)
print(P_6.1.1)
print(P_6.1.2)
print(P_6.1.3)
dev.off()

# 6.2   Figure 4 Venn-Diagramm ####

carbon_pricing_incidence_6.2 <- carbon_pricing_incidence_6 %>%
  mutate(barrier_0 = wtd.quantile(burden_CO2_national, probs = 0.8, weights = hh_weights))%>%
  mutate(poorest_20_percent  = ifelse(Income_Group_5 == 1,1,0),
         access_to_transfers = ifelse((!is.na(inc_gov_cash)|!is.na(inc_gov_monetary))&(inc_gov_cash > 0 | inc_gov_monetary > 0),1,0),
         most_affected       = ifelse(burden_CO2_national > barrier_0,1,0))%>%
  filter(poorest_20_percent == 1 | most_affected == 1 | access_to_transfers == 1)

carbon_pricing_incidence_6.2.1 <- carbon_pricing_incidence_6.2 %>%
  select(access_to_transfers, poorest_20_percent, most_affected, hh_weights) %>%
  mutate(A = ifelse(most_affected == 1 & access_to_transfers == 0 & poorest_20_percent == 0, hh_weights,0),
         B = ifelse(most_affected == 0 & access_to_transfers == 0 & poorest_20_percent == 1, hh_weights,0),
         C = ifelse(most_affected == 0 & access_to_transfers == 1 & poorest_20_percent == 0, hh_weights,0),
         
         D = ifelse(most_affected == 1 & access_to_transfers == 0 & poorest_20_percent == 1, hh_weights,0),
         E = ifelse(most_affected == 1 & access_to_transfers == 1 & poorest_20_percent == 0, hh_weights,0),
         G = ifelse(most_affected == 0 & access_to_transfers == 1 & poorest_20_percent == 1, hh_weights,0),
         
         H = ifelse(most_affected == 1 & access_to_transfers == 1 & poorest_20_percent == 1, hh_weights,0))%>%
  summarise("Most Affected"       = sum(A),
            "The Poorest"         = sum(B),
            "Access to Transfers" = sum(C),
            "Most Affected&The Poorest"         = sum(D),
            "Most Affected&Access to Transfers" = sum(E),
            "The Poorest&Access to Transfers"   = sum(G),
            "Most Affected&The Poorest&Access to Transfers" = sum(H))

carbon_pricing_incidence_6.2.2 <- c(
    "Most Affected"                                 = carbon_pricing_incidence_6.2.1$'Most Affected',
    "The Poorest"                                   = carbon_pricing_incidence_6.2.1$'The Poorest',
    "Access to Transfers"                           = carbon_pricing_incidence_6.2.1$'Access to Transfers',
    "Most Affected&The Poorest"                     = carbon_pricing_incidence_6.2.1$'Most Affected&The Poorest',
    "Most Affected&Access to Transfers"             = carbon_pricing_incidence_6.2.1$'Most Affected&Access to Transfers',
    "The Poorest&Access to Transfers"               = carbon_pricing_incidence_6.2.1$'The Poorest&Access to Transfers',
    "The Poorest&Access to Transfers&Most Affected" = carbon_pricing_incidence_6.2.1$'Most Affected&The Poorest&Access to Transfers'
  )
  
  P.venn.3 <- plot(euler(carbon_pricing_incidence_6.2.2, shape = "ellipse"), labels = FALSE,
                 quantities = list(type = "percent", fontsize = 8), fills = list(fill = c("#BC3C29FF", "#FFDC91FF", "#6F99ADFF"), alpha = 0.8),
                 main = list(label = "Brazil", fontsize = 7),
                 legend = list(side = "bottom", nrow = 1, ncol = 3, fontsize = 7)
                 )

  carbon_pricing_incidence_6.2.3 <- carbon_pricing_incidence_6 
  
  pop <- sum(carbon_pricing_incidence_6.2.3$hh_weights)
  
  carbon_pricing_incidence_6.2.4 <- rownames_to_column(as.data.frame(carbon_pricing_incidence_6.2.2))%>%
    rename(Type = rowname, value = carbon_pricing_incidence_6.2.2)%>%
    mutate(total = pop)%>%
    mutate(percent = round(value/total,3))%>%
    mutate(label = paste0(percent*100, "%"))
  
  P.venn.3$children$canvas.grob$children$diagram.grob.1$children$tags$children$tag.number.1$children$tag.quantity.1$label <- carbon_pricing_incidence_6.2.4$label[carbon_pricing_incidence_6.2.4$Type == "Most Affected"]
  P.venn.3$children$canvas.grob$children$diagram.grob.1$children$tags$children$tag.number.2$children$tag.quantity.2$label <- carbon_pricing_incidence_6.2.4$label[carbon_pricing_incidence_6.2.4$Type == "The Poorest"]
  P.venn.3$children$canvas.grob$children$diagram.grob.1$children$tags$children$tag.number.3$children$tag.quantity.3$label <- carbon_pricing_incidence_6.2.4$label[carbon_pricing_incidence_6.2.4$Type == "Access to Transfers"]
  P.venn.3$children$canvas.grob$children$diagram.grob.1$children$tags$children$tag.number.4$children$tag.quantity.4$label <- carbon_pricing_incidence_6.2.4$label[carbon_pricing_incidence_6.2.4$Type == "Most Affected&The Poorest"]
  P.venn.3$children$canvas.grob$children$diagram.grob.1$children$tags$children$tag.number.5$children$tag.quantity.5$label <- carbon_pricing_incidence_6.2.4$label[carbon_pricing_incidence_6.2.4$Type == "Most Affected&Access to Transfers"]
  P.venn.3$children$canvas.grob$children$diagram.grob.1$children$tags$children$tag.number.6$children$tag.quantity.6$label <- carbon_pricing_incidence_6.2.4$label[carbon_pricing_incidence_6.2.4$Type == "The Poorest&Access to Transfers"]
  P.venn.3$children$canvas.grob$children$diagram.grob.1$children$tags$children$tag.number.7$children$tag.quantity.7$label <- carbon_pricing_incidence_6.2.4$label[carbon_pricing_incidence_6.2.4$Type == "The Poorest&Access to Transfers&Most Affected"]


jpeg("../1_Carbon_Pricing_Incidence/3_Analyses/1_LAC_2021/01_Drafts/0_EAERE_2022/Graphics/Venn_Brazil_%d.jpg", width = 11, height = 10, unit = "cm", res = 400)
print(P.venn.3)
dev.off()

