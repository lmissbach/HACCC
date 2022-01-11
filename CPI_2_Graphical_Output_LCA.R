# 0     General ####

# Author: L. Missbach, missbach@mcc-berlin.net

# 0.1   Packages ####

library("cowplot")
library("eulerr")
library("ggpubr")
library("ggsci")
library("haven")
library("Hmisc")
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
                      "El_Salvador", "Guatemala", "Mexico", "Nicaragua", "Peru", "Uruguay")) {
  
  carbon_pricing_incidence_0 <- read_csv(sprintf("../1_Carbon_Pricing_Incidence/3_Analyses/1_LAC_2021/4_Transformed Data/Carbon_Pricing_Incidence_%s.csv", Country.Name))
  
  household_information_0    <- read_csv(sprintf("../1_Carbon_Pricing_Incidence/3_Analyses/1_LAC_2021/4_Transformed Data/household_information_%s_new.csv", Country.Name))
  
  if(Country.Name == "El_Salvador") Country.Name.2 <- "El Salvador" else Country.Name.2 <- Country.Name
  
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
  
  data_joint_0 <- data_joint_0 %>%
    bind_rows(carbon_pricing_incidence_1)
}

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
  
  P_2 <- ggplot(data_0, aes(x = factor(Income_Group_5)))+
    geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95), stat = "identity", position = position_dodge(0.5), outlier.shape = NA, width = 0.5, size = 0.3) +
    theme_bw()+
    facet_wrap(.~Country, nrow = 3)+
    xlab(XLAB)+ ylab(YLAB)+
    geom_point(aes(y = mean), shape = 23, size = 1.1, stroke = 0.4, fill = "white")+
    scale_y_continuous(labels = scales::percent_format(accuracy = accuracy_0), expand = c(0,0))+
    scale_x_discrete(labels = c("1", "2", "3", "4", "5"))+
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

P_4.1.1 <- plot_figure_2()

jpeg("../1_Carbon_Pricing_Incidence/3_Analyses/1_LAC_2021/1_Figures/Figure_2_Boxplot/National_Carbon_Price_Figure_2_Joint.jpg", width = 15.5, height = 15, unit = "cm", res = 400)
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
    facet_wrap(.~Country, nrow = 3)+
    xlab(XLAB)+ ylab(YLAB)+
    geom_point(aes(y = mean), shape = 23, size = 1.1, stroke = 0.4, fill = "white")+
    scale_y_continuous(labels = scales::percent_format(accuracy = accuracy_0), expand = c(0,0))+
    scale_x_discrete(labels = c("1", "2", "3", "4", "5"))+
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
  
  return(P_3)
}

P_4.1.2 <- plot_figure_3()

jpeg("../1_Carbon_Pricing_Incidence/3_Analyses/1_LAC_2021/1_Figures/Figure_2_Boxplot/Fossil_Fuel_Subsidy_Reform_Figure_3_Joint.jpg", width = 15.5, height = 15, unit = "cm", res = 400)
print(P_4.1.2)
dev.off()

# 4.2   Carbon Footprints over Household Expenditures ####

P_4.2 <- ggplot(data_joint_0)+
  geom_smooth(formula = y ~ x, aes(y = CO2_t_national, x = hh_expenditures_USD_2014), method = "lm", se = FALSE, colour = "black", size = 0.6, fullrange = TRUE)+
  geom_point(aes(y = CO2_t_national, x = hh_expenditures_USD_2014, fill = Country), colour = "black", shape = 21, alpha = 0.2, size = 0.8)+
  facet_wrap(.~Country, nrow = 3)+
  coord_cartesian(xlim = c(0,99000), ylim = c(0,50))+
  guides(fill = "none", colour = "none")+
  theme_bw()+
  ylab("Carbon Footprint in tCO2")+
  xlab("Total Household Expenditures (USD)")+
  scale_x_continuous(labels = scales::unit_format(unit = "", scale = 1/1000), expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))+
  ggtitle("Carbon Footprint of Consumption")+
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
        plot.margin = unit(c(0.1,0.1,0,0), "cm"),
        panel.border = element_rect(size = 0.3))

jpeg("../1_Carbon_Pricing_Incidence/3_Analyses/1_LAC_2021/1_Figures/Figure_0_Carbon_Footprints/Figure_0_Joint.jpg", width = 15.5, height = 15, unit = "cm", res = 400)
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

data_Mexico_3.1 <- c("Most Affected" = 4181882,
                   "The Poorest"   = 3030246,
                   "Access to Transfers" = 7322681,
                   "Most Affected&The Poorest" = 759395,
                   "Most Affected&Access to Transfers" = 1659255,
                   "The Poorest&Access to Transfers"   = 2811277,
                   "The Poorest&Access to Transfers&Most Affected" = 542196)

data_Mexico_3 <- c("Most Affected" = 11157,
                   "The Poorest"   = 7862,
                   "Access to Transfers" = 18481,
                   "Most Affected&The Poorest" = 2268,
                   "Most Affected&Access to Transfers" = 4929,
                   "The Poorest&Access to Transfers"   = 7758,
                   "The Poorest&Access to Transfers&Most Affected" = 1780)



plot(euler(data_Mexico_3, shape = "ellipse"), fill = c("#BC3C29FF", "#FFDC91FF", "#6F99ADFF"), quantities = TRUE)

plot(euler(data_Mexico_1, shape = "ellipse"), quantities = TRUE, weights = weigths_Mexico)
plot(venn(data_Mexico_1))
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

data_Brazil_3.1 <- c("Most Affected" = 7193562,
                     "The Poorest"   = 4874763,
                     "Access to Transfers" = 12089309,
                     "Most Affected&The Poorest" = 2664433,
                     "Most Affected&Access to Transfers" = 1916138,
                     "The Poorest&Access to Transfers"   = 4229206,
                     "The Poorest&Access to Transfers&Most Affected" = 1994806)

data_Mexico_3 <- c("Most Affected" = 11157,
                   "The Poorest"   = 7862,
                   "Access to Transfers" = 18481,
                   "Most Affected&The Poorest" = 2268,
                   "Most Affected&Access to Transfers" = 4929,
                   "The Poorest&Access to Transfers"   = 7758,
                   "The Poorest&Access to Transfers&Most Affected" = 1780)



plot(euler(data_Mexico_3, shape = "ellipse"), fill = c("#BC3C29FF", "#FFDC91FF", "#6F99ADFF"), quantities = TRUE)

plot(euler(data_Mexico_1, shape = "ellipse"), quantities = TRUE, weights = weigths_Mexico)
plot(venn(data_Mexico_1))

P.4.3 <- plot(euler(data_Brazil_3.1, shape = "ellipse"), quantities = TRUE, fill = c("#BC3C29FF", "#FFDC91FF", "#6F99ADFF"),
              main = "Typology of Brazilian Households")

jpeg("../1_Carbon_Pricing_Incidence/3_Analyses/1_LAC_2021/1_Figures/Figure_4_Euler_Diagrams/Figure_4_Brazil.jpg", width = 15.5, height = 12, unit = "cm", res = 400)
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



# 4.X Summary Statistics

data_4.X <- data_joint_0 %>%
  mutate(population = hh_size*hh_weights)%>%
  group_by(Country)%>%
  summarise(number = n(),
            population = sum(population))%>%
  ungroup()

t <- read_dta("../0_Data/1_Household Data/3_Chile/1_Data_Clean/LAC_Clean/CHL_EPF_2016-2017.dta")
