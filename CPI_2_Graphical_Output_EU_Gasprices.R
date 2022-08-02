# 0   General ####

# Author: L. Missbach, missbach@mcc-berlin.net

# 0.1 Packages ####

library("boot")
library("cowplot")
library("ggpubr")
library("ggsci")
library("haven")
library("Hmisc")
library("openxlsx")
library("rattle")
library("scales")
library("tidyverse")
options(scipen=999)

# 1   Loading Data ####

Country.Name <- "Europe"

household_information_0    <- read_csv("K:/WorkInProgress/2021_Carbon_Footprint_Analysis/Data_Transformed/household_information_Europe_new.csv")
carbon_pricing_incidence_0 <- read_csv("K:/WorkInProgress/2021_Carbon_Footprint_Analysis/Data_Transformed/Gas_Price_Incidence_Europe.csv")
indirect_expenditures_0    <- read_csv("K:/WorkInProgress/2021_Carbon_Footprint_Analysis/Data_Transformed/Indirect_Effects_Europe.csv")

# 2   Graphics Individual ####

carbon_pricing_incidence_1 <- left_join(carbon_pricing_incidence_0, household_information_0)

indirect_expenditures_1 <- left_join(indirect_expenditures_0, household_information_0)

# 2.1 Boxplots in Europe (CO2-Price in Gas-Sector) ####

plot_figure_2 <- function(ATT  = element_text(size = 7), ATX = element_text(size = 7), ATY = element_text(size = 7),
                          XLAB = "Expenditure Quintiles",
                          YLAB = "Gas Price Incidence", 
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

carbon_pricing_incidence_2.2 <- carbon_pricing_incidence_1 %>%
  group_by(Income_Group_5, Country)%>%
  summarise(
    y5  = wtd.quantile(burden_CO2_gas, weights = hh_weights, probs = 0.05),
    y25 = wtd.quantile(burden_CO2_gas, weights = hh_weights, probs = 0.25),
    y50 = wtd.quantile(burden_CO2_gas, weights = hh_weights, probs = 0.5),
    y75 = wtd.quantile(burden_CO2_gas, weights = hh_weights, probs = 0.75),
    y95 = wtd.quantile(burden_CO2_gas, weights = hh_weights, probs = 0.95),
    mean = wtd.mean(   burden_CO2_gas, weights = hh_weights))%>%
  ungroup()

for(i in c("Belgium", "Bulgaria", "Cyprus", "Czech Republic", "Germany", "Denmark", "Estonia", "Greece", "Spain", "Finland",
           "France", "Croatia", "Hungary" ,"Ireland", "Italy", "Lithuania", "Luxembourg", "Latvia", "Netherlands", "Poland",
           "Portugal", "Romania", "Sweden", "Slovak Republic")){
  carbon_pricing_incidence_2.2.1 <- carbon_pricing_incidence_2.2 %>%
    filter(Country == i)
  
  ylim0 <- 0.2
  
  P_2 <- plot_figure_2(data_0 = carbon_pricing_incidence_2.2.1, title_0 = i)
  
  # jpeg(sprintf("K:/WorkInProgress/2021_Carbon_Footprint_Analysis/Gas Price Analysis/Figures/Figure_2_Boxplot_National_Carbon_Price/Figure_2_%s.jpg", i), width = 6, height = 6, unit = "cm", res = 400)
  # print(P_2)
  # dev.off()
  
  
}

P_3 <- ggplot(carbon_pricing_incidence_2.2, aes(x = factor(Income_Group_5)))+
  geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95), stat = "identity", position = position_dodge(0.5), outlier.shape = NA, width = 0.5, size = 0.3) +
  theme_bw()+
  xlab("Expenditure Quintiles")+ ylab("Gas Price Incidence")+
  facet_wrap(. ~ Country)+
  geom_point(aes(y = mean), shape = 23, size = 1.3, stroke = 0.2, fill = "white")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0))+
  scale_x_discrete(labels = c("1 \n Poorest \n 20 Percent", "2", "3", "4", "5 \n Richest \n 20 Percent"))+
  coord_cartesian(ylim = c(0,0.3))+
  ggtitle("")+
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
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.1,0.1,0,0), "cm"),
        panel.border = element_rect(size = 0.3))

# jpeg("K:/WorkInProgress/2021_Carbon_Footprint_Analysis/Gas Price Analysis/Figures/Figure_2_Boxplot_National_Carbon_Price/Figure_2_Europe.jpg", width = 30, height = 15, unit = "cm", res = 400)
# print(P_3)
# dev.off()
  
# 2.2 Vertical Distribution across Instruments ####

plot_figure_3 <- function(ATT  = element_text(size = 7), ATX = element_text(size = 7), ATY = element_text(size = 7),
                          XLAB = "Expenditure Quintiles",
                          YLAB = "Gas Price Incidence", 
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
    scale_colour_npg(  labels = c("Gas Price Increase","Gas Price Increase (Direct)", "Gas Price Increase (Indirect)")) +
    scale_fill_npg  (  labels = c("Gas Price Increase","Gas Price Increase (Direct)", "Gas Price Increase (Indirect)"))+
    scale_shape_manual(labels = c("Gas Price Increase","Gas Price Increase (Direct)", "Gas Price Increase (Indirect)"), values = c(21,22,23))+
    scale_alpha_manual(labels = c("Gas Price Increase","Gas Price Increase (Direct)", "Gas Price Increase (Indirect)"), values = c(1,1,1))+
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


# 2.3 Vertical Distribution Across Instruments in Europe ####

carbon_pricing_incidence_2.3.0 <- data.frame()

carbon_pricing_incidence_2.3 <- carbon_pricing_incidence_1 %>%
  group_by(Income_Group_5, Country)%>%
  summarise(
    wtd.median_CO2_gas          = wtd.quantile(burden_CO2_gas,          weight = hh_weights, probs = 0.5),
    wtd.median_CO2_gas_direct   = wtd.quantile(burden_CO2_gas_direct,   weight = hh_weights, probs = 0.5),
    wtd.median_CO2_gas_indirect = wtd.quantile(burden_CO2_gas_indirect, weight = hh_weights, probs = 0.5)
  )%>%
  ungroup()

for(i in c("Belgium", "Bulgaria", "Cyprus", "Czech Republic", "Germany", "Denmark", "Estonia", "Greece", "Spain", "Finland",
           "France", "Croatia", "Hungary" ,"Ireland", "Italy", "Lithuania", "Luxembourg", "Latvia", "Netherlands", "Poland",
           "Portugal", "Romania", "Sweden", "Slovak Republic")){
  
  carbon_pricing_incidence_2.3.1 <- carbon_pricing_incidence_2.3 %>%
    filter(Country == i)
  
  carbon_pricing_incidence_2.3.1 <- carbon_pricing_incidence_2.3.1 %>%
    mutate(CO2_gas          = wtd.median_CO2_gas  /carbon_pricing_incidence_2.3.1$wtd.median_CO2_gas[1],
           CO2_gas_direct   = wtd.median_CO2_gas_direct/carbon_pricing_incidence_2.3.1$wtd.median_CO2_gas_direct[1],
           CO2_gas_indirect = wtd.median_CO2_gas_indirect/carbon_pricing_incidence_2.3.1$wtd.median_CO2_gas_indirect[1])%>%
    select(-starts_with("wtd."))%>%
    pivot_longer(CO2_gas:CO2_gas_indirect, names_to = "type", values_to = "Value")%>%
    unite(help, c("type", "Income_Group_5", "Country"), sep = "_", remove = FALSE)
  
  carbon_pricing_incidence_2.3.0 <- carbon_pricing_incidence_2.3.0 %>%
    bind_rows(carbon_pricing_incidence_2.3.1)
}

for(i in c("Belgium", "Bulgaria", "Cyprus", "Czech Republic", "Germany", "Denmark", "Estonia", "Greece", "Spain", "Finland",
           "France", "Croatia", "Hungary" ,"Ireland", "Italy", "Lithuania", "Luxembourg", "Latvia", "Netherlands", "Poland",
           "Portugal", "Romania", "Sweden", "Slovak Republic")){
  carbon_pricing_incidence_2.3.2 <- carbon_pricing_incidence_2.3.0 %>%
    filter(Country == i)
  
  P_3 <- plot_figure_3(data_0 = carbon_pricing_incidence_2.3.2, title_0 = i)
  
  # jpeg(sprintf("K:/WorkInProgress/2021_Carbon_Footprint_Analysis/Gas Price Analysis/Figures/Figure_3_Vertical_Effects/Figure_3_%s.jpg", i), width = 6, height = 6, unit = "cm", res = 400)
  # print(P_3)
  # dev.off()
  
  
}

P_3.1 <- ggplot(carbon_pricing_incidence_2.3.0, aes(x = factor(Income_Group_5)))+
  geom_hline(yintercept = 1, colour = "black", size = 0.3)+
  #geom_ribbon(aes(ymin = low, ymax = upper, group = type, fill = type), alpha = 0.2)+
  #geom_label_repel(aes(y = 1,    group = type,  label = label),   size = 1.6, segment.linetype = 1, segment.size = 0.1, box.padding = 0.00, label.padding = 0.10, label.r = 0.05, direction = "y", min.segment.length = 0, nudge_y = nudge_0)+
  #geom_label_repel(aes(y = 3,    group = type,  label = Label_2), size = 1.6, segment.linetype = 1, segment.size = 0.1, box.padding = 0.00, label.padding = 0.10, label.r = 0.05, direction = "y", min.segment.length = 0, nudge_y = -0.6)+
  #geom_label_repel(aes(y = pure, group = type, segment.linetype = 1, label = label_emissions_coverage, segment.size = 1, size = 15), min.segment.length = 0, hjust = 1, force_pull = 0, nudge_x = 1)+
  geom_line(aes( y = Value, group = type, colour = type, alpha = type), size = 0.4, position = position_dodge(0.2))+
  geom_point(aes(y = Value, group = type, fill = type, shape = type, alpha = type), size = 1.5, colour = "black", position = position_dodge(0.2), stroke = 0.2)+
  scale_colour_npg(  labels = c("Gas Price Increase","Gas Price Increase (Direct)", "Gas Price Increase (Indirect)")) +
  scale_fill_npg  (  labels = c("Gas Price Increase","Gas Price Increase (Direct)", "Gas Price Increase (Indirect)"))+
  scale_shape_manual(labels = c("Gas Price Increase","Gas Price Increase (Direct)", "Gas Price Increase (Indirect)"), values = c(21,22,23))+
  scale_alpha_manual(labels = c("Gas Price Increase","Gas Price Increase (Direct)", "Gas Price Increase (Indirect)"), values = c(1,1,1))+
  labs(fill = "", colour = "", shape = "", alpha = "", linetype = "")+
  facet_wrap(. ~ Country)+
  theme_bw() + 
  scale_x_discrete(labels = c("1","2","3","4","5"))+
  #scale_y_continuous(breaks = seq(limit_low, limit_up, step_0))+
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
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.1,0.1,0,0), "cm"),
        panel.border = element_rect(size = 0.3))+
  coord_cartesian(ylim = c(0.0,2))+
  guides(fill = guide_legend(nrow = 2, order = 1), colour = guide_legend(nrow = 2, order = 1), shape = guide_legend(nrow = 2, order = 1), alpha = FALSE, size = FALSE)+
  #guides(fill = fill0, colour = fill0, shape = fill0, size = fill0, alpha = fill0)+
  xlab("Expenditure Quintiles")+
  ylab("Gas Price Incidence")+ 
  ggtitle("")

# jpeg("K:/WorkInProgress/2021_Carbon_Footprint_Analysis/Gas Price Analysis/Figures/Figure_3_Vertical_Effects/Figure_3_Europe.jpg", width = 30, height = 15, unit = "cm", res = 400)
# print(P_3.1)
# dev.off()
  

# 2.4 Tracking Gas Expenditures ####

carbon_pricing_incidence_2.4 <- carbon_pricing_incidence_1 %>%
  mutate(exp_share_Gas = ifelse(!is.na(exp_LCU_Gas), exp_LCU_Gas/hh_expenditures, 0))%>%
  group_by(Income_Group_10, Country)%>%
  summarise(mean_exp_share_GAS = wtd.mean(exp_share_Gas, hh_weights))%>%
  ungroup()

P.2.4 <- ggplot(carbon_pricing_incidence_2.4, aes(x = factor(Income_Group_10), y = mean_exp_share_GAS, group = Country))+
  facet_wrap(. ~ Country)+
  geom_point()+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0), breaks = c(0,0.02, 0.04, 0.06))+
  coord_cartesian(ylim = c(0,0.07))+
  ylab("Gas Expenditure Share")+
  xlab("Expenditure Decile")+
  ggtitle("")+
  theme_bw()+
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
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.1,0.1,0,0), "cm"),
        panel.border = element_rect(size = 0.3))

# jpeg("K:/WorkInProgress/2021_Carbon_Footprint_Analysis/Gas Price Analysis/Figures/Figure_1_Expenditure_Shares_Gas/Figure_1_Europe.jpg", width = 30, height = 15, unit = "cm", res = 400)
# print(P.2.4)
# dev.off()

# 3.1 Via Gas Expenditures ####

carbon_pricing_incidence_3.1 <- carbon_pricing_incidence_1 %>%
  group_by(Income_Group_5, Country)%>%
  summarise(
    y5  = wtd.quantile(burden_GAS_gas, weights = hh_weights, probs = 0.05),
    y25 = wtd.quantile(burden_GAS_gas, weights = hh_weights, probs = 0.25),
    y50 = wtd.quantile(burden_GAS_gas, weights = hh_weights, probs = 0.5),
    y75 = wtd.quantile(burden_GAS_gas, weights = hh_weights, probs = 0.75),
    y95 = wtd.quantile(burden_GAS_gas, weights = hh_weights, probs = 0.95),
    mean = wtd.mean(   burden_GAS_gas, weights = hh_weights))%>%
  ungroup()

P_3.1 <- ggplot(carbon_pricing_incidence_3.1, aes(x = factor(Income_Group_5)))+
  geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95), stat = "identity", position = position_dodge(0.5), outlier.shape = NA, width = 0.5, size = 0.3) +
  theme_bw()+
  xlab("Expenditure Quintiles")+ ylab("Gas Price Incidence")+
  facet_wrap(. ~ Country)+
  geom_point(aes(y = mean), shape = 23, size = 1.3, stroke = 0.2, fill = "white")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0))+
  scale_x_discrete(labels = c("1", "2", "3", "4", "5"))+
  coord_cartesian(ylim = c(0,0.47))+
  ggtitle("")+
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
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.1,0.1,0,0), "cm"),
        panel.border = element_rect(size = 0.3))

# jpeg("K:/WorkInProgress/2021_Carbon_Footprint_Analysis/Gas Price Analysis/Figures/Figure_4_Boxplot_International_Gas_Price_Increase/Figure_4_Europe.jpg", width = 30, height = 15, unit = "cm", res = 400)
# print(P_3.1)
# dev.off()

jpeg("C:/Users/misl/ownCloud/EU_Gasprice_Analysis/Figures/2022_04_19_Figures/Figure_5.jpg", width = 15, height = 25, unit = "cm", res = 400)
print(P_3.1)
dev.off()

# 3.2 Via Gas Expenditures: Direct / Indirect ####


carbon_pricing_incidence_3.2.1 <- carbon_pricing_incidence_1 %>%
  group_by(Income_Group_5, Country)%>%
  summarise(
    y5  = wtd.quantile(burden_GAS_direct, weights = hh_weights, probs = 0.05),
    y25 = wtd.quantile(burden_GAS_direct, weights = hh_weights, probs = 0.25),
    y50 = wtd.quantile(burden_GAS_direct, weights = hh_weights, probs = 0.5),
    y75 = wtd.quantile(burden_GAS_direct, weights = hh_weights, probs = 0.75),
    y95 = wtd.quantile(burden_GAS_direct, weights = hh_weights, probs = 0.95),
    mean = wtd.mean(   burden_GAS_direct, weights = hh_weights))%>%
  ungroup()%>%
  mutate(Type = "Direct Effects")

# jpeg("K:/WorkInProgress/2021_Carbon_Footprint_Analysis/Gas Price Analysis/Figures/Figure_4_Boxplot_International_Gas_Price_Increase/Figure_4_Europe.jpg", width = 30, height = 15, unit = "cm", res = 400)
# print(P_3.1)
# dev.off()

carbon_pricing_incidence_3.2.2 <- carbon_pricing_incidence_1 %>%
  group_by(Income_Group_5, Country)%>%
  summarise(
    y5  = wtd.quantile(burden_GAS_indirect, weights = hh_weights, probs = 0.05),
    y25 = wtd.quantile(burden_GAS_indirect, weights = hh_weights, probs = 0.25),
    y50 = wtd.quantile(burden_GAS_indirect, weights = hh_weights, probs = 0.5),
    y75 = wtd.quantile(burden_GAS_indirect, weights = hh_weights, probs = 0.75),
    y95 = wtd.quantile(burden_GAS_indirect, weights = hh_weights, probs = 0.95),
    mean = wtd.mean(   burden_GAS_indirect, weights = hh_weights))%>%
  ungroup()%>%
  mutate(Type = "Indirect Effects")

carbon_pricing_incidence_3.2.3 <- bind_rows(carbon_pricing_incidence_3.2.1,
                                            carbon_pricing_incidence_3.2.2)

P_3.2 <- ggplot(carbon_pricing_incidence_3.2.3, aes(x = factor(Income_Group_5)))+
  geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95, fill = Type), stat = "identity", position = position_dodge(0.7), outlier.shape = NA, width = 0.5, size = 0.3) +
  theme_bw()+
  xlab("Expenditure Quintiles")+ ylab("Gas Price Incidence")+
  facet_wrap(. ~ Country)+
  stat_summary(aes(y = mean, group = interaction(Income_Group_5, Type)), fun = "mean", geom = "point", position =  position_dodge(0.7), shape = 23, size = 1.3, stroke = 0.5, fill = "white")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0))+
  scale_x_discrete(labels = c("1 \n Poorest \n 20 Percent", "2", "3", "4", "5 \n Richest \n 20 Percent"))+
  coord_cartesian(ylim = c(0,0.301))+
  scale_fill_nejm()+
  ggtitle("")+
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
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.1,0.1,0,0), "cm"),
        panel.border = element_rect(size = 0.3))

# jpeg("K:/WorkInProgress/2021_Carbon_Footprint_Analysis/Gas Price Analysis/Figures/Figure_5_Gas_Price_Increase_Direct_vs_Indirect_Effects/Figure_5_Europe.jpg", width = 30, height = 15, unit = "cm", res = 400)
# print(P_3.2)
# dev.off()

# 3.3 Burden Decile per Country ####

carbon_pricing_incidence_3.3 <- carbon_pricing_incidence_1 %>%
  mutate(Burden_Decile = as.numeric(binning(burden_GAS_gas, bins = 10, method = c("wtd.quantile"), weights = hh_weights)))%>%
  group_by(Burden_Decile, Country)%>%
  summarise(hh_weights = sum(hh_weights))%>%
  ungroup()%>%
  group_by(Burden_Decile)%>%
  mutate(hh_weights_Decile = sum(hh_weights))%>%
  ungroup()%>%
  mutate(share = hh_weights/hh_weights_Decile)

carbon_pricing_incidence_3.3.1 <- carbon_pricing_incidence_1 %>%
  group_by(Country)%>%
  summarise(hh_expenditures_mean = wtd.mean(hh_expenditures, hh_weights))%>%
  ungroup()%>%
  mutate(hh_expenditures_mean = ifelse(Country == "Luxembourg", 34999,hh_expenditures_mean))

carbon_pricing_incidence_3.3.2 <- left_join(carbon_pricing_incidence_3.3, carbon_pricing_incidence_3.3.1)%>%
  arrange(Burden_Decile, hh_expenditures_mean)

carbon_pricing_incidence_3.3.2$Country_factor <- factor(carbon_pricing_incidence_3.3.2$Country,
                                                           levels = c("Romania", "Bulgaria", "Hungary", "Lithuania", "Latvia",
                                                                      "Czech Republic", "Estonia", "Croatia", "Poland", "Slovak Republic",
                                                                      "Portugal", "Greece", "Spain", "Italy", "France", "Cyprus",
                                                                      "Germany", "Sweden", "Belgium", "Finland", "Ireland",
                                                                      "Netherlands", "Denmark", "Luxembourg"))

table <- expand_grid(Country_factor = carbon_pricing_incidence_3.3.2$Country_factor, Burden_Decile = c(1:10))%>%
  distinct()%>%
  left_join(select(carbon_pricing_incidence_3.3.2, Burden_Decile, Country_factor, share))%>%
  pivot_wider(names_from = "Burden_Decile", values_from = "share")%>%
  arrange(Country_factor)

# write.xlsx(table, "K:/WorkInProgress/2021_Carbon_Footprint_Analysis/Gas Price Analysis/Figures/Figure_6_Gas_Price_Burden_Deciles/Burden_Deciles_Country.xlsx")

P_3.3.1 <- ggplot(data = carbon_pricing_incidence_3.3.2, aes(x = factor(Burden_Decile), y = share))+
  geom_col(aes(fill = hh_expenditures_mean), colour = "black", size = 0.75)+
  theme_bw()+
  xlab("Burden Deciles")+ ylab("Share of households")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0))+
  scale_fill_viridis_c(limits = c(4000,35000), labels = scales::label_dollar(prefix = "€"), breaks = c(5000,15000,25000))+
  labs(fill = "Average Household Expenditures per Country")+
  coord_cartesian(ylim = c(0,1.0))+
  ggtitle("")+
  theme(axis.text.y = element_text(size = 7), 
        axis.text.x = element_text(size = 7),
        axis.title  = element_text(size = 7),
        plot.title = element_text(size = 7),
        legend.position = "right",
        strip.text = element_text(size = 7),
        strip.text.y = element_text(angle = 180),
        panel.grid.major = element_line(size = 0.3),
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(size = 0.2),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.1,0.1,0,0), "cm"),
        panel.border = element_rect(size = 0.3))

library(RColorBrewer)

colours <- colorRampPalette(brewer.pal(8, "YlOrRd"))(24)

P_3.3.2 <- ggplot(data = carbon_pricing_incidence_3.3.2, aes(x = factor(Burden_Decile), y = share))+
  geom_col(aes(fill = fct_reorder(Country_factor, desc(hh_expenditures_mean))), colour = "black", width = 0.75)+
  theme_bw()+
  xlab("Burden Deciles")+ ylab("Share of households")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0))+
  scale_fill_manual(values = colours, guide = guide_legend(reverse = TRUE))+
  labs(fill = "Country")+
  coord_cartesian(ylim = c(0,1.0))+
  ggtitle("")+
  theme(axis.text.y = element_text(size = 7), 
        axis.text.x = element_text(size = 7),
        axis.title  = element_text(size = 7),
        plot.title = element_text(size = 7),
        legend.position = "right",
        strip.text = element_text(size = 7),
        strip.text.y = element_text(angle = 180),
        panel.grid.major = element_line(size = 0.3),
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(size = 0.2),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.1,0.1,0,0), "cm"),
        panel.border = element_rect(size = 0.3))



# 3.3.1 Burden Decile segregated ####

carbon_pricing_incidence_3.3.3 <- carbon_pricing_incidence_1 %>%
  mutate(Burden_Decile = as.numeric(binning(burden_GAS_gas, bins = 10, method = c("wtd.quantile"), weights = hh_weights)))%>%
  mutate(hh_expenditures_round = round(hh_expenditures,-1))%>%
  mutate(hh_expenditures_round = ifelse(hh_expenditures_round > 50000, 50000, hh_expenditures_round))%>%
  group_by(Burden_Decile, hh_expenditures_round)%>%
  summarise(hh_weights = sum(hh_weights))%>%
  ungroup()%>%
  group_by(Burden_Decile)%>%
  mutate(hh_weights_Decile = sum(hh_weights))%>%
  ungroup()%>%
  mutate(share = hh_weights/hh_weights_Decile)

P_3.3.3 <- ggplot(data = carbon_pricing_incidence_3.3.3, aes(x = factor(Burden_Decile), y = share))+
  geom_col(aes(fill = hh_expenditures_round), size = 0.75)+
  theme_bw()+
  xlab("Burden Deciles")+ ylab("Share of households")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0))+
  scale_fill_viridis_c(limits = c(100,50001), labels = scales::label_dollar(prefix = "€"), breaks = c(5000,25000,50000))+
  scale_fill_gradient(low = "black", high = "white")+
  labs(fill = "Household Expenditures")+
  coord_cartesian(ylim = c(0,1.0))+
  ggtitle("")+
  theme(axis.text.y = element_text(size = 7), 
        axis.text.x = element_text(size = 7),
        axis.title  = element_text(size = 7),
        plot.title = element_text(size = 7),
        legend.position = "right",
        strip.text = element_text(size = 7),
        strip.text.y = element_text(angle = 180),
        panel.grid.major = element_line(size = 0.3),
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(size = 0.2),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.1,0.1,0,0), "cm"),
        panel.border = element_rect(size = 0.3))

# jpeg("K:/WorkInProgress/2021_Carbon_Footprint_Analysis/Gas Price Analysis/Figures/Figure_6_Gas_Price_Burden_Deciles/Figure_6_Europe_%d.jpg", width = 30, height = 15, unit = "cm", res = 400)
# print(P_3.3.1)
# print(P_3.3.2)
# print(P_3.3.3)
# 
# dev.off()

# 4.  Joint Figures ####

# 5.  Econometric Analysis ####
# 5.1 Inequality Decomposition ####

# Country-Level

data_5.1.0 <- data.frame()
decomposition_Fields <- list()

for(i in c("Belgium", "Bulgaria", "Cyprus", "Czech Republic", "Germany", "Denmark", "Estonia", "Greece", "Spain", "Finland",
           "France", "Croatia", "Hungary" ,"Ireland", "Italy", "Lithuania", "Luxembourg", "Latvia", "Netherlands", "Poland",
           "Portugal", "Romania", "Sweden", "Slovak Republic")){
  
  data_5.1.1 <- carbon_pricing_incidence_1 %>%
    filter(Country == i)%>%
    mutate(Income_Group_5 = as.character(Income_Group_5))
  
  data_5.1.2 <- carbon_pricing_incidence_1 %>%
    filter(Country == i)%>%
    mutate(Income_Group_5 = "Full Sample")
  
  data_5.1.3 <- bind_rows(data_5.1.1, data_5.1.2)%>%
    mutate(log_hh_expenditures = log(hh_expenditures))%>%
    mutate_at(vars(hh_type, main_source_income, edu_hhh), list(~ as.character(.)))%>%
    mutate(edu_hhh = ifelse((Country == "Denmark" | Country == "Hungary" | Country == "Sweden") & is.na(edu_hhh), 9, edu_hhh))
  
  df_5.1.4 <- data.frame()
  
  for(j in c(1,2,3,4,5, "Full Sample")){
    data_5.1.5 <- data_5.1.3 %>%
      filter(Income_Group_5 == j)
    
    variance_incidence <- wtd.var(data_5.1.5$burden_GAS_gas, weights = data_5.1.5$hh_weights)
    
    formula_0 <- "burden_GAS_gas ~ log_hh_expenditures + urban_01"
    
    if(nrow(distinct(data_5.1.5, district))>1) formula_0 <- paste0(formula_0, " + district")
    if(nrow(distinct(data_5.1.5, edu_hhh))>1)  formula_0 <- paste0(formula_0, " + edu_hhh")
    if(nrow(distinct(data_5.1.5, hh_type))>1)  formula_0 <- paste0(formula_0, " + hh_type")
    if(nrow(distinct(data_5.1.5, main_source_income))>1)  formula_0 <- paste0(formula_0, " + main_source_income")
    
    formula_1 <- as.formula(formula_0)
    
    model_5.1 <- lm(formula_1, data = data_5.1.5, weights = hh_weights)
    
    prediction_5.1 <- as.data.frame(predict.lm(model_5.1, data_5.1.5, type = "terms"))%>%
      mutate(residuals = resid(model_5.1))
    
    correlations <- sapply(prediction_5.1, function(.) corr(d = cbind(., data_5.1.5$burden_GAS_gas), w = data_5.1.5$hh_weights))
    variance     <- sapply(prediction_5.1, function(x) wtd.var(x,                                   weights = data_5.1.5$hh_weights))
    
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
             R_squared = summary(model_5.1)$r.squared,
             p_j = ifelse(factor != "residuals", s_j/R_squared,NA))%>%
      select(factor, Income_Group_5, p_j)%>%
      rename(s_k = p_j)%>%
      mutate(factor = ifelse(factor == "hh_size", "HH Size",
                             ifelse(factor == "car.01", "Car Ownership",
                                    ifelse(factor == "refrigerator.01", "Refrigerator Own.",
                                           ifelse(factor == "urban_01", "Urban Area",
                                                  ifelse(factor == "log_hh_expenditures", "HH Exp. (log)", 
                                                         ifelse(factor == "CF", "Cooking Fuel",
                                                                ifelse(factor == "edu_hhh", "Education",
                                                                       ifelse(factor == "factor(Ethnicity)", "Ethnicity",
                                                                              ifelse(factor == "residuals", factor, 
                                                                                     ifelse(factor == "district", "District",
                                                                                            ifelse(factor == "main_source_income", "Main Source of Income", 
                                                                                                   ifelse(factor == "hh_type", "HH Type", factor)))))))))))))%>%
      rename("Sample:" = factor)
    
    df_5.1.4 <- df_5.1.4 %>%
      bind_rows(joined_1)
  }
  
  df_5.1.4 <- df_5.1.4 %>%
    pivot_wider(names_from = "Income_Group_5", values_from = "s_k")%>%
    mutate_at(vars(-"Sample:"), list(~ round(.,3)))%>%
    filter("Sample:" != "residuals")%>%
    select("Sample:", 'Full Sample', everything())%>%
    mutate(Country = i)
  
  data_5.1.0 <- data_5.1.0 %>%
    bind_rows(df_5.1.4)
  print(i)
  decomposition_Fields[[i]] <- df_5.1.4
  
}

data_5.1.6 <- data_5.1.0 

colnames(data_5.1.6) <- c("Type", "Full_Sample", 
                          "IG_1", "IG_2", "IG_3", "IG_4", "IG_5", "Country")

data_5.1.6 <- data_5.1.6 %>%
  arrange(Country, desc(Full_Sample))%>%
  group_by(Country)%>%
  mutate(cumulative = cumsum(Full_Sample))%>%
  ungroup()%>%
  mutate(important = ifelse(cumulative < 0.95 | lag(cumulative) < 0.95,1,0))

data_5.1.7 <- expand_grid(Country = data_5.1.6$Country, Type = data_5.1.6$Type)%>%
  distinct()%>%
  left_join(data_5.1.6)%>%
  mutate(important = ifelse(is.na(important), 0, important))

function_summarise <- function(x){
  x1 <- x %>%
    summarise(
      y5  = wtd.quantile(burden_GAS_gas, weights = hh_weights, probs = 0.05),
      y25 = wtd.quantile(burden_GAS_gas, weights = hh_weights, probs = 0.25),
      y50 = wtd.quantile(burden_GAS_gas, weights = hh_weights, probs = 0.5),
      y75 = wtd.quantile(burden_GAS_gas, weights = hh_weights, probs = 0.75),
      y95 = wtd.quantile(burden_GAS_gas, weights = hh_weights, probs = 0.95),
      mean = wtd.mean(   burden_GAS_gas, weights = hh_weights))
  
  return(x1)
}

# Create Plots of Interest

# Load Codes

District.Codes  <- read_csv("K:/WorkInProgress/2021_Carbon_Footprint_Analysis/Matching Tables/Codes/District.Code.csv")
Education.Codes <- read_csv("K:/WorkInProgress/2021_Carbon_Footprint_Analysis/Matching Tables/Codes/Education.Code.csv")
HHType.Codes    <- read_csv("K:/WorkInProgress/2021_Carbon_Footprint_Analysis/Matching Tables/Codes/HHType.Code.csv")
Income.Code <- distinct(carbon_pricing_incidence_1, main_source_income)%>%
  arrange(main_source_income)%>%
  mutate(Main_Source = c("Wages", "Self-Employment", "Property Rent", "Pensions",
                         "Unemployment Benefits", "Other", "Not specified"))

carbon_pricing_incidence_1.1 <- left_join(carbon_pricing_incidence_0, household_information_0) %>%
  mutate(edu_hhh = ifelse((Country == "Denmark" | Country == "Hungary" | Country == "Sweden") & is.na(edu_hhh), 9, edu_hhh))%>%
  left_join(District.Codes)%>%
  left_join(Education.Codes)%>%
  left_join(HHType.Codes)%>%
  left_join(Income.Code)

carbon_pricing_incidence_1 <- carbon_pricing_incidence_1.1

carbon_pricing_incidence_1$EDU_HHH <- factor(carbon_pricing_incidence_1$EDU_HHH, levels = c("Early Childhood Education",
                                                                                            "Primary", "Lower Secondary",
                                                                                            "Upper Secondary", "Post-secondary Non-Tertiary",
                                                                                            "Short Cycle Tertiary", "Bachelor", "Master", "Not specified"))
carbon_pricing_incidence_1$HH_TYPE <- factor(carbon_pricing_incidence_1$HH_TYPE, levels = c("One Person Household",
                                                                                            "Lone Parent with children",
                                                                                            "Couple without children",
                                                                                            "Couple with children",
                                                                                            "Couple with children and others",
                                                                                            "Other Type")) 
carbon_pricing_incidence_1$Main_Source <- factor(carbon_pricing_incidence_1$Main_Source, levels = c("Wages", "Self-Employment",
                                                                                                    "Unemployment Benefits", "Pensions",
                                                                                                    "Property Rent", "Other", "Not specified"))


data_5.2.1 <- carbon_pricing_incidence_1 %>%
  group_by(Country, urban_01)%>%
  function_summarise()%>%
  ungroup()

data_5.2.2 <- carbon_pricing_incidence_1 %>%
  group_by(Country, district)%>%
  function_summarise()%>%
  ungroup()

data_5.2.3 <- carbon_pricing_incidence_1 %>%
  group_by(Country, EDU_HHH)%>%
  function_summarise()%>%
  ungroup()

data_5.2.4 <- carbon_pricing_incidence_1 %>%
  group_by(Country, Main_Source)%>%
  function_summarise()%>%
  ungroup()

data_5.2.5 <- carbon_pricing_incidence_1 %>%
  group_by(Country, HH_TYPE)%>%
  function_summarise()%>%
  ungroup()

for(i in c("Belgium", "Bulgaria", "Cyprus", "Czech Republic", "Germany", "Denmark", "Estonia", "Greece", "Spain", "Finland",
           "France", "Croatia", "Hungary" ,"Ireland", "Italy", "Lithuania", "Luxembourg", "Latvia", "Netherlands", "Poland",
           "Portugal", "Romania", "Sweden", "Slovak Republic")){
  
  data_5.2.0.1 <- carbon_pricing_incidence_1 %>%
    filter(Country == i)
  
  data_5.2.1.1 <- data_5.2.1 %>%
    filter(Country == i)
  
  data_5.2.2.1 <- data_5.2.2 %>%
    filter(Country == i)
  
  data_5.2.3.1 <- data_5.2.3 %>%
    filter(Country == i)
  
  data_5.2.4.1 <- data_5.2.4 %>%
    filter(Country == i)
  
  data_5.2.5.1 <- data_5.2.5 %>%
    filter(Country == i)
  
  data_5.2.6 <- bind_rows(data_5.2.1.1, data_5.2.2.1, data_5.2.3.1, data_5.2.4.1, data_5.2.5.1)
  
  if(max(data_5.2.6$y95) < 0.05) upper <- 0.05 else
    if(max(data_5.2.6$y95) < 0.1) upper <- 0.1 else
    if(max(data_5.2.6$y95) < 0.15) upper <- 0.15 else 
      if(max(data_5.2.6$y95) < 0.2) upper <- 0.2 else
        if(max(data_5.2.6$y95) < 0.25) upper <- 0.25 else 
          if(max(data_5.2.6$y95) < 0.3) upper <- 0.3 else 
            if(max(data_5.2.6$y95) < 0.35) upper <- 0.35 else 
              if(max(data_5.2.6$y95) < 0.4) upper <- 0.4 else 
                if(max(data_5.2.6$y95) < 0.45) upper <- 0.45 else 
                  if(max(data_5.2.6$y95) < 0.5) upper <- 0.5 else upper <- 0.55
              
  
  
  if(filter(data_5.1.7, Country == i & Type == "Urban Area")$important[1] == 1)            alpha_1 <- 1 else alpha_1 <- 0.2
  if(filter(data_5.1.7, Country == i & Type == "District")$important[1] == 1)              alpha_2 <- 1 else alpha_2 <- 0.2
  if(filter(data_5.1.7, Country == i & Type == "Education")$important[1] == 1)             alpha_3 <- 1 else alpha_3 <- 0.2
  if(filter(data_5.1.7, Country == i & Type == "Main Source of Income")$important[1] == 1) alpha_4 <- 1 else alpha_4 <- 0.2
  if(filter(data_5.1.7, Country == i & Type == "HH Type")$important[1] == 1)               alpha_5 <- 1 else alpha_5 <- 0.2
  
  plot_5.2.1 <- ggplot(data_5.2.1.1, aes(x = factor(urban_01)))+
    geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95), fill = "grey", alpha = alpha_1, stat = "identity", position = position_dodge(0.5), outlier.shape = NA, width = 0.5, size = 0.3) +
    theme_bw()+
    xlab("Urban / Rural")+ ylab("Gas Price Incidence")+
    geom_point(aes(y = mean), shape = 23, size = 1.3, stroke = 0.2, fill = "white")+
    scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0))+
    scale_x_discrete(labels = c("Rural", "Urban"))+
    coord_cartesian(ylim = c(0,upper))+
    #coord_cartesian(ylim = c(0,0.3))+
    ggtitle("")+
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
          legend.text = element_text(size = 7),
          legend.title = element_text(size = 7),
          plot.margin = unit(c(0.1,0.1,0,0), "cm"),
          panel.border = element_rect(size = 0.3))
  
  plot_5.2.2 <- ggplot(data_5.2.2.1, aes(x = factor(district)))+
    geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95), fill = "grey", alpha = alpha_2, stat = "identity", position = position_dodge(0.5), outlier.shape = NA, width = 0.5, size = 0.3) +
    theme_bw()+
    xlab("District")+ ylab("Gas Price Incidence")+
    geom_point(aes(y = mean), shape = 23, size = 1.3, stroke = 0.2, fill = "white")+
    scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0))+
    coord_cartesian(ylim = c(0,upper))+
    #coord_cartesian(ylim = c(0,0.3))+
    ggtitle("")+
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
          legend.text = element_text(size = 7),
          legend.title = element_text(size = 7),
          plot.margin = unit(c(0.1,0.1,0,0), "cm"),
          panel.border = element_rect(size = 0.3))
  
  plot_5.2.3 <- ggplot(data_5.2.3.1, aes(x = factor(EDU_HHH)))+
    geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95), fill = "grey", alpha = alpha_3, stat = "identity", position = position_dodge(0.5), outlier.shape = NA, width = 0.5, size = 0.3) +
    theme_bw()+
    xlab("Education of Household Head")+ ylab("Gas Price Incidence")+
    geom_point(aes(y = mean), shape = 23, size = 1.3, stroke = 0.2, fill = "white")+
    scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0))+
    coord_cartesian(ylim = c(0,upper))+
    #coord_cartesian(ylim = c(0,0.3))+
    ggtitle("")+
    theme(axis.text.y = element_text(size = 7), 
          axis.text.x = element_text(size = 7, angle = 90),
          axis.title  = element_text(size = 7),
          plot.title = element_text(size = 7),
          legend.position = "bottom",
          strip.text = element_text(size = 7),
          strip.text.y = element_text(angle = 180),
          panel.grid.major = element_line(size = 0.3),
          panel.grid.minor = element_blank(),
          axis.ticks = element_line(size = 0.2),
          legend.text = element_text(size = 7),
          legend.title = element_text(size = 7),
          plot.margin = unit(c(0.1,0.3,0,0), "cm"),
          panel.border = element_rect(size = 0.3))
  
  plot_5.2.4 <- ggplot(data_5.2.4.1, aes(x = factor(Main_Source)))+
    geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95), fill = "grey", alpha = alpha_4, stat = "identity", position = position_dodge(0.5), outlier.shape = NA, width = 0.5, size = 0.3) +
    theme_bw()+
    xlab("Main Source of Income")+ ylab("Gas Price Incidence")+
    geom_point(aes(y = mean), shape = 23, size = 1.3, stroke = 0.2, fill = "white")+
    scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0))+
    coord_cartesian(ylim = c(0,upper))+
    #coord_cartesian(ylim = c(0,0.3))+
    ggtitle("")+
    theme(axis.text.y = element_text(size = 7), 
          axis.text.x = element_text(size = 7, angle = 90),
          axis.title  = element_text(size = 7),
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
  
  plot_5.2.5 <- ggplot(data_5.2.5.1, aes(x = factor(HH_TYPE)))+
    geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95), fill = "grey", alpha = alpha_5, stat = "identity", position = position_dodge(0.5), outlier.shape = NA, width = 0.5, size = 0.3) +
    theme_bw()+
    xlab("Household Type")+ ylab("Gas Price Incidence")+
    geom_point(aes(y = mean), shape = 23, size = 1.3, stroke = 0.2, fill = "white")+
    scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0))+
    coord_cartesian(ylim = c(0,upper))+
    #coord_cartesian(ylim = c(0,0.3))+
    ggtitle("")+
    theme(axis.text.y = element_text(size = 7), 
          axis.text.x = element_text(size = 7, angle = 90),
          axis.title  = element_text(size = 7),
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
  
  # This smoothes burden over expenditures 
  
  data_5.2.0.1 <- data_5.2.0.1
  
  upper_2 <- round(mean(data_5.2.0.1$hh_expenditures)+3*sqrt(var(data_5.2.0.1$hh_expenditures)),-3)
  
  plot_5.2.6 <- ggplot(data_5.2.0.1, aes(x = hh_expenditures, y = burden_GAS_gas))+
    geom_point(size = 0.5, shape = 21, colour = "black", alpha = 0.1)+
    #geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95), stat = "identity", position = position_dodge(0.5), outlier.shape = NA, width = 0.5, size = 0.3) +
    theme_bw()+
    xlab("Household Expenditures [€]")+ ylab("Gas Price Incidence")+
    scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0))+
    scale_x_continuous(labels = scales::dollar_format(prefix = "€", big.mark = ","), expand = c(0,0))+
    coord_cartesian(ylim = c(0,upper), xlim = c(0, upper_2))+
    geom_smooth(method = "lm", formula = y ~ poly(x,4), se = TRUE, colour = "#6F99ADFF", fullrange = TRUE, size = 0.5)+
    ggtitle("")+
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
          legend.text = element_text(size = 7),
          legend.title = element_text(size = 7),
          plot.margin = unit(c(0.1,0.4,0,0), "cm"),
          panel.border = element_rect(size = 0.3))
  
  plot_5.2.7.1 <- ggarrange(plot_5.2.6,
                            plot_5.2.1, plot_5.2.2, align = "hv", ncol = 3, nrow = 1)
  plot_5.2.7.2 <- ggarrange(plot_5.2.3, plot_5.2.4, plot_5.2.5, align = "hv", ncol = 3, nrow = 1)
  
  plot_5.2.7 <- ggarrange(plot_5.2.7.1, plot_5.2.7.2, align = "v", ncol = 1, nrow = 2)
  
  # jpeg(sprintf("K:/WorkInProgress/2021_Carbon_Footprint_Analysis/Gas Price Analysis/Figures/Figure_7_Combined_Analysis/Figure_7_%s.jpg", i), width = 30, height = 15, unit = "cm", res = 400)
  # print(plot_5.2.7)
  # dev.off()
  
  print(i)
}

# 5.3 Inequality Decomposition European Level ####

data_5.3.0 <- data.frame()
decomposition_Fields <- list()

data_5.3.1 <- carbon_pricing_incidence_1 %>%
  mutate(Income_Group_5 = as.character(Income_Group_5))
  
data_5.3.2 <- carbon_pricing_incidence_1 %>%
  mutate(Income_Group_5 = "Full Sample")
  
data_5.3.3 <- bind_rows(data_5.3.1, data_5.3.2)%>%
  mutate(log_hh_expenditures = log(hh_expenditures))%>%
  mutate_at(vars(hh_type, main_source_income, edu_hhh), list(~ as.character(.)))%>%
  mutate(edu_hhh = ifelse((Country == "Denmark" | Country == "Hungary" | Country == "Sweden") & is.na(edu_hhh), 9, edu_hhh))
  
df_5.3.4 <- data.frame()
  
for(j in c(1,2,3,4,5, "Full Sample")){
  data_5.3.5 <- data_5.3.3 %>%
    filter(Income_Group_5 == j)
  
  variance_incidence <- wtd.var(data_5.3.5$burden_GAS_gas, weights = data_5.3.5$hh_weights)
  
  formula_0 <- "burden_GAS_gas ~ log_hh_expenditures + urban_01 + Country"
  
  if(nrow(distinct(data_5.3.5, edu_hhh))>1)  formula_0 <- paste0(formula_0, " + edu_hhh")
  if(nrow(distinct(data_5.3.5, hh_type))>1)  formula_0 <- paste0(formula_0, " + hh_type")
  if(nrow(distinct(data_5.3.5, main_source_income))>1)  formula_0 <- paste0(formula_0, " + main_source_income")
  
  formula_1 <- as.formula(formula_0)
  
  model_5.3 <- lm(formula_1, data = data_5.3.5, weights = hh_weights)
  
  prediction_5.3 <- as.data.frame(predict.lm(model_5.3, data_5.3.5, type = "terms"))%>%
    mutate(residuals = resid(model_5.3))
  
  correlations <- sapply(prediction_5.3, function(.) corr(d = cbind(., data_5.3.5$burden_GAS_gas), w = data_5.3.5$hh_weights))
  variance     <- sapply(prediction_5.3, function(x) wtd.var(x,                                   weights = data_5.3.5$hh_weights))
  
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
           R_squared = summary(model_5.3)$r.squared,
           p_j = ifelse(factor != "residuals", s_j/R_squared,NA))%>%
    select(factor, Income_Group_5, p_j)%>%
    rename(s_k = p_j)%>%
    mutate(factor = ifelse(factor == "hh_size", "HH Size",
                           ifelse(factor == "car.01", "Car Ownership",
                                  ifelse(factor == "refrigerator.01", "Refrigerator Own.",
                                         ifelse(factor == "urban_01", "Urban Area",
                                                ifelse(factor == "log_hh_expenditures", "HH Exp. (log)", 
                                                       ifelse(factor == "CF", "Cooking Fuel",
                                                              ifelse(factor == "edu_hhh", "Education",
                                                                     ifelse(factor == "factor(Ethnicity)", "Ethnicity",
                                                                            ifelse(factor == "residuals", factor, 
                                                                                   ifelse(factor == "district", "District",
                                                                                          ifelse(factor == "main_source_income", "Main Source of Income", 
                                                                                                 ifelse(factor == "hh_type", "HH Type", factor)))))))))))))%>%
    rename("Sample:" = factor)
  
  df_5.3.4 <- df_5.3.4 %>%
    bind_rows(joined_1)
}
  
df_5.3.4 <- df_5.3.4 %>%
  pivot_wider(names_from = "Income_Group_5", values_from = "s_k")%>%
  mutate_at(vars(-"Sample:"), list(~ round(.,3)))%>%
  filter("Sample:" != "residuals")%>%
  select("Sample:", 'Full Sample', everything())
  
data_5.3.0 <- data_5.3.0 %>%
  bind_rows(df_5.3.4)

data_5.3.6 <- data_5.3.0 

colnames(data_5.3.6) <- c("Type", "Full_Sample", 
                          "IG_1", "IG_2", "IG_3", "IG_4", "IG_5")

data_5.3.6 <- data_5.3.6 %>%
  arrange(desc(Full_Sample))%>%
  mutate(cumulative = cumsum(Full_Sample))%>%
  mutate(important = ifelse(cumulative < 0.95 | lag(cumulative) < 0.95,1,0))

function_summarise <- function(x){
  x1 <- x %>%
    summarise(
      y5  = wtd.quantile(burden_GAS_gas, weights = hh_weights, probs = 0.05),
      y25 = wtd.quantile(burden_GAS_gas, weights = hh_weights, probs = 0.25),
      y50 = wtd.quantile(burden_GAS_gas, weights = hh_weights, probs = 0.5),
      y75 = wtd.quantile(burden_GAS_gas, weights = hh_weights, probs = 0.75),
      y95 = wtd.quantile(burden_GAS_gas, weights = hh_weights, probs = 0.95),
      mean = wtd.mean(   burden_GAS_gas, weights = hh_weights))
  
  return(x1)
}

# Create Plots of Interest

# Load Codes

District.Codes  <- read_csv("K:/WorkInProgress/2021_Carbon_Footprint_Analysis/Matching Tables/Codes/District.Code.csv")
Education.Codes <- read_csv("K:/WorkInProgress/2021_Carbon_Footprint_Analysis/Matching Tables/Codes/Education.Code.csv")
HHType.Codes    <- read_csv("K:/WorkInProgress/2021_Carbon_Footprint_Analysis/Matching Tables/Codes/HHType.Code.csv")
Income.Code <- distinct(carbon_pricing_incidence_1, main_source_income)%>%
  arrange(main_source_income)%>%
  mutate(Main_Source = c("Wages", "Self-Employment", "Property Rent", "Pensions",
                         "Unemployment Benefits", "Other", "Not specified"))
Occupation.Codes <- read_csv("K:/WorkInProgress/2021_Carbon_Footprint_Analysis/Matching Tables/Codes/Occupation.Code.csv")
Activity.Codes <- read_csv("K:/WorkInProgress/2021_Carbon_Footprint_Analysis/Matching Tables/Codes/Activity.Code.csv")
Industry.Codes <- read_csv("K:/WorkInProgress/2021_Carbon_Footprint_Analysis/Matching Tables/Codes/Industry.Code.csv")


carbon_pricing_incidence_1.1 <- left_join(carbon_pricing_incidence_0, household_information_0) %>%
  mutate(edu_hhh = ifelse((Country == "Denmark" | Country == "Hungary" | Country == "Sweden") & is.na(edu_hhh), 9, edu_hhh))%>%
  left_join(District.Codes)%>%
  left_join(Education.Codes)%>%
  left_join(HHType.Codes)%>%
  left_join(Income.Code)%>%
  left_join(Occupation.Codes)%>%
  left_join(Activity.Codes)%>%
  left_join(Industry.Codes)

carbon_pricing_incidence_1 <- carbon_pricing_incidence_1.1

carbon_pricing_incidence_1$EDU_HHH <- factor(carbon_pricing_incidence_1$EDU_HHH, levels = c("Early Childhood Education",
                                                                                            "Primary", "Lower Secondary",
                                                                                            "Upper Secondary", "Post-secondary Non-Tertiary",
                                                                                            "Short Cycle Tertiary", "Bachelor", "Master", "Not specified"))
carbon_pricing_incidence_1$HH_TYPE <- factor(carbon_pricing_incidence_1$HH_TYPE, levels = c("One Person Household",
                                                                                            "Lone Parent with children",
                                                                                            "Couple without children",
                                                                                            "Couple with children",
                                                                                            "Couple with children and others",
                                                                                            "Other Type")) 
carbon_pricing_incidence_1$Main_Source <- factor(carbon_pricing_incidence_1$Main_Source, levels = c("Wages", "Self-Employment",
                                                                                                    "Unemployment Benefits", "Pensions",
                                                                                                    "Property Rent", "Other", "Not specified"))

data_5.4.1 <- carbon_pricing_incidence_1 %>%
  group_by(urban_01)%>%
  function_summarise()%>%
  ungroup()

data_5.4.2 <- carbon_pricing_incidence_1 %>%
  group_by(Country)%>%
  function_summarise()%>%
  ungroup()

data_5.4.3 <- carbon_pricing_incidence_1 %>%
  group_by(EDU_HHH)%>%
  function_summarise()%>%
  ungroup()

data_5.4.4 <- carbon_pricing_incidence_1 %>%
  group_by(Main_Source)%>%
  function_summarise()%>%
  ungroup()

data_5.4.5 <- carbon_pricing_incidence_1 %>%
  group_by(HH_TYPE)%>%
  function_summarise()%>%
  ungroup()

data_5.4.6 <- carbon_pricing_incidence_1 %>%
  group_by(Occupation)%>%
  function_summarise()%>%
  ungroup()

data_5.4.7 <- carbon_pricing_incidence_1 %>%
  group_by(IND_hhh)%>%
  function_summarise()%>%
  ungroup()

data_5.4.8 <- carbon_pricing_incidence_1 %>%
  group_by(Activity)%>%
  function_summarise()%>%
  ungroup()

data_5.4.9 <- bind_rows(data_5.4.1, data_5.4.2, data_5.4.3, data_5.4.4, data_5.4.5,
                        data_5.4.6, data_5.4.7, data_5.4.8)

upper <- 0.445

alpha_1 <- 0.2
alpha_2 <- 1
alpha_3 <- 0.2
alpha_4 <- 0.2
alpha_5 <- 0.2
alpha_6 <- 1
alpha_7 <- 1
alpha_8 <- 1

plot_5.4.1 <- ggplot(data_5.4.1, aes(x = factor(urban_01)))+
  geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95), fill = "grey", alpha = alpha_1, stat = "identity", position = position_dodge(0.5), outlier.shape = NA, width = 0.5, size = 0.3) +
  theme_bw()+
  xlab("Urban / Rural")+ ylab("Gas Price Incidence")+
  geom_point(aes(y = mean), shape = 23, size = 1.3, stroke = 0.2, fill = "white")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0))+
  scale_x_discrete(labels = c("Rural", "Urban"))+
  coord_cartesian(ylim = c(0,upper))+
  #coord_cartesian(ylim = c(0,0.3))+
  ggtitle("")+
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
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.1,0.1,0,0), "cm"),
        panel.border = element_rect(size = 0.3))

plot_5.4.2 <- ggplot(data_5.4.2, aes(x = reorder(Country, desc(y50))))+
  geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95), fill = "grey", alpha = alpha_2, stat = "identity", position = position_dodge(0.5), outlier.shape = NA, width = 0.5, size = 0.3) +
  theme_bw()+
  xlab("Country")+ ylab("Gas Price Incidence")+
  geom_point(aes(y = mean), shape = 23, size = 1.3, stroke = 0.2, fill = "white")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0))+
  coord_cartesian(ylim = c(0,upper))+
  #coord_cartesian(ylim = c(0,0.3))+
  ggtitle("")+
  theme(axis.text.y = element_text(size = 7), 
        axis.text.x = element_text(size = 7, angle = 90, hjust = 0.95),
        axis.title  = element_text(size = 7),
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

plot_5.4.3 <- ggplot(data_5.4.3, aes(x = factor(EDU_HHH)))+
  geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95), fill = "grey", alpha = alpha_3, stat = "identity", position = position_dodge(0.5), outlier.shape = NA, width = 0.5, size = 0.3) +
  theme_bw()+
  xlab("Education of Household Head")+ ylab("Gas Price Incidence")+
  geom_point(aes(y = mean), shape = 23, size = 1.3, stroke = 0.2, fill = "white")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0))+
  coord_cartesian(ylim = c(0,upper))+
  #coord_cartesian(ylim = c(0,0.3))+
  ggtitle("")+
  theme(axis.text.y = element_text(size = 7), 
        axis.text.x = element_text(size = 7, angle = 90, hjust = 0.95),
        axis.title  = element_text(size = 7),
        plot.title = element_text(size = 7),
        legend.position = "bottom",
        strip.text = element_text(size = 7),
        strip.text.y = element_text(angle = 180),
        panel.grid.major = element_line(size = 0.3),
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(size = 0.2),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.1,0.3,0,0), "cm"),
        panel.border = element_rect(size = 0.3))

plot_5.4.4 <- ggplot(data_5.4.4, aes(x = factor(Main_Source)))+
  geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95), fill = "grey", alpha = alpha_4, stat = "identity", position = position_dodge(0.5), outlier.shape = NA, width = 0.5, size = 0.3) +
  theme_bw()+
  xlab("Main Source of Income")+ ylab("Gas Price Incidence")+
  geom_point(aes(y = mean), shape = 23, size = 1.3, stroke = 0.2, fill = "white")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0))+
  coord_cartesian(ylim = c(0,upper))+
  #coord_cartesian(ylim = c(0,0.3))+
  ggtitle("")+
  theme(axis.text.y = element_text(size = 7), 
        axis.text.x = element_text(size = 7, angle = 90, hjust = 0.95),
        axis.title  = element_text(size = 7),
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

plot_5.4.5 <- ggplot(data_5.4.5, aes(x = factor(HH_TYPE)))+
  geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95), fill = "grey", alpha = alpha_5, stat = "identity", position = position_dodge(0.5), outlier.shape = NA, width = 0.5, size = 0.3) +
  theme_bw()+
  xlab("Household Type")+ ylab("Gas Price Incidence")+
  geom_point(aes(y = mean), shape = 23, size = 1.3, stroke = 0.2, fill = "white")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0))+
  coord_cartesian(ylim = c(0,upper))+
  #coord_cartesian(ylim = c(0,0.3))+
  ggtitle("")+
  theme(axis.text.y = element_text(size = 7), 
        axis.text.x = element_text(size = 7, angle = 90, hjust = 0.95),
        axis.title  = element_text(size = 7),
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

# This smoothes burden over expenditures 

data_5.4.0.1 <- carbon_pricing_incidence_1

upper_2 <- round(mean(data_5.4.0.1$hh_expenditures)+3*sqrt(var(data_5.4.0.1$hh_expenditures)),-3)

plot_5.4.6 <- ggplot(data_5.4.0.1, aes(x = hh_expenditures, y = burden_GAS_gas))+
  geom_point(size = 0.5, shape = 21, colour = "black", alpha = 0.01)+
  #geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95), stat = "identity", position = position_dodge(0.5), outlier.shape = NA, width = 0.5, size = 0.3) +
  theme_bw()+
  xlab("Household Expenditures [€]")+ ylab("Gas Price Incidence")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0))+
  scale_x_continuous(labels = scales::dollar_format(prefix = "€", big.mark = ","), expand = c(0,0))+
  coord_cartesian(ylim = c(0,upper), xlim = c(0, upper_2))+
  geom_smooth(method = "lm", formula = y ~ poly(x,4), se = TRUE, colour = "#6F99ADFF", fullrange = TRUE, size = 0.5)+
  ggtitle("")+
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
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.1,0.4,0,0), "cm"),
        panel.border = element_rect(size = 0.3))

plot_5.4.7.1 <- ggarrange(plot_5.4.6,
                          plot_5.4.1, plot_5.4.2, align = "hv", ncol = 3, nrow = 1)
plot_5.4.7.2 <- ggarrange(plot_5.4.3,
                          plot_5.4.4, plot_5.4.5, align = "hv", ncol = 3, nrow = 1)

plot_5.4.7 <- ggarrange(plot_5.4.7.1, plot_5.4.7.2, align = "v", ncol = 1, nrow = 2)

# jpeg("K:/WorkInProgress/2021_Carbon_Footprint_Analysis/Gas Price Analysis/Figures/Figure_7_Combined_Analysis/Figure_7_Europe.jpg", width = 30, height = 15, unit = "cm", res = 400)
# print(plot_5.4.7)
# dev.off()

plot_5.4.6.1 <- ggplot(data_5.4.6, aes(x = reorder(Occupation, desc(y50))))+
  geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95), fill = "grey", alpha = alpha_6, stat = "identity", position = position_dodge(0.5), outlier.shape = NA, width = 0.5, size = 0.3) +
  theme_bw()+
  xlab("Occupation")+ ylab("Gas Price Incidence")+
  geom_point(aes(y = mean), shape = 23, size = 1.3, stroke = 0.2, fill = "white")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0))+
  coord_cartesian(ylim = c(0,upper))+
  #coord_cartesian(ylim = c(0,0.3))+
  ggtitle("")+
  theme(axis.text.y = element_text(size = 7), 
        axis.text.x = element_text(size = 7, angle = 90, hjust = 0.95),
        axis.title  = element_text(size = 7),
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

plot_5.4.7.1 <- ggplot(filter(data_5.4.7, !is.na(IND_hhh)), aes(x = reorder(IND_hhh, desc(y50))))+
  geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95), fill = "grey", alpha = alpha_7, stat = "identity", position = position_dodge(0.5), outlier.shape = NA, width = 0.5, size = 0.3) +
  theme_bw()+
  xlab("Industry")+ ylab("Gas Price Incidence")+
  geom_point(aes(y = mean), shape = 23, size = 1.3, stroke = 0.2, fill = "white")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0))+
  coord_cartesian(ylim = c(0,upper))+
  #coord_cartesian(ylim = c(0,0.3))+
  ggtitle("")+
  theme(axis.text.y = element_text(size = 7), 
        axis.text.x = element_text(size = 7, angle = 90, hjust = 0.95),
        axis.title  = element_text(size = 7),
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

plot_5.4.8.1 <- ggplot(filter(data_5.4.8, !is.na(Activity)), aes(x = reorder(Activity, desc(y50))))+
  geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95), fill = "grey", alpha = alpha_8, stat = "identity", position = position_dodge(0.5), outlier.shape = NA, width = 0.5, size = 0.3) +
  theme_bw()+
  xlab("Main Activity")+ ylab("Gas Price Incidence")+
  geom_point(aes(y = mean), shape = 23, size = 1.3, stroke = 0.2, fill = "white")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0))+
  coord_cartesian(ylim = c(0,upper))+
  #coord_cartesian(ylim = c(0,0.3))+
  ggtitle("")+
  theme(axis.text.y = element_text(size = 7), 
        axis.text.x = element_text(size = 7, angle = 90, hjust = 0.95),
        axis.title  = element_text(size = 7),
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

plot_add <- ggarrange(plot_5.4.6.1, 
                      #plot_5.4.7.1,
                      plot_5.4.8.1)

# jpeg("K:/WorkInProgress/2021_Carbon_Footprint_Analysis/Gas Price Analysis/Figures/Figure_7_Combined_Analysis/Figure_7_Europe_add_%d.jpg", width = 30, height = 15, unit = "cm", res = 400)
# print(plot_add)
# print(plot_5.4.7.1)
# dev.off()

# 5.3 Regression ####

data_5.3 <- carbon_pricing_incidence_1 %>%
  mutate(log_hh_expenditures = log(hh_expenditures))

model_5.3.1 <- lm(burden_GAS_gas ~ log_hh_expenditures, weights = hh_weights, data = data_5.3)
model_5.3.2 <- lm(burden_GAS_gas ~ log_hh_expenditures + Country, weights = hh_weights, data = data_5.3)
model_5.3.3 <- lm(burden_GAS_gas ~ log_hh_expenditures + Country + urban_01, weights = hh_weights, data = data_5.3)
model_5.3.4 <- lm(burden_GAS_gas ~ log_hh_expenditures + Country + urban_01 + edu_hhh, weights = hh_weights, data = data_5.3)
model_5.3.5 <- lm(burden_GAS_gas ~ log_hh_expenditures + Country + urban_01 + edu_hhh + hh_type, weights = hh_weights, data = data_5.3)
model_5.3.6 <- lm(burden_GAS_gas ~ log_hh_expenditures + Country + urban_01 + edu_hhh + hh_type + main_source_income, weights = hh_weights, data = data_5.3)


summary(model_5.3.2)

library(broom)

tidy_5.3.2 <- tidy(model_5.3.2)

# 5.4 Regression and Definition of Hardship-Cases ####
# 5.4.1 Summary Statistics ####
# 5.4.2 Logit-Regression ####
# 5   K-Means Clustering ####

library(fastDummies)
library(cluster)

data_5.1 <- carbon_pricing_incidence_1 %>%
  filter(Country == "Germany")%>%
  select(hh_id, hh_size, hh_weights, urban_01, district, main_source_income, hh_type, burden_GAS_gas, Income_Group_10)%>%
  mutate(Burden_Decile = as.numeric(binning(burden_GAS_gas, bins = 10, method = c("wtd.quantile"), weights = hh_weights)))%>%
  mutate_at(vars(urban_01, district, main_source_income, hh_type, Income_Group_10, Burden_Decile), list(~ as.character(.)))

data_5.1.1 <- data_5.1 %>%
  select(urban_01, district, Burden_Decile, Income_Group_10)%>%
  dummy_cols(remove_selected_columns = TRUE)

model <- kmeans(data_5.1.1, centers = 5)

data_5.1.2 <- data_5.1.1 %>%
  mutate(cluster = model$cluster)%>%
  group_by(cluster)%>%
  summarise_all(.funs = (~ mean(.)))%>%
  ungroup()

data_5.1.3 <- data_5.1 %>%
  select(urban_01, district, burden_GAS_gas, Income_Group_10)%>%
  mutate(urban_01 = factor(urban_01),
         district = factor(district),
         Income_Group_10 = factor(Income_Group_10))%>%
  slice(1:1000)

start_time <- Sys.time()
model_daisy <- daisy(data_5.1.3, metric = "gower")
end_time <- Sys.time()
Time <- end_time-start_time
print(Time)


pam_model <- pam(model_daisy, k = 2)
hc_model <- hclust(model_daisy, method = "complete")
cluster <- cutree(hc_model, k = 2)


data_5.1.4 <- data_5.1.3 %>%
  mutate(cluster = cluster)%>%
  group_by(cluster)%>%
  summarise(burden_GAS_gas = mean(burden_GAS_gas))%>%
  ungroup()

ggplot(data_5.1.4, aes(x = Income_Group_10, y = burden_GAS_gas, fill = factor(cluster)))+
  geom_point(shape = 21, size = 3)


ggplot(hc_model, aes(x = Income_Group))


tot_withinss <- map_dbl(1:50, function(k){
  model <- kmeans(x = slice(data_5.1.1, 1:2000), centers = k)
  model$tot.withinss
})

elbow_df <- data.frame(k = 1:50,
                       tot_withinss = tot_withinss)

ggplot(elbow_df, aes(x = k, y = tot_withinss))+
  geom_line()+
  scale_x_continuous(breaks = 1:50)

sil_width <- map_dbl(2:10, function(k){
  model <- pam(x = slice(data_5.1.1, 1:2000), k = k)
  model$silinfo$avg.width
})

sil_df <- data.frame(
  k = 2:10,
  sil_width = sil_width
)

ggplot(sil_df, aes(x = k, y = sil_width))+
  geom_line()

data_5.1.5 <- data_5.1 %>%
  group_by(Income_Group_10, urban_01, district, main_source_income)%>%
  summarise(burden_GAS_gas = wtd.mean(burden_GAS_gas, hh_weights),
            hhs = sum(hh_weights))%>%
  ungroup()

model <- lm(burden_GAS_gas ~ hh_expenditures + urban_01 + factor(district) + factor(main_source_income), weights = hh_weights, 
   data = filter(carbon_pricing_incidence_1, Country == "Germany"))
summary(model)

# 6   Gas vs. Coal vs Fuels ####

data_6 <- left_join(carbon_pricing_incidence_0, household_information_0) %>%
  mutate(edu_hhh = ifelse((Country == "Denmark" | Country == "Hungary" | Country == "Sweden") & is.na(edu_hhh), 9, edu_hhh))%>%
  select(hh_id, Country, hh_weights, burden_P_C_p_c, burden_COAL_coal, burden_GAS_gas)%>%
  pivot_longer(burden_P_C_p_c:burden_GAS_gas, names_to = "Type", values_to = "burden")%>%
  mutate(Type = ifelse(Type == "burden_GAS_gas", "Gas Price Increase 138%",
                       ifelse(Type == "burden_COAL_coal", "Coal Price Increase 15%",
                              ifelse(Type == "burden_P_C_p_c", "Liquid Fuel Price Increase 15%", NA))))%>%
  group_by(Country, Type)%>%
  summarise(
    y5  = wtd.quantile(burden, weights = hh_weights, probs = 0.05),
    y25 = wtd.quantile(burden, weights = hh_weights, probs = 0.25),
    y50 = wtd.quantile(burden, weights = hh_weights, probs = 0.5),
    y75 = wtd.quantile(burden, weights = hh_weights, probs = 0.75),
    y95 = wtd.quantile(burden, weights = hh_weights, probs = 0.95),
    mean = wtd.mean(   burden, weights = hh_weights))%>%
  ungroup()

plot_6.1 <- ggplot(filter(data_6, Type == "Gas Price Increase 138%"), aes(x = reorder(Country, desc(y50))))+
  geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95), fill = "lightgrey", alpha = 1, stat = "identity", position = position_dodge(0.5), outlier.shape = NA, width = 0.5, size = 0.3) +
  theme_bw()+
  xlab("Country")+ ylab("Gas Price Incidence")+
  geom_point(aes(y = mean), shape = 23, size = 1.3, stroke = 0.2, fill = "white")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0))+
  coord_cartesian(ylim = c(0,0.45))+
  #coord_cartesian(ylim = c(0,0.3))+
  ggtitle("Gas Price Increase 138%")+
  theme(axis.text.y = element_text(size = 7), 
        axis.text.x = element_text(size = 7, hjust = 0.95, vjust = 0.5, angle = 90),
        axis.title  = element_text(size = 7),
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

plot_6.2 <- ggplot(filter(data_6, Type == "Coal Price Increase 15%"), aes(x = reorder(Country, desc(y50))))+
  geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95), fill = "grey", alpha = 1, stat = "identity", position = position_dodge(0.5), outlier.shape = NA, width = 0.5, size = 0.3) +
  theme_bw()+
  xlab("Country")+ ylab("Coal Price Incidence")+
  geom_point(aes(y = mean), shape = 23, size = 1.3, stroke = 0.2, fill = "white")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1), expand = c(0,0))+
  coord_cartesian(ylim = c(0,0.02))+
  #coord_cartesian(ylim = c(0,0.3))+
  ggtitle("Coal Price Increase 15%")+
  theme(axis.text.y = element_text(size = 7), 
        axis.text.x = element_text(size = 7, hjust = 0.95, vjust = 0.5, angle = 90),
        axis.title  = element_text(size = 7),
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

plot_6.3 <- ggplot(filter(data_6, Type == "Liquid Fuel Price Increase 15%"), aes(x = reorder(Country, desc(y50))))+
  geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95), fill = "grey", alpha = 1, stat = "identity", position = position_dodge(0.5), outlier.shape = NA, width = 0.5, size = 0.3) +
  theme_bw()+
  xlab("Country")+ ylab("Liquid Fuel Price Incidence")+
  geom_point(aes(y = mean), shape = 23, size = 1.3, stroke = 0.2, fill = "white")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0))+
  coord_cartesian(ylim = c(0,0.07))+
  #coord_cartesian(ylim = c(0,0.3))+
  ggtitle("Liquid Fuel Price Increase 15%")+
  theme(axis.text.y = element_text(size = 7), 
        axis.text.x = element_text(size = 7, angle = 90, hjust = 0.95, vjust = 0.5),
        axis.title  = element_text(size = 7),
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

plot_6.1.2.3 <- ggarrange(plot_6.1, plot_6.2, plot_6.3, nrow = 1)


# jpeg("K:/WorkInProgress/2021_Carbon_Footprint_Analysis/Gas Price Analysis/Figures/Figure_8_Gas_Coal_Fuels/Figure_8_Europe.jpg", width = 30, height = 15, unit = "cm", res = 400)
# print(plot_6.1.2.3)
# dev.off()

jpeg("C:/Users/misl/ownCloud/EU_Gasprice_Analysis/Figures/2022_04_19_Figures/Figure_3.jpg", width = 15, height = 15, unit = "cm", res = 400)
print(plot_6.1)
dev.off()


# 7     Updated Analysis ####

# 7.1   Gas / Oil / Coal: Aggregate vs. Direct vs. Indirect ####

data_7.1 <- left_join(carbon_pricing_incidence_0, household_information_0) %>%
  mutate(edu_hhh = ifelse((Country == "Denmark" | Country == "Hungary" | Country == "Sweden") & is.na(edu_hhh), 9, edu_hhh))%>%
  select(hh_id, Country, hh_weights, 
         burden_P_C_p_c, burden_COAL_coal, burden_GAS_gas,
         burden_P_C_direct, burden_COAL_direct, burden_GAS_direct,
         burden_P_C_indirect, burden_COAL_indirect, burden_GAS_indirect)%>%
  pivot_longer(burden_P_C_p_c:burden_GAS_indirect, names_to = "Type", values_to = "burden")%>%
  mutate(Type = ifelse(Type == "burden_GAS_gas", "Gas Price Increase 138%",
                       ifelse(Type == "burden_COAL_coal", "Coal Price Increase 15%",
                              ifelse(Type == "burden_P_C_p_c", "Liquid Fuel Price Increase 15%", 
                                     ifelse(Type == "burden_GAS_direct", "Gas Price Increase 138% (Direct Effects)",
                                            ifelse(Type == "burden_COAL_direct", "Coal Price Increase 15% (Direct Effects)",
                                                   ifelse(Type == "burden_P_C_direct", "Liquid Fuel Price Increase 15% (Direct Effects)", 
                                                          ifelse(Type == "burden_GAS_indirect", "Gas Price Increase 138% (Indirect Effects)",
                                                                 ifelse(Type == "burden_COAL_indirect", "Coal Price Increase 15% (Indirect Effects)",
                                                                        ifelse(Type == "burden_P_C_indirect", "Liquid Fuel Price Increase 15% (Indirect Effects)", NA))))))))))%>%
  group_by(Country, Type)%>%
  summarise(
    y5  = wtd.quantile(burden, weights = hh_weights, probs = 0.05),
    y25 = wtd.quantile(burden, weights = hh_weights, probs = 0.25),
    y50 = wtd.quantile(burden, weights = hh_weights, probs = 0.5),
    y75 = wtd.quantile(burden, weights = hh_weights, probs = 0.75),
    y95 = wtd.quantile(burden, weights = hh_weights, probs = 0.95),
    mean = wtd.mean(   burden, weights = hh_weights))%>%
  ungroup()

data_7.1.1 <- data_7.1 %>%
  filter(Type %in% c("Gas Price Increase 138%", "Gas Price Increase 138% (Direct Effects)", "Gas Price Increase 138% (Indirect Effects)" ))

data_7.1.2 <- data_7.1 %>%
  filter(Type %in% c("Coal Price Increase 15%", "Coal Price Increase 15% (Direct Effects)", "Coal Price Increase 15% (Indirect Effects)" ))

data_7.1.3 <- data_7.1 %>%
  filter(Type %in% c("Liquid Fuel Price Increase 15%", "Liquid Fuel Price Increase 15% (Direct Effects)", "Liquid Fuel Price Increase 15% (Indirect Effects)" ))


plot_7.1.1 <- ggplot(data_7.1.1, aes(x = reorder(Country, desc(y50))))+
  geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95), fill = "grey", alpha = 1, stat = "identity", position = position_dodge(0.5), outlier.shape = NA, width = 0.5, size = 0.3) +
  theme_bw()+
  facet_wrap(. ~ Type)+
  xlab("Country")+ ylab("Gas Price Incidence")+
  geom_point(aes(y = mean), shape = 23, size = 1.3, stroke = 0.2, fill = "white")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0))+
  coord_cartesian(ylim = c(0,0.45))+
  #coord_cartesian(ylim = c(0,0.3))+
  ggtitle("Gas Price Increase 138%")+
  theme(axis.text.y = element_text(size = 7), 
        axis.text.x = element_text(size = 7, hjust = 0.95, vjust = 0.5, angle = 90),
        axis.title  = element_text(size = 7),
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

plot_7.1.2 <- ggplot(data_7.1.2, aes(x = reorder(Country, desc(y50))))+
  geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95), fill = "grey", alpha = 1, stat = "identity", position = position_dodge(0.5), outlier.shape = NA, width = 0.5, size = 0.3) +
  theme_bw()+
  facet_wrap(. ~ Type)+
  xlab("Country")+ ylab("Coal Price Incidence")+
  geom_point(aes(y = mean), shape = 23, size = 1.3, stroke = 0.2, fill = "white")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1), expand = c(0,0))+
  coord_cartesian(ylim = c(0,0.02))+
  #coord_cartesian(ylim = c(0,0.3))+
  ggtitle("Coal Price Increase 15%")+
  theme(axis.text.y = element_text(size = 7), 
        axis.text.x = element_text(size = 7, hjust = 0.95, vjust = 0.5, angle = 90),
        axis.title  = element_text(size = 7),
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

plot_7.1.3 <- ggplot(data_7.1.3, aes(x = reorder(Country, desc(y50))))+
  geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95), fill = "grey", alpha = 1, stat = "identity", position = position_dodge(0.5), outlier.shape = NA, width = 0.5, size = 0.3) +
  theme_bw()+
  facet_wrap(.~ Type)+
  xlab("Country")+ ylab("Liquid Fuel Price Incidence")+
  geom_point(aes(y = mean), shape = 23, size = 1.3, stroke = 0.2, fill = "white")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0))+
  coord_cartesian(ylim = c(0,0.07))+
  #coord_cartesian(ylim = c(0,0.3))+
  ggtitle("Liquid Fuel Price Increase 15%")+
  theme(axis.text.y = element_text(size = 7), 
        axis.text.x = element_text(size = 7, angle = 90, hjust = 0.95, vjust = 0.5),
        axis.title  = element_text(size = 7),
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

# jpeg("K:/WorkInProgress/2021_Carbon_Footprint_Analysis/Gas Price Analysis/Figures/Figure_9_Gas_Coal_Fuels_Direct_Indirect/Figure_9_Europe_%d.jpg", width = 30, height = 15, unit = "cm", res = 400)
# print(plot_7.1.1)
# print(plot_7.1.2)
# print(plot_7.1.3)
# dev.off()

data_7.1.4 <- data_7.1.1 %>%
  filter(Type != "Gas Price Increase 138%")%>%
  arrange(Type, desc(y50))%>%
  mutate(number = 1:n())%>%
  group_by(Country)%>%
  mutate(number = min(number))%>%
  ungroup()

plot_7.1.4 <- ggplot(data_7.1.4, aes(x = reorder(Country, number)))+
  geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95), fill = "lightgrey", alpha = 1, stat = "identity", position = position_dodge(0.5), outlier.shape = NA, width = 0.5, size = 0.3) +
  theme_bw()+
  facet_wrap(. ~ Type)+
  xlab("Country")+ ylab("Gas Price Incidence")+
  geom_point(aes(y = mean), shape = 23, size = 1.3, stroke = 0.2, fill = "white")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0))+
  coord_cartesian(ylim = c(0,0.28))+
  #coord_cartesian(ylim = c(0,0.3))+
  ggtitle("Gas Price Increase 138%")+
  theme(axis.text.y = element_text(size = 7), 
        axis.text.x = element_text(size = 7, hjust = 0.95, vjust = 0.5, angle = 90),
        axis.title  = element_text(size = 7),
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

jpeg("C:/Users/misl/ownCloud/EU_Gasprice_Analysis/Figures/2022_04_19_Figures/Figure_4.jpg", width = 15, height = 12, unit = "cm", res = 400)
print(plot_7.1.4)
dev.off()


# 7.2   Country-Specific Figures ####

agg_expenditures <- data.frame()

for(i in c("Belgium", "Bulgaria", "Cyprus", "Czech Republic", "Germany", "Denmark", "Estonia", "Greece", "Spain", "Finland",
           "France", "Croatia", "Hungary" ,"Ireland", "Italy", "Lithuania", "Luxembourg", "Latvia", "Netherlands", "Poland",
           "Portugal", "Romania", "Sweden", "Slovak Republic")){

data_7.2.0 <- left_join(carbon_pricing_incidence_0, household_information_0)%>%
  filter(Country == i)%>%
  mutate(Heating_Gas = ifelse(is.na(Heating_Gas),0,Heating_Gas),
                             Heating_NA  = ifelse(is.na(Heating_NA), 0,Heating_NA),
                             Heating_Oil = ifelse(is.na(Heating_Oil),0, Heating_Oil),
                             Heating_District_Heat = ifelse(is.na(Heating_District_Heat),0, Heating_District_Heat))

data_7.2.0b <- left_join(indirect_expenditures_0, household_information_0)%>%
  filter(Country == i)

# 7.2.1 Coal / Oil / Gas per Quintiles ####

data_7.2.1 <- data_7.2.0 %>%
  select(hh_id, Country, hh_weights, burden_P_C_p_c, burden_COAL_coal, burden_GAS_gas, Income_Group_5)%>%
  pivot_longer(burden_P_C_p_c:burden_GAS_gas, names_to = "Type", values_to = "burden")%>%
  mutate(Type = ifelse(Type == "burden_GAS_gas", "Gas Price Increase 138%",
                       ifelse(Type == "burden_COAL_coal", "Coal Price Increase 15%",
                              ifelse(Type == "burden_P_C_p_c", "Liquid Fuel Price Increase 15%", NA))))%>%
  group_by(Income_Group_5, Type)%>%
  summarise(
    y5  = wtd.quantile(burden, weights = hh_weights, probs = 0.05),
    y25 = wtd.quantile(burden, weights = hh_weights, probs = 0.25),
    y50 = wtd.quantile(burden, weights = hh_weights, probs = 0.5),
    y75 = wtd.quantile(burden, weights = hh_weights, probs = 0.75),
    y95 = wtd.quantile(burden, weights = hh_weights, probs = 0.95),
    mean = wtd.mean(   burden, weights = hh_weights))%>%
  ungroup()

data_7.2.1$Type <- factor(data_7.2.1$Type, levels = c("Gas Price Increase 138%", "Liquid Fuel Price Increase 15%", "Coal Price Increase 15%"))

upper_7.2.1.1 <- round(max(filter(data_7.2.1, Type == "Gas Price Increase 138%")$y95)+0.01,2)
upper_7.2.1.2 <- round(max(filter(data_7.2.1, Type == "Liquid Fuel Price Increase 15%")$y95)+0.01,2)
upper_7.2.1.3 <- round(max(filter(data_7.2.1, Type == "Coal Price Increase 15%")$y95)+0.01,2)

plot_7.2.1.1 <- ggplot(filter(data_7.2.1, Type == "Gas Price Increase 138%"), aes(x = factor(Income_Group_5)))+
  geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95), fill = "grey", alpha = 1, stat = "identity", position = position_dodge(0.5), outlier.shape = NA, width = 0.5, size = 0.3) +
  theme_bw()+
  xlab("Expenditure Quintile")+ ylab("Gas Price Incidence")+
  geom_point(aes(y = mean), shape = 23, size = 1.3, stroke = 0.2, fill = "white")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0))+
  coord_cartesian(ylim = c(0,upper_7.2.1.1))+
  #coord_cartesian(ylim = c(0,0.3))+
  ggtitle("Gas Price Increase 138%")+
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
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.1,0.1,0,0), "cm"),
        panel.border = element_rect(size = 0.3))

plot_7.2.1.2 <- ggplot(filter(data_7.2.1, Type == "Liquid Fuel Price Increase 15%"), aes(x = factor(Income_Group_5)))+
  geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95), fill = "grey", alpha = 1, stat = "identity", position = position_dodge(0.5), outlier.shape = NA, width = 0.5, size = 0.3) +
  theme_bw()+
  xlab("Expenditure Quintile")+ ylab("Liquid Fuel Price Incidence")+
  geom_point(aes(y = mean), shape = 23, size = 1.3, stroke = 0.2, fill = "white")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1), expand = c(0,0))+
  coord_cartesian(ylim = c(0,upper_7.2.1.2))+
  #coord_cartesian(ylim = c(0,0.3))+
  ggtitle("Coal Price Increase 15%")+
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
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.1,0.1,0,0), "cm"),
        panel.border = element_rect(size = 0.3))

plot_7.2.1.3 <- ggplot(filter(data_7.2.1, Type == "Coal Price Increase 15%"), aes(x = factor(Income_Group_5)))+
  geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95), fill = "grey", alpha = 1, stat = "identity", position = position_dodge(0.5), outlier.shape = NA, width = 0.5, size = 0.3) +
  theme_bw()+
  xlab("Expenditure Quintile")+ ylab("Coal Price Incidence")+
  geom_point(aes(y = mean), shape = 23, size = 1.3, stroke = 0.2, fill = "white")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1), expand = c(0,0))+
  coord_cartesian(ylim = c(0,upper_7.2.1.3))+
  #coord_cartesian(ylim = c(0,0.3))+
  ggtitle("Liquid Fuel Price Increase 15%")+
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
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.1,0.1,0,0), "cm"),
        panel.border = element_rect(size = 0.3))

# 7.2.2 How many people use Gas / District Heat / Oil for Heating per Quintile ?####

data_7.2.2 <- data_7.2.0 %>%
  select(hh_id, Heating_Gas, Heating_NA, Heating_Oil, Heating_District_Heat, Income_Group_5, hh_weights)%>%
  mutate(Heating_Gas_w = ifelse(Heating_Gas > 0, hh_weights,0),
         Heating_Oil_w = ifelse(Heating_Oil > 0, hh_weights,0),
         Heating_District_Heat_w = ifelse(Heating_District_Heat > 0, hh_weights, 0),
         No_Heating = ifelse(Heating_Gas == 0 & Heating_Oil == 0 & Heating_District_Heat == 0,hh_weights,0))%>%
  group_by(Income_Group_5)%>%
  summarise("Heating with Gas" = sum(Heating_Gas_w),
            "Heating with Oil" = sum(Heating_Oil_w),
            "Heating with District Heat" = sum(Heating_District_Heat_w),
            "No Heating Expenditures"    = sum(No_Heating),
            hh_weights = sum(hh_weights))%>%
  ungroup()%>%
  pivot_longer("Heating with Gas":"No Heating Expenditures", names_to = "Type", values_to = "weights")%>%
  mutate(share = weights/hh_weights)

data_7.2.2$Type <- factor(data_7.2.2$Type, levels = c("Heating with Gas", "Heating with Oil",
                                                      "Heating with District Heat", "No Heating Expenditures"))

plot_7.2.2 <- ggplot(data_7.2.2, aes(x = factor(Income_Group_5)))+
  geom_col(aes(y = share, fill = Type), width = 0.5, position = position_dodge(0.7), colour = "black")+
  theme_bw()+
  xlab("Expenditure Quintile")+ ylab("Household Share")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0))+
  coord_cartesian(ylim = c(0,round(max(data_7.2.2$share + 0.01),2)))+
  #coord_cartesian(ylim = c(0,0.3))+
  scale_fill_nejm()+
  labs(fill = "Heating Type")+
  ggtitle("Source of Heating")+
  guides(fill = guide_legend(nrow = 2))+
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
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.1,0.1,0,0), "cm"),
        panel.border = element_rect(size = 0.3))

# 7.2.3 Expenditure Shares within Quintiles for Oil, District Heat and Gas ####

# Boxplot mit Non-Zeros und Expenditure Shares über Quintiles für Gas, Oil and District Heat

if(nrow(filter(data_7.2.0, Heating_Gas != 0))>10){
  data_7.2.3.1 <- data_7.2.0 %>%
    select(hh_id, Income_Group_5, hh_weights, Heating_Gas, hh_expenditures)%>%
    mutate(share_Gas = ifelse(Heating_Gas > 0, Heating_Gas/hh_expenditures, NA))%>%
    group_by(Income_Group_5)%>%
    summarise(
      y5  = wtd.quantile(share_Gas, weights = hh_weights, na.rm = TRUE, probs = 0.05),
      y25 = wtd.quantile(share_Gas, weights = hh_weights, na.rm = TRUE, probs = 0.25),
      y50 = wtd.quantile(share_Gas, weights = hh_weights, na.rm = TRUE, probs = 0.5),
      y75 = wtd.quantile(share_Gas, weights = hh_weights, na.rm = TRUE, probs = 0.75),
      y95 = wtd.quantile(share_Gas, weights = hh_weights, na.rm = TRUE, probs = 0.95),
      mean = wtd.mean(   share_Gas, weights = hh_weights, na.rm = TRUE))%>%
    ungroup()
} else {
  data_7.2.3.1 <- data_7.2.0 %>%
    mutate(share_Gas = Heating_Gas/hh_expenditures)%>%
    group_by(Income_Group_5)%>%
    summarise(
      y5  = wtd.quantile(share_Gas, weights = hh_weights, na.rm = TRUE, probs = 0.05),
      y25 = wtd.quantile(share_Gas, weights = hh_weights, na.rm = TRUE, probs = 0.25),
      y50 = wtd.quantile(share_Gas, weights = hh_weights, na.rm = TRUE, probs = 0.5),
      y75 = wtd.quantile(share_Gas, weights = hh_weights, na.rm = TRUE, probs = 0.75),
      y95 = wtd.quantile(share_Gas, weights = hh_weights, na.rm = TRUE, probs = 0.95),
      mean = wtd.mean(   share_Gas, weights = hh_weights, na.rm = TRUE))%>%
    ungroup()
}

if(nrow(filter(data_7.2.0, Heating_Oil != 0))>10){
  data_7.2.3.2 <- data_7.2.0 %>%
    select(hh_id, Income_Group_5, hh_weights, Heating_Oil, hh_expenditures)%>%
    mutate(share_Oil = ifelse(Heating_Oil > 0, Heating_Oil/hh_expenditures, NA))%>%
    group_by(Income_Group_5)%>%
    summarise(
      y5  = wtd.quantile(share_Oil, weights = hh_weights, na.rm = TRUE, probs = 0.05),
      y25 = wtd.quantile(share_Oil, weights = hh_weights, na.rm = TRUE, probs = 0.25),
      y50 = wtd.quantile(share_Oil, weights = hh_weights, na.rm = TRUE, probs = 0.5),
      y75 = wtd.quantile(share_Oil, weights = hh_weights, na.rm = TRUE, probs = 0.75),
      y95 = wtd.quantile(share_Oil, weights = hh_weights, na.rm = TRUE, probs = 0.95),
      mean = wtd.mean(   share_Oil, weights = hh_weights, na.rm = TRUE))%>%
    ungroup()
} else {
  data_7.2.3.2 <- data_7.2.0 %>%
    mutate(share_Oil = Heating_Oil/hh_expenditures)%>%
    group_by(Income_Group_5)%>%
    summarise(
      y5  = wtd.quantile(share_Oil, weights = hh_weights, na.rm = TRUE, probs = 0.05),
      y25 = wtd.quantile(share_Oil, weights = hh_weights, na.rm = TRUE, probs = 0.25),
      y50 = wtd.quantile(share_Oil, weights = hh_weights, na.rm = TRUE, probs = 0.5),
      y75 = wtd.quantile(share_Oil, weights = hh_weights, na.rm = TRUE, probs = 0.75),
      y95 = wtd.quantile(share_Oil, weights = hh_weights, na.rm = TRUE, probs = 0.95),
      mean = wtd.mean(   share_Oil, weights = hh_weights, na.rm = TRUE))%>%
    ungroup()
}

if(nrow(filter(data_7.2.0, Heating_District_Heat != 0))>10 & i != "Portugal"){
data_7.2.3.3 <- data_7.2.0 %>%
  select(hh_id, Income_Group_5, hh_weights, Heating_District_Heat, hh_expenditures)%>%
  mutate(share_DHs = ifelse(Heating_District_Heat > 0, Heating_District_Heat/hh_expenditures, NA))%>%
  group_by(Income_Group_5)%>%
  summarise(
    y5  = wtd.quantile(share_DHs, weights = hh_weights, na.rm = TRUE, probs = 0.05),
    y25 = wtd.quantile(share_DHs, weights = hh_weights, na.rm = TRUE, probs = 0.25),
    y50 = wtd.quantile(share_DHs, weights = hh_weights, na.rm = TRUE, probs = 0.5),
    y75 = wtd.quantile(share_DHs, weights = hh_weights, na.rm = TRUE, probs = 0.75),
    y95 = wtd.quantile(share_DHs, weights = hh_weights, na.rm = TRUE, probs = 0.95),
    mean = wtd.mean(   share_DHs, weights = hh_weights, na.rm = TRUE))%>%
  ungroup()
} else {
  data_7.2.3.3 <- data_7.2.0 %>%
    mutate(share_DHs = Heating_District_Heat/hh_expenditures)%>%
    group_by(Income_Group_5)%>%
    summarise(
      y5  = wtd.quantile(share_DHs, weights = hh_weights, na.rm = TRUE, probs = 0.05),
      y25 = wtd.quantile(share_DHs, weights = hh_weights, na.rm = TRUE, probs = 0.25),
      y50 = wtd.quantile(share_DHs, weights = hh_weights, na.rm = TRUE, probs = 0.5),
      y75 = wtd.quantile(share_DHs, weights = hh_weights, na.rm = TRUE, probs = 0.75),
      y95 = wtd.quantile(share_DHs, weights = hh_weights, na.rm = TRUE, probs = 0.95),
      mean = wtd.mean(   share_DHs, weights = hh_weights, na.rm = TRUE))%>%
    ungroup()
}

plot_7.2.3.1 <- ggplot(data_7.2.3.1, aes(x = factor(Income_Group_5)))+
  geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95), fill = "grey", alpha = 1, stat = "identity", position = position_dodge(0.5), outlier.shape = NA, width = 0.5, size = 0.3) +
  theme_bw()+
  xlab("Expenditure Quintile")+ ylab("Gas Expenditure Share")+
  geom_point(aes(y = mean), shape = 23, size = 1.3, stroke = 0.2, fill = "white")+
    scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0), breaks = c(0,0.05, 0.1, 0.15, 0.2))+
  coord_cartesian(ylim = c(0,round(max(data_7.2.3.1$y95 + 0.01),2)))+
  #coord_cartesian(ylim = c(0,0.3))+
  scale_fill_nejm()+
  ggtitle("Gas Expenditure Shares")+
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
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.1,0.1,0,0), "cm"),
        panel.border = element_rect(size = 0.3))

plot_7.2.3.2 <- ggplot(data_7.2.3.2, aes(x = factor(Income_Group_5)))+
  geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95), fill = "grey", alpha = 1, stat = "identity", position = position_dodge(0.5), outlier.shape = NA, width = 0.5, size = 0.3) +
  theme_bw()+
  xlab("Expenditure Quintile")+ ylab("Oil Expenditure Share")+
  geom_point(aes(y = mean), shape = 23, size = 1.3, stroke = 0.2, fill = "white")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0), breaks = c(0,0.05, 0.1, 0.15, 0.2))+
  coord_cartesian(ylim = c(0,round(max(data_7.2.3.2$y95 + 0.01),2)))+
  #coord_cartesian(ylim = c(0,0.3))+
  scale_fill_nejm()+
  ggtitle("Oil Expenditure Shares")+
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
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.1,0.1,0,0), "cm"),
        panel.border = element_rect(size = 0.3))

plot_7.2.3.3 <- ggplot(data_7.2.3.3, aes(x = factor(Income_Group_5)))+
  geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95), fill = "grey", alpha = 1, stat = "identity", position = position_dodge(0.5), outlier.shape = NA, width = 0.5, size = 0.3) +
  theme_bw()+
  xlab("Expenditure Quintile")+ ylab("District Heat Expenditure Share")+
  geom_point(aes(y = mean), shape = 23, size = 1.3, stroke = 0.2, fill = "white")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0), breaks = c(0,0.05, 0.1, 0.15, 0.2))+
  coord_cartesian(ylim = c(0,round(max(data_7.2.3.3$y95 + 0.01),2)))+
  #coord_cartesian(ylim = c(0,0.3))+
  scale_fill_nejm()+
  ggtitle("District Heat Expenditure Shares")+
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
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.1,0.1,0,0), "cm"),
        panel.border = element_rect(size = 0.3))

# 7.2.4 Burden Direct vs. Indirect for Coal, Oil and Gas over per Quintile ####

data_7.2.4 <- data_7.2.0 %>%
  select(hh_id, Income_Group_5, hh_weights, 
         burden_GAS_direct, burden_GAS_indirect,
         burden_COAL_direct, burden_COAL_indirect,
         burden_P_C_direct, burden_P_C_indirect)%>%
  pivot_longer(burden_GAS_direct:burden_P_C_indirect, names_to = "Type", values_to = "burden")%>%
  group_by(Income_Group_5, Type)%>%
  summarise(
    y5  = wtd.quantile(burden, weights = hh_weights, probs = 0.05),
    y25 = wtd.quantile(burden, weights = hh_weights, probs = 0.25),
    y50 = wtd.quantile(burden, weights = hh_weights, probs = 0.5),
    y75 = wtd.quantile(burden, weights = hh_weights, probs = 0.75),
    y95 = wtd.quantile(burden, weights = hh_weights, probs = 0.95),
    mean = wtd.mean(   burden, weights = hh_weights))%>%
  ungroup()%>%
  mutate(Type_A = ifelse(Type == "burden_COAL_direct" | Type == "burden_COAL_indirect", "Coal",
                         ifelse(Type == "burden_GAS_direct" | Type == "burden_GAS_indirect", "Gas", "Liquid Fuel")))%>%
  mutate(Type_B = ifelse(Type == "burden_COAL_direct" | Type == "burden_GAS_direct" | Type == "burden_P_C_direct", "Direct Effects", "Indirect Effects"))

plot_7.2.4.1 <- ggplot(filter(data_7.2.4, Type_A == "Gas"), aes(x = factor(Income_Group_5)))+
  geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95, fill = Type_B), stat = "identity", position = position_dodge(0.7), outlier.shape = NA, width = 0.5, size = 0.3) +
  theme_bw()+
  xlab("Expenditure Quintiles")+ ylab("Gas Price Incidence")+
  stat_summary(aes(y = mean, group = interaction(Income_Group_5, Type_B)), fun = "mean", geom = "point", position =  position_dodge(0.7), shape = 23, size = 1.3, stroke = 0.5, fill = "white")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0))+
  coord_cartesian(ylim = c(0,round(max(filter(data_7.2.4, Type_A == "Gas")$y95)+0.01,2)))+
  scale_fill_nejm()+
  labs(fill = "")+
  ggtitle("Gas Price Increase 138%")+
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
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.1,0.1,0,0), "cm"),
        panel.border = element_rect(size = 0.3))

plot_7.2.4.2 <- ggplot(filter(data_7.2.4, Type_A == "Coal"), aes(x = factor(Income_Group_5)))+
  geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95, fill = Type_B), stat = "identity", position = position_dodge(0.7), outlier.shape = NA, width = 0.5, size = 0.3) +
  theme_bw()+
  xlab("Expenditure Quintiles")+ ylab("Coal Price Incidence")+
  stat_summary(aes(y = mean, group = interaction(Income_Group_5, Type_B)), fun = "mean", geom = "point", position =  position_dodge(0.7), shape = 23, size = 1.3, stroke = 0.5, fill = "white")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0))+
  coord_cartesian(ylim = c(0,round(max(filter(data_7.2.4, Type_A == "Coal")$y95)+0.01,2)))+
  scale_fill_nejm()+
  labs(fill = "")+
  ggtitle("Coal Price Increase 15%")+
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
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.1,0.1,0,0), "cm"),
        panel.border = element_rect(size = 0.3))

plot_7.2.4.3 <- ggplot(filter(data_7.2.4, Type_A == "Liquid Fuel"), aes(x = factor(Income_Group_5)))+
  geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95, fill = Type_B), stat = "identity", position = position_dodge(0.7), outlier.shape = NA, width = 0.5, size = 0.3) +
  theme_bw()+
  xlab("Expenditure Quintiles")+ ylab("Liquid Fuel Price Incidence")+
  stat_summary(aes(y = mean, group = interaction(Income_Group_5, Type_B)), fun = "mean", geom = "point", position =  position_dodge(0.7), shape = 23, size = 1.3, stroke = 0.5, fill = "white")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0))+
  coord_cartesian(ylim = c(0,round(max(filter(data_7.2.4, Type_A == "Liquid Fuel")$y95)+0.01,2)))+
  scale_fill_nejm()+
  labs(fill = "Liquid Fuel Price Increase 15%")+
  ggtitle("")+
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
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.1,0.1,0,0), "cm"),
        panel.border = element_rect(size = 0.3))

# 7.2.5 Stapel-Diagramm Indirekte Effekte / Direkte Effekte ####

data_7.2.5 <- data_7.2.0b %>%
  left_join(select(carbon_pricing_incidence_0, hh_id, Income_Group_5))%>%
  select(hh_id, hh_weights, Income_Group_5, Electricity_burden_GAS_gas:Other_burden_P_C_p_c)%>%
  pivot_longer(Electricity_burden_GAS_gas:Other_burden_P_C_p_c, names_to = "Type", values_to = "burden")%>%
  mutate(burden = ifelse(is.na(burden),0,burden))%>%
  mutate(Type_A = ifelse(grepl("COAL", Type),"Coal",
                         ifelse(grepl("GAS", Type), "Gas",
                                ifelse(grepl("P_C", Type), "Liquid Fuels", NA))))%>%
  mutate(Type_B = ifelse(grepl("Food", Type), "Food", 
                         ifelse(grepl("Electricity", Type), "Electricity",
                                ifelse(grepl("Goods", Type), "Goods",
                                       ifelse(grepl("Services", Type), "Services", 
                                              ifelse(grepl("Other Energy", Type), "Other Energy", 
                                                     ifelse(grepl("Gas_Heating", Type), "Gas Heating",
                                                            ifelse(grepl("Oil_Heating", Type), "Oil Heating",
                                                                   ifelse(grepl("District_Heating", Type), "District Heating", "Other")))))))))%>%
  group_by(Income_Group_5, Type_A, Type_B)%>%
  summarise(
    mean = wtd.mean(burden, weights = hh_weights))%>%
  ungroup()%>%
  group_by(Income_Group_5, Type_A)%>%
  mutate(mean_agg = sum(mean))%>%
  ungroup()
  
plot_7.2.5.1 <- ggplot(filter(data_7.2.5, Type_A == "Gas"), aes(x = factor(Income_Group_5)))+
  geom_col(aes(y = mean, fill = Type_B), width = 0.5, position = "stack", colour = "black")+
  theme_bw()+
  xlab("Expenditure Quintile")+ ylab("Average Burden in Percent")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0))+
  coord_cartesian(ylim = c(0, round(max(filter(data_7.2.5, Type_A == "Gas")$mean_agg)+0.01,2)))+
  #coord_cartesian(ylim = c(0,0.3))+
  labs(fill = "")+
  ggtitle("Decomposition - Gas Price Increase 138%")+
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
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.1,0.1,0,0), "cm"),
        panel.border = element_rect(size = 0.3))

plot_7.2.5.2 <- ggplot(filter(data_7.2.5, Type_A == "Coal"), aes(x = factor(Income_Group_5)))+
  geom_col(aes(y = mean, fill = Type_B), width = 0.5, position = "stack", colour = "black")+
  theme_bw()+
  xlab("Expenditure Quintile")+ ylab("Average Burden in Percent")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1), expand = c(0,0))+
  coord_cartesian(ylim = c(0, round(max(filter(data_7.2.5, Type_A == "Coal")$mean_agg)+0.01,2)))+
  #coord_cartesian(ylim = c(0,0.3))+
  labs(fill = "")+
  ggtitle("Decomposition - Coal Price Increase 15%")+
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
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.1,0.1,0,0), "cm"),
        panel.border = element_rect(size = 0.3))

plot_7.2.5.3 <- ggplot(filter(data_7.2.5, Type_A == "Liquid Fuels"), aes(x = factor(Income_Group_5)))+
  geom_col(aes(y = mean, fill = Type_B), width = 0.5, position = "stack", colour = "black")+
  theme_bw()+
  xlab("Expenditure Quintile")+ ylab("Average Burden in Percent")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1), expand = c(0,0))+
  coord_cartesian(ylim = c(0, round(max(filter(data_7.2.5, Type_A == "Liquid Fuels")$mean_agg)+0.01,2)))+
  #coord_cartesian(ylim = c(0,0.3))+
  labs(fill = "")+
  ggtitle("Decomposition - Liquid Fuel Price Increase 15%")+
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
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.1,0.1,0,0), "cm"),
        panel.border = element_rect(size = 0.3))

# 7.2.6 Differentiation by Gas-Expenditures ####

# a) Gas-Expenditures Yes/No - for Gas Price Increases

data_7.2.6.1 <- data_7.2.0 %>%
  mutate(Type = ifelse(Heating_Gas > 0, "Heats with Gas", "No heating with Gas"))%>%
  group_by(Type)%>%
  summarise(
    y5  = wtd.quantile(burden_GAS_gas, weights = hh_weights, probs = 0.05),
    y25 = wtd.quantile(burden_GAS_gas, weights = hh_weights, probs = 0.25),
    y50 = wtd.quantile(burden_GAS_gas, weights = hh_weights, probs = 0.5),
    y75 = wtd.quantile(burden_GAS_gas, weights = hh_weights, probs = 0.75),
    y95 = wtd.quantile(burden_GAS_gas, weights = hh_weights, probs = 0.95),
    mean = wtd.mean(   burden_GAS_gas, weights = hh_weights))%>%
  ungroup()
  
plot_7.2.6.1 <- ggplot(data_7.2.6.1, aes(x = factor(Type)))+
  geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95), fill = "grey", alpha = 1, stat = "identity", position = position_dodge(0.5), outlier.shape = NA, width = 0.5, size = 0.3) +
  theme_bw()+
  xlab("")+ ylab("Gas Price Incidence")+
  geom_point(aes(y = mean), shape = 23, size = 1.3, stroke = 0.2, fill = "white")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0))+
  coord_cartesian(ylim = c(0,round(max(data_7.2.6.1$y95)+0.01,2)))+
  #coord_cartesian(ylim = c(0,0.3))+
  ggtitle("")+
  #guides(fill = guide_legend(nrow = 3))+
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
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.1,0.1,0,0), "cm"),
        panel.border = element_rect(size = 0.3))


# b) Gas-Expenditures Yes/No & Quintiles - for Gas Price Increases

data_7.2.6.2 <- data_7.2.0 %>%
  mutate(Type = ifelse(Heating_Gas > 0, "Heats with Gas", "No heating with Gas"))%>%
  group_by(Income_Group_5, Type)%>%
  summarise(
    y5  = wtd.quantile(burden_GAS_gas, weights = hh_weights, probs = 0.05),
    y25 = wtd.quantile(burden_GAS_gas, weights = hh_weights, probs = 0.25),
    y50 = wtd.quantile(burden_GAS_gas, weights = hh_weights, probs = 0.5),
    y75 = wtd.quantile(burden_GAS_gas, weights = hh_weights, probs = 0.75),
    y95 = wtd.quantile(burden_GAS_gas, weights = hh_weights, probs = 0.95),
    mean = wtd.mean(   burden_GAS_gas, weights = hh_weights))%>%
  ungroup()

plot_7.2.6.2 <- ggplot(data_7.2.6.2, aes(fill = factor(Type), x = factor(Income_Group_5)))+
  geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95,
                   group = interaction(Type, Income_Group_5)), alpha = 1, stat = "identity", position = position_dodge(0.5), outlier.shape = NA, width = 0.5, size = 0.3) +
  theme_bw()+
  xlab("Expenditure Quintile")+ ylab("Gas Price Incidence")+
  geom_point(aes(y = mean, group = interaction(Type, Income_Group_5)), shape = 23, size = 1.3, stroke = 0.2, fill = "white",
             position = position_dodge(0.5))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0))+
  coord_cartesian(ylim = c(0,round(max(data_7.2.6.2$y95)+0.01,2)))+
  scale_fill_nejm()+
  labs(fill = "")+
  #guides(fill = guide_legend(nrow = 3))+
  #coord_cartesian(ylim = c(0,0.3))+
  ggtitle("")+
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
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.1,0.1,0,0), "cm"),
        panel.border = element_rect(size = 0.3))

# c) Predominant Heating: Oil, Gas, District Heating - for Gas and Oil

data_7.2.6.3 <- data_7.2.0 %>%
  mutate(Type = ifelse(Heating_Gas > Heating_Oil & Heating_Gas > Heating_District_Heat, "Gas", 
                       ifelse(Heating_Oil > Heating_Gas & Heating_Oil > Heating_District_Heat, "Oil", 
                              ifelse(Heating_District_Heat > Heating_Gas & Heating_District_Heat > Heating_Oil, "District Heat", "No Heating"))))%>%
  select(hh_id, hh_weights, burden_GAS_gas, burden_P_C_p_c, Type, Income_Group_5)%>%
  pivot_longer(burden_GAS_gas:burden_P_C_p_c, names_to = "Type_A", values_to = "burden")

data_7.2.6.3.1 <- data_7.2.6.3 %>%
  group_by(Type_A, Type)%>%
  summarise(
    y5  = wtd.quantile(burden, weights = hh_weights, probs = 0.05),
    y25 = wtd.quantile(burden, weights = hh_weights, probs = 0.25),
    y50 = wtd.quantile(burden, weights = hh_weights, probs = 0.5),
    y75 = wtd.quantile(burden, weights = hh_weights, probs = 0.75),
    y95 = wtd.quantile(burden, weights = hh_weights, probs = 0.95),
    mean = wtd.mean(   burden, weights = hh_weights))%>%
  ungroup()

plot_7.2.6.3.1 <- ggplot(filter(data_7.2.6.3.1, Type_A =="burden_GAS_gas"), aes(x = factor(Type)))+
  geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95), fill = "grey", alpha = 1, stat = "identity", position = position_dodge(0.5), outlier.shape = NA, width = 0.5, size = 0.3) +
  theme_bw()+
  xlab("Predominant Heating Fuel")+ ylab("Gas Price Incidence")+
  geom_point(aes(y = mean), shape = 23, size = 1.3, stroke = 0.2, fill = "white")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0))+
  coord_cartesian(ylim = c(0,round(max(filter(data_7.2.6.3.1, Type_A == "burden_GAS_gas")$y95)+0.01,2)))+
  #coord_cartesian(ylim = c(0,0.3))+
  ggtitle("")+
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
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.1,0.1,0,0), "cm"),
        panel.border = element_rect(size = 0.3))

plot_7.2.6.3.1b <- ggplot(filter(data_7.2.6.3.1, Type_A =="burden_P_C_p_c"), aes(x = factor(Type)))+
  geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95), fill = "grey", alpha = 1, stat = "identity", position = position_dodge(0.5), outlier.shape = NA, width = 0.5, size = 0.3) +
  theme_bw()+
  xlab("Predominant Heating Fuel")+ ylab("Liquid Fuel Price Incidence")+
  geom_point(aes(y = mean), shape = 23, size = 1.3, stroke = 0.2, fill = "white")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0))+
  coord_cartesian(ylim = c(0,round(max(filter(data_7.2.6.3.1, Type_A == "burden_P_C_p_c")$y95)+0.01,2)))+
  #coord_cartesian(ylim = c(0,0.3))+
  ggtitle("")+
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
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.1,0.1,0,0), "cm"),
        panel.border = element_rect(size = 0.3))

# d) Predominant Heating and Quintiles - for Gas and Oil

data_7.2.6.3.2 <- data_7.2.6.3 %>%
  group_by(Income_Group_5, Type_A, Type)%>%
  summarise(
    y5  = wtd.quantile(burden, weights = hh_weights, probs = 0.05),
    y25 = wtd.quantile(burden, weights = hh_weights, probs = 0.25),
    y50 = wtd.quantile(burden, weights = hh_weights, probs = 0.5),
    y75 = wtd.quantile(burden, weights = hh_weights, probs = 0.75),
    y95 = wtd.quantile(burden, weights = hh_weights, probs = 0.95),
    mean = wtd.mean(   burden, weights = hh_weights))%>%
  ungroup()

plot_7.2.6.3.2 <- ggplot(filter(data_7.2.6.3.2, Type_A == "burden_GAS_gas"), aes(fill = factor(Type), x = factor(Income_Group_5)))+
  geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95,
                   group = interaction(Type, Income_Group_5)), alpha = 1, stat = "identity", position = position_dodge(0.7), outlier.shape = NA, width = 0.5, size = 0.3) +
  theme_bw()+
  xlab("Expenditure Quintile")+ ylab("Gas Price Incidence")+
  geom_point(aes(y = mean, group = interaction(Type, Income_Group_5)), shape = 23, size = 1.3, stroke = 0.2, fill = "white",
             position = position_dodge(0.7))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0))+
  coord_cartesian(ylim = c(0,round(max(filter(data_7.2.6.3.2, Type_A == "burden_GAS_gas")$y95)+0.01,2)))+
  #coord_cartesian(ylim = c(0,0.3))+
  ggtitle("")+
  scale_fill_nejm()+
  labs(fill = "Predominant Heating Fuel")+
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
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.1,0.1,0,0), "cm"),
        panel.border = element_rect(size = 0.3))

plot_7.2.6.3.2b <- ggplot(filter(data_7.2.6.3.2, Type_A == "burden_P_C_p_c"), aes(fill = factor(Type), x = factor(Income_Group_5)))+
  geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95,
                   group = interaction(Type, Income_Group_5)), alpha = 1, stat = "identity", position = position_dodge(0.7), outlier.shape = NA, width = 0.5, size = 0.3) +
  theme_bw()+
  xlab("Expenditure Quintile")+ ylab("Liquid Fuel Price Incidence")+
  geom_point(aes(y = mean, group = interaction(Type, Income_Group_5)), shape = 23, size = 1.3, stroke = 0.2, fill = "white",
             position = position_dodge(0.7))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0))+
  coord_cartesian(ylim = c(0,round(max(filter(data_7.2.6.3.2, Type_A == "burden_P_C_p_c")$y95)+0.01,2)))+
  #coord_cartesian(ylim = c(0,0.3))+
  ggtitle("")+
  scale_fill_nejm()+
  labs(fill = "Predominant Heating Fuel")+
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
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.1,0.1,0,0), "cm"),
        panel.border = element_rect(size = 0.3))

# 7.2.7 Modelling Lump-Sum Transfers ####

data_7.2.7 <- data_7.2.0 %>%
  mutate(exp_add_gas = hh_weights * exp_GAS_gas)

compensation <- sum(data_7.2.7$exp_add_gas)/sum(data_7.2.7$hh_weights)

data_7.2.7 <- data_7.2.7 %>%
  mutate(exp_GAS_gas_compensation = compensation - exp_GAS_gas,
         burden_GAS_compensation  = exp_GAS_gas_compensation/hh_expenditures_USD_2014,
         burden_GAS_gas = -burden_GAS_gas)%>%
  select(hh_id, hh_weights, Income_Group_5, burden_GAS_gas, burden_GAS_compensation)%>%
  pivot_longer(starts_with("burden"), names_to = "Type", values_to = "burden")%>%
  group_by(Income_Group_5, Type)%>%
  summarise(
    y5  = wtd.quantile(burden, weights = hh_weights, probs = 0.05),
    y25 = wtd.quantile(burden, weights = hh_weights, probs = 0.25),
    y50 = wtd.quantile(burden, weights = hh_weights, probs = 0.5),
    y75 = wtd.quantile(burden, weights = hh_weights, probs = 0.75),
    y95 = wtd.quantile(burden, weights = hh_weights, probs = 0.95),
    mean = wtd.mean(   burden, weights = hh_weights))%>%
  ungroup()%>%
  mutate(Type = ifelse(Type == "burden_GAS_gas", "Uncompensated", "Compensated"))

plot_7.2.7 <- ggplot(data_7.2.7, aes(fill = factor(Type), x = factor(Income_Group_5)))+
  geom_hline(aes(yintercept = 0), size = 0.5)+
  geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95,
                   group = interaction(Type, Income_Group_5)), alpha = 1, stat = "identity", position = position_dodge(0.7), outlier.shape = NA, width = 0.5, size = 0.3) +
  theme_bw()+
  xlab("Expenditure Quintile")+ ylab("Gas Price Incidence")+
  geom_point(aes(y = mean, group = interaction(Type, Income_Group_5)), shape = 23, size = 1.3, stroke = 0.2, fill = "white",
             position = position_dodge(0.7))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0))+
  coord_cartesian(ylim = c(round(min(data_7.2.7$y5)-0.01,2),round(max(data_7.2.7$y95)+0.01,2)))+
  #coord_cartesian(ylim = c(0,0.3))+
  scale_fill_nejm()+
  labs(fill = "Scenario")+
  ggtitle("")+
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
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.1,0.1,0,0), "cm"),
        panel.border = element_rect(size = 0.3))

# 7.2.8 Sum of Additional Costs ####

data_7.2.8.0 <- data_7.2.0 %>%
  mutate(exp_add_gas = hh_weights * exp_GAS_gas,
         exp_add_coal = hh_weights * exp_COAL_coal,
         exp_add_p_c = hh_weights * exp_P_C_p_c)

sum_exp_gas <- sum(data_7.2.8.0$exp_add_gas)
sum_exp_coal <- sum(data_7.2.8.0$exp_add_coal)
sum_exp_p_c <- sum(data_7.2.8.0$exp_add_p_c)
sum_hhs <- sum(data_7.2.8.0$hh_weights)

data_frame_7.2.8 <- data.frame(Country = i,
                               Expenditures_Total_Gas = sum_exp_gas,
                               Expenditures_Total_Coal = sum_exp_coal,
                               Expenditures_Total_P_C = sum_exp_p_c,
                               No_HHs = sum_hhs)
agg_expenditures <- agg_expenditures %>%
  bind_rows(data_frame_7.2.8)

# 7.2.X Join Figure for Country ####

plot_A <- ggarrange(plot_7.2.1.1,
                    plot_7.2.1.2,
                    plot_7.2.1.3, nrow = 1)

plot_B <- ggarrange(plot_7.2.3.1,
                    plot_7.2.3.2,
                    plot_7.2.3.3, nrow = 1)

plot_C <- ggarrange(plot_7.2.4.1,
                    plot_7.2.4.2,
                    plot_7.2.4.3, nrow = 1, common.legend = TRUE, legend = "bottom")

plot_D <- ggarrange(plot_7.2.5.1,
                    plot_7.2.5.2,
                    plot_7.2.5.3, nrow = 1, common.legend = TRUE, legend = "bottom")

plot_E <- ggarrange(plot_7.2.6.3.1,
                    plot_7.2.6.3.1b,
                    plot_7.2.6.3.2,
                    plot_7.2.6.3.2b, nrow = 2, ncol = 2, common.legend = TRUE, legend = "bottom")

plot_F <- ggarrange(plot_7.2.2, plot_7.2.7, nrow = 1)


plot_7.2.X <- ggarrange(plot_A,
                        plot_F,
                        plot_B, 
                        plot_C,
                        plot_D,
                        plot_E,
                        labels = c("a)", "b)", "c)", "d)", "e)", "f)"))

# jpeg(sprintf("K:/WorkInProgress/2021_Carbon_Footprint_Analysis/Gas Price Analysis/Figures/Figure_10_Joint/Figure_8_%s.jpg", i), width = 60, height = 26.333333, unit = "cm", res = 400)
# print(plot_7.2.X)
# dev.off()

print(i)

}

agg_expenditures_1 <- agg_expenditures

#write.xlsx(agg_expenditures_1, "K:/WorkInProgress/2021_Carbon_Footprint_Analysis/Gas Price Analysis/Compensation_Requirement.xlsx")

# 7.3   Cross-European Analyses ####

data_7.3 <- left_join(carbon_pricing_incidence_0, household_information_0)

# 7.3.1 Compute Cross-European Income Groups ####

ppp_rates_0 <- read.xlsx("K:/WorkInProgress/2021_Carbon_Footprint_Analysis/Gas Price Analysis/WB_PPP_Conversion_Exchange_Rates.xlsx")%>%
  rename(Country = Country.Name, Value = "2015.[YR2015]")%>%
  select(Country, Value, Series.Name)
ppp_rates_1 <- filter(ppp_rates_0, Series.Name == "PPP conversion factor, private consumption (LCU per international $)")%>%
  rename(Value_PPP = Value)%>%
  mutate(Value_PPP = 1/Value_PPP)
exchange_rates_1 <- filter(ppp_rates_0, Series.Name == "Official exchange rate (LCU per US$, period average)")%>%
  mutate(Value = ifelse(is.na(Value),0.9012964, Value))%>%
  rename(Value_Exchange = Value)

data_7.3.1 <- data_7.3 %>%
  left_join(exchange_rates_1)%>%
  # First we convert EURO values to Local Currencies
  mutate(hh_expenditures_LCU = hh_expenditures_USD_2014*Value_Exchange)%>%
  left_join(ppp_rates_1, by = "Country")%>%
  # Then we convert Local Currencies to International PPP_adjusted Dollars (2015 PPP-rates)
  mutate(hh_expenditures_USD_PPP       = hh_expenditures_LCU*Value_PPP)%>%
  mutate(hh_expenditures_pc            = hh_expenditures/hh_size,
         hh_expenditures_USD_2014_pc   = hh_expenditures_USD_2014/hh_size,
         hh_expenditures_USD_PPP_pc    = hh_expenditures_USD_PPP/hh_size)%>%
  mutate(Income_Group_100_hh_pc         = as.numeric(binning(hh_expenditures_pc,          bins = 100, method = c("wtd.quantile"), weights = hh_weights)),
         Income_Group_100_hh_USD_pc     = as.numeric(binning(hh_expenditures_USD_2014_pc, bins = 100, method = c("wtd.quantile"), weights = hh_weights)),
         Income_Group_100_hh_USD_PPP_pc = as.numeric(binning(hh_expenditures_USD_PPP_pc,  bins = 100, method = c("wtd.quantile"), weights = hh_weights)),
         Income_Group_10_hh_USD_PPP_pc  = as.numeric(binning(hh_expenditures_USD_PPP_pc,  bins = 10,  method = c("wtd.quantile"), weights = hh_weights)))

# 7.3.2 Vertical Distributional Effects ####

data_7.3.2.1 <- data_7.3.1 %>%
  select(hh_id, Country, hh_weights, burden_GAS_gas, burden_COAL_coal, burden_P_C_p_c, starts_with("Income_Group_100"))%>%
  pivot_longer(starts_with("burden"), names_to = "Type", values_to = "burden")%>%
  group_by(Income_Group_100_hh_USD_pc, Type)%>%
  summarise(
    y5  = wtd.quantile(burden, weights = hh_weights, probs = 0.05),
    y25 = wtd.quantile(burden, weights = hh_weights, probs = 0.25),
    y50 = wtd.quantile(burden, weights = hh_weights, probs = 0.5),
    y75 = wtd.quantile(burden, weights = hh_weights, probs = 0.75),
    y95 = wtd.quantile(burden, weights = hh_weights, probs = 0.95),
    mean = wtd.mean(   burden, weights = hh_weights))%>%
  ungroup()%>%
  mutate(Type = ifelse(Type == "burden_GAS_gas", "Gas Price Increase 138%",
                       ifelse(Type == "burden_COAL_coal", "Coal Price Increase 15%", "Liquid Fuel Price Increase 15%")))


data_7.3.2.2 <- data_7.3.1 %>%
  select(hh_id, Country, hh_weights, burden_GAS_gas, burden_COAL_coal, burden_P_C_p_c, starts_with("Income_Group_100"))%>%
  pivot_longer(starts_with("burden"), names_to = "Type", values_to = "burden")%>%
  group_by(Income_Group_100_hh_USD_PPP_pc, Type)%>%
  summarise(
    y5  = wtd.quantile(burden, weights = hh_weights, probs = 0.05),
    y25 = wtd.quantile(burden, weights = hh_weights, probs = 0.25),
    y50 = wtd.quantile(burden, weights = hh_weights, probs = 0.5),
    y75 = wtd.quantile(burden, weights = hh_weights, probs = 0.75),
    y95 = wtd.quantile(burden, weights = hh_weights, probs = 0.95),
    mean = wtd.mean(   burden, weights = hh_weights))%>%
  ungroup()%>%
  mutate(Type = ifelse(Type == "burden_GAS_gas", "Gas Price Increase 138%",
                       ifelse(Type == "burden_COAL_coal", "Coal Price Increase 15%", "Liquid Fuel Price Increase 15%")))

plot_7.3.2.1.1 <- ggplot(filter(data_7.3.2.1, Type == "Gas Price Increase 138%"), aes(x = factor(Income_Group_100_hh_USD_pc)))+
  geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95), fill = "grey", alpha = 1, stat = "identity", position = position_dodge(0.5), outlier.shape = NA, width = 0.5, size = 0.3) +
  theme_bw()+
  xlab("European Expenditure Percentile (EUR pc)")+ ylab("Gas Price Incidence")+
  geom_point(aes(y = mean), shape = 23, size = 1.3, stroke = 0.2, fill = "white")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0))+
  coord_cartesian(ylim = c(0,0.35))+
  scale_x_discrete(breaks = c(1, rep("",23),25,rep("",24),50, rep("",24),75,rep("",24),100))+
  #coord_cartesian(ylim = c(0,0.3))+
  ggtitle("Gas Price Increase 138%")+
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
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.2,0.2,0.2,0.2), "cm"),
        panel.border = element_rect(size = 0.3))

plot_7.3.2.1.2 <- ggplot(filter(data_7.3.2.1, Type == "Coal Price Increase 15%"), aes(x = factor(Income_Group_100_hh_USD_pc)))+
  geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95), fill = "grey", alpha = 1, stat = "identity", position = position_dodge(0.5), outlier.shape = NA, width = 0.5, size = 0.3) +
  theme_bw()+
  xlab("European Expenditure Percentile (EUR pc)")+ ylab("Coal Price Incidence")+
  geom_point(aes(y = mean), shape = 23, size = 1.3, stroke = 0.2, fill = "white")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1), expand = c(0,0))+
  scale_x_discrete(breaks = c(1, rep("",23),25,rep("",24),50, rep("",24),75,rep("",24),100))+
  coord_cartesian(ylim = c(0,0.01))+
  #coord_cartesian(ylim = c(0,0.3))+
  ggtitle("Coal Price Increase 15%")+
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
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.2,0.2,0.2,0.2), "cm"),
        panel.border = element_rect(size = 0.3))

plot_7.3.2.1.3 <- ggplot(filter(data_7.3.2.1, Type == "Liquid Fuel Price Increase 15%"), aes(x = factor(Income_Group_100_hh_USD_pc)))+
  geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95), fill = "grey", alpha = 1, stat = "identity", position = position_dodge(0.5), outlier.shape = NA, width = 0.5, size = 0.3) +
  theme_bw()+
  xlab("European Expenditure Percentile (EUR pc)")+ ylab("Liquid Fuel Price Incidence")+
  scale_x_discrete(breaks = c(1, rep("",23),25,rep("",24),50, rep("",24),75,rep("",24),100))+
  geom_point(aes(y = mean), shape = 23, size = 1.3, stroke = 0.2, fill = "white")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0))+
  coord_cartesian(ylim = c(0,0.04))+
  #coord_cartesian(ylim = c(0,0.3))+
  ggtitle("Liquid Fuel Price Increase 15%")+
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
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.2,0.2,0.2,0.2), "cm"),
        panel.border = element_rect(size = 0.3))

plot_7.3.2.2.1 <- ggplot(filter(data_7.3.2.2, Type == "Gas Price Increase 138%"), aes(x = factor(Income_Group_100_hh_USD_PPP_pc)))+
  geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95), fill = "lightgrey", alpha = 1, stat = "identity", position = position_dodge(0.5), outlier.shape = NA, width = 0.5, size = 0.3) +
  theme_bw()+
  xlab("European Expenditure Percentile (USD PPP pc)")+ ylab("Gas Price Incidence")+
  geom_point(aes(y = mean), shape = 23, size = 1.3, stroke = 0.2, fill = "white")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0))+
  scale_x_discrete(breaks = c(1, rep("",23),25,rep("",24),50, rep("",24),75,rep("",24),100))+
  coord_cartesian(ylim = c(0,0.38))+
  #coord_cartesian(ylim = c(0,0.3))+
  ggtitle("Gas Price Increase 138%")+
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
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.2,0.2,0.2,0.2), "cm"),
        panel.border = element_rect(size = 0.3))

plot_7.3.2.2.2 <- ggplot(filter(data_7.3.2.2, Type == "Coal Price Increase 15%"), aes(x = factor(Income_Group_100_hh_USD_PPP_pc)))+
  geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95), fill = "lightgrey", alpha = 1, stat = "identity", position = position_dodge(0.5), outlier.shape = NA, width = 0.5, size = 0.3) +
  theme_bw()+
  xlab("European Expenditure Percentile (USD PPP pc)")+ ylab("Coal Price Incidence")+
  geom_point(aes(y = mean), shape = 23, size = 1.3, stroke = 0.2, fill = "white")+
  scale_x_discrete(breaks = c(1, rep("",23),25,rep("",24),50, rep("",24),75,rep("",24),100))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1), expand = c(0,0))+
  coord_cartesian(ylim = c(0,0.022))+
  #coord_cartesian(ylim = c(0,0.3))+
  ggtitle("Coal Price Increase 30%")+
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
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.2,0.2,0.2,0.2), "cm"),
        panel.border = element_rect(size = 0.3))

plot_7.3.2.2.3 <- ggplot(filter(data_7.3.2.2, Type == "Liquid Fuel Price Increase 15%"), aes(x = factor(Income_Group_100_hh_USD_PPP_pc)))+
  geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95), fill = "lightgrey", alpha = 1, stat = "identity", position = position_dodge(0.5), outlier.shape = NA, width = 0.5, size = 0.3) +
  theme_bw()+
  xlab("European Expenditure Percentile (USD PPP pc)")+ ylab("Liquid Fuel Price Incidence")+
  scale_x_discrete(breaks = c(1, rep("",23),25,rep("",24),50, rep("",24),75,rep("",24),100))+
  geom_point(aes(y = mean), shape = 23, size = 1.3, stroke = 0.2, fill = "white")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0))+
  coord_cartesian(ylim = c(0,0.081))+
  #coord_cartesian(ylim = c(0,0.3))+
  ggtitle("Liquid Fuel Price Increase 30%")+
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
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.2,0.2,0.2,0.2), "cm"),
        panel.border = element_rect(size = 0.3))

data_7.3.2.3 <- data_7.3.1 %>%
  select(hh_id, Country, hh_weights, burden_GAS_gas, burden_COAL_coal, burden_P_C_p_c, starts_with("Income_Group_10"))%>%
  mutate(burden_total = burden_GAS_gas + burden_COAL_coal + burden_P_C_p_c)%>%
  group_by(Income_Group_10_hh_USD_PPP_pc)%>%
  summarise(
    y5  = wtd.quantile(burden_total, weights = hh_weights, probs = 0.05),
    y25 = wtd.quantile(burden_total, weights = hh_weights, probs = 0.25),
    y50 = wtd.quantile(burden_total, weights = hh_weights, probs = 0.5),
    y75 = wtd.quantile(burden_total, weights = hh_weights, probs = 0.75),
    y95 = wtd.quantile(burden_total, weights = hh_weights, probs = 0.95),
    mean = wtd.mean(   burden_total, weights = hh_weights),
    mean_coal = wtd.mean(burden_COAL_coal, weights = hh_weights),
    mean_p_c  = wtd.mean(burden_P_C_p_c,   weights = hh_weights),
    mean_gas  = wtd.mean(burden_GAS_gas,   weights = hh_weights))%>%
  ungroup()

data_7.3.2.4 <- data_7.3.2.3 %>%
  pivot_longer(starts_with("mean"), names_to = "Type", values_to = "Mean")%>%
  #mutate_at(vars(y5:y95), ~ ifelse(Type == "mean", ., NA))%>%
  select(Income_Group_10_hh_USD_PPP_pc, Type, Mean)%>%
  mutate(Type_A = ifelse(Type == "mean", "Total",
                         ifelse(Type == "mean_coal", "Coal Price Increase 30%",
                                ifelse(Type == "mean_p_c", "Liquid Fuel Price Increase 30%",
                                       ifelse(Type == "mean_gas", "Gas Price Increase 138%", NA)))))

data_7.3.2.4$Type_A <- factor(data_7.3.2.4$Type_A, levels = c("Total",
                                                          "Gas Price Increase 138%",
                                                          "Liquid Fuel Price Increase 30%",
                                                          "Coal Price Increase 30%"))


plot_7.3.2.3 <- ggplot()+
  geom_boxplot(data = data_7.3.2.3, aes(x = factor(Income_Group_10_hh_USD_PPP_pc), ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95), fill = "lightgrey", colour = "black", alpha = 1, stat = "identity", position = position_dodge(0.5), outlier.shape = NA, width = 0.5, size = 0.3) +
  geom_segment(aes(x = 1.01, xend = 0.99, y = 0.13116240, yend = 0.13116240, colour = "Median Incidence"), size = 0.8)+
  theme_bw()+
  xlab("European Expenditure Decile (EUR PPP pc)")+ ylab("Additional Costs (in % of Total Expenditures)")+
  geom_point(data = data_7.3.2.4, aes(y = Mean, fill = Type_A, x = factor(Income_Group_10_hh_USD_PPP_pc), shape = Type_A), size = 1.8, stroke = 0.5)+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0))+
  scale_x_discrete(labels = c("1 \n Poorest \n 10 Percent", seq(2,9,1), "10 \n Richest \n 10 Percent"))+
  coord_cartesian(ylim = c(0,0.38))+
  guides(fill = guide_legend(nrow = 2, order = 1, override.aes = list(shape = c(21,22,23,24))), colour = guide_legend(order = 2), shape = "none")+
  labs(fill = "Average Incidence", alpha = "")+
  scale_fill_manual(values = c("white", "#20854EFF", "#0072B5FF", "#BC3C29FF"))+
  scale_colour_manual("", values = "black", breaks = "Median Incidence")+
  scale_shape_manual(values = c(21,22,23,24))+
  #coord_cartesian(ylim = c(0,0.3))+
  ggtitle("")+
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
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        legend.box = "vertical",
        plot.margin = unit(c(0.2,0.2,0.2,0.2), "cm"),
        panel.border = element_rect(size = 0.3))

jpeg("C:/Users/misl/ownCloud/EU_Gasprice_Analysis/Figures/2022_04_26_Figures/Figure_2a.jpg", width = 15, height = 14 , unit = "cm", res = 400)
print(plot_7.3.2.3)

dev.off()

data_7.3.2.5 <- data_7.3.1 %>%
  select(hh_id, Country, hh_weights, burden_GAS_gas, burden_COAL_coal, burden_P_C_p_c, starts_with("Income_Group_10"))%>%
  mutate(burden_100_gas   = burden_GAS_gas,
         burden_100_coal  = burden_COAL_coal,
         burden_100_p_c   = burden_P_C_p_c)%>%
  mutate(burden_100_total = burden_100_gas + burden_100_coal + burden_100_p_c)%>%
  group_by(Income_Group_10_hh_USD_PPP_pc)%>%
  summarise(
    y5  = wtd.quantile(burden_100_total, weights = hh_weights, probs = 0.05),
    y25 = wtd.quantile(burden_100_total, weights = hh_weights, probs = 0.25),
    y50 = wtd.quantile(burden_100_total, weights = hh_weights, probs = 0.5),
    y75 = wtd.quantile(burden_100_total, weights = hh_weights, probs = 0.75),
    y95 = wtd.quantile(burden_100_total, weights = hh_weights, probs = 0.95),
    mean = wtd.mean(   burden_100_total, weights = hh_weights),
    mean_coal = wtd.mean(burden_100_coal, weights = hh_weights),
    mean_p_c  = wtd.mean(burden_100_p_c,   weights = hh_weights),
    mean_gas  = wtd.mean(burden_100_gas,   weights = hh_weights))%>%
  ungroup()

data_7.3.2.6 <- data_7.3.2.5 %>%
  pivot_longer(starts_with("mean"), names_to = "Type", values_to = "Mean")%>%
  #mutate_at(vars(y5:y95), ~ ifelse(Type == "mean", ., NA))%>%
  select(Income_Group_10_hh_USD_PPP_pc, Type, Mean)%>%
  mutate(Type_A = ifelse(Type == "mean", "All fuels",
                         ifelse(Type == "mean_coal", "Coal",
                                ifelse(Type == "mean_p_c", "Oil",
                                       ifelse(Type == "mean_gas", "Gas", NA)))))

data_7.3.2.6$Type_A <- factor(data_7.3.2.6$Type_A, levels = c("All fuels",
                                                              "Gas",
                                                              "Oil",
                                                              "Coal"))


plot_7.3.2.4 <- ggplot()+
  geom_boxplot(data = data_7.3.2.5, aes(x = factor(Income_Group_10_hh_USD_PPP_pc), ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95), fill = "lightgrey", colour = "black", alpha = 1, stat = "identity", position = position_dodge(0.5), outlier.shape = NA, width = 0.5, size = 0.3) +
  geom_segment(aes(x = 1.01, xend = 0.99, y = 0.13116240, yend = 0.13116240, colour = "Median incidence, all fuels"), size = 0.8)+
  theme_bw()+
  xlab("European Expenditure Decile")+ ylab("Additional Costs (in % of Total Expenditures)")+
  geom_point(data = data_7.3.2.6, aes(y = Mean, fill = Type_A, x = factor(Income_Group_10_hh_USD_PPP_pc), shape = Type_A), size = 1.8, stroke = 0.5)+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0))+
  scale_x_discrete(labels = c("1 \n Poorest \n 10 Percent", seq(2,9,1), "10 \n Richest \n 10 Percent"))+
  coord_cartesian(ylim = c(0,0.38))+
  guides(fill = guide_legend(nrow = 1, order = 1, override.aes = list(shape = c(21,22,23,24)), title.position = "top", title.hjust = 0.5), colour = guide_legend(order = 2), shape = "none")+
  scale_fill_manual(name = "Average incidence of fuel price increases", values = c("white", "#20854EFF", "#0072B5FF", "#BC3C29FF"))+
  scale_colour_manual("", values = "black", breaks = "Median incidence, all fuels")+
  scale_shape_manual (name = "Average incidence of fuel price increases", values = c(21,22,23,24))+
  #coord_cartesian(ylim = c(0,0.3))+
  ggtitle("")+
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
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        legend.box = "vertical",
        plot.margin = unit(c(0.2,0.2,0.2,0.2), "cm"),
        panel.border = element_rect(size = 0.3))

jpeg("C:/Users/misl/ownCloud/EU_Gasprice_Analysis/Figures/2022_05_20_Figures/Figure_A6.jpg", width = 15, height = 14 , unit = "cm", res = 400)
print(plot_7.3.2.4)

dev.off()

# 7.3.3 Introducing Gas ####

data_7.3.3.1 <- data_7.3.1 %>%
  select(hh_id, Country, hh_weights, burden_GAS_gas, starts_with("Income_Group_100"), Heating_Gas)%>%
  filter(Heating_Gas > 0)%>%
  pivot_longer(starts_with("burden"), names_to = "Type", values_to = "burden")%>%
  group_by(Income_Group_100_hh_USD_PPP_pc, Type)%>%
  summarise(
    y5  = wtd.quantile(burden, weights = hh_weights, probs = 0.05),
    y25 = wtd.quantile(burden, weights = hh_weights, probs = 0.25),
    y50 = wtd.quantile(burden, weights = hh_weights, probs = 0.5),
    y75 = wtd.quantile(burden, weights = hh_weights, probs = 0.75),
    y95 = wtd.quantile(burden, weights = hh_weights, probs = 0.95),
    mean = wtd.mean(   burden, weights = hh_weights))%>%
  ungroup()%>%
  mutate(Type = ifelse(Type == "burden_GAS_gas", "Gas Price Increase 138%",
                       ifelse(Type == "burden_COAL_coal", "Coal Price Increase 15%", "Liquid Fuel Price Increase 15%")))

plot_7.3.3.1 <- ggplot(filter(data_7.3.3.1, Type == "Gas Price Increase 138%"), aes(x = factor(Income_Group_100_hh_USD_PPP_pc)))+
  geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95), fill = "grey", alpha = 1, stat = "identity", position = position_dodge(0.5), outlier.shape = NA, width = 0.5, size = 0.3) +
  theme_bw()+
  xlab("European Expenditure Percentile (USD PPP pc)")+ ylab("Gas Price Incidence")+
  geom_point(aes(y = mean), shape = 23, size = 1.3, stroke = 0.2, fill = "white")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0))+
  scale_x_discrete(breaks = c(1, rep("",23),25,rep("",24),50, rep("",24),75,rep("",24),100))+
  coord_cartesian(ylim = c(0,0.5))+
  #coord_cartesian(ylim = c(0,0.3))+
  ggtitle("Gas Price Increase 138% - Households with Gas Expenditures")+
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
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.2,0.2,0.2,0.2), "cm"),
        panel.border = element_rect(size = 0.3))

data_7.3.3.2 <- data_7.3.1 %>%
  select(hh_id, Country, hh_weights, starts_with("Income_Group_100"), Heating_Gas)%>%
  mutate(Heating_Gas = ifelse(is.na(Heating_Gas),0, Heating_Gas))%>%
  mutate(weights_GAS = ifelse(Heating_Gas > 0, hh_weights,0))%>%
  group_by(Income_Group_100_hh_USD_PPP_pc)%>%
  summarise(weights_GAS = sum(weights_GAS),
            hh_weights = sum(hh_weights))%>%
  ungroup()%>%
  mutate(share = weights_GAS/hh_weights)

plot_7.3.3.2 <- ggplot(data_7.3.3.2, aes(x = factor(Income_Group_100_hh_USD_PPP_pc)))+
  theme_bw()+
  geom_col(aes(y = share), width = 0.7, colour = "black", fill = "lightgrey")+
  xlab("European Expenditure Percentile (USD PPP pc)")+ ylab("Households reporting Expenditures on Gas (in %)")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0))+
  scale_x_discrete(breaks = c(1, rep("",23),25,rep("",24),50, rep("",24),75,rep("",24),100))+
  coord_cartesian(ylim = c(0,1))+
  #coord_cartesian(ylim = c(0,0.3))+
  ggtitle("Households with Gas Expenditures")+
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
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.2,0.2,0.2,0.2), "cm"),
        panel.border = element_rect(size = 0.3))

plot_7.3.2.1 <- ggarrange(plot_7.3.2.2.1, plot_7.3.2.1.1)
plot_7.3.2.2 <- ggarrange(plot_7.3.2.2.2, plot_7.3.2.1.2)
plot_7.3.2.3 <- ggarrange(plot_7.3.2.2.3, plot_7.3.2.1.3)
plot_7.3.2.4 <- ggarrange(plot_7.3.2.2.1, plot_7.3.2.1.1,
                          plot_7.3.3.1, plot_7.3.3.2, nrow = 2, ncol = 2, labels = c("a)",
                                                                                     "b)",
                                                                                     "c)",
                                                                                     "d)"))

# jpeg("K:/WorkInProgress/2021_Carbon_Footprint_Analysis/Gas Price Analysis/Figures/Figure_11_Accross_Europe/Figure_11_%d.jpg", width = 30, height = 13.33 , unit = "cm", res = 400)
# print(plot_7.3.2.1)
# print(plot_7.3.2.2)
# print(plot_7.3.2.3)
# print(plot_7.3.2.4)
# 
# dev.off()

plot_7.3.2.5 <- ggarrange(plot_7.3.2.2.1, plot_7.3.2.2.2, plot_7.3.2.2.3,
                          nrow = 3, ncol = 1, labels = c("a)", "b)", "c)"))

jpeg("C:/Users/misl/ownCloud/EU_Gasprice_Analysis/Figures/2022_04_19_Figures/Figure_1.jpg", width = 15, height = 25 , unit = "cm", res = 400)
print(plot_7.3.2.5)

dev.off()

# 7.4   More Elaborate Transfers ####

data_7.4 <- left_join(indirect_expenditures_0, household_information_0)%>%
  left_join(carbon_pricing_incidence_0, by = "hh_id")%>%
  select(hh_id, hh_weights, Country, District_Heating_burden_GAS_gas, Gas_Heating_burden_GAS_gas,
         Heating_Gas, Heating_District_Heat, Income_Group_5, hh_expenditures_USD_2014)%>%
  mutate(District_Heating_exp = District_Heating_burden_GAS_gas*hh_expenditures_USD_2014,
         Gas_Heating_exp      = Gas_Heating_burden_GAS_gas*hh_expenditures_USD_2014)%>%
  filter(Income_Group_5 == 1 | Income_Group_5 == 2)%>%
  filter(!is.na(District_Heating_exp)|!is.na(Gas_Heating_exp))%>%
  group_by(Country)%>%
  summarise(mean_District_Heating_exp = wtd.mean(District_Heating_exp, hh_weights, na.rm = TRUE),
            mean_Gas_Heating_exp      = wtd.mean(Gas_Heating_exp, hh_weights, na.rm = TRUE))%>%
  ungroup()%>%
  mutate(mean_District_Heating_exp = ifelse(mean_District_Heating_exp == "NaN",0, mean_District_Heating_exp),
         mean_Gas_Heating_exp      = ifelse(mean_Gas_Heating_exp == "NaN",0, mean_Gas_Heating_exp))%>%
  mutate(bonus = mean_Gas_Heating_exp + mean_District_Heating_exp)

# This dataframe entails the bonusses for all households in the first income quintiles with non-negative additional costs

data_7.4.1 <- left_join(carbon_pricing_incidence_0, household_information_0) %>%
  left_join(data_7.4)%>%
  mutate(Heating_Gas = ifelse(is.na(Heating_Gas), 0, Heating_Gas),
         Heating_District_Heat = ifelse(is.na(Heating_District_Heat),0, Heating_District_Heat))%>%
  mutate(compensation = ifelse((Income_Group_5 == 1 | Income_Group_5 == 2) & Heating_Gas > 0 & Heating_District_Heat == 0, mean_Gas_Heating_exp,
                               ifelse((Income_Group_5 == 1 | Income_Group_5 == 2) & Heating_District_Heat > 0 & Heating_Gas == 0, mean_District_Heating_exp,
                                      ifelse((Income_Group_5 == 1 | Income_Group_5 == 2) & Heating_District_Heat > 0 & Heating_Gas > 0, bonus,0))))%>%
  mutate(exp_GAS_Gas_compensation = compensation - exp_GAS_gas,
         burden_GAS_compensation  = exp_GAS_Gas_compensation/hh_expenditures_USD_2014,
         burden_GAS_gas = - burden_GAS_gas)%>%
  select(hh_id, hh_weights, Country, Income_Group_5, burden_GAS_gas, burden_GAS_compensation)%>%
  pivot_longer(starts_with("burden"), names_to = "Type", values_to = "burden")%>%
  group_by(Country, Income_Group_5, Type)%>%
  summarise(
    y5  = wtd.quantile(burden, weights = hh_weights, probs = 0.05),
    y25 = wtd.quantile(burden, weights = hh_weights, probs = 0.25),
    y50 = wtd.quantile(burden, weights = hh_weights, probs = 0.5),
    y75 = wtd.quantile(burden, weights = hh_weights, probs = 0.75),
    y95 = wtd.quantile(burden, weights = hh_weights, probs = 0.95),
    mean = wtd.mean(   burden, weights = hh_weights))%>%
  ungroup()%>%
  mutate(Type = ifelse(Type == "burden_GAS_gas", "Uncompensated", "Compensated"))

plot_7.4.1 <- ggplot(data_7.4.1, aes(fill = factor(Type), x = factor(Income_Group_5)))+
  geom_hline(aes(yintercept = 0), size = 0.5)+
  geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95,
                   group = interaction(Type, Income_Group_5)), alpha = 1, stat = "identity", position = position_dodge(0.7), outlier.shape = NA, width = 0.5, size = 0.3) +
  theme_bw()+
  facet_wrap(. ~ Country, scales = "free_y")+
  xlab("Expenditure Quintile")+ ylab("Budget Change (in % of Total Expenditures)")+
  geom_point(aes(y = mean, group = interaction(Type, Income_Group_5)), shape = 23, size = 1.3, stroke = 0.2, fill = "white",
             position = position_dodge(0.7))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0))+
  scale_fill_nejm()+
  ggtitle("Scenario Compensating Average Gas / District Heat Expenditures to Lower Quintiles")+
  labs(fill = "")+
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
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.1,0.1,0,0), "cm"),
        panel.border = element_rect(size = 0.3))

# jpeg("K:/WorkInProgress/2021_Carbon_Footprint_Analysis/Gas Price Analysis/Figures/Figure_11_Accross_Europe/Figure_11_5_Compensation.jpg", width = 30, height = 13.33 , unit = "cm", res = 400)
# print(plot_7.4.1)
# 
# dev.off()

# Excel-Table 

data_7.4.2 <- left_join(carbon_pricing_incidence_0, household_information_0) %>%
  left_join(data_7.4)%>%
  mutate(Heating_Gas = ifelse(is.na(Heating_Gas), 0, Heating_Gas),
         Heating_District_Heat = ifelse(is.na(Heating_District_Heat),0, Heating_District_Heat))%>%
  mutate(compensation = ifelse((Income_Group_5 == 1 | Income_Group_5 == 2) & Heating_Gas > 0 & Heating_District_Heat == 0, mean_Gas_Heating_exp,
                               ifelse((Income_Group_5 == 1 | Income_Group_5 == 2) & Heating_District_Heat > 0 & Heating_Gas == 0, mean_District_Heating_exp,
                                      ifelse((Income_Group_5 == 1 | Income_Group_5 == 2) & Heating_District_Heat > 0 & Heating_Gas > 0, bonus,0))))%>%
  mutate(compensation_indicator = ifelse(compensation > 0,hh_weights,0))%>%
  mutate(compensation_weighted = hh_weights*compensation)%>%
  group_by(Country)%>%
  summarise(compensation_hhs = sum(compensation_indicator),
            hhs = sum(hh_weights),
            compensation_weighted = sum(compensation_weighted))%>%
  ungroup()%>%
  mutate(share_compensated = compensation_hhs/hhs)%>%
  select(Country, compensation_weighted, share_compensated)
  

GDP_required <- read.xlsx("K:/WorkInProgress/2021_Carbon_Footprint_Analysis/Gas Price Analysis/Compensation_Requirement.xlsx", colNames = TRUE, startRow = 2)%>%
  mutate(Country = ifelse(Country == "Slovak Republic", "Slovakia", Country))

GDP_required_2 <- read.xlsx("K:/WorkInProgress/2021_Carbon_Footprint_Analysis/Gas Price Analysis/Compensation_Requirement.xlsx", colNames = TRUE, startRow = 2)%>%
  select(Country, No_HHs, GDP, starts_with("Expenditures"), everything())

hhs <- household_information_0 %>%
  mutate(pop = hh_size*hh_weights)%>%
  group_by(Country)%>%
  summarise(pop = sum(pop))%>%
  ungroup()

GDP_required_2 <- left_join(GDP_required_2, hhs)%>%
  mutate(pop = ifelse(Country == "Czech Republic", 10700000, pop))%>%
  mutate(GDP_pc = GDP/pop)%>%
  select(Country, pop, No_HHs, GDP, GDP_pc, everything())%>%
  mutate(compensation_Gas  = Expenditures_Total_Gas/No_HHs,
         compensation_Coal = Expenditures_Total_Coal/No_HHs,
         compensation_P_C  = Expenditures_Total_P_C/No_HHs)%>%
  select(Country, pop, No_HHs, GDP, GDP_pc, starts_with("Expenditures"), starts_with("compensation"), everything())%>%
  left_join(select(data_7.4, -bonus))%>%
  left_join(data_7.4.2)%>%
  mutate(Gas_partially_compensated = compensation_weighted/GDP)

write.xlsx(GDP_required_2, "K:/WorkInProgress/2021_Carbon_Footprint_Analysis/Gas Price Analysis/Compensation_Requirement_2.1.xlsx")

GDP_required_2.1 <- GDP_required_2 %>%
  select(Country, Gas_partially_compensated)

# 7.5   EU-Karte einfärben ####

world <- map_data("world")

europe <- world %>%
  filter(region %in% c("Belgium", "Bulgaria", "Cyprus", "Czech Republic", "Germany", "Denmark", "Estonia", "Greece", "Spain", "Finland",
                       "France", "Croatia", "Hungary" ,"Ireland", "Italy", "Lithuania", "Luxembourg", "Latvia", "Netherlands", "Poland",
                       "Portugal", "Romania", "Sweden", "Slovakia", "Switzerland", "Austria",
                       "UK", "Norway", "Slovenia", "Serbia", "Kosovo", "Albania", "Bosnia and Herzegovina", 
                       "North Macedonia", "Montenegro", "Ukraine", "Moldova", "Turkey", "Russia", "Belarus"))%>%
  left_join(select(GDP_required, Country, Gas), by = c("region" = "Country"))%>%
  left_join(GDP_required_2.1, by = c("region" = "Country"))%>%
  mutate(Gas = ifelse(region == "Czech Republic", NA, Gas),
         Gas_partially_compensated = ifelse(region == "Czech Republic", NA, Gas_partially_compensated))

plot_7.5.1 <- ggplot()+
  geom_map(data = europe, map = world, aes(map_id = region), fill = "lightgrey")+
  geom_map(data = europe, map = world,
           aes(long, lat, map_id = region, fill = Gas), colour = "black", size = 0.1)+
  theme_bw()+
  coord_map(ylim = c(35,70), xlim = c(-10,35))+
  theme(axis.text.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.title  = element_blank(),
        plot.title = element_text(size = 9),
        legend.position = "bottom",
        strip.text = element_text(size = 7),
        strip.text.y = element_text(angle = 180),
        panel.grid.major = element_line(size = 0.3),
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(size = 0.2),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.2,0.2,0.2,0.2), "cm"),
        panel.border = element_rect(size = 0.3))+
  scale_fill_continuous(na.value = "lightgrey", low = "lightgreen", high = "darkred", labels = percent_format(accuracy = 1), limits = c(0,0.05))+
  #scale_fill_manual(values = c("#6F99ADFF", "white", "#0072B5FF"), na.translate = FALSE, na.value = "lightgrey")+
  labs(fill = "Compensation costs (in % of GDP)")+
  ggtitle("Compensating households with average additional costs \n (Scenario I)")+
  guides(colour = "none")

plot_7.5.2 <- ggplot()+
  geom_map(data = europe, map = world, aes(map_id = region), fill = "lightgrey")+
  geom_map(data = europe, map = world,
           aes(long, lat, map_id = region, fill = Gas_partially_compensated), colour = "black", size = 0.1)+
  theme_bw()+
  coord_map(ylim = c(35,70), xlim = c(-10,35))+
  theme(axis.text.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.title  = element_blank(),
        plot.title = element_text(size = 9),
        legend.position = "bottom",
        strip.text = element_text(size = 7),
        strip.text.y = element_text(angle = 180),
        panel.grid.major = element_line(size = 0.3),
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(size = 0.2),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.2,0.2,0.2,0.2), "cm"),
        panel.border = element_rect(size = 0.3))+
  scale_fill_continuous(na.value = "lightgrey", low = "lightgreen", high = "darkred", labels = percent_format(accuracy = 1), limits = c(0,0.05))+
  #scale_fill_manual(values = c("#6F99ADFF", "white", "#0072B5FF"), na.translate = FALSE, na.value = "lightgrey")+
  labs(fill = "Compensation costs (in % of GDP)")+
  ggtitle("Compensating households with average additional costs \n for gas and district heat in lower quintiles (Scenario II)")+
  guides(colour = "none")

plot_7.5.3 <- ggarrange(plot_7.5.1, plot_7.5.2, align = "hv", common.legend = TRUE, legend = "bottom")

# jpeg("K:/WorkInProgress/2021_Carbon_Footprint_Analysis/Gas Price Analysis/Figures/Figure_12_Maps/Figure_12_%d.jpg", width = 20, height = 13.33 , unit = "cm", res = 400)
# print(plot_7.5.1)
# print(plot_7.5.2)
# print(plot_7.5.3)
# dev.off()


# 7.6   Stapel-Diagramm / Dekomposition ####

data_7.6 <- left_join(indirect_expenditures_0, household_information_0)%>%
  left_join(select(carbon_pricing_incidence_0, hh_id, Income_Group_5))%>%
  select(hh_id, hh_weights, Income_Group_5, Diesel_burden_GAS_gas:Other_burden_P_C_p_c, Country)%>%
  pivot_longer(Diesel_burden_GAS_gas:Other_burden_P_C_p_c, names_to = "Type", values_to = "burden")%>%
  mutate(burden = ifelse(is.na(burden),0,burden))

data_7.6.help <- distinct(data_7.6, Type)%>%
  mutate(Type_A = ifelse(grepl("COAL", Type),"Coal",
                         ifelse(grepl("GAS", Type), "Gas",
                                ifelse(grepl("P_C", Type), "Oil", NA))))%>%
  mutate(Type_B = ifelse(grepl("District_Heating", Type), "District heating",
                         ifelse(grepl("Electricity", Type), "Electricity",
                                ifelse(grepl("Food", Type), "Food",
                                       ifelse(grepl("Gas_Heating", Type), "Gas heating",
                                              ifelse(grepl("Goods", Type), "Goods, services, other energy",
                                                     ifelse(grepl("Oil_Heating", Type), "Oil heating",
                                                            ifelse(grepl("Other Energy", Type),"Goods, services, other energy",
                                                                   ifelse(grepl("LPG", Type), "Goods, services, other energy",
                                                                          ifelse(grepl("Services", Type), "Goods, services, other energy",
                                                                                 ifelse(grepl("Diesel", Type), "Transport fuels", 
                                                                                        ifelse(grepl("Gasoline", Type), "Transport fuels",
                                                                                               ifelse(grepl("Other Transport Fuels", Type), "Transport fuels", "Goods, services, other energy")))))))))))))

data_7.6 <- data_7.6 %>%
  left_join(data_7.6.help)

agg_function <- function(x){
  x1 <- x %>%
    group_by(Income_Group_5, Type_B, Country)%>%
    summarise(
      mean = wtd.mean(burden, weights = hh_weights, na.rm = TRUE))%>%
    ungroup()%>%
    group_by(Income_Group_5, Country)%>%
    mutate(mean_agg = sum(mean))%>%
    ungroup()
  
  return(x1)
}

data_7.6.1 <- data_7.6 %>%
  filter(Type_A == "Gas")%>%
  agg_function()

data_7.6.2 <- data_7.6 %>%
  filter(Type_A == "Coal")%>%
  agg_function()

data_7.6.3 <- data_7.6 %>%
  filter(Type_A == "Oil")%>%
  agg_function()

data_7.6.4 <- data_7.6 %>%
  filter(Type_A == "Gas")%>%
  left_join(select(carbon_pricing_incidence_0, hh_id, Heating_Gas))%>%
  mutate(burden = ifelse(is.na(Heating_Gas) | Heating_Gas == 0,NA, burden))%>%
  agg_function()%>%
  mutate(mean = ifelse(mean == "NaN",0,mean))

plot_7.6.1.100 <- ggplot(data_7.6.1, aes(x = factor(Income_Group_5)))+
  geom_col(aes(y = mean, fill = Type_B), width = 0.5, position = "stack", colour = "black", size = 0.2)+
  theme_bw()+
  facet_wrap(. ~ Country)+
  xlab("Expenditure Quintile")+ ylab("Average Burden in Percent")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0))+
  coord_cartesian(ylim = c(0, round(max(data_7.6.1$mean_agg)+0.01,2)))+
  #coord_cartesian(ylim = c(0,0.3))+
  labs(fill = "")+
  scale_fill_manual(values = c("#20854EFF", "#E18727FF", "#7876B1FF", "#20854EFF", "#6F99ADFF", "#0072B5FF", "#0072B5FF"))+
  ggtitle("Decomposition - Gas Price Increase 100%")+
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
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.1,0.1,0,0), "cm"),
        panel.border = element_rect(size = 0.3))

plot_7.6.2.100 <- ggplot(data_7.6.2, aes(x = factor(Income_Group_5)))+
  geom_col(aes(y = mean, fill = Type_B), width = 0.5, position = "stack", colour = "black", size = 0.2)+
  theme_bw()+
  facet_wrap(. ~ Country)+
  xlab("Expenditure Quintile")+ ylab("Average Burden in Percent")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0))+
  coord_cartesian(ylim = c(0, round(max(data_7.6.2$mean_agg)+0.01,2)))+
  #coord_cartesian(ylim = c(0,0.3))+
  labs(fill = "")+
  ggtitle("Decomposition - Coal Price Increase 100%")+
  scale_fill_manual(values = c("#20854EFF", "#E18727FF", "#7876B1FF", "#20854EFF", "#6F99ADFF", "#0072B5FF", "#0072B5FF"))+
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
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.1,0.1,0,0), "cm"),
        panel.border = element_rect(size = 0.3))

plot_7.6.3.100<- ggplot(data_7.6.3, aes(x = factor(Income_Group_5)))+
  geom_col(aes(y = mean, fill = Type_B), width = 0.5, position = "stack", colour = "black", size = 0.2)+
  theme_bw()+
  facet_wrap(. ~ Country)+
  xlab("Expenditure Quintile")+ ylab("Average Burden in Percent")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0))+
  coord_cartesian(ylim = c(0, round(max(data_7.6.3$mean_agg)+0.01,2)))+
  #coord_cartesian(ylim = c(0,0.3))+
  labs(fill = "")+
  ggtitle("Decomposition - Liquid Fuel Price Increase 100%")+
  scale_fill_manual(values = c("#20854EFF", "#E18727FF", "#7876B1FF", "#20854EFF", "#6F99ADFF", "#0072B5FF", "#0072B5FF"))+
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
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.1,0.1,0,0), "cm"),
        panel.border = element_rect(size = 0.3))

plot_7.6.4.100 <- ggplot(data_7.6.4, aes(x = factor(Income_Group_5)))+
  geom_col(aes(y = mean, fill = Type_B), width = 0.5, position = "stack", colour = "black", size = 0.2)+
  theme_bw()+
  facet_wrap(. ~ Country)+
  xlab("Expenditure Quintile")+ ylab("Average Burden in Percent")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0))+
  coord_cartesian(ylim = c(0, 0.16))+
  #coord_cartesian(ylim = c(0,0.3))+
  labs(fill = "")+
  ggtitle("Decomposition - Gas Price Increase 100% - Households with Gas Expenditures Only")+
  scale_fill_manual(values = c("#20854EFF", "#E18727FF", "#7876B1FF", "#20854EFF", "#6F99ADFF", "#0072B5FF", "#0072B5FF"))+
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
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.1,0.1,0,0), "cm"),
        panel.border = element_rect(size = 0.3))

jpeg("C:/Users/misl/ownCloud/EU_Gasprice_Analysis/Figures/2022_05_06_Figures/Figure_Appendix_Decomposition_100_%d.jpg", width = 15, height = 20, unit = "cm", res = 400)
print(plot_7.6.1.100)
print(plot_7.6.4.100)
print(plot_7.6.2.100)
print(plot_7.6.3.100)
dev.off()

# 7.6.1 Dekomposition Update ####

data_7.6.X <- left_join(indirect_expenditures_0, household_information_0)%>%
  left_join(select(carbon_pricing_incidence_0, hh_id, Income_Group_5))%>%
  select(hh_id, hh_weights, Income_Group_5, Electricity_burden_GAS_gas:Other_burden_P_C_p_c, Country)%>%
  pivot_longer(Electricity_burden_GAS_gas:Other_burden_P_C_p_c, names_to = "Type", values_to = "burden")%>%
  mutate(burden = ifelse(is.na(burden),0,burden))%>%
  mutate(Type_A = ifelse(grepl("COAL", Type),"Coal",
                         ifelse(grepl("GAS", Type), "Gas",
                                ifelse(grepl("P_C", Type), "Liquid Fuels", NA))))%>%
  mutate(Type_B = ifelse(grepl("Food", Type), "Food", 
                         ifelse(grepl("Electricity", Type), "Electricity",
                                ifelse(grepl("Goods", Type), "Goods",
                                       ifelse(grepl("Services", Type), "Services", 
                                              ifelse(grepl("Other Energy", Type), "Other Energy", 
                                                     ifelse(grepl("Gas_Heating", Type), "Gas Heating",
                                                            ifelse(grepl("Oil_Heating", Type), "Oil Heating",
                                                                   ifelse(grepl("District_Heating", Type), "District Heating", 
                                                                          ifelse(grepl("Transport", Type), "Transport Fuels", "Other"))))))))))%>%
  group_by(hh_id, Type_B)%>%
  mutate(burden_total = sum(burden))%>%
  ungroup()%>%
  select(-Type_A, - burden, - Type)%>%
  distinct()

data_7.6.X.1 <- data_7.6.X %>%
  group_by(Country, Income_Group_5, Type_B)%>%
  summarise(mean_burden_total = wtd.mean(burden_total, weights = hh_weights))%>%
  ungroup()%>%
  group_by(Country, Income_Group_5)%>%
  mutate(mean_agg = sum(mean_burden_total))%>%
  ungroup()%>%
  mutate(Country_Type = ifelse(Country %in% c("Hungary", "Italy", "Germany", "Romania", "Czech Republic"), "High Incidence Countries",
                               ifelse(Country %in% c("Netherlands", "Poland", "Croatia", "Slovak Republic", "Belgium"), "Relatively High Incidence Countries",
                                      ifelse(Country %in% c("Portugal", "Luxembourg", "Ireland", "Spain", "Latvia"), "Medium Incidence Countries",
                                             ifelse(Country %in% c("Lithuania", "Greece", "France", "Denmark"), "Relatively Low Incidence Countries",
                                                    ifelse(Country %in% c("Sweden", "Finland", "Estonia", "Cyprus", "Bulgaria"), "Low Incidence Countries", NA))))))

data_7.6.X.2 <- left_join(carbon_pricing_incidence_0, household_information_0)%>%
  select(hh_id, Income_Group_5, hh_weights, Country, burden_GAS_gas, burden_COAL_coal, burden_P_C_p_c, Heating_Gas)%>%
  mutate(burden_total = burden_GAS_gas + burden_COAL_coal + burden_P_C_p_c)%>%
  mutate(burden_Gas_Users = ifelse(Heating_Gas > 0, burden_total, 0),
         Gas_Users        = ifelse(Heating_Gas > 0 & !is.na(Heating_Gas), hh_weights, 0))%>%
  group_by(Country, Income_Group_5)%>%
  summarise(mean_burden_Gas_Users = wtd.mean(burden_Gas_Users, hh_weights),
            Gas_Users             = sum(Gas_Users),
            sum_hh_weights        = sum(hh_weights))%>%
  ungroup()%>%
  mutate(share_Gas_Users = Gas_Users/sum_hh_weights)%>%
  mutate(mean_burden_Gas_Users = ifelse(mean_burden_Gas_Users == "NaN",NA,mean_burden_Gas_Users))%>%
  select(Country, Income_Group_5, mean_burden_Gas_Users, share_Gas_Users)%>%
  mutate(Country_Type = ifelse(Country %in% c("Hungary", "Italy", "Germany", "Romania", "Czech Republic"), "High Incidence Countries",
                               ifelse(Country %in% c("Netherlands", "Poland", "Croatia", "Slovak Republic", "Belgium"), "Relatively High Incidence Countries",
                                      ifelse(Country %in% c("Portugal", "Luxembourg", "Ireland", "Spain", "Latvia"), "Medium Incidence Countries",
                                             ifelse(Country %in% c("Lithuania", "Greece", "France", "Denmark"), "Relatively Low Incidence Countries",
                                                    ifelse(Country %in% c("Sweden", "Finland", "Estonia", "Cyprus", "Bulgaria"), "Low Incidence Countries", NA))))))%>%
  mutate(label_0 = paste0(round(share_Gas_Users,2)*100, "%"))

plot_function <- function(Type_0){
  
  plot_7.6.X <- ggplot()+
    geom_col(data = filter(data_7.6.X.1, Country_Type == Type_0),
             aes(x = factor(Income_Group_5), y = mean_burden_total, fill = Type_B), 
             width = 0.5, position = "stack", colour = "black", size = 0.2)+
    geom_point(data = filter(data_7.6.X.2, Country_Type == Type_0),
               aes(x = factor(Income_Group_5), y = mean_burden_Gas_Users, shape = "Average Incidence for Gas Users"),
               colour = "black", size = 1.8, fill = "#20854EFF")+
    geom_text(data = filter(data_7.6.X.2, Country_Type == Type_0),
              aes(x = factor(Income_Group_5), 
                  y = round(max(filter(data_7.6.X.2, Country_Type == Type_0)$mean_burden_Gas_Users, na.rm = TRUE)+0.01,2), 
                  label = label_0),
              size = 1.5)+
    theme_bw()+
    facet_wrap(. ~ Country, nrow = 1)+
    xlab("Expenditure Quintile")+ ylab("Average Additional Costs (in % of Total Expenditures)")+
    scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0))+
    coord_cartesian(ylim = c(0, round(max(filter(data_7.6.X.2, Country_Type == Type_0)$mean_burden_Gas_Users, na.rm = TRUE)+0.02,2)))+
    #coord_cartesian(ylim = c(0,0.3))+
    labs(fill = "", shape = "")+
    scale_shape_manual(name = "", values = c(22))+
    guides(shape = guide_legend(override.aes = list(fill = "#20854EFF", colour = "black"), order = 2),
           fill  = guide_legend(order = 1))+
    ggtitle(paste0("Decomposition - Gas, Coal and Oil Price increase ", Type_0))+
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
          legend.text = element_text(size = 7),
          legend.title = element_text(size = 7),
          legend.box = "vertical",
          plot.margin = unit(c(0.1,0.1,0,0), "cm"),
          panel.border = element_rect(size = 0.3))
  
}

plot_7.6.1.1 <- plot_function("High Incidence Countries")
plot_7.6.1.2 <- plot_function("Relatively High Incidence Countries")
plot_7.6.1.3 <- plot_function("Medium Incidence Countries")
plot_7.6.1.4 <- plot_function("Relatively Low Incidence Countries")
plot_7.6.1.5 <- plot_function("Low Incidence Countries")


jpeg("C:/Users/misl/ownCloud/EU_Gasprice_Analysis/Figures/2022_04_26_Figures/Figure_3_new_%d.jpg", width = 15, height = 10, unit = "cm", res = 400)
print(plot_7.6.1.1)
print(plot_7.6.1.2)
print(plot_7.6.1.3)
print(plot_7.6.1.4)
print(plot_7.6.1.5)
dev.off()

# 7.6.2 Generische 100/100/100 Plots - Coal/Gas/Fuel - Direct and Indirect ####

data_7.6.2.X <- left_join(carbon_pricing_incidence_0, household_information_0)%>%
  select(hh_id, hh_weights, Income_Group_5, Country,
         burden_GAS_direct,  burden_GAS_indirect,
         burden_COAL_direct, burden_COAL_indirect,
         burden_P_C_direct,  burden_P_C_indirect)%>%
  pivot_longer(burden_GAS_direct:burden_P_C_indirect, names_to = "Type", values_to = "burden")%>%
  mutate(burden = ifelse(is.na(burden), 0, burden))%>%
  mutate(Type_A = ifelse(grepl("COAL", Type),"Coal",
                         ifelse(grepl("GAS", Type), "Gas",
                                ifelse(grepl("P_C", Type), "Oil", NA))))%>%
  mutate(Type_B = ifelse(grepl("indirect", Type), "Indirect Effects", 
                         ifelse(grepl("direct", Type), "Direct Effects", NA)))%>%
  mutate(burden_factor = ifelse(Type_A == "Gas",1,
                                ifelse(Type_A == "Coal", 1,
                                       ifelse(Type_A == "Oil", 1,NA))))%>%
  mutate(burden_100 = burden*burden_factor)%>%
  select(hh_id, hh_weights, Income_Group_5, Type_A, Type_B, burden_100, Country)%>%
  group_by(Country, Income_Group_5, Type_A, Type_B)%>%
  summarise(mean_burden_100 = wtd.mean(burden_100, hh_weights))%>%
  ungroup()%>%
  group_by(Country, Income_Group_5)%>%
  mutate(mean_agg = sum(mean_burden_100))%>%
  ungroup()%>%
  mutate(Country_Type = ifelse(Country %in% c("Hungary", "Italy", "Germany", "Romania", "Czech Republic"), "High Incidence Countries",
                               ifelse(Country %in% c("Netherlands", "Poland", "Croatia", "Slovak Republic", "Belgium"), "Relatively High Incidence Countries",
                                      ifelse(Country %in% c("Portugal", "Luxembourg", "Ireland", "Spain", "Latvia"), "Medium Incidence Countries",
                                             ifelse(Country %in% c("Lithuania", "Greece", "France", "Denmark"), "Relatively Low Incidence Countries",
                                                    ifelse(Country %in% c("Sweden", "Finland", "Estonia", "Cyprus", "Bulgaria"), "Low Incidence Countries", NA))))))%>%
  mutate(Type_C = paste0(Type_B, " (", Type_A, ")"))

data_7.6.2.X$Type_C <- factor(data_7.6.2.X$Type_C, levels = c("Direct Effects (Coal)",
                                                              "Indirect Effects (Coal)",
                                                              "Direct Effects (Oil)",
                                                              "Indirect Effects (Oil)",
                                                              "Direct Effects (Gas)",
                                                              "Indirect Effects (Gas)"))

plot_7.6.2.X <- ggplot()+
  geom_col(data = data_7.6.2.X,
           aes(x = factor(Income_Group_5), y = mean_burden_100, fill = Type_C, alpha = Type_C), 
           width = 0.5, position = "stack", colour = "black", size = 0.2)+
  theme_bw()+
  facet_wrap(. ~ Country)+
  xlab("Expenditure Quintile")+ ylab("Average Additional Costs (in % of Total Expenditures)")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0))+
  coord_cartesian(ylim = c(0, round(max(data_7.6.2.X$mean_agg, na.rm = TRUE)+0.02,2)))+
  #coord_cartesian(ylim = c(0,0.3))+
  labs(fill = "", shape = "")+
  scale_fill_manual(name = "", values = c("#BC2C29FF",
                                          "#BC2C29FF", 
                                          "#0072B5FF",
                                          "#0072B5FF", 
                                          "#E18727FF",
                                          "#E18727FF"))+
  scale_alpha_manual(name = "", values = c(1,0.5,1,0.5,1,0.5))+
  ggtitle("100% Price Increase for Coal, Oil and Gas (Stylized Scenario)")+
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
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        legend.box = "vertical",
        plot.margin = unit(c(0.1,0.1,0,0), "cm"),
        panel.border = element_rect(size = 0.3))

jpeg("C:/Users/misl/ownCloud/EU_Gasprice_Analysis/Figures/2022_05_20_Figures/Figure_A4.jpg", width = 15, height = 20, unit = "cm", res = 400)
print(plot_7.6.2.X)
#print(plot_7.6.1.2)
#print(plot_7.6.1.3)
#print(plot_7.6.1.4)
#print(plot_7.6.1.5)
dev.off()

# Dekomposition of Expenditure Type for 100/100/100

data_7.6.100 <- left_join(indirect_expenditures_0, household_information_0)%>%
  left_join(select(carbon_pricing_incidence_0, hh_id, Income_Group_5))%>%
  select(hh_id, hh_weights, Income_Group_5, Electricity_burden_GAS_gas:Other_burden_P_C_p_c, Country)%>%
  pivot_longer(Electricity_burden_GAS_gas:Other_burden_P_C_p_c, names_to = "Type", values_to = "burden")%>%
  mutate(burden = ifelse(is.na(burden),0,burden))%>%
  mutate(Type_A = ifelse(grepl("COAL", Type),"Coal",
                         ifelse(grepl("GAS", Type), "Gas",
                                ifelse(grepl("P_C", Type), "Liquid Fuels", NA))))%>%
  mutate(Type_B = ifelse(grepl("Food", Type), "Food", 
                         ifelse(grepl("Electricity", Type), "Electricity",
                                ifelse(grepl("Goods", Type), "Goods",
                                       ifelse(grepl("Services", Type), "Services", 
                                              ifelse(grepl("Other Energy", Type), "Other Energy", 
                                                     ifelse(grepl("Gas_Heating", Type), "Gas Heating",
                                                            ifelse(grepl("Oil_Heating", Type), "Oil Heating",
                                                                   ifelse(grepl("District_Heating", Type), "District Heating", 
                                                                          ifelse(grepl("Transport", Type), "Transport Fuels", "Other"))))))))))%>%
  mutate(burden_100 = ifelse(Type_A == "Coal", burden*100/30,
                         ifelse(Type_A == "Gas", burden*100/138,
                                ifelse(Type_A == "Liquid Fuels", burden*100/30, NA))))%>%
  group_by(hh_id, hh_weights, Income_Group_5, Type_B, Country)%>%
  summarise(burden_100 = sum(burden_100))%>%
  ungroup()%>%
  group_by(Country, Income_Group_5, Type_B)%>%
  summarise(mean_100 = wtd.mean(burden_100, weights = hh_weights))%>%
  ungroup()%>%
  group_by(Income_Group_5, Country)%>%
  mutate(agg_mean_100 = sum(mean_100))%>%
  ungroup()

plot_7.6.100 <- ggplot(data_7.6.100, aes(x = factor(Income_Group_5)))+
  geom_col(aes(y = mean_100, fill = Type_B), width = 0.5, position = "stack", colour = "black", size = 0.2)+
  theme_bw()+
  facet_wrap(. ~ Country)+
  xlab("Expenditure Quintile")+ ylab("Average Burden in Percent")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0))+
  coord_cartesian(ylim = c(0, round(max(data_7.6.100$agg_mean_100)+0.01,2)))+
  #coord_cartesian(ylim = c(0,0.3))+
  labs(fill = "")+
  ggtitle("Decomposition - 100% Price Increase for Coal, Oil and Gas")+
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
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.1,0.1,0,0), "cm"),
        panel.border = element_rect(size = 0.3))


jpeg("C:/Users/misl/ownCloud/EU_Gasprice_Analysis/Figures/2022_04_26_Figures/Figure_6_100.jpg", width = 15, height = 20, unit = "cm", res = 400)
print(plot_7.6.100)
dev.off()

# 7.7   Transfers ####

# Lump Sum Transfers per Household (average gas expenditures)

data_7.7.0 <- left_join(carbon_pricing_incidence_0, household_information_0)%>%
  mutate(Heating_Gas = ifelse(is.na(Heating_Gas),0,Heating_Gas),
         Heating_NA  = ifelse(is.na(Heating_NA), 0,Heating_NA),
         Heating_Oil = ifelse(is.na(Heating_Oil),0, Heating_Oil),
         Heating_District_Heat = ifelse(is.na(Heating_District_Heat),0, Heating_District_Heat))%>%
  mutate(exp_add_gas = hh_weights*exp_GAS_gas)%>%
  group_by(Country)%>%
  summarise(exp_add_gas = sum(exp_add_gas),
            hh_weights  = sum(hh_weights))%>%
  ungroup()%>%
  mutate(compensation = exp_add_gas/hh_weights)

# Average gas expenditures in lower two expenditure quintiles

data_7.7.1 <- left_join(indirect_expenditures_0, household_information_0)%>%
  left_join(carbon_pricing_incidence_0, by = "hh_id")%>%
  select(hh_id, hh_weights, Country, District_Heating_burden_GAS_gas, Gas_Heating_burden_GAS_gas,
         Heating_Gas, Heating_District_Heat, Income_Group_5, hh_expenditures_USD_2014)%>%
  mutate(District_Heating_exp = District_Heating_burden_GAS_gas*hh_expenditures_USD_2014,
         Gas_Heating_exp      = Gas_Heating_burden_GAS_gas*hh_expenditures_USD_2014)%>%
  filter(Income_Group_5 == 1 | Income_Group_5 == 2)%>%
  filter(!is.na(District_Heating_exp)|!is.na(Gas_Heating_exp))%>%
  group_by(Country)%>%
  summarise(mean_District_Heating_exp = wtd.mean(District_Heating_exp, hh_weights, na.rm = TRUE),
            mean_Gas_Heating_exp      = wtd.mean(Gas_Heating_exp, hh_weights, na.rm = TRUE))%>%
  ungroup()%>%
  mutate(mean_District_Heating_exp = ifelse(mean_District_Heating_exp == "NaN",0, mean_District_Heating_exp),
         mean_Gas_Heating_exp      = ifelse(mean_Gas_Heating_exp == "NaN",0, mean_Gas_Heating_exp))%>%
  mutate(bonus = mean_Gas_Heating_exp + mean_District_Heating_exp)

# Average additional gas expenditures

data_7.7.2 <- left_join(indirect_expenditures_0, household_information_0)%>%
  left_join(carbon_pricing_incidence_0, by = "hh_id")%>%
  select(hh_id, hh_weights, Country, Gas_Heating_burden_GAS_gas, hh_expenditures_USD_2014)%>%
  mutate(Gas_Heating_exp = Gas_Heating_burden_GAS_gas*hh_expenditures_USD_2014)%>%
  group_by(Country)%>%
  summarise(mean_Gas_Heating_exp = wtd.mean(Gas_Heating_exp, hh_weights, na.rm = TRUE))%>%
  ungroup()%>%
  mutate(mean_Gas_Heating_exp = ifelse(mean_Gas_Heating_exp == "NaN", 0, mean_Gas_Heating_exp))%>%
  mutate(bonus_2 = mean_Gas_Heating_exp)

data_7.7 <- left_join(carbon_pricing_incidence_0, household_information_0)%>%
  mutate(Heating_Gas = ifelse(is.na(Heating_Gas),0,Heating_Gas),
         Heating_NA  = ifelse(is.na(Heating_NA), 0,Heating_NA),
         Heating_Oil = ifelse(is.na(Heating_Oil),0, Heating_Oil),
         Heating_District_Heat = ifelse(is.na(Heating_District_Heat),0, Heating_District_Heat))%>%
  left_join(select(data_7.7.0, Country, compensation))%>%
  left_join(select(data_7.7.1, Country, mean_District_Heating_exp, mean_Gas_Heating_exp, bonus))%>%
  left_join(select(data_7.7.2, Country, bonus_2))%>%
  # Model compensation
  mutate(compensation_1 = compensation,
         compensation_2 = ifelse((Income_Group_5 == 1 | Income_Group_5 == 2) & Heating_Gas > 0 & Heating_District_Heat == 0, mean_Gas_Heating_exp,
                                 ifelse((Income_Group_5 == 1 | Income_Group_5 == 2) & Heating_Gas == 0 & Heating_District_Heat > 0, mean_District_Heating_exp,
                                        ifelse((Income_Group_5 == 1 | Income_Group_5 == 2) & Heating_Gas > 0 & Heating_District_Heat > 0, bonus, 0))),
         compensation_3 = ifelse(Heating_Gas > 0 | Heating_District_Heat > 0, bonus_2, 0))%>%
  select(hh_id, hh_size, hh_weights, Country, Income_Group_5,
         compensation_1, compensation_2, compensation_3,
         hh_expenditures_USD_2014, exp_GAS_gas)%>%
  mutate(exp_GAS_gas_0 = -exp_GAS_gas)%>%
  mutate(compensation_0 = 0)%>%
  pivot_longer(c(compensation_1:compensation_3, compensation_0), names_to = "Type", values_to = "compensation")%>%
  mutate(Type_A = ifelse(Type == "compensation_1", "Household Transfer Country-Level \n Average Additional Costs",
                         ifelse(Type == "compensation_2", "Compensation via Gas / District Heat \n Payments for Lower Income Households", 
                                ifelse(Type == "compensation_0", "No Compensation", 
                                       ifelse(Type == "compensation_3", "Compensation via Gas / District Heat \n Payments for all Households with Gas Use", NA)))))%>%
  mutate(exp_GAS_compensation     = compensation - exp_GAS_gas)%>%
  mutate(burden_GAS_compensation  = exp_GAS_compensation/hh_expenditures_USD_2014)


data_7.7.3 <- data_7.7 %>%
  group_by(Country, Income_Group_5, Type_A)%>%
  summarise(
    y5  = wtd.quantile(burden_GAS_compensation, weights = hh_weights, probs = 0.05),
    y25 = wtd.quantile(burden_GAS_compensation, weights = hh_weights, probs = 0.25),
    y50 = wtd.quantile(burden_GAS_compensation, weights = hh_weights, probs = 0.5),
    y75 = wtd.quantile(burden_GAS_compensation, weights = hh_weights, probs = 0.75),
    y95 = wtd.quantile(burden_GAS_compensation, weights = hh_weights, probs = 0.95),
    mean = wtd.mean(   burden_GAS_compensation, weights = hh_weights))%>%
  ungroup()%>%
  mutate(Country_Type = ifelse(Country %in% c("Hungary", "Italy", "Germany", "Romania", "Czech Republic"), "High Incidence Countries",
                               ifelse(Country %in% c("Netherlands", "Poland", "Croatia", "Slovak Republic", "Belgium"), "Relatively High Incidence Countries",
                                      ifelse(Country %in% c("Portugal", "Luxembourg", "Ireland", "Spain", "Latvia"), "Medium Incidence Countries",
                                             ifelse(Country %in% c("Lithuania", "Greece", "France", "Denmark"), "Relatively Low Incidence Countries",
                                                    ifelse(Country %in% c("Sweden", "Finland", "Estonia", "Cyprus", "Bulgaria"), "Low Incidence Countries", NA))))))
  

data_7.7.3$Type_A <- factor(data_7.7.3$Type_A, levels = c("No Compensation", "Household Transfer Country-Level \n Average Additional Costs",
                                                      "Compensation via Gas / District Heat \n Payments for Lower Income Households",
                                                      "Compensation via Gas / District Heat \n Payments for all Households with Gas Use"))

plot_7_function <- function(Type_0){
    plot_7 <- ggplot(filter(data_7.7.3, Country_Type == Type_0), aes(x = factor(Income_Group_5)))+
    geom_hline(aes(yintercept = 0), size = 0.5)+
    geom_line(aes(y = mean, group = interaction(Country, Type_A), colour = factor(Type_A)))+
    geom_point(aes(y = mean, fill = factor(Type_A)), shape = 21, colour = "black")+
    theme_bw()+
    facet_wrap(. ~ Country, nrow = 1)+
    xlab("Expenditure Quintile")+ ylab("Average Budget Change (in % of Total Expenditures)")+
    scale_y_continuous(labels = scales::percent_format(), expand = c(0,0))+
    coord_cartesian(ylim = c(round(min(filter(data_7.7.3, Country_Type == Type_0)$mean-0.01),2),
                             round(max(filter(data_7.7.3, Country_Type == Type_0)$mean+0.01),2)))+
    scale_fill_nejm()+
    scale_colour_nejm()+
    ggtitle(paste0("Compensation Scenarios - ", Type_0))+
    labs(fill = "")+
    guides(colour = "none", fill = guide_legend(nrow = 2))+
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
          legend.text = element_text(size = 7),
          legend.title = element_text(size = 7),
          plot.margin = unit(c(0.1,0.1,0,0), "cm"),
          panel.border = element_rect(size = 0.3))
  return(plot_7)
}

plot_7.7.1 <- plot_7_function("High Incidence Countries")
plot_7.7.2 <-plot_7_function("Relatively High Incidence Countries")
plot_7.7.3 <-plot_7_function("Medium Incidence Countries")
plot_7.7.4 <-plot_7_function("Relatively Low Incidence Countries")
plot_7.7.5 <-plot_7_function("Low Incidence Countries")


jpeg("C:/Users/misl/ownCloud/EU_Gasprice_Analysis/Figures/2022_04_26_Figures/Figure_4_%d.jpg", width = 15, height = 8 , unit = "cm", res = 400)
print(plot_7.7.1)
print(plot_7.7.2)
print(plot_7.7.3)
print(plot_7.7.4)
print(plot_7.7.5)
dev.off()

# Share of households more affected than 5%

data_7.7.4 <- data_7.7 %>%
  mutate(more_affected_5  = ifelse(burden_GAS_compensation < -0.05, hh_weights, 0),
         more_affected_10 = ifelse(burden_GAS_compensation < -0.10, hh_weights, 0))%>%
  group_by(Country, Income_Group_5, Type_A)%>%
  mutate(sum_hh_weights = sum(hh_weights))%>%
  summarise(hh_weights_ma5  = sum(more_affected_5),
            hh_weights_ma10 = sum(more_affected_10),
            sum_hh_weights = mean(sum_hh_weights))%>%
  ungroup()%>%
  mutate(share_ma5  = hh_weights_ma5/sum_hh_weights,
         share_ma10 = hh_weights_ma10/sum_hh_weights)%>%
  pivot_longer(share_ma5:share_ma10, names_to = "Type_B", values_to = "share")%>%
  unite(Type_C, c("Type_A", "Income_Group_5"), sep = " ")%>%
  select(Country, Type_C, Type_B, share)%>%
  arrange(Country, Type_C)%>%
  pivot_wider(names_from = "Type_C", values_from = "share")%>%
  mutate(Type_B = rep(c("5%","10%"),24))%>%
  select(Country, Type_B, starts_with("No"),
         starts_with("Household Transfer Country-Level \n Average Additional Costs"),
         starts_with("Compensation via Gas / District Heat \n Payments for Lower Income Households"),
         starts_with("Compensation via Gas / District Heat \n Payments for all Households with Gas Use"))

write.xlsx(data_7.7.4, "C:/Users/misl/ownCloud/EU_Gasprice_Analysis/Figures/2022_04_26_Figures/Former_Figure_5_Table.xlsx")

data_7.7.4$Type_A <- factor(data_7.7.4$Type_A, levels = c("No Compensation", 
                                                          "Compensation via Gas / District Heat \n Payments for Lower Income Households",
                                                          "Household Transfer Country-Level \n Average Additional Costs",
                                                          "Compensation via Gas / District Heat \n Payments for all Households with Gas Use"))

plot_7.7.2 <- ggplot(data_7.7.4, aes(x = factor(Income_Group_5)))+
  geom_col(aes(y = share, fill = Type_A), position = position_dodge(0.7), width = 0.3, colour = "black", size = 0.2)+
  theme_bw()+
  facet_wrap(. ~ Country)+
  xlab("Expenditure Quintile")+ ylab("Share of Households with higher costs than 5%")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0))+
  coord_cartesian(ylim = c(0,1))+
  scale_fill_nejm()+
  scale_colour_nejm()+
  ggtitle("Compensation Scenarios - Share of Households with Exceptionally High Costs")+
  labs(fill = "")+
  guides(colour = "none", fill = guide_legend(nrow = 2))+
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
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.1,0.1,0,0), "cm"),
        panel.border = element_rect(size = 0.3))

jpeg("C:/Users/misl/ownCloud/EU_Gasprice_Analysis/Figures/2022_04_19_Figures/Figure_8.jpg", width = 15, height = 22 , unit = "cm", res = 400)
print(plot_7.7.2)
dev.off()

# Maps with Transfers

data_7.7.5 <- data_7.7 %>%
  mutate(compensation_weighted = compensation*hh_weights)%>%
  group_by(Country, Type_A)%>%
  summarise(compensation_weighted = sum(compensation_weighted))%>%
  ungroup()%>%
  mutate(Country = ifelse(Country == "Slovak Republic", "Slovakia", Country))

GDP_0 <- read.xlsx("C:/Users/misl/ownCloud/EU_Gasprice_Analysis/Supplementary Information/Compensation_Requirement.xlsx", colNames = TRUE, startRow = 2)%>%
  mutate(Country = ifelse(Country == "Slovak Republic", "Slovakia", Country))%>%
  select(Country, GDP)

data_7.7.5.1 <- left_join(data_7.7.5, GDP_0)%>%
  mutate(share_GDP = compensation_weighted/GDP)

world <- map_data("world")

map_data <- map_data("world")%>%
  filter(region %in% c("Belgium", "Bulgaria", "Cyprus", "Czech Republic", "Germany", "Denmark", "Estonia", "Greece", "Spain", "Finland",
                       "France", "Croatia", "Hungary" ,"Ireland", "Italy", "Lithuania", "Luxembourg", "Latvia", "Netherlands", "Poland",
                       "Portugal", "Romania", "Sweden", "Slovakia", "Switzerland", "Austria",
                       "UK", "Norway", "Slovenia", "Serbia", "Kosovo", "Albania", "Bosnia and Herzegovina", 
                       "North Macedonia", "Montenegro", "Ukraine", "Moldova", "Turkey", "Russia", "Belarus"))%>%
  left_join(data_7.7.5.1, by = c("region" = "Country"))%>%
  mutate(share_GDP = ifelse(region == "Czech Republic", NA, share_GDP))

map_data_1 <- expand_grid(select(filter(map_data, is.na(Type_A)), - Type_A), Type_A = c("Compensation via Gas / District Heat \n Payments for Lower Income Households",
                                                                                        "Household Transfer Country-Level \n Average Additional Costs",
                                                                                        "Compensation via Gas / District Heat \n Payments for all Households with Gas Use"))%>%
  bind_rows(filter(map_data, !is.na(Type_A)))%>%
  filter(Type_A != "No Compensation")%>%
  mutate(share_GDP = ifelse(region == "Czech Republic", NA, share_GDP))

plot_7.7.3 <- ggplot()+
  geom_map(data = map_data_1, map = world, aes(map_id = region), fill = "lightgrey")+
  geom_map(data = map_data_1, map = world,
           aes(long, lat, map_id = region, fill = share_GDP), colour = "black", size = 0.2)+
  theme_bw()+
  facet_wrap(. ~ Type_A)+
  coord_map(ylim = c(35,70), xlim = c(-10,35))+
  theme(axis.text.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.title  = element_blank(),
        plot.title = element_text(size = 9),
        legend.position = "bottom",
        strip.text = element_text(size = 6),
        strip.text.y = element_text(angle = 180),
        panel.grid.major = element_line(size = 0.3),
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(size = 0.2),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"),
        panel.border = element_rect(size = 0.3))+
  scale_fill_distiller(palette = "Spectral", na.value = "lightgrey", labels = percent_format(accuracy = 0.1), limits = c(0,0.045), breaks = seq(0,0.045,0.005))+
  labs(fill = "Compensation costs (in % of GDP)")+
  ggtitle("Compensation Scenarios")+
  guides(colour = "none", fill = guide_colourbar(barwidth = 15, barheight = 1, ticks.colour = "black"))


jpeg("C:/Users/misl/ownCloud/EU_Gasprice_Analysis/Figures/2022_04_19_Figures/Figure_9_2.jpg", width = 15, height = 14 , unit = "cm", res = 400)
#print(plot_7.7.3.1)
print(plot_7.7.3)
dev.off()

# 7.8   Combined Burden + Fuel-Specific Means Included ####

data_7.8 <- left_join(carbon_pricing_incidence_0, household_information_0)%>%
  select(hh_id, hh_weights, hh_size, Income_Group_5, Country,
         burden_GAS_gas, burden_COAL_coal, burden_P_C_p_c)%>%
  mutate(burden_total = burden_GAS_gas + burden_COAL_coal + burden_P_C_p_c)%>%
  mutate(burden_100_gas  = burden_GAS_gas,
         burden_100_coal = burden_COAL_coal,
         burden_100_p_c  = burden_P_C_p_c)%>%
  mutate(burden_total_100 = burden_100_gas + burden_100_coal + burden_100_p_c)

data_7.8.1 <- data_7.8 %>%
  group_by(Country)%>%
  summarise(
    y5   = wtd.quantile(burden_total, weights = hh_weights, probs = 0.05),
    y25  = wtd.quantile(burden_total, weights = hh_weights, probs = 0.25),
    y50  = wtd.quantile(burden_total, weights = hh_weights, probs = 0.5),
    y75  = wtd.quantile(burden_total, weights = hh_weights, probs = 0.75),
    y95  = wtd.quantile(burden_total, weights = hh_weights, probs = 0.95),
    mean = wtd.mean(   burden_total, weights = hh_weights),
    mean_coal = wtd.mean(burden_COAL_coal, weights = hh_weights),
    mean_p_c  = wtd.mean(burden_P_C_p_c,   weights = hh_weights),
    mean_gas  = wtd.mean(burden_GAS_gas,   weights = hh_weights))%>%
  ungroup()

data_7.8.1.1 <- data_7.8.1 %>%
  pivot_longer(starts_with("mean"), names_to = "Type", values_to = "Mean")%>%
  #mutate_at(vars(y5:y95), ~ ifelse(Type == "mean", ., NA))%>%
  select(Country, Type, Mean)%>%
  mutate(Type_A = ifelse(Type == "mean", "All fuels",
                         ifelse(Type == "mean_coal", "Coal",
                                ifelse(Type == "mean_p_c", "Oil",
                                       ifelse(Type == "mean_gas", "Gas", NA)))))

data_7.8.1.1$Type_A <- factor(data_7.8.1.1$Type_A, levels = c("All fuels",
                                                          "Gas",
                                                          "Oil",
                                                          "Coal"))

plot_7.8.1 <- ggplot()+
  geom_boxplot(data = data_7.8.1, aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95, x = reorder(Country, desc(y50))), fill = "lightgrey", alpha = 1, stat = "identity", position = position_dodge(0), outlier.shape = NA, width = 0.5, size = 0.3) +
  geom_segment(aes(x = 1.01, xend = 0.99, y = 0.13116240, yend = 0.13116240, colour = "Median incidence, all fuels"), size = 0.8)+
  theme_bw()+
  xlab("")+ ylab("Additional Costs (in % of Total Expenditures)")+
  geom_point(data = data_7.8.1.1, aes(y = Mean, fill = Type_A, x = factor(Country), shape = Type_A), size = 1.7, stroke = 0.5)+
  #geom_point(aes(y = mean_coal), shape = 24, fill = "#BC3C29FF", colour = "black", size = 1.5, stroke = 0.2)+
  #geom_point(aes(y = mean_p_c),  shape = 22, fill = "#0072B5FF", colour = "black", size = 1.5, stroke = 0.2)+
  #geom_point(aes(y = mean_gas),  shape = 25, fill = "#20854EFF", colour = "black", size = 1.5, stroke = 0.2)+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0))+
  #scale_x_discrete(limits = rev)+
  #coord_flip(ylim = c(0,0.45))+
  coord_cartesian(ylim = c(0,0.45))+
  ggtitle("")+
  scale_fill_manual( name = "Average incidence of fuel price increases", values = c("white", "#20854EFF", "#0072B5FF", "#BC3C29FF"))+
  scale_shape_manual(name = "Average incidence of fuel price increases", values = c(21,22,23,24))+
  scale_colour_manual("", values = "black", breaks = "Median incidence, all fuels")+
  #scale_fill_nejm()+
  guides(fill = guide_legend(nrow = 1, order = 1, override.aes = list(shape = c(21,22,23,24)), title.position = "top", title.hjust = 0.5), colour = guide_legend(order = 2),
         shape = "none")+
  theme(axis.text.y = element_text(size = 7), 
        axis.text.x = element_text(size = 7, hjust = 0.95, vjust = 0.5, angle = 90),
        axis.title  = element_text(size = 7),
        plot.title = element_text(size = 7),
        legend.position = "bottom",
        strip.text = element_text(size = 7),
        strip.text.y = element_text(angle = 180),
        panel.grid.major = element_line(size = 0.3),
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(size = 0.2),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        legend.box = "vertical",
        plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"),
        panel.border = element_rect(size = 0.3))

jpeg("C:/Users/misl/ownCloud/EU_Gasprice_Analysis/Figures/2022_05_20_Figures/Figure_A5.jpg", width = 15, height = 17, unit = "cm", res = 400)
print(plot_7.8.1)
dev.off()

data_7.8.2 <- data_7.8 %>%
  group_by(Country)%>%
  summarise(
    y5   = wtd.quantile(burden_total_100, weights = hh_weights, probs = 0.05),
    y25  = wtd.quantile(burden_total_100, weights = hh_weights, probs = 0.25),
    y50  = wtd.quantile(burden_total_100, weights = hh_weights, probs = 0.5),
    y75  = wtd.quantile(burden_total_100, weights = hh_weights, probs = 0.75),
    y95  = wtd.quantile(burden_total_100, weights = hh_weights, probs = 0.95),
    mean = wtd.mean(   burden_total_100, weights = hh_weights),
    mean_coal = wtd.mean(burden_100_coal, weights = hh_weights),
    mean_p_c  = wtd.mean(burden_100_p_c,   weights = hh_weights),
    mean_gas  = wtd.mean(burden_100_gas,   weights = hh_weights))%>%
  ungroup()

data_7.8.2.1 <- data_7.8.2 %>%
  pivot_longer(starts_with("mean"), names_to = "Type", values_to = "Mean")%>%
  #mutate_at(vars(y5:y95), ~ ifelse(Type == "mean", ., NA))%>%
  select(Country, Type, Mean)%>%
  mutate(Type_A = ifelse(Type == "mean", "Total",
                         ifelse(Type == "mean_coal", "Coal Price Increase 100%",
                                ifelse(Type == "mean_p_c", "Liquid Fuel Price Increase 100%",
                                       ifelse(Type == "mean_gas", "Gas Price Increase 100%", NA)))))

data_7.8.2.1$Type_A <- factor(data_7.8.2.1$Type_A, levels = c("Total",
                                                              "Gas Price Increase 100%",
                                                              "Liquid Fuel Price Increase 100%",
                                                              "Coal Price Increase 100%"))

plot_7.8.2 <- ggplot()+
  geom_boxplot(data = data_7.8.2, aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95, x = Country), fill = "lightgrey", alpha = 1, stat = "identity", position = position_dodge(0), outlier.shape = NA, width = 0.5, size = 0.3) +
  geom_segment(aes(x = 1.01, xend = 0.99, y = 0.13116240, yend = 0.13116240, colour = "Median Incidence"), size = 0.8)+
  theme_bw()+
  xlab("")+ ylab("Additional Costs (in % of Total Expenditures)")+
  geom_point(data = data_7.8.2.1, aes(y = Mean, fill = Type_A, x = factor(Country), shape = Type_A), size = 1.7, stroke = 0.5)+
  #geom_point(aes(y = mean_coal), shape = 24, fill = "#BC3C29FF", colour = "black", size = 1.5, stroke = 0.2)+
  #geom_point(aes(y = mean_p_c),  shape = 22, fill = "#0072B5FF", colour = "black", size = 1.5, stroke = 0.2)+
  #geom_point(aes(y = mean_gas),  shape = 25, fill = "#20854EFF", colour = "black", size = 1.5, stroke = 0.2)+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0))+
  #scale_x_discrete(limits = rev)+
  #coord_flip(ylim = c(0,0.45))+
  coord_cartesian(ylim = c(0,0.45))+
  ggtitle("")+
  labs(fill = "Average Incidence", shape = "Average Incidence")+
  scale_fill_manual( name = "Average Incidence", values = c("white", "#20854EFF", "#0072B5FF", "#BC3C29FF"))+
  scale_shape_manual(name = "Average Incidence", values = c(21,22,23,24))+
  scale_colour_manual("", values = "black", breaks = "Median Incidence")+
  #scale_fill_nejm()+
  guides(fill = guide_legend(nrow = 2, order = 1, override.aes = list(shape = c(21,22,23,24))), colour = guide_legend(order = 2),
         shape = "none")+
  theme(axis.text.y = element_text(size = 7), 
        axis.text.x = element_text(size = 7, hjust = 0.95, vjust = 0.5, angle = 90),
        axis.title  = element_text(size = 7),
        plot.title = element_text(size = 7),
        legend.position = "bottom",
        strip.text = element_text(size = 7),
        strip.text.y = element_text(angle = 180),
        panel.grid.major = element_line(size = 0.3),
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(size = 0.2),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        legend.box = "vertical",
        plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"),
        panel.border = element_rect(size = 0.3))

jpeg("C:/Users/misl/ownCloud/EU_Gasprice_Analysis/Figures/2022_04_26_Figures/Figure_1a_100.jpg", width = 15, height = 17, unit = "cm", res = 400)
print(plot_7.8.2)
dev.off()

data_7.8.3 <- data_7.8 %>%
  select(hh_id, hh_weights, Country, burden_100_gas, burden_100_coal, burden_100_p_c)%>%
  pivot_longer(starts_with("burden"), names_to = "type", values_to = "burden")%>%
  group_by(Country, type)%>%
  summarise(
    y5   = wtd.quantile(burden, weights = hh_weights, probs = 0.05),
    y25  = wtd.quantile(burden, weights = hh_weights, probs = 0.25),
    y50  = wtd.quantile(burden, weights = hh_weights, probs = 0.5),
    y75  = wtd.quantile(burden, weights = hh_weights, probs = 0.75),
    y95  = wtd.quantile(burden, weights = hh_weights, probs = 0.95),
    mean = wtd.mean(   burden, weights = hh_weights))%>%
  ungroup()%>%
  mutate(Type_A = ifelse(type == "burden_100_coal", "Coal",
                         ifelse(type == "burden_100_gas", "Gas",
                                ifelse(type == "burden_100_p_c", "Oil",
                                       ifelse(type == "mean_gas", "Gas Price Increase 100%", NA)))))

data_7.8.3$Type_A <- factor(data_7.8.3$Type_A, levels = c("Gas",
                                                              "Oil",
                                                              "Coal"))

data_7.8.3 <- left_join(data_7.8.3, data_8.1.3.X)

data_7.8.3$Country <- fct_reorder(data_7.8.3$Country, data_7.8.3$order_no)

plot_7.8.3 <- ggplot(data_7.8.3)+
  facet_wrap(. ~ Type_A, ncol = 1)+
  geom_boxplot(data = data_7.8.3, aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95, x = factor(Country)), fill = "lightgrey", alpha = 1, stat = "identity", position = position_dodge(0), outlier.shape = NA, width = 0.5, size = 0.3) +
  geom_segment(aes(x = 1.01, xend = 0.99, y = 0.13116240, yend = 0.13116240, colour = "Median incidence"), size = 0.8)+
  theme_bw()+
  xlab("")+ ylab("Additional Costs (in % of Total Expenditures)")+
  geom_point(data = data_7.8.3, aes(y = mean, fill = Type_A, x = factor(Country), shape = Type_A), size = 1.7, stroke = 0.5)+
  #geom_point(aes(y = mean_coal), shape = 24, fill = "#BC3C29FF", colour = "black", size = 1.5, stroke = 0.2)+
  #geom_point(aes(y = mean_p_c),  shape = 22, fill = "#0072B5FF", colour = "black", size = 1.5, stroke = 0.2)+
  #geom_point(aes(y = mean_gas),  shape = 25, fill = "#20854EFF", colour = "black", size = 1.5, stroke = 0.2)+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0.01))+
  #scale_x_discrete(limits = rev)+
  #coord_flip(ylim = c(0,0.45))+
  coord_cartesian(ylim = c(0,0.32))+
  ggtitle("")+
  labs(fill = "Average incidence of fuel price increase", shape = "Average incidence of fuel price increase")+
  scale_fill_manual( name = "Average incidence of fuel price increase", values = c("#20854EFF", "#0072B5FF", "#BC3C29FF"))+
  scale_shape_manual(name = "Average incidence of fuel price increase", values = c(22,23,24))+
  scale_colour_manual("", values = "black", breaks = "Median incidence")+
  #scale_fill_nejm()+
  guides(fill = guide_legend(nrow = 1, override.aes = list(shape = c(22,23,24)), order = 1, title.position = "top", title.hjust = 0.5), colour = guide_legend(order = 2),
         shape = "none")+
  theme(axis.text.y = element_text(size = 7), 
        axis.text.x = element_text(size = 7, hjust = 0.95, vjust = 0.5, angle = 90),
        axis.title  = element_text(size = 7),
        plot.title = element_text(size = 7),
        legend.position = "bottom",
        strip.text = element_text(size = 7),
        strip.text.y = element_text(angle = 180),
        panel.grid.major = element_line(size = 0.3),
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(size = 0.2),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        legend.box = "horizontal",
        plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"),
        panel.border = element_rect(size = 0.3))

jpeg("C:/Users/misl/ownCloud/EU_Gasprice_Analysis/Figures/2022_05_20_Figures/Figure_5.jpg", width = 15, height = 20, unit = "cm", res = 400)
print(plot_7.8.3)
dev.off()


# 7.9   Energy Mix in 2020 (Simon) ####

country_list <- c("Belgium", "Bulgaria", "Cyprus", "Czech Republic", "Germany", "Denmark", "Estonia", "Greece", "Spain", "Finland",
                  "France", "Croatia", "Hungary" ,"Ireland", "Italy", "Lithuania", "Luxembourg", "Latvia", "Netherlands", "Poland",
                  "Portugal", "Romania", "Sweden", "Slovak Republic")

energy_use <- read.xlsx("K:/WorkInProgress/2021_Carbon_Footprint_Analysis/Supplementary Information/EnergyMixDependencyImports_25-03-2022.xlsx")%>%
  rename("Country" = Energy.mix.in.2020,
         "Solid fossil fuels" = X2,
         "Natural gas" = X3,
         "Oil and petroleum" = X4,
         "Renewables and biofuels" = X5,
         "Non-renewable waste" = X6,
         "Nuclear heat" = X7,
         "Electricity (net imports)" = X8,
         "Other" = X9)%>%
  mutate(Country = if_else(Country=="Czechia","Czech Republic",
                           if_else(Country=="Slovakia","Slovak Republic",Country)))%>%
  slice(8:n())%>%
  filter(Country %in% country_list)%>%
  pivot_longer(cols = 2:9, names_to="Energy_type", values_to="share")%>%
  mutate(share = as.numeric(share))%>%
  mutate(Energy_type = ifelse(Energy_type == "Electricity (net imports)" & share < 0, "Electricity (Exports)",
                              ifelse(Energy_type == "Electricity (net imports)" & share >= 0, "Electricity (Imports)", Energy_type)))%>%
  group_by(Country)%>%
  mutate(sum_share = sum(share))%>%
  ungroup()%>%
  mutate(gas_share = ifelse(Energy_type == "Natural gas", share,NA))%>%
  arrange(gas_share)%>%
  mutate(number = 1:n())%>%
  mutate(number = ifelse(gas_share == 0,0, number))%>%
  group_by(Country)%>%
  mutate(number_0 = sum(number, na.rm = TRUE))%>%
  ungroup()

energy_use$Energy_type <- factor(energy_use$Energy_type, levels = c("Natural gas", "Oil and petroleum", "Solid fossil fuels",
                                                                    "Renewables and biofuels", "Electricity (Imports)", "Electricity (Exports)",
                                                                    "Nuclear heat", "Non-renewable waste", "Other"))

plot_7.9 <- ggplot(data = energy_use)+
  geom_col(aes (y = reorder(Country, number_0), x = share, fill = reorder(Energy_type, desc(Energy_type))), colour = "black", width = 0.8)+
  scale_fill_brewer(name = "Energy type", limits = rev, palette = "Set3")+
  scale_x_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0.02))+
  theme_bw()+
  ylab("Country")+ xlab("Share of Gross Available Energy")+
  guides(fill = guide_legend(nrow = 3, order = 1), colour = guide_legend(order = 2))+
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
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        legend.box = "vertical",
        plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"),
        panel.border = element_rect(size = 0.3))

jpeg("C:/Users/misl/ownCloud/EU_Gasprice_Analysis/Figures/2022_05_20_Figures/Figure_A1.jpg", width = 15, height = 20, unit = "cm", res = 400)
print(plot_7.9)
dev.off()


# 7.10  Scenarios / Table ####

data_7.10 <- left_join(carbon_pricing_incidence_0, household_information_0)%>%
  mutate(burden_GAS_gas_100   = burden_GAS_gas*100/138,
         burden_GAS_gas_200   = burden_GAS_gas*200/138,
         burden_COAL_coal_100 = burden_COAL_coal*100/30,
         burden_COAL_coal_50  = burden_COAL_coal*50/30,
         burden_P_C_p_c_100   = burden_P_C_p_c*100/30,
         burden_P_C_p_c_50    = burden_P_C_p_c*50/30)%>%
  mutate(burden_total_base    = burden_GAS_gas + burden_COAL_coal + burden_P_C_p_c,
         burden_total_100     = burden_GAS_gas_100 + burden_COAL_coal_100 + burden_P_C_p_c_100,
         burden_total_200     = burden_GAS_gas_200 + burden_COAL_coal_50  + burden_P_C_p_c_50)%>%
  mutate(Heating_Gas = ifelse(is.na(Heating_Gas),0,Heating_Gas),
         Heating_NA  = ifelse(is.na(Heating_NA), 0,Heating_NA),
         Heating_Oil = ifelse(is.na(Heating_Oil),0, Heating_Oil),
         Heating_District_Heat = ifelse(is.na(Heating_District_Heat),0, Heating_District_Heat))%>%
  mutate(exp_total_base = burden_total_base*hh_expenditures_USD_2014,
         exp_total_100  = burden_total_100 *hh_expenditures_USD_2014,
         exp_total_200  = burden_total_200 *hh_expenditures_USD_2014)

data_7.10.1 <- data_7.10 %>%
  mutate(exp_total_base_weighted = exp_total_base*hh_weights,
         exp_total_100_weighted  = exp_total_100*hh_weights,
         exp_total_200_weighted  = exp_total_200*hh_weights)%>%
  group_by(Country)%>%
  summarise(compensation_weighted      = sum(exp_total_base_weighted, na.rm = TRUE),
            compensation_weighted_100  = sum(exp_total_100_weighted,  na.rm = TRUE),
            compensation_weighted_200  = sum(exp_total_200_weighted,  na.rm = TRUE),
            hh_weights_sum          = sum(hh_weights))%>%
  ungroup()%>%
  pivot_longer(starts_with("compensation_weighted"), names_to = "Comp_Scenario", values_to = "Comp")%>%
  mutate(Type_A = "Household Transfer Country-Level \n Average Additional Costs")

data_7.7.10.2 <- data_7.7.5 %>%
  mutate(compensation_weighted_100 = compensation_weighted*100/138,
         compensation_weighted_200 = compensation_weighted*200/138)

data_7.7.10.3 <- data_7.7.10.2 %>%
  filter(Type_A == "Compensation via Gas / District Heat \n Payments for all Households with Gas Use" |
           Type_A == "Compensation via Gas / District Heat \n Payments for Lower Income Households")%>%
  pivot_longer(starts_with("compensation_weighted"), names_to = "Comp_Scenario", values_to = "Comp")

GDP_0 <- read.xlsx("C:/Users/misl/ownCloud/EU_Gasprice_Analysis/Supplementary Information/Compensation_Requirement.xlsx", colNames = TRUE, startRow = 2)%>%
  mutate(Country = ifelse(Country == "Slovak Republic", "Slovakia", Country))%>%
  select(Country, GDP)

data_7.7.10.4 <- bind_rows(data_7.10.1, data_7.7.10.3)%>%
  mutate(Country = ifelse(Country == "Slovak Republic", "Slovakia", Country))%>%
  left_join(GDP_0)%>%
  mutate(share = Comp/GDP)%>%
  select(Country, Type_A, Comp_Scenario, share)%>%
  unite("Type", c("Type_A", "Comp_Scenario"), sep = "_")%>%
  pivot_wider(names_from = "Type", values_from = "share")%>%
  arrange(Country)%>%
  select(Country, starts_with("Compensation via Gas / District Heat \n Payments for all Households with Gas Use"),
         starts_with("Compensation via Gas / District Heat \n Payments for Lower Income Households"),
         everything())%>%
  select(Country, ends_with("compensation_weighted"), ends_with("compensation_weighted_100"), ends_with("compensation_weighted_200"))

write.xlsx(data_7.7.10.4, "C:/Users/misl/ownCloud/EU_Gasprice_Analysis/Supplementary Information/Compensation_over_GDP.xlsx")  



# 7.11  Historical Gas Price Increase vs. Scenario vs. Range of Estimates ####

data_scenario <- data.frame(Szenario = c("Reference", "Stylized Scenario", "Baseline Scenario", "Embargo Scenario"), 
                            Gas_average = c(25,50,110,200), 
                            Oil_average = c(60, 120, 110, 140), 
                            Coal_average = c(NA, NA, NA, NA))

data_scenario$Szenario <- factor(data_scenario$Szenario, levels = c("Reference", "Stylized Scenario", "Baseline Scenario", "Embargo Scenario"))

library(readxl)

data_prices <- read_excel("C:/Users/misl/OwnCloud/EU_Gasprice_Analysis/Natural_Gas_Prices.xlsx", sheet = "Data")%>%
  rename(Country = TIME)%>%
  select(-starts_with("."))%>%
  pivot_longer(-Country, names_to = "Date", values_to = "value")%>%
  mutate(Date = as.numeric(Date))%>%
  mutate(Date_0 = as.Date(Date, origin = "1900-01-01"))%>%
  filter(Country == "EU")%>%
  mutate(value = value*1000)

data_prices_2 <- read_excel("C:/Users/misl/OwnCloud/EU_Gasprice_Analysis/CMO-Historical-Data-Monthly_edit.xlsx", 
                               sheet = "Monthly Prices", skip = 7)%>%
  select(Date, Natural_gas)%>%
  mutate(Date = as.Date(as.character(Date)))

data_prices_3 <- read_excel("C:/Users/misl/OwnCloud/EU_Gasprice_Analysis/CMO-Historical-Data-Monthly_edit.xlsx", sheet = "Monthly Prices",
                            skip = 7)%>%
  select(Date, Oil_Brent)%>%
  mutate(Date = as.Date(as.character(Date)))%>%
  filter(Oil_Brent != "..")%>%
  mutate(Oil_Brent = as.numeric(Oil_Brent))

data_others <- read.xlsx("C:/Users/misl/OwnCloud/EU_Gasprice_Analysis/2022_04_21_Ukraine_gas_prices_data.xlsx", sheet = "Data")%>%
  filter(!is.na(Szenario))%>%
  mutate(number = 1:n())%>%
  select(Publisher, number, Szenario, Gas_high, Gas_low, Gas_average, Oil_high, Oil_low, Oil_average)%>%
  filter(number != 23 & number != 21)%>%
  arrange(Gas_low)%>%
  mutate(Gas_number = (1:n())/20+0.2)%>%
  mutate(Szenario = ifelse(Szenario == "Basis", "Low", 
                           ifelse(Szenario == "Moderate", "Middle",
                                  ifelse(Szenario == "Extreme", "Upper", Szenario))))%>%
  arrange(Oil_low)%>%
  mutate(Oil_number = (1:n())/20+0.2)

data_others$Szenario <- factor(data_others$Szenario, levels = c("Low", "Middle", "Upper", "Positive"))

add_MWH <- function(x){
  format(paste0(x, "€/MWh"))
}

plot_7.11.1 <- ggplot()+
  annotate("rect", xmin = -0.25, xmax = 0.64, ymin = 109, ymax = 111, fill = "lightgrey", colour = "lightgrey")+
  annotate("rect", xmin = -0.25, xmax = 0.64, ymin = 199, ymax = 201, fill = "lightgrey", colour = "lightgrey")+
  annotate("rect", xmin = -0.25, xmax = 0.64, ymin = 24, ymax = 26, fill = "lightgrey", colour = "lightgrey")+
  annotate("rect", xmin = -0.25, xmax = 0.64, ymin = 49, ymax = 51, fill = "lightgrey", colour = "lightgrey")+
  geom_point(data = filter(data_others, !is.na(Gas_average)), aes(x = 0, y = Gas_average, fill = "Prices in other studies"), 
             position = position_jitter(width = 0.2, height = 0), colour = "black", shape = 21)+
  geom_errorbar(data = filter(data_others, !is.na(Gas_low)), aes(x = Gas_number, ymin = Gas_low, ymax = Gas_high, colour = "Prices in other studies"), width = 0.05, size = 0.3)+
  theme_bw()+
  scale_x_continuous(expand = c(0,0), breaks = c(0.2), labels = "2022")+
  scale_y_continuous(labels = add_MWH, expand = c(0,0))+
  scale_fill_manual(  values = c("#6F99ADFF"))+
  scale_colour_manual(values = c("#6F99ADFF"))+
  #scale_fill_nejm()+
  #scale_colour_nejm()+
  ylab("")+
  labs(fill = "Prices in other studies", colour = "Prices in other studies")+
  xlab("Other Studies")+
  ggtitle("C.1)")+
  coord_cartesian(ylim = c(0,250))+
  theme(axis.text.y = element_blank(), 
        axis.text.x = element_text(size = 7),
        axis.title  = element_text(size = 7),
        plot.title = element_text(size = 7, face = "bold"),
        legend.position = "bottom",
        strip.text = element_text(size = 7),
        strip.text.y = element_text(angle = 180),
        panel.grid.major.y = element_line(size = 0.3),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(size = 0.2),
        legend.text = element_text(size = 6),
        legend.title = element_text(size = 6),
        legend.margin = margin(t = 0, unit = "cm"),
        legend.justification = "left",
        plot.margin = unit(c(0.2,0.2,0.2,0), "cm"),
        panel.border = element_rect(size = 0.3))

plot_7.11.2 <- ggplot()+
  annotate("rect", xmin = 0.5, xmax = 1.5, ymin = 109, ymax = 111, fill = "lightgrey", colour = "lightgrey")+
  annotate("rect", xmin = 0.5, xmax = 1.5, ymin = 199, ymax = 201, fill = "lightgrey", colour = "lightgrey")+
  annotate("rect", xmin = 0.5, xmax = 1.5, ymin = 24, ymax = 26, fill = "lightgrey", colour = "lightgrey")+
  annotate("rect", xmin = -0.5, xmax = 1.5, ymin = 49, ymax = 51, fill = "lightgrey", colour = "lightgrey")+
  geom_point(data = data_scenario, aes(x = 1, y = Gas_average, fill = Szenario, shape = Szenario), colour = "black")+
  theme_bw()+
  scale_x_continuous(expand = c(0,0.1), breaks = c(1), labels = "2022")+
  scale_y_continuous(labels = add_MWH, expand = c(0,0))+
  coord_cartesian(ylim = c(0,250), xlim = c(0.7,1.3))+
  ylab("")+
  xlab("This Study")+
  labs(fill = "Prices in this study", shape = "Prices in this study")+
  scale_fill_nejm()+
  ggtitle("B.1)")+
  scale_shape_manual(values = c(22,23,24,25))+
  theme(axis.text.y = element_blank(), 
        axis.text.x = element_text(size = 7),
        axis.title  = element_text(size = 7),
        plot.title = element_text(size = 7, face = "bold"),
        legend.position = "bottom",
        strip.text = element_text(size = 7),
        strip.text.y = element_text(angle = 180),
        panel.grid.major.y = element_line(size = 0.3),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(size = 0.2),
        legend.text = element_text(size = 6),
        legend.title = element_text(size = 6),
        legend.justification = "left",
        legend.box = "vertical",
        legend.margin = margin(t = 0, unit = "cm"),
        plot.margin = unit(c(0.2,0.2,0.2,0), unit = "cm"),
        panel.border = element_rect(size = 0.3))

legend_1 <- ggdraw(get_legend(plot_7.11.2))
legend_2 <- ggdraw(get_legend(plot_7.11.1))

plot_7.11.3 <- ggplot()+
  annotate("rect", xmin = as.Date("2014-01-01"), xmax = as.Date("2022-09-01"), ymin = 109, ymax = 111, fill = "lightgrey", colour = "lightgrey")+
  annotate("rect", xmin = as.Date("2014-01-01"), xmax = as.Date("2022-09-01"), ymin = 199, ymax = 201, fill = "lightgrey", colour = "lightgrey")+
  annotate("rect", xmin = as.Date("2014-01-01"), xmax = as.Date("2022-09-01"), ymin = 24, ymax = 26, fill = "lightgrey", colour = "lightgrey")+
  annotate("rect", xmin = as.Date("2014-01-01"), xmax = as.Date("2022-09-01"), ymin = 49, ymax = 51, fill = "lightgrey", colour = "lightgrey")+
  #geom_line(data = data_prices, aes(x = Date_0, y = value), colour = "#BC3C29FF")+
  geom_line(data = data_prices_2, aes(x = Date, y = Natural_gas), colour = "#BC3C29FF")+
  theme_bw()+
  scale_y_continuous(expand = c(0,0), sec.axis = sec_axis(~ ((./25)-1), name = "Price Change in %", labels = scales::percent_format(accuracy = 1), breaks = c(0,1,3.4,7)))+
  scale_x_date(expand = c(0,0), breaks = seq(as.Date("2014-01-01"), as.Date("2022-01-01"), by = "2 years"), labels = date_format("%b %Y"))+
  coord_cartesian(ylim = c(0,250),
                  xlim = c(as.Date("2014-01-01"),as.Date("2022-06-01")))+
  xlab("Historical prices")+
  ylab("Natural Gas Price in €/MWh")+
  ggtitle("A.1) Natural gas")+
  theme(axis.text.y = element_text(size = 7), 
        axis.text.x = element_text(size = 7),
        axis.text.y.right = element_text(size = 6),
        axis.title  = element_text(size = 7),
        plot.title = element_text(size = 7, face = "bold"),
        legend.position = "bottom",
        strip.text = element_text(size = 7),
        strip.text.y = element_text(angle = 180),
        panel.grid.major = element_line(size = 0.3),
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(size = 0.2),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        legend.box = "vertical",
        plot.margin = unit(c(0.4,0.,0.4,0.4), "cm"),
        panel.border = element_rect(size = 0.3))

p_1 <- plot_grid(plot_7.11.3 + theme(legend.position = "none"),
          plot_7.11.2 + theme(legend.position = "none"),
          plot_7.11.1 + theme(legend.position = "none"), align = "h", axis = "bt", nrow = 1,
          labels = c("auto"), rel_widths = c(5,1,1), label_size = 7)

p_2 <- plot_grid(p_1,
                 legend_1,
                 legend_2, nrow = 3, rel_heights = c(8,1,1), greedy = FALSE)




#jpeg("C:/Users/misl/ownCloud/EU_Gasprice_Analysis/Figures/2022_05_20_Figures/Figure_1.jpg", width = 15, height = 8, unit = "cm", res = 400)
#print(p_2)
#dev.off()

# For Oil 

plot_7.12.1 <- ggplot()+
  annotate("rect", xmin = -0.25, xmax = 0.5, ymin = 139, ymax = 141, fill = "lightgrey", colour = "lightgrey")+
  annotate("rect", xmin = -0.25, xmax = 0.5, ymin = 109, ymax = 111, fill = "lightgrey", colour = "lightgrey")+
  annotate("rect", xmin = -0.25, xmax = 0.5, ymin = 59, ymax = 61, fill = "lightgrey", colour = "lightgrey")+
  annotate("rect", xmin = -0.25, xmax = 0.5, ymin = 119, ymax = 121, fill = "lightgrey", colour = "lightgrey")+
  geom_point(data = filter(data_others, !is.na(Oil_average)), aes(x = 0, y = Oil_average, fill = "Prices in other studies"), 
             position = position_jitter(width = 0.2, height = 0), colour = "black", shape = 21)+
  geom_errorbar(data = filter(data_others, !is.na(Oil_low)), aes(x = Oil_number, ymin = Oil_low, ymax = Oil_high, colour = "Prices in other studies"), width = 0.05, size = 0.3)+
  theme_bw()+
  scale_x_continuous(expand = c(0,0), breaks = 0.125, labels = "2022")+
  scale_y_continuous(labels = add_MWH, expand = c(0,0))+
  scale_fill_manual(values   = c("#6F99ADFF"))+
  scale_colour_manual(values = c("#6F99ADFF"))+
  #scale_fill_nejm()+
  #scale_colour_nejm()+
  ylab("")+
  labs(fill = "Prices in other studies", colour = "Prices in other studies")+
  xlab("Other Studies")+
  ggtitle("C.2)")+
  coord_cartesian(ylim = c(0,200))+
  theme(axis.text.y = element_blank(), 
        axis.text.x = element_text(size = 7),
        axis.title  = element_text(size = 7),
        plot.title = element_text(size = 7, face = "bold"),
        legend.position = "bottom",
        strip.text = element_text(size = 7),
        strip.text.y = element_text(angle = 180),
        panel.grid.major.y = element_line(size = 0.3),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(size = 0.2),
        legend.text = element_text(size = 6),
        legend.title = element_text(size = 6),
        legend.margin = margin(t = 0, unit = "cm"),
        legend.justification = "left",
        plot.margin = unit(c(0.2,0.2,0.2,0), "cm"),
        panel.border = element_rect(size = 0.3))

plot_7.12.2 <- ggplot()+
  annotate("rect", xmin = 0.5, xmax = 1.5, ymin = 139, ymax = 141, fill = "lightgrey", colour = "lightgrey")+
  annotate("rect", xmin = 0.5, xmax = 1.5, ymin = 109, ymax = 111, fill = "lightgrey", colour = "lightgrey")+
  annotate("rect", xmin = 0.5, xmax = 1.5, ymin = 59, ymax = 61, fill = "lightgrey", colour = "lightgrey")+
  annotate("rect", xmin = 0.5, xmax = 1.5, ymin = 119, ymax = 121, fill = "lightgrey", colour = "lightgrey")+
  geom_point(data = data_scenario, aes(x = 1, y = Oil_average, fill = Szenario, shape = Szenario), colour = "black")+
  theme_bw()+
  scale_x_continuous(expand = c(0,0.1), breaks = c(1), labels = "2022")+
  scale_y_continuous(labels = add_MWH, expand = c(0,0))+
  coord_cartesian(ylim = c(0,200), xlim = c(0.7,1.3))+
  ylab("")+
  xlab("This Study")+
  ggtitle("B.2)")+
  labs(fill = "Prices in this study", shape = "Prices in this study")+
  scale_fill_nejm()+
  scale_shape_manual(values = c(22,23,24,25))+
  theme(axis.text.y = element_blank(), 
        axis.text.x = element_text(size = 7),
        axis.title  = element_text(size = 7),
        plot.title = element_text(size = 7, face = "bold"),
        legend.position = "bottom",
        strip.text = element_text(size = 7),
        strip.text.y = element_text(angle = 180),
        panel.grid.major.y = element_line(size = 0.3),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(size = 0.2),
        legend.text = element_text(size = 6),
        legend.title = element_text(size = 6),
        legend.justification = "left",
        legend.box = "vertical",
        legend.margin = margin(t = 0, unit = "cm"),
        plot.margin = unit(c(0.2,0.2,0.2,0), unit = "cm"),
        panel.border = element_rect(size = 0.3))

legend_1 <- ggdraw(get_legend(plot_7.12.2))
legend_2 <- ggdraw(get_legend(plot_7.12.1))

plot_7.12.3 <- ggplot()+
  annotate("rect", xmin = as.Date("2014-01-01"), xmax = as.Date("2022-09-01"), ymin = 109, ymax = 111, fill = "lightgrey", colour = "lightgrey")+
  annotate("rect", xmin = as.Date("2014-01-01"), xmax = as.Date("2022-09-01"), ymin = 139, ymax = 141, fill = "lightgrey", colour = "lightgrey")+
  annotate("rect", xmin = as.Date("2014-01-01"), xmax = as.Date("2022-09-01"), ymin = 59, ymax = 61, fill = "lightgrey", colour = "lightgrey")+
  annotate("rect", xmin = as.Date("2014-01-01"), xmax = as.Date("2022-09-01"), ymin = 119, ymax = 121, fill = "lightgrey", colour = "lightgrey")+
  #geom_line(data = data_prices, aes(x = Date_0, y = value), colour = "#BC3C29FF")+
  geom_line(data = data_prices_3, aes(x = Date, y = Oil_Brent), colour = "#BC3C29FF")+
  theme_bw()+
  ggtitle("A.2) Crude oil")+
  scale_y_continuous(expand = c(0,0), sec.axis = sec_axis(~ ((./60)-1), name = "Price Change in %", labels = scales::percent_format(accuracy = 1), breaks = c(0,0.83,1,1.33)))+
  scale_x_date(expand = c(0,0), breaks = seq(as.Date("2014-01-01"), as.Date("2022-01-01"), by = "2 years"), labels = date_format("%b %Y"))+
  coord_cartesian(ylim = c(0,200),
                  xlim = c(as.Date("2014-01-01"),as.Date("2022-06-01")))+
  xlab("Historical prices")+
  ylab("Crude Oil (Brent) Price in USD/bbl")+
  theme(axis.text.y = element_text(size = 7),
        axis.text.y.right = element_text(size = 6),
        axis.text.x = element_text(size = 7),
        axis.title  = element_text(size = 7),
        plot.title = element_text(size = 7, face = "bold"),
        legend.position = "bottom",
        strip.text = element_text(size = 7),
        strip.text.y = element_text(angle = 180),
        panel.grid.major = element_line(size = 0.3),
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(size = 0.2),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        legend.box = "vertical",
        plot.margin = unit(c(0.4,0.0,0.4,0.4), "cm"),
        panel.border = element_rect(size = 0.3))

p_1.1 <- plot_grid(plot_7.12.3 + theme(legend.position = "none"),
                 plot_7.12.2 + theme(legend.position = "none"),
                 plot_7.12.1 + theme(legend.position = "none"), align = "h", axis = "bt", nrow = 1,
                 labels = c("auto"), rel_widths = c(5,1,1), label_size = 7)

p_2.1 <- plot_grid(p_1.1,
                 legend_1,
                 legend_2, nrow = 3, rel_heights = c(8,1,1), greedy = FALSE)


# jpeg("C:/Users/misl/ownCloud/EU_Gasprice_Analysis/Figures/2022_05_06_Figures/Figure_1_Oilprices.jpg", width = 15, height = 8, unit = "cm", res = 400)
# print(p_2.1)
# dev.off()

p_3.0 <- plot_grid(legend_1, legend_2, nrow = 2)

p_3.1 <- plot_grid(plot_7.11.3 + theme(legend.position = "none"),
                 plot_7.11.2 + theme(legend.position = "none"),
                 plot_7.11.1 + theme(legend.position = "none"), 
                 plot_7.12.3 + theme(legend.position = "none"),
                 plot_7.12.2 + theme(legend.position = "none"),
                 plot_7.12.1 + theme(legend.position = "none"),
                 align = "h", axis = "bt", nrow = 2, rel_widths = c(5,1,1,5,1,1), rel_heights = c(8,8,8,8,8,8),
                 greedy = FALSE)

p_3.2 <- plot_grid(p_3.1, p_3.0, nrow = 2, rel_heights = c(4,1))


jpeg("C:/Users/misl/ownCloud/EU_Gasprice_Analysis/Figures/2022_05_20_Figures/Figure_1.jpg", width = 15, height = 13, unit = "cm", res = 400)
print(p_3.2)
dev.off()

# 8.    Updates Prices (from Nils) ####

# Baseline-Scenario 

Gas_prices_households <- read.xlsx("C:/Users/misl/ownCloud/EU_Gasprice_Analysis/Price markups/nrg_pc_202_c__custom_2588515_spreadsheet_households_baseline.xlsx", sheet = "Markup calculation",
                                   startRow = 12)%>%
  select(X1, Total.markup.factor)%>%
  rename(Country = X1, Markup_Gas_Direct = Total.markup.factor)%>%
  mutate(Country = ifelse(Country == "Slovakia", "Slovak Republic",
                          ifelse(Country == "Germany (until 1990 former territory of the FRG)", "Germany",
                                 ifelse(Country == "Czechia", "Czech Republic", Country))))%>%
  # Average for Europe
  bind_rows(data.frame(Country = c("Finland", "Cyprus"), Markup_Gas_Direct = c(2.039338, 2.039338)))

Gas_prices_industry <- read.xlsx("C:/Users/misl/ownCloud/EU_Gasprice_Analysis/Price markups/nrg_pc_203_c__custom_2591778_spreadsheet_non_households_baseline.xlsx", sheet = "Markup calculation",
                                   startRow = 12)%>%
  select(X1, Total.markup.factor)%>%
  rename(Country = X1, Markup_Gas_Indirect = Total.markup.factor)%>%
  mutate(Country = ifelse(Country == "Slovakia", "Slovak Republic",
                          ifelse(Country == "Germany (until 1990 former territory of the FRG)", "Germany",
                                 ifelse(Country == "Czechia", "Czech Republic", Country))))%>%
  # Average for Europe
  bind_rows(data.frame(Country = c("Cyprus"), Markup_Gas_Indirect = c(2.697971)))

# Oil prices: Decomposition

Oil_prices_0 <- read.xlsx("C:/Users/misl/ownCloud/EU_Gasprice_Analysis/Oil_Bulletin_Prices_History_arbeitsversion_baseline_final.xlsx", sheet = "Overview clean",
                        startRow = 4)%>%
  rename(Gasoline_Markup_0 = X31,
         Diesel_Markup_0   = X32,
         Heating_Markup_0  = X33,
         LPG_Markup_0      = X34,
         Country           = X35)%>%
  select(Country, Gasoline_Markup_0, Diesel_Markup_0, Heating_Markup_0, LPG_Markup_0)%>%
  filter(!is.na(Country))%>%
  pivot_longer(-Country, names_to = "Type", values_to = "Markup")%>%
  filter(!is.na(Markup))%>%
  group_by(Country)%>%
  mutate(Markup_Average    = mean(Markup))%>%
  ungroup()%>%
  pivot_wider(names_from = "Type", values_from = "Markup")%>%
  mutate(Transport_Markup = (Diesel_Markup_0 + Gasoline_Markup_0)/2)%>%
  mutate(Heating_Markup_0 = ifelse(is.na(Heating_Markup_0), 0.9252044, Heating_Markup_0),
         LPG_Markup_0     = ifelse(is.na(LPG_Markup_0),     0.9316285, LPG_Markup_0))%>%
  pivot_longer(-Country, names_to = "Type_0", values_to = "Markup")%>%
  mutate(Type = ifelse(Type_0 == "Diesel_Markup_0", "Diesel",
                       ifelse(Type_0 == "Gasoline_Markup_0", "Gasoline",
                              ifelse(Type_0 == "Heating_Markup_0", "Heating_0",
                                     ifelse(Type_0 == "LPG_Markup_0", "LPG_0",
                                            ifelse(Type_0 == "Transport_Markup", "Other Transport Fuels", NA))))))

Oil_prices_1 <- Oil_prices_0 %>%
  filter(Type_0 == "Markup_Average")%>%
  rename(average_oil = Markup)%>%
  select(Country, average_oil)

Direct_Fuel_Expenditures <- read_csv("K:/WorkInProgress/2021_Carbon_Footprint_Analysis/Data_Transformed/Fuel_Direct_Effects_Europe.csv")

direct_fuel_exp_1 <- left_join(Direct_Fuel_Expenditures, select(household_information_0, hh_id, Country))%>%
  pivot_longer(starts_with("burden"), names_to = "Type", values_to = "burden_100", names_prefix = "burden_lf_100_")%>%
  left_join(Oil_prices_0, by = c("Country", "Type"))%>%
  mutate(burden_liquid_fuel_direct = burden_100*Markup)%>%
  group_by(hh_id)%>%
  summarise(burden_liquid_fuel_direct = sum(burden_liquid_fuel_direct))%>%
  ungroup()

data_8 <- left_join(carbon_pricing_incidence_0, household_information_0)%>%
  left_join(Gas_prices_households)%>%
  left_join(Gas_prices_industry)%>%
  left_join(Oil_prices_1)%>%
  left_join(direct_fuel_exp_1)%>%
  mutate(burden_liquid_fuel_direct = ifelse(is.na(burden_liquid_fuel_direct), 0, burden_liquid_fuel_direct))%>%
  mutate(coal_markup = 1.5)%>%
  mutate(burden_gas_scenario_direct    = burden_GAS_direct*Markup_Gas_Direct,
         burden_gas_scenario_indirect  = burden_GAS_indirect*Markup_Gas_Indirect,
         burden_p_c_scenario_direct    = burden_liquid_fuel_direct,
         burden_p_c_scenario_indirect  = burden_P_C_indirect*average_oil,
         burden_coal_scenario          = burden_COAL_coal*coal_markup,
         burden_coal_scenario_direct   = burden_COAL_direct*coal_markup,
         burden_coal_scenario_indirect = burden_COAL_indirect*coal_markup)%>%
  mutate(burden_gas_scenario           = burden_gas_scenario_direct + burden_gas_scenario_indirect,
         burden_p_c_scenario           = burden_p_c_scenario_direct + burden_p_c_scenario_indirect)%>%
  mutate(burden_total_scenario         = burden_gas_scenario_direct + burden_gas_scenario_indirect + burden_coal_scenario + burden_p_c_scenario_direct + burden_p_c_scenario_indirect,
         burden_direct                 = burden_gas_scenario_direct + burden_coal_scenario_direct + burden_p_c_scenario_direct,
         burden_indirect               = burden_gas_scenario_indirect + burden_coal_scenario_indirect + burden_p_c_scenario_indirect,
         share_burden_direct           = burden_direct/(burden_direct + burden_indirect),
         burden_direct_total           = burden_direct*hh_weights,
         burden_indirect_total         = burden_indirect*hh_weights)

# 8.1   Figures ####

# Figure 1a

data_8.1.0 <- data_8 %>%
  mutate(hh_weights = ifelse(Country == "Czech Republic", hh_weights*129.1408, hh_weights))%>%
  summarise(
    y5   = wtd.quantile(burden_total_scenario, weights = hh_weights, probs = 0.05),
    y25  = wtd.quantile(burden_total_scenario, weights = hh_weights, probs = 0.25),
    y50  = wtd.quantile(burden_total_scenario, weights = hh_weights, probs = 0.5),
    y75  = wtd.quantile(burden_total_scenario, weights = hh_weights, probs = 0.75),
    y95  = wtd.quantile(burden_total_scenario, weights = hh_weights, probs = 0.95),
    mean = wtd.mean(   burden_total_scenario, weights = hh_weights),
    mean_coal = wtd.mean(burden_coal_scenario, weights = hh_weights),
    mean_p_c  = wtd.mean(burden_p_c_scenario,   weights = hh_weights),
    mean_gas  = wtd.mean(burden_gas_scenario,   weights = hh_weights))%>%
  mutate(Country = "Europe (Average)")

data_8.1 <- data_8 %>%
  group_by(Country)%>%
  summarise(
    y5   = wtd.quantile(burden_total_scenario, weights = hh_weights, probs = 0.05),
    y25  = wtd.quantile(burden_total_scenario, weights = hh_weights, probs = 0.25),
    y50  = wtd.quantile(burden_total_scenario, weights = hh_weights, probs = 0.5),
    y75  = wtd.quantile(burden_total_scenario, weights = hh_weights, probs = 0.75),
    y95  = wtd.quantile(burden_total_scenario, weights = hh_weights, probs = 0.95),
    mean = wtd.mean(   burden_total_scenario, weights = hh_weights),
    mean_coal = wtd.mean(burden_coal_scenario, weights = hh_weights),
    mean_p_c  = wtd.mean(burden_p_c_scenario,   weights = hh_weights),
    mean_gas  = wtd.mean(burden_gas_scenario,   weights = hh_weights))%>%
  ungroup()%>%
  bind_rows(data_8.1.0)%>%
  mutate(Country.Type = ifelse(Country == "Europe (Average)",1,0))

write.xlsx(select(data_8.1, Country, mean, y50), "C:/Users/misl/ownCloud/EU_Gasprice_Analysis/Supplementary Information/Mean_Median_Baseline.xlsx")

data_8.1.3.X <- data_8.1 %>%
  select(Country, y50)%>%
  arrange(desc(y50))%>%
  mutate(order_no = 1:n())%>%
  select(Country, order_no)%>%
  mutate(group_no = c(rep(1,5), rep(2,2),0,rep(2,3), rep(3,5), rep(4,5), rep(5,4)))%>%
  mutate(group_max = c(rep(0.5,20),rep(0.5,5)))

data_8.1.1 <- data_8.1 %>%
  pivot_longer(starts_with("mean"), names_to = "Type", values_to = "Mean")%>%
  #mutate_at(vars(y5:y95), ~ ifelse(Type == "mean", ., NA))%>%
  select(Country, Type, Mean)%>%
  mutate(Type_A = ifelse(Type == "mean", "All fuels",
                         ifelse(Type == "mean_coal", "Coal",
                                ifelse(Type == "mean_p_c", "Oil",
                                       ifelse(Type == "mean_gas", "Gas", NA)))))%>%
  mutate(Country.Type = ifelse(Country == "Europe (Average)",1,0))

data_8.1.1$Type_A <- factor(data_8.1.1$Type_A, levels = c("All fuels",
                                                              "Gas",
                                                              "Oil",
                                                              "Coal"))

plot_8.1.1 <- ggplot()+
  geom_boxplot(data = data_8.1, aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95, x = reorder(Country, desc(y50)), alpha = factor(Country.Type)), fill = "lightgrey", stat = "identity", position = position_dodge(0), outlier.shape = NA, width = 0.5, size = 0.3) +
  geom_segment(aes(x = 1.01, xend = 0.99, y = 0.13116240, yend = 0.13116240, colour = "Median incidence, all fuels"), size = 0.8)+
  theme_bw()+
  xlab("")+ ylab("Additional Costs (in % of Total Expenditures)")+
  geom_point(data = data_8.1.1, aes(y = Mean, fill = Type_A, x = factor(Country), shape = Type_A), size = 1.7, stroke = 0.5)+
  #geom_point(aes(y = mean_coal), shape = 24, fill = "#BC3C29FF", colour = "black", size = 1.5, stroke = 0.2)+
  #geom_point(aes(y = mean_p_c),  shape = 22, fill = "#0072B5FF", colour = "black", size = 1.5, stroke = 0.2)+
  #geom_point(aes(y = mean_gas),  shape = 25, fill = "#20854EFF", colour = "black", size = 1.5, stroke = 0.2)+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0))+
  #scale_x_discrete(limits = rev)+
  #coord_flip(ylim = c(0,0.45))+
  coord_cartesian(ylim = c(0,0.85))+
  ggtitle("")+
  labs(fill = "Average incidence of fuel price increases", shape = "Average incidence of fuel price increases")+
  scale_fill_manual( name = "Average incidence of fuel price increases", values = c("white", "#20854EFF", "#0072B5FF", "#BC3C29FF"))+
  scale_shape_manual(name = "Average incidence of fuel price increases", values = c(21,22,23,24))+
  scale_colour_manual("", values = "black", breaks = "Median incidence, all fuels")+
  scale_alpha_manual(values = c(1,0))+
  #scale_fill_nejm()+
  guides(fill = guide_legend(nrow = 1, order = 1, override.aes = list(shape = c(21,22,23,24)), title.position = "top", title.hjust = 0.5), colour = guide_legend(order = 2),
         shape = "none", alpha = "none")+
  theme(axis.text.y = element_text(size = 7), 
        axis.text.x = element_text(size = 7, hjust = 0.95, vjust = 0.5, angle = 90),
        axis.title  = element_text(size = 7),
        plot.title = element_text(size = 7),
        legend.position = "bottom",
        strip.text = element_text(size = 7),
        strip.text.y = element_text(angle = 180),
        panel.grid.major = element_line(size = 0.3),
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(size = 0.2),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        legend.box = "vertical",
        plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"),
        panel.border = element_rect(size = 0.3))

jpeg("C:/Users/misl/ownCloud/EU_Gasprice_Analysis/Figures/2022_05_20_Figures/Figure_3.jpg", width = 15, height = 17, unit = "cm", res = 400)
print(plot_8.1.1)
dev.off()

# Figure 2a

data_8.2 <- data_8 %>%
  left_join(select(data_7.3.1, hh_id, Income_Group_10_hh_USD_PPP_pc))%>%
  group_by(Income_Group_10_hh_USD_PPP_pc)%>%
  summarise(
    y5  = wtd.quantile(burden_total_scenario, weights = hh_weights, probs = 0.05),
    y25 = wtd.quantile(burden_total_scenario, weights = hh_weights, probs = 0.25),
    y50 = wtd.quantile(burden_total_scenario, weights = hh_weights, probs = 0.5),
    y75 = wtd.quantile(burden_total_scenario, weights = hh_weights, probs = 0.75),
    y95 = wtd.quantile(burden_total_scenario, weights = hh_weights, probs = 0.95),
    mean = wtd.mean(   burden_total_scenario, weights = hh_weights),
    mean_coal = wtd.mean(burden_coal_scenario, weights = hh_weights),
    mean_p_c  = wtd.mean(burden_p_c_scenario,   weights = hh_weights),
    mean_gas  = wtd.mean(burden_gas_scenario,   weights = hh_weights))%>%
  ungroup()

data_8.2.1 <- data_8.2 %>%
  pivot_longer(starts_with("mean"), names_to = "Type", values_to = "Mean")%>%
  #mutate_at(vars(y5:y95), ~ ifelse(Type == "mean", ., NA))%>%
  select(Income_Group_10_hh_USD_PPP_pc, Type, Mean)%>%
  mutate(Type_A = ifelse(Type == "mean", "All fuels",
                         ifelse(Type == "mean_coal", "Coal",
                                ifelse(Type == "mean_p_c", "Oil",
                                       ifelse(Type == "mean_gas", "Gas", NA)))))

data_8.2.1$Type_A <- factor(data_8.2.1$Type_A, levels = c("All fuels",
                                                              "Gas",
                                                              "Oil",
                                                              "Coal"))


plot_8.2 <- ggplot()+
  geom_boxplot(data = data_8.2, aes(x = factor(Income_Group_10_hh_USD_PPP_pc), ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95), fill = "lightgrey", colour = "black", alpha = 1, stat = "identity", position = position_dodge(0.5), outlier.shape = NA, width = 0.5, size = 0.3) +
  geom_segment(aes(x = 1.01, xend = 0.99, y = 0.53116240, yend = 0.53116240, colour = "Median incidence, all fuels"), size = 0.8)+
  theme_bw()+
  xlab("European Expenditure Decile")+ ylab("Additional Costs (in % of Total Expenditures)")+
  geom_point(data = data_8.2.1, aes(y = Mean, fill = Type_A, x = factor(Income_Group_10_hh_USD_PPP_pc), shape = Type_A), size = 1.8, stroke = 0.5)+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0))+
  scale_x_discrete(labels = c("1 \n Poorest \n 10 Percent", seq(2,9,1), "10 \n Richest \n 10 Percent"))+
  coord_cartesian(ylim = c(0,0.60))+
  guides(fill = guide_legend(nrow = 1, order = 1, override.aes = list(shape = c(21,22,23,24)), title.position = "top", title.hjust = 0.5), colour = guide_legend(order = 2), shape = "none")+
  labs(fill = "Average incidence of fuel price increases", alpha = "")+
  scale_fill_manual(values = c("white", "#20854EFF", "#0072B5FF", "#BC3C29FF"))+
  scale_colour_manual("", values = "black", breaks = "Median incidence, all fuels")+
  scale_shape_manual(values = c(21,22,23,24))+
  #coord_cartesian(ylim = c(0,0.3))+
  ggtitle("")+
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
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        legend.box = "vertical",
        plot.margin = unit(c(0.2,0.2,0.2,0.2), "cm"),
        panel.border = element_rect(size = 0.3))

jpeg("C:/Users/misl/ownCloud/EU_Gasprice_Analysis/Figures/2022_05_20_Figures/Figure_4.jpg", width = 15, height = 14 , unit = "cm", res = 400)
print(plot_8.2)

dev.off()

# 8.1.1 Figure 7 ####

# Lump Sum Transfers per Household (average gas expenditures)

data_8.1.1.1 <- data_8 %>%
  mutate(Heating_Gas = ifelse(is.na(Heating_Gas),0,Heating_Gas),
         Heating_NA  = ifelse(is.na(Heating_NA), 0,Heating_NA),
         Heating_Oil = ifelse(is.na(Heating_Oil),0, Heating_Oil),
         Heating_District_Heat = ifelse(is.na(Heating_District_Heat),0, Heating_District_Heat))%>%
  mutate(exp_total_scenario = hh_expenditures_USD_2014*burden_total_scenario)%>%
  mutate(exp_add_total_scenario = hh_weights*exp_total_scenario)%>%
  group_by(Country)%>%
  summarise(exp_add_total_scenario = sum(exp_add_total_scenario),
            hh_weights  = sum(hh_weights))%>%
  ungroup()%>%
  mutate(compensation = exp_add_total_scenario/hh_weights)

# Average gas expenditures in lower two expenditure quintiles

data_8.1.1.2 <- left_join(indirect_expenditures_0, household_information_0)%>%
  left_join(carbon_pricing_incidence_0, by = "hh_id")%>%
  select(hh_id, hh_weights, Country, District_Heating_burden_GAS_gas, Gas_Heating_burden_GAS_gas,
         Heating_Gas, Heating_District_Heat, Income_Group_5, hh_expenditures_USD_2014)%>%
  left_join(Gas_prices_households)%>%
  mutate(District_Heating_burden_GAS_gas = District_Heating_burden_GAS_gas*Markup_Gas_Direct,
         Gas_Heating_burden_GAS_gas      = Gas_Heating_burden_GAS_gas*Markup_Gas_Direct)%>%
  mutate(District_Heating_exp            = District_Heating_burden_GAS_gas*hh_expenditures_USD_2014,
         Gas_Heating_exp                 = Gas_Heating_burden_GAS_gas*hh_expenditures_USD_2014)%>%
  filter(Income_Group_5 == 1 | Income_Group_5 == 2)%>%
  filter(!is.na(District_Heating_exp)|!is.na(Gas_Heating_exp))%>%
  group_by(Country)%>%
  summarise(mean_District_Heating_exp = wtd.mean(District_Heating_exp, hh_weights, na.rm = TRUE),
            mean_Gas_Heating_exp      = wtd.mean(Gas_Heating_exp, hh_weights, na.rm = TRUE))%>%
  ungroup()%>%
  mutate(mean_District_Heating_exp = ifelse(mean_District_Heating_exp == "NaN",0, mean_District_Heating_exp),
         mean_Gas_Heating_exp      = ifelse(mean_Gas_Heating_exp == "NaN",0, mean_Gas_Heating_exp))%>%
  mutate(bonus = mean_Gas_Heating_exp + mean_District_Heating_exp)

# Average additional gas expenditures

data_8.1.1.3 <- left_join(indirect_expenditures_0, household_information_0)%>%
  left_join(carbon_pricing_incidence_0, by = "hh_id")%>%
  select(hh_id, hh_weights, Country, Gas_Heating_burden_GAS_gas, District_Heating_burden_GAS_gas,
         hh_expenditures_USD_2014)%>%
  left_join(Gas_prices_households)%>%
  mutate(Gas_Heating_burden_GAS_gas      = Gas_Heating_burden_GAS_gas*Markup_Gas_Direct,
         District_Heating_burden_GAS_gas = District_Heating_burden_GAS_gas*Markup_Gas_Direct)%>%
  mutate(Gas_Heating_exp                 = Gas_Heating_burden_GAS_gas*hh_expenditures_USD_2014,
         District_Heating_exp            = District_Heating_burden_GAS_gas*hh_expenditures_USD_2014)%>%
  group_by(Country)%>%
  summarise(mean_Gas_Heating_exp         = wtd.mean(Gas_Heating_exp, hh_weights, na.rm = TRUE),
            mean_District_Heating_exp    = wtd.mean(District_Heating_exp, hh_weights, na.rm = TRUE))%>%
  ungroup()%>%
  mutate(mean_Gas_Heating_exp            = ifelse(mean_Gas_Heating_exp == "NaN", 0, mean_Gas_Heating_exp),
         mean_District_Heating_exp       = ifelse(mean_District_Heating_exp == "NaN", 0, mean_District_Heating_exp))%>%
  mutate(bonus_2                         = mean_Gas_Heating_exp + mean_District_Heating_exp)

data_8.1.1 <- data_8 %>%
  mutate(Heating_Gas           = ifelse(is.na(Heating_Gas),0,Heating_Gas),
         Heating_NA            = ifelse(is.na(Heating_NA), 0,Heating_NA),
         Heating_Oil           = ifelse(is.na(Heating_Oil),0, Heating_Oil),
         Heating_District_Heat = ifelse(is.na(Heating_District_Heat),0, Heating_District_Heat))%>%
  left_join(select(data_8.1.1.1, Country, compensation))%>%
  left_join(select(data_8.1.1.2, Country, mean_District_Heating_exp, mean_Gas_Heating_exp, bonus))%>%
  left_join(select(data_8.1.1.3, Country, bonus_2))%>%
  # Model compensation
  mutate(compensation_1 = compensation,
         compensation_2 = ifelse((Income_Group_5 == 1 | Income_Group_5 == 2) & Heating_Gas > 0 & Heating_District_Heat == 0, mean_Gas_Heating_exp,
                                 ifelse((Income_Group_5 == 1 | Income_Group_5 == 2) & Heating_Gas == 0 & Heating_District_Heat > 0, mean_District_Heating_exp,
                                        ifelse((Income_Group_5 == 1 | Income_Group_5 == 2) & Heating_Gas > 0 & Heating_District_Heat > 0, bonus, 0))),
         compensation_3 = ifelse(Heating_Gas > 0 | Heating_District_Heat > 0, bonus_2, 0))%>%
  select(hh_id, hh_size, hh_weights, Country, Income_Group_5,
         compensation_1, compensation_2, compensation_3,
         hh_expenditures_USD_2014, burden_total_scenario, Heating_Gas, Heating_District_Heat)%>%
  mutate(exp_total_scenario = burden_total_scenario*hh_expenditures_USD_2014)%>%
  mutate(compensation_0 = 0)%>%
  pivot_longer(c(compensation_1:compensation_3, compensation_0), names_to = "Type", values_to = "compensation")%>%
  mutate(Type_A = ifelse(Type == "compensation_1", "All households, all fuels",
                         ifelse(Type == "compensation_2", "Poor households, only gas", 
                                ifelse(Type == "compensation_0", "No Compensation", 
                                       ifelse(Type == "compensation_3", "All households, only gas", NA)))))%>%
  mutate(exp_total_compensation_scenario     = compensation - exp_total_scenario)%>%
  mutate(burden_total_compensation_scenario  = exp_total_compensation_scenario/hh_expenditures_USD_2014)%>%
  mutate(Type_B = "All Households")

data_8.1.1.0 <- data_8.1.1 %>%
  mutate(burden_total_compensation_scenario = ifelse(Heating_Gas > 0 | Heating_District_Heat > 0, burden_total_compensation_scenario, NA))%>%
  mutate(Type_B = "Gas Users Only")
  
data_8.1.1 <- data_8.1.1 %>%
  bind_rows(data_8.1.1.0)

data_8.1.2 <- data_8.1.1 %>%
  group_by(Country, Income_Group_5, Type_A, Type_B)%>%
  summarise(
    y5  = wtd.quantile(burden_total_compensation_scenario, weights = hh_weights, probs = 0.05),
    y25 = wtd.quantile(burden_total_compensation_scenario, weights = hh_weights, probs = 0.25),
    y50 = wtd.quantile(burden_total_compensation_scenario, weights = hh_weights, probs = 0.5),
    y75 = wtd.quantile(burden_total_compensation_scenario, weights = hh_weights, probs = 0.75),
    y95 = wtd.quantile(burden_total_compensation_scenario, weights = hh_weights, probs = 0.95),
    mean = wtd.mean(   burden_total_compensation_scenario, weights = hh_weights))%>%
  ungroup()%>%
  mutate(Country_Type = ifelse(Country %in% c("Hungary", "Italy", "Germany", "Romania", "Czech Republic"), "High Incidence Countries",
                               ifelse(Country %in% c("Netherlands", "Poland", "Croatia", "Slovak Republic", "Belgium"), "Relatively High Incidence Countries",
                                      ifelse(Country %in% c("Portugal", "Luxembourg", "Ireland", "Spain", "Latvia"), "Medium Incidence Countries",
                                             ifelse(Country %in% c("Lithuania", "Greece", "France", "Denmark"), "Relatively Low Incidence Countries",
                                                    ifelse(Country %in% c("Sweden", "Finland", "Estonia", "Cyprus", "Bulgaria"), "Low Incidence Countries", NA))))))


data_8.1.2$Type_A <- factor(data_8.1.2$Type_A, levels = c("No Compensation", 
                                                          "All households, all fuels",
                                                          "All households, only gas",
                                                          "Poor households, only gas"))

data_8.1.2.X <- data_8.1.2 %>%
  filter(Country == "Hungary" | Country == "Belgium" | Country == "Denmark")

data_8.1.2.X$Country <- factor(data_8.1.2.X$Country, levels = c("Hungary", "Belgium", "Denmark"))

plot_8.1.2 <- ggplot(data_8.1.2.X, aes(x = factor(Income_Group_5)))+
  geom_hline(aes(yintercept = 0), size = 0.5)+
  geom_line(aes(y = mean, group = interaction(Country, Type_A), colour = factor(Type_A)))+
  geom_point(aes(y = mean, fill = factor(Type_A)), shape = 21, colour = "black")+
  theme_bw()+
  facet_grid(Type_B ~ Country)+
  xlab("Expenditure Quintile")+ ylab("Average Budget Change (in % of Total Expenditures)")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0), breaks = seq(-0.3,0.2,0.1))+
  coord_cartesian(ylim = c(round(min(data_8.1.2.X$mean-0.02),2),
                           round(max(data_8.1.2.X$mean+0.02),2)))+
  scale_fill_nejm()+
  scale_colour_nejm()+
  ggtitle("Distributional Effects of Different Compensation Scenarios")+
  labs(fill = "")+
  guides(colour = "none", fill = guide_legend(nrow = 1))+
  theme(axis.text.y = element_text(size = 7), 
        axis.text.x = element_text(size = 7),
        axis.title  = element_text(size = 7),
        plot.title = element_text(size = 7),
        legend.position = "bottom",
        strip.text = element_text(size = 7),
        strip.text.y = element_text(angle = -90),
        panel.grid.major = element_line(size = 0.3),
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(size = 0.2),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.1,0.1,0,0), "cm"),
        panel.border = element_rect(size = 0.3))

# jpeg("C:/Users/misl/ownCloud/EU_Gasprice_Analysis/Figures/2022_05_06_Figures/Figure_5.jpg", width = 15, height = 12 , unit = "cm", res = 400)
# print(plot_8.1.2)
# dev.off()
# 
# 
plot_7_function <- function(Type_0){
  plot_7 <- ggplot(filter(data_8.1.2, Country_Type == Type_0 & Type_B == "All Households"), aes(x = factor(Income_Group_5)))+
    geom_hline(aes(yintercept = 0), size = 0.5)+
    geom_line(aes(y = mean, group = interaction(Country, Type_A), colour = factor(Type_A)))+
    geom_point(aes(y = mean, fill = factor(Type_A)), shape = 21, colour = "black")+
    theme_bw()+
    facet_wrap(. ~ Country, nrow = 1)+
    xlab("Expenditure Quintile")+ ylab("Average Budget Change (in % of Total Expenditures)")+
    scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0))+
    coord_cartesian(ylim = c(round(min(filter(data_8.1.2, Country_Type == Type_0)$mean-0.02),2),
                             round(max(filter(data_8.1.2, Country_Type == Type_0)$mean+0.02),2)))+
    scale_fill_nejm()+
    scale_colour_nejm()+
    ggtitle(paste0("Compensation Scenarios - ", Type_0))+
    labs(fill = "")+
    guides(colour = "none", fill = guide_legend(nrow = 2))+
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
          legend.text = element_text(size = 7),
          legend.title = element_text(size = 7),
          plot.margin = unit(c(0.1,0.1,0,0), "cm"),
          panel.border = element_rect(size = 0.3))
  return(plot_7)
}
# 
# plot_7.7.1 <- plot_7_function("High Incidence Countries")
# plot_7.7.2 <-plot_7_function("Relatively High Incidence Countries")
# plot_7.7.3 <-plot_7_function("Medium Incidence Countries")
# plot_7.7.4 <-plot_7_function("Relatively Low Incidence Countries")
# plot_7.7.5 <-plot_7_function("Low Incidence Countries")
# 
# 
# jpeg("C:/Users/misl/ownCloud/EU_Gasprice_Analysis/Figures/2022_05_06_Figures/Figure_4_%d.jpg", width = 15, height = 8 , unit = "cm", res = 400)
# print(plot_7.7.1)
# print(plot_7.7.2)
# print(plot_7.7.3)
# print(plot_7.7.4)
# print(plot_7.7.5)
# dev.off()

data_8.1.2.0 <- left_join(data_8.1.2, data_8.1.3.X)
  
data_8.1.2.0$Country <- fct_reorder(data_8.1.2.0$Country, data_8.1.2.0$order_no)


plot_7 <- ggplot(filter(data_8.1.2.0, Type_B == "All Households"), aes(x = factor(Income_Group_5)))+
  geom_hline(aes(yintercept = 0), size = 0.5)+
  geom_line(aes(y = mean, group = interaction(Country, Type_A), colour = factor(Type_A)))+
  geom_point(aes(y = mean, fill = factor(Type_A), shape = factor(Type_A)), colour = "black")+
  theme_bw()+
  facet_wrap(. ~ Country, nrow = 5)+
  xlab("Expenditure Quintile")+ ylab("Average Budget Change (in % of Total Expenditures)")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0))+
  coord_cartesian(ylim = c(round(min(filter(data_8.1.2.0, Type_B == "All Households")$mean-0.02),2),
                           round(max(filter(data_8.1.2.0, Type_B == "All Households")$mean+0.02),2)))+
  scale_fill_nejm(breaks = c("No Compensation", "Poor households, only gas",
                             "All households, only gas", "All households, all fuels"))+
  scale_colour_nejm(breaks = c("No Compensation", "Poor households, only gas",
                               "All households, only gas", "All households, all fuels"))+
  scale_shape_manual(values = c(21,22,23,24), breaks = c("No Compensation", "Poor households, only gas",
                                                         "All households, only gas", "All households, all fuels"))+
  ggtitle("Compensation Schemes - Baseline Scenario")+
  labs(fill = "", shape = "")+
  guides(colour = "none", fill = guide_legend(nrow = 1))+
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
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.1,0.1,0,0), "cm"),
        panel.border = element_rect(size = 0.3))

jpeg("C:/Users/misl/ownCloud/EU_Gasprice_Analysis/Figures/2022_05_20_Figures/Figure_7.jpg", width = 15, height = 20, unit = "cm", res = 400)
print(plot_7)

dev.off()

plot_7.1 <- ggplot(filter(data_8.1.2.0, Type_B == "Gas Users Only"), aes(x = factor(Income_Group_5)))+
  geom_hline(aes(yintercept = 0), size = 0.5)+
  geom_line(aes(y = mean, group = interaction(Country, Type_A), colour = factor(Type_A)))+
  geom_point(aes(y = mean, fill = factor(Type_A), shape = factor(Type_A)), colour = "black")+
  theme_bw()+
  facet_wrap(. ~ Country, nrow = 5)+
  xlab("Expenditure Quintile")+ ylab("Average Budget Change (in % of Total Expenditures)")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0))+
  coord_cartesian(ylim = c(round(min(filter(data_8.1.2.0, Type_B == "Gas Users Only")$mean-0.02),2),
                           round(max(filter(data_8.1.2.0, Type_B == "Gas Users Only")$mean+0.02),2)))+
  scale_fill_nejm(breaks = c("No Compensation", "Poor households, only gas",
                             "All households, only gas", "All households, all fuels"))+
  scale_colour_nejm(breaks = c("No Compensation", "Poor households, only gas",
                               "All households, only gas", "All households, all fuels"))+
  scale_shape_manual(values = c(21,22,23,24), breaks = c("No Compensation", "Poor households, only gas",
                                                         "All households, only gas", "All households, all fuels"))+
  ggtitle("Compensation Schemes - Baseline Scenario")+
  labs(fill = "", shape = "")+
  guides(colour = "none", fill = guide_legend(nrow = 1))+
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
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.1,0.1,0,0), "cm"),
        panel.border = element_rect(size = 0.3))

jpeg("C:/Users/misl/ownCloud/EU_Gasprice_Analysis/Figures/2022_05_20_Figures/Figure_A7.jpg", width = 15, height = 20, unit = "cm", res = 400)
print(plot_7.1)

dev.off()

# 8.1.2 Figure 8 ####

# For Czech Republic

czech_weights <- select(filter(household_information_0, Country == "Czech Republic"), Country, hh_weights)
  

czech_factor <- 4347840/sum(czech_weights$hh_weights) # Source: CEICdata

data_8.1.2.1 <- data_8.1.1 %>%
  mutate(hh_weights = ifelse(Country == "Czech Republic", hh_weights*czech_factor, hh_weights))%>%
  mutate(compensation_weighted = compensation*hh_weights)%>%
  mutate(compensation_weighted_EUR = compensation_weighted/1.329)%>%
  filter(Type_B == "All Households")%>%
  group_by(Country, Type_A)%>%
  summarise(compensation_weighted = sum(compensation_weighted),
            compensation_weighted_EUR = sum(compensation_weighted_EUR),
            hhs = sum(hh_weights))%>%
  ungroup()%>%
  mutate(Country = ifelse(Country == "Slovak Republic", "Slovakia", Country))

GDP_0 <- read.xlsx("C:/Users/misl/ownCloud/EU_Gasprice_Analysis/Supplementary Information/Compensation_Requirement.xlsx", colNames = TRUE, startRow = 2)%>%
  mutate(Country = ifelse(Country == "Slovak Republic", "Slovakia", Country))%>%
  select(Country, GDP)

data_8.1.2.2 <- left_join(data_8.1.2.1, GDP_0)%>%
  mutate(share_GDP = compensation_weighted/GDP)

world <- map_data("world")

map_data <- map_data("world")%>%
  filter(region %in% c("Belgium", "Bulgaria", "Cyprus", "Czech Republic", "Germany", "Denmark", "Estonia", "Greece", "Spain", "Finland",
                       "France", "Croatia", "Hungary" ,"Ireland", "Italy", "Lithuania", "Luxembourg", "Latvia", "Netherlands", "Poland",
                       "Portugal", "Romania", "Sweden", "Slovakia", "Switzerland", "Austria",
                       "UK", "Norway", "Slovenia", "Serbia", "Kosovo", "Albania", "Bosnia and Herzegovina", 
                       "North Macedonia", "Montenegro", "Ukraine", "Moldova", "Turkey", "Russia", "Belarus"))%>%
  left_join(data_8.1.2.2, by = c("region" = "Country"))

map_data_1 <- expand_grid(select(filter(map_data, is.na(Type_A)), - Type_A), Type_A = c("Poor households, only gas",
                                                                                        "All households, only gas",
                                                                                        "All households, all fuels"))%>%
  bind_rows(filter(map_data, !is.na(Type_A)))%>%
  filter(Type_A != "No Compensation")

map_data_1$Type_A <- factor(map_data_1$Type_A, levels = c("Poor households, only gas",
                                                          "All households, only gas",
                                                          "All households, all fuels"))

plot_8.1.2 <- ggplot()+
  geom_map(data = map_data_1, map = world, aes(map_id = region), fill = "lightgrey")+
  geom_map(data = map_data_1, map = world,
           aes(long, lat, map_id = region, fill = share_GDP), colour = "black", size = 0.2)+
  theme_bw()+
  facet_wrap(. ~ Type_A)+
  coord_map(ylim = c(35,70), xlim = c(-10,35))+
  theme(axis.text.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.title  = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(size = 9),
        legend.position = "bottom",
        strip.text = element_text(size = 8),
        strip.text.y = element_text(angle = 180),
        panel.grid.major = element_line(size = 0.3),
        panel.grid.minor = element_blank(),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"),
        panel.border = element_rect(size = 0.3))+
  scale_fill_distiller(palette = "Spectral", na.value = "lightgrey", labels = percent_format(accuracy = 1), limits = c(0,0.09), breaks = seq(0,0.1,0.01))+
  labs(fill = "Compensation costs (in % of GDP)")+
  ggtitle("Compensation Schemes in Baseline Scenario")+
  guides(colour = "none", fill = guide_colourbar(barwidth = 15, barheight = 1, ticks.colour = "black"))


jpeg("C:/Users/misl/ownCloud/EU_Gasprice_Analysis/Figures/2022_05_20_Figures/Figure_8.jpg", width = 15, height = 10 , unit = "cm", res = 400)
#print(plot_7.7.3.1)
print(plot_8.1.2)
dev.off()

# Corresponding Table for Horizontal Inequality

data_8.1.table <- data_8.1.1 %>%
  mutate(more_affected_10 = ifelse(burden_total_compensation_scenario < -0.1,  hh_weights,0),
         more_affected_25 = ifelse(burden_total_compensation_scenario < -0.25, hh_weights, 0))%>%
  group_by(Country, Income_Group_5, Type_A)%>%
  mutate(sum_hh_weights = sum(hh_weights, na.rm = TRUE))%>%
  summarise(hh_weights_ma10 = sum(more_affected_10, na.rm = TRUE),
            hh_weights_ma25 = sum(more_affected_25, na.rm = TRUE),
            sum_hh_weights  = mean(sum_hh_weights))%>%
  ungroup()%>%
  mutate(share_ma10  = hh_weights_ma10/sum_hh_weights,
         share_ma25  = hh_weights_ma25/sum_hh_weights)%>%
  pivot_longer(share_ma10:share_ma25, names_to = "Type_B", values_to = "share")%>%
  unite(Type_C, c("Type_A", "Income_Group_5"), sep = " ")%>%
  select(Country, Type_C, Type_B, share)%>%
  arrange(Country, Type_C)%>%
  pivot_wider(names_from = "Type_C", values_from = "share")%>%
  mutate(Type_B = rep(c("10%","25%"),24))%>%
  select(Country, Type_B,
         starts_with("No"),
         starts_with("Poor"),
         starts_with("All households, only"),
         starts_with("All households"))

# write.xlsx(data_8.1.table, "C:/Users/misl/ownCloud/EU_Gasprice_Analysis/Figures/2022_05_20_Figures/Table_Appendix_Figure_7.xlsx")

# 8.1.3 Figure 3 ####

Oil_prices_2 <- Oil_prices_0 %>%
  select(Country, Markup, Type)%>%
  rename(Markup_Fuels = Markup)%>%
  mutate(Type = ifelse(Type == "LPG_0", "LPG",
                       ifelse(Type == "Heating_0", "Oil Heating", Type)))

Type_df <- distinct(pivot_longer(indirect_expenditures_0, Diesel_burden_GAS_gas:Other_burden_P_C_p_c_indirect, names_to = "Type", values_to = "burden"), Type)%>%
  mutate(Type_A = ifelse(grepl("COAL", Type),"Coal",
                         ifelse(grepl("GAS", Type), "Gas",
                                ifelse(grepl("P_C", Type), "Liquid Fuels", NA))))%>%
  mutate(Type_C = ifelse(grepl("indirect", Type), "Indirect",
                         ifelse(grepl("direct", Type), "Direct", "Total")))%>%
  mutate(Type_D = ifelse(grepl("Diesel", Type), "Diesel",
                         ifelse(grepl("Gasoline", Type), "Gasoline",
                                ifelse(grepl("LPG", Type), "LPG",
                                       ifelse(grepl("Oil_Heating", Type), "Oil Heating",
                                              ifelse(grepl("Other Transport Fuels", Type), "Other Transport Fuels", NA))))))%>%
  mutate(Type_E = ifelse(grepl("District_Heating", Type), "District heating",
                         ifelse(grepl("Electricity", Type), "Electricity",
                                ifelse(grepl("Food", Type), "Food",
                                       ifelse(grepl("Gas_Heating", Type), "Gas heating",
                                              ifelse(grepl("Goods", Type), "Goods, services, other energy",
                                                     ifelse(grepl("Oil_Heating", Type), "Oil heating",
                                                            ifelse(grepl("Other Energy", Type),"Goods, services, other energy",
                                                                   ifelse(grepl("LPG", Type), "Goods, services, other energy",
                                                                          ifelse(grepl("Services", Type), "Goods, services, other energy",
                                                                                 ifelse(grepl("Diesel", Type), "Transport fuels", 
                                                                                        ifelse(grepl("Gasoline", Type), "Transport fuels",
                                                                                               ifelse(grepl("Other Transport Fuels", Type), "Transport fuels", "Goods, services, other energy")))))))))))))

data_8.1.3.1 <- left_join(indirect_expenditures_0, household_information_0)%>%
  left_join(select(carbon_pricing_incidence_0, hh_id, Income_Group_5))%>%
  select(hh_id, hh_weights, Income_Group_5, Diesel_burden_GAS_gas:Other_burden_P_C_p_c_indirect, Country)%>%
  pivot_longer(Diesel_burden_GAS_gas:Other_burden_P_C_p_c_indirect, names_to = "Type", values_to = "burden")%>%
  mutate(burden = ifelse(is.na(burden),0,burden))%>%
  left_join(Gas_prices_households)%>%
  left_join(Gas_prices_industry)%>%
  left_join(Oil_prices_1)%>%
  mutate(coal_markup = 1.5)%>%
  left_join(Type_df, by = "Type")%>%
  filter(Type_C != "Total" | Type_A == "Coal")%>%
  left_join(Oil_prices_2, by = c("Type_D" = "Type", "Country"))%>%
  mutate(Markup = ifelse(Type_A == "Coal", coal_markup,
                         ifelse(Type_A == "Gas" & Type_C == "Direct", Markup_Gas_Direct,
                                ifelse(Type_A == "Gas" & Type_C == "Indirect", Markup_Gas_Indirect,
                                       ifelse(Type_A == "Liquid Fuels" & Type_C == "Indirect", average_oil, 
                                              ifelse(Type_A == "Liquid Fuels" & Type_D == "Diesel" & Type_C == "Direct", Markup_Fuels,
                                                     ifelse(Type_A == "Liquid Fuels" & Type_D == "Gasoline" & Type_C == "Direct", Markup_Fuels,
                                                            ifelse(Type_A == "Liquid Fuels" & Type_D == "LPG" & Type_C == "Direct", Markup_Fuels,
                                                                   ifelse(Type_A == "Liquid Fuels" & Type_D == "Oil Heating" & Type_C == "Direct", Markup_Fuels,
                                                                          ifelse(Type_A == "Liquid Fuels" & Type_D == "Other Transport Fuels" & Type_C == "Direct", Markup_Fuels, 0))))))))))%>%
  mutate(Markup = ifelse(is.na(Markup),0, Markup))%>%
  mutate(burden_scenario = burden*Markup)%>%
  select(hh_id, hh_weights, Income_Group_5, Country, Type, Type_A, Type_C, Type_E, burden_scenario)%>%
  group_by(hh_id, Type_E)%>%
  mutate(burden_scenario_total = sum(burden_scenario))%>%
  ungroup()%>%
  select(-Type_A, - burden_scenario, - Type, - Type_C)%>%
  distinct()

data_8.1.3.X <- data_8.1 %>%
  select(Country, y50)%>%
  arrange(desc(y50))%>%
  mutate(order_no = 1:n())%>%
  select(Country, order_no)%>%
  mutate(group_no = c(rep(1,5), rep(2,5), rep(3,5), rep(4,5), rep(5,4)))%>%
  mutate(group_max = c(rep(0.5,20),rep(0.5,4)))

data_8.1.3.2 <- data_8.1.3.1 %>%
  group_by(Country, Income_Group_5, Type_E)%>%
  summarise(mean_burden_scenario_total = wtd.mean(burden_scenario_total, weights = hh_weights))%>%
  ungroup()%>%
  group_by(Country, Income_Group_5)%>%
  mutate(mean_agg = sum(mean_burden_scenario_total))%>%
  ungroup()%>%
  mutate(Country_Type = ifelse(Country %in% c("Hungary", "Italy", "Germany", "Romania", "Czech Republic"), "High Incidence Countries",
                               ifelse(Country %in% c("Netherlands", "Poland", "Croatia", "Slovak Republic", "Belgium"), "Relatively High Incidence Countries",
                                      ifelse(Country %in% c("Portugal", "Luxembourg", "Ireland", "Spain", "Latvia"), "Medium Incidence Countries",
                                             ifelse(Country %in% c("Lithuania", "Greece", "France", "Denmark"), "Relatively Low Incidence Countries",
                                                    ifelse(Country %in% c("Sweden", "Finland", "Estonia", "Cyprus", "Bulgaria"), "Low Incidence Countries", NA))))))%>%
  left_join(data_8.1.3.X)%>%
  group_by(group_no)%>%
  mutate(upper_limit = max(mean_agg))%>%
  ungroup()

data_8.1.3.2$Country <- fct_reorder(data_8.1.3.2$Country, data_8.1.3.2$order_no)

data_8.1.3.3 <- data_8.1.3.1%>%
  group_by(hh_id)%>%
  summarise(burden_scenario_total = sum(burden_scenario_total),
            Income_Group_5 = first(Income_Group_5),
            hh_weights = first(hh_weights),
            Country = first(Country))%>%
  ungroup()%>%
  left_join(select(carbon_pricing_incidence_0, hh_id, Heating_Gas))%>%
  mutate(burden_scenario_Gas_Users = ifelse(Heating_Gas > 0, burden_scenario_total, NA),
         Gas_Users        = ifelse(Heating_Gas > 0 & !is.na(Heating_Gas), hh_weights, 0))%>%
  group_by(Country, Income_Group_5)%>%
  summarise(mean_burden_scenario_Gas_Users = wtd.mean(burden_scenario_Gas_Users, hh_weights, na.rm = TRUE),
            Gas_Users             = sum(Gas_Users),
            sum_hh_weights        = sum(hh_weights))%>%
  ungroup()%>%
  mutate(share_Gas_Users = Gas_Users/sum_hh_weights)%>%
  mutate(mean_burden_scenario_Gas_Users = ifelse(mean_burden_scenario_Gas_Users == "NaN",NA,mean_burden_scenario_Gas_Users))%>%
  select(Country, Income_Group_5, mean_burden_scenario_Gas_Users, share_Gas_Users)%>%
  mutate(Country_Type = ifelse(Country %in% c("Hungary", "Italy", "Germany", "Romania", "Czech Republic"), "High Incidence Countries",
                               ifelse(Country %in% c("Netherlands", "Poland", "Croatia", "Slovak Republic", "Belgium"), "Relatively High Incidence Countries",
                                      ifelse(Country %in% c("Portugal", "Luxembourg", "Ireland", "Spain", "Latvia"), "Medium Incidence Countries",
                                             ifelse(Country %in% c("Lithuania", "Greece", "France", "Denmark"), "Relatively Low Incidence Countries",
                                                    ifelse(Country %in% c("Sweden", "Finland", "Estonia", "Cyprus", "Bulgaria"), "Low Incidence Countries", NA))))))%>%
  mutate(label_0 = paste0(round(share_Gas_Users,2)*100, "%"))%>%
  left_join(data_8.1.3.X)%>%
  group_by(group_no)%>%
  mutate(upper_limit = max(mean_burden_scenario_Gas_Users, na.rm = TRUE))%>%
  ungroup()%>%
  mutate(upper_limit_point = round(upper_limit+0.04,2))%>%
  mutate(mean_burden_scenario_Gas_Users = ifelse(mean_burden_scenario_Gas_Users > 0.5,0.47, mean_burden_scenario_Gas_Users))%>%
  mutate(label_0 = ifelse(Country == "Germany", "*", label_0),
         mean_burden_scenario_Gas_Users = ifelse(Country == "Germany", NA, mean_burden_scenario_Gas_Users))

data_8.1.3.3$Country <- fct_reorder(data_8.1.3.3$Country, data_8.1.3.3$order_no)
data_8.1.3.2$Type_E  <- factor(data_8.1.3.2$Type_E, levels = c("Goods, services, other energy",
                                                               "Food",
                                                               "Electricity",
                                                              "District heating",
                                                               "Gas heating",
                                                               "Oil heating",
                                                               "Transport fuels"))

library(lemon)

plot_8.1.3 <- ggplot()+
  geom_col(data = data_8.1.3.2,
           aes(x = factor(Income_Group_5), y = mean_burden_scenario_total, fill = Type_E, alpha = Type_E), 
           width = 0.5, position = "stack", colour = "black", size = 0.2)+
  geom_point(data = data_8.1.3.3,
             aes(x = factor(Income_Group_5), y = mean_burden_scenario_Gas_Users, shape = "Average incidence for households using gas heating"),
             colour = "black", size = 1.8, fill = "#20854EFF")+
  geom_text(data = data_8.1.3.3,
            aes(x = factor(Income_Group_5), 
                y = round(group_max,2), 
                label = label_0),
            size = 1.5)+
  geom_point(data = data_8.1.3.3,
             aes(x = factor(Income_Group_5), y = group_max), alpha = 0)+
  theme_bw()+
  facet_wrap(. ~ Country)+
  xlab("Expenditure Quintile")+ ylab("Average Additional Costs (in % of Total Expenditures)")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0))+
  coord_cartesian(ylim = c(0, max(round(max(data_8.1.3.3$mean_burden_scenario_Gas_Users, na.rm = TRUE)+0.05,2),
                                round(max(data_8.1.3.2$mean_agg, na.rm = TRUE)+0.05,2))))+
  #coord_cartesian(ylim = c(0,0.3))+
  labs(fill = "", shape = "", alpha = "")+
  scale_shape_manual(name = "", values = c(22))+
  scale_fill_manual(values = c("#E18727FF", "#FFDC91FF", "#EE4C97FF",
                               "#20854EFF", "#20854EFF",
                               "#0072B5FF", "#0072B5FF"))+
  scale_alpha_manual(values = c(1,1,1,0.5,1,0.5,1))+
  guides(shape = guide_legend(override.aes = list(fill = "#20854EFF", colour = "black"), order = 2),
         fill  = guide_legend(order = 1),
         alpha = guide_legend(order = 1))+
  ggtitle("Decomposition - Gas, Coal and Oil Price increase (Baseline Scenario)")+
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
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        legend.box = "vertical",
        plot.margin = unit(c(0.1,0.1,0,0), "cm"),
        panel.border = element_rect(size = 0.3))

jpeg("C:/Users/misl/ownCloud/EU_Gasprice_Analysis/Figures/2022_05_20_Figures/Figure_6.jpg", width = 15, height = 22, unit = "cm", res = 400)
print(plot_8.1.3)
dev.off()

plot_function <- function(Type_0){
  
  plot_7.6.X <- ggplot()+
    geom_col(data = filter(data_8.1.3.2, Country_Type == Type_0),
             aes(x = factor(Income_Group_5), y = mean_burden_scenario_total, fill = Type_E), 
             width = 0.5, position = "stack", colour = "black", size = 0.2)+
    geom_point(data = filter(data_8.1.3.3, Country_Type == Type_0),
               aes(x = factor(Income_Group_5), y = mean_burden_scenario_Gas_Users, shape = "Average Incidence for Gas Users"),
               colour = "black", size = 1.8, fill = "#20854EFF")+
    geom_text(data = filter(data_8.1.3.3, Country_Type == Type_0),
              aes(x = factor(Income_Group_5), 
                  y = max(round(max(filter(data_8.1.3.3, Country_Type == Type_0)$mean_burden_scenario_Gas_Users, na.rm = TRUE)+0.02,2),
                          round(max(filter(data_8.1.3.2, Country_Type == Type_0)$mean_agg, na.rm = TRUE)+0.02,2)), 
                  label = label_0),
              size = 1.5)+
    theme_bw()+
    facet_wrap(. ~ Country, nrow = 1)+
    xlab("Expenditure Quintile")+ ylab("Average Additional Costs (in % of Total Expenditures)")+
    scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0))+
    coord_cartesian(ylim = c(0, max(round(max(filter(data_8.1.3.3, Country_Type == Type_0)$mean_burden_scenario_Gas_Users, na.rm = TRUE)+0.03,2),
                                    round(max(filter(data_8.1.3.2, Country_Type == Type_0)$mean_agg, na.rm = TRUE)+0.03,2))))+
    #coord_cartesian(ylim = c(0,0.3))+
    labs(fill = "", shape = "")+
    scale_shape_manual(name = "", values = c(22))+
    guides(shape = guide_legend(override.aes = list(fill = "#20854EFF", colour = "black"), order = 2),
           fill  = guide_legend(order = 1))+
    ggtitle(paste0("Decomposition - Gas, Coal and Oil Price increase ", Type_0))+
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
          legend.text = element_text(size = 7),
          legend.title = element_text(size = 7),
          legend.box = "vertical",
          plot.margin = unit(c(0.1,0.1,0,0), "cm"),
          panel.border = element_rect(size = 0.3))
  
}

plot_7.6.1.1 <- plot_function("High Incidence Countries")
plot_7.6.1.2 <- plot_function("Relatively High Incidence Countries")
plot_7.6.1.3 <- plot_function("Medium Incidence Countries")
plot_7.6.1.4 <- plot_function("Relatively Low Incidence Countries")
plot_7.6.1.5 <- plot_function("Low Incidence Countries")


jpeg("C:/Users/misl/ownCloud/EU_Gasprice_Analysis/Figures/2022_05_06_Figures/Figure_4_new_%d.jpg", width = 15, height = 10, unit = "cm", res = 400)
print(plot_7.6.1.1)
print(plot_7.6.1.2)
print(plot_7.6.1.3)
print(plot_7.6.1.4)
print(plot_7.6.1.5)
dev.off()



# 8.1.4 Figure Aggregate Distributional Effects: Vertical / Horizontal / Hardship ####

data_8.1.4.1 <- data_8 %>%
  group_by(Country)%>%
  summarise(
    y5   = wtd.quantile(burden_total_scenario, weights = hh_weights, probs = 0.05),
    y25  = wtd.quantile(burden_total_scenario, weights = hh_weights, probs = 0.25),
    y50  = wtd.quantile(burden_total_scenario, weights = hh_weights, probs = 0.5),
    y75  = wtd.quantile(burden_total_scenario, weights = hh_weights, probs = 0.75),
    y95  = wtd.quantile(burden_total_scenario, weights = hh_weights, probs = 0.95),
    mean = wtd.mean(   burden_total_scenario, weights = hh_weights))%>%
  ungroup()

data_8.1.4.2 <- data_8 %>%
  filter(Income_Group_5 == 1 | Income_Group_5 == 2)%>%
  mutate(more_than_25 = ifelse(burden_total_scenario > 0.25, hh_weights,0))%>%
  group_by(Country)%>%
  summarise(more_than_25 = sum(more_than_25),
            hh_weights   = sum(hh_weights))%>%
  ungroup()%>%
  mutate(share_more_than_25 = more_than_25/hh_weights)

data_8.1.4.3 <- data_8.1.1 %>%
  filter(Type_A == "Poor households, only gas")%>%
  filter(Type_B == "All Households")%>%
  mutate(burden_total_compensation_scenario = burden_total_compensation_scenario*-1)

data_8.1.4.4 <- data_8.1.4.3 %>%
  group_by(Country)%>%
  summarise(
    y5_c   = wtd.quantile(burden_total_compensation_scenario, weights = hh_weights, probs = 0.05),
    y25_c  = wtd.quantile(burden_total_compensation_scenario, weights = hh_weights, probs = 0.25),
    y50_c  = wtd.quantile(burden_total_compensation_scenario, weights = hh_weights, probs = 0.5),
    y75_c  = wtd.quantile(burden_total_compensation_scenario, weights = hh_weights, probs = 0.75),
    y95_c  = wtd.quantile(burden_total_compensation_scenario, weights = hh_weights, probs = 0.95),
    mean_c = wtd.mean(   burden_total_compensation_scenario, weights = hh_weights))%>%
  ungroup()

data_8.1.4.5 <- data_8.1.4.3 %>%
  filter(Income_Group_5 == 1 | Income_Group_5 == 2)%>%
  mutate(more_than_25_c = ifelse(burden_total_compensation_scenario > 0.25, hh_weights,0))%>%
  group_by(Country)%>%
  summarise(more_than_25_c = sum(more_than_25_c),
            hh_weights_c   = sum(hh_weights))%>%
  ungroup()%>%
  mutate(share_more_than_25_c = more_than_25_c/hh_weights_c)

data_8.1.4.6 <- left_join(data_8.1.4.1, select(data_8.1.4.2, Country, share_more_than_25))%>%
  left_join(data_8.1.4.4)%>%
  left_join(select(data_8.1.4.5, Country, share_more_than_25_c))%>%
  mutate(y75_y25 = y75 - y25,
         y95_y5  = y95 - y5,
         y95_y5c = y95_c - y5_c)%>%
  bind_cols(Country_Code = c("BE", "BG", "HR", "CY", "CZ", "DK", "EE", "FI", "FR", "DE",
                             "EL", "HU", "IE", "IT", "LV", "LT", "LU", "NL", "PL", "PT",
                             "RO", "SK", "ES", "SE"))

library(ggrepel)

plot_8.1.4.3.1 <- ggplot(data_8.1.4.6)+
  theme_bw()+
  geom_text_repel(aes(x = y50, y = y95, label = Country_Code), nudge_x = 0.02, direction = "both", size = 2, segment.linetype = 5, segment.size = 0.03, segment.color = "grey")+
  geom_point(aes(x = y50, y = y95, fill = share_more_than_25, size = share_more_than_25), colour = "black", stroke = 0.5, shape = 21)+
  coord_cartesian(ylim = c(0,0.92), xlim = c(0,0.29))+
  scale_x_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0))+
  scale_fill_distiller(palette = "Spectral", 
                       guide = "colourbar",
                       na.value = "lightgrey", labels = percent_format(accuracy = 1), 
                       limits = c(0,0.5), breaks = seq(0,0.5,0.05))+
  scale_size_continuous(limits = c(0,0.5), breaks = seq(0,0.5,0.05), labels = percent_format(accuracy = 1))+
  guides(size = guide_legend("Share of hardship cases among poorest 40% of households"), 
         fill = guide_legend("Share of hardship cases among poorest 40% of households",
                                               barwidth = 15, barheight = 1, ticks.colour = "black",
                             nrow = 1, label.position = "bottom"))+
  ylab("Minimum additional cost for 5% of most affected households")+
  xlab("Additional Cost (Median)")+
  ggtitle("A) No compensation")+
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
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        legend.box = "horizontal",
        plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"),
        panel.border = element_rect(size = 0.3))

plot_8.1.4.3.2 <- ggplot(data_8.1.4.6)+
  theme_bw()+
  geom_segment(aes(x = y50, xend = (y50_c+y50)/2, y = y95, yend = (y95_c+y95)/2, group = Country), size = 0.2, arrow = arrow(length = unit(0.02, "npc")))+
  geom_segment(aes(x = (y50_c+y50)/2, xend = y50_c, y = (y95_c+y95)/2, yend = y95_c, group = Country), size = 0.2)+
  geom_point(aes(x = y50, y = y95, fill = share_more_than_25, size = share_more_than_25), colour = "black", stroke = 0.5, shape = 21, alpha = 0.3)+
  geom_point(aes(x = y50_c, y = y95_c, fill = share_more_than_25_c, size = share_more_than_25_c), colour = "black", stroke = 0.5, shape = 21)+
  geom_text_repel(data = filter(data_8.1.4.6, Country_Code %in% c("HU", "CZ", "SK", "RO", "DE", "IT", "PL", "HR")),aes(x = y50, y = y95, label = Country_Code), nudge_x = 0.02, size = 2, segment.size = 0.1, direction = "both",)+
  coord_cartesian(ylim = c(0,0.92), xlim = c(0,0.29))+
  scale_x_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0))+
  scale_fill_distiller(palette = "Spectral", 
                       guide = "colourbar",
                       na.value = "lightgrey", labels = percent_format(accuracy = 1), 
                       limits = c(0,0.5), breaks = seq(0,0.5,0.1))+
  scale_size_continuous()+
  guides(size = "none", fill = guide_colourbar("Share of hardship cases among poorest 40% of households",
                                               barwidth = 15, barheight = 1, ticks.colour = "black"))+
  ylab("")+
  xlab("Additional Cost (Median)")+
  ggtitle("B) Poor households, only gas")+
  theme(axis.text.y = element_blank(), 
        axis.text.x = element_text(size = 7),
        axis.title  = element_text(size = 7),
        plot.title = element_text(size = 7),
        legend.position = "bottom",
        strip.text = element_text(size = 7),
        strip.text.y = element_text(angle = 180),
        panel.grid.major = element_line(size = 0.3),
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(size = 0.2),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        legend.box = "vertical",
        plot.margin = unit(c(0.1,0.1,0.1,0), "cm"),
        panel.border = element_rect(size = 0.3),
        legend.key.width = unit(1.5, "cm"))

plot_8.1.4.3 <- ggarrange(plot_8.1.4.3.1, plot_8.1.4.3.2, common.legend = TRUE, legend = "bottom", align = "v")

jpeg("C:/Users/misl/ownCloud/EU_Gasprice_Analysis/Figures/2022_05_20_Figures/Figure_9.jpg", width = 15, height = 10, unit = "cm", res = 400)
print(plot_8.1.4.3)
dev.off()
  
# 8.2   Figures Embargo Scenario ####

Gas_prices_households_em <- read.xlsx("C:/Users/misl/ownCloud/EU_Gasprice_Analysis/Price markups/nrg_pc_202_c__custom_2588515_spreadsheet_households_embargo.xlsx", sheet = "Markup calculation",
                                   startRow = 12)%>%
  select(X1, Total.markup.factor)%>%
  rename(Country = X1, Markup_Gas_Direct = Total.markup.factor)%>%
  mutate(Country = ifelse(Country == "Slovakia", "Slovak Republic",
                          ifelse(Country == "Germany (until 1990 former territory of the FRG)", "Germany",
                                 ifelse(Country == "Czechia", "Czech Republic", Country))))%>%
  # Average for Europe
  bind_rows(data.frame(Country = c("Finland", "Cyprus"), Markup_Gas_Direct = c(4.202335, 4.202335)))

Gas_prices_industry_em <- read.xlsx("C:/Users/misl/ownCloud/EU_Gasprice_Analysis/Price markups/nrg_pc_203_c__custom_2591778_spreadsheet_non_households_embargo.xlsx", sheet = "Markup calculation",
                                 startRow = 12)%>%
  select(X1, Total.markup.factor)%>%
  rename(Country = X1, Markup_Gas_Indirect = Total.markup.factor)%>%
  mutate(Country = ifelse(Country == "Slovakia", "Slovak Republic",
                          ifelse(Country == "Germany (until 1990 former territory of the FRG)", "Germany",
                                 ifelse(Country == "Czechia", "Czech Republic", Country))))%>%
  # Average for Europe
  bind_rows(data.frame(Country = c("Cyprus"), Markup_Gas_Indirect = c(5.142419)))

# Oil prices: Decomposition

Oil_prices_0_em <- read.xlsx("C:/Users/misl/ownCloud/EU_Gasprice_Analysis/Oil_Bulletin_Prices_History_arbeitsversion_embargo_final.xlsx", sheet = "Overview clean",
                          startRow = 4)%>%
  rename(Gasoline_Markup_0 = X31,
         Diesel_Markup_0   = X32,
         Heating_Markup_0  = X33,
         LPG_Markup_0      = X34,
         Country           = X35)%>%
  select(Country, Gasoline_Markup_0, Diesel_Markup_0, Heating_Markup_0, LPG_Markup_0)%>%
  filter(!is.na(Country))%>%
  pivot_longer(-Country, names_to = "Type", values_to = "Markup")%>%
  filter(!is.na(Markup))%>%
  group_by(Country)%>%
  mutate(Markup_Average    = mean(Markup))%>%
  ungroup()%>%
  pivot_wider(names_from = "Type", values_from = "Markup")%>%
  mutate(Transport_Markup = (Diesel_Markup_0 + Gasoline_Markup_0)/2)%>%
  mutate(Heating_Markup_0 = ifelse(is.na(Heating_Markup_0), 0.9252044, Heating_Markup_0),
         LPG_Markup_0     = ifelse(is.na(LPG_Markup_0),     0.9316285, LPG_Markup_0))%>%
  pivot_longer(-Country, names_to = "Type_0", values_to = "Markup")%>%
  mutate(Type = ifelse(Type_0 == "Diesel_Markup_0", "Diesel",
                       ifelse(Type_0 == "Gasoline_Markup_0", "Gasoline",
                              ifelse(Type_0 == "Heating_Markup_0", "Heating_0",
                                     ifelse(Type_0 == "LPG_Markup_0", "LPG_0",
                                            ifelse(Type_0 == "Transport_Markup", "Other Transport Fuels", NA))))))

Oil_prices_1_em <- Oil_prices_0_em %>%
  filter(Type_0 == "Markup_Average")%>%
  rename(average_oil = Markup)%>%
  select(Country, average_oil)

Direct_Fuel_Expenditures <- read_csv("K:/WorkInProgress/2021_Carbon_Footprint_Analysis/Data_Transformed/Fuel_Direct_Effects_Europe.csv")

direct_fuel_exp_1 <- left_join(Direct_Fuel_Expenditures, select(household_information_0, hh_id, Country))%>%
  pivot_longer(starts_with("burden"), names_to = "Type", values_to = "burden_100", names_prefix = "burden_lf_100_")%>%
  left_join(Oil_prices_0_em, by = c("Country", "Type"))%>%
  mutate(burden_liquid_fuel_direct_em = burden_100*Markup)%>%
  group_by(hh_id)%>%
  summarise(burden_liquid_fuel_direct_em = sum(burden_liquid_fuel_direct_em))%>%
  ungroup()

data_8_em <- left_join(carbon_pricing_incidence_0, household_information_0)%>%
  left_join(Gas_prices_households_em)%>%
  left_join(Gas_prices_industry_em)%>%
  left_join(Oil_prices_1_em)%>%
  left_join(direct_fuel_exp_1)%>%
  mutate(burden_liquid_fuel_direct_em = ifelse(is.na(burden_liquid_fuel_direct_em), 0, burden_liquid_fuel_direct_em))%>%
  mutate(coal_markup = 2.5)%>%
  mutate(burden_gas_scenario_direct_em   = burden_GAS_direct*Markup_Gas_Direct,
         burden_gas_scenario_indirect_em = burden_GAS_indirect*Markup_Gas_Indirect,
         burden_p_c_scenario_direct_em   = burden_liquid_fuel_direct_em,
         burden_p_c_scenario_indirect_em = burden_P_C_indirect*average_oil,
         burden_coal_scenario_em         = burden_COAL_coal*coal_markup)%>%
  mutate(burden_gas_scenario_em          = burden_gas_scenario_direct_em + burden_gas_scenario_indirect_em,
         burden_p_c_scenario_em          = burden_p_c_scenario_direct_em + burden_p_c_scenario_indirect_em)%>%
  mutate(burden_total_scenario_em        = burden_gas_scenario_direct_em + burden_gas_scenario_indirect_em + burden_coal_scenario_em + burden_p_c_scenario_direct_em + burden_p_c_scenario_indirect_em)

data_8.1_em <- data_8_em %>%
  group_by(Country)%>%
  summarise(
    y5   = wtd.quantile(burden_total_scenario_em, weights = hh_weights, probs = 0.05),
    y25  = wtd.quantile(burden_total_scenario_em, weights = hh_weights, probs = 0.25),
    y50  = wtd.quantile(burden_total_scenario_em, weights = hh_weights, probs = 0.5),
    y75  = wtd.quantile(burden_total_scenario_em, weights = hh_weights, probs = 0.75),
    y95  = wtd.quantile(burden_total_scenario_em, weights = hh_weights, probs = 0.95),
    mean = wtd.mean(   burden_total_scenario_em, weights = hh_weights),
    mean_coal = wtd.mean(burden_coal_scenario_em, weights = hh_weights),
    mean_p_c  = wtd.mean(burden_p_c_scenario_em,   weights = hh_weights),
    mean_gas  = wtd.mean(burden_gas_scenario_em,   weights = hh_weights))%>%
  ungroup()

data_8.1.1_em <- data_8.1_em %>%
  pivot_longer(starts_with("mean"), names_to = "Type", values_to = "Mean")%>%
  #mutate_at(vars(y5:y95), ~ ifelse(Type == "mean", ., NA))%>%
  select(Country, Type, Mean)%>%
  mutate(Type_A = ifelse(Type == "mean", "All fuels",
                         ifelse(Type == "mean_coal", "Coal",
                                ifelse(Type == "mean_p_c", "Oil",
                                       ifelse(Type == "mean_gas", "Gas", NA)))))

data_8.1.1_em$Type_A <- factor(data_8.1.1_em$Type_A, levels = c("All fuels",
                                                          "Gas",
                                                          "Oil",
                                                          "Coal"))

plot_8.1.1_em <- ggplot()+
  geom_boxplot(data = data_8.1_em, aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95, x = reorder(Country, desc(y50))), fill = "lightgrey", alpha = 1, stat = "identity", position = position_dodge(0), outlier.shape = NA, width = 0.5, size = 0.3) +
  geom_segment(aes(x = 1.01, xend = 0.99, y = 0.13116240, yend = 0.13116240, colour = "Median incidence, all fuels"), size = 0.8)+
  theme_bw()+
  xlab("")+ ylab("Additional Costs (in % of Total Expenditures)")+
  geom_point(data = data_8.1.1_em, aes(y = Mean, fill = Type_A, x = factor(Country), shape = Type_A), size = 1.7, stroke = 0.5)+
  #geom_point(aes(y = mean_coal), shape = 24, fill = "#BC3C29FF", colour = "black", size = 1.5, stroke = 0.2)+
  #geom_point(aes(y = mean_p_c),  shape = 22, fill = "#0072B5FF", colour = "black", size = 1.5, stroke = 0.2)+
  #geom_point(aes(y = mean_gas),  shape = 25, fill = "#20854EFF", colour = "black", size = 1.5, stroke = 0.2)+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0), breaks = seq(0,1.75,0.25))+
  #scale_x_discrete(limits = rev)+
  #coord_flip(ylim = c(0,0.45))+
  coord_cartesian(ylim = c(0,1.75))+
  ggtitle("Embargo Scenario")+
  labs()+
  scale_fill_manual( name = "Average incidence of fuel price increase", values = c("white", "#20854EFF", "#0072B5FF", "#BC3C29FF"))+
  scale_shape_manual(name = "Average incidence of fuel price increase", values = c(21,22,23,24))+
  scale_colour_manual("", values = "black", breaks = "Median incidence, all fuels")+
  #scale_fill_nejm()+
  guides(fill = guide_legend(nrow = 1, order = 1, override.aes = list(shape = c(21,22,23,24)), title.position = "top", title.hjust = 0.5), colour = guide_legend(order = 2),
         shape = "none")+
  theme(axis.text.y = element_text(size = 7), 
        axis.text.x = element_text(size = 7, hjust = 0.95, vjust = 0.5, angle = 90),
        axis.title  = element_text(size = 7),
        plot.title = element_text(size = 7),
        legend.position = "bottom",
        strip.text = element_text(size = 7),
        strip.text.y = element_text(angle = 180),
        panel.grid.major = element_line(size = 0.3),
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(size = 0.2),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        legend.box = "vertical",
        plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"),
        panel.border = element_rect(size = 0.3))

jpeg("C:/Users/misl/ownCloud/EU_Gasprice_Analysis/Figures/2022_05_20_Figures/Figure_A2.jpg", width = 15, height = 17, unit = "cm", res = 400)
print(plot_8.1.1_em)
dev.off()

# Figure 2a

data_8.2_em <- data_8_em %>%
  left_join(select(data_7.3.1, hh_id, Income_Group_10_hh_USD_PPP_pc))%>%
  group_by(Income_Group_10_hh_USD_PPP_pc)%>%
  summarise(
    y5  = wtd.quantile(burden_total_scenario_em, weights = hh_weights, probs = 0.05),
    y25 = wtd.quantile(burden_total_scenario_em, weights = hh_weights, probs = 0.25),
    y50 = wtd.quantile(burden_total_scenario_em, weights = hh_weights, probs = 0.5),
    y75 = wtd.quantile(burden_total_scenario_em, weights = hh_weights, probs = 0.75),
    y95 = wtd.quantile(burden_total_scenario_em, weights = hh_weights, probs = 0.95),
    mean = wtd.mean(   burden_total_scenario_em, weights = hh_weights),
    mean_coal = wtd.mean(burden_coal_scenario_em, weights = hh_weights),
    mean_p_c  = wtd.mean(burden_p_c_scenario_em,   weights = hh_weights),
    mean_gas  = wtd.mean(burden_gas_scenario_em,   weights = hh_weights))%>%
  ungroup()

data_8.2.1_em <- data_8.2_em %>%
  pivot_longer(starts_with("mean"), names_to = "Type", values_to = "Mean")%>%
  #mutate_at(vars(y5:y95), ~ ifelse(Type == "mean", ., NA))%>%
  select(Income_Group_10_hh_USD_PPP_pc, Type, Mean)%>%
  mutate(Type_A = ifelse(Type == "mean", "All fuels",
                         ifelse(Type == "mean_coal", "Coal",
                                ifelse(Type == "mean_p_c", "Oil",
                                       ifelse(Type == "mean_gas", "Gas", NA)))))

data_8.2.1_em$Type_A <- factor(data_8.2.1_em$Type_A, levels = c("All fuels",
                                                          "Gas",
                                                          "Oil",
                                                          "Coal"))


plot_8.2_em <- ggplot()+
  geom_boxplot(data = data_8.2_em, aes(x = factor(Income_Group_10_hh_USD_PPP_pc), ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95), fill = "lightgrey", colour = "black", alpha = 1, stat = "identity", position = position_dodge(0.5), outlier.shape = NA, width = 0.5, size = 0.3) +
  geom_segment(aes(x = 1.01, xend = 0.99, y = 0.53116240, yend = 0.53116240, colour = "Median incidence, all fuels"), size = 0.8)+
  theme_bw()+
  xlab("European Expenditure Decile")+ ylab("Additional Costs (in % of Total Expenditures)")+
  geom_point(data = data_8.2.1_em, aes(y = Mean, fill = Type_A, x = factor(Income_Group_10_hh_USD_PPP_pc), shape = Type_A), size = 1.8, stroke = 0.5)+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0), breaks = seq(0,1,0.25))+
  scale_x_discrete(labels = c("1 \n Poorest \n 10 Percent", seq(2,9,1), "10 \n Richest \n 10 Percent"))+
  coord_cartesian(ylim = c(0,1.2))+
  guides(fill = guide_legend(nrow = 1, order = 1, override.aes = list(shape = c(21,22,23,24)), title.position = "top", title.hjust = 0.5), colour = guide_legend(order = 2), shape = "none")+
  labs(fill = "Average incidence of fuel price increases", alpha = "")+
  scale_fill_manual(values = c("white", "#20854EFF", "#0072B5FF", "#BC3C29FF"))+
  scale_colour_manual("", values = "black", breaks = "Median incidence, all fuels")+
  scale_shape_manual(values = c(21,22,23,24))+
  #coord_cartesian(ylim = c(0,0.3))+
  ggtitle("Embargo Scenario")+
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
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        legend.box = "vertical",
        plot.margin = unit(c(0.2,0.2,0.2,0.2), "cm"),
        panel.border = element_rect(size = 0.3))

jpeg("C:/Users/misl/ownCloud/EU_Gasprice_Analysis/Figures/2022_05_20_Figures/Figure_A3.jpg", width = 15, height = 14 , unit = "cm", res = 400)
print(plot_8.2_em)

dev.off()

# Compensation

data_8.2.1_em <- left_join(indirect_expenditures_0, household_information_0)%>%
  left_join(carbon_pricing_incidence_0, by = "hh_id")%>%
  select(hh_id, hh_weights, Country, District_Heating_burden_GAS_gas, Gas_Heating_burden_GAS_gas,
         Heating_Gas, Heating_District_Heat, Income_Group_5, hh_expenditures_USD_2014, hh_expenditures)%>%
  left_join(Gas_prices_households_em)%>%
  mutate(District_Heating_burden_GAS_gas = District_Heating_burden_GAS_gas*Markup_Gas_Direct,
         Gas_Heating_burden_GAS_gas      = Gas_Heating_burden_GAS_gas*Markup_Gas_Direct)%>%
  mutate(District_Heating_exp_EUR            = District_Heating_burden_GAS_gas*hh_expenditures,
         Gas_Heating_exp_EUR                 = Gas_Heating_burden_GAS_gas*hh_expenditures)%>%
  filter(Income_Group_5 == 1 | Income_Group_5 == 2)%>%
  filter(!is.na(District_Heating_exp_EUR)|!is.na(Gas_Heating_exp_EUR))%>%
  mutate(weighted_District_Heating_exp_EUR = District_Heating_exp_EUR*hh_weights,
         weighted_Gas_Heating_exp_EUR      = Gas_Heating_exp_EUR*hh_weights)%>%
  group_by(Country)%>%
  summarise(sum_weighted_District_Heating_exp_EUR = sum(weighted_District_Heating_exp_EUR, hh_weights, na.rm = TRUE),
            sum_weighted_Gas_Heating_exp_EUR      = sum(weighted_Gas_Heating_exp_EUR, hh_weights, na.rm = TRUE))%>%
  ungroup()%>%
  mutate(bonus = sum_weighted_District_Heating_exp_EUR + sum_weighted_Gas_Heating_exp_EUR)

# 9.    Appendix Figures ####
# 9.1   Energy Expenditure Shares for each Country ####

data_9.1 <- left_join(carbon_pricing_incidence_0, household_information_0)%>%
  select(hh_id, Income_Group_5, Country, hh_weights, hh_expenditures, starts_with("exp_LCU_"))

data_9.1[is.na(data_9.1)] <- 0

data_9.1 <- data_9.1 %>%
  pivot_longer(starts_with("exp_LCU"), names_to = "Type", values_to = "exp_LCU", names_prefix = "exp_LCU_")%>%
  mutate(share_energy = exp_LCU/hh_expenditures)%>%
  mutate(Type = ifelse(Type == "Diesel" | Type == "Petrol", "Transport Fuels", Type))%>%
  group_by(Country, Type)%>%
  summarise(y5   = wtd.quantile(share_energy, weights = hh_weights, probs = 0.05),
            y25  = wtd.quantile(share_energy, weights = hh_weights, probs = 0.25),
            y50  = wtd.quantile(share_energy, weights = hh_weights, probs = 0.5),
            y75  = wtd.quantile(share_energy, weights = hh_weights, probs = 0.75),
            y95  = wtd.quantile(share_energy, weights = hh_weights, probs = 0.95),
            mean = wtd.mean(    share_energy, weights = hh_weights))%>%
  ungroup()

plot_9.1.1 <- ggplot()+
  geom_boxplot(data = data_9.1, aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95, x = Type, fill = Type), alpha = 1, stat = "identity", position = position_dodge(0), outlier.shape = NA, width = 0.5, size = 0.3) +
  #geom_segment(aes(x = 1.01, xend = 0.99, y = 0.13116240, yend = 0.13116240, colour = "Median Expenditure Share"), size = 0.8)+
  theme_bw()+
  geom_point(data = data_9.1, aes(y = mean, x = Type), fill = "white", size = 1.4, stroke = 0.5, shape = 21)+
  facet_wrap(.~Country)+
  xlab("")+ ylab("Expenditure Share (in % of Total Expenditures)")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0))+
  coord_cartesian(ylim = c(0,0.2))+
  ggtitle("")+
  labs(fill = "Energy Type")+
  scale_fill_nejm()+
  #scale_fill_manual( name = "Average Incidence", values = c("white", "#20854EFF", "#0072B5FF", "#BC3C29FF"))+
  guides()+
  theme(axis.text.y = element_text(size = 7), 
        axis.text.x = element_text(size = 7, hjust = 0.95, vjust = 0.5, angle = 90),
        axis.title  = element_text(size = 7),
        plot.title = element_text(size = 7),
        legend.position = "bottom",
        strip.text = element_text(size = 7),
        strip.text.y = element_text(angle = 180),
        panel.grid.major = element_line(size = 0.3),
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(size = 0.2),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        legend.box = "vertical",
        plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"),
        panel.border = element_rect(size = 0.3))

jpeg("C:/Users/misl/ownCloud/EU_Gasprice_Analysis/Figures/2022_05_06_Figures/Figure_Appendix_Energy_Shares.jpg", width = 15, height = 20, unit = "cm", res = 400)
print(plot_9.1.1)
dev.off()

# 9.2   Lesehilfe Boxplot ####

data_9.2 <- data_8.1 %>%
  filter(Country == "Poland")%>%
  select(-Country.Type)

data_9.2.1 <- data_9.2 %>%
  pivot_longer(starts_with("mean"), names_to = "Type", values_to = "Mean")%>%
  #mutate_at(vars(y5:y95), ~ ifelse(Type == "mean", ., NA))%>%
  select(Country, Type, Mean)%>%
  mutate(Type_A = ifelse(Type == "mean", "All fuels",
                         ifelse(Type == "mean_coal", "Coal",
                                ifelse(Type == "mean_p_c", "Oil",
                                       ifelse(Type == "mean_gas", "Gas", NA)))))

data_9.2.1$Type_A <- factor(data_9.2.1$Type_A, levels = c("All fuels",
                                                          "Gas",
                                                          "Oil",
                                                          "Coal"))

data_9.2.2 <- data_9.2 %>%
  pivot_longer(-Country, names_to = "type", values_to = "value")%>%
  mutate(value = round(value, 2))%>%
  mutate(value_start = ifelse(type == "y50" | type == "y25" | type == "y75", 1.35,
                              ifelse(type %in% c("mean", "mean_gas", "mean_p_c", "mean_coal"), NA, 1.1)),
         value_end  = ifelse(type == "y5" | type == "y95", 1.75, 2.25))%>%
  mutate(value = ifelse(type == "y95", 0.3, value))%>%
  mutate(text_0 = ifelse(type == "y5", "5th Percentile: 5% of households \nare less affected than this point", 
                         ifelse(type == "y50", "Median: 50% of all households \nare more affected than this point",
                                ifelse(type == "y95", "95th Percentile: 5% of households \nare more affected than this point", NA))))

data_9.2.3 <- pivot_wider(mutate(pivot_longer(data_9.2, -Country, names_to = "type", values_to = "value"), value = round(value,2)), names_from = "type", values_from = "value")
data_9.2.3$y95[1] <- 0.3

library(ggtext)

plot_9.2 <- ggplot()+
  geom_boxplot(data = data_9.2.3, aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95, x = 1), fill = "lightgrey", alpha = 1, stat = "identity", position = position_dodge(0), outlier.shape = NA, width = 0.5, size = 0.3) +
  theme_bw()+
  geom_point(data = filter(data_9.2.2, type != "y25" & type != "y75"), aes(x = value_start, y = value))+
  geom_text(data = data_9.2.2, aes(x = value_end + 0.01, y = value, label = text_0, hjust = 0), size = 2.5)+
  
  geom_segment(data = filter(data_9.2.2, type == "y25" | type == "y75"), aes(x = value_start-0.05, xend = value_start + 0.1, y = value, yend = value))+
  geom_segment(aes(x = 1.45, xend = 1.45, y = 0.11, yend = 0.21))+
  geom_text(aes(x = 1.50, hjust = 0, y = 0.21), 
            label = paste("25th to 75th percentile: 50% of all households \nare within the range of this box", sep = ""), size = 2.5)+
  geom_segment(data = filter(data_9.2.2, type != "y25" & type != "y75"), aes(x = value_start, xend = value_end, y = value, yend = value, group = type))+
  xlab("")+ ylab("Additional Costs (in % of Total Expenditures)")+
  geom_point(data = data_9.2.1, aes(y = Mean, fill = Type_A, x = 1, shape = Type_A), size = 1.7, stroke = 0.5)+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0))+
  coord_cartesian(ylim = c(0,0.33), xlim = c(0.78, 4.7))+
  ggtitle("")+
  scale_fill_manual( name = "Average additional costs of \nfuel price increases from", values = c("white", "#20854EFF", "#0072B5FF", "#BC3C29FF"))+
  scale_shape_manual(name = "Average additional costs of \nfuel price increases from", values = c(21,22,23,24))+
  scale_colour_manual("", values = "black", breaks = "Median incidence, all fuels")+
  #scale_fill_nejm()+
  guides(colour = "none", fill = guide_legend(title.position = "top", title.hjust = 0.5))+
  theme(axis.text.y = element_text(size = 7), 
        axis.text.x = element_blank(),
        axis.title  = element_text(size = 7),
        plot.title = element_text(size = 7),
        legend.position = "right",
        strip.text = element_text(size = 7),
        strip.text.y = element_text(angle = 180),
        panel.grid.major.y = element_line(size = 0.3),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        legend.box = "vertical",
        plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"),
        panel.border = element_rect(size = 0.3))

jpeg("C:/Users/misl/ownCloud/EU_Gasprice_Analysis/Figures/2022_05_20_Figures/Figure_2.jpg", width = 12, height = 6, unit = "cm", res = 400)
print(plot_9.2)
dev.off()  

# 9.3   Population ####

population <- household_information_0 %>%
  mutate(hh_weights = ifelse(Country == "Czech Republic", hh_weights*czech_factor, hh_weights))%>%
  mutate(pop = hh_size*hh_weights)%>%
  group_by(Country)%>%
  summarise(pop = sum(pop))%>%
  ungroup()

population_0 <- read.xlsx("C:/Users/misl/OwnCloud/EU_Gasprice_Analysis/Supplementary Information/Population.xlsx", 
                         sheet = "Data")%>%
  select(TIME, "2014", "2021")%>%
  rename(Country = TIME, Pop_2014 = "2014", Pop_2021 = "2021")

population_1 <- left_join(population, population_0)%>%
  mutate(round_pop = round(pop,-5))%>%
  mutate(share_covered = pop/Pop_2014)

write.xlsx(population_1, "C:/Users/misl/OwnCloud/EU_Gasprice_Analysis/Supplementary Information/Population_Clean.xlsx")

# 9.4   Numbers ####

data_9.4 <- left_join(carbon_pricing_incidence_0, household_information_0)%>%
  mutate(hh_weights = ifelse(Country == "Czech Republic", hh_weights*129.1408, hh_weights),
         exp_GAS_gas_total = (exp_GAS_gas*hh_weights)/1.329,
         exp_P_C_p_c_total = (exp_P_C_p_c*hh_weights)/1.329,
         burden_direct   = burden_GAS_direct   + burden_P_C_direct   + burden_COAL_direct,
         burden_indirect = burden_GAS_indirect + burden_P_C_indirect + burden_COAL_indirect)%>%
  mutate(burden_direct_share   = burden_direct/(burden_direct + burden_indirect),
         burden_indirect_share = burden_indirect/(burden_direct + burden_indirect),
         burden = burden_direct + burden_indirect)%>%
  mutate(Gas_users = ifelse(Heating_Gas > 0 | Heating_District_Heat > 0 ,1,0))

# Average Cost increase for doubling of Gas

wtd.mean(data_9.4$burden_GAS_gas, data_9.4$hh_weights)

# Average Cost increase for doubling of Coal

wtd.mean(data_9.4$burden_COAL_coal, data_9.4$hh_weights)

# Average Cost increase for doubling of oil

wtd.mean(data_9.4$burden_P_C_p_c, data_9.4$hh_weights)

# Total Costs of Gas

sum(data_9.4$exp_GAS_gas_total)
sum(data_9.4$exp_P_C_p_c_total)

# Average Direct vs. indirect

wtd.mean(data_8$share_burden_direct, data_8$hh_weights)

# Total costs for compensating for average total costs

data_9.4.1 <- data_8.1.2.2 %>%
  filter(Type_A == "All households, all fuels")%>%
  mutate(Interest = ifelse(Country %in% c("Hungary", "Italy", "Romania", "Germany", "Poland"),1,0))%>%
  group_by(Interest)%>%
  mutate(sum_compensation_weighted = sum(compensation_weighted))%>%
  ungroup()%>%
  mutate(compensation_weighted_per_hh = compensation_weighted_EUR/hhs)

data_9.4.2 <- data_8.1.2.2 %>%
  filter(Type_A == "Poor households, only gas")%>%
  mutate(compensation_weighted_EUR = compensation_weighted/1.329)

sum(data_9.4.2$compensation_weighted)
sum(data_9.4.2$compensation_weighted_EUR)

data_9.4.3 <- data_9.4 %>%
  group_by(Gas_users)%>%
  summarise(burden_GAS_gas = wtd.mean(burden))%>%
  ungroup()

data_9.4.4 <- data_8 %>%
  mutate(hh_weights = ifelse(Country == "Czech Republic", hh_weights*129.1408, hh_weights))%>%
  left_join(select(data_7.3.1, hh_id, Income_Group_10_hh_USD_PPP_pc))%>%
  mutate(hardship = ifelse(Income_Group_10_hh_USD_PPP_pc < 5 & burden_total_scenario > 0.25, 1,0),
         pop = hh_weights*hh_size)%>%
  #filter(Income_Group_10_hh_USD_PPP_pc < 5)%>%
  mutate(pop_total = sum(pop),
         weights_total = sum(hh_weights))%>%
  group_by(hardship)%>%
  summarise(pop_hardship = sum(pop),
            pop_total = first(pop_total),
            weights_hardship = sum(hh_weights),
            weights_total = first(weights_total))%>%
  ungroup()
  
data_9.4.5 <- data_8 %>%
  mutate(hh_weights = ifelse(Country == "Czech Republic", hh_weights*129.1408, hh_weights))%>%
  mutate(exp_EUR_scenario_total          = burden_total_scenario*hh_expenditures)%>%
  mutate(exp_EUR_scenario_total_weighted = exp_EUR_scenario_total*hh_weights)

# Total additional costs

sum(data_9.4.5$exp_EUR_scenario_total_weighted)
wtd.mean(data_9.4.5$exp_EUR_scenario_total, data_9.4.5$hh_weights)

# Compensation Costs

exchange.rate <- left_join(carbon_pricing_incidence_0, household_information_0)%>%
  mutate(exchange.rate = hh_expenditures_USD_2014/hh_expenditures)%>%
  group_by(Country)%>%
  summarise(exchange.rate_0 = mean(exchange.rate))%>%
  ungroup()%>%
  mutate(Country = ifelse(Country == "Slovak Republic", "Slovakia", Country))

data_9.4.6 <- data_8.1.2.2 %>%
  left_join(exchange.rate)%>%
  mutate(compensation_weighted_EUR = compensation_weighted/exchange.rate_0)%>%
  mutate(compensation_weighted_per_HH = compensation_weighted_EUR/hhs)

data_9.4.6.1 <- data_9.4.6 %>%
  group_by(Type_A)%>%
  summarise(compensation_weighted_EUR_sum = sum(compensation_weighted_EUR),
            min_share_GDP = min(share_GDP),
            max_share_GDP = max(share_GDP))%>%
  ungroup()

data_9.4.6.2 <- data_9.4.6 %>%
  group_by(Type_A)%>%
  summarise(max_compensation_weighted_per_HH = max(compensation_weighted_per_HH),
            min_compensation_weighted_per_HH = min(compensation_weighted_per_HH),
            mean_compensation_weighted_per_HH = wtd.mean(compensation_weighted_per_HH, hhs))%>%
  ungroup()


t1 <- count(t, Country, exchange.rate)

# 10    Technical Appendix - Country-level ####

# Für jedes Land die Graphiken zusammenstellen, Daten aber schon vorher aggregieren

# 10.1   Gas Expenditures Shares per Quintile ####

data_10.1 <- left_join(carbon_pricing_incidence_0, household_information_0)%>%
  mutate(Heating_Gas = ifelse(is.na(Heating_Gas), 0, Heating_Gas),
         Heating_District_Heat = ifelse(is.na(Heating_District_Heat), 0, Heating_District_Heat))%>%
  mutate(Heating_Gas_DH = Heating_Gas + Heating_District_Heat)%>%
  mutate(share_Heating_Gas_DH = Heating_Gas_DH/hh_expenditures)%>%
  group_by(Country, Income_Group_5)%>%
  summarise(
    y5   = wtd.quantile(share_Heating_Gas_DH, weights = hh_weights, probs = 0.05),
    y25  = wtd.quantile(share_Heating_Gas_DH, weights = hh_weights, probs = 0.25),
    y50  = wtd.quantile(share_Heating_Gas_DH, weights = hh_weights, probs = 0.5),
    y75  = wtd.quantile(share_Heating_Gas_DH, weights = hh_weights, probs = 0.75),
    y95  = wtd.quantile(share_Heating_Gas_DH, weights = hh_weights, probs = 0.95),
    mean = wtd.mean(    share_Heating_Gas_DH, weights = hh_weights))%>%
  ungroup()

# 10.2  Heating Types ####

data_10.2 <- left_join(carbon_pricing_incidence_0, household_information_0)%>%
  mutate(Heating_Gas           = ifelse(is.na(Heating_Gas), 0, Heating_Gas),
         Heating_District_Heat = ifelse(is.na(Heating_District_Heat), 0, Heating_District_Heat),
         Heating_Oil           = ifelse(is.na(Heating_Oil),0, Heating_Oil))%>%
  mutate(Heating_Gas_w           = ifelse(Heating_Gas > 0, hh_weights,0),
         Heating_Oil_w           = ifelse(Heating_Oil > 0, hh_weights,0),
         Heating_District_Heat_w = ifelse(Heating_District_Heat > 0, hh_weights, 0),
         No_Heating_w            = ifelse(Heating_Gas == 0 & Heating_Oil == 0 & Heating_District_Heat == 0,hh_weights,0))%>%
  group_by(Country, Income_Group_5)%>%
  summarise("Heating with Gas" = sum(Heating_Gas_w),
            "Heating with Oil" = sum(Heating_Oil_w),
            "District Heat" = sum(Heating_District_Heat_w),
            "No Heating Exp."    = sum(No_Heating_w),
            hh_weights = sum(hh_weights))%>%
  ungroup()%>%
  pivot_longer("Heating with Gas":"No Heating Exp.", names_to = "Type", values_to = "weights")%>%
  mutate(share = weights/hh_weights)

data_10.2$Type <- factor(data_10.2$Type, levels = c("Heating with Gas", "Heating with Oil",
                                                      "District Heat", "No Heating Exp."))

# 10.3   Engel-Curves ####

data_10.3 <- left_join(carbon_pricing_incidence_0, household_information_0)%>%
  select(hh_id, Income_Group_5, Country, hh_weights, hh_expenditures, hh_expenditures_USD_2014,
         starts_with("share_"), Heating_Gas)%>%
  mutate(Heating_Gas = ifelse(is.na(Heating_Gas),0, Heating_Gas))%>%
  mutate(share_Gas = Heating_Gas/hh_expenditures)%>%
  pivot_longer(starts_with("share_"), names_to = "Type", values_to = "Share", names_prefix = "share_")%>%
  mutate(Type = ifelse(Type == "energy", "Energy",
                       ifelse(Type == "food", "Food",
                              ifelse(Type == "goods", "Goods",
                                     ifelse(Type == "services", "Services", Type)))))%>%
  filter(Type != "other_binning")

data_10.3$Type <- factor(data_10.3$Type, levels = c("Energy", "Gas", "Food", "Goods", "Services"))

# 10.4   Burden over Expenditures (Stylized) ####

data_10.4 <- left_join(carbon_pricing_incidence_0, household_information_0)%>%
  mutate(burden_100 = burden_GAS_gas + burden_P_C_p_c + burden_COAL_coal)

# 10.5  Burden over Expenditures (Baseline) ####

data_10.5 <- data_8

# 10.6  Burden over Expenditures (Embargo) ####

data_10.6 <- data_8_em

# 10.7  Boxplots (Stylized) ####

data_10.7 <- data_10.4 %>%
  group_by(Country, Income_Group_5)%>%
  summarise(
    y5   = wtd.quantile(burden_100, weights = hh_weights, probs = 0.05),
    y25  = wtd.quantile(burden_100, weights = hh_weights, probs = 0.25),
    y50  = wtd.quantile(burden_100, weights = hh_weights, probs = 0.5),
    y75  = wtd.quantile(burden_100, weights = hh_weights, probs = 0.75),
    y95  = wtd.quantile(burden_100, weights = hh_weights, probs = 0.95),
    mean = wtd.mean(    burden_100, weights = hh_weights),
    mean_coal = wtd.mean(burden_COAL_coal, weights = hh_weights),
    mean_p_c  = wtd.mean(burden_P_C_p_c,   weights = hh_weights),
    mean_gas  = wtd.mean(burden_GAS_gas,   weights = hh_weights))%>%
  ungroup()

# 10.8  Boxplots (Baseline) ####

data_10.8 <- data_10.5 %>%
  group_by(Country, Income_Group_5)%>%
  summarise(
    y5   = wtd.quantile(burden_total_scenario, weights = hh_weights, probs = 0.05),
    y25  = wtd.quantile(burden_total_scenario, weights = hh_weights, probs = 0.25),
    y50  = wtd.quantile(burden_total_scenario, weights = hh_weights, probs = 0.5),
    y75  = wtd.quantile(burden_total_scenario, weights = hh_weights, probs = 0.75),
    y95  = wtd.quantile(burden_total_scenario, weights = hh_weights, probs = 0.95),
    mean = wtd.mean(    burden_total_scenario, weights = hh_weights),
    mean_coal = wtd.mean(burden_coal_scenario, weights = hh_weights),
    mean_p_c  = wtd.mean(burden_p_c_scenario,   weights = hh_weights),
    mean_gas  = wtd.mean(burden_gas_scenario,   weights = hh_weights))%>%
  ungroup()

# 10.9   Boxplots (Embargo) ####

data_10.9 <- data_10.6 %>%
  group_by(Country, Income_Group_5)%>%
  summarise(
    y5   = wtd.quantile(burden_total_scenario_em, weights = hh_weights, probs = 0.05),
    y25  = wtd.quantile(burden_total_scenario_em, weights = hh_weights, probs = 0.25),
    y50  = wtd.quantile(burden_total_scenario_em, weights = hh_weights, probs = 0.5),
    y75  = wtd.quantile(burden_total_scenario_em, weights = hh_weights, probs = 0.75),
    y95  = wtd.quantile(burden_total_scenario_em, weights = hh_weights, probs = 0.95),
    mean = wtd.mean(    burden_total_scenario_em, weights = hh_weights),
    mean_coal = wtd.mean(burden_coal_scenario_em, weights = hh_weights),
    mean_p_c  = wtd.mean(burden_p_c_scenario_em,   weights = hh_weights),
    mean_gas  = wtd.mean(burden_gas_scenario_em,   weights = hh_weights))%>%
  ungroup()

# 10.X   Figures ####

for (i in c("Belgium", "Bulgaria", "Cyprus", "Czech Republic", "Germany", "Denmark", "Estonia", "Greece", "Spain", "Finland",
            "France", "Croatia", "Hungary" ,"Ireland", "Italy", "Lithuania", "Luxembourg", "Latvia", "Netherlands", "Poland",
            "Portugal", "Romania", "Sweden", "Slovak Republic")){

Country.Name.0 <- i

data_10.1.1 <- filter(data_10.1, Country == Country.Name.0)
data_10.2.1 <- filter(data_10.2, Country == Country.Name.0)
data_10.3.1 <- filter(data_10.3, Country == Country.Name.0)
data_10.4.1 <- filter(data_10.4, Country == Country.Name.0)
data_10.5.1 <- filter(data_10.5, Country == Country.Name.0)
data_10.6.1 <- filter(data_10.6, Country == Country.Name.0)
data_10.7.1 <- filter(data_10.7, Country == Country.Name.0)
data_10.8.1 <- filter(data_10.8, Country == Country.Name.0)
data_10.9.1 <- filter(data_10.9, Country == Country.Name.0)
data_10.10.1 <- filter(data_7.6.1, Country == Country.Name.0)
data_10.11.1 <- filter(data_7.6.2, Country == Country.Name.0)
data_10.12.1 <- filter(data_7.6.3, Country == Country.Name.0)

data_10.10.1$Type_B  <- factor(data_10.10.1$Type_B, levels = c("Goods, services, other energy", "Food", "Electricity", "District heating", "Gas heating", "Oil heating", "Transport fuels"))
data_10.11.1$Type_B  <- factor(data_10.11.1$Type_B, levels = c("Goods, services, other energy", "Food", "Electricity", "District heating", "Gas heating", "Oil heating", "Transport fuels"))
data_10.12.1$Type_B  <- factor(data_10.12.1$Type_B, levels = c("Goods, services, other energy", "Food", "Electricity", "District heating", "Gas heating", "Oil heating", "Transport fuels"))

plot_10.7 <- ggplot(data_10.7.1, aes(x = factor(Income_Group_5)))+
  geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95), fill = "lightgrey", stat = "identity", position = position_dodge(0.5), outlier.shape = NA, width = 0.5, size = 0.3) +
  theme_bw()+
  xlab("Expenditure Quintile")+ ylab("Additional Costs (in % of Total Expenditures)")+
  geom_point(aes(y = mean),     shape = 23, size = 1.8, stroke = 0.5, fill = "white")+
  geom_point(aes(y = mean_gas), shape = 22, size = 1.8, stroke = 0.5, fill = "#20854EFF")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0))+
  scale_x_discrete(labels = c("1", "2", "3", "4", "5"))+
  coord_cartesian(ylim = c(0,round(max(data_10.7.1$y95)+0.02,2)))+
  ggtitle("A) Stylized Scenario")+
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
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        legend.box = "vertical",
        plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"),
        panel.border = element_rect(size = 0.3))

plot_10.8 <- ggplot(data_10.8.1, aes(x = factor(Income_Group_5)))+
  geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95), fill = "lightgrey", stat = "identity", position = position_dodge(0.5), outlier.shape = NA, width = 0.5, size = 0.3) +
  theme_bw()+
  xlab("Expenditure Quintile")+ ylab("Additional Costs (in % of Total Expenditures)")+
  geom_point(aes(y = mean),     shape = 23, size = 1.8, stroke = 0.5, fill = "white")+
  geom_point(aes(y = mean_gas), shape = 22, size = 1.8, stroke = 0.5, fill = "#20854EFF")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0))+
  scale_x_discrete(labels = c("1", "2", "3", "4", "5"))+
  coord_cartesian(ylim = c(0,round(max(data_10.9.1$y95)+0.02,2)))+
  ggtitle("B) Baseline Scenario")+
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
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        legend.box = "vertical",
        plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"),
        panel.border = element_rect(size = 0.3))

plot_10.9 <- ggplot(data_10.9.1, aes(x = factor(Income_Group_5)))+
  geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95), fill = "lightgrey", stat = "identity", position = position_dodge(0.5), outlier.shape = NA, width = 0.5, size = 0.3) +
  theme_bw()+
  xlab("Expenditure Quintile")+ ylab("Additional Costs (in % of Total Expenditures)")+
  geom_point(aes(y = mean),     shape = 23, size = 1.8, stroke = 0.5, fill = "white")+
  geom_point(aes(y = mean_gas), shape = 22, size = 1.8, stroke = 0.5, fill = "#20854EFF")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0))+
  scale_x_discrete(labels = c("1", "2", "3", "4", "5"))+
  coord_cartesian(ylim = c(0,round(max(data_10.9.1$y95)+0.02,2)))+
  ggtitle("C) Embargo Scenario")+
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
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        legend.box = "vertical",
        plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"),
        panel.border = element_rect(size = 0.3))

if(Country.Name.0 != "Estonia" & Country.Name.0 != "Greece" & Country.Name.0 != "Romania") upper_10 <- min(round(max(data_10.4.1$hh_expenditures)/2,-4)+2000, 54000)else if
  (Country.Name.0 == "Estonia" | Country.Name.0 == "Greece") upper_10 <- 54000 else if 
  (Country.Name.0 == "Romania") upper_10 <- 35000


plot_10.4 <- ggplot(data_10.4.1, aes(x = hh_expenditures, y = burden_100))+
  geom_point(size = 0.5, shape = 21, colour = "black", alpha = 0.1)+
  #geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95), stat = "identity", position = position_dodge(0.5), outlier.shape = NA, width = 0.5, size = 0.3) +
  theme_bw()+
  xlab("Household Expenditures [€]")+ ylab("Additional Costs (in % of Total Expenditures)")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0))+
  scale_x_continuous(labels = scales::dollar_format(prefix = "€", big.mark = ","), expand = c(0,0))+
  coord_cartesian(ylim = c(0,round(max(data_10.7.1$y95),2)), xlim = c(0, upper_10))+
  geom_smooth(method = "lm", formula = y ~ poly(x,4), se = TRUE, colour = "#6F99ADFF", fullrange = TRUE, size = 0.5)+
  ggtitle("D) Stylized Scenario")+
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
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"),
        panel.border = element_rect(size = 0.3))

plot_10.5 <- ggplot(data_10.5.1, aes(x = hh_expenditures, y = burden_total_scenario))+
  geom_point(size = 0.5, shape = 21, colour = "black", alpha = 0.1)+
  #geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95), stat = "identity", position = position_dodge(0.5), outlier.shape = NA, width = 0.5, size = 0.3) +
  theme_bw()+
  xlab("Household Expenditures [€]")+ ylab("Additional Costs (in % of Total Expenditures)")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0))+
  scale_x_continuous(labels = scales::dollar_format(prefix = "€", big.mark = ","), expand = c(0,0))+
  coord_cartesian(ylim = c(0,round(max(data_10.9.1$y95),2)), xlim = c(0, upper_10))+
  geom_smooth(method = "lm", formula = y ~ poly(x,4), se = TRUE, colour = "#6F99ADFF", fullrange = TRUE, size = 0.5)+
  ggtitle("E) Baseline Scenario")+
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
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"),
        panel.border = element_rect(size = 0.3))

plot_10.6 <- ggplot(data_10.6.1, aes(x = hh_expenditures, y = burden_total_scenario_em))+
  geom_point(size = 0.5, shape = 21, colour = "black", alpha = 0.1)+
  #geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95), stat = "identity", position = position_dodge(0.5), outlier.shape = NA, width = 0.5, size = 0.3) +
  theme_bw()+
  xlab("Household Expenditures [€]")+ ylab("Additional Costs (in % of Total Expenditures)")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0))+
  scale_x_continuous(labels = scales::dollar_format(prefix = "€", big.mark = ","), expand = c(0,0))+
  coord_cartesian(ylim = c(0,round(max(data_10.9.1$y95),2)), xlim = c(0, upper_10))+
  geom_smooth(method = "lm", formula = y ~ poly(x,4), se = TRUE, colour = "#6F99ADFF", fullrange = TRUE, size = 0.5)+
  ggtitle("F) Embargo Scenario")+
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
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"),
        panel.border = element_rect(size = 0.3))


if(max(data_10.10.1$mean_agg)+0.01 >= 0.03) {acc_10.1 <- 1} else {acc_10.1 <- 0.1}

if(max(data_10.11.1$mean_agg)+0.01 >= 0.03) {acc_10.2 <- 1} else {acc_10.2 <- 0.1}

if(max(data_10.12.1$mean_agg)+0.01 >= 0.03) {acc_10.3 <- 1} else {acc_10.3 <- 0.1}

plot_10.10 <- ggplot(data_10.10.1, aes(x = factor(Income_Group_5)))+
  geom_col(aes(y = mean, fill = Type_B, alpha = Type_B), width = 0.5, position = "stack", colour = "black", size = 0.2)+
  theme_bw()+
  xlab("Expenditure Quintile")+ ylab("Average Additional Costs (in % of Total Expenditures)")+
  scale_y_continuous(labels = scales::percent_format(accuracy = acc_10.1), expand = c(0,0))+
  coord_cartesian(ylim = c(0, round(max(data_10.10.1$mean_agg)+0.01,2)))+
  #coord_cartesian(ylim = c(0,0.3))+
  labs(fill = "", alpha = "")+
  scale_fill_manual(values = c("#E18727FF", "#FFDC91FF", "#EE4C97FF",
                               "#20854EFF", "#20854EFF",
                               "#0072B5FF", "#0072B5FF"))+
  scale_alpha_manual(values = c(1,1,1,0.5,1,0.5,1))+
  ggtitle("G) Decomposition - Gas Price Increase 100%")+
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
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"),
        panel.border = element_rect(size = 0.3))  

plot_10.11 <- ggplot(data_10.11.1, aes(x = factor(Income_Group_5)))+
  geom_col(aes(y = mean, fill = Type_B, alpha = Type_B), width = 0.5, position = "stack", colour = "black", size = 0.2)+
  theme_bw()+
  xlab("Expenditure Quintile")+ ylab("Average Additional Costs (in % of Total Expenditures)")+
  scale_y_continuous(labels = scales::percent_format(accuracy = acc_10.2), expand = c(0,0))+
  coord_cartesian(ylim = c(0, round(max(data_10.11.1$mean_agg)+0.01,2)))+
  #coord_cartesian(ylim = c(0,0.3))+
  labs(fill = "")+
  scale_fill_manual(values = c("#E18727FF", "#FFDC91FF", "#EE4C97FF",
                               "#20854EFF", "#20854EFF",
                               "#0072B5FF", "#0072B5FF"))+
  scale_alpha_manual(values = c(1,1,1,0.5,1,0.5,1))+
  ggtitle("H) Decomposition - Coal Price Increase 100%")+
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
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"),
        panel.border = element_rect(size = 0.3))  

plot_10.12 <- ggplot(data_10.12.1, aes(x = factor(Income_Group_5)))+
  geom_col(aes(y = mean, fill = Type_B, alpha = Type_B), width = 0.5, position = "stack", colour = "black", size = 0.2)+
  theme_bw()+
  xlab("Expenditure Quintile")+ ylab("Average Additional Costs (in % of Total Expenditures)")+
  scale_y_continuous(labels = scales::percent_format(accuracy = acc_10.3), expand = c(0,0))+
  coord_cartesian(ylim = c(0, round(max(data_10.12.1$mean_agg)+0.01,2)))+
  #coord_cartesian(ylim = c(0,0.3))+
  labs(fill = "")+
  scale_fill_manual(values = c("#E18727FF", "#FFDC91FF", "#EE4C97FF",
                               "#20854EFF", "#20854EFF",
                               "#0072B5FF", "#0072B5FF"))+
  scale_alpha_manual(values = c(1,1,1,0.5,1,0.5,1))+
  ggtitle("I) Decomposition - Oil Price Increase 100%")+
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
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"),
        panel.border = element_rect(size = 0.3))  

plot_10.1 <- ggplot(data_10.1.1, aes(x = factor(Income_Group_5)))+
  geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95), fill = "lightgrey", stat = "identity", position = position_dodge(0.5), outlier.shape = NA, width = 0.5, size = 0.3) +
  theme_bw()+
  xlab("Expenditure Quintile")+ ylab("Gas and District Heat Expenditure Share")+
  geom_point(aes(y = mean), shape = 23, size = 1.8, stroke = 0.5, fill = "white")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0))+
  scale_x_discrete(labels = c("1", "2", "3", "4", "5"))+
  coord_cartesian(ylim = c(0,round(max(data_10.1.1$y95)+0.02,2)))+
  ggtitle("J) Gas Expenditure Share")+
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
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        legend.box = "vertical",
        plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"),
        panel.border = element_rect(size = 0.3))

plot_10.2 <- ggplot(data_10.2.1, aes(x = factor(Income_Group_5)))+
  geom_col(aes(y = share, fill = Type), width = 0.5, position = position_dodge(0.7), colour = "black")+
  theme_bw()+
  xlab("Expenditure Quintile")+ ylab("Household Share")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0))+
  coord_cartesian(ylim = c(0,round(max(data_10.2.1$share + 0.03),2)))+
  #coord_cartesian(ylim = c(0,0.3))+
  scale_fill_nejm()+
  labs(fill = "")+
  ggtitle("K) Source of Heating")+
  guides(fill = guide_legend(nrow = 2))+
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
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"),
        panel.border = element_rect(size = 0.3))

plot_10.3 <- ggplot(data_10.3.1, aes(x = hh_expenditures, y = Share, group = Type, fill = Type, colour = Type))+
  theme_bw()+
  xlab("Household Expenditures [€]")+ ylab("Expenditures Share")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0))+
  scale_x_continuous(labels = scales::dollar_format(prefix = "€", big.mark = ","), expand = c(0,0))+
  coord_cartesian(ylim = c(0,0.6), xlim = c(0, upper_10))+
  geom_smooth(method = "lm", formula = y ~ poly(x,4), se = TRUE, fullrange = TRUE, size = 1)+
  ggtitle("L) Engel Curve")+
  scale_fill_manual(  name = "", values = c("#BC3C29FF", "#20854EFF", "#0072B5FF", "#E18727FF", "#7876B1FF"))+
  scale_colour_manual(name = "", values = c("#BC3C29FF", "#20854EFF", "#0072B5FF", "#E18727FF", "#7876B1FF"))+
  guides(fill = guide_legend(nrow = 2, name = ""),
         colour = guide_legend(nrow = 2, name = ""))+
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
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"),
        panel.border = element_rect(size = 0.3))

plot_10_1 <- ggarrange(plot_10.1,  plot_10.2,  plot_10.3, nrow = 1, align = "v")
plot_10_2 <- ggarrange(plot_10.4,  plot_10.5,  plot_10.6, nrow = 1, align = "hv")
plot_10_3 <- ggarrange(plot_10.7,  plot_10.8,  plot_10.9, nrow = 1, align = "hv")
plot_10_4 <- ggarrange(plot_10.10,  plot_10.11,  plot_10.12, nrow = 1, align = "hv", common.legend = TRUE, legend = "bottom")

plot_10 <- ggarrange(plot_10_3, plot_10_2, plot_10_4, plot_10_1,
                     nrow = 4, ncol = 1, align = "v",
                     heights = c(1,1,1.25,1))

#plot_10

jpeg(sprintf("C:/Users/misl/ownCloud/EU_Gasprice_Analysis/Technical Appendix/EU-Ukraine-Analyse/Figures/Figure_%s.jpg", Country.Name.0), width = 20, height = 30, unit = "cm", res = 400)

print(plot_10)

dev.off()

print(i)

}

