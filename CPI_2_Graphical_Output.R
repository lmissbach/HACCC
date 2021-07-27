# 0   General ####

# Author: L. Missbach, missbach@mcc-berlin.net

# 0.1 Packages ####

library("cowplot")
library("ggsci")
library("haven")
library("Hmisc")
library("openxlsx")
library("rattle")
library("scales")
library("tidyverse")
options(scipen=999)

# 1   Loading Data ####

Country.Name <- "Bolivia"

carbon_pricing_incidence_0 <- read_csv(sprintf("../1_Carbon_Pricing_Incidence/1_Data_Incidence_Analysis/1_Transformed_and_Modeled/Carbon_Pricing_Incidence_%s.csv", Country.Name))

household_information_0    <- read_csv(sprintf("../1_Carbon_Pricing_Incidence/1_Data_Incidence_Analysis/1_Transformed_and_Modeled/household_information_%s_new.csv", Country.Name))

fuel_expenditures_0        <- read_csv(sprintf("../1_Carbon_Pricing_Incidence/1_Data_Incidence_Analysis/2_Fuel_Expenditure_Data/fuel_expenditures_%s.csv", Country.Name))

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

jpeg(sprintf("../1_Carbon_Pricing_Incidence/2_Figures/Figure_1_Distribution_National_Carbon_Price/Figure_1_%s.jpg", Country.Name), width = 6, height = 6, unit = "cm", res = 400)
P_1
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

plot_figure_2 <- function(ATT  = element_text(size = 7), ATX = element_text(size = 7), ATY = element_text(size = 7),
                          XLAB = "Expenditure Quintiles",
                          YLAB = "Carbon Price Incidence", 
                          fill0 = "none"){

P_2 <- ggplot(carbon_pricing_incidence_2.2, aes(x = factor(Income_Group_5)))+
  geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95), stat = "identity", position = position_dodge(0.5), outlier.shape = NA, width = 0.5, size = 0.3) +
  theme_bw()+
  xlab(XLAB)+ ylab(YLAB)+
  geom_point(aes(y = mean), shape = 23, size = 1.3, stroke = 0.2, fill = "white")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0))+
  scale_x_discrete(labels = c("1 \n Poorest \n 20 Percent", "2", "3", "4", "5 \n Richest \n 20 Percent"))+
  coord_cartesian(ylim = c(0,0.065))+
  ggtitle(Country.Name)+
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

jpeg(sprintf("../1_Carbon_Pricing_Incidence/2_Figures/Figure_2_Boxplot_National_Carbon_Price/Figure_2_%s.jpg", Country.Name), width = 6, height = 6, unit = "cm", res = 400)
P_2
dev.off()


# 2.2 Vertical Distribution across Instruments ####

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
                          fill0 = "none"){
  P_3 <- ggplot(carbon_pricing_incidence_2.3, aes(x = factor(Income_Group_5)))+
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
    coord_cartesian(ylim = c(0.5,1.5))+
    #guides(fill = guide_legend(nrow = 2, order = 1), colour = guide_legend(nrow = 2, order = 1), shape = guide_legend(nrow = 2, order = 1), alpha = FALSE, size = FALSE)+
    guides(fill = fill0, colour = fill0, shape = fill0, size = fill0, alpha = fill0)+
    xlab(XLAB)+
    ylab(YLAB)+ 
    ggtitle(Country.Name)
  
  return(P_3)
  
}

P_3 <- plot_figure_3()

jpeg(sprintf("../1_Carbon_Pricing_Incidence/2_Figures/Figure_3_Vertical_Effects/Figure_3_%s.jpg", Country.Name), width = 6, height = 6, unit = "cm", res = 400)
P_3
dev.off()

# 3.X ####


rm(list = ls())

