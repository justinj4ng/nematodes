# Nematodes
# Draft script
# Last edited April 2025

# Working directory
setwd("/Users/whitneywyche/Desktop/MCC/Nematodes")

# Libraries
library(tidyr)
library(dplyr)
library(ggplot2)
library(broom)
# package for generating color hex codes
require("colourpicker")

# data
Nematodes <- read.csv("/Users/whitneywyche/Desktop/MCC/Nematodes/Nematodes2.csv")
#view
glimpse(Nematodes)

#
# OVERALL DATA PREP
#
# format dates
Nematodes$Date = as.Date(Nematodes$Date,format ="%m/%d/%y")
# format to be categorical variable
Nematodes$Group = as.factor(Nematodes$Group)
Nematodes$Plot = as.factor(Nematodes$Plot)
Nematodes$Burn = as.factor(Nematodes$Burn)
Nematodes$Time = as.factor(Nematodes$Time)
# order of unburned/burned and pre/post for graph
Nematodes$Burn = factor(Nematodes$Burn, levels=c("Unburned","Burned"), ordered=TRUE)
Nematodes$Time = factor(Nematodes$Time, levels=c("Pre","Post"), ordered=TRUE)

#
# DATA PREP FOR GRAPHS
#
## (1) FUNCTIONAL GROUP for BURNED PLOTS
#
# subset data only MOST INTERESTING (EBA, SFV,SPP, APR)
Nematodes_interesting <- Nematodes[(Nematodes$Group=="EBA" | Nematodes$Group=="SFV.SPP" | Nematodes$Group=="APR") & (Nematodes$Burn=="Burned"),]
# summarize data by ALL functional group
interesting_summary <- Nematodes_interesting %>% 
  group_by(Group, Date) %>%
  summarise (Avg_prop_group = mean(Proportion))
print(interesting_summary)
#
# ALL GROUPS
Nematodes_burn <- Nematodes[(Nematodes$Burn=="Burned"),]
# summarize data by functional group for time series graph
group_summary <- Nematodes_burn %>% 
  group_by(Group, Date) %>%
  summarise (Avg_prop_group = mean(Proportion))
print(group_summary)

## (2) BURN VS UNBURNED
#
## BA (before-after)
#
# ALL GROUPS summarize data by functional group for bar graph
group_summary_bar <- Nematodes_burn %>% 
  group_by(Group, Time) %>%
  summarise (Avg_prop_group = mean(Proportion),
             PropSD = sd(Proportion),
             PropN = n(),
             PropSE = PropSD/sqrt(PropN))
print(group_summary_bar)
#
# MOST INTERESTING summarize data by functional group for bar graph
interesting_summary_bar <- Nematodes_interesting %>% 
  group_by(Group, Time) %>%
  summarise (Avg_prop_group = mean(Proportion),
             PropSD = sd(Proportion),
             PropN = n(),
             PropSE = PropSD/sqrt(PropN))
print(group_summary_bar)
# subset data only SFV.SPP burned
Nematodes_SFV.SPP_burn <- Nematodes[(Nematodes$Group=="SFV.SPP")& (Nematodes$Burn=="Burned"),]
# subset data only APR burned
Nematodes_APR_burn <- Nematodes[(Nematodes$Group=="APR")& (Nematodes$Burn=="Burned"),]
# subset data only EBA burned
Nematodes_EBA_burn <- Nematodes[(Nematodes$Group=="EBA")& (Nematodes$Burn=="Burned"),]
# subset data only EBA burned
Nematodes_EBA_burn <- Nematodes[(Nematodes$Group=="EBA")& (Nematodes$Burn=="Burned"),]

## BACI (before after control impact)
#
# subset data only SFV.SPP
Nematodes_SFV.SPP <- Nematodes[(Nematodes$Group=="SFV.SPP"),]
# summarize data by burn status
SFV.SPP_summary <- Nematodes_SFV.SPP %>%
  group_by(Burn, Time) %>%
  summarise(Proportion_mean = mean(Proportion),
            PropSD = sd(Proportion),
            PropN = n(),
            PropSE = PropSD/sqrt(PropN))
print(SFV.SPP_summary)
#
# subset data only APR
Nematodes_APR <- Nematodes[(Nematodes$Group=="APR"),]
# summarize data by burn status
APR_summary <- Nematodes_APR %>%
  group_by(Burn, Time) %>%
  summarise(Proportion_mean = mean(Proportion),
            PropSD = sd(Proportion),
            PropN = n(),
            PropSE = PropSD/sqrt(PropN))
#
# subset data only EBA
Nematodes_EBA <- Nematodes[(Nematodes$Group=="EBA"),]
# summarize data by burn status
EBA_summary <- Nematodes_EBA %>%
  group_by(Burn, Time) %>%
  summarise(Proportion_mean = mean(Proportion),
            PropSD = sd(Proportion),
            PropN = n(),
            PropSE = PropSD/sqrt(PropN))
#
# subset data only BBA
Nematodes_BBA <- Nematodes[(Nematodes$Group=="BBA"),]
# summarize data by burn status
BBA_summary <- Nematodes_BBA %>%
  group_by(Burn, Time) %>%
  summarise(Proportion_mean = mean(Proportion),
            PropSD = sd(Proportion),
            PropN = n(),
            PropSE = PropSD/sqrt(PropN))
#
# subset data only OMN
Nematodes_OMN <- Nematodes[(Nematodes$Group=="OMN"),]
# summarize data by burn status
OMN_summary <- Nematodes_OMN %>%
  group_by(Burn, Time) %>%
  summarise(Proportion_mean = mean(Proportion),
            PropSD = sd(Proportion),
            PropN = n(),
            PropSE = PropSD/sqrt(PropN))
#
# subset data only OPP
Nematodes_OPP <- Nematodes[(Nematodes$Group=="OPP"),]
# summarize data by burn status
OPP_summary <- Nematodes_OPP %>%
  group_by(Burn, Time) %>%
  summarise(Proportion_mean = mean(Proportion),
            PropSD = sd(Proportion),
            PropN = n(),
            PropSE = PropSD/sqrt(PropN))
#
# subset data only PRE
Nematodes_PRE <- Nematodes[(Nematodes$Group=="PRE"),]
# summarize data by burn status
PRE_summary <- Nematodes_PRE %>%
  group_by(Burn, Time) %>%
  summarise(Proportion_mean = mean(Proportion),
            PropSD = sd(Proportion),
            PropN = n(),
            PropSE = PropSD/sqrt(PropN))
#
# subset data only Plants/Plant Associated (SFV.SPP, OPP)
Nematodes_plants <- Nematodes[(Nematodes$Group=="SFV.SPP"|Nematodes$Group=="OPP"),]
# summarize data by burn status
plants_summary <- Nematodes_plants %>%
  group_by(Burn, Time) %>%
  summarise(Proportion_mean = mean(Proportion),
            PropSD = sd(Proportion),
            PropN = n(),
            PropSE = PropSD/sqrt(PropN))
#
# subset data only Bacterivores (EBV, BBV)
Nematodes_bacteria <- Nematodes[(Nematodes$Group=="BBA" | Nematodes$Group=="EBA"),]
# summarize data by burn status
bacteria_summary <- Nematodes_bacteria %>%
  group_by(Burn, Time) %>%
  summarise(Proportion_mean = mean(Proportion),
            PropSD = sd(Proportion),
            PropN = n(),
            PropSE = PropSD/sqrt(PropN))

#
# GRAPHS
#
# colors for each group
# colourPicker(numCols = 9)
colors <-  c("#B82C2C", "#4086C7", "#366B22", "#90C7C7", "#76428F", "#BD8944", "#5C2315", "#A8A825", "#BD35A6")
burn_colors <- c("#90C7C7","#B82C2C")

# VISUALIZATION 1: Time series of proportions by group
#
# MOST INTERESTING GROUPS
(ggplot(interesting_summary, aes(x = Date, y = Avg_prop_group, color =  Group)) +
    geom_point()+
    geom_smooth(method="lm", se=FALSE)+
    geom_vline(aes(xintercept=as.numeric(as.Date("2024-10-20"))), color="#B82C2C", size=.5)+
    annotate("text", x=as.Date("2024-12-2"), y=.58,label ="Prescribed burn", color="#B82C2C")+
    theme_bw()+
    ylab("\nAverage Nematode Proportion \n by Functional Group for Burned Plots\n")+
    xlab("\nTime\n"))
#
# ALL GROUPS
(ggplot(group_summary, aes(x = Date, y = Avg_prop_group, color =  Group)) +
    geom_point()+
    geom_smooth(method="lm", se=FALSE)+
    geom_vline(aes(xintercept=as.numeric(as.Date("2024-10-20"))), color="#B82C2C", size=.5)+
    annotate("text", x=as.Date("2024-12-2"), y=.58,label ="Prescribed burn", color="#B82C2C")+
    theme_bw()+
    ylab("\nAverage Nematode Proportion \n by Functional Group for Burned Plots\n")+
    xlab("\nTime\n"))
  

# VISUALIZATION 2: Bar graph pre vs. post burn for burned vs unburned plots

## BA: Before After Experiment Design (burned plots)
#
# Most interesting functional groups
(ggplot(interesting_summary_bar, aes(x= Group, y=Avg_prop_group, fill=Time))+
    geom_bar(stat="identity", position=position_dodge())+
    geom_errorbar(aes(ymin=Avg_prop_group - PropSE, ymax=Avg_prop_group + PropSE), width = .5, position = position_dodge(.9))+
    theme_bw()+
    ylab("\nNematode Functional Group Proportion\n")+
    xlab("\nFunctional Group\n")+
    scale_fill_manual(values = burn_colors))
# anova
SFV.SPP_anova_burn <- aov(Proportion ~ Time, data=Nematodes_SFV.SPP_burn)
EBA_anova_burn <- aov(Proportion ~ Time, data=Nematodes_EBA_burn)
APR_anova_burn <- aov(Proportion ~ Time, data=Nematodes_APR_burn)
summary(SFV.SPP_anova_burn)
summary(EBA_anova_burn)
summary(APR_anova_burn)


# All functional groups
(ggplot(group_summary_bar, aes(x= Group, y=Avg_prop_group, fill=Time))+
    geom_bar(stat="identity", position=position_dodge())+
    geom_errorbar(aes(ymin=Avg_prop_group - PropSE, ymax=Avg_prop_group + PropSE), width = .5, position = position_dodge(.9))+
    theme_bw()+
    ylab("\nNematode Functional Group Proportion\n")+
    xlab("\nFunctional Group\n")+
    scale_fill_manual(values = burn_colors))
    
## BACI: Before After Control Impact Experiment Design (all plots)
#
## MOST INTERESTING:
#
# SFV.SPP
(ggplot(SFV.SPP_summary, aes(x = Burn, y = Proportion_mean, fill = Time)) +
    geom_bar(stat="identity",position=position_dodge())+
    geom_errorbar(aes(ymin=Proportion_mean - PropSE, ymax=Proportion_mean + PropSE, group = interaction(Burn, Time)), width = .5, position = position_dodge(.9))+
    theme_bw()+
    ylab("\nSFV.SPP Proportion\n")+
    xlab("\nPlot Type\n")+
    scale_fill_manual(values = burn_colors))
# two-way anova
SFV.SPP_anova <- aov(Proportion ~ Time * Burn,
                     data = Nematodes_SFV.SPP)
summary(SFV.SPP_anova)


# EBA
(ggplot(EBA_summary, aes(x = Burn, y = Proportion_mean, fill = Time)) +
    geom_bar(stat="identity",position=position_dodge())+
    geom_errorbar(aes(ymin=Proportion_mean - PropSE, ymax=Proportion_mean + PropSE, group = interaction(Burn, Time)), width = .5, position = position_dodge(.9))+
    theme_bw()+
    ylab("\nEBA Proportio\n")+
    xlab("\nPlot Type\n")+
    scale_fill_manual(values = burn_colors))
# two-way anova
EBA_anova <- aov(Proportion ~ Time * Burn,
                 data = Nematodes_EBA)
summary(EBA_anova)


# APR
(ggplot(APR_summary, aes(x = Burn, y = Proportion_mean, fill = Time)) +
    geom_bar(stat="identity",position=position_dodge())+
    geom_errorbar(aes(ymin=Proportion_mean - PropSE, ymax=Proportion_mean + PropSE, group = interaction(Burn, Time)), width = .5, position = position_dodge(.9))+
    theme_bw()+
    ylab("\nAPR Proportion\n")+
    xlab("\nPlot Type\n")+
    scale_fill_manual(values = burn_colors))
# two-way anova
APR_anova <- aov(Proportion ~ Time * Burn,
                 data = Nematodes_APR)
summary(APR_anova)


## ALSO INTERESTING:
#
# BBA
  (ggplot(BBA_summary, aes(x = Burn, y = Proportion_mean, fill = Time)) +
      geom_bar(stat="identity",position=position_dodge())+
      geom_errorbar(aes(ymin=Proportion_mean - PropSE, ymax=Proportion_mean + PropSE, group = interaction(Burn, Time)), width = .5, position = position_dodge(.9))+
      theme_bw()+
      ylab("\nBBA Proportion\n")+
      xlab("\nPlot Type\n")+
      scale_fill_manual(values = burn_colors))
#
# PLANTS
(ggplot(plants_summary, aes(x = Burn, y = Proportion_mean, fill = Time)) +
    geom_bar(stat="identity",position=position_dodge())+
    geom_errorbar(aes(ymin=Proportion_mean - PropSE, ymax=Proportion_mean + PropSE, group = interaction(Burn, Time)), width = .5, position = position_dodge(.9))+
    theme_bw()+
    ylab("\nAverage Plant Nematode Proportion\n")+
    xlab("\nPlot Type\n")+
    scale_fill_manual(values = burn_colors))



## LESS INTERESTING:
#
# OMN
(ggplot(OMN_summary, aes(x = Burn, y = Proportion_mean, fill = Time)) +
    geom_bar(stat="identity",position=position_dodge())+
    geom_errorbar(aes(ymin=Proportion_mean - PropSE, ymax=Proportion_mean + PropSE, group = interaction(Burn, Time)), width = .5, position = position_dodge(.9))+
    theme_bw()+
    ylab("\nOMN Proportion\n")+
    xlab("\nPlot Type\n")+
    scale_fill_manual(values = burn_colors))
#
# OPP
(ggplot(OPP_summary, aes(x = Burn, y = Proportion_mean, fill = Time)) +
    geom_bar(stat="identity",position=position_dodge())+
    geom_errorbar(aes(ymin=Proportion_mean - PropSE, ymax=Proportion_mean + PropSE, group = interaction(Burn, Time)), width = .5, position = position_dodge(.9))+
    theme_bw()+
    ylab("\nOPP Proportion\n")+
    xlab("\nPlot Type\n")+
    scale_fill_manual(values = burn_colors))
#
# PRE
(ggplot(PRE_summary, aes(x = Burn, y = Proportion_mean, fill = Time)) +
    geom_bar(stat="identity",position=position_dodge())+
    geom_errorbar(aes(ymin=Proportion_mean - PropSE, ymax=Proportion_mean + PropSE, group = interaction(Burn, Time)), width = .5, position = position_dodge(.9))+
    theme_bw()+
    ylab("\nPRE Proportion\n")+
    xlab("\nPlot Type\n")+
    scale_fill_manual(values = burn_colors))

