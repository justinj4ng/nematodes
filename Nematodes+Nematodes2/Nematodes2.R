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
Precip <- read.csv("/Users/whitneywyche/Desktop/MCC/Nematodes/Precipitation_Kentfield.csv")
#view
glimpse(Nematodes)
glimpse(Precip)


#
# OVERALL DATA PREP
#
# filter only burned plots
Nematodes <- Nematodes[(Nematodes$Burn=="Burned"),]
# join precip and nematode data
Nematodes <- full_join(Nematodes,Precip)
#
# format dates
Nematodes$Date = as.Date(Nematodes$Date,format ="%m/%d/%y")
Precip$Date = as.Date(Precip$Date,format ="%m/%d/%y")
# format to be categorical variable
Nematodes$Group = as.factor(Nematodes$Group)
Nematodes$Plot = as.factor(Nematodes$Plot)
Nematodes$Burn = as.factor(Nematodes$Burn)
Nematodes$Time = as.factor(Nematodes$Time)
# format to be numeric variable
Precip$Max_Precip = as.double(Precip$Max_Precip)
# order of pre/post for graph
Nematodes$Time = factor(Nematodes$Time, levels=c("Pre","Post"), ordered=TRUE)


#
# DATA PREP FOR GRAPHS
#
## (1) FUNCTIONAL GROUP for BURNED PLOTS
#
# subset data only MOST INTERESTING (EBA, SFV.SPP, APR)
Nematodes_interesting <- Nematodes[(Nematodes$Group=="EBA" | Nematodes$Group=="SFV.SPP" | Nematodes$Group=="APR") & (Nematodes$Burn=="Burned"),]
interesting_summary <- Nematodes_interesting %>% 
  group_by(Group, Date) %>%
  summarise (Avg_prop_group = mean(Proportion))
print(interesting_summary)
#
# summarize data by ALL functional group
group_summary <- Nematodes %>% 
  group_by(Group, Date) %>%
  summarise (Avg_prop_group = mean(Proportion))
print(group_summary)


#
## (2) PRECIPITATION PROPORTION PLOTS
#
# subset data only Plants/Plant Associated (SFV.SPP, OPP)
Nematodes_plants <- Nematodes[(Nematodes$Group=="SFV.SPP"|Nematodes$Group=="OPP"),]
# summarize data by group/date for time series
plants_summary <- Nematodes_plants %>%
  group_by(Group, Date) %>%
  summarise(Proportion_mean = mean(Proportion),
            PropSD = sd(Proportion),
            PropN = n(),
            PropSE = PropSD/sqrt(PropN))


## (3) BURNED BAR GRAPH PREP
#
# ALL GROUPS summarize data by functional group for bar graph
group_summary_bar <- Nematodes %>% 
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

##
# subset for individual group analysis
#
# subset data only SFV.SPP
Nematodes_SFV.SPP <- Nematodes[(Nematodes$Group=="SFV.SPP"),]
# summarize data by burn status
SFV.SPP_summary <- Nematodes_SFV.SPP %>%
  group_by(Time) %>%
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
  group_by(Time) %>%
  summarise(Proportion_mean = mean(Proportion),
            PropSD = sd(Proportion),
            PropN = n(),
            PropSE = PropSD/sqrt(PropN))
#
# subset data only EBA
Nematodes_EBA <- Nematodes[(Nematodes$Group=="EBA"),]
# summarize data by burn status
EBA_summary <- Nematodes_EBA %>%
  group_by(Time) %>%
  summarise(Proportion_mean = mean(Proportion),
            PropSD = sd(Proportion),
            PropN = n(),
            PropSE = PropSD/sqrt(PropN))
#
# subset data only BBA
Nematodes_BBA <- Nematodes[(Nematodes$Group=="BBA"),]
# summarize data by burn status
BBA_summary <- Nematodes_BBA %>%
  group_by(Time) %>%
  summarise(Proportion_mean = mean(Proportion),
            PropSD = sd(Proportion),
            PropN = n(),
            PropSE = PropSD/sqrt(PropN))
#
# subset data only OMN
Nematodes_OMN <- Nematodes[(Nematodes$Group=="OMN"),]
# summarize data by burn status
OMN_summary <- Nematodes_OMN %>%
  group_by(Time) %>%
  summarise(Proportion_mean = mean(Proportion),
            PropSD = sd(Proportion),
            PropN = n(),
            PropSE = PropSD/sqrt(PropN))
#
# subset data only OPP
Nematodes_OPP <- Nematodes[(Nematodes$Group=="OPP"),]
# summarize data by burn status
OPP_summary <- Nematodes_OPP %>%
  group_by(Time) %>%
  summarise(Proportion_mean = mean(Proportion),
            PropSD = sd(Proportion),
            PropN = n(),
            PropSE = PropSD/sqrt(PropN))
#
# subset data only PRE
Nematodes_PRE <- Nematodes[(Nematodes$Group=="PRE"),]
# summarize data by burn status
PRE_summary <- Nematodes_PRE %>%
  group_by(Time) %>%
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
precip_color <- c("#4086C7")

# VISUALIZATION 1: Time series of proportions by group
#
# MOST INTERESTING GROUPS
(ggplot(na.omit(interesting_summary), aes(x = Date, y = Avg_prop_group, color =  Group)) +
    geom_point()+
    geom_smooth(method="lm", se=FALSE)+
    geom_vline(aes(xintercept=as.numeric(as.Date("2024-10-11"))), color="#B82C2C", size=.5)+
    annotate("text", x=as.Date("2024-11-20"), y=.58,label ="Prescribed burn", color="#B82C2C")+
    theme_bw()+
    ylab("\nAverage Nematode Proportion \n by Functional Group for Burned Plots\n")+
    xlab("\nTime\n"))
#
# ALL GROUPS
(ggplot(na.omit(group_summary), aes(x = Date, y = Avg_prop_group, color =  Group)) +
    geom_point()+
    geom_smooth(method="lm", se=FALSE)+
    geom_vline(aes(xintercept=as.numeric(as.Date("2024-10-11"))), color="#B82C2C", size=.5)+
    annotate("text", x=as.Date("2024-11-20"), y=.58,label ="Prescribed burn", color="#B82C2C")+
    theme_bw()+
    ylab("\nAverage Nematode Proportion \n by Functional Group for Burned Plots\n")+
    xlab("\nTime\n"))


# VISUALIZATION 2: Time series of max rainfall
#
# Precipitation
(ggplot(Precip, aes(x = Date, y = Max_Precip)) +
    geom_line(color=precip_color)+
    theme_bw()+
    ylab("\nMaximum Daily Precipitation (inches)\n")+
    xlab("\nTime\n"))

# Precipitation and PLANT PARASITES
(ggplot() +
    geom_line(data=Precip, aes(x = Date, y = Max_Precip/10), color=precip_color)+
    geom_point(data=na.omit(plants_summary),aes(x=Date, y=Proportion_mean,  color=Group))+
    geom_smooth(data=na.omit(plants_summary),aes(x=Date, y=Proportion_mean,  color=Group),se=FALSE)+
    scale_y_continuous(sec.axis = sec_axis(~ . *10))+
    theme_bw())


# VISUALIZATION 3: Bar graph pre vs. post burn for burned vs unburned plots
#
# Most interesting functional groups
(ggplot(na.omit(interesting_summary_bar), aes(x= Group, y=Avg_prop_group, fill=Time))+
    geom_bar(stat="identity", position=position_dodge())+
    geom_errorbar(aes(ymin=Avg_prop_group - PropSE, ymax=Avg_prop_group + PropSE), width = .5, position = position_dodge(.9))+
    theme_bw()+
    ylab("\nNematode Functional Group Proportion\n")+
    xlab("\nFunctional Group\n")+
    scale_fill_manual(values = burn_colors))

# All functional groups
(ggplot(na.omit(group_summary_bar), aes(x= Group, y=Avg_prop_group, fill=Time))+
    geom_bar(stat="identity", position=position_dodge())+
    geom_errorbar(aes(ymin=Avg_prop_group - PropSE, ymax=Avg_prop_group + PropSE), width = .5, position = position_dodge(.9))+
    theme_bw()+
    ylab("\nNematode Functional Group Proportion\n")+
    xlab("\nFunctional Group\n")+
    scale_fill_manual(values = burn_colors))


#
# ANALYSIS
#
# PRECIPITATION BY GROUP ANOVA
APR_precip_anova <- aov(Proportion ~ Max_Precip, data=Nematodes_APR)
BBA_precip_anova <- aov(Proportion ~ Max_Precip, data=Nematodes_BBA)
EBA_precip_anova <- aov(Proportion ~ Max_Precip, data=Nematodes_EBA)
OMN_precip_anova <- aov(Proportion ~ Max_Precip, data=Nematodes_OMN)
OPP_precip_anova <- aov(Proportion ~ Max_Precip, data=Nematodes_OPP)
PRE_precip_anova <- aov(Proportion ~ Max_Precip, data=Nematodes_PRE)
SFV.SPP_precip_anova <- aov(Proportion ~ Max_Precip, data=Nematodes_SFV.SPP)
summary(APR_precip_anova)
summary(BBA_precip_anova)
summary(EBA_precip_anova)
summary(OMN_precip_anova)
#OPP <.05 p value
summary(OPP_precip_anova)
summary(PRE_precip_anova)
#SFV.SPP <.05 p value
summary(SFV.SPP_precip_anova)

#
# BEFORE/AFTER ANOVA FOR PROPORTION
APR_anova <- aov(Proportion ~ Time, data=Nematodes_APR)
BBA_anova <- aov(Proportion ~ Time, data=Nematodes_BBA)
EBA_anova <- aov(Proportion ~ Time, data=Nematodes_EBA)
OMN_anova <- aov(Proportion ~ Time, data=Nematodes_OMN)
OPP_anova <- aov(Proportion ~ Time, data=Nematodes_OPP)
PRE_anova <- aov(Proportion ~ Time, data=Nematodes_PRE)
SFV.SPP_anova <- aov(Proportion ~ Time, data=Nematodes_SFV.SPP)
# none with <.05 p vale
# <.1 p value:
summary(APR_anova)
summary(BBA_anova)
# >.1 p value:
summary(EBA_anova)
summary(OMN_anova)
summary(OPP_anova)
summary(PRE_anova)
summary(SFV.SPP_anova)

