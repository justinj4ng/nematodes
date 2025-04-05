# Nematodes
# Draft script
# January 2025

# Working directory
setwd("/Users/whitneywyche/Desktop/MCC/Nematodes")

# Libraries
library(tidyr)
library(dplyr)
library(ggplot2)
# package for generating color hex codes
require("colourpicker")

# data
Nematodes <- read.csv("/Users/whitneywyche/Desktop/MCC/Nematodes/Nematodes.csv")

# format dates
Nematodes$Date = as.Date(Nematodes$Date,format ="%m/%d/%y")


## FUNCTIONAL GROUP
# make functional groups a categorical variable
Nematodes$Group <- as.factor(Nematodes$Group)

# summarize data by functional group
Nematodes_group <- Nematodes %>% 
  group_by(Group,Date) %>%
  summarize (Count = sum(Count))

# colors for each group
# colourPicker(numCols = 9)
colors <-  c("#B82C2C", "#4086C7", "#366B22", "#90C7C7", "#76428F", "#BD8944", "#5C2315", "#A8A825", "#BD35A6")

# scatter plot by species
(Nematodes_groupcount <- ggplot(Nematodes_group, aes(x = Date, y = Count, color = Group)) + 
    geom_point() +
    geom_line())


## MASS COUNT BY DATE
# standard error and group by date
Nematodes_mass <- Nematodes %>% 
  group_by(Date) %>%
  summarize (Mean_masscount = mean(Mass_count),
             MassSD = sd(Mass_count),
             MassN = n(),
             MassSE = MassSD/sqrt(MassN))

# plot mean and SE mass count
(Nematodes_mass_plot <- ggplot(Nematodes_mass, aes (x = Date, y = Mean_masscount)) +
                                 geom_bar(stat="identity") +
                                 geom_errorbar(aes(ymin = Mean_masscount - MassSE, ymax = Mean_masscount + MassSE), width = .3, position = "identity"))


