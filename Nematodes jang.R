library(tidyverse)
library(lubridate)
library(ggplot2)

setwd("C:/Users/Justin Jang/Documents/nematodes")
nema_data <- read_csv("nematodes.csv")

# Initial data inspection
glimpse(nema_data)
summary(nema_data)

# Convert Date to proper date format
nema_data$Date <- mdy(nema_data$Date)

# Check for missing values
sum(is.na(nema_data))

# Basic summary statistics
group_summary <- nema_data %>%
  group_by(Group) %>%
  summarise(
    Avg_Count = mean(Count),
    Total_Count = sum(Count),
    Avg_Mass = mean(Mass_count),
    Observations = n()
  )

print(group_summary)

# Summary by ID and Date
id_date_summary <- nema_data %>%
  group_by(ID, Date) %>%
  summarise(
    Total_Count = sum(Count),
    Avg_Mass = mean(Mass_count),
    .groups = 'drop'
  )

print(id_date_summary)

# Visualization 1: Time series of counts by group
ggplot(nema_data, aes(x = Date, y = Count, color = Group)) +
  geom_line(stat = "summary", fun = sum) +
  labs(title = "Total Nematode Counts by Group Over Time",
       y = "Total Count",
       x = "Date") +
  theme_minimal()

# Visualization 2: Boxplot of counts by group
ggplot(nema_data, aes(x = Group, y = Count, fill = Group)) +
  geom_boxplot() +
  labs(title = "Distribution of Counts by Nematode Group",
       y = "Count",
       x = "Group") +
  theme_minimal()

# Visualization 3: Mass vs. Count relationship
ggplot(nema_data, aes(x = Count, y = Mass_count, color = Group)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Relationship Between Count and Mass",
       x = "Count",
       y = "Mass Count") +
  theme_minimal()

# Visualization 4: Total counts per ID
ggplot(nema_data, aes(x = factor(ID), y = Count, fill = Group)) +
  geom_col() +
  labs(title = "Total Nematode Counts by ID",
       x = "ID",
       y = "Total Count") +
  theme_minimal()

# Statistical analysis: ANOVA for group differences
anova_result <- aov(Count ~ Group, data = nema_data)
summary(anova_result)

# Correlation analysis
correlation <- cor(nema_data[c("Count", "Total_grams", "Mass_count")])
print(correlation)

# Temporal analysis: Monthly trends
monthly_trends <- nema_data %>%
  mutate(Month = month(Date, label = TRUE)) %>%
  group_by(Month, Group) %>%
  summarise(
    Monthly_Count = sum(Count),
    .groups = 'drop'
  )

ggplot(monthly_trends, aes(x = Month, y = Monthly_Count, group = Group, color = Group)) +
  geom_line(size = 1) +
  labs(title = "Monthly Trends in Nematode Counts",
       y = "Total Count",
       x = "Month") +
  theme_minimal()

# Save cleaned data and summaries
write_csv(group_summary, "group_summary.csv")
write_csv(id_date_summary, "id_date_summary.csv")
ggsave("nematode_plots.pdf", width = 11, height = 8)