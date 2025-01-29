## Set working directory
setwd("/home/pawan/myfiles/tidy-tuesday/2025/2025-01-28/")

## Load necessary libraries
library(tidyverse)
library(data.table)
library(ggplot2)
library(ggpubr)

## Read the data
data <- tidytuesdayR::tt_load("2025-01-28")
water_insecurity_2022 <- data$water_insecurity_2022
water_insecurity_2023 <- data$water_insecurity_2023

## Combine data for easier analysis
water_insecurity_combined <- bind_rows(
  mutate(water_insecurity_2022, year = 2022),
  mutate(water_insecurity_2023, year = 2023)
)

## Explore the data
summary(water_insecurity_combined)
str(water_insecurity_combined)

## Calculate the change in percent lacking plumbing between years
water_insecurity_combined <- water_insecurity_combined %>%
  group_by(name) %>%
  mutate(change_percent = percent_lacking_plumbing - lag(percent_lacking_plumbing))

# Create a scatter plot to visualize the change
scatter_plot <- ggplot(water_insecurity_combined, aes(x = percent_lacking_plumbing, y = change_percent)) +
  geom_point(alpha = 0.5) +
  labs(
    title = "Change in Percent of Households \nLacking Plumbing Facilities",
    x = "Percent Lacking Plumbing in 2022",
    y = "Change in Percent Lacking Plumbing (2023 - 2022)"
  )+
    theme_bw(base_size=10)

scatter_plot

# Create a density plot to visualize the distribution of changes
density_plot <- ggplot(water_insecurity_combined, aes(x = change_percent)) +
  geom_density() +
  labs(
    title = "Distribution of Changes in \nPercent Lacking Plumbing",
    x = "Change in Percent Lacking Plumbing (2023 - 2022)"
  ) +
    theme_bw(base_size=10)

density_plot


# Combine plots using ggpubr
combined_plot <- ggarrange(scatter_plot, density_plot, ncol = 2, common.legend = TRUE)

# Print the combined plot
combined_plot

# Save the plot (optional)
ggsave("water_insecurity_changes.png", plot = combined_plot, width = 8, height = 4)

