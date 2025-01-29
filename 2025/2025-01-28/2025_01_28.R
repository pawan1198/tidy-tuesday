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
combined <- bind_rows(
    mutate(water_insecurity_2022, year = 2022),
    mutate(water_insecurity_2023, year = 2023)
)

## Explore the data
summary(combined)
str(combined)

## Calculate the change in percent lacking plumbing between years
combined <- combined %>%
    group_by(name) %>%
    mutate(change_percent = percent_lacking_plumbing - lag(percent_lacking_plumbing))

## Create a scatter plot to visualize the change
plot1<- ggplot(combined, aes(x = percent_lacking_plumbing, y = change_percent)) +
    geom_point(alpha = 0.5) +
    labs(
        title = "Change in Percent of Households \nLacking Plumbing Facilities",
        x = "Percent Lacking Plumbing",
        y = "Change in Percent Lacking Plumbing (2023-2022)"
    ) +
    theme_bw(base_size = 10)

plot1

## Create a density plot to visualize the distribution of changes
plot2 <- ggplot(combined, aes(x = change_percent)) +
    geom_density() +
    labs(
        title = "Distribution of Changes in \nPercent Lacking Plumbing",
        x = "Change in Percent Lacking Plumbing (2023-2022)"
    ) +
    theme_bw(base_size = 10)

plot2


## Combine plots using ggpubr
combined_plot <- ggarrange(plot1, plot2, ncol = 2, common.legend = TRUE)

combined_plot

## Save the plot
ggsave("water_insecurity_changes.png", plot = combined_plot, width = 8, height = 4)

