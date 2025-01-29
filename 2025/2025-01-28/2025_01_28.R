# Load necessary libraries
setwd("/home/pawan/temp/")
## library(tidyverse)
## library(sf)
## library(tmap)
## library(plotly)
## library(dplyr)

## # Load data
## water_insecurity_2022 <- read_csv("water_insecurity_2022.csv") %>% 
##   st_as_sf(coords = NULL) 

## water_insecurity_2023 <- read_csv("water_insecurity_2023.csv") %>% 
##   st_as_sf(coords = NULL) 

## # Combine data
## combined_data <- bind_rows(
##   water_insecurity_2022 %>% mutate(year = 2022),
##   water_insecurity_2023 %>% mutate(year = 2023)
## )

## # Explore data
## summary(combined_data)
## str(combined_data)

## # Visualize data
## # Create an interactive map using plotly
## map <- ggplot(combined_data) +
##   geom_sf(aes(fill = percent_lacking_plumbing, geometry = geometry)) +
##   scale_fill_gradient(low = "lightblue", high = "darkblue", name = "% Households \nLacking Plumbing") +
##   facet_wrap(~ year) +
##   labs(title = "Percent of Households Lacking Complete Plumbing Facilities by County",
##        subtitle = "Comparison between 2022 and 2023") +
##   theme_bw()

## interactive_map <- ggplotly(map)
## interactive_map

## # Save the plot
## ggsave("water_insecurity_map.png", plot = map, width = 10, height = 6)

## # Share on social media with #TidyTuesday
## # (Replace with your actual social media post)
## # twitter_post <- "Exploring water insecurity across the US using #TidyTuesday data. 
## #                  Visualizing the percent of households lacking plumbing facilities in 2022 and 2023. 
## #                  #Rstats #DataViz #GIS"
## # tweet(twitter_post, media = "water_insecurity_map.png") 

## # Submit your own dataset (optional)
                                        # ...




# Load necessary libraries
library(tidyverse)
library(ggplot2)
library(ggpubr)

# Read the data
water_insecurity_2022 <- read.csv("water_insecurity_2022.csv")
water_insecurity_2023 <- read.csv("water_insecurity_2023.csv")

# Combine data for easier analysis
water_insecurity_combined <- bind_rows(
  mutate(water_insecurity_2022, year = 2022),
  mutate(water_insecurity_2023, year = 2023)
)

# Explore the data
summary(water_insecurity_combined)
str(water_insecurity_combined)

# Calculate the change in percent lacking plumbing between years
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
combined_plot <- ggarrange(scatter_plot, density_plot, ncol = 2, common.legend = TRUE)+theme(labs(caption="Data"))

# Print the combined plot
combined_plot

# Save the plot (optional)
ggsave("water_insecurity_changes.png", plot = combined_plot, width = 8, height = 4)



# Load necessary libraries
library(tidyverse)
library(plotly)

# Load the data
water_insecurity_2022 <- read_csv("water_insecurity_2022.csv")
water_insecurity_2023 <- read_csv("water_insecurity_2023.csv")

# Combine data for easier analysis
combined_data <- bind_rows(
  water_insecurity_2022 %>% mutate(year = 2022),
  water_insecurity_2023 %>% mutate(year = 2023)
)

# Create an interactive map using plotly
map <- plot_geo() %>%
  add_trace(
    data = combined_data,
    locations = combined_data$geoid,
    z = combined_data$percent_lacking_plumbing,
    color = combined_data$percent_lacking_plumbing,
    colorscale = "Viridis",
    text = paste("County:", combined_data$name, "<br>",
                 "Year:", combined_data$year, "<br>",
                 "Percent Lacking Plumbing:", round(combined_data$percent_lacking_plumbing, 2)),
    hoverinfo = "text"
  ) %>%
  layout(
    title = "Percent of Households Lacking Complete Plumbing Facilities by County",
    geo = list(
      scope = "usa",
      projection = list(type = "albers usa"),
      showland = TRUE,
      landcolor = "lightgray",
      subunitcolor = "gray",
      countrycolor = "gray",
      showlakes = TRUE
    )
  )

# Display the interactive map
map
