install.packages("tidyverse")
install.packages("plotly")

library(tidyverse)
library(plotly)

unicef_metadata <- read_csv("unicef_metadata.csv")
unicef_indicator_2 <- read_csv("unicef_indicator_2.csv", cols(year = col_integer(), GDPPerCap = col_number()))

data_join <- full_join(unicef_metadata, unicef_indicator_2, by = c("country" = "country", "year" = "time_period"))

map_world <- map_data("world")

data_join_2005 <- data_join %>%
  filter(year == 2005)

map_data_join_2005 <- full_join(data_join_2005, map_world, by = c("country" = "region"))

ggplot(map_data_join_2005) +
  aes(x = long, y = lat, group = group, fill = LifeExp) +
  geom_polygon(color = "black") + # Add country borders +
  scale_fill_gradient(low = "red", high = "blue", name = "Life Expectancy") +
  labs(title = "Life Expectancy by Country in 2005",
       caption = "Life Expectancy",
       fill = "Life Expectancy") +
  theme_minimal() +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(size = 14, face = "bold"),
        plot.caption = element_text(hjust = 0),
        legend.position = "bottom")

# map above, dont touch






#time series



# 

ggplot(data_join) +
  aes(x = indicator, y = LifeExp, colour = country, size = Population) +
  geom_point(alpha = 0.2) +
  facet_wrap(~ year) +
  labs(
    x = "GDP per Capita in USD",
    y = "Life Expectancy",
    title = "Evolution of the relationship between life expectancy and GDP from 1952 to 2007 per Continent"
  ) +
  theme_classic() +
  theme(
    text = element_text(family = "serif"),
        guides(colour = "none", size = "none"))

#chat

data_join_2005 <- data_join %>%
  filter(year == 2005)


# Scatter Plot

p <- ggplot(data_join_2005) +
  aes(GDPPerCap, LifeExp, colour = country, size = Population, text = country) +
  geom_point(alpha = 0.6) +
  facet_wrap(~ year) +
  labs(
    x = "GDP Per Capita",
    y = "Life Expectancy",
    title = "The Relationship of Life Expectancy and GDP Per Capita in 2005"
  ) +
  guides(colour = "none", size = "none") +
  theme_classic() +
  theme(text = element_text(family = "serif"))
  
p <- ggplotly(p, tooltip = "text")

p




# bar chart

library(ggplot2)

country_agg <- aggregate(GDPPerCap ~ country, data_join, mean)

country_agg <- country_agg[order(-country_agg$GDPPerCap), ]

horizontal_bar_plot <- ggplot(country_agg, aes(x = GDPPerCap, y = reorder(country, GDPPerCap), text = country)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(
    x = "Average GDP Per Capita",
    y = "Country",
    title = "Average GDP Per Capita by Country"
  ) +
  theme_classic() +
  theme(
    text = element_text(family = "serif"),
    axis.text.x = element_blank()
  ) +
  coord_flip()

horizontal_bar_plot <- ggplotly(horizontal_bar_plot, tooltip = "text")

horizontal_bar_plot




# time series


# time series visualisation, showing evolution of countries life expectancy

timeseries_plot_1 <- data_join %>%
  ggplot() +
  aes(year, LifeExp, colour = country) +
  geom_line() +
  labs(
    x = "Year",
    y = "Life Expectancy",
    title = "Time Series of Life Expectancy by Country",
    colour = "Country"
  )

ggplotly(timeseries_plot_1)


# enhanced




country_agg <- aggregate(GDPPerCap ~ country, data_join, mean)

country_agg <- country_agg[order(-country_agg$GDPPerCap), ]

horizontal_bar_plot <- ggplot(country_agg, aes(x = GDPPerCap, y = reorder(country, GDPPerCap), text = country)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(
    x = "Average GDP Per Capita",
    y = "Country",
    title = "Average GDP Per Capita by Country"
  ) +
  theme_classic() +
  theme(
    text = element_text(family = "serif"),
    axis.text.x = element_blank()
  ) +
  coord_flip()

horizontal_bar_plot <- ggplotly(horizontal_bar_plot, tooltip = "text")

horizontal_bar_plot

