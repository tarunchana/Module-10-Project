# Utilising data from ONS articles to create visualisations for analysis

# Import packages
library(tidyverse) # contains ggplot2, tidyr, dplyr and readr
library(ggplot2)
library(janitor) # To clean column names
library(scales) # Methods for automatically determining labels for axes
library(patchwork) # Used to combine multiple plots
library(gt) # For creating tables 
library(reshape2)

G7_data <- read.csv("Data/The_increase_in_the_price_level_since_the_pandemic_in_the_UK_is_the_highest_across_the_G7.csv", header = TRUE, skip = 8)
names(G7_data)[1] = "Date"
G7_data <- melt(G7_data, id.vars = "Date")
names(G7_data)[2] = "Country"
G7_data$Date <- as.Date(paste0("01-", G7_data$Date), format = "%d-%b-%y")

G7_graph <- ggplot(data = G7_data, aes(x = Date, y = value, color = Country, group = Country)) +
  geom_line() +
  geom_line(data = subset(G7_data, Country == "UK"), size = 1.5) +
  labs(x = "Date", y = "CPI", title = "CPI Inflation across major G7 Countries since the Pandemic") +
  theme_minimal()
plot(G7_graph)

