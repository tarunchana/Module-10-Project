# Utilising data from ONS articles to create visualisations for analysis

# Import packages
library(tidyverse) # contains ggplot2, tidyr, dplyr and readr
library(janitor) # To clean column names
library(reshape2)

# Graph of CPI inflation since the pandemic compared against other major G7 countries  
G7_data <- read.csv("Data/The_increase_in_the_price_level_since_the_pandemic_in_the_UK_is_the_highest_across_the_G7.csv", header = TRUE, skip = 8)
names(G7_data)[1] = "Date"
G7_data <- melt(G7_data, id.vars = "Date")
names(G7_data)[2] = "Country"
G7_data$Date <- as.Date(paste0("01-", G7_data$Date), format = "%d-%b-%y")

G7_graph <- ggplot(data = G7_data, aes(x = Date, y = value, color = Country, group = Country)) +
  geom_line(size = 0.8) +
  geom_line(data = subset(G7_data, Country == "UK"), size = 1.5) +
  scale_color_manual(values = c("UK" = "red",
                                "France" = "blue",
                                "Germany" = "green",
                                "Italy" = "purple",
                                "Japan" = "orange",
                                "US" = "black",
                                "Canada" = "pink"), guide = guide_legend(title = "Country")) +
  labs(x = "Date (Year)", y = "CPI",
       title = expression(bold(underline("CPI Inflation across major G7 Countries since the Pandemic")))) +
  theme_minimal()

# Graph of energy inflation since the pandemic compared against other major G7 countries 
G7_energy <- read.csv("Data/UK_energy_price_inflation_was_the_highest_across_major_advanced_economies_in_March_2023.csv", header = TRUE, skip = 6)
names(G7_energy)[1] = "Date"
G7_energy <- melt(G7_energy, id.vars = "Date")
names(G7_energy)[2] = "Country"
G7_energy$Date <- as.Date(paste0("01-", G7_energy$Date), format = "%d-%b-%y")

G7_nrg_graph <- ggplot(data = G7_energy, aes(x = Date, y = value, color = Country, group = Country)) +
  geom_line(size = 0.8) +
  geom_line(data = subset(G7_energy, Country == "UK"), size = 1.5) +
  scale_color_manual(values = c("UK" = "red",
                                "France" = "blue",
                                "Germany" = "green",
                                "Italy" = "purple",
                                "Japan" = "orange",
                                "US" = "black",
                                "Canada" = "pink"), guide = guide_legend(title = "Country")) +
  labs(x = "Date (Year)", y = "12-Month Rate (%)", 
       title = expression(bold(underline("12-Month Rate of Energy Inflation across major G7 Countries since the Pandemic")))) +
  theme_minimal()

# Energy inflation isn't the only factor effecting the cost of living crisis however, 
# many goods and services are having a substantial impact on the cost of living crisis too. 
# Here are the CPI inflation rates of classes of goods and services to show how they contribute

GS_CPI <- read.csv("Data/In_the_year_to_March_2023_over_70%_of_CPI_items_experienced_a_price_increase_of_at_least_4%.csv", header = TRUE, skip = 6)
names(GS_CPI)[1] = "Item"
GS_CPI <- GS_CPI %>%
  gather(key = "Variable", value = "Value", -Item) %>%
  filter(complete.cases(.)) %>%
  arrange(Value)
names(GS_CPI)[2] = "Category"


GS_graph <- ggplot(data = GS_CPI, aes(x = reorder(Item, -Value), y = Value, fill = Category)) +
  geom_bar(stat = "identity", width = 0.8) +
  labs(
    x = "Item",
    y = "%",
    title = expression(bold(underline("12-Month Rate of CPI Inflation for Goods and Services from March 2022-2023")))
  ) +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 6), 
        legend.position = "bottom",
        legend.text = element_text(size = 8)) + 
  scale_fill_manual(values = c( "Food.and.non.alcoholic.beverages"  = "purple", "Energy" = "blue", "Other" = "green")) +
  coord_flip() 


