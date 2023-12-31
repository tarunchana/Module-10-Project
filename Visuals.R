# Utilising data from ONS articles to create visualisations for analysis

# Import packages
library(tidyverse) # contains ggplot2, tidyr, dplyr, readr and reshape2


# Read in the ONS article data
G7_data <- read.csv("Data/The_increase_in_the_price_level_since_the_pandemic_in_the_UK_is_the_highest_across_the_G7.csv", header = TRUE, skip = 8)
# Some data manipulation to rename and retype some columns 
names(G7_data)[1] = "Date"
G7_data <- melt(G7_data, id.vars = "Date")
names(G7_data)[2] = "Country"
G7_data$Date <- as.Date(paste0("01-", G7_data$Date), format = "%d-%b-%y")

# Graph of CPI inflation since the pandemic compared against other major G7 countries  
G7_graph <- ggplot(data = G7_data, aes(x = Date, y = value, color = Country, group = Country)) +
  geom_line(size = 0.8) +
  geom_line(data = subset(G7_data, Country == "UK"), line = 1.5) +
  scale_color_manual(values = c("UK" = "red",
                                "France" = "blue",
                                "Germany" = "green",
                                "Italy" = "purple",
                                "Japan" = "orange",
                                "US" = "black",
                                "Canada" = "pink"), guide = guide_legend(title = "Country")) +
  labs(x = "Date (Year)", y = "CPI",
       title = expression(bold(underline("CPI Inflation across major G7 Countries since the Pandemic")))) +
  theme_minimal() +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y")  
G7_graph <- G7_graph + theme(axis.text.x = element_text(angle = 45, hjust = 1))


#Read in data from ONS article
G7_energy <- read.csv("Data/UK_energy_price_inflation_was_the_highest_across_major_advanced_economies_in_March_2023.csv", header = TRUE, skip = 6)
# Some data manipulation to rename and retype some columns 
names(G7_energy)[1] = "Date"
G7_energy <- melt(G7_energy, id.vars = "Date")
names(G7_energy)[2] = "Country"
G7_energy$Date <- as.Date(paste0("01-", G7_energy$Date), format = "%d-%b-%y")

# Graph of energy inflation since the pandemic compared against other major G7 countries 
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
  labs(x = "Date (Year)", y = "12-Month Rate of Energy CPI Inflation (%)", 
       title = expression(bold(underline("12-Month Rate of Energy Inflation across major G7 Countries since the Pandemic")))) +
  theme_minimal() +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y")  
G7_nrg_graph <- G7_nrg_graph + theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Energy inflation isn't the only factor effecting the cost of living crisis however, 
# many goods and services are having a substantial impact on the cost of living crisis too. 
# Here are the CPI inflation rates of classes of goods and services to show how they contribute

#Read in data from ONS article
GS_CPI <- read.csv("Data/In_the_year_to_March_2023_over_70%_of_CPI_items_experienced_a_price_increase_of_at_least_4%.csv", header = TRUE, skip = 6)
# Some data manipulation to rename and retype some columns 
names(GS_CPI)[1] = "Item"
names(GS_CPI)[2] = "Food and non-alcoholic beverages"
GS_CPI <- GS_CPI %>%
  gather(key = "Variable", value = "Value", -Item) %>%
  filter(complete.cases(.)) %>%
  arrange(Value)
names(GS_CPI)[2] = "Category"

# Graph of multiple goods and services classes impact on CPI inflation
GS_graph <- ggplot(data = GS_CPI, aes(x = reorder(Item, -Value), y = Value, fill = Category)) +
  geom_bar(stat = "identity", width = 0.8) +
  labs(
    x = "Goods and Services",
    y = "%",
    title = expression(bold(underline("12-Month Rate of CPI Inflation for Goods and Services from March 2022-2023")))
  ) +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 6), 
        legend.position = "bottom",
        legend.text = element_text(size = 8)) + 
  scale_fill_manual(values = c( "Food and non-alcoholic beverages"  = "purple", "Energy" = "blue", "Other" = "darkgreen")) +
  coord_flip()


# Energy intensity at the CPI division level, excluding direct effects of energy
nrg_intensity <- read.csv("Data/Restaurants_and_hotels_have_the_highest_energy_intensity_at_the_CPI_division_level,_if_we_exclude_those_that_include_the_direct_effects_of_energy.csv", header = TRUE, skip = 7)
# Some data manipulation to rename and retype some columns 
names(nrg_intensity)[1] = "Division"

# Plot of energy intensity at the CPI division level, excluding direct effects of energy
nrg_intensity_graph <- ggplot(data = nrg_intensity, aes(x = reorder(Division, -Energy.Intensity), y = Energy.Intensity)) +
  geom_bar(stat = "identity", width = 0.9, fill = "darkgreen") +
  labs(
    x = "",
    y = "%",
    title = expression(bold(underline("Energy Intensity at the CPI Division Level, Excluding Direct Effects of Energy")))) +
  theme_minimal() +
  coord_flip()

# Read data from ONS article
intensity_CPI <- read.csv("Data/More_than_three-quarters_of_the_10.csv", header = TRUE, skip = 8)
# Some data manipulation to rename and retype some columns
names(intensity_CPI)[1] = "Date"
intensity_CPI$Date <- as.Date(paste0("01-", intensity_CPI$Date), format = "%d-%Y %b")
intensity_CPI <- intensity_CPI %>%
  pivot_longer(cols = c("Rents","Energy", "Very.high", "High", "Low","Very.low"), names_to = "Category", values_to = "Value")
intensity_CPI$Category <- factor(intensity_CPI$Category, levels = c("Rents","Energy", "Very.high", "High", "Low","Very.low"))

# Graph of Contributions to Consumer Prices Index (CPI) inflation by energy intensity, percentage points, UK, January 2019 to February 2023
intensity_CPI_comp <- ggplot(data = intensity_CPI, aes(x = Date, y = Value, fill = Category)) +
  geom_bar(stat = "identity", position = "stack", width = 10) +
  labs(
    x = "Date",
    y = "Contribution by Percentage Points (%)",
    title = expression(bold(underline("Contributions to CPI inflation by Energy Intensity from January 2019 to February 2023")))) +
  theme_minimal() +
  scale_fill_manual(values = c(
    "Rents" = "purple",
    "Energy" = "blue",
    "Very.high" = "red",
    "High" = "orange",
    "Low" = "darkgreen",
    "Very.low" = "green")) +
  scale_x_date(date_breaks = "2 months", date_labels = "%b %Y")  
intensity_CPI_comp <- intensity_CPI_comp + theme(axis.text.x = element_text(angle = 45, hjust = 1))
