library(jsonlite)
library(readxl)
library(httr)
library(tidyverse)
library(dplyr)
library(rvest)
library(here)
library(janitor)
library(ggplot2)

#2012 DATA
CO2_emitters_2012 <- read_excel("/Users/murphychesley/Desktop/Data Science for Economists/Final Project/Data and R Files/2023_data_summary_spreadsheets 2/ghgp_data_2012.xlsx",
                                sheet = "Direct Emitters",
                                skip = 3,        
                                col_names = TRUE # make states the column names
)

names(CO2_emitters_2012)
head(CO2_emitters_2012)

#Clean data so that all column names are lowercase and use underscores instead of spaces.
CO2_emitters_2012 <- CO2_emitters_2012 |>
  clean_names() #makes all names lowercase

emissions_by_state_2012 <- CO2_emitters_2012 |>
  group_by(state) |>
  summarise(total_emissions = sum(total_reported_direct_emissions, na.rm = TRUE)) |>
  arrange(state)
emissions_by_state_2012 <- emissions_by_state_2012 |>
  mutate(year = 2012)

# 2015 DATA
CO2_emitters_2015 <- read_excel("/Users/murphychesley/Desktop/Data Science for Economists/Final Project/Data and R Files/2023_data_summary_spreadsheets 2/ghgp_data_2015.xlsx",
                                sheet = "Direct Emitters",
                                skip = 3,       
                                col_names = TRUE 
)

names(CO2_emitters_2015)
head(CO2_emitters_2015)

CO2_emitters_2015 <- CO2_emitters_2015 |>
  clean_names() 

emissions_by_state_2015 <- CO2_emitters_2015 |>
  group_by(state) |>
  summarise(total_emissions = sum(total_reported_direct_emissions, na.rm = TRUE)) |>
  arrange(state)
emissions_by_state_2015 <- emissions_by_state_2015 |>
  mutate(year = 2015)

# 2018 DATA
CO2_emitters_2018 <- read_excel("/Users/murphychesley/Desktop/Data Science for Economists/Final Project/Data and R Files/2023_data_summary_spreadsheets 2/ghgp_data_2018.xlsx",
                                sheet = "Direct Point Emitters",
                                skip = 3,        
                                col_names = TRUE 
)

names(CO2_emitters_2018)
head(CO2_emitters_2018)

CO2_emitters_2018 <- CO2_emitters_2018 |>
  clean_names()  

emissions_by_state_2018 <- CO2_emitters_2018 |>
  group_by(state) |>
  summarise(total_emissions = sum(total_reported_direct_emissions, na.rm = TRUE)) |>
  arrange(state)
emissions_by_state_2018 <- emissions_by_state_2018 |>
  mutate(year = 2018)

# Combine the data frames for 2012, 2015, and 2018 and make them wide formatting.
emissions_all <- bind_rows(
  emissions_by_state_2012,
  emissions_by_state_2015,
  emissions_by_state_2018
)

emissions_2012_2018 <- emissions_all |>
  pivot_wider(
    names_from = year,
    values_from = total_emissions
  )

#Partition data into regions for graphing.

south <- emissions_all |>
  filter(state %in% c("TX", "OK", "AR", "LA", "MS", "AL", "GA", "FL"))

northeast <- emissions_all |>
  filter(state %in% c("ME", "NH", "VT", "MA", "RI", "CT", "NY", "PA","NJ", "DC"))

midwest <- emissions_all |>
  filter(state %in% c("ND", "SD", "NE", "KS", "MN", "IA", "MO", "WI", "IL", "IN", "OH"))

west <- emissions_all |>
  filter(state %in% c("CA", "WA", "OR", "UT", "ID", "NV", "WY", "AZ", "NM", "CO", "MO"))

non_continuous <- emissions_all |>
  filter(state %in% c("AK", "HI", "PR", "VI", "GU", "AS", "MP"))

#Graph data by region. The Y-axis will be metric tons and the X-axis will be the year. Each line represents a state
#within the region. This should help us visualize the trends in GHG emissions before and after the implementation 
#of the Obama administration's "Clean Power Plan" in 2015. 

#SOUTH GRAPH

ggplot(south, aes(x = factor(year), y = total_emissions, color = state, group = state)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Southern State GHG Emissions 2012-2018",
    x = "Year",
    y = "Metric Tons CO2e",
    color = "State"
  ) +
  theme_minimal()

#NORTHEAST GRAPH

ggplot(northeast, aes(x = factor(year), y = total_emissions, color = state, group = state)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Northeastern States Emissions 2012-2018",
    x = "Year",
    y = "Metric Tons CO2e",
    color = "State"
  ) +
  theme_minimal()

#WEST GRAPH

ggplot(west, aes(x = factor(year), y = total_emissions, color = state, group = state)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Western States GHG Emissions 2012-2018",
    x = "Year",
    y = "Metric Tons CO2e",
    color = "State"
  ) +
  theme_minimal()

#MIDWEST GRAPH

ggplot(midwest, aes(x = factor(year), y = total_emissions, color = state, group = state)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Midwest States GHG Emissions 2012-2018",
    x = "Year",
    y = "Metric Tons CO2e",
    color = "State"
  ) +
  theme_minimal()

#NONCONTINUOUS GRAPH

ggplot(non_continuous, aes(x = factor(year), y = total_emissions, color = state, group = state)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Noncontinuous States and Territories GHG Emissions 2012-2018",
    x = "Year",
    y = "Metric Tons CO2e",
    color = "State"
  ) +
  theme_minimal()
