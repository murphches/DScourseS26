library(tidyverse)
library(dplyr)
library(rvest)
library(tidycensus)

#WITHOUT API

url <- "https://en.wikipedia.org/wiki/Big_Ten_Conference_women%27s_volleyball"

webpage <- read_html(url)

poy <- webpage %>%
  html_element("#mw-content-text > div.mw-content-ltr.mw-parser-output > div:nth-child(12) > table > tbody > tr > td:nth-child(2) > table") %>% 
  html_table()  

#WITH API
library(jsonlite)
library(eia)
library(httr)

eia_api <- Sys.getenv("EIA_API_KEY")


url2 <- paste0(
  "https://api.eia.gov/v2/total-energy/data?api_key=", eia_api,
  "&data[]=value",
  "&frequency=annual",
  "&facets[msn][]=TETCEUS",   # Total CO2 emissions
  "&facets[msn][]=PARCEUS",   # Petroleum CO2
  "&facets[msn][]=NGRCEUS",   # Natural gas CO2
  "&facets[msn][]=CLRCEUS",   # Coal CO2
  "&sort[0][column]=period",
  "&sort[0][direction]=desc",
  "&length=200"
)

resp <- GET(url2)

df <- content(resp, as = "text") %>%
  fromJSON() %>%
  .$response %>%
  .$data %>%
  as_tibble()

View(df)

# Clean and widen the table
co2_clean <- df %>%
  mutate(
    period = as.integer(period),
    value  = as.numeric(value),
    # Normalize unit labels to be identical
    unit = "Million Metric Tons CO2"
  ) %>%
  select(year = period, fuel = seriesDescription, value, unit) %>%
  mutate(fuel = case_when(
    str_detect(fuel, "Total")       ~ "Total CO2",
    str_detect(fuel, "Petroleum")   ~ "Petroleum",
    str_detect(fuel, "Natural Gas") ~ "Natural Gas",
    str_detect(fuel, "Coal")        ~ "Coal",
    TRUE ~ fuel
  )) %>%
  pivot_wider(names_from = fuel, values_from = value) %>%
  arrange(year)

View(co2_clean)