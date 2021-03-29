## helpers for data load and cleaning

#*save the following code in a file named app.R *
library(shiny)
library(maps)
library(tidyverse)
library(aweek)
library()



## load case data
data <- readr::read_csv(
  "https://opendata.ecdc.europa.eu/covid19/nationalcasedeath/csv", 
  na = c("", " "))
#data

## load vaccination data
data_vac <- readr::read_csv(
  "https://opendata.ecdc.europa.eu/covid19/vaccine_tracker/csv/data.csv", 
  na = c("", " "))
#data_vac

## preprocess and clean data
convert_week_to_date <- function(x, y){
  map2(
    x, 
    y,
    get_date) %>%
    unlist() %>%
    lubridate::as_date()
  
}

data <- data %>%
  separate(col = "year_week", into = c("year", "week")) %>%
  mutate(
    date =
      convert_week_to_date(x = week, y = year)
  )

# adding a month column 
data <- data %>%
  mutate(date_chr = as.character(date)) %>%
  separate(date_chr, 
           into = c("year2", "month", "day"), 
           sep = "-",
           remove = FALSE) %>%
  mutate(month = as.integer(month))

## preprocess and clean data_vac
data_vac <- data_vac %>%
  separate(col = "YearWeekISO", into = c("year", "week"), sep = "-") %>%
  mutate(
    week = str_replace_all(
      string = week, 
      pattern = "W", 
      replacement = "")) %>%
  mutate(
    date =
      convert_week_to_date(x = week, y = year)
  )

# adding a month column 
data_vac <- data_vac %>%
  mutate(date_chr = as.character(date)) %>%
  separate(date_chr, 
           into = c("year2", "month", "day"), 
           sep = "-",
           remove = FALSE) %>%
  mutate(month = as.integer(month)) %>%
  rename(Code = ReportingCountry)


## add country 
#download.file("https://datahub.io/core/country-list/r/data.csv", 
#              destfile = here::here(
#                "data-raw", 
#                "codes_countries"))
data_country_codes <- read_csv(
  "https://datahub.io/core/country-list/r/data.csv")

## join
data_vac <- left_join(data_vac, 
                      data_country_codes) %>%
  rename(country = Name) %>%
  janitor::clean_names()


