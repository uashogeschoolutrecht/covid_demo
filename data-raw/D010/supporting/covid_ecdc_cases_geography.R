#######################################################
# Read Daily updates on COVID-19 data from the 
# European Center for Disease Control (ECDC)
# Author: Marc Teunis
# Date: 22 June 2020
#######################################################

## Packages
library(utils)
library(tidyverse)
library(tools)
library(readxl) 
library(httr) 

## Read data from public url to csv file
data <- readr::read_csv(
  "https://opendata.ecdc.europa.eu/covid19/casedistribution/csv/data.csv", 
  na = c("", " "))

#write_csv(data, here::here(
#  "data-raw",
#  "D010",
#  paste(Sys.Date(), "covid_ecdc_cases_geography.csv", sep = "_")
#  )
#)
  