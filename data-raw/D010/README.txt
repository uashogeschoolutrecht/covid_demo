###################################################################################
# Data journal to files yyyy_mm_dd_covid_ecdc_cases_geography.csv
# Original dat from the European Center for Desease Control
# Author: Marc Teunis
# Date: 22 June, 2020
###################################################################################
 
## README for File "./data-raw/Do10/covid_ecdc_cases_geography.R"

The data in this file was derived from a direct download of the csv file from R with script:

"./data-raw/D010/supporting/covid_ecdc_cases_geography.R"

The data dictionary: 

|col_name                |key                                     |
|:-----------------------|:---------------------------------------|
|dateRep                 |date of reporting                       |
|day                     |day of the month                        |
|month                   |month of the year                       |
|year                    |year of reporting                       |
|cases                   |number of new cases                     |
|deaths                  |number of new deaths                    |
|countriesAndTerritories |name of geographic country of territory |
|geoId                   |id for geolocation                      |
|countryterritoryCode    |country or territory code               |
|popData2018             |population total number for year 2018   |
|continentExp            |name of continent                       |

The .md5 file corresponding to the .csv data file (same name), holds an md5sums that can be used to check the contents of the file

The licence can be found here:
https://www.ecdc.europa.eu/en/copyright

