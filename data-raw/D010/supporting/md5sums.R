####################################################################
# Calculate md5sum checks for downloaded data file, and write file
# Author: Marc Teunis
# Date: 22 June, 2020
####################################################################

md5sum(files = here::here(
  "data-raw",
  "D010",
  "v01",
  "2020-05-31_covid_ecdc_cases_geography.csv")) %>% 
  write_lines(
    path = 
      here::here(
        "data-raw",
        "D010",
        "v01",
        "2020-05-31_md5sums_covid_ecdc_cases_geography.md5"))

