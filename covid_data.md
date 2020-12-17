Covid cases and deaths for selected countries and months
================
Marc A.T. Teunis
2020-12-17 15:47:40

## Packages

``` r
library(utils)
library(tidyverse)
library(tools)
library(glue)
library(blscrapeR)
```

## Selections

The data was filtered for months: April May June July August September
October November December

The data is displayed for countries: NL BE

## Data

Download data from European Center for Disease Prevention and Control
[ECDC](https://www.ecdc.europa.eu/en)

``` r
## Read data from public url to csv file
data <- readr::read_csv(
  "https://opendata.ecdc.europa.eu/covid19/casedistribution/csv/data.csv", 
  na = c("", " "))
data
```

    ## # A tibble: 8,915 x 10
    ##    dateRep year_week cases_weekly deaths_weekly countriesAndTer~ geoId
    ##    <chr>   <chr>            <dbl>         <dbl> <chr>            <chr>
    ##  1 14/12/~ 2020-50           1757            71 Afghanistan      AF   
    ##  2 07/12/~ 2020-49           1672           137 Afghanistan      AF   
    ##  3 30/11/~ 2020-48           1073            68 Afghanistan      AF   
    ##  4 23/11/~ 2020-47           1368            69 Afghanistan      AF   
    ##  5 16/11/~ 2020-46           1164            61 Afghanistan      AF   
    ##  6 09/11/~ 2020-45            606            24 Afghanistan      AF   
    ##  7 02/11/~ 2020-44            800            27 Afghanistan      AF   
    ##  8 26/10/~ 2020-43            633            22 Afghanistan      AF   
    ##  9 19/10/~ 2020-42            401            15 Afghanistan      AF   
    ## 10 12/10/~ 2020-41            458            15 Afghanistan      AF   
    ## # ... with 8,905 more rows, and 4 more variables: countryterritoryCode <chr>,
    ## #   popData2019 <dbl>, continentExp <chr>,
    ## #   `notification_rate_per_100000_population_14-days` <dbl>

## Data cleaning

``` r
data <- data %>%
  mutate(
    date = lubridate::dmy(dateRep),
    ) %>%
  separate(
    col = date, 
    into = c("year", "month", "day"), remove = FALSE) %>%
  mutate(
    month = as.integer(month)
  )

## Filter for `r params$month` and `r params$geo_id`
## create a conversion table to go from month name to month integer
convert_months <- tibble(
  month = c(
    "january",
    "february",
    "march",
    "april",
    "may",
    "june",
    "july",
    "august",
    "september",
    "october",
    "november",
    "december"),
  month_number = c(1:12))

  month_id <- convert_months %>%
    dplyr::filter(
      month %in% m_names$value) 
id <- month_id$month_number

data_filter <- data %>%
  dplyr::filter(
    month %in% id,
     geoId %in% geo_ids
)
  
char_col <- map_lgl(
  .x = data_filter,
  .f = is.numeric 
)

country_names <- data_filter$countriesAndTerritories %>% unique() %>%
  paste(collapse = ", ")

Caps <- function(x) {
s <- strsplit(x, " ")[[1]]
paste(toupper(substring(s, 1,1)), substring(s, 2),
sep="", collapse=" ")
}

range <- data_filter$month %>% unique() %>% enframe() %>%
  arrange(value)

month_df <- left_join(range, convert_months, by = c("value" = "month_number")) %>%
  dplyr::mutate(month = firstupper(month))

min_month <- month_df %>%
  dplyr::filter(value == min(month_df$value))

countries_vec <- str_split(
  country_names, 
  pattern = ",", 
  simplify = FALSE
  ) %>%
  unlist() %>%
  trimws() %>%
  blscrapeR::firstupper()  
  

max_month <- month_df %>%
  dplyr::filter(value == max(month_df$value))

cap_cases <- paste(
  "COVID-19 positive cases from", 
  min_month$month, "to", max_month$month,
  "for", glue_collapse(countries_vec, ", ", last = " and ")
)

cap_deaths <- paste(
  "COVID-19 related deaths from", 
  min_month$month, "to", max_month$month,
  "for", glue_collapse(countries_vec, ", ", last = " and ")
)


data_filter  
```

    ## # A tibble: 74 x 14
    ##    dateRep year_week cases_weekly deaths_weekly countriesAndTer~ geoId
    ##    <chr>   <chr>            <dbl>         <dbl> <chr>            <chr>
    ##  1 14/12/~ 2020-50          14882           624 Belgium          BE   
    ##  2 07/12/~ 2020-49          15243           694 Belgium          BE   
    ##  3 30/11/~ 2020-48          15544           832 Belgium          BE   
    ##  4 23/11/~ 2020-47          20433          1060 Belgium          BE   
    ##  5 16/11/~ 2020-46          31145          1332 Belgium          BE   
    ##  6 09/11/~ 2020-45          48769          1401 Belgium          BE   
    ##  7 02/11/~ 2020-44          93531          1147 Belgium          BE   
    ##  8 26/10/~ 2020-43         109633           570 Belgium          BE   
    ##  9 19/10/~ 2020-42          78010           252 Belgium          BE   
    ## 10 12/10/~ 2020-41          43315           161 Belgium          BE   
    ## # ... with 64 more rows, and 8 more variables: countryterritoryCode <chr>,
    ## #   popData2019 <dbl>, continentExp <chr>,
    ## #   `notification_rate_per_100000_population_14-days` <dbl>, date <date>,
    ## #   year <chr>, month <int>, day <chr>

## Cases

``` r
Sys.setlocale("LC_TIME", "English")
```

    ## [1] "English_United States.1252"

``` r
plot_cases <- data_filter %>%
  na.omit() %>%
  ggplot(aes(x = date, y = cases_weekly)) +
  geom_point(aes(colour = geoId)) +
  geom_line(aes(group = geoId, colour = geoId)) +
  xlab("Date") +
  ylab("COVID-19 positive tested cases") +
  scale_x_date(breaks="month", date_labels = "%b") +
  xlab(NULL) +
  theme_bw()

plot_cases
```

![COVID-19 positive cases from April to December for Belgium and
Netherlands](covid_data_files/figure-gfm/unnamed-chunk-6-1.png)

## Deaths

``` r
## setting month abbreviations to "English"
Sys.setlocale("LC_TIME", "English")
```

    ## [1] "English_United States.1252"

``` r
plot_deaths <- data_filter %>%
  ggplot(aes(x = date, y = deaths_weekly)) +
  geom_point(aes(colour = geoId)) +
  geom_line(aes(group = geoId, colour = geoId))  +
  xlab("Date") +
  ylab("COVID-19 related deaths") +
  scale_x_date(breaks="month", date_labels = "%b") +
  xlab(NULL) +
  theme_bw()

plot_deaths
```

![COVID-19 related deaths from April to December for Belgium and
Netherlands](covid_data_files/figure-gfm/unnamed-chunk-7-1.png)
