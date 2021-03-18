## Getting_COVID_Data_20200118 ##
## Programmer: Kees Schipper ##
## Date Created: 2020-01-18 ##
## Updated by Kees Schipper, 2020-01-18 ##

setwd()

# setting up libraries ----------------------------------------------------

library(tidyverse)
library(httr)
library(data.table)
library(datasets)

# Getting JHU CSSE COVID data from the web --------------------------------

?url()

## Google something like "Johns hopkins COVID-19 county level data," click on the github link,
## and navigate to the csv files under timeseries analysis. Once viewing the raw data, copy and paste
## the url into R, and store as an object
US.Cases.Confirmed <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv"
url(US.Cases.Confirmed)

## going to merge state abbreviations onto our data so that we don't have to type the state name
?state.abb
?state.name
state_relation <- cbind(state.name, state.abb)

US_cases_raw <- read_csv(url(US.Cases.Confirmed)) 
  


# Cleaning the data with functions from dplyr -----------------------------

US_cases_clean <- US_cases_raw %>%
  left_join(state_relation, by = c("Province_State" = "state.name"), copy = TRUE) %>% ## merge state abbs
  select(state.abb, Province_State, everything()) %>% ## move state.abb and province_state to the left columns
  select(-iso2, -iso3, -code3, -Lat, -Long_, -Country_Region) %>% ##remove unneccesary columns
  rename(state = Province_State, ##rename our columns so they make more sense
         county = Admin2) %>%
  pivot_longer(cols = c("1/22/20":last_col()), ## pivot our date into long time-series format
               names_to = "dates", ## storing date column names in column "dates"
               values_to = "cases") ## storing values in column named cases (as that's what our data represent)

Massachusetts <- US_cases_clean %>% ## going to select data on only Massachusetts
  filter(state.abb == "MA") %>% ## filter rows with only MA (for Massachusetts)
  mutate(daily_cases = ave(cases, FIPS, FUN = function(x) c(0, diff(x))), # differentiate cases column
         dates = as.Date(dates, format = "%m/%d/%y")) %>% ## convert dates to "Date" data type
  group_by(dates, state.abb, state) %>% ## grouping by certain variables to aggregate our data
  summarise(state_cases = sum(daily_cases)) ## get case counts for all of MA by day

plot(Massachusetts$state_cases, type = "o") ## looking at case counts by day for all of MA


## for more info on these dplyr functions, check out the chapter on data transformations in R
## for data science

## https://r4ds.had.co.nz/transform.html

