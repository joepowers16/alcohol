##############################################################################-
## Project: alcohol
## Script purpose: merge alcohol data for report
## Date: 2019-10-16
## Author: Joseph T. Powers
##############################################################################-

##  Overview ----
##############################################################################-
# This script cleans and merges data on alcohol consumption, wealth, and latitude.

## Packages, Parameters, & Input Data ----
##############################################################################-
suppressPackageStartupMessages(library(breadcrumbs))
suppressPackageStartupMessages(library(magrittr))
suppressPackageStartupMessages(library(tidyverse))

source_file_paths()

read_lines(file_consumption_raw, n_max = 5)
consumption <- read_csv(file_consumption_raw)

read_lines(file_longitude_latitude_raw, n_max = 5)
longlat <- read_csv(file_longitude_latitude_raw)

readLines(file_gdp_ppp_raw, n = 10)
ppp <- read_csv(file_gdp_ppp_raw, skip = 3)

## clean up data ----
##############################################################################-

consumption <- consumption %>% rename(total = total_litres_of_pure_alcohol)

longlat <- 
  longlat %>% 
  rename(abbr_2 = country, country = name)

ppp <- 
  ppp %>% 
  select(country = `Country Name`, abbr = `Country Code`, ppp = `2016`)

# join the data 
d <- consumption %>% 
  left_join(longlat, by = "country") %>% 
  left_join(ppp, by = "country")

write_rds(d, file_consumption_latlong_ppp)
