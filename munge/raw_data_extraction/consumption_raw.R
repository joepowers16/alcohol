##############################################################################-
## Project: Alcohol
## Script purpose: downloads data on alcohol consumption 
## Date: 2019-10-13
## Author: Joseph T. Powers
##############################################################################-

## Packages, Parameters, & Input Data ----
##############################################################################-
suppressPackageStartupMessages(library(breadcrumbs))
suppressPackageStartupMessages(library(tidyverse))

source_file_paths()

consumption <- 
  read_csv(
    paste0(
      "https://raw.githubusercontent.com/fivethirtyeight/data/master/",
      "alcohol-consumption/drinks.csv"
    )
  )

write_csv(consumption, file_consumption_raw)
