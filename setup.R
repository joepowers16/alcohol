##############################################################################-
## Project: Humane Workflows
## Script purpose: Load required packages
## Date: 2019-10-16
## Author: Joseph T. Powers 
##############################################################################-

# Please execute this script to ensure you have all the packages you need for 
# this demo 

install.packages(
  c("devtools", "roxygen2", "usethis", "testthat", "tidyverse", "magrittr", 
    "knitr", "kableExtra", "scales", "here", "fs", "glue")
  )

devtools::install_github("joepowers16/breadcrumbs")

