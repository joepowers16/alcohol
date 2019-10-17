##############################################################################-
## Project: Humane Workflows
## Script purpose: Load required packages
## Date: 2019-10-16
## Author: Joseph T. Powers (copied from H. Wickham's Tidy Tools workshop)
##############################################################################-

# Please execute this script to ensure you have all the packages you need for 
# this demo 

please_install <- function(pkgs, install_fun = install.packages) {
  if (length(pkgs) == 0) {
    return(invisible())
  }
  if (!interactive()) {
    stop("Please run in interactive session", call. = FALSE)
  }
  
  title <- paste0(
    "Ok to install these packges?\n",
    paste("* ", pkgs, collapse = "\n")
  )
  ok <- menu(c("Yes", "No"), title = title) == 1
  
  if (!ok) {
    return(invisible())
  }
  
  install_fun(pkgs)
}

# Do you have all the needed packages? ------------------------------------

tidytools <- c(
  "devtools", "roxygen2", "usethis", "testthat", "tidyverse", "magrittr", 
"knitr", "kableExtra", "scales", 
)

have <- rownames(installed.packages())
needed <- setdiff(tidytools, have)

please_install(needed)

devtools::install_github("joepowers16/breadcrumbs")

