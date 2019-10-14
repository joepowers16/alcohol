##############################################################################-
## Script purpose: Single source of file path strings
#' Overview: Source this script to load all file path strings for your project
##############################################################################-

dir_proj <- here::here()
dir_data <- fs::path(dir_proj, "data")
dir_raw <- fs::path(dir_data, "raw")
dir_munge <- fs::path(dir_proj, "munge")
dir_analysis <- fs::path(dir_proj, "analysis")

file_consumption_raw <- 
  fs::path(dir_raw, "consumption.csv")
file_longitude_latitude_raw <- 
  fs::path(dir_raw, "longitude_latitude_raw.csv")
# http://data.worldbank.org/indicator/NY.GDP.PCAP.PP.CD
file_gdp_ppp_raw <- 
  fs::path(dir_raw, "API_NY.GDP.PCAP.PP.CD_DS2_en_csv_v2_247784.csv")

file_consumption_latlong_ppp <- 
  fs::path(dir_data, "consumption_latlong_ppp.rds")