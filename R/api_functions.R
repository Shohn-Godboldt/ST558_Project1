# simple, readable PUMS helpers (novice–intermediate style) ---
# Packages you’ll need
library(httr)
library(jsonlite)
library(dplyr)
library(tibble)
library(stringr)
library(forcats)

# constants 
pums_base <- function(year) sprintf("https://api.census.gov/data/%d/acs/acs1/pums", year)
ALLOWED_YEARS <- 2010:2022
ALLOWED_NUMERIC <- c("AGEP","GASP","GRPIP","JWAP","JWDP","JWMNP")  # PWGTP always added
ALLOWED_CATEG  <- c("FER","HHL","HISPEED","JWTRNS","SCH","SCHL","SEX")
GEO_FIELD <- c("All"=NA, "Region"="REGION", "Division"="DIVISION", "State"="ST")

# Make returned tibbles carry a 'census' class (for your later summary/plot)
as_census <- function(df){
  class(df) <- c("census", class(df))
  df
}
