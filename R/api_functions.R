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

# Input validation (clear, beginner-friendly errors) ----
check_inputs <- function(year, numeric_vars, cat_vars, geo, subset) {
  
  # Year must be a single integer in our allowed range
  if (!is.numeric(year) || length(year) != 1 || !(year %in% ALLOWED_YEARS)) {
    stop("year must be a single integer in 2010–2022.")
  }
  
  # At least one numeric variable (besides the weight PWGTP, which we auto-include)
  if (length(numeric_vars) < 1) stop("provide at least one numeric variable.")
  
  # Only allow variables listed in the spec
  bad_num <- setdiff(numeric_vars, ALLOWED_NUMERIC)
  if (length(bad_num)) stop("unknown numeric variable(s): ", paste(bad_num, collapse = ", "))
  
  if (length(cat_vars) < 1) stop("provide at least one categorical variable.")
  bad_cat <- setdiff(cat_vars, ALLOWED_CATEG)
  if (length(bad_cat)) stop("unknown categorical variable(s): ", paste(bad_cat, collapse = ", "))
  
  # Geography must be one of these levels
  if (!(geo %in% names(GEO_FIELD))) {
    stop("geo must be one of: ", paste(names(GEO_FIELD), collapse = ", "))
  }
  
  # If a subset is given, it must match the chosen geography and be coded as API codes
  if (!is.null(subset)) {
    if (geo == "All") stop("subset cannot be used when geo = 'All'.")
    if (!is.character(subset) || any(!nzchar(subset))) {
      stop("subset must be a character vector of codes for the chosen geography.")
    }
  }
  
  invisible(TRUE)
}
