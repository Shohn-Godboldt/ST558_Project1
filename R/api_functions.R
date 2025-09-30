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

# ---- STEP 3: metadata helpers (simple + commented) -------------------------

# Get the variable dictionary (metadata) for a given year.
# Returns a big list, or NULL if the request fails.
get_variables_metadata <- function(year) {
  # Build the variables.json URL for the chosen year
  url  <- paste0(pums_base(year), "/variables.json")
  
  # Make a GET request
  resp <- httr::GET(url)
  
  # If HTTP error (e.g., 404/500), just return NULL quietly
  if (httr::http_error(resp)) return(NULL)
  
  # Pull the response text and convert from JSON to an R list
  txt <- httr::content(resp, as = "text", encoding = "UTF-8")
  meta <- jsonlite::fromJSON(txt, simplifyVector = FALSE)
  
  return(meta)
}

# From the metadata list, build a simple lookup (named character vector)
# for a variable: names = codes, values = human-readable labels.
# If the variable has no labels, return NULL.
# label_lookup (handles list/vector values safely) ----
# label_lookup(): return a named vector {code -> label} ----
label_lookup <- function(meta, var) {
  if (is.null(meta) || is.null(meta$variables)) return(NULL)
  
  entry <- meta$variables[[var]]
  if (is.null(entry) || is.null(entry$values)) return(NULL)
  
  vals <- entry$values
  
  # Case 1: top-level has item & label VECTORS (e.g., SEX)
  if (is.list(vals) && !is.null(vals$item) && !is.null(vals$label)) {
    codes  <- as.character(vals$item)
    labels <- as.character(vals$label)
    names(labels) <- codes
    return(labels)
  }
  
  # Case 2: list of small objects each with $item and $label
  if (is.list(vals) && length(vals) > 0 && all(vapply(vals, is.list, logical(1)))) {
    has_item  <- all(vapply(vals, function(x) !is.null(x$item),  logical(1)))
    has_label <- all(vapply(vals, function(x) !is.null(x$label), logical(1)))
    if (has_item && has_label) {
      codes  <- vapply(vals, function(x) as.character(x$item)[1],  character(1))
      labels <- vapply(vals, function(x) as.character(x$label)[1], character(1))
      names(labels) <- codes
      return(labels)
    }
  }
  
  # Case 3: named list/vector (names are already codes)
  if (!is.null(names(vals))) {
    labels <- vapply(vals, function(x) as.character(if (is.list(x)) x[[1]] else x)[1],
                     character(1))
    names(labels) <- names(vals)
    return(labels)
  }
  
  # Fallback: flatten to character (codes unknown)
  as.character(unlist(vals, use.names = FALSE))
}


# building the Census API URL 
build_url <- function(year, get_vars, geo, subset_codes) {
  base <- pums_base(year)
  geo_field <- GEO_FIELD[[geo]]
  
  # Always include PWGTP (weight). Include geo field so it appears in results.
  get_list <- unique(c(get_vars, "PWGTP", if (!is.na(geo_field)) geo_field))
  
  # Encode only the list of columns after get=
  url <- paste0(base, "?get=", URLencode(paste(get_list, collapse = ",")))
  
  # If user asked for a subset (e.g., specific states), add "&ST=37,45"
  if (!is.na(geo_field) && length(subset_codes)) {
    url <- paste0(url, "&", geo_field, "=", paste(subset_codes, collapse = ","))
  }
  url
}


