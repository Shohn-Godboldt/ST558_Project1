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

# Input validation (clear, beginner-friendly errors) --------------------------
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

# Metadata helpers--------------------------------------------------------

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
# label_lookup(): always return named {code -> label} 
label_lookup <- function(meta, var) {
  if (is.null(meta) || is.null(meta$variables)) return(NULL)
  entry <- meta$variables[[var]]
  if (is.null(entry) || is.null(entry$values)) return(NULL)
  
  vals <- entry$values
  
  # 0) If it's already a nice data.frame with code/label columns
  if (is.data.frame(vals)) {
    code_col  <- intersect(c("item","value","code","id"), names(vals))[1]
    label_col <- intersect(c("label","text","description","name"), names(vals))[1]
    if (!is.na(code_col) && !is.na(label_col)) {
      labs <- as.character(vals[[label_col]])
      names(labs) <- as.character(vals[[code_col]])
      return(labs)
    }
  }
  
  # 1) Try to normalize any odd list shape into a data.frame
  #    by round-tripping through JSON with simplifyVector = TRUE
  norm <- try(
    jsonlite::fromJSON(jsonlite::toJSON(vals, auto_unbox = TRUE),
                       simplifyVector = TRUE),
    silent = TRUE
  )
  
  if (!inherits(norm, "try-error")) {
    # A) data.frame with columns
    if (is.data.frame(norm)) {
      code_col  <- intersect(c("item","value","code","id"), names(norm))[1]
      label_col <- intersect(c("label","text","description","name"), names(norm))[1]
      if (!is.na(code_col) && !is.na(label_col)) {
        labs <- as.character(norm[[label_col]])
        names(labs) <- as.character(norm[[code_col]])
        return(labs)
      }
    }
    # B) list with vectors item/label
    if (is.list(norm) && !is.null(norm$item) && !is.null(norm$label)) {
      labs <- as.character(norm$label)
      names(labs) <- as.character(norm$item)
      return(labs)
    }
  }
  
  # 2) As a last resort, handle named list/vector directly
  if (is.list(vals) && !is.null(vals$item) && !is.null(vals$label)) {
    labs <- as.character(vals$label)
    names(labs) <- as.character(vals$item)
    return(labs)
  }
  if (!is.null(names(vals))) {
    labs <- vapply(vals, function(x) {
      if (is.list(x)) as.character(x[[1]])[1] else as.character(x)[1]
    }, character(1))
    names(labs) <- names(vals)
    return(labs)
  }
  
  # No reliable mapping
  NULL
}





# Building the Census API URL--------------------------------------------------
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



# Default labels we can fall back to if metadata is awkward
default_labels <- function(var) {
  if (var == "SEX")  return(c(`1`="Male", `2`="Female"))
  if (var == "SCHL") return(c(`16`="Regular high school diploma",
                              `21`="Bachelor's degree"))
  NULL
}

# Is the label map usable for these codes? (must have names, and at least one match)
valid_labs <- function(codes_chr, labs_named) {
  if (is.null(labs_named) || is.null(names(labs_named))) return(FALSE)
  nm2    <- sub("^0+", "", names(labs_named))
  codes2 <- sub("^0+", "", codes_chr)
  any(codes2 %in% nm2)
}

# turn raw strings into useful R columns---------------------------------
coerce_columns <- function(df, year, numeric_vars, cat_vars, geo) {
  meta <- get_variables_metadata(year)
  
  # 1) plain numeric variables (JWAP/JWDP handled below)
  plain_numeric <- setdiff(numeric_vars, c("JWAP","JWDP"))
  for (v in plain_numeric) {
    if (v %in% names(df)) df[[v]] <- suppressWarnings(as.numeric(df[[v]]))
  }
  
  # 2) time variables -> numeric midpoints (prefer labels; fallback numeric)
  for (tv in intersect(c("JWAP","JWDP"), numeric_vars)) {
    if (tv %in% names(df)) {
      labs_raw <- label_lookup(meta, tv)
      labs     <- if (valid_labs(df[[tv]], labs_raw)) labs_raw else default_labels(tv)
      
      lbl <- map_labels(as.character(df[[tv]]), labs)
      if (all(is.na(lbl))) {
        df[[tv]] <- suppressWarnings(as.numeric(df[[tv]]))  # fallback if still no match
      } else {
        df[[tv]] <- vapply(lbl, midpoint_from_label, numeric(1))
      }
    }
  }
  
  # 3) categorical variables -> factors with readable labels
  for (cv in cat_vars) {
    if (cv %in% names(df)) {
      labs_raw <- label_lookup(meta, cv)
      labs     <- if (valid_labs(df[[cv]], labs_raw)) labs_raw else default_labels(cv)
      
      lbl <- map_labels(as.character(df[[cv]]), labs)
      if (all(is.na(lbl))) {
        df[[cv]] <- factor(df[[cv]])              # keep codes if we truly can’t map
      } else {
        df[[cv]] <- factor(lbl, levels = unique(unname(labs)))
      }
    }
  }
  
  # 4) weights numeric
  if ("PWGTP" %in% names(df)) df$PWGTP <- suppressWarnings(as.numeric(df$PWGTP))
  
  # 5) label the geography field too (if present)
  geo_field <- GEO_FIELD[[geo]]
  if (!is.na(geo_field) && geo_field %in% names(df)) {
    labs_raw <- label_lookup(meta, geo_field)
    labs     <- if (valid_labs(df[[geo_field]], labs_raw)) labs_raw else default_labels(geo_field)
    
    if (!is.null(labs)) {
      lbl <- map_labels(as.character(df[[geo_field]]), labs)
      if (all(is.na(lbl))) {
        df[[geo_field]] <- factor(df[[geo_field]])
      } else {
        df[[geo_field]] <- factor(lbl, levels = unique(unname(labs)))
      }
    } else {
      df[[geo_field]] <- factor(df[[geo_field]])
    }
  }
  
  df
}

# pulling one year of PUMS--------------------------------- 
pums_get_one_year <- function(year = 2022,
                              numeric_vars = c("AGEP"),
                              cat_vars = c("SEX"),
                              geo = "All",
                              subset = NULL) {
  
  # 1) validate inputs
  check_inputs(year, numeric_vars, cat_vars, geo, subset)
  
  # 2) build URL (PWGTP auto-added in build_url)
  get_vars <- unique(c(numeric_vars, cat_vars))
  subset_codes <- if (is.null(subset)) character(0) else subset
  url <- build_url(year, get_vars, geo, subset_codes)
  
  # 3) call the API
  resp <- httr::GET(url)
  if (httr::http_error(resp)) {
    stop("API request failed. Status: ", httr::status_code(resp))
  }
  
  # 4) parse JSON (first row = header)
  txt <- httr::content(resp, as = "text", encoding = "UTF-8")
  arr <- jsonlite::fromJSON(txt, simplifyVector = TRUE)
  if (NROW(arr) < 2) stop("API returned no data for these parameters.")
  
  header <- arr[1, ]
  dat <- tibble::as_tibble(as.data.frame(arr[-1, , drop = FALSE],
                                         stringsAsFactors = FALSE))
  names(dat) <- header
  
  # 5) coerce + label
  dat <- coerce_columns(dat, year, numeric_vars, cat_vars, geo)
  
  # 6) tag and return
  as_census(dat)
}


# pulling multiple years (loop + bind rows)------------------------
pums_get_multi_year <- function(years,
                                numeric_vars = c("AGEP"),
                                cat_vars = c("SEX"),
                                geo = "All",
                                subset = NULL) {
  
  if (!is.numeric(years) || any(!(years %in% ALLOWED_YEARS))) {
    stop("all years must be in 2010–2022.")
  }
  
  parts <- vector("list", length(years))
  for (i in seq_along(years)) {
    y <- years[i]
    one <- pums_get_one_year(y, numeric_vars, cat_vars, geo, subset)
    one$year <- y
    parts[[i]] <- one
  }
  
  out <- dplyr::bind_rows(parts)
  as_census(out)
}

