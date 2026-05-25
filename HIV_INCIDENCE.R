################################################################################
# ANC RECENCY / INCIDENCE ANALYSIS
# Protocol-compliant HIV incidence estimation using RITA algorithm
################################################################################
#
# PURPOSE:
#   Estimate HIV incidence among pregnant women attending antenatal care (ANC)
#   using recency testing and the RITA (Recent Infection Testing Algorithm).
#
# KEY OUTPUTS:
#   1. HIV prevalence among known serostatus at ANC1
#   2. RITA classification of HIV+ individuals (Recent vs Long-term)
#   3. Proportion RITA recent among at-risk population
#   4. HIV incidence estimates with bootstrap confidence intervals
#   5. Statistical comparisons across geographic strata
#
# METHODOLOGY:
#   - RITA Algorithm: LAg RECENT + VL >= 1000 => RECENT, else LONGTERM
#   - Proportion RITA Recent: R / (HIV-negative + R)
#   - Incidence: Uses inctools::incprops with protocol-specified parameters
#   - MDRI = 130 days (95% CI: 118-142)
#   - FRR = 0.00 (false recent rate)
#   - Time cutoff (BigT) = 365 days
#
# REQUIRED INPUT DATA:
#   
#   1. RECENCY DATA (individual-level):
#      - id: Participant ID (first 2 letters = county code)
#      - preliinary_lag: LAg assay result ("RECENT" or "LONGTERM")
#      - vl_result: Viral load value (numeric)
#      - age/age_group: (optional) For age stratification
#
#   2. ANC DATA (county aggregates):
#      - county: County name
#      - attend_known_serostatus: Total women tested at ANC1
#      - total_new_positive: Newly diagnosed HIV+
#      - total_known_pos: Known HIV+ at enrollment
#      - age_group: (optional) For age stratification
#
# DEPENDENCIES:
#   Required: inctools (v1.0.15+), dplyr, tidyr, readxl, stringr, purrr, readr
#   Optional: janitor (for automatic column name cleaning)
#

################################################################################


################################################################################
# SECTION 0: USER SETTINGS AND CONFIGURATION
################################################################################

# -----------------------------------------------------------------------------
# File Paths
# -----------------------------------------------------------------------------
# Update these paths to point to your actual data files


# -----------------------------------------------------------------------------
# Protocol Parameters for Incidence Estimation
# -----------------------------------------------------------------------------
# These parameters are specified by the surveillance protocol and should not
# be changed unless the protocol is updated

# Mean Duration of Recent Infection (MDRI) in days
MDRI_DAYS   <- 130

# 95% Confidence Interval for MDRI (used to derive RSE_MDRI)
MDRI_CI     <- c(118, 142)

# False Recent Rate (proportion of long-term infections misclassified as recent)
FRR         <- 0.00

# Relative Standard Error for FRR (required by inctools even when FRR=0)
RSE_FRR     <- 0.20

# Time cutoff for recency definition (BigT) in days
BIGT_DAYS   <- 365

# Significance level for confidence intervals
ALPHA       <- 0.05

# Number of bootstrap iterations for CI estimation
BS_COUNT    <- 10000

# -----------------------------------------------------------------------------
# Numerical Stability Settings
# -----------------------------------------------------------------------------
# These settings help prevent computational issues in bootstrap procedures

# Minimum RSE for bootstrap (prevents near-singular covariance matrices)
MIN_RSE_FOR_BOOT <- 0.05

# Maximum RSE (prevents extreme variance estimates)
MAX_RSE          <- 1.00

# Protocol precision threshold for highlighting unstable incidence estimates
HIGH_RSE_THRESHOLD <- 0.25

# -----------------------------------------------------------------------------
# Stratification and Filtering Options
# -----------------------------------------------------------------------------

# Columns to use for stratification
# Must exist in data OR will be created as "All"
# Options: c("county", "age_group") or c("county") for county-only analysis
# STRATA_COLS <- c("county", "age_group")
# 
# # Optional: Filter to Adolescent Girls and Young Women (AGYW) only
# FILTER_TO_AGYW <- FALSE
# AGYW_MIN_AGE <- 15
# AGYW_MAX_AGE <- 24

################################################################################
# SECTION 1: PACKAGE LOADING
################################################################################

#' Load Required Package
#'
#' Loads a package and provides informative error if not installed
#'
#' @param pkg Character string of package name
#' @return NULL (loads package into environment)
load_pkg <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(sprintf(
      "Package '%s' is required but not installed.\nInstall with: install.packages('%s')",
      pkg, pkg
    ))
  }
  suppressPackageStartupMessages(library(pkg, character.only = TRUE))
}

# Load required packages
load_pkg("dplyr")      # Data manipulation
load_pkg("tidyr")      # Data tidying
load_pkg("readxl")     # Excel file reading
load_pkg("stringr")    # String manipulation
load_pkg("purrr")      # Functional programming
load_pkg("readr")      # Data parsing
load_pkg("inctools")   # Incidence estimation

# Check for optional janitor package (nice-to-have for clean_names)
HAVE_JANITOR <- requireNamespace("janitor", quietly = TRUE)

################################################################################
# SECTION 2: HELPER FUNCTIONS
################################################################################

#' Clean Column Names Safely
#'
#' Standardizes column names to lowercase with underscores.
#' Uses janitor::clean_names() if available, otherwise manual cleaning.
#'
#' @param df Data frame
#' @return Data frame with cleaned column names
#' @examples
#' df <- data.frame("First Name" = 1, "Age (years)" = 2)
#' clean_names_safe(df)  # Returns: first_name, age_years
clean_names_safe <- function(df) {
  if (HAVE_JANITOR) {
    return(janitor::clean_names(df))
  }
  
  # Manual cleaning if janitor not available
  nm <- names(df)
  nm <- tolower(nm)                    # Convert to lowercase
  nm <- gsub("[^a-z0-9]+", "_", nm)    # Replace non-alphanumeric with underscore
  nm <- gsub("^_|_$", "", nm)          # Remove leading/trailing underscores
  names(df) <- nm
  df
}

#' Find First Existing Column
#'
#' Searches for the first column name that exists in the data frame from
#' a list of candidates. Useful for handling different naming conventions.
#'
#' @param df Data frame to search
#' @param candidates Character vector of candidate column names
#' @param required Logical; if TRUE, stops with error if no match found
#' @return Character string of first matching column name, or NA if not required
#' @examples
#' first_existing(df, c("county", "county_name", "location"))
first_existing <- function(df, candidates, required = TRUE) {
  hit <- candidates[candidates %in% names(df)]
  
  if (length(hit) == 0) {
    if (required) {
      stop("Could not find any of these columns: ", paste(candidates, collapse = ", "))
    }
    return(NA_character_)
  }
  
  hit[1]
}

#' Standardize Text Values
#'
#' Trims whitespace and converts to uppercase for consistent matching
#'
#' @param x Character vector
#' @return Character vector with standardized text
#' @examples
#' std_text("  recent  ") # Returns: "RECENT"
std_text <- function(x) {
  x %>%
    as.character() %>%
    stringr::str_trim() %>%
    toupper()
}

first_non_missing_chr <- function(x) {
  vals <- x %>%
    as.character() %>%
    stringr::str_trim()
  vals <- vals[!is.na(vals) & vals != ""]

  if (length(vals) == 0) {
    return(NA_character_)
  }

  vals[1]
}

normalize_facility_name <- function(x) {
  x %>%
    as.character() %>%
    tolower() %>%
    stringr::str_replace_all("&", " and ") %>%
    stringr::str_replace_all("[^a-z0-9 ]+", " ") %>%
    stringr::str_replace_all(
      "\\b(hospital|dispensary|health center|health centre|medical centre|medical center|clinic|county referral|sub county hospital|sub county|county|district hospital|district|level ?[23456]|maternity|centre|center)\\b",
      " "
    ) %>%
    stringr::str_squish()
}

build_name_match_suggestions <- function(unmatched_df, ref_df, top_n = 3) {
  if (nrow(unmatched_df) == 0 || nrow(ref_df) == 0) {
    return(tibble::tibble())
  }

  purrr::map_dfr(seq_len(nrow(unmatched_df)), function(i) {
    row <- unmatched_df[i, , drop = FALSE]
    recency_name_norm <- normalize_facility_name(row$recency_facility_name)
    recency_county_std <- gsub(" County$", "", as.character(row$recency_county))

    candidate_pool <- ref_df
    same_county_pool <- ref_df %>%
      dplyr::filter(khis_county_std == recency_county_std)

    if (nrow(same_county_pool) > 0) {
      candidate_pool <- same_county_pool
    }

    candidate_pool <- candidate_pool %>%
      dplyr::mutate(
        name_distance = utils::adist(recency_name_norm, khis_name_norm)[1, ],
        county_match = khis_county_std == recency_county_std
      ) %>%
      dplyr::arrange(name_distance, dplyr::desc(county_match), facilityname) %>%
      dplyr::slice_head(n = top_n)

    suggestion_row <- tibble::tibble(
      recency_mfl = row$mfl,
      recency_facility_name = row$recency_facility_name,
      recency_county = row$recency_county,
      recency_records_all = row$recency_records_all,
      N_testR_site = row$N_testR_site,
      N_R_site = row$N_R_site
    )

    for (j in seq_len(top_n)) {
      if (j <= nrow(candidate_pool)) {
        suggestion_row[[paste0("suggestion_", j, "_mfl")]] <- candidate_pool$mfl[j]
        suggestion_row[[paste0("suggestion_", j, "_facilityname")]] <- candidate_pool$facilityname[j]
        suggestion_row[[paste0("suggestion_", j, "_county")]] <- candidate_pool$County[j]
        suggestion_row[[paste0("suggestion_", j, "_subcounty")]] <- candidate_pool$SubCounty[j]
        suggestion_row[[paste0("suggestion_", j, "_source")]] <- candidate_pool$reference_source[j]
        suggestion_row[[paste0("suggestion_", j, "_name_distance")]] <- candidate_pool$name_distance[j]
      } else {
        suggestion_row[[paste0("suggestion_", j, "_mfl")]] <- NA_real_
        suggestion_row[[paste0("suggestion_", j, "_facilityname")]] <- NA_character_
        suggestion_row[[paste0("suggestion_", j, "_county")]] <- NA_character_
        suggestion_row[[paste0("suggestion_", j, "_subcounty")]] <- NA_character_
        suggestion_row[[paste0("suggestion_", j, "_source")]] <- NA_character_
        suggestion_row[[paste0("suggestion_", j, "_name_distance")]] <- NA_real_
      }
    }

    suggestion_row
  })
}

#' Convert to Numeric Safely
#'
#' Robustly converts values to numeric, handling special cases like:
#' - "<20" (viral load below detection)
#' - "1,234" (numbers with comma separators)
#' - Text with embedded numbers
#'
#' @param x Vector to convert
#' @return Numeric vector
#' @examples
#' to_numeric_safe(c("<20", "1,234", "456"))  # Returns: c(20, 1234, 456)
to_numeric_safe <- function(x) {
  readr::parse_number(as.character(x))
}

#' Normalize Viral Load Values
#'
#' Converts viral load values with symbols into numeric values:
#' - values starting with "<" are set to 0
#' - values starting with ">" are capped at 100000
#' - other values are parsed numerically
#'
#' @param x Viral load vector (character/numeric)
#' @return Numeric vector suitable for RITA thresholding
normalize_vl <- function(x) {
  raw <- stringr::str_trim(as.character(x))
  raw_upper <- toupper(raw)

  below_limit <- grepl("^\\s*<", raw) |
    grepl("LDL|BELOW DETECTION|NOT DETECTED|TARGET NOT DETECTED|UNDETECTABLE", raw_upper)
  above_limit <- grepl("^\\s*>", raw)

  numeric_token <- stringr::str_extract(
    stringr::str_replace_all(raw, ",", ""),
    "\\d+(?:\\.\\d+)?"
  )
  parsed <- suppressWarnings(as.numeric(numeric_token))

  parsed[below_limit] <- 0
  parsed[above_limit] <- 100000

  parsed
}

pull_optional_chr <- function(df, col) {
  if (is.na(col)) {
    return(rep(NA_character_, nrow(df)))
  }
  as.character(df[[col]])
}

pull_optional_num <- function(df, col) {
  if (is.na(col)) {
    return(rep(NA_real_, nrow(df)))
  }
  suppressWarnings(as.numeric(df[[col]]))
}

#' Derive Relative Standard Error from Confidence Interval
#'
#' Calculates RSE from a 95% CI assuming approximate normality:
#' SE ≈ (upper - lower) / (2 * 1.96)
#'
#' @param est Point estimate
#' @param ci_low Lower bound of 95% CI
#' @param ci_up Upper bound of 95% CI
#' @return Relative standard error (SE/estimate)
rse_from_ci <- function(est, ci_low, ci_up) {
  se <- (ci_up - ci_low) / (2 * stats::qnorm(0.975))
  se / est
}

#' Calculate Binomial Relative Standard Error
#'
#' Computes RSE for a proportion accounting for design effects
#'
#' @param p Proportion (between 0 and 1)
#' @param n Sample size
#' @param de Design effect (default = 1 for simple random sampling)
#' @param min_rse Minimum RSE to return (for numerical stability)
#' @param max_rse Maximum RSE to return (prevents extreme values)
#' @return Relative standard error, or NA if not computable
#'
#' @details
#' Returns NA if:
#' - Proportion is 0 or 1 (RSE undefined at boundaries)
#' - Sample size is 0 or negative
#' - Inputs are missing
rse_binom <- function(p, n, de = 1, min_rse = 0, max_rse = 1) {
  # Check for invalid inputs
  if (is.na(p) || is.na(n) || n <= 0) return(NA_real_)
  if (p <= 0 || p >= 1) return(NA_real_)  # RSE undefined at boundaries
  
  # Calculate standard error
  se <- sqrt((p * (1 - p) / n) * de)
  rse <- se / p
  
  # Apply bounds
  rse <- max(rse, min_rse)
  rse <- min(rse, max_rse)
  
  rse
}

#' Ensure Stratification Columns Exist
#'
#' Creates missing stratification columns and fills with "All" or "Unknown"
#'
#' @param df Data frame
#' @param strata_cols Character vector of required column names
#' @return Data frame with all stratification columns present
ensure_strata <- function(df, strata_cols) {
  out <- df
  
  for (cc in strata_cols) {
    # Create column if missing
    if (!cc %in% names(out)) {
      out[[cc]] <- "All"
    }
    
    # Fill missing/empty values
    out[[cc]] <- ifelse(
      is.na(out[[cc]]) | out[[cc]] == "",
      "Unknown",
      as.character(out[[cc]])
    )
  }
  
  out
}

#' Add Overall Summary Row
#'
#' Creates an "Overall" row by summing across all strata
#'
#' @param df Data frame with stratified counts
#' @param strata_cols Character vector of stratification column names
#' @param sum_cols Character vector of columns to sum
#' @return Data frame with overall row prepended
#'
#' @examples
#' # Add overall row summing N_tested and N_positive across counties
#' add_overall(df, strata_cols = "county", sum_cols = c("N_tested", "N_positive"))
add_overall <- function(df, strata_cols, sum_cols) {
  # Sum all numeric columns
  overall <- df %>%
    summarise(across(all_of(sum_cols), ~sum(.x, na.rm = TRUE)), .groups = "drop")
  
  # Add stratification columns with "Overall" value
  for (col in strata_cols) {
    overall[[col]] <- "Overall"
  }
  
  # Return with overall row first
  bind_rows(overall, df)
}

################################################################################
# SECTION 3: DATA LOADING AND STANDARDIZATION
################################################################################

# -----------------------------------------------------------------------------
# 3.4: Process Recency Data
#read lag data from API
#------------------------------------------------------------------
library(httr)
library(dplyr)

url <- Sys.getenv("RECENCY_API_URL", unset = "https://eiddash.nascop.org/api/recency/lag")
if (identical(url, "")) {
  stop("Missing RECENCY_API_URL environment variable. Set it in .Renviron before running this script.")
}

recency_api_key <- Sys.getenv("RECENCY_API_KEY")
if (identical(recency_api_key, "")) {
  stop("Missing RECENCY_API_KEY environment variable. Set it before running this script.")
}

# Send request with API key
response <- GET(
  url,
  add_headers(apikey = recency_api_key)
)

# Convert response to text
json_data <- content(response, as = "text", encoding = "UTF-8")

if (httr::status_code(response) < 200 || httr::status_code(response) >= 300) {
  stop(
    sprintf(
      "Recency API request failed [%s] for %s. Response: %s",
      httr::status_code(response),
      url,
      substr(json_data, 1, 500)
    )
  )
}

# Parse JSON
recency_raw <- tryCatch(
  jsonlite::fromJSON(json_data, flatten = TRUE),
  error = function(e) {
    stop(sprintf("Failed to parse recency API response for %s: %s", url, e$message))
  }
)

recency_raw <- clean_names_safe(as.data.frame(recency_raw))

col_vl_date <- first_existing(
  recency_raw,
  c("vl_date_collected", "date_collected", "sample_date"),
  required = FALSE
)
col_birth_year <- first_existing(
  recency_raw,
  c("dob", "birth_year", "year_of_birth"),
  required = FALSE
)

if (!"age" %in% names(recency_raw) && !is.na(col_vl_date) && !is.na(col_birth_year)) {
  recency_raw$age <- suppressWarnings(
    as.numeric(substr(as.character(recency_raw[[col_vl_date]]), 1, 4)) -
      as.numeric(recency_raw[[col_birth_year]])
  )
}


cat("Extracting county from participant IDs...\n")

# recency_raw <- recency_raw %>%
#   mutate(
#     # Extract first 2 characters from ID as county code
#     county_code = substr(id, 1, 2),
#     
#     # Map county codes to full county names
#     county = case_when(
#       county_code == "KI" ~ "Kisii",
#       county_code == "KL" ~ "Kilifi",
#       county_code == "KM" ~ "Kiambu",
#       county_code == "KS" ~ "Kisumu",
#       county_code == "MK" ~ "Machakos",
#       county_code == "MS" ~ "Mombasa",
#       county_code == "NB" ~ "Nairobi",
#       county_code == "NK" ~ "Nakuru",
#       TRUE ~ "Unknown"  # Default for unrecognized codes
#     )
#   )

# Display county distribution
cat("  County distribution:\n")
print(table(recency_raw$county, useNA = "ifany"))

# -----------------------------------------------------------------------------
# 3.3: Identify Key Columns in Recency Data
# -----------------------------------------------------------------------------
# Searches for columns with different possible names to handle various
# naming conventions across datasets

cat("Identifying key columns in recency data...\n")

col_prelim_lag <- first_existing(
  recency_raw,
  c("preliminary_lag", "preliinary_lag", "lag_result", "lag_status"),
  required = FALSE
)

col_lag_odn <- first_existing(
  recency_raw,
  c("lag_final_odn", "final_odn", "lag_odn", "odn"),
  required = FALSE
)

col_lag_status <- first_existing(
  recency_raw,
  c("lag_status", "lag_final_status", "status"),
  required = FALSE
)

col_lag_result <- first_existing(
  recency_raw,
  c("lag_result", "final_lag_result", "final_result", "rita_result"),
  required = FALSE
)

col_vl <- first_existing(
  recency_raw,
  c("vl_result", "viral_load", "viral_load_result", "vl")
)

col_county_r <- first_existing(
  recency_raw,
  c("county", "county_name"),
  required = FALSE
)

col_site_mfl <- first_existing(
  recency_raw,
  c("facility_mfl_code", "mfl", "mfl_code", "facility_code"),
  required = FALSE
)

col_site_name <- first_existing(
  recency_raw,
  c("facility_name", "facility", "site_name", "health_facility", "clinic"),
  required = FALSE
)

col_age_r <- first_existing(
  recency_raw,
  c("age", "age_years", "ageyrs"),
  required = FALSE
)

col_agegrp_r <- first_existing(
  recency_raw,
  c("age_group", "agegroup"),
  required = FALSE
)

cat("  Found columns:\n")
cat("    LAg result:", ifelse(is.na(col_prelim_lag), "NOT FOUND", col_prelim_lag), "\n")
cat("    LAg ODn:", ifelse(is.na(col_lag_odn), "NOT FOUND", col_lag_odn), "\n")
cat("    LAg status:", ifelse(is.na(col_lag_status), "NOT FOUND", col_lag_status), "\n")
cat("    LAg final result:", ifelse(is.na(col_lag_result), "NOT FOUND", col_lag_result), "\n")
cat("    Viral load:", col_vl, "\n")
cat("    County:", ifelse(is.na(col_county_r), "NOT FOUND", col_county_r), "\n")
cat("    Site MFL:", ifelse(is.na(col_site_mfl), "NOT FOUND", col_site_mfl), "\n")
cat("    Site name:", ifelse(is.na(col_site_name), "NOT FOUND", col_site_name), "\n")
cat("    Age:", ifelse(is.na(col_age_r), "NOT FOUND", col_age_r), "\n")

lag_prelim_values <- std_text(pull_optional_chr(recency_raw, col_prelim_lag))
lag_status_values <- std_text(pull_optional_chr(recency_raw, col_lag_status))
lag_result_values <- std_text(pull_optional_chr(recency_raw, col_lag_result))
lag_odn_values <- pull_optional_num(recency_raw, col_lag_odn)

recent_labels <- c("RECENT", "R", "PRELIMINARY RECENT")
longterm_labels <- c("LONGTERM", "LT", "NOT RECENT", "NON-RECENT", "COMPLETED")
pending_labels <- c("PENDING", "IN PROGRESS")

recency_raw <- recency_raw %>%
  mutate(
    nnew_vl = normalize_vl(.data[[col_vl]]),
    lag_prelim_std = lag_prelim_values,
    lag_status_std = lag_status_values,
    lag_result_std = lag_result_values,
    lag_odn_value = lag_odn_values
  )

recency <- recency_raw %>%
  mutate(
    # Convert viral load to numeric if needed (but you already have nnew_vl)
    county = if (!is.na(col_county_r)) as.character(.data[[col_county_r]]) else "Unknown",
    site_mfl = if (!is.na(col_site_mfl)) readr::parse_number(as.character(.data[[col_site_mfl]])) else NA_real_,
    site_name = if (!is.na(col_site_name)) as.character(.data[[col_site_name]]) else NA_character_,
    age = if (!is.na(col_age_r)) suppressWarnings(as.numeric(.data[[col_age_r]])) else NA_real_,
  ) %>%
  
  # Create age_group if missing but age exists
  mutate(
    age_group = dplyr::case_when(
      is.na(age) ~ "All",
      TRUE ~ as.character(cut(
        age,
        breaks = c(-Inf, 14, 19, 24, 29, 34, 39, 44, 49, Inf),
        labels = c("<15", "15-19", "20-24", "25-29", "30-34", 
                   "35-39", "40-44", "45-49", "50+"),
        right = TRUE
      ))
    )
  ) %>%
  
  # Apply RITA Classification Algorithm
  mutate(
    has_lag_report = (
      (!is.na(lag_result_std) & lag_result_std != "") |
        (!is.na(lag_status_std) & lag_status_std != "") |
        (!is.na(lag_prelim_std) & lag_prelim_std != "") |
        !is.na(lag_odn_value)
    ),
    rita_final = dplyr::case_when(
      # Recent: LAg recent (ODn ≤ 1.5) AND viral load >= 1000
      !is.na(lag_odn_value) & lag_odn_value <= 1.5 &
        !is.na(nnew_vl) & nnew_vl >= 1000 ~ "RECENT",
      
      # Long-term: LAg recent (ODn ≤ 1.5) but VL < 1000
      !is.na(lag_odn_value) & lag_odn_value <= 1.5 &
        !is.na(nnew_vl) & nnew_vl < 1000 ~ "LONGTERM",

      ((lag_result_std %in% recent_labels) |
         (lag_status_std %in% recent_labels) |
         (lag_prelim_std %in% recent_labels)) &
        !is.na(nnew_vl) & nnew_vl >= 1000 ~ "RECENT",

      ((lag_result_std %in% recent_labels) |
         (lag_status_std %in% recent_labels) |
         (lag_prelim_std %in% recent_labels)) &
        !is.na(nnew_vl) & nnew_vl < 1000 ~ "LONGTERM",
      
      (lag_status_std %in% longterm_labels) |
        (lag_result_std %in% longterm_labels) |
        (lag_prelim_std %in% longterm_labels) ~ "LONGTERM",
      
      # Keep pending/other statuses
      (lag_status_std %in% pending_labels) |
        (lag_result_std %in% pending_labels) |
        (lag_prelim_std %in% pending_labels) ~ "PENDING",
      
      # Default: keep original if nothing else matches
      TRUE ~ dplyr::coalesce(lag_result_std, lag_status_std, lag_prelim_std)
    )
  )
summary(recency$rita_final)

lag_reported_counties <- recency %>%
  dplyr::filter(has_lag_report, !is.na(county), county != "", county != "Unknown") %>%
  dplyr::distinct(county) %>%
  dplyr::arrange(county)

cat("  Counties with reported LAg result:", nrow(lag_reported_counties), "\n")

recency_site_summary <- recency %>%
  dplyr::filter(!is.na(site_mfl)) %>%
  dplyr::group_by(site_mfl) %>%
  dplyr::summarise(
    recency_facility_name = first_non_missing_chr(site_name),
    recency_county = first_non_missing_chr(county),
    recency_records_all = dplyr::n(),
    N_testR_site = sum(rita_final %in% c("LONGTERM", "RECENT"), na.rm = TRUE),
    N_R_site = sum(rita_final == "RECENT", na.rm = TRUE),
    .groups = "drop"
  )

matched_preart_vl_site_mfl <- recency_site_summary %>%
  dplyr::pull(site_mfl) %>%
  unique()

#--------------------------------------------------------------
#analytics data comes from KHIS pulling
#--------------------------------------------------------------
# analytics_rds_path <- Sys.getenv("ANC_ANALYTICS_RDS", unset = file.path("output", "analytics_latest.rds"))
BASE_DIR <- Sys.getenv("KHIS_BASE_DIR", unset = ".")
analytics_rds_path <- file.path(BASE_DIR, "output", "analytics_latest.rds")
if (!exists("analytics")) {
  if (!file.exists(analytics_rds_path)) {
    stop(
      paste0(
        "'analytics' object not found and persisted analytics file is missing at: ",
        analytics_rds_path,
        ". Run KHIS_AUTO_DATA_PULL.R first or ensure the analytics_latest.rds file exists in the output folder"
      )
    )
  }

  analytics <- readRDS(analytics_rds_path)
  cat("Loaded analytics dataset from:", analytics_rds_path, "\n")
}

analytics <- dplyr::ungroup(analytics)
analytics <- analytics %>%
  dplyr::mutate(mfl = readr::parse_number(as.character(mfl)))

khis_name_reference <- analytics %>%
  dplyr::filter(!is.na(mfl)) %>%
  dplyr::transmute(
    mfl,
    County,
    SubCounty,
    facilityname,
    official_name = facilityname,
    closed = NA_character_,
    reference_source = "KHIS_analytics",
    khis_county_std = gsub(" County$", "", as.character(County)),
    khis_name_norm = normalize_facility_name(facilityname)
  ) %>%
  dplyr::distinct(mfl, .keep_all = TRUE)

mfl_lookup_path <- Sys.getenv(
  "MFL_LOOKUP_PATH",
  unset = file.path("documents", "MFLCODE_FacilityExportMaterialList_26022026_Recent.xlsx")
)

if (!file.exists(mfl_lookup_path)) {
  legacy_lookup_path <- Sys.getenv(
    "MFL_LOOKUP_CSV",
    unset = file.path("documents", "MFLCODE_KHIS_latest_26022026.xlsx.csv")
  )

  if (file.exists(legacy_lookup_path)) {
    mfl_lookup_path <- legacy_lookup_path
  }
}

mfl_master_reference <- tibble::tibble(
  mfl = numeric(),
  County = character(),
  SubCounty = character(),
  facilityname = character(),
  official_name = character(),
  closed = character(),
  reference_source = character(),
  khis_county_std = character(),
  khis_name_norm = character()
)

if (file.exists(mfl_lookup_path)) {
  lookup_ext <- tolower(tools::file_ext(mfl_lookup_path))

  mfl_lookup <- if (lookup_ext %in% c("xlsx", "xls")) {
    readxl::read_excel(mfl_lookup_path)
  } else {
    readr::read_csv(mfl_lookup_path, show_col_types = FALSE)
  }

  mfl_lookup <- clean_names_safe(mfl_lookup)

  col_mfl_lookup <- first_existing(mfl_lookup, c("code", "mfl"), required = FALSE)
  col_county_lookup <- first_existing(mfl_lookup, c("county"), required = FALSE)
  col_subcounty_lookup <- first_existing(mfl_lookup, c("sub_county", "subcounty"), required = FALSE)
  col_name_lookup <- first_existing(mfl_lookup, c("officialname", "official_name", "name"), required = FALSE)
  col_official_lookup <- first_existing(mfl_lookup, c("officialname", "official_name"), required = FALSE)
  col_closed_lookup <- first_existing(mfl_lookup, c("closed", "operation_status"), required = FALSE)

  mfl_master_reference <- mfl_lookup %>%
    dplyr::transmute(
      mfl = readr::parse_number(as.character(.data[[col_mfl_lookup]])),
      County = if (!is.na(col_county_lookup)) as.character(.data[[col_county_lookup]]) else NA_character_,
      SubCounty = if (!is.na(col_subcounty_lookup)) as.character(.data[[col_subcounty_lookup]]) else NA_character_,
      facilityname = if (!is.na(col_name_lookup)) as.character(.data[[col_name_lookup]]) else NA_character_,
      official_name = if (!is.na(col_official_lookup)) as.character(.data[[col_official_lookup]]) else NA_character_,
      closed = if (!is.na(col_closed_lookup)) as.character(.data[[col_closed_lookup]]) else NA_character_,
      reference_source = "MFL_master",
      khis_county_std = gsub(" County$", "", if (!is.na(col_county_lookup)) as.character(.data[[col_county_lookup]]) else NA_character_),
      khis_name_norm = normalize_facility_name(if (!is.na(col_name_lookup)) as.character(.data[[col_name_lookup]]) else NA_character_)
    ) %>%
    dplyr::filter(!is.na(mfl)) %>%
    dplyr::distinct(mfl, .keep_all = TRUE)
}

facility_name_reference <- dplyr::bind_rows(
  khis_name_reference,
  mfl_master_reference
) %>%
  dplyr::distinct(reference_source, mfl, .keep_all = TRUE)

all_anc1_prevalence <- analytics %>%
  dplyr::group_by(County) %>%
  dplyr::summarise(
    All_ANC1_N_Known = sum(total_known_serostatus_anc1, na.rm = TRUE),
    All_ANC1_N_HIV_Positive = sum(total_positive_anc1, na.rm = TRUE),
    All_ANC1_HIV_Prevalence = dplyr::if_else(
      All_ANC1_N_Known > 0,
      All_ANC1_N_HIV_Positive / All_ANC1_N_Known,
      NA_real_
    ),
    .groups = "drop"
  ) %>%
  dplyr::mutate(county = gsub(" County$", "", County))

if (is.na(col_site_mfl)) {
  stop(
    paste0(
      "Recency data does not contain a facility MFL column, so ANC denominators ",
      "cannot be restricted to pre-ART VL-contributing sites."
    )
  )
}

if (length(matched_preart_vl_site_mfl) == 0) {
  stop(
    paste0(
      "No pre-ART VL-contributing sites were found in the recency feed, ",
      "so KHIS ANC denominators cannot be restricted at site level."
    )
  )
}

khis_preart_vl_sites <- analytics %>%
  dplyr::filter(!is.na(mfl), mfl %in% matched_preart_vl_site_mfl) %>%
  dplyr::left_join(recency_site_summary, by = c("mfl" = "site_mfl")) %>%
  dplyr::select(
    County,
    SubCounty,
    facilityname,
    uid_khis,
    mfl,
    recency_facility_name,
    recency_county,
    recency_records_all,
    N_testR_site,
    N_R_site,
    total_attending_anc1,
    total_tested_anc1,
    total_known_pos,
    total_new_pos_anc1,
    total_positive_anc1,
    total_known_serostatus_anc1
  ) %>%
  dplyr::arrange(County, SubCounty, facilityname)

unmatched_preart_vl_sites <- recency_site_summary %>%
  dplyr::filter(!site_mfl %in% unique(khis_preart_vl_sites$mfl)) %>%
  dplyr::transmute(
    mfl = site_mfl,
    recency_facility_name,
    recency_county,
    recency_records_all,
    N_testR_site,
    N_R_site
  ) %>%
  dplyr::arrange(recency_county, recency_facility_name)

unmatched_preart_vl_site_suggestions <- build_name_match_suggestions(
  unmatched_preart_vl_sites,
  facility_name_reference,
  top_n = 3
) %>%
  dplyr::left_join(
    mfl_master_reference %>%
      dplyr::transmute(
        recency_mfl = mfl,
        master_exact_match = TRUE,
        master_exact_facilityname = facilityname,
        master_exact_official_name = official_name,
        master_exact_county = County,
        master_exact_subcounty = SubCounty,
        master_exact_closed = closed
      ),
    by = "recency_mfl"
  ) %>%
  dplyr::mutate(master_exact_match = dplyr::if_else(is.na(master_exact_match), FALSE, master_exact_match)) %>%
  dplyr::relocate(
    master_exact_match,
    master_exact_facilityname,
    master_exact_official_name,
    master_exact_county,
    master_exact_subcounty,
    master_exact_closed,
    .after = recency_county
  ) %>%
  dplyr::arrange(recency_county, recency_facility_name)

cat(
  "Restricting incidence HIV-negative denominator to",
  dplyr::n_distinct(khis_preart_vl_sites$mfl),
  "matched pre-ART VL-contributing KHIS ANC sites.\n"
)

has_protocol_anc1_cols <- all(
  c("total_new_pos_anc1", "total_positive_anc1", "total_known_serostatus_anc1") %in% names(analytics)
)

# Protocol note:
# - ANC1-only known serostatus is the protocol denominator.
# - PrevR and RSE_PrevR remain on the same protocol-consistent at-risk basis.
if (!has_protocol_anc1_cols) {
  warning(
    paste0(
      "ANC1 protocol summary columns were not found in analytics. ",
      "Protocol-reference ANC1 counts will not be available for workbook notes."
    )
  )
}

anc1_known_serostatus <- if ("total_known_serostatus_anc1" %in% names(analytics)) {
  analytics$total_known_serostatus_anc1
} else {
  NA_real_
}

anc1_positive <- if ("total_positive_anc1" %in% names(analytics)) {
  analytics$total_positive_anc1
} else {
  analytics$total_known_pos + analytics$total_new_pos_anc1
}

anc1_negative <- if ("total_negative_anc1" %in% names(analytics)) {
  analytics$total_negative_anc1
} else {
  anc1_known_serostatus - anc1_positive
}

cat("Using protocol ANC1-only prevalence inputs at county level.\n")
cat("Restricting incidence HIV-negative denominator to KHIS ANC sites with matched pre-ART VL contribution.\n")

full_anc_counts <- tibble::tibble(
  County = analytics$County,
  N_known = anc1_known_serostatus,
  N_new_pos = analytics$total_new_pos_anc1,
  N_known_pos = analytics$total_known_pos,
  N_H = anc1_positive
)

matched_anc_neg_counts <- khis_preart_vl_sites %>%
  dplyr::transmute(
    County,
    N_neg = total_known_serostatus_anc1 - total_positive_anc1
  ) %>%
  dplyr::group_by(County) %>%
  dplyr::summarise(N_neg = sum(N_neg, na.rm = TRUE), .groups = "drop")

library(dplyr)

anc_counts <- full_anc_counts %>%
  group_by(County) %>%
  summarise(across(c(N_known, N_new_pos, N_known_pos, N_H),
                   \(x) sum(x, na.rm = TRUE)), .groups = "drop") %>%
  dplyr::left_join(matched_anc_neg_counts, by = "County") %>%
  dplyr::mutate(N_neg = tidyr::replace_na(N_neg, 0))

#remove county from county name
anc_counts$county <- gsub(" County$", "", anc_counts$County)

# -----------------------------------------------------------------------------
# 4.2: Recency Counts Among HIV-Positive
# -----------------------------------------------------------------------------
# Only includes those with valid RITA classification

cat("Calculating recency counts...\n")

recency_counts <- recency %>%
  dplyr::filter(lag_status_std == "TESTED", !is.na(lag_result_std)) %>%
  group_by(across(all_of("county"))) %>%
  summarise(
    N_testR = n(),                                      # Number tested for recency in dashboard
    N_R = sum(rita_final == "RECENT", na.rm = TRUE),   # Number RITA recent
    .groups = "drop"
  )

cat("  Strata:", nrow(recency_counts), "\n")
cat("  Total tested for recency:", sum(recency_counts$N_testR), "\n")
cat("  Total recent infections:", sum(recency_counts$N_R), "\n")

vls_counts <- recency %>%
  dplyr::filter(!is.na(nnew_vl)) %>%
  dplyr::group_by(across(all_of("county"))) %>%
  dplyr::summarise(
    N_With_VL = dplyr::n(),
    N_VL_Suppressed = sum(nnew_vl < 1000, na.rm = TRUE),
    VLS = dplyr::if_else(
      N_With_VL > 0,
      N_VL_Suppressed / N_With_VL,
      NA_real_
    ),
    .groups = "drop"
  )

cat("  Counties with parseable VL for VLS:", nrow(vls_counts), "\n")
cat("  Total records with parseable VL:", sum(vls_counts$N_With_VL), "\n")
cat("  Total records with VL < 1000:", sum(vls_counts$N_VL_Suppressed), "\n")

# # Add overall summary row
# recency_counts <- add_overall(
#   recency_counts,
#   strata_cols = STRATA_COLS,
#   sum_cols = c("N_testR", "N_R")
# )

# -----------------------------------------------------------------------------
# 4.3: Merge ANC and Recency Data
# -----------------------------------------------------------------------------

cat("Merging ANC and recency data...\n")

analysis_df <- anc_counts %>%
  left_join(recency_counts, by = "county") %>%
  mutate(
    # Replace NA with 0 for strata with no recency testing
    N_testR = tidyr::replace_na(N_testR, 0L),
    N_R = tidyr::replace_na(N_R, 0L)
  )

# -----------------------------------------------------------------------------
# 4.4: Quality Control Flags
# -----------------------------------------------------------------------------

analysis_df <- analysis_df %>%
  mutate(
    # Flag if HIV-negative count is negative (data error)
    flag_neg_negative = N_neg < 0,
    
    # Flag if more recency tests than HIV+ individuals (data error)
    flag_recency_gt_pos = N_testR > N_H
  )

# Report any QC issues
n_flags <- sum(analysis_df$flag_neg_negative | analysis_df$flag_recency_gt_pos)
if (n_flags > 0) {
  cat("  WARNING:", n_flags, "strata with data quality issues\n")
  cat("  Review 'flag_neg_negative' and 'flag_recency_gt_pos' columns\n")
}

################################################################################
# SECTION 5: CALCULATE HIV PREVALENCE AND PROPORTION RITA RECENT
################################################################################

cat("\n=== CALCULATING PREVALENCE MEASURES ===\n")

analysis_df <- analysis_df %>%
  mutate(
    # HIV prevalence using the protocol ANC1-only known-serostatus denominator.
    PrevH = dplyr::if_else(N_known > 0, N_H / N_known, NA_real_),
    
    # Recency prevalence among HIV-positive tested for recency
    # Used as input for inctools (PrevR parameter)
    PrevR_HIVpos = dplyr::if_else(N_testR > 0, N_R / N_testR, NA_real_),
    
    # Protocol "Proportion RITA recent" among at-risk population
    # Denominator = HIV-negative + RITA recent
    # This is the key measure for surveillance
    PropRITA_AtRisk = dplyr::if_else(
      (N_neg + N_R) > 0,
      N_R / (N_neg + N_R),
      NA_real_
    )
  )


################################################################################
# SECTION 6: HIV INCIDENCE ESTIMATION
################################################################################

cat("\n=== ESTIMATING HIV INCIDENCE ===\n")

# -----------------------------------------------------------------------------
# 6.1: Calculate RSE for MDRI from Confidence Interval
# -----------------------------------------------------------------------------

RSE_MDRI <- rse_from_ci(MDRI_DAYS, MDRI_CI[1], MDRI_CI[2])
cat("MDRI:", MDRI_DAYS, "days (RSE =", sprintf("%.4f", RSE_MDRI), ")\n")

# -----------------------------------------------------------------------------
# 6.2: Define Incidence Estimation Function
# -----------------------------------------------------------------------------

#' Estimate Incidence for One Stratum
#'
#' Uses inctools::incprops to estimate HIV incidence with bootstrap CI
#'
#' @param N_known Total with known serostatus
#' @param N_H Total HIV-positive
#' @param N_neg Total HIV-negative
#' @param N_testR Number tested for recency
#' @param N_R Number classified as recent
#' @param de_h Design effect for HIV prevalence (default = 1)
#' @param de_r Design effect for recency prevalence (default = 1)
#' @param mdri_days Mean duration of recent infection in days
#' @param rse_mdri Relative standard error of MDRI
#' @param frr False recent rate
#' @param rse_frr Relative standard error of FRR
#' @param bigt_days Time cutoff for recency definition in days
#' @param boot Use bootstrap for CI (vs delta method)
#' @param bs_count Number of bootstrap iterations
#' @param alpha Significance level for CI
#' @param min_rse Minimum RSE for numerical stability
#'
#' @return Tibble with incidence estimate, CI, RSE, method, and error message
#'
#' @details
#' Performs extensive validity checks before estimation:
#' - Requires N_known > 0
#' - Requires both HIV+ and HIV- > 0
#' - Requires recency testing (N_testR > 0)
#' - Requires at least one recent infection (N_R > 0)
#' 
#' Returns NA with descriptive error message if any check fails.
#' Attempts bootstrap first, falls back to delta method if bootstrap fails.
estimate_one_stratum <- function(N_known, N_H, N_neg, N_testR, N_R,
                                 de_h = 1, de_r = 1,
                                 mdri_days = MDRI_DAYS, 
                                 rse_mdri = RSE_MDRI,
                                 frr = FRR, 
                                 rse_frr = RSE_FRR,
                                 bigt_days = BIGT_DAYS,
                                 boot = TRUE, 
                                 bs_count = BS_COUNT,
                                 alpha = ALPHA,
                                 min_rse = MIN_RSE_FOR_BOOT) {
  
  # -------------------------------------------------------------------------
  # Validity Checks
  # -------------------------------------------------------------------------
  
  # Check 1: Valid denominator (known serostatus)
  if (is.na(N_known) || N_known <= 0) {
    return(tibble::tibble(
      Incidence = NA_real_,
      CI_low = NA_real_,
      CI_up = NA_real_,
      RSE_Inc = NA_real_,
      Method = NA_character_,
      Error = "No ANC known-serostatus denominator (N_known <= 0)"
    ))
  }
  
  # Check 2: Both HIV+ and HIV- required
  if (is.na(N_H) || N_H <= 0 || is.na(N_neg) || N_neg <= 0) {
    return(tibble::tibble(
      Incidence = NA_real_,
      CI_low = NA_real_,
      CI_up = NA_real_,
      RSE_Inc = NA_real_,
      Method = NA_character_,
      Error = "Cannot estimate incidence (requires HIV+ >0 AND HIV- >0)"
    ))
  }
  
  # Check 3: Recency testing required
  if (is.na(N_testR) || N_testR <= 0) {
    return(tibble::tibble(
      Incidence = NA_real_,
      CI_low = NA_real_,
      CI_up = NA_real_,
      RSE_Inc = NA_real_,
      Method = NA_character_,
      Error = "No recency-tested HIV-positive (N_testR <= 0)"
    ))
  }
  
  # Check 4: At least one recent infection required
  # Note: inctools breaks at PrevR=0 due to undefined RSE
  if (is.na(N_R) || N_R <= 0) {
    return(tibble::tibble(
      Incidence = NA_real_,
      CI_low = NA_real_,
      CI_up = NA_real_,
      RSE_Inc = NA_real_,
      Method = NA_character_,
      Error = "No recent infections observed in stratum (N_R == 0) => not estimable"
    ))
  }
  
  # -------------------------------------------------------------------------
  # Calculate Prevalences
  # -------------------------------------------------------------------------
  
  PrevH <- N_H / N_known      # HIV prevalence

  # Protocol metric: proportion RITA recent in at-risk population.
  prevr_denom <- N_R + N_neg
  PrevR <- N_R / prevr_denom
  
  # -------------------------------------------------------------------------
  # Calculate Relative Standard Errors
  # -------------------------------------------------------------------------
  
  # RSE for HIV prevalence
  rse_ph <- rse_binom(PrevH, N_known, de = de_h, 
                      min_rse = min_rse, max_rse = MAX_RSE)
  
  # RSE for recency prevalence uses the same denominator basis as PrevR.
  rse_pr <- rse_binom(PrevR, prevr_denom, de = de_r,
                      min_rse = min_rse, max_rse = MAX_RSE)
  
  # Check 5: Valid RSEs required
  if (is.na(rse_ph) || is.na(rse_pr)) {
    return(tibble::tibble(
      Incidence = NA_real_,
      CI_low = NA_real_,
      CI_up = NA_real_,
      RSE_Inc = NA_real_,
      Method = NA_character_,
      Error = "Invalid prevalence/RSE inputs (PrevH or PrevR at boundary or undefined RSE)"
    ))
  }
  
  # -------------------------------------------------------------------------
  # Run inctools::incprops
  # -------------------------------------------------------------------------
  
  # Wrapper function for incprops call
  run_incprops <- function(use_boot) {
    withCallingHandlers(
      inctools::incprops(
        PrevH = PrevH,
        RSE_PrevH = rse_ph,
        PrevR = PrevR,
        RSE_PrevR = rse_pr,
        MDRI = mdri_days,
        RSE_MDRI = rse_mdri,
        FRR = frr,
        RSE_FRR = rse_frr,
        BigT = bigt_days,
        Boot = use_boot,
        BS_Count = bs_count,
        alpha = alpha,
        BMest = "same.test",  # Both prevalences from same test
        Covar_HR = 0          # Assume independence
      ),
      warning = function(w) {
        if (identical(frr, 0) && grepl("Zero estimated FRR", conditionMessage(w), fixed = TRUE)) {
          invokeRestart("muffleWarning")
        }
      }
    )
  }
  
  # Attempt estimation with error handling
  out <- tryCatch({
    # Try bootstrap first
    res <- run_incprops(use_boot = boot)
    list(
      res = res,
      method = if (boot) "Bootstrap" else "Delta",
      err = NA_character_
    )
  }, error = function(e) {
    # If bootstrap fails, try delta method
    if (boot) {
      res2 <- tryCatch(
        run_incprops(use_boot = FALSE),
        error = function(e2) NULL
      )
      
      if (is.null(res2)) {
        return(list(
          res = NULL,
          method = NA_character_,
          err = paste("Bootstrap failed:", e$message, "| Delta also failed")
        ))
      }
      
      return(list(
        res = res2,
        method = "Delta (fallback)",
        err = paste("Bootstrap failed:", e$message)
      ))
    }
    
    # Delta method also failed
    list(
      res = NULL,
      method = NA_character_,
      err = paste("Delta failed:", e$message)
    )
  })
  
  # Check if estimation succeeded
  if (is.null(out$res)) {
    return(tibble::tibble(
      Incidence = NA_real_,
      CI_low = NA_real_,
      CI_up = NA_real_,
      RSE_Inc = NA_real_,
      Method = out$method,
      Error = out$err
    ))
  }
  
  # -------------------------------------------------------------------------
  # Extract Results
  # -------------------------------------------------------------------------
  
  # --- helper: safely pull first value from a DF/list with flexible column names ---
  extract_first_numeric <- function(x, candidates) {
    if (is.null(x)) return(NA_real_)
    
    # data.frame / tibble
    if (is.data.frame(x)) {
      # exact names
      for (nm in candidates) {
        if (nm %in% names(x)) {
          v <- x[[nm]]
          return(if (length(v) >= 1) suppressWarnings(as.numeric(v[1])) else NA_real_)
        }
      }
      # try normalized names (turn spaces/dots into syntactic names)
      x2 <- x
      names(x2) <- make.names(names(x2))
      cand2 <- make.names(candidates)
      for (nm in cand2) {
        if (nm %in% names(x2)) {
          v <- x2[[nm]]
          return(if (length(v) >= 1) suppressWarnings(as.numeric(v[1])) else NA_real_)
        }
      }
      return(NA_real_)
    }
    
    # matrix
    if (is.matrix(x)) {
      return(extract_first_numeric(as.data.frame(x), candidates))
    }
    
    # list
    if (is.list(x)) {
      for (nm in candidates) {
        if (!is.null(names(x)) && nm %in% names(x)) {
          v <- x[[nm]]
          return(if (length(v) >= 1) suppressWarnings(as.numeric(v[1])) else NA_real_)
        }
      }
    }
    
    NA_real_
  }
  
  # --- Extract results robustly ---
  inc_stats <- out$res[["Incidence.Statistics"]]
  
  Incidence <- extract_first_numeric(inc_stats, c("Incidence", "I", "incidence"))
  CI_low    <- extract_first_numeric(inc_stats, c("CI low", "CI.low", "CI_Low", "CI_lower", "CI.Lower"))
  CI_up     <- extract_first_numeric(inc_stats, c("CI up", "CI.up", "CI_Up", "CI_upper", "CI.Upper"))
  RSE_Inc   <- extract_first_numeric(inc_stats, c("RSE", "RSE_I", "RSE.Incidence", "RSE_Inc"))
  
  # If we still couldn't extract, write an explicit error so you SEE it
  err_final <- out$err
  if (is.na(err_final) && is.na(Incidence)) {
    err_final <- paste0(
      "incprops output did not contain expected Incidence.Statistics columns. ",
      "Available columns: ",
      if (is.null(inc_stats) || !is.data.frame(inc_stats)) "NULL/Not a data.frame" else paste(names(inc_stats), collapse = ", ")
    )
  }
  
  tibble::tibble(
    Incidence = Incidence,
    CI_low = CI_low,
    CI_up = CI_up,
    RSE_Inc = RSE_Inc,
    Method = out$method,
    Error = err_final
  )
}


# -----------------------------------------------------------------------------
# 6.3: Apply Incidence Estimation to All Strata
# -----------------------------------------------------------------------------

cat("Estimating incidence for each stratum...\n")
cat("  (This may take several minutes with", BS_COUNT, "bootstrap iterations)\n")

# Add row ID for joining
analysis_df <- analysis_df %>%
  mutate(row_id = dplyr::row_number())

# Apply estimation function to each row
incidence_results <- analysis_df %>%
  dplyr::select(row_id, all_of("county"), N_known, N_H, N_neg, N_testR, N_R) %>%
  mutate(
    inc = purrr::pmap(
      list(N_known, N_H, N_neg, N_testR, N_R),
      ~estimate_one_stratum(..1, ..2, ..3, ..4, ..5)
    )
  ) %>%
  tidyr::unnest(inc, keep_empty = TRUE)

cat("  Incidence estimation complete\n")

# Count successful estimates
n_estimated <- sum(!is.na(incidence_results$Incidence))
n_total <- nrow(incidence_results)
cat("  Successfully estimated:", n_estimated, "of", n_total, "strata\n")

n_high_rse <- sum(!is.na(incidence_results$RSE_Inc) & incidence_results$RSE_Inc > HIGH_RSE_THRESHOLD)
cat("  High-RSE strata (RSE >", sprintf("%.0f%%", HIGH_RSE_THRESHOLD * 100), "):", n_high_rse, "\n")

# Merge back to main analysis dataframe
final_results <- analysis_df %>%
  left_join(
    incidence_results %>% dplyr::select(-N_known, -N_H, -N_neg, -N_testR, -N_R),
    by = c("row_id", "county")
  ) %>%
  dplyr::select(-row_id)

# Pool counties with dashboard recency-tested records into a supplemental stratum.
pooled_all_inputs <- analysis_df %>%
  dplyr::filter(
    !is.na(N_known),
    !is.na(N_H),
    !is.na(N_neg),
    !is.na(N_testR),
    !is.na(N_R),
    N_testR > 0
  )

if (nrow(pooled_all_inputs) > 0) {
  pooled_all_counts <- pooled_all_inputs %>%
    dplyr::summarise(
      County_Count = dplyr::n(),
      Included_Counties = paste(sort(unique(county)), collapse = ", "),
      N_known = sum(N_known, na.rm = TRUE),
      N_new_pos = sum(N_new_pos, na.rm = TRUE),
      N_H = sum(N_H, na.rm = TRUE),
      N_neg = sum(N_neg, na.rm = TRUE),
      N_testR = sum(N_testR, na.rm = TRUE),
      N_R = sum(N_R, na.rm = TRUE)
    )

  pooled_all_estimate <- estimate_one_stratum(
    N_known = pooled_all_counts$N_known,
    N_H = pooled_all_counts$N_H,
    N_neg = pooled_all_counts$N_neg,
    N_testR = pooled_all_counts$N_testR,
    N_R = pooled_all_counts$N_R
  )

  pooled_all_summary <- dplyr::bind_cols(
    tibble::tibble(
      Stratum = "Combined_Recency_Tested",
      County_Count = pooled_all_counts$County_Count,
      Included_Counties = pooled_all_counts$Included_Counties,
      N_Known_Serostatus = pooled_all_counts$N_known,
      N_New_HIV_Diagnoses = pooled_all_counts$N_new_pos,
      N_HIV_Positive = pooled_all_counts$N_H,
      HIV_Prevalence_percent = ifelse(
        pooled_all_counts$N_known > 0,
        (pooled_all_counts$N_H / pooled_all_counts$N_known) * 100,
        NA_real_
      ),
      N_Tested_Recency = pooled_all_counts$N_testR,
      N_neg = pooled_all_counts$N_neg,
      N_R = pooled_all_counts$N_R,
      N_Recent_Infections = pooled_all_counts$N_R,
      At_Risk_Population = pooled_all_counts$N_neg + pooled_all_counts$N_R,
      Prop_RITA_Recent_percent = ifelse(
        (pooled_all_counts$N_neg + pooled_all_counts$N_R) > 0,
        (pooled_all_counts$N_R / (pooled_all_counts$N_neg + pooled_all_counts$N_R)) * 100,
        NA_real_
      )
    ),
    pooled_all_estimate %>%
      dplyr::transmute(
        HIV_Incidence_percent = Incidence * 100,
        RSE = RSE_Inc,
        Lower_CI_percent = pmax(CI_low * 100, 0),
        Upper_CI_percent = CI_up * 100,
        Method = Method,
        Data_Quality = dplyr::case_when(
          is.na(RSE_Inc) ~ "Not Estimable",
          RSE_Inc <= HIGH_RSE_THRESHOLD ~ "Within Protocol RSE Threshold",
          RSE_Inc <= 0.50 ~ "High RSE (>25%)",
          TRUE ~ "Very High RSE (>50%)"
        ),
        Estimation_Note = Error
      )
  )
} else {
  pooled_all_summary <- tibble::tibble(
    Stratum = "Combined_Recency_Tested",
    County_Count = 0L,
    Included_Counties = "No counties with dashboard recency-tested records were available to pool",
    N_Known_Serostatus = NA_real_,
    N_HIV_Positive = NA_real_,
    HIV_Prevalence_percent = NA_real_,
    N_Tested_Recency = NA_real_,
    N_Recent_Infections = NA_real_,
    At_Risk_Population = NA_real_,
    Prop_RITA_Recent_percent = NA_real_,
    HIV_Incidence_percent = NA_real_,
    RSE = NA_real_,
    Lower_CI_percent = NA_real_,
    Upper_CI_percent = NA_real_,
    Method = NA_character_,
    Data_Quality = "Not Estimable",
    Estimation_Note = "No counties with dashboard recency-tested records were available to pool"
  )
}


################################################################################
# SECTION 9: FINAL OUTPUTS AND REPORTING
################################################################################

cat("\n=== GENERATING FINAL REPORT ===\n")

# -----------------------------------------------------------------------------
# 9.1: Create Final Report Table
# -----------------------------------------------------------------------------

county_site_counts <- khis_preart_vl_sites %>%
  dplyr::group_by(County) %>%
  dplyr::summarise(
    N_Sites_Contributing_PreART_VL = dplyr::n_distinct(mfl),
    .groups = "drop"
  ) %>%
  dplyr::mutate(county = gsub(" County$", "", County)) %>%
  dplyr::select(county, N_Sites_Contributing_PreART_VL)

report_table <- final_results %>%
  dplyr::left_join(county_site_counts, by = "county") %>%
  dplyr::left_join(all_anc1_prevalence, by = "county") %>%
  dplyr::left_join(vls_counts, by = "county") %>%
  mutate(
    HIV_Prevalence = PrevH,
    RecencyPrev_HIVpos = PrevR_HIVpos,
    PropRITA_Recent_AtRisk = PropRITA_AtRisk,
    At_Risk_Denominator = N_R + N_neg,
    # Protocol-facing precision summary based on the 25% RSE threshold.
    Data_Quality_Flag = dplyr::case_when(
      is.na(RSE_Inc) ~ "Not Estimable",
      RSE_Inc <= HIGH_RSE_THRESHOLD ~ "Within Protocol RSE Threshold",
      RSE_Inc <= 0.50 ~ "High RSE (>25%)",
      TRUE ~ "Very High RSE (>50%)"
    )
  ) %>%
  dplyr::select(
    all_of("county"),
    N_Sites_Contributing_PreART_VL, # KHIS ANC sites matched to pre-ART VL contribution
    N_known,                    # Total with known serostatus
    N_new_pos,                  # ANC1 new HIV diagnoses
    N_H,                        # Total HIV-positive
    HIV_Prevalence,             # HIV prevalence across all ANC1 KHIS sites
    N_testR,                    # Number tested for recency
    N_With_VL,                  # Number with parseable viral load
    N_VL_Suppressed,            # Number with VL < 1000 copies/mL
    VLS,                        # Viral load suppression among parseable VLs
    N_neg,                      # Restricted HIV-negative denominator
    N_R = N_R,                  # Number recent
    N_Recent_Infections = N_R,  # Human-readable label for N_R
    At_Risk_Denominator,        # At-risk population
    PropRITA_AtRisk,            # Proportion RITA recent at risk
    Incidence,                  # Incidence estimate
    RSE_Inc,                    # Relative Standard Error
    CI_low, CI_up,              # Confidence interval bounds
    Method,                     # Estimation method
    Data_Quality_Flag           # Data quality indicator
  )

 colnames(report_table) = c("County", "N_Sites_Contributing_PreART_VL", "N_Known_Serostatus", "N_New_HIV_Diagnoses", "N_HIV_Positive", "HIV_Prevalence_%", 
                           "N_Tested_Recency", "N_With_VL", "N_VL_Suppressed", "VLS_%", "N_neg", "N_R", "N_Recent_Infections", "At_Risk_Population", 
                           "Prop_RITA_Recent_%", "HIV_Incidence_%", "RSE", "Lower_CI_%", "Upper_CI_%", 
                           "Method", "Data_Quality")

# Convert percentages and truncate negative CIs at zero
report_table[["HIV_Prevalence_%"]] <- report_table[["HIV_Prevalence_%"]] * 100
report_table[["VLS_%"]] <- report_table[["VLS_%"]] * 100
report_table[["Prop_RITA_Recent_%"]] <- report_table[["Prop_RITA_Recent_%"]] * 100
report_table[["HIV_Incidence_%"]] <- report_table[["HIV_Incidence_%"]] * 100
report_table[["Lower_CI_%"]] <- report_table[["Lower_CI_%"]] * 100
report_table[["Upper_CI_%"]] <- report_table[["Upper_CI_%"]] * 100

# Truncate negative LCL at zero (incidence cannot be negative)
report_table[["Lower_CI_%"]][report_table[["Lower_CI_%"]] < 0] <- 0

high_rse_summary <- report_table %>%
  dplyr::filter(!is.na(RSE), RSE > HIGH_RSE_THRESHOLD) %>%
  dplyr::arrange(dplyr::desc(RSE))

run_summary <- tibble::tibble(
  Metric = c(
    "Implemented denominator",
    "Protocol denominator reference",
    "Deviation note",
    "Prevalence scope",
    "ANC site restriction",
    "Matched KHIS pre-ART VL sites",
    "Unmatched pre-ART VL sites",
    "PrevR basis",
    "RSE_PrevR basis",
    "VL threshold basis",
    "VLS basis",
    "Pooled recency-tested-counties sheet",
    "Pooled recency-tested-counties county count",
    "Total strata in report",
    "Incidence estimable strata",
    "High-RSE threshold",
    "High-RSE strata count"
  ),
  Value = c(
    "ANC1 HIV-negative + ANC1 HIV-positive",
    "ANC1 HIV-negative + ANC1 HIV-positive",
    "None; analysis follows protocol ANC1-only denominator",
    "County-level ANC1 prevalence uses all ANC1 KHIS sites",
    "Incidence HIV-negative denominator restricted to KHIS ANC sites with matched pre-ART VL contribution",
    as.character(dplyr::n_distinct(khis_preart_vl_sites$mfl)),
    as.character(nrow(unmatched_preart_vl_sites)),
    "N_R / (N_neg + N_R)",
    "Same denominator as PrevR: N_neg + N_R",
    "VL >= 1000 = RECENT; VL < 1000 = LONGTERM",
    "VL < 1000 copies/mL among parseable recency VL results",
    "Pools counties with dashboard recency-tested records using the same incidence method",
    as.character(nrow(pooled_all_inputs)),
    as.character(nrow(report_table)),
    as.character(sum(!is.na(report_table$RSE))),
    sprintf("%.0f%%", HIGH_RSE_THRESHOLD * 100),
    as.character(nrow(high_rse_summary))
  )
)

if (nrow(high_rse_summary) == 0) {
  high_rse_summary <- tibble::tibble(
    County = "None",
    N_Known_Serostatus = NA_real_,
    N_HIV_Positive = NA_real_,
    N_Tested_Recency = NA_real_,
    N_With_VL = NA_real_,
    N_VL_Suppressed = NA_real_,
    VLS_percent = NA_real_,
    N_Recent_Infections = NA_real_,
    At_Risk_Population = NA_real_,
    Prop_RITA_Recent_percent = NA_real_,
    HIV_Incidence_percent = NA_real_,
    RSE = NA_real_,
    Lower_CI_percent = NA_real_,
    Upper_CI_percent = NA_real_,
    Method = NA_character_,
    Data_Quality = paste0("No strata exceeded ", sprintf("%.0f%%", HIGH_RSE_THRESHOLD * 100), " RSE")
  )
} else {
  names(high_rse_summary)[names(high_rse_summary) == "Prop_RITA_Recent_%"] <- "Prop_RITA_Recent_percent"
  names(high_rse_summary)[names(high_rse_summary) == "HIV_Incidence_%"] <- "HIV_Incidence_percent"
  names(high_rse_summary)[names(high_rse_summary) == "Lower_CI_%"] <- "Lower_CI_percent"
  names(high_rse_summary)[names(high_rse_summary) == "Upper_CI_%"] <- "Upper_CI_percent"
}

if (nrow(unmatched_preart_vl_sites) == 0) {
  unmatched_preart_vl_sites <- tibble::tibble(
    mfl = NA_real_,
    recency_facility_name = "All pre-ART VL-contributing sites matched to KHIS",
    recency_county = NA_character_,
    recency_records_all = NA_real_,
    N_testR_site = NA_real_,
    N_R_site = NA_real_
  )
}

if (nrow(unmatched_preart_vl_site_suggestions) == 0) {
  unmatched_preart_vl_site_suggestions <- tibble::tibble(
    recency_mfl = NA_real_,
    recency_facility_name = "All pre-ART VL-contributing sites matched to KHIS",
    recency_county = NA_character_,
    master_exact_match = FALSE,
    master_exact_facilityname = NA_character_,
    master_exact_official_name = NA_character_,
    master_exact_county = NA_character_,
    master_exact_subcounty = NA_character_,
    master_exact_closed = NA_character_,
    recency_records_all = NA_real_,
    N_testR_site = NA_real_,
    N_R_site = NA_real_,
    suggestion_1_mfl = NA_real_,
    suggestion_1_facilityname = NA_character_,
    suggestion_1_county = NA_character_,
    suggestion_1_subcounty = NA_character_,
    suggestion_1_source = NA_character_,
    suggestion_1_name_distance = NA_real_,
    suggestion_2_mfl = NA_real_,
    suggestion_2_facilityname = NA_character_,
    suggestion_2_county = NA_character_,
    suggestion_2_subcounty = NA_character_,
    suggestion_2_source = NA_character_,
    suggestion_2_name_distance = NA_real_,
    suggestion_3_mfl = NA_real_,
    suggestion_3_facilityname = NA_character_,
    suggestion_3_county = NA_character_,
    suggestion_3_subcounty = NA_character_,
    suggestion_3_source = NA_character_,
    suggestion_3_name_distance = NA_real_
  )
}

exact_mfl_master_matches <- unmatched_preart_vl_site_suggestions %>%
  dplyr::filter(master_exact_match %in% TRUE)

still_unresolved_name_suggestions <- unmatched_preart_vl_site_suggestions %>%
  dplyr::filter(!master_exact_match %in% TRUE)

if (nrow(exact_mfl_master_matches) == 0) {
  exact_mfl_master_matches <- unmatched_preart_vl_site_suggestions[0, ]
  exact_mfl_master_matches[1, ] <- NA
  exact_mfl_master_matches$recency_facility_name[1] <- "No exact master-file matches among unmatched pre-ART VL sites"
  exact_mfl_master_matches$master_exact_match[1] <- FALSE
}

if (nrow(still_unresolved_name_suggestions) == 0) {
  still_unresolved_name_suggestions <- unmatched_preart_vl_site_suggestions[0, ]
  still_unresolved_name_suggestions[1, ] <- NA
  still_unresolved_name_suggestions$recency_facility_name[1] <- "No unresolved unmatched pre-ART VL sites remain after master-file lookup"
  still_unresolved_name_suggestions$master_exact_match[1] <- FALSE
}

data_dictionary <- tibble::tibble(
  `Column name` = c(
    "County",
    "N_Sites_Contributing_PreART_VL",
    "N_Known_Serostatus",
    "N_New_HIV_Diagnoses",
    "N_HIV_Positive",
    "HIV_Prevalence_%",
    "N_Tested_Recency",
    "N_With_VL",
    "N_VL_Suppressed",
    "VLS_%",
    "N_neg",
    "N_R",
    "N_Recent_Infections",
    "At_Risk_Population",
    "Prop_RITA_Recent_%",
    "HIV_Incidence_%",
    "RSE",
    "Lower_CI_%",
    "Upper_CI_%",
    "Method",
    "Data_Quality"
  ),
  `Source/calculation` = c(
    "County analysis stratum from KHIS totals joined to county-level recency counts.",
    "Diagnostic count of KHIS ANC facilities in the county matched to facilities contributing at least one pre-ART VL sample; not used to restrict the incidence denominator.",
    "County sum of KHIS total_known_serostatus_anc1 = ANC1 HIV-negative + ANC1 HIV-positive.",
    "County sum of KHIS total_new_pos_anc1.",
    "County sum of KHIS total_positive_anc1, including known positives and new ANC1 positives.",
    "Computed as N_HIV_Positive / N_Known_Serostatus * 100 using all ANC1 KHIS sites at county level.",
    "County count of dashboard recency-tested records, defined as lag_status == TESTED with a reported lag_result.",
    "County count of recency records with parseable viral load values.",
    "County count of parseable recency viral load values below 1000 copies/mL.",
    "Computed as N_VL_Suppressed / N_With_VL * 100. Below-limit VL values such as '<40' are counted as suppressed.",
    "County sum of KHIS ANC1 HIV-negative clients restricted to ANC sites with matched pre-ART VL contribution.",
    "Number of final RITA recent infections used in PrevR and incidence estimation.",
    "County count of recency records with final RITA classification RECENT.",
    "Computed as N_neg + N_R, where N_neg is the matched-site KHIS ANC1 HIV-negative county denominator.",
    "Computed as N_R / (N_neg + N_R) * 100.",
    "Computed with inctools::incprops() using PrevH, PrevR, MDRI = 130, FRR = 0, and BigT = 365, then expressed as percent.",
    "Relative standard error of the incidence estimate.",
    "Lower confidence bound for incidence, converted to percent and truncated at zero if negative.",
    "Upper confidence bound for incidence, converted to percent.",
    "Estimation method used for the incidence result, for example bootstrap-based output or Delta (fallback).",
    "Interpretive flag based on estimability and RSE thresholds."
  )
)

report_out_base <- Sys.getenv("INCIDENCE_REPORT_XLSX", unset = file.path("documents", "report_table_new.xlsx"))
report_out_dir <- dirname(report_out_base)
report_out_ext <- tools::file_ext(report_out_base)
report_out_stem <- tools::file_path_sans_ext(basename(report_out_base))
report_timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
report_out_name <- paste0(report_out_stem, "_", report_timestamp)

if (!identical(report_out_ext, "")) {
  report_out_name <- paste0(report_out_name, ".", report_out_ext)
}

report_out_path <- file.path(report_out_dir, report_out_name)
dir.create(report_out_dir, showWarnings = FALSE, recursive = TRUE)

writexl::write_xlsx(
  list(
    County_Results = report_table,
    Data_Dictionary = data_dictionary,
    KHIS_PreART_VL_Sites = khis_preart_vl_sites,
    Unmatched_PreART_VL_Sites = unmatched_preart_vl_sites,
    Exact_MFL_Master_Matches = exact_mfl_master_matches,
    Still_Unresolved = still_unresolved_name_suggestions,
    Combined_Recency_Tested = pooled_all_summary,
    High_RSE_Summary = high_rse_summary,
    Run_Summary = run_summary
  ),
  report_out_path
)
cat("Saved comprehensive incidence report to:", report_out_path, "\n")

cat("\n")

# Write to external access folder

incidence_out_path <- file.path(EXTERNAL_OUTPUT_DIR,"hivit_incidence.xlsx")
dir.create(EXTERNAL_OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)

writexl::write_xlsx(
  list(
    County_Results = report_table,
    Data_Dictionary = data_dictionary
  ),
  incidence_out_path
)
cat("Saved external incidence report to:", incidence_out_path, "\n")


cat("================================================================================\n")
cat("                     ANALYSIS COMPLETE                                          \n")
cat("================================================================================\n")
cat("\n")

# End of script
