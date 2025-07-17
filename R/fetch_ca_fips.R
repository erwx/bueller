#' Fetch California county FIPS codes from Census API
#'
#' @param year ACS year to use for the API call (default: 2022)
#' @return Data frame with columns: name, state_fips, county_fips
#' @importFrom jsonlite fromJSON
#' @keywords internal
fetch_ca_county_fips <- function(year = 2022) {
  # Census API endpoint for CA counties
  url <- paste0("https://api.census.gov/data/", year, "/acs/acs5?get=NAME&for=county:*&in=state:06")
  
  # Make API call using curl
  response <- system(paste0("curl -s '", url, "'"), intern = TRUE)
  
  if (length(response) == 0) {
    stop("Failed to fetch data from Census API")
  }
  
  # Parse JSON response
  json_data <- jsonlite::fromJSON(response)
  
  if (is.null(json_data) || nrow(json_data) < 2) {
    stop("Invalid response from Census API")
  }
  
  # Convert to data frame (first row is headers)
  county_data <- data.frame(
    name = json_data[-1, 1],  # Skip header row
    state_fips = json_data[-1, 2],
    county_fips = json_data[-1, 3],
    stringsAsFactors = FALSE
  )
  
  # Clean county names (remove ", California" suffix)
  county_data$name <- gsub(", California$", "", county_data$name)
  county_data$name <- gsub(" County$", "", county_data$name)
  
  # Sort by county name
  county_data <- county_data[order(county_data$name), ]
  
  return(county_data)
}

#' Fetch ACS data for a county
#'
#' @param state_fips State FIPS code
#' @param county_fips County FIPS code  
#' @param year ACS year (default: 2022)
#' @return Data frame with ACS variables
#' @keywords internal
fetch_acs_data <- function(state_fips, county_fips, year = 2022) {
  variables <- c(
    "B19013_001E", "B17001_001E", "B17001_002E", "B25070_001E",
    "B25070_007E", "B25070_008E", "B25070_009E", "B25070_010E",
    "B19057_001E", "B19057_002E"
  )
  
  vars_string <- paste(variables, collapse = ",")
  url <- paste0(
    "https://api.census.gov/data/", year, "/acs/acs5?",
    "get=", vars_string,
    "&for=county:", county_fips,
    "&in=state:", state_fips
  )
  
  response <- system(paste0("curl -s '", url, "'"), intern = TRUE)
  if (length(response) == 0) stop("Failed to fetch ACS data")
  
  json_data <- jsonlite::fromJSON(response)
  if (is.null(json_data) || nrow(json_data) < 2) stop("Invalid ACS response")
  
  # Create named list first, then convert to data frame
  acs_list <- list()
  
  # First row contains column names, second row contains values
  for (i in 1:ncol(json_data)) {
    col_name <- json_data[1, i]
    col_value <- json_data[2, i]
    acs_list[[col_name]] <- col_value
  }
  
  # Convert list to data frame
  acs_raw <- data.frame(acs_list, stringsAsFactors = FALSE)
  
  # Convert numeric columns
  for (var in variables) {
    if (var %in% names(acs_raw)) {
      acs_raw[[var]] <- as.numeric(acs_raw[[var]])
    }
  }
  
  return(acs_raw)
}

#' Calculate derived ACS metrics
#'
#' @param acs_raw Raw ACS data from fetch_acs_data
#' @return List with calculated metrics
#' @keywords internal
calculate_acs_metrics <- function(acs_raw) {
  median_income <- acs_raw[["B19013_001E"]]
  total_pop <- acs_raw[["B17001_001E"]]
  poverty_pop <- acs_raw[["B17001_002E"]]
  total_renters <- acs_raw[["B25070_001E"]]
  renters_30_35 <- acs_raw[["B25070_007E"]]
  renters_35_40 <- acs_raw[["B25070_008E"]]
  renters_40_50 <- acs_raw[["B25070_009E"]]
  renters_50_plus <- acs_raw[["B25070_010E"]]
  total_households <- acs_raw[["B19057_001E"]]
  assistance_households <- acs_raw[["B19057_002E"]]
  
  poverty_rate <- if (!is.na(total_pop) && total_pop > 0) poverty_pop / total_pop else NA
  
  cost_burden_30_plus <- if (!is.na(total_renters) && total_renters > 0) {
    (renters_30_35 + renters_35_40 + renters_40_50 + renters_50_plus) / total_renters
  } else NA
  
  cost_burden_50_plus <- if (!is.na(total_renters) && total_renters > 0) {
    renters_50_plus / total_renters
  } else NA
  
  assistance_rate <- if (!is.na(total_households) && total_households > 0) {
    assistance_households / total_households
  } else NA
  
  list(
    median_household_income = median_income,
    poverty_rate = poverty_rate,
    housing_cost_burden_30_plus = cost_burden_30_plus,
    housing_cost_burden_50_plus = cost_burden_50_plus,
    public_assistance_rate = assistance_rate,
    total_population = total_pop,
    total_households = total_households,
    total_renters = total_renters
  )
}

#' Get ACS data for a county by name
#'
#' @param county_name County name
#' @param year ACS year (default: 2022)
#' @return List with ACS metrics
#' @keywords internal
get_county_acs_data <- function(county_name, year = 2022) {
  ca_fips <- fetch_ca_county_fips(year)
  match_idx <- match(county_name, ca_fips$name)
  
  if (is.na(match_idx)) {
    stop("County not found: ", county_name)
  }
  
  state_fips <- ca_fips$state_fips[match_idx]
  county_fips <- ca_fips$county_fips[match_idx]
  
  acs_raw <- fetch_acs_data(state_fips, county_fips, year)
  acs_metrics <- calculate_acs_metrics(acs_raw)
  
  return(acs_metrics)
}
