#' Analyze single district chronic absence patterns
#'
#' @param data Student-level data frame
#' @param district_name Name of district to analyze
#' @return List containing district analysis results
#' @importFrom stats median
#' @export
analyze_district <- function(data, district_name) {
  # Filter to district
  district_data <- data[data[["DISTRICT_NAME"]] == district_name, ]
  
  if (nrow(district_data) == 0) {
    stop("District not found: ", district_name)
  }
  
  # Get years available
  years <- sort(unique(district_data[["YEAR"]]))
  current_year <- max(years)
  
  # Remove duplicates by student-year
  district_data <- district_data[
    !duplicated(district_data[c("STUDENT_ID", "YEAR")]), 
  ]
  
  # Run analysis components
  overview <- district_overview(district_data, current_year)
  performance <- district_performance(district_data, data, 
                                     current_year)
  schools <- district_schools(district_data, current_year)
  demographics <- district_demographics(district_data, 
                                       current_year)
  grades <- district_grades(district_data, current_year)
  benchmarks <- district_benchmarks(district_data, data, 
                                   current_year)
  
  # Get county context
  county_context <- NULL
  tryCatch({
    # Get county name from district data
    county_name <- unique(district_data[["COUNTY_NAME"]])[1]
    
    if (!is.na(county_name) && nchar(county_name) > 0) {
      cat("Fetching ACS data for", county_name, "County...\n")
      county_context <- district_county_context(district_data, county_name, 2023)
    } else {
      warning("County name not found in district data")
    }
  }, error = function(e) {
    warning("Failed to fetch ACS data: ", e$message)
  })
  
  # Combine results
  result <- list(
    district_name = district_name,
    current_year = current_year,
    years_available = years,
    overview = overview,
    performance = performance,
    schools = schools,
    demographics = demographics,
    grades = grades,
    benchmarks = benchmarks,
    county_context = county_context
  )
  
  return(result)
}

#' District overview analysis
#'
#' @param district_data Filtered district data
#' @param current_year Current year
#' @return List with overview metrics
#' @keywords internal
district_overview <- function(district_data, current_year) {
  # Multi-year enrollment trends
  years <- sort(unique(district_data[["YEAR"]]))
  enrollment_trend <- data.frame()
  
  for (year in years) {
    year_data <- district_data[district_data[["YEAR"]] == year, ]
    total <- nrow(year_data)
    chronic <- sum(year_data[["CHRONIC_ABSENT"]])
    rate <- chronic / total
    
    trend_row <- data.frame(
      YEAR = year,
      TOTAL_STUDENTS = total,
      CHRONIC_ABSENT = chronic,
      CHRONIC_RATE = rate,
      stringsAsFactors = FALSE
    )
    enrollment_trend <- rbind(enrollment_trend, trend_row)
  }
  
  # Current year data
  current_data <- district_data[
    district_data[["YEAR"]] == current_year, 
  ]
  
  # Basic metrics
  total_students <- nrow(current_data)
  chronic_students <- sum(current_data[["CHRONIC_ABSENT"]])
  chronic_rate <- chronic_students / total_students
  
  # School and grade info
  schools <- unique(current_data[["SCHOOL_NAME"]])
  grades <- sort(unique(current_data[["GRADE"]]))
  
  result <- list(
    total_students = total_students,
    chronic_students = chronic_students,
    chronic_rate = chronic_rate,
    n_schools = length(schools),
    grade_span = c(min(grades), max(grades)),
    enrollment_trend = enrollment_trend
  )
  
  return(result)
}

#' District performance analysis
#'
#' @param district_data Filtered district data
#' @param full_data Complete dataset
#' @param current_year Current year
#' @return List with performance metrics
#' @keywords internal
district_performance <- function(district_data, full_data, 
                                current_year) {
  # Current district rate
  current_data <- district_data[
    district_data[["YEAR"]] == current_year, 
  ]
  district_rate <- sum(current_data[["CHRONIC_ABSENT"]]) / 
                  nrow(current_data)
  
  # State rate for comparison
  state_data <- full_data[full_data[["YEAR"]] == current_year, ]
  state_data <- state_data[
    !duplicated(state_data[["STUDENT_ID"]]), 
  ]
  state_rate <- sum(state_data[["CHRONIC_ABSENT"]]) / 
                nrow(state_data)
  
  # Year-over-year change
  years <- sort(unique(district_data[["YEAR"]]))
  if (length(years) > 1) {
    prev_year <- years[length(years) - 1]
    prev_data <- district_data[
      district_data[["YEAR"]] == prev_year, 
    ]
    prev_rate <- sum(prev_data[["CHRONIC_ABSENT"]]) / 
                 nrow(prev_data)
    yoy_change <- district_rate - prev_rate
  } else {
    yoy_change <- NA
  }
  
  # Peak year analysis
  trend_rates <- numeric(length(years))
  for (i in seq_along(years)) {
    year_data <- district_data[
      district_data[["YEAR"]] == years[i], 
    ]
    trend_rates[i] <- sum(year_data[["CHRONIC_ABSENT"]]) / 
                      nrow(year_data)
  }
  
  peak_rate <- max(trend_rates)
  peak_year <- years[which.max(trend_rates)]
  target_rate <- peak_rate * 0.5
  
  # Projection calculation
  if (district_rate > target_rate) {
    if (length(years) > 1) {
      avg_annual_change <- (district_rate - trend_rates[1]) / 
                          (length(years) - 1)
      if (avg_annual_change < 0) {
        years_to_target <- ceiling(
          (district_rate - target_rate) / abs(avg_annual_change)
        )
      } else {
        years_to_target <- NA
      }
    } else {
      years_to_target <- NA
    }
  } else {
    years_to_target <- 0
  }
  
  result <- list(
    district_rate = district_rate,
    state_rate = state_rate,
    gap_from_state = district_rate - state_rate,
    yoy_change = yoy_change,
    peak_year = peak_year,
    peak_rate = peak_rate,
    target_rate = target_rate,
    years_to_target = years_to_target
  )
  
  return(result)
}

#' District school analysis
#'
#' @param district_data Filtered district data
#' @param current_year Current year
#' @return List with school-level analysis
#' @keywords internal
district_schools <- function(district_data, current_year) {
  current_data <- district_data[
    district_data[["YEAR"]] == current_year, 
  ]
  
  # School-level aggregation
  schools <- unique(current_data[["SCHOOL_NAME"]])
  school_summary <- data.frame()
  
  for (school in schools) {
    school_data <- current_data[
      current_data[["SCHOOL_NAME"]] == school, 
    ]
    
    total <- nrow(school_data)
    chronic <- sum(school_data[["CHRONIC_ABSENT"]])
    rate <- chronic / total
    
    school_row <- data.frame(
      SCHOOL_NAME = school,
      TOTAL_STUDENTS = total,
      CHRONIC_ABSENT = chronic,
      CHRONIC_RATE = rate,
      stringsAsFactors = FALSE
    )
    school_summary <- rbind(school_summary, school_row)
  }
  
  # Sort by chronic absence count
  school_summary <- school_summary[
    order(-school_summary[["CHRONIC_ABSENT"]]), 
  ]
  
  # Concentration analysis
  total_district_chronic <- sum(school_summary[["CHRONIC_ABSENT"]])
  school_summary[["CUMULATIVE_CHRONIC"]] <- cumsum(
    school_summary[["CHRONIC_ABSENT"]]
  )
  school_summary[["CUMULATIVE_PCT"]] <- 
    school_summary[["CUMULATIVE_CHRONIC"]] / total_district_chronic
  
  # Top 20% of schools concentration
  n_schools <- nrow(school_summary)
  top_20_count <- max(1, round(n_schools * 0.2))
  top_20_chronic <- sum(
    school_summary[["CHRONIC_ABSENT"]][1:top_20_count]
  )
  top_20_share <- top_20_chronic / total_district_chronic
  
  result <- list(
    school_summary = school_summary,
    total_schools = n_schools,
    top_20_pct_share = top_20_share
  )
  
  return(result)
}

#' District demographic analysis
#'
#' @param district_data Filtered district data
#' @param current_year Current year
#' @return List with demographic analysis
#' @keywords internal
district_demographics <- function(district_data, current_year) {
  current_data <- district_data[
    district_data[["YEAR"]] == current_year, 
  ]
  
  # Overall district rate
  district_rate <- sum(current_data[["CHRONIC_ABSENT"]]) / 
                  nrow(current_data)
  
  # Demographic columns
  demo_cols <- c("MALE", "FEMALE", "HISPANIC", "WHITE", 
                 "ASIAN", "BLACK", "ELL", "DISADVANTAGE", 
                 "DISABILITY")
  demo_names <- c("male", "female", "hispanic", "white", 
                  "asian", "black", "ell", "disadvantaged", 
                  "disability")
  
  demographic_summary <- data.frame()
  
  for (i in seq_along(demo_cols)) {
    col <- demo_cols[i]
    name <- demo_names[i]
    
    group_data <- current_data[current_data[[col]] == 1, ]
    
    if (nrow(group_data) > 0) {
      total <- nrow(group_data)
      chronic <- sum(group_data[["CHRONIC_ABSENT"]])
      rate <- chronic / total
      gap <- rate - district_rate
      
      demo_row <- data.frame(
        GROUP = name,
        TOTAL_STUDENTS = total,
        CHRONIC_ABSENT = chronic,
        CHRONIC_RATE = rate,
        GAP_FROM_DISTRICT = gap,
        stringsAsFactors = FALSE
      )
      
      demographic_summary <- rbind(demographic_summary, demo_row)
    }
  }
  
  result <- list(
    district_rate = district_rate,
    demographic_summary = demographic_summary
  )
  
  return(result)
}

#' District grade analysis
#'
#' @param district_data Filtered district data
#' @param current_year Current year
#' @return List with grade-level analysis
#' @keywords internal
district_grades <- function(district_data, current_year) {
  current_data <- district_data[
    district_data[["YEAR"]] == current_year, 
  ]
  
  grades <- sort(unique(current_data[["GRADE"]]))
  grade_summary <- data.frame()
  
  for (grade in grades) {
    grade_data <- current_data[current_data[["GRADE"]] == grade, ]
    
    total <- nrow(grade_data)
    chronic <- sum(grade_data[["CHRONIC_ABSENT"]])
    rate <- chronic / total
    
    grade_row <- data.frame(
      GRADE = grade,
      TOTAL_STUDENTS = total,
      CHRONIC_ABSENT = chronic,
      CHRONIC_RATE = rate,
      stringsAsFactors = FALSE
    )
    
    grade_summary <- rbind(grade_summary, grade_row)
  }
  
  result <- list(
    grade_summary = grade_summary
  )
  
  return(result)
}

#' District benchmark analysis
#'
#' @param district_data Filtered district data
#' @param full_data Complete dataset
#' @param current_year Current year
#' @return List with benchmark comparisons
#' @keywords internal
district_benchmarks <- function(district_data, full_data, 
                               current_year) {
  # Current district metrics
  current_data <- district_data[
    district_data[["YEAR"]] == current_year, 
  ]
  district_rate <- sum(current_data[["CHRONIC_ABSENT"]]) / 
                  nrow(current_data)
  district_enrollment <- nrow(current_data)
  
  # Calculate district disadvantage rate
  district_disadvantage <- sum(current_data[["DISADVANTAGE"]]) / 
                          nrow(current_data)
  
  # State-level data for current year
  state_data <- full_data[full_data[["YEAR"]] == current_year, ]
  state_data <- state_data[
    !duplicated(state_data[["STUDENT_ID"]]), 
  ]
  state_rate <- sum(state_data[["CHRONIC_ABSENT"]]) / 
                nrow(state_data)
  
  # Get all districts for peer comparison
  districts <- unique(state_data[["DISTRICT_NAME"]])
  peer_data <- data.frame()
  
  for (district in districts) {
    dist_data <- state_data[
      state_data[["DISTRICT_NAME"]] == district, 
    ]
    
    if (nrow(dist_data) > 0) {
      enrollment <- nrow(dist_data)
      chronic_rate <- sum(dist_data[["CHRONIC_ABSENT"]]) / 
                     enrollment
      disadvantage_rate <- sum(dist_data[["DISADVANTAGE"]]) / 
                          enrollment
      
      peer_row <- data.frame(
        DISTRICT_NAME = district,
        ENROLLMENT = enrollment,
        CHRONIC_RATE = chronic_rate,
        DISADVANTAGE_RATE = disadvantage_rate,
        stringsAsFactors = FALSE
      )
      
      peer_data <- rbind(peer_data, peer_row)
    }
  }
  
  # Size classification
  peer_data[["SIZE_BAND"]] <- ifelse(
    peer_data[["ENROLLMENT"]] < 2500, "SMALL",
    ifelse(peer_data[["ENROLLMENT"]] <= 10000, "MEDIUM", "LARGE")
  )
  
  # Economic classification
  peer_data[["ECON_BAND"]] <- ifelse(
    peer_data[["DISADVANTAGE_RATE"]] < 0.4, "LOW",
    ifelse(peer_data[["DISADVANTAGE_RATE"]] <= 0.7, "MEDIUM", "HIGH")
  )
  
  # District classification
  district_size <- ifelse(
    district_enrollment < 2500, "SMALL",
    ifelse(district_enrollment <= 10000, "MEDIUM", "LARGE")
  )
  
  district_econ <- ifelse(
    district_disadvantage < 0.4, "LOW",
    ifelse(district_disadvantage <= 0.7, "MEDIUM", "HIGH")
  )
  
  # Find peer districts
  peer_districts <- peer_data[
    peer_data[["SIZE_BAND"]] == district_size & 
    peer_data[["ECON_BAND"]] == district_econ, 
  ]
  
  # Peer ranking
  if (nrow(peer_districts) >= 3) {
    peer_districts <- peer_districts[
      order(peer_districts[["CHRONIC_RATE"]]), 
    ]
    district_rank <- which(
      peer_districts[["DISTRICT_NAME"]] == 
      unique(district_data[["DISTRICT_NAME"]])
    )
    peer_count <- nrow(peer_districts)
    peer_median <- median(peer_districts[["CHRONIC_RATE"]])
  } else {
    district_rank <- NA
    peer_count <- nrow(peer_districts)
    peer_median <- NA
  }
  
  result <- list(
    state_rate = state_rate,
    district_rate = district_rate,
    gap_from_state = district_rate - state_rate,
    size_band = district_size,
    econ_band = district_econ,
    peer_group_size = peer_count,
    peer_rank = district_rank,
    peer_median = peer_median
  )
  
  return(result)
}

#' Analyze district in county context
#'
#' @param district_data Filtered district data
#' @param county_name County name
#' @param acs_year ACS year
#' @return List with county context analysis
#' @keywords internal
district_county_context <- function(district_data, county_name, acs_year) {
  
  # Get ACS data for county
  acs_data <- get_county_acs_data(county_name, acs_year)
  
  # Current year district data
  current_year <- max(district_data[["YEAR"]])
  current_data <- district_data[district_data[["YEAR"]] == current_year, ]
  
  # District metrics
  district_rate <- sum(current_data[["CHRONIC_ABSENT"]]) / nrow(current_data)
  district_disadvantage <- sum(current_data[["DISADVANTAGE"]]) / nrow(current_data)
  
  # County context analysis
  result <- list(
    county_name = county_name,
    acs_year = acs_year,
    
    # ACS metrics
    county_median_income = acs_data$median_household_income,
    county_poverty_rate = acs_data$poverty_rate,
    county_housing_burden_30 = acs_data$housing_cost_burden_30_plus,
    county_housing_burden_50 = acs_data$housing_cost_burden_50_plus,
    county_assistance_rate = acs_data$public_assistance_rate,
    
    # District vs county comparison
    district_chronic_rate = district_rate,
    district_disadvantage_rate = district_disadvantage,
    
    # Context flags
    high_poverty_county = ifelse(is.na(acs_data$poverty_rate), NA, 
                                acs_data$poverty_rate > 0.15),
    high_housing_burden = ifelse(is.na(acs_data$housing_cost_burden_30_plus), NA,
                                acs_data$housing_cost_burden_30_plus > 0.30),
    low_income_county = ifelse(is.na(acs_data$median_household_income), NA,
                              acs_data$median_household_income < 60000)
  )
  
  return(result)
}
