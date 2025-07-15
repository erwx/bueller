#' Analyze current year chronic absence patterns
#'
#' @param data Student-level data frame
#' @param current_year Numeric current year
#' @return List containing current year analysis
#' @keywords internal
current <- function(data, current_year) {
  
  # Filter to current year and remove duplicates
  current_data <- data[data[["YEAR"]] == current_year, ]
  current_data <- current_data[
    !duplicated(current_data[["STUDENT_ID"]]), 
  ]
  
  # Basic descriptives
  total_students <- nrow(current_data)
  chronic_absent <- sum(current_data[["CHRONIC_ABSENT"]])
  state_rate <- chronic_absent / total_students
  
  # Count entities analyzed
  n_counties <- length(unique(current_data[["COUNTY_NAME"]]))
  n_districts <- length(unique(current_data[["DISTRICT_NAME"]]))
  n_schools <- length(unique(current_data[["SCHOOL_NAME"]]))
  
  overview <- list(
    year = current_year,
    total_students = total_students,
    chronic_absent = chronic_absent,
    chronic_rate = state_rate,
    n_counties = n_counties,
    n_districts = n_districts,
    n_schools = n_schools
  )
  
  # Grade distribution
  grades <- sort(unique(current_data[["GRADE"]]))
  grade_distribution <- data.frame()
  
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
      PERCENT_OF_TOTAL = total / total_students,
      stringsAsFactors = FALSE
    )
    
    grade_distribution <- rbind(grade_distribution, grade_row)
  }
  
  # Demographic analysis
  demo_cols <- c("MALE", "FEMALE", "HISPANIC", "WHITE", "ASIAN",
                 "BLACK", "ELL", "DISADVANTAGE", "DISABILITY")
  demo_names <- c("male", "female", "hispanic", "white", "asian",
                  "black", "ell", "disadvantaged", "disability")
  
  demographic_summary <- data.frame()
  equity_gaps <- numeric(length(demo_names))
  names(equity_gaps) <- demo_names
  
  for (i in seq_along(demo_cols)) {
    col <- demo_cols[i]
    name <- demo_names[i]
    
    group_data <- current_data[current_data[[col]] == 1, ]
    total <- nrow(group_data)
    chronic <- sum(group_data[["CHRONIC_ABSENT"]])
    rate <- chronic / total
    gap <- rate - state_rate
    equity_gaps[i] <- gap
    
    demo_row <- data.frame(
      GROUP = name,
      TOTAL_STUDENTS = total,
      CHRONIC_ABSENT = chronic,
      CHRONIC_RATE = rate,
      GAP_FROM_STATE = gap,
      ABOVE_STATE_AVG = gap > 0,
      stringsAsFactors = FALSE
    )
    
    demographic_summary <- rbind(demographic_summary, demo_row)
  }
  
  # Identify largest disparities
  largest_positive_gap <- names(which.max(equity_gaps))
  largest_negative_gap <- names(which.min(equity_gaps))
  groups_above_average <- sum(equity_gaps > 0)
  groups_below_average <- sum(equity_gaps < 0)
  
  # County distribution analysis
  counties <- unique(current_data[["COUNTY_NAME"]])
  county_rates <- numeric(length(counties))
  county_students <- numeric(length(counties))
  
  for (i in seq_along(counties)) {
    county_data <- current_data[
      current_data[["COUNTY_NAME"]] == counties[i], 
    ]
    county_students[i] <- nrow(county_data)
    county_rates[i] <- sum(county_data[["CHRONIC_ABSENT"]]) / 
                      nrow(county_data)
  }
  
  # District distribution analysis
  districts <- unique(current_data[["DISTRICT_NAME"]])
  district_rates <- numeric(length(districts))
  district_students <- numeric(length(districts))
  
  for (i in seq_along(districts)) {
    district_data <- current_data[
      current_data[["DISTRICT_NAME"]] == districts[i], 
    ]
    district_students[i] <- nrow(district_data)
    district_rates[i] <- sum(district_data[["CHRONIC_ABSENT"]]) / 
                         nrow(district_data)
  }
  
  # Distribution statistics
  county_percentiles <- quantile(county_rates, 
                                c(0.1, 0.25, 0.5, 0.75, 0.9))
  district_percentiles <- quantile(district_rates, 
                                  c(0.1, 0.25, 0.5, 0.75, 0.9))
  
  # Concentration analysis
  # Sort counties by rate and calculate cumulative student share
  county_order <- order(county_rates, decreasing = TRUE)
  sorted_students <- county_students[county_order]
  cumulative_students <- cumsum(sorted_students)
  cumulative_pct <- cumulative_students / total_students
  
  # Find how many counties contain 50% of students in high-rate areas
  counties_for_50pct <- which(cumulative_pct >= 0.5)[1]
  
  # High-rate area analysis
  high_rate_threshold <- 0.3  # 30% chronic absence rate
  high_rate_counties <- sum(county_rates > high_rate_threshold)
  high_rate_districts <- sum(district_rates > high_rate_threshold)
  
  # Students in high-rate areas
  high_rate_county_mask <- county_rates > high_rate_threshold
  students_in_high_rate_counties <- sum(
    county_students[high_rate_county_mask]
  )
  pct_students_high_rate <- students_in_high_rate_counties / total_students
  
  distribution_analysis <- list(
    county_rate_range = c(min(county_rates), max(county_rates)),
    district_rate_range = c(min(district_rates), max(district_rates)),
    county_percentiles = county_percentiles,
    district_percentiles = district_percentiles,
    counties_for_50pct_students = counties_for_50pct,
    high_rate_counties = high_rate_counties,
    high_rate_districts = high_rate_districts,
    students_in_high_rate_counties = students_in_high_rate_counties,
    pct_students_high_rate = pct_students_high_rate
  )
  
  # Create detailed county summary for reporting
  county_summary <- data.frame(
    COUNTY_NAME = counties,
    TOTAL_STUDENTS = county_students,
    CHRONIC_RATE = county_rates,
    stringsAsFactors = FALSE
  )
  
  # Sort by rate for easy identification of extremes
  county_summary <- county_summary[order(county_summary[["CHRONIC_RATE"]]), ]
  
  # Create detailed district summary (top/bottom performers)
  district_summary <- data.frame(
    DISTRICT_NAME = districts,
    TOTAL_STUDENTS = district_students,
    CHRONIC_RATE = district_rates,
    stringsAsFactors = FALSE
  )
  
  # Filter to districts with meaningful size and sort
  district_summary <- district_summary[district_summary[["TOTAL_STUDENTS"]] >= 100, ]
  district_summary <- district_summary[order(district_summary[["CHRONIC_RATE"]]), ]
  
  # Return comprehensive results
  result <- list(
    overview = overview,
    grade_distribution = grade_distribution,
    demographic_summary = demographic_summary,
    equity_analysis = list(
      largest_positive_gap = largest_positive_gap,
      largest_negative_gap = largest_negative_gap,
      groups_above_average = groups_above_average,
      groups_below_average = groups_below_average,
      max_gap = max(equity_gaps),
      min_gap = min(equity_gaps)
    ),
    distribution_analysis = distribution_analysis,
    county_summary = county_summary,
    district_summary = district_summary
  )
  
  return(result)
}
