#' Analyze chronic absence trends across years
#'
#' @param data Student-level data frame
#' @param baseline_year Numeric baseline year
#' @param current_year Numeric current year
#' @return List containing trend analysis results
#' @keywords internal
trends <- function(data, baseline_year, current_year) {
  
  # Get unique years and sort
  years <- sort(unique(data[["YEAR"]]))
  n_years <- length(years)
  
  # Remove duplicates by student-year
  data <- data[!duplicated(data[c("STUDENT_ID", "YEAR")]), ]
  
  # State-level trends
  state_rates <- numeric(n_years)
  state_counts <- numeric(n_years)
  state_totals <- numeric(n_years)
  
  for (i in seq_len(n_years)) {
    year_data <- data[data[["YEAR"]] == years[i], ]
    state_totals[i] <- nrow(year_data)
    state_counts[i] <- sum(year_data[["CHRONIC_ABSENT"]])
    state_rates[i] <- state_counts[i] / state_totals[i]
  }
  
  # Year-over-year changes
  yoy_change <- c(NA, diff(state_rates))
  yoy_pct_change <- c(NA, diff(state_rates) / state_rates[-n_years])
  
  state_trends <- data.frame(
    YEAR = years,
    TOTAL_STUDENTS = state_totals,
    CHRONIC_ABSENT = state_counts,
    CHRONIC_RATE = state_rates,
    YOY_CHANGE = yoy_change,
    YOY_PCT_CHANGE = yoy_pct_change,
    stringsAsFactors = FALSE
  )
  
  # County trends
  counties <- unique(data[["COUNTY_NAME"]])
  county_trends <- data.frame()
  
  for (county in counties) {
    county_data <- data[data[["COUNTY_NAME"]] == county, ]
    
    for (year in years) {
      year_county <- county_data[county_data[["YEAR"]] == year, ]
      
      if (nrow(year_county) > 0) {
        total <- nrow(year_county)
        chronic <- sum(year_county[["CHRONIC_ABSENT"]])
        rate <- chronic / total
        
        county_row <- data.frame(
          COUNTY_NAME = county,
          YEAR = year,
          TOTAL_STUDENTS = total,
          CHRONIC_ABSENT = chronic,
          CHRONIC_RATE = rate,
          stringsAsFactors = FALSE
        )
        
        county_trends <- rbind(county_trends, county_row)
      }
    }
  }
  
  # County improvement/decline analysis
  county_changes <- data.frame()
  
  for (county in counties) {
    county_subset <- county_trends[
      county_trends[["COUNTY_NAME"]] == county, 
    ]
    
    if (nrow(county_subset) >= 2) {
      # Sort by year to get first and last available years
      county_subset <- county_subset[order(county_subset[["YEAR"]]), ]
      
      first_year_row <- county_subset[1, ]
      last_year_row <- county_subset[nrow(county_subset), ]
      
      change <- last_year_row[["CHRONIC_RATE"]] - 
               first_year_row[["CHRONIC_RATE"]]
      pct_change <- change / first_year_row[["CHRONIC_RATE"]]
      
      change_row <- data.frame(
        COUNTY_NAME = county,
        FIRST_YEAR = first_year_row[["YEAR"]],
        LAST_YEAR = last_year_row[["YEAR"]],
        FIRST_RATE = first_year_row[["CHRONIC_RATE"]],
        LAST_RATE = last_year_row[["CHRONIC_RATE"]],
        CHANGE = change,
        PCT_CHANGE = pct_change,
        stringsAsFactors = FALSE
      )
      
      county_changes <- rbind(county_changes, change_row)
    }
  }
  
  # District trends (simplified - major districts only)
  districts <- unique(data[["DISTRICT_NAME"]])
  major_districts <- character(0)
  
  for (district in districts) {
    district_size <- nrow(data[data[["DISTRICT_NAME"]] == district, ])
    if (district_size >= 1000) {
      major_districts <- c(major_districts, district)
    }
  }
  
  district_trends <- data.frame()
  
  for (district in major_districts) {
    district_data <- data[data[["DISTRICT_NAME"]] == district, ]
    
    for (year in years) {
      year_district <- district_data[district_data[["YEAR"]] == year, ]
      
      if (nrow(year_district) > 0) {
        total <- nrow(year_district)
        chronic <- sum(year_district[["CHRONIC_ABSENT"]])
        rate <- chronic / total
        
        district_row <- data.frame(
          DISTRICT_NAME = district,
          YEAR = year,
          TOTAL_STUDENTS = total,
          CHRONIC_ABSENT = chronic,
          CHRONIC_RATE = rate,
          stringsAsFactors = FALSE
        )
        
        district_trends <- rbind(district_trends, district_row)
      }
    }
  }
  
  # Demographic trends
  demo_cols <- c("MALE", "FEMALE", "HISPANIC", "WHITE", "ASIAN", 
                 "BLACK", "ELL", "DISADVANTAGE", "DISABILITY")
  demo_names <- c("male", "female", "hispanic", "white", "asian",
                  "black", "ell", "disadvantaged", "disability")
  
  demographic_trends <- list()
  
  for (i in seq_along(demo_cols)) {
    col <- demo_cols[i]
    name <- demo_names[i]
    
    demo_data <- data.frame()
    
    for (year in years) {
      year_data <- data[data[["YEAR"]] == year, ]
      
      # Group members
      group_data <- year_data[year_data[[col]] == 1, ]
      
      if (nrow(group_data) > 0) {
        total <- nrow(group_data)
        chronic <- sum(group_data[["CHRONIC_ABSENT"]])
        rate <- chronic / total
        
        # State rate for this year
        state_rate <- sum(year_data[["CHRONIC_ABSENT"]]) / 
                     nrow(year_data)
        gap <- rate - state_rate
        
        demo_row <- data.frame(
          YEAR = year,
          TOTAL_STUDENTS = total,
          CHRONIC_ABSENT = chronic,
          CHRONIC_RATE = rate,
          STATE_RATE = state_rate,
          GAP = gap,
          stringsAsFactors = FALSE
        )
        
        demo_data <- rbind(demo_data, demo_row)
      }
    }
    
    demographic_trends[[name]] <- demo_data
  }
  
  # Grade trends
  grades <- sort(unique(data[["GRADE"]]))
  grade_trends <- data.frame()
  
  for (grade in grades) {
    grade_data <- data[data[["GRADE"]] == grade, ]
    
    for (year in years) {
      year_grade <- grade_data[grade_data[["YEAR"]] == year, ]
      
      if (nrow(year_grade) > 0) {
        total <- nrow(year_grade)
        chronic <- sum(year_grade[["CHRONIC_ABSENT"]])
        rate <- chronic / total
        
        grade_row <- data.frame(
          GRADE = grade,
          YEAR = year,
          TOTAL_STUDENTS = total,
          CHRONIC_ABSENT = chronic,
          CHRONIC_RATE = rate,
          stringsAsFactors = FALSE
        )
        
        grade_trends <- rbind(grade_trends, grade_row)
      }
    }
  }
  
  # Summary statistics
  most_improved_county <- county_changes[
    which.min(county_changes[["CHANGE"]]), 
  ][["COUNTY_NAME"]]
  
  most_declined_county <- county_changes[
    which.max(county_changes[["CHANGE"]]), 
  ][["COUNTY_NAME"]]
  
  # Find demographic group with most improvement
  demo_improvements <- numeric(length(demo_names))
  names(demo_improvements) <- demo_names
  
  for (i in seq_along(demo_names)) {
    name <- demo_names[i]
    demo_trend <- demographic_trends[[name]]
    
    if (nrow(demo_trend) >= 2) {
      # Use first and last available years for each demographic
      demo_sorted <- demo_trend[order(demo_trend[["YEAR"]]), ]
      first_rate <- demo_sorted[["CHRONIC_RATE"]][1]
      last_rate <- demo_sorted[["CHRONIC_RATE"]][nrow(demo_sorted)]
      
      # Improvement = reduction in rate (first - last)
      demo_improvements[i] <- first_rate - last_rate
    }
  }
  
  most_improved_demo <- names(which.max(demo_improvements))
  most_declined_demo <- names(which.min(demo_improvements))
  
  # Return results
  result <- list(
    state_trends = state_trends,
    county_trends = county_trends,
    county_changes = county_changes,
    district_trends = district_trends,
    demographic_trends = demographic_trends,
    grade_trends = grade_trends,
    summary = list(
      years_analyzed = years,
      baseline_year = baseline_year,
      current_year = current_year,
      most_improved_county = most_improved_county,
      most_declined_county = most_declined_county,
      most_improved_demo = most_improved_demo,
      most_declined_demo = most_declined_demo
    )
  )
  
  return(result)
}
