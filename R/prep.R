#' Prepare Comprehensive Chronic Absenteeism Analysis
#' 
#' Orchestrates all chronic absenteeism analysis functions to generate
#' a complete set of results for reporting. Processes student-level data
#' through all geographic levels and analytical perspectives.
#' 
#' @param data Student-level data frame with demographic columns and CHRONIC_ABSENT indicator
#' @return List containing all analysis results for report generation
#' @keywords internal
prep <- function(data) {
  
  cat("Starting comprehensive chronic absenteeism analysis...\n")
  
  # Validate required columns
  required_cols <- c("CHRONIC_ABSENT", "YEAR", "COUNTY_ID", "DISTRICT_ID", "SCHOOL_ID",
                     "COUNTY_NAME", "DISTRICT_NAME", "SCHOOL_NAME", "STUDENT_ID",
                     "HISPANIC", "WHITE", "ASIAN", "BLACK", "ELL", "DISADVANTAGE", 
                     "DISABILITY", "MALE", "FEMALE")
  
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  cat("Data validation complete. Processing", nrow(data), "student records...\n")
  
  # Calculate means at all geographic levels
  cat("\n=== CALCULATING GEOGRAPHIC MEANS ===\n")
  state_means <- group_means(data)
  county_means_result <- county_means(data)
  district_means_result <- district_means(data)
  school_means_result <- school_means(data)
  
  # Calculate year-to-year changes
  cat("\n=== CALCULATING ANNUAL CHANGES ===\n")
  annual_changes <- annual(state_means, county_means_result, district_means_result, school_means_result)
  
  # Calculate pre-pandemic comparisons
  cat("\n=== CALCULATING PANDEMIC COMPARISONS ===\n")
  pandemic_changes <- pandemic(state_means, county_means_result, district_means_result, school_means_result)
  
  # Calculate school demographics
  cat("\n=== CALCULATING DEMOGRAPHICS ===\n")
  demographics_result <- demo(data)
  
  # Calculate tercile analysis
  cat("\n=== CALCULATING TERCILE ANALYSIS ===\n")
  tercile_result <- tercile(school_means_result, demographics_result)
  
  # Generate distribution plots
  cat("\n=== GENERATING DISTRIBUTION PLOTS ===\n")
  distribution_plots <- school_distribution_plots(school_means_result)
  
  # Calculate summary statistics
  cat("\n=== CALCULATING SUMMARY STATISTICS ===\n")
  summary_stats <- calculate_summary_stats(state_means, county_means_result, 
                                          district_means_result, school_means_result)
  
  # Compile comprehensive results
  results <- list(
    # Metadata
    analysis_date = Sys.time(),
    total_students = nrow(data),
    years_analyzed = sort(unique(data$YEAR)),
    first_year = min(data$YEAR),
    current_year = max(data$YEAR),
    
    # Geographic means
    state_means = state_means,
    county_means = county_means_result,
    district_means = district_means_result,
    school_means = school_means_result,
    
    # Change analyses
    annual_changes = annual_changes,
    pandemic_changes = pandemic_changes,
    
    # Demographics and interactions
    demographics = demographics_result,
    tercile_analysis = tercile_result,
    
    # Visualizations
    distribution_plots = distribution_plots,
    
    # Summary statistics
    summary_statistics = summary_stats
  )
  
  cat("\n=== ANALYSIS COMPLETE ===\n")
  cat("Results generated for", length(results$years_analyzed), "years\n")
  cat("Geographic levels: State, County (", length(unique(county_means_result$COUNTY_ID)), "), ",
      "District (", length(unique(district_means_result$DISTRICT_ID)), "), ",
      "School (", length(unique(school_means_result$SCHOOL_ID)), ")\n", sep = "")
  
  return(results)
}

#' Calculate Summary Statistics Across All Levels
#' 
#' Helper function to calculate key summary statistics for the analysis
#' 
#' @param state_data State-level means
#' @param county_data County-level means  
#' @param district_data District-level means
#' @param school_data School-level means
#' @return List of summary statistics
#' @keywords internal
calculate_summary_stats <- function(state_data, county_data, district_data, school_data) {
  
  # Get most recent year data for current state summary
  current_year <- max(state_data$YEAR)
  
  # State-level current rates by subgroup
  current_state <- state_data[state_data$YEAR == current_year, ]
  
  # District-level summaries for current year
  current_districts <- district_data[district_data$YEAR == current_year & 
                                   district_data$SUBGROUP == "ALL", ]
  
  # School-level summaries for current year  
  current_schools <- school_data[school_data$YEAR == current_year & 
                                school_data$SUBGROUP == "ALL", ]
  
  # Calculate key statistics
  stats <- list(
    # Current state overview
    current_year = current_year,
    state_all_rate = current_state$absenteeism_rate[current_state$SUBGROUP == "ALL"],
    
    # District-level stats
    total_districts = nrow(current_districts),
    district_median = round(median(current_districts$absenteeism_rate, na.rm = TRUE), 2),
    district_range = range(current_districts$absenteeism_rate, na.rm = TRUE),
    districts_above_20 = sum(current_districts$absenteeism_rate > 20, na.rm = TRUE),
    districts_above_30 = sum(current_districts$absenteeism_rate > 30, na.rm = TRUE),
    
    # School-level stats
    total_schools = nrow(current_schools),
    school_median = round(median(current_schools$absenteeism_rate, na.rm = TRUE), 2),
    school_range = range(current_schools$absenteeism_rate, na.rm = TRUE),
    schools_above_20 = sum(current_schools$absenteeism_rate > 20, na.rm = TRUE),
    schools_above_30 = sum(current_schools$absenteeism_rate > 30, na.rm = TRUE),
    
    # Extreme examples
    worst_district = current_districts[which.max(current_districts$absenteeism_rate), ],
    best_district = current_districts[which.min(current_districts$absenteeism_rate), ],
    worst_school = current_schools[which.max(current_schools$absenteeism_rate), ],
    best_school = current_schools[which.min(current_schools$absenteeism_rate), ]
  )
  
  return(stats)
}

