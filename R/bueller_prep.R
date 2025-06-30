#' Prepare all analysis components for engagement report
#'
#' This function does all the heavy analytical work that was previously
#' embedded in the Quarto template. It returns a structured list of results
#' that can be passed to the template for formatting.
#'
#' @param data Raw merged dataset
#' @param min_enrollment Minimum enrollment threshold (default: 30)
#'
#' @return List containing all analysis results for report generation
#' @export
bueller_prep <- function(data, min_enrollment = 30) {
  
  # Filter out County Offices of Education
  county_mask <- !grepl("County Office of Education|County Department of Education", 
                       data$DISTRICT_NAME, ignore.case = TRUE)
  df <- data[county_mask, ]
  
  # Calculate district-level data using existing functions
  district_data <- calc_district_metrics(df)
  school_variation <- calc_school_variation(df)
  district_trends <- prepare_district_trends(district_data)
  
  # Filter by minimum enrollment
  district_trends <- district_trends[district_trends$students_enrolled >= min_enrollment, ]
  
  # Determine analysis year (always use most recent)
  current_year <- max(district_trends$YEAR, na.rm = TRUE)
  first_year <- min(district_trends$YEAR, na.rm = TRUE)
  
  # === BASIC SUMMARY STATISTICS ===
  current_data <- district_trends[
    district_trends$YEAR == current_year & 
    district_trends$DISAGGREGATED_GROUP == "All_Students", 
  ]
  
  ela_data <- current_data[current_data$SUBJECT == "ELA", ]
  math_data <- current_data[current_data$SUBJECT == "MATH", ]
  
  summary_stats <- rbind(
    data.frame(
      SUBJECT = "ELA",
      total_districts = nrow(ela_data),
      median_participation = round(median(ela_data$participation_rate, na.rm = TRUE), 2),
      median_absenteeism = round(median(ela_data$chronic_absent_rate, na.rm = TRUE), 2),
      pct_below_95 = round(mean(ela_data$participation_rate < 95, na.rm = TRUE) * 100, 2),
      pct_above_20 = round(mean(ela_data$chronic_absent_rate > 20, na.rm = TRUE) * 100, 2),
      total_enrollment = sum(ela_data$students_enrolled, na.rm = TRUE)
    ),
    data.frame(
      SUBJECT = "MATH",
      total_districts = nrow(math_data),
      median_participation = round(median(math_data$participation_rate, na.rm = TRUE), 2),
      median_absenteeism = round(median(math_data$chronic_absent_rate, na.rm = TRUE), 2),
      pct_below_95 = round(mean(math_data$participation_rate < 95, na.rm = TRUE) * 100, 2),
      pct_above_20 = round(mean(math_data$chronic_absent_rate > 20, na.rm = TRUE) * 100, 2),
      total_enrollment = sum(math_data$students_enrolled, na.rm = TRUE)
    )
  )
  
  # === YEAR-OVER-YEAR TRENDS ===
  yearly_trends_ela <- calc_participation_trends(district_trends, "ELA")
  yearly_trends_math <- calc_participation_trends(district_trends, "MATH")
  yearly_absent_ela <- calc_absenteeism_trends(district_trends, "ELA")
  yearly_absent_math <- calc_absenteeism_trends(district_trends, "MATH")
  
  # === SCHOOL-LEVEL DRIVER ANALYSIS ===
  current_year_data <- df[
    df$YEAR == current_year & 
    df$DISAGGREGATED_GROUP == "All_Students", 
  ]
  
  # Participation drivers by subject
  participation_drivers <- aggregate(
    PARTICIPATION_RATE ~ COUNTY_ID + DISTRICT_ID + COUNTY_NAME + DISTRICT_NAME + SUBJECT,
    data = current_year_data,
    FUN = function(x) {
      if (length(x) >= 3) {
        c(
          total_schools = length(x),
          schools_below_90_participation = sum(x < 90, na.rm = TRUE),
          min_participation = min(x, na.rm = TRUE),
          max_participation = max(x, na.rm = TRUE),
          participation_range = max(x, na.rm = TRUE) - min(x, na.rm = TRUE)
        )
      } else {
        c(0, 0, 0, 0, 0)
      }
    }
  )
  
  # Flatten participation results
  if (nrow(participation_drivers) > 0) {
    part_matrix <- participation_drivers$PARTICIPATION_RATE
    participation_drivers <- data.frame(
      participation_drivers[1:5],
      total_schools = part_matrix[, 1],
      schools_below_90_participation = part_matrix[, 2],
      min_participation = part_matrix[, 3],
      max_participation = part_matrix[, 4],
      participation_range = part_matrix[, 5]
    )
    
    participation_drivers <- participation_drivers[participation_drivers$total_schools >= 3, ]
    participation_drivers$pct_schools_low_participation <- round(
      participation_drivers$schools_below_90_participation / participation_drivers$total_schools * 100, 2
    )
    participation_drivers$driven_by_few_schools_participation <- 
      participation_drivers$schools_below_90_participation > 0 & 
      participation_drivers$pct_schools_low_participation <= 33
  }
  
  # Absenteeism drivers (aggregated across subjects)
  school_absent_data <- aggregate(
    CHRONIC_ABSENT_RATE ~ COUNTY_ID + DISTRICT_ID + SCHOOL_ID,
    data = current_year_data,
    FUN = function(x) x[1]
  )
  
  # Add district names
  district_info <- unique(current_year_data[c("COUNTY_ID", "DISTRICT_ID", "COUNTY_NAME", "DISTRICT_NAME")])
  school_key <- paste(school_absent_data$COUNTY_ID, school_absent_data$DISTRICT_ID)
  district_key <- paste(district_info$COUNTY_ID, district_info$DISTRICT_ID)
  match_idx <- match(school_key, district_key)
  school_absent_data$COUNTY_NAME <- district_info$COUNTY_NAME[match_idx]
  school_absent_data$DISTRICT_NAME <- district_info$DISTRICT_NAME[match_idx]
  
  # Aggregate absenteeism by district
  absenteeism_drivers <- aggregate(
    CHRONIC_ABSENT_RATE ~ COUNTY_ID + DISTRICT_ID + COUNTY_NAME + DISTRICT_NAME,
    data = school_absent_data,
    FUN = function(x) {
      if (length(x) >= 3) {
        c(
          total_schools = length(x),
          schools_above_30_absenteeism = sum(x > 30, na.rm = TRUE),
          min_absenteeism = min(x, na.rm = TRUE),
          max_absenteeism = max(x, na.rm = TRUE),
          absenteeism_range = max(x, na.rm = TRUE) - min(x, na.rm = TRUE)
        )
      } else {
        c(0, 0, 0, 0, 0)
      }
    }
  )
  
  # Flatten absenteeism results
  if (nrow(absenteeism_drivers) > 0) {
    absent_matrix <- absenteeism_drivers$CHRONIC_ABSENT_RATE
    absenteeism_drivers <- data.frame(
      absenteeism_drivers[1:4],
      total_schools = absent_matrix[, 1],
      schools_above_30_absenteeism = absent_matrix[, 2],
      min_absenteeism = absent_matrix[, 3],
      max_absenteeism = absent_matrix[, 4],
      absenteeism_range = absent_matrix[, 5]
    )
    
    absenteeism_drivers <- absenteeism_drivers[absenteeism_drivers$total_schools >= 3, ]
    absenteeism_drivers$pct_schools_high_absenteeism <- round(
      absenteeism_drivers$schools_above_30_absenteeism / absenteeism_drivers$total_schools * 100, 2
    )
    absenteeism_drivers$driven_by_few_schools_absenteeism <- 
      absenteeism_drivers$schools_above_30_absenteeism > 0 & 
      absenteeism_drivers$pct_schools_high_absenteeism <= 33
  }
  
  # Count driven districts
  ela_driven_participation <- if (exists("participation_drivers") && nrow(participation_drivers) > 0) {
    sum(participation_drivers$SUBJECT == "ELA" & participation_drivers$driven_by_few_schools_participation, na.rm = TRUE)
  } else { 0 }
  
  math_driven_participation <- if (exists("participation_drivers") && nrow(participation_drivers) > 0) {
    sum(participation_drivers$SUBJECT == "MATH" & participation_drivers$driven_by_few_schools_participation, na.rm = TRUE)
  } else { 0 }
  
  districts_driven_absenteeism <- if (exists("absenteeism_drivers") && nrow(absenteeism_drivers) > 0) {
    sum(absenteeism_drivers$driven_by_few_schools_absenteeism, na.rm = TRUE)
  } else { 0 }
  
  # === STUDENT GROUP ANALYSIS ===
  group_data <- district_trends[
    district_trends$YEAR == current_year & 
    district_trends$DISAGGREGATED_GROUP != "All_Students", 
  ]
  
  all_students_current <- district_trends[
    district_trends$YEAR == current_year & 
    district_trends$DISAGGREGATED_GROUP == "All_Students", 
    c("COUNTY_ID", "DISTRICT_ID", "SUBJECT", "participation_rate", "chronic_absent_rate")
  ]
  
  # Initialize defaults
  largest_participation_gap <- data.frame(
    DISAGGREGATED_GROUP = "None", DISTRICT_NAME = "None",
    participation_rate = 0, district_participation = 0, participation_gap = 0
  )
  largest_absenteeism_gap <- data.frame(
    DISAGGREGATED_GROUP = "None", DISTRICT_NAME = "None",
    chronic_absent_rate = 0, district_absenteeism = 0, absenteeism_gap = 0
  )
  significant_participation_gaps <- 0
  significant_absenteeism_gaps <- 0
  
  # Calculate gaps if data exists
  if (nrow(group_data) > 0 && nrow(all_students_current) > 0) {
    group_key <- paste(group_data$COUNTY_ID, group_data$DISTRICT_ID, group_data$SUBJECT)
    all_key <- paste(all_students_current$COUNTY_ID, all_students_current$DISTRICT_ID, all_students_current$SUBJECT)
    match_indices <- match(group_key, all_key)
    
    group_data$district_participation <- all_students_current$participation_rate[match_indices]
    group_data$district_absenteeism <- all_students_current$chronic_absent_rate[match_indices]
    group_data$participation_gap <- group_data$district_participation - group_data$participation_rate
    group_data$absenteeism_gap <- group_data$chronic_absent_rate - group_data$district_absenteeism
    
    # Filter by enrollment
    group_analysis <- group_data[group_data$students_enrolled >= 10, ]
    
    if (nrow(group_analysis) > 0) {
      largest_participation_gap <- group_analysis[order(-group_analysis$participation_gap, na.last = TRUE), ][1, ]
      largest_absenteeism_gap <- group_analysis[order(-group_analysis$absenteeism_gap, na.last = TRUE), ][1, ]
      significant_participation_gaps <- sum(group_analysis$participation_gap >= 15, na.rm = TRUE)
      significant_absenteeism_gaps <- sum(group_analysis$absenteeism_gap >= 15, na.rm = TRUE)
    }
  }
  
  # === EXTREME DISTRICTS ===
  if (nrow(ela_data) > 0) {
    worst_participation <- ela_data[order(ela_data$participation_rate), ][1, ]
    best_participation <- ela_data[order(-ela_data$participation_rate), ][1, ]
    worst_absenteeism <- ela_data[order(-ela_data$chronic_absent_rate), ][1, ]
    best_absenteeism <- ela_data[order(ela_data$chronic_absent_rate), ][1, ]
  } else {
    default_row <- data.frame(participation_rate = 0, chronic_absent_rate = 0, DISTRICT_NAME = "None")
    worst_participation <- best_participation <- worst_absenteeism <- best_absenteeism <- default_row
  }
  
  # === COMPILE RESULTS ===
  results <- list(
    # Basic info
    current_year = current_year,
    first_year = first_year,
    
    # Summary statistics
    summary_stats = summary_stats,
    ela_summary = summary_stats[summary_stats$SUBJECT == "ELA", ],
    math_summary = summary_stats[summary_stats$SUBJECT == "MATH", ],
    
    # Trends
    yearly_trends_ela = yearly_trends_ela,
    yearly_trends_math = yearly_trends_math,
    yearly_absent_ela = yearly_absent_ela,
    yearly_absent_math = yearly_absent_math,
    
    # School drivers
    participation_drivers = if(exists("participation_drivers")) participation_drivers else data.frame(),
    absenteeism_drivers = if(exists("absenteeism_drivers")) absenteeism_drivers else data.frame(),
    ela_driven_participation = ela_driven_participation,
    math_driven_participation = math_driven_participation,
    districts_driven_absenteeism = districts_driven_absenteeism,
    
    # Student groups
    largest_participation_gap = largest_participation_gap,
    largest_absenteeism_gap = largest_absenteeism_gap,
    significant_participation_gaps = significant_participation_gaps,
    significant_absenteeism_gaps = significant_absenteeism_gaps,
    
    # Extremes
    worst_participation = worst_participation,
    best_participation = best_participation,
    worst_absenteeism = worst_absenteeism,
    best_absenteeism = best_absenteeism
  )
  
  return(results)
}