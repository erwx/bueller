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
prep <- function(data, min_enrollment = 30) {
  
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

  # === PANDEMIC RECOVERY ANALYSIS ===
  pandemic_recovery_ela <- calc_pandemic_recovery(district_trends, "ELA")
  pandemic_recovery_math <- calc_pandemic_recovery(district_trends, "MATH")
  
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

    # Pandemic recovery analysis
    pandemic_recovery_ela = pandemic_recovery_ela,
    pandemic_recovery_math = pandemic_recovery_math,
    
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



# Aggregates school-level data to district level,
# computing participation rates and chronic
# absenteeism rates with proper enrollment weighting
calc_district_metrics <- function(data) {
  # Use aggregate efficiently
  agg_data <- aggregate(
    cbind(CHRONIC_ABSENT_ELIGIBLE, CHRONIC_ABSENT_COUNT, 
          STUDENTS_ENROLLED, STUDENTS_TESTED) ~ 
    COUNTY_ID + DISTRICT_ID + COUNTY_NAME + DISTRICT_NAME + 
    DISAGGREGATED_GROUP + SUBJECT + YEAR,
    data = data,
    FUN = sum,
    na.rm = TRUE
  )
  
  # Count schools per group
  school_counts <- aggregate(
    SCHOOL_ID ~ COUNTY_ID + DISTRICT_ID + COUNTY_NAME + 
    DISTRICT_NAME + DISAGGREGATED_GROUP + SUBJECT + YEAR,
    data = data,
    FUN = function(x) length(unique(x))
  )
  
  # Merge school counts
  agg_data$schools_count <- school_counts$SCHOOL_ID[
    match(
      paste(agg_data$COUNTY_ID, agg_data$DISTRICT_ID, 
            agg_data$DISAGGREGATED_GROUP, agg_data$SUBJECT, 
            agg_data$YEAR),
      paste(school_counts$COUNTY_ID, school_counts$DISTRICT_ID,
            school_counts$DISAGGREGATED_GROUP, school_counts$SUBJECT,
            school_counts$YEAR)
    )
  ]
  
  # Calculate rates
  agg_data$chronic_absent_rate <- round(
    agg_data$CHRONIC_ABSENT_COUNT / 
    agg_data$CHRONIC_ABSENT_ELIGIBLE * 100, 2
  )
  
  agg_data$participation_rate <- round(
    agg_data$STUDENTS_TESTED / 
    agg_data$STUDENTS_ENROLLED * 100, 2
  )
  
  # Rename columns
  names(agg_data)[names(agg_data) == "CHRONIC_ABSENT_ELIGIBLE"] <- 
    "chronic_absent_eligible"
  names(agg_data)[names(agg_data) == "CHRONIC_ABSENT_COUNT"] <- 
    "chronic_absent_count"
  names(agg_data)[names(agg_data) == "STUDENTS_ENROLLED"] <- 
    "students_enrolled"
  names(agg_data)[names(agg_data) == "STUDENTS_TESTED"] <- 
    "students_tested"
  
  agg_data
}



# Computes variation statistics across schools
# within each district - OPTIMIZED VERSION
calc_school_variation <- function(data) {
  # Step 1: Calculate school-level weighted means using aggregate
  school_chronic <- aggregate(
    cbind(CHRONIC_ABSENT_RATE * CHRONIC_ABSENT_ELIGIBLE, 
          CHRONIC_ABSENT_ELIGIBLE) ~ 
    COUNTY_ID + DISTRICT_ID + SCHOOL_ID + YEAR + SUBJECT,
    data = data,
    FUN = sum,
    na.rm = TRUE
  )
  
  school_chronic$chronic_absent_rate <- 
    school_chronic[, 6] / school_chronic[, 7]
  
  school_participation <- aggregate(
    cbind(PARTICIPATION_RATE * STUDENTS_ENROLLED, 
          STUDENTS_ENROLLED) ~ 
    COUNTY_ID + DISTRICT_ID + SCHOOL_ID + YEAR + SUBJECT,
    data = data,
    FUN = sum,
    na.rm = TRUE
  )
  
  school_participation$participation_rate <- 
    school_participation[, 6] / school_participation[, 7]
  
  # Merge school data
  school_key <- paste(
    school_chronic$COUNTY_ID, school_chronic$DISTRICT_ID,
    school_chronic$SCHOOL_ID, school_chronic$YEAR, 
    school_chronic$SUBJECT
  )
  
  part_key <- paste(
    school_participation$COUNTY_ID, school_participation$DISTRICT_ID,
    school_participation$SCHOOL_ID, school_participation$YEAR,
    school_participation$SUBJECT
  )
  
  match_idx <- match(school_key, part_key)
  school_data <- data.frame(
    school_chronic[1:5],
    chronic_absent_rate = school_chronic$chronic_absent_rate,
    participation_rate = school_participation$participation_rate[match_idx]
  )
  
  # Step 2: Aggregate to district level using efficient operations
  district_stats <- aggregate(
    cbind(participation_rate, chronic_absent_rate) ~ 
    COUNTY_ID + DISTRICT_ID + YEAR + SUBJECT,
    data = school_data,
    FUN = function(x) {
      if (length(x) >= 3) {
        c(
          schools_count = length(x),
          min_val = min(x, na.rm = TRUE),
          max_val = max(x, na.rm = TRUE),
          sd_val = sd(x, na.rm = TRUE),
          below_95 = sum(x < 95, na.rm = TRUE),
          above_20 = sum(x > 20, na.rm = TRUE),
          above_30 = sum(x > 30, na.rm = TRUE)
        )
      } else {
        c(0, 0, 0, 0, 0, 0, 0)
      }
    }
  )
  
  # Extract results and filter
  part_results <- district_stats$participation_rate
  chronic_results <- district_stats$chronic_absent_rate
  
  result <- data.frame(
    district_stats[1:4],
    schools_count = part_results[, 1],
    participation_min = part_results[, 2],
    participation_max = part_results[, 3],
    participation_sd = round(part_results[, 4], 2),
    schools_below_95 = part_results[, 5],
    chronic_absent_min = chronic_results[, 2],
    chronic_absent_max = chronic_results[, 3],
    chronic_absent_sd = round(chronic_results[, 4], 2),
    schools_above_20 = chronic_results[, 6],
    schools_above_30 = chronic_results[, 7]
  )
  
  # Filter to districts with at least 3 schools
  result[result$schools_count >= 3, ]
}



# Filter district data to include only those
# with sufficient years of data
prepare_district_trends <- function(district_data, min_years = 3) {
  # Count years per group efficiently
  year_counts <- aggregate(
    YEAR ~ COUNTY_ID + DISTRICT_ID + DISAGGREGATED_GROUP + SUBJECT,
    data = district_data,
    FUN = function(x) length(unique(x))
  )
  
  # Create merge keys
  data_key <- paste(
    district_data$COUNTY_ID, district_data$DISTRICT_ID,
    district_data$DISAGGREGATED_GROUP, district_data$SUBJECT
  )
  
  count_key <- paste(
    year_counts$COUNTY_ID, year_counts$DISTRICT_ID,
    year_counts$DISAGGREGATED_GROUP, year_counts$SUBJECT
  )
  
  # Add year counts to original data
  district_data$year_count <- year_counts$YEAR[match(data_key, count_key)]
  
  # Filter by minimum years
  district_data[district_data$year_count >= min_years, ]
}



# Calculate student group participation gaps
# Identifies student groups with largest participation gaps 
# relative to district average
calc_group_participation_gaps <- function(
 district_trends,
 subject,
 output_year,
 min_gap = 5,
 top_n = 15
) {
 # Filter to target year/subject, exclude All_Students
 group_data <- district_trends[
   district_trends$YEAR == output_year &
   district_trends$SUBJECT == subject &
   district_trends$DISAGGREGATED_GROUP != "All_Students", 
 ]
 
 # Get district averages (All_Students only)
 district_avg <- district_trends[
   district_trends$YEAR == output_year &
   district_trends$SUBJECT == subject &
   district_trends$DISAGGREGATED_GROUP == "All_Students",
   c("COUNTY_ID", "DISTRICT_ID", "participation_rate")
 ]
 names(district_avg)[3] <- "district_avg"
 
 # Merge district averages
 merge_key <- paste(
   group_data$COUNTY_ID,
   group_data$DISTRICT_ID
 )
 avg_key <- paste(
   district_avg$COUNTY_ID,
   district_avg$DISTRICT_ID
 )
 
 # Add district averages to group data
 group_data$district_avg <- district_avg$district_avg[
   match(merge_key, avg_key)
 ]
 
 # Calculate differences
 group_data$difference <- round(
   group_data$participation_rate - group_data$district_avg, 2
 )
 
 # Filter by minimum gap
 significant_gaps <- group_data[
   abs(group_data$difference) >= min_gap, 
 ]
 
 # Sort by difference (ascending)
 sorted_data <- significant_gaps[
   order(significant_gaps$difference), 
 ]
 
 # Select columns and limit rows
 result_cols <- c(
   "COUNTY_NAME",
   "DISTRICT_NAME", 
   "DISAGGREGATED_GROUP",
   "participation_rate",
   "district_avg",
   "difference"
 )
 
 result <- sorted_data[1:min(top_n, nrow(sorted_data)), 
                      result_cols]
 
 result
}



# Calculate student group absenteeism gaps
# Identifies student groups with largest absenteeism gaps 
# relative to district average
calc_group_absenteeism_gaps <- function(
 district_trends,
 subject,
 output_year,
 min_gap = 5,
 top_n = 15
) {
 # Filter to target year/subject, exclude All_Students
 group_data <- district_trends[
   district_trends$YEAR == output_year &
   district_trends$SUBJECT == subject &
   district_trends$DISAGGREGATED_GROUP != "All_Students", 
 ]
 
 # Get district averages (All_Students only)
 district_avg <- district_trends[
   district_trends$YEAR == output_year &
   district_trends$SUBJECT == subject &
   district_trends$DISAGGREGATED_GROUP == "All_Students",
   c("COUNTY_ID", "DISTRICT_ID", "chronic_absent_rate")
 ]
 names(district_avg)[3] <- "district_avg"
 
 # Merge district averages
 merge_key <- paste(
   group_data$COUNTY_ID,
   group_data$DISTRICT_ID
 )
 avg_key <- paste(
   district_avg$COUNTY_ID,
   district_avg$DISTRICT_ID
 )
 
 # Add district averages to group data
 group_data$district_avg <- district_avg$district_avg[
   match(merge_key, avg_key)
 ]
 
 # Calculate differences
 group_data$difference <- round(
   group_data$chronic_absent_rate - group_data$district_avg, 2
 )
 
 # Filter by minimum gap
 significant_gaps <- group_data[
   abs(group_data$difference) >= min_gap, 
 ]
 
 # Sort by difference (descending)
 sorted_data <- significant_gaps[
   order(-significant_gaps$difference), 
 ]
 
 # Select columns and limit rows
 result_cols <- c(
   "COUNTY_NAME",
   "DISTRICT_NAME", 
   "DISAGGREGATED_GROUP",
   "chronic_absent_rate",
   "district_avg",
   "difference"
 )
 
 result <- sorted_data[1:min(top_n, nrow(sorted_data)), 
                      result_cols]
 
 result
}



# Calculate districts with dual engagement challenges
# Identifies districts with both low participation and 
# high absenteeism
calc_dual_challenges <- function(
 district_trends,
 subject,
 output_year,
 participation_threshold = 95,
 absenteeism_threshold = 20,
 top_n = 15
) {
 # Filter to All_Students for target year/subject
 district_data <- district_trends[
   district_trends$DISAGGREGATED_GROUP == "All_Students" &
   district_trends$YEAR == output_year &
   district_trends$SUBJECT == subject, 
 ]
 
 # Create flags
 district_data$participation_flag <- 
   district_data$participation_rate < participation_threshold
 district_data$absenteeism_flag <- 
   district_data$chronic_absent_rate > absenteeism_threshold
 district_data$dual_challenge <- 
   district_data$participation_flag & 
   district_data$absenteeism_flag
 
 # Filter to dual challenges only
 dual_districts <- district_data[
   district_data$dual_challenge, 
 ]
 
 # Sort by participation (ascending), then absenteeism (descending)
 sorted_data <- dual_districts[
   order(
     dual_districts$participation_rate,
     -dual_districts$chronic_absent_rate
   ), 
 ]
 
 # Select columns and limit rows
 result_cols <- c(
   "COUNTY_NAME",
   "DISTRICT_NAME",
   "participation_rate",
   "chronic_absent_rate",
   "students_enrolled",
   "schools_count"
 )
 
 result <- sorted_data[1:min(top_n, nrow(sorted_data)), 
                      result_cols]
 
 result
}



# Identifies student groups with both low
# participation and high absenteeism
calc_group_dual_issues <- function(
 district_trends,
 subject,
 output_year,
 participation_threshold = 90,
 absenteeism_threshold = 25,
 top_n = 15
) {
 # Filter to target year/subject, exclude All_Students
 group_data <- district_trends[
   district_trends$YEAR == output_year &
   district_trends$SUBJECT == subject &
   district_trends$DISAGGREGATED_GROUP != "All_Students", 
 ]
 
 # Create flags
 group_data$low_participation <- 
   group_data$participation_rate < participation_threshold
 group_data$high_absenteeism <- 
   group_data$chronic_absent_rate > absenteeism_threshold
 group_data$compound_issue <- 
   group_data$low_participation & group_data$high_absenteeism
 
 # Filter to compound issues only
 compound_groups <- group_data[group_data$compound_issue, ]
 
 # Sort by participation rate (ascending)
 sorted_data <- compound_groups[
   order(compound_groups$participation_rate), 
 ]
 
 # Select columns and limit rows
 result_cols <- c(
   "COUNTY_NAME",
   "DISTRICT_NAME",
   "DISAGGREGATED_GROUP",
   "participation_rate",
   "chronic_absent_rate",
   "students_enrolled"
 )
 
 result <- sorted_data[1:min(top_n, nrow(sorted_data)), 
                      result_cols]
 
 result
}



# Calculate correlation between participation and absenteeism
# Computes year-by-year correlations between participation 
# and attendance
calc_participation_absenteeism_correlation <- function(
 district_trends,
 subject
) {
 # Filter to All_Students for target subject
 correlation_data <- district_trends[
   district_trends$DISAGGREGATED_GROUP == "All_Students" &
   district_trends$SUBJECT == subject, 
   c("COUNTY_ID", "DISTRICT_ID", "YEAR", 
     "participation_rate", "chronic_absent_rate")
 ]
 
 # Remove rows with missing values
 complete_rows <- complete.cases(correlation_data)
 correlation_data <- correlation_data[complete_rows, ]
 
 # Calculate yearly correlations
 years <- unique(correlation_data$YEAR)
 yearly_results <- data.frame()
 
 for (year in years) {
   year_data <- correlation_data[
     correlation_data$YEAR == year, 
   ]
   
   if (nrow(year_data) > 1) {
     year_cor <- cor(
       year_data$participation_rate,
       year_data$chronic_absent_rate
     ) * -1
     
     year_row <- data.frame(
       YEAR         = year,
       observations = nrow(year_data),
       correlation  = round(year_cor, 2)
     )
     
     yearly_results <- rbind(yearly_results, year_row)
   }
 }
 
 # Calculate overall correlation
 overall_correlation <- round(
   cor(
     correlation_data$participation_rate,
     correlation_data$chronic_absent_rate
   ) * -1, 2
 )
 
 list(
   yearly  = yearly_results,
   overall = overall_correlation
 )
}



# Calculate variance decomposition
# Decomposes variance into between-district and 
# within-district components
calc_variance_decomposition <- function(data) {
 # Get unique year-subject combinations
 year_subject <- unique(data[, c("YEAR", "SUBJECT")])
 
 result <- data.frame()
 
 for (i in seq_len(nrow(year_subject))) {
   current_data <- data[
     data$YEAR == year_subject$YEAR[i] &
     data$SUBJECT == year_subject$SUBJECT[i], 
   ]
   
   if (nrow(current_data) > 0) {
     # Calculate district means for participation
     district_ids <- unique(current_data$DISTRICT_ID)
     dist_means_part <- numeric(length(district_ids))
     
     for (j in seq_along(district_ids)) {
       district_data <- current_data[
         current_data$DISTRICT_ID == district_ids[j], 
       ]
       dist_means_part[j] <- mean(
         district_data$PARTICIPATION_RATE, 
         na.rm = TRUE
       )
     }
     
     # Calculate district means for absenteeism
     dist_means_absent <- numeric(length(district_ids))
     
     for (j in seq_along(district_ids)) {
       district_data <- current_data[
         current_data$DISTRICT_ID == district_ids[j], 
       ]
       dist_means_absent[j] <- mean(
         district_data$CHRONIC_ABSENT_RATE, 
         na.rm = TRUE
       )
     }
     
     # Between and total variance calculations
     between_var_part <- var(dist_means_part, na.rm = TRUE)
     total_var_part <- var(
       current_data$PARTICIPATION_RATE, 
       na.rm = TRUE
     )
     
     between_var_absent <- var(dist_means_absent, na.rm = TRUE)
     total_var_absent <- var(
       current_data$CHRONIC_ABSENT_RATE, 
       na.rm = TRUE
     )
     
     # Calculate percentages
     between_pct_part <- round(
       between_var_part / total_var_part * 100, 2
     )
     within_pct_part <- round(
       (total_var_part - between_var_part) / 
       total_var_part * 100, 2
     )
     
     between_pct_absent <- round(
       between_var_absent / total_var_absent * 100, 2
     )
     within_pct_absent <- round(
       (total_var_absent - between_var_absent) / 
       total_var_absent * 100, 2
     )
     
     row_result <- data.frame(
       YEAR                        = year_subject$YEAR[i],
       SUBJECT                     = year_subject$SUBJECT[i],
       between_pct_participation   = between_pct_part,
       within_pct_participation    = within_pct_part,
       between_pct_absenteeism     = between_pct_absent,
       within_pct_absenteeism      = within_pct_absent
     )
     
     result <- rbind(result, row_result)
   }
 }
 
 result
}



# Calculate sample size reliability statistics
# Summarizes district enrollment sizes for reliability 
# assessment
calc_sample_size_reliability <- function(
 district_trends,
 subject
) {
 # Filter to All_Students for target subject
 reliability_data <- district_trends[
   district_trends$DISAGGREGATED_GROUP == "All_Students" &
   district_trends$SUBJECT == subject, 
 ]
 
 # Get unique years
 years <- unique(reliability_data$YEAR)
 
 result <- data.frame()
 
 for (year in years) {
   year_data <- reliability_data[
     reliability_data$YEAR == year, 
   ]
   
   if (nrow(year_data) > 0) {
     year_row <- data.frame(
       YEAR               = year,
       districts          = nrow(year_data),
       median_enrolled    = round(
         median(year_data$students_enrolled, na.rm = TRUE), 2
       ),
       small_districts_pct = round(
         mean(year_data$students_enrolled < 100, na.rm = TRUE) * 100, 2
       ),
       very_small_pct     = round(
         mean(year_data$students_enrolled < 50, na.rm = TRUE) * 100, 2
       )
     )
     
     result <- rbind(result, year_row)
   }
 }
 
 result
}



# Summarizes district participation rates by year with 
# quartiles and federal compliance thresholds
calc_participation_trends <- function(district_trends, subject) {
 # Filter to All_Students for target subject
 trend_data <- district_trends[
   district_trends$DISAGGREGATED_GROUP == "All_Students" &
   district_trends$SUBJECT == subject, 
 ]
 
 # Get unique years
 years <- unique(trend_data$YEAR)
 
 result <- data.frame()
 
 for (year in years) {
   year_data <- trend_data[trend_data$YEAR == year, ]
   
   if (nrow(year_data) > 0) {
     year_row <- data.frame(
       YEAR        = year,
       districts   = nrow(year_data),
       median_rate = round(
         median(year_data$participation_rate, na.rm = TRUE), 2
       ),
       q1_rate     = round(
         quantile(year_data$participation_rate, 0.25, na.rm = TRUE), 2
       ),
       q3_rate     = round(
         quantile(year_data$participation_rate, 0.75, na.rm = TRUE), 2
       ),
       below_95_pct = round(
         mean(year_data$participation_rate < 95, na.rm = TRUE) * 100, 2
       )
     )
     
     result <- rbind(result, year_row)
   }
 }
 
 # Sort by year
 result[order(result$YEAR), ]
}



# Summarizes district chronic absenteeism rates by year 
# with quartiles and threshold percentages
calc_absenteeism_trends <- function(district_trends, subject) {
 # Filter to All_Students for target subject
 trend_data <- district_trends[
   district_trends$DISAGGREGATED_GROUP == "All_Students" &
   district_trends$SUBJECT == subject, 
 ]
 
 # Get unique years
 years <- unique(trend_data$YEAR)
 
 result <- data.frame()
 
 for (year in years) {
   year_data <- trend_data[trend_data$YEAR == year, ]
   
   if (nrow(year_data) > 0) {
     year_row <- data.frame(
       YEAR        = year,
       districts   = nrow(year_data),
       median_rate = round(
         median(year_data$chronic_absent_rate, na.rm = TRUE), 2
       ),
       q1_rate     = round(
         quantile(year_data$chronic_absent_rate, 0.25, na.rm = TRUE), 2
       ),
       q3_rate     = round(
         quantile(year_data$chronic_absent_rate, 0.75, na.rm = TRUE), 2
       ),
       above_20_pct = round(
         mean(year_data$chronic_absent_rate > 20, na.rm = TRUE) * 100, 2
       ),
       above_30_pct = round(
         mean(year_data$chronic_absent_rate > 30, na.rm = TRUE) * 100, 2
       )
     )
     
     result <- rbind(result, year_row)
   }
 }
 
 # Sort by year
 result[order(result$YEAR), ]
}



# Calculate participation variation within districts
# Identifies districts with highest school-to-school 
# participation variation
calc_participation_variation <- function(
 school_variation,
 district_trends,
 subject,
 output_year,
 top_n = 15
) {
 # Get district-level participation rates
 district_rates <- district_trends[
   district_trends$DISAGGREGATED_GROUP == "All_Students", 
   c("COUNTY_ID", "DISTRICT_ID", "COUNTY_NAME", 
     "DISTRICT_NAME", "YEAR", "SUBJECT", "participation_rate")
 ]
 
 # Create merge keys
 school_key <- paste(
   school_variation$COUNTY_ID,
   school_variation$DISTRICT_ID,
   school_variation$YEAR,
   school_variation$SUBJECT
 )
 
 district_key <- paste(
   district_rates$COUNTY_ID,
   district_rates$DISTRICT_ID,
   district_rates$YEAR,
   district_rates$SUBJECT
 )
 
 # Merge district rates with school variation data
 merged_data <- school_variation
 match_indices <- match(school_key, district_key)
 
 merged_data$COUNTY_NAME <- district_rates$COUNTY_NAME[
   match_indices
 ]
 merged_data$DISTRICT_NAME <- district_rates$DISTRICT_NAME[
   match_indices
 ]
 merged_data$participation_rate <- district_rates$participation_rate[
   match_indices
 ]
 
 # Create participation range text
 merged_data$participation_range <- paste0(
   round(merged_data$participation_min, 2),
   "% - ",
   round(merged_data$participation_max, 2),
   "%"
 )
 
 # Filter to target year and subject
 filtered_data <- merged_data[
   merged_data$YEAR == output_year &
   merged_data$SUBJECT == subject &
   !is.na(merged_data$participation_rate), 
 ]
 
 # Sort by participation standard deviation (descending)
 sorted_data <- filtered_data[
   order(-filtered_data$participation_sd), 
 ]
 
 # Select columns and limit rows
 result_cols <- c(
   "COUNTY_NAME",
   "DISTRICT_NAME",
   "participation_rate",
   "participation_range",
   "participation_sd",
   "schools_below_95",
   "schools_count"
 )
 
 result <- sorted_data[1:min(top_n, nrow(sorted_data)), 
                      result_cols]
 
 result
}



# Calculate absenteeism variation within districts
# Identifies districts with highest school-to-school 
# absenteeism variation
calc_absenteeism_variation <- function(
 school_variation,
 district_trends,
 subject,
 output_year,
 top_n = 15
) {
 # Get district-level absenteeism rates
 district_rates <- district_trends[
   district_trends$DISAGGREGATED_GROUP == "All_Students", 
   c("COUNTY_ID", "DISTRICT_ID", "COUNTY_NAME", 
     "DISTRICT_NAME", "YEAR", "SUBJECT", "chronic_absent_rate")
 ]
 
 # Create merge keys
 school_key <- paste(
   school_variation$COUNTY_ID,
   school_variation$DISTRICT_ID,
   school_variation$YEAR,
   school_variation$SUBJECT
 )
 
 district_key <- paste(
   district_rates$COUNTY_ID,
   district_rates$DISTRICT_ID,
   district_rates$YEAR,
   district_rates$SUBJECT
 )
 
 # Merge district rates with school variation data
 merged_data <- school_variation
 match_indices <- match(school_key, district_key)
 
 merged_data$COUNTY_NAME <- district_rates$COUNTY_NAME[
   match_indices
 ]
 merged_data$DISTRICT_NAME <- district_rates$DISTRICT_NAME[
   match_indices
 ]
 merged_data$chronic_absent_rate <- district_rates$chronic_absent_rate[
   match_indices
 ]
 
 # Create absenteeism range text
 merged_data$absent_range <- paste0(
   round(merged_data$chronic_absent_min, 2),
   "% - ",
   round(merged_data$chronic_absent_max, 2),
   "%"
 )
 
 # Filter to target year and subject
 filtered_data <- merged_data[
   merged_data$YEAR == output_year &
   merged_data$SUBJECT == subject &
   !is.na(merged_data$chronic_absent_rate), 
 ]
 
 # Sort by absenteeism standard deviation (descending)
 sorted_data <- filtered_data[
   order(-filtered_data$chronic_absent_sd), 
 ]
 
 # Select columns and limit rows
 result_cols <- c(
   "COUNTY_NAME",
   "DISTRICT_NAME",
   "chronic_absent_rate",
   "absent_range",
   "chronic_absent_sd",
   "schools_above_20",
   "schools_above_30",
   "schools_count"
 )
 
 result <- sorted_data[1:min(top_n, nrow(sorted_data)), 
                      result_cols]
 
 result
}







# Calculate pandemic recovery patterns for districts
# Analyzes how close districts are to pre-pandemic (2018-19) levels
# compared to pandemic disruption (2020-21) levels
calc_pandemic_recovery <- function(district_trends, subject) {
  
  # Filter to All_Students for target subject
  trend_data <- district_trends[
    district_trends$DISAGGREGATED_GROUP == "All_Students" &
    district_trends$SUBJECT == subject, 
  ]
  
  # Get current year
  current_year <- max(trend_data$YEAR, na.rm = TRUE)
  
  # Check if we have the required years
  available_years <- unique(trend_data$YEAR)
  has_2018 <- 2018 %in% available_years  # 2018-19 school year (pre-pandemic)
  has_2020 <- 2020 %in% available_years  # 2020-21 school year (pandemic)
  
  # Set required years based on data structure
  pre_pandemic_year <- if (has_2018) 2018 else NA  # 2018-19 school year
  pandemic_year <- if (has_2020) 2020 else NA      # 2020-21 school year
  
  # Check if we have required data
  if (is.na(pre_pandemic_year) || is.na(pandemic_year) || current_year <= pandemic_year) {
    return(list(
      participation_details = data.frame(),
      absenteeism_details = data.frame(),
      years_analyzed = paste("Insufficient data: pre-pandemic", pre_pandemic_year, "pandemic", pandemic_year, "current", current_year)
    ))
  }
  
  # Get districts with data in all three time periods
  districts_all_years <- trend_data[
    trend_data$YEAR %in% c(pre_pandemic_year, pandemic_year, current_year),
  ]
  
  # Count years per district
  district_year_counts <- aggregate(
    YEAR ~ COUNTY_ID + DISTRICT_ID + COUNTY_NAME + DISTRICT_NAME,
    data = districts_all_years,
    FUN = length
  )
  
  # Filter to districts with all 3 years
  complete_districts <- district_year_counts[district_year_counts$YEAR == 3, ]
  
  if (nrow(complete_districts) == 0) {
    return(list(
      participation_details = data.frame(),
      absenteeism_details = data.frame(),
      years_analyzed = paste("No districts with complete data for", pre_pandemic_year, pandemic_year, current_year)
    ))
  }
  
  # Calculate recovery metrics for each district
  recovery_results <- data.frame()
  
  for (i in 1:nrow(complete_districts)) {
    district_key <- paste(complete_districts$COUNTY_ID[i], complete_districts$DISTRICT_ID[i])
    
    district_data <- trend_data[
      paste(trend_data$COUNTY_ID, trend_data$DISTRICT_ID) == district_key,
    ]
    
    # Get rates for each time period
    pre_pandemic_data <- district_data[district_data$YEAR == pre_pandemic_year, ]
    pandemic_data <- district_data[district_data$YEAR == pandemic_year, ]
    current_data <- district_data[district_data$YEAR == current_year, ]
    
    if (nrow(pre_pandemic_data) == 1 && nrow(pandemic_data) == 1 && nrow(current_data) == 1) {
      
      # Participation calculations
      part_pre <- pre_pandemic_data$participation_rate
      part_pandemic <- pandemic_data$participation_rate
      part_current <- current_data$participation_rate
      
      # Absenteeism calculations
      absent_pre <- pre_pandemic_data$chronic_absent_rate
      absent_pandemic <- pandemic_data$chronic_absent_rate
      absent_current <- current_data$chronic_absent_rate
      
      # Store results
      result_row <- data.frame(
        COUNTY_ID = complete_districts$COUNTY_ID[i],
        DISTRICT_ID = complete_districts$DISTRICT_ID[i],
        COUNTY_NAME = complete_districts$COUNTY_NAME[i],
        DISTRICT_NAME = complete_districts$DISTRICT_NAME[i],
        students_enrolled = current_data$students_enrolled,
        
        # Participation metrics
        participation_pre_pandemic = round(part_pre, 2),
        participation_pandemic = round(part_pandemic, 2),
        participation_current = round(part_current, 2),
        participation_change_since_pandemic = round(part_current - part_pandemic, 2),
        participation_change_since_pre = round(part_current - part_pre, 2),
        
        # Absenteeism metrics
        absenteeism_pre_pandemic = round(absent_pre, 2),
        absenteeism_pandemic = round(absent_pandemic, 2),
        absenteeism_current = round(absent_current, 2),
        absenteeism_change_since_pandemic = round(absent_current - absent_pandemic, 2),
        absenteeism_change_since_pre = round(absent_current - absent_pre, 2),
        
        stringsAsFactors = FALSE
      )
      
      recovery_results <- rbind(recovery_results, result_row)
    }
  }
  
  # Create detailed recovery tables
  
  # Participation recovery details
  part_details <- recovery_results[
    c("COUNTY_NAME", "DISTRICT_NAME", "students_enrolled",
      "participation_pre_pandemic", "participation_pandemic", "participation_current",
      "participation_change_since_pandemic", "participation_change_since_pre")
  ]
  
  # Sort by change since pandemic (descending - most improvement first)
  part_details <- part_details[order(-part_details$participation_change_since_pandemic), ]
  
  # Absenteeism recovery details  
  absent_details <- recovery_results[
    c("COUNTY_NAME", "DISTRICT_NAME", "students_enrolled",
      "absenteeism_pre_pandemic", "absenteeism_pandemic", "absenteeism_current", 
      "absenteeism_change_since_pandemic", "absenteeism_change_since_pre")
  ]
  
  # Sort by change since pandemic (ascending - most improvement first, since negative is better)
  absent_details <- absent_details[order(absent_details$absenteeism_change_since_pandemic), ]
  
  # Return results
  list(
    participation_details = part_details,
    absenteeism_details = absent_details,
    full_results = recovery_results,
    years_analyzed = paste("Pre-pandemic:", pre_pandemic_year, "| Pandemic:", pandemic_year, "| Current:", current_year)
  )
}
