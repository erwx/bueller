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