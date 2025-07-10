#' Calculate School-Level Demographic Composition
#' 
#' Calculates demographic composition percentages for each school by year.
#' Uses efficient aggregation for fast processing.
#' 
#' @param data Student-level data frame with demographic columns
#' @return Data frame with demographic percentages by school and year
#' @keywords internal
demo <- function(data) {
  
  cat("Calculating school-level demographic composition...\n")
  
  # Get unique school-year combinations
  school_years <- unique(data[, c("COUNTY_ID", "DISTRICT_ID", "SCHOOL_ID", 
                                 "COUNTY_NAME", "DISTRICT_NAME", "SCHOOL_NAME", "YEAR")])
  
  cat("Processing", nrow(school_years), "school-year combinations...\n")
  
  # Calculate total enrollment by school-year
  enrollment_counts <- aggregate(
    STUDENT_ID ~ COUNTY_ID + DISTRICT_ID + SCHOOL_ID + YEAR,
    data = data,
    FUN = length
  )
  names(enrollment_counts)[names(enrollment_counts) == "STUDENT_ID"] <- "enrollment"
  
  # Calculate demographic counts by school-year using aggregate
  hispanic_counts <- aggregate(
    HISPANIC ~ COUNTY_ID + DISTRICT_ID + SCHOOL_ID + YEAR,
    data = data,
    FUN = sum
  )
  
  white_counts <- aggregate(
    WHITE ~ COUNTY_ID + DISTRICT_ID + SCHOOL_ID + YEAR,
    data = data,
    FUN = sum
  )
  
  asian_counts <- aggregate(
    ASIAN ~ COUNTY_ID + DISTRICT_ID + SCHOOL_ID + YEAR,
    data = data,
    FUN = sum
  )
  
  black_counts <- aggregate(
    BLACK ~ COUNTY_ID + DISTRICT_ID + SCHOOL_ID + YEAR,
    data = data,
    FUN = sum
  )
  
  ell_counts <- aggregate(
    ELL ~ COUNTY_ID + DISTRICT_ID + SCHOOL_ID + YEAR,
    data = data,
    FUN = sum
  )
  
  disadvantaged_counts <- aggregate(
    DISADVANTAGE ~ COUNTY_ID + DISTRICT_ID + SCHOOL_ID + YEAR,
    data = data,
    FUN = sum
  )
  
  disability_counts <- aggregate(
    DISABILITY ~ COUNTY_ID + DISTRICT_ID + SCHOOL_ID + YEAR,
    data = data,
    FUN = sum
  )
  
  male_counts <- aggregate(
    MALE ~ COUNTY_ID + DISTRICT_ID + SCHOOL_ID + YEAR,
    data = data,
    FUN = sum
  )
  
  female_counts <- aggregate(
    FEMALE ~ COUNTY_ID + DISTRICT_ID + SCHOOL_ID + YEAR,
    data = data,
    FUN = sum
  )
  
  # Merge all counts together
  cat("Merging demographic counts...\n")
  
  merge_key <- c("COUNTY_ID", "DISTRICT_ID", "SCHOOL_ID", "YEAR")
  
  demographics <- merge(school_years, enrollment_counts, by = merge_key, all.x = TRUE)
  demographics <- merge(demographics, hispanic_counts, by = merge_key, all.x = TRUE)
  demographics <- merge(demographics, white_counts, by = merge_key, all.x = TRUE)
  demographics <- merge(demographics, asian_counts, by = merge_key, all.x = TRUE)
  demographics <- merge(demographics, black_counts, by = merge_key, all.x = TRUE)
  demographics <- merge(demographics, ell_counts, by = merge_key, all.x = TRUE)
  demographics <- merge(demographics, disadvantaged_counts, by = merge_key, all.x = TRUE)
  demographics <- merge(demographics, disability_counts, by = merge_key, all.x = TRUE)
  demographics <- merge(demographics, male_counts, by = merge_key, all.x = TRUE)
  demographics <- merge(demographics, female_counts, by = merge_key, all.x = TRUE)
  
  # Replace NAs with 0s
  demographics[is.na(demographics)] <- 0
  
  # Calculate percentages
  cat("Calculating demographic percentages...\n")
  
  demographics$pct_hispanic <- round(demographics$HISPANIC / demographics$enrollment * 100, 2)
  demographics$pct_white <- round(demographics$WHITE / demographics$enrollment * 100, 2)
  demographics$pct_asian <- round(demographics$ASIAN / demographics$enrollment * 100, 2)
  demographics$pct_black <- round(demographics$BLACK / demographics$enrollment * 100, 2)
  demographics$pct_ell <- round(demographics$ELL / demographics$enrollment * 100, 2)
  demographics$pct_disadvantaged <- round(demographics$DISADVANTAGE / demographics$enrollment * 100, 2)
  demographics$pct_disability <- round(demographics$DISABILITY / demographics$enrollment * 100, 2)
  demographics$pct_male <- round(demographics$MALE / demographics$enrollment * 100, 2)
  demographics$pct_female <- round(demographics$FEMALE / demographics$enrollment * 100, 2)
  
  # Select final columns
  final_demographics <- demographics[, c(
    "COUNTY_ID", "DISTRICT_ID", "SCHOOL_ID", "COUNTY_NAME", "DISTRICT_NAME", "SCHOOL_NAME", "YEAR",
    "enrollment", "pct_hispanic", "pct_white", "pct_asian", "pct_black", 
    "pct_ell", "pct_disadvantaged", "pct_disability", "pct_male", "pct_female"
  )]
  
  # Clean up environment
  rm(school_years, enrollment_counts, hispanic_counts, white_counts, asian_counts, 
     black_counts, ell_counts, disadvantaged_counts, disability_counts, 
     male_counts, female_counts, merge_key, demographics)
  
  cat("School demographics calculation complete.\n")
  return(final_demographics)
}

