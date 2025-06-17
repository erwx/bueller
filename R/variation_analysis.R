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