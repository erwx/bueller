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