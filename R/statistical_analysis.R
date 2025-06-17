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