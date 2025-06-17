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