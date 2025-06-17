pretty_print <- function(data, min_enrollment = 30) {
  
  # Basic calculations
  current_year <- max(data$YEAR, na.rm = TRUE)
  years        <- sort(unique(data$YEAR))
  
  # Filter and calculate basic stats
  current_data <- data %>%
    filter(YEAR == current_year, DISAGGREGATED_GROUP == "All_Students") %>%
    filter(!grepl("County Office", DISTRICT_NAME))
  
  ela_med <- median(
    current_data$PARTICIPATION_RATE[current_data$SUBJECT == "ELA"],
    na.rm = TRUE
  )
  
  math_med <- median(
    current_data$PARTICIPATION_RATE[current_data$SUBJECT == "MATH"],
    na.rm = TRUE
  )
  
  absent_med <- median(
    current_data$CHRONIC_ABSENT_RATE,
    na.rm = TRUE
  )
  
  # Simple formatted output
  cat("\n==============================================================\n")
  cat("              STUDENT ENGAGEMENT ANALYSIS REPORT             \n")
  cat("==============================================================\n\n")
  
  cat("Analysis Period:", paste(range(years), collapse = " - "), "\n")
  cat("Current Year Focus:", current_year, "\n\n")
  
  cat("CURRENT STATE:\n")
  cat("  ELA Participation (median):", sprintf("%.1f%%", ela_med), "\n")
  cat("  MATH Participation (median):", sprintf("%.1f%%", math_med), "\n") 
  cat("  Chronic Absenteeism (median):", sprintf("%.1f%%", absent_med), "\n\n")
  
  cat("==============================================================\n\n")
}