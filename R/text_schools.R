#' Generate comprehensive school analysis text (100+ words longer)
#'
#' @param analysis_results Results from analyze_district function
#' @return Character string with school analysis text
#' @keywords internal
text_schools <- function(analysis_results) {
  district_name     <- analysis_results[["district_name"]]
  current_year      <- analysis_results[["current_year"]]
  school_summary    <- analysis_results[["schools"]][["school_summary"]]
  total_schools     <- analysis_results[["schools"]][["total_schools"]]
  top_20_pct_share  <- analysis_results[["schools"]][["top_20_pct_share"]]
  
  total_chronic  <- sum(school_summary[["CHRONIC_ABSENT"]])
  total_students <- sum(school_summary[["TOTAL_STUDENTS"]])
  
  top_20_count       <- max(1, round(total_schools * 0.2))
  top_school_chronic <- school_summary[["CHRONIC_ABSENT"]][1]
  top_school_name    <- school_summary[["SCHOOL_NAME"]][1]
  
  highest_rate <- max(school_summary[["CHRONIC_RATE"]])
  lowest_rate  <- min(school_summary[["CHRONIC_RATE"]])
  rate_range   <- highest_rate - lowest_rate
  
  highest_rate_school <- school_summary[["SCHOOL_NAME"]][
    which.max(school_summary[["CHRONIC_RATE"]])
  ]
  
  lowest_rate_school <- school_summary[["SCHOOL_NAME"]][
    which.min(school_summary[["CHRONIC_RATE"]])
  ]
  
  # Additional analysis for comprehensive report
  median_rate         <- median(school_summary[["CHRONIC_RATE"]])
  mean_rate           <- mean(school_summary[["CHRONIC_RATE"]])
  schools_above_median <- sum(school_summary[["CHRONIC_RATE"]] > median_rate)
  schools_above_30pct  <- sum(school_summary[["CHRONIC_RATE"]] > 0.30)
  schools_below_10pct  <- sum(school_summary[["CHRONIC_RATE"]] < 0.10)
  
  # Size analysis
  large_schools <- school_summary[
    school_summary[["TOTAL_STUDENTS"]] > 500, 
  ]
  small_schools <- school_summary[
    school_summary[["TOTAL_STUDENTS"]] < 200, 
  ]
  
  if (nrow(large_schools) > 0) {
    large_school_avg_rate <- mean(large_schools[["CHRONIC_RATE"]])
    large_school_text <- paste0(
      "Large schools (500+ students) average ", 
      round(large_school_avg_rate * 100, 1), 
      "% chronic absence. "
    )
  } else {
    large_school_text <- ""
  }
  
  if (nrow(small_schools) > 0) {
    small_school_avg_rate <- mean(small_schools[["CHRONIC_RATE"]])
    small_school_text <- paste0(
      "Small schools (under 200 students) average ", 
      round(small_school_avg_rate * 100, 1), 
      "% chronic absence. "
    )
  } else {
    small_school_text <- ""
  }
  
  # Format values
  top_20_pct_share_fmt   <- paste0(round(top_20_pct_share * 100, 1), "%")
  highest_rate_fmt       <- paste0(round(highest_rate * 100, 1), "%")
  lowest_rate_fmt        <- paste0(round(lowest_rate * 100, 1), "%")
  rate_range_fmt         <- paste0(
    round(rate_range * 100, 1), 
    " percentage points"
  )
  top_school_chronic_fmt <- format(top_school_chronic, big.mark = ",")
  total_chronic_fmt      <- format(total_chronic, big.mark = ",")
  median_rate_fmt        <- paste0(round(median_rate * 100, 1), "%")
  
  if (total_schools == 1) {
    school_count_text <- "1 school"
  } else {
    school_count_text <- paste0(total_schools, " schools")
  }
  
  # Concentration analysis
  if (top_20_count == 1) {
    if (total_schools == 1) {
      concentration_text <- paste0(
        "The district's single school accounts for all ", 
        total_chronic_fmt, 
        " chronically absent students."
      )
    } else {
      concentration_text <- paste0(
        "The highest-impact school, ", 
        top_school_name, 
        ", accounts for ", 
        top_20_pct_share_fmt, 
        " of the district's chronic absence, representing ", 
        top_school_chronic_fmt, 
        " of ", 
        total_chronic_fmt, 
        " chronically absent students."
      )
    }
  } else {
    concentration_text <- paste0(
      "The top ", 
      top_20_count, 
      " schools (20% of district schools) account for ", 
      top_20_pct_share_fmt, 
      " of the district's chronic absence, demonstrating ", 
      if (top_20_pct_share > 0.5) "high concentration" else "moderate distribution", 
      " of attendance challenges."
    )
  }
  
  # Distribution analysis
  if (total_schools == 1) {
    distribution_text <- ""
  } else {
    distribution_text <- paste0(
      "Chronic absence rates across schools range from ", 
      lowest_rate_fmt, 
      " at ", 
      lowest_rate_school, 
      " to ", 
      highest_rate_fmt, 
      " at ", 
      highest_rate_school, 
      ", a spread of ", 
      rate_range_fmt, 
      ". The median school rate is ", 
      median_rate_fmt, 
      ", with ", 
      schools_above_median, 
      " schools above this threshold."
    )
  }
  
  # Performance categories
  if (schools_above_30pct > 0) {
    high_need_text <- paste0(
      " ", 
      schools_above_30pct, 
      " schools have chronic absence rates exceeding 30%, ", 
      "indicating severe attendance challenges requiring ", 
      "intensive intervention."
    )
  } else {
    high_need_text <- paste0(
      " No schools exceed the 30% chronic absence threshold, ", 
      "suggesting district-wide attendance challenges are ", 
      "more moderate."
    )
  }
  
  if (schools_below_10pct > 0) {
    high_performing_text <- paste0(
      " ", 
      schools_below_10pct, 
      " schools maintain chronic absence rates below 10%, ", 
      "demonstrating effective attendance practices that ", 
      "could be replicated."
    )
  } else {
    high_performing_text <- paste0(
      " No schools achieve chronic absence rates below 10%, ", 
      "indicating system-wide opportunities for improvement."
    )
  }
  
  # Impact analysis for top schools
  schools_for_30 <- which(school_summary[["CUMULATIVE_PCT"]] >= 0.3)[1]
  schools_for_50 <- which(school_summary[["CUMULATIVE_PCT"]] >= 0.5)[1]
  
  if (is.na(schools_for_30)) schools_for_30 <- total_schools
  if (is.na(schools_for_50)) schools_for_50 <- total_schools
  
  if (total_schools == 1) {
    impact_text <- ""
  } else {
    impact_text <- paste0(
      "Focusing interventions on the highest-impact schools ", 
      "could yield significant results: the top ", 
      schools_for_30, 
      if (schools_for_30 == 1) " school accounts" else " schools account", 
      " for 30% of district chronic absence, while the top ", 
      schools_for_50, 
      if (schools_for_50 == 1) " school accounts" else " schools account", 
      " for 50% of district chronic absence."
    )
  }
  
  schools_text <- paste0(
    "The district operates ", 
    school_count_text, 
    " serving a total of ", 
    format(total_students, big.mark = ","), 
    " students in ", 
    current_year, 
    ". ", 
    concentration_text, 
    " ", 
    distribution_text, 
    high_need_text, 
    high_performing_text, 
    " ", 
    large_school_text, 
    small_school_text, 
    impact_text
  )
  
  return(schools_text)
}
