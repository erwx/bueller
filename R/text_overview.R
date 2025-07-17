#' Generate district overview text
#'
#' @param analysis_results Results from analyze_district function
#' @return Character string with district overview text
#' @keywords internal
text_overview <- function(analysis_results) {
  district_name    <- analysis_results[["district_name"]]
  current_year     <- analysis_results[["current_year"]]
  years_available  <- analysis_results[["years_available"]]
  total_students   <- analysis_results[["overview"]][["total_students"]]
  chronic_students <- analysis_results[["overview"]][["chronic_students"]]
  chronic_rate     <- analysis_results[["overview"]][["chronic_rate"]]
  n_schools        <- analysis_results[["overview"]][["n_schools"]]
  grade_span       <- analysis_results[["overview"]][["grade_span"]]
  enrollment_trend <- analysis_results[["overview"]][["enrollment_trend"]]
  
  total_students_fmt   <- format(total_students, big.mark = ",")
  chronic_students_fmt <- format(chronic_students, big.mark = ",")
  chronic_rate_pct     <- paste0(round(chronic_rate * 100, 1), "%")
  
  if (grade_span[1] == grade_span[2]) {
    grade_text <- paste0(
      "grade ", grade_span[1]
    )
  } else {
    grade_text <- paste0(
      "grades ",
      grade_span[1],
      " through ", 
      grade_span[2]
    )
  }
  
  if (n_schools == 1) {
    school_text <- "1 school"
  } else {
    school_text <- paste0(n_schools, " schools")
  }
  
  if (nrow(enrollment_trend) > 1) {
    first_year       <- enrollment_trend[["YEAR"]][1]
    last_year        <- enrollment_trend[["YEAR"]][nrow(enrollment_trend)]
    first_enrollment <- enrollment_trend[["TOTAL_STUDENTS"]][1]
    last_enrollment  <- enrollment_trend[["TOTAL_STUDENTS"]][nrow(enrollment_trend)]
    
    enrollment_change <- last_enrollment - first_enrollment
    
    if (enrollment_change == 0) {
      enrollment_text <- paste0(
        "Total enrollment has remained stable at ",
        format(total_students, big.mark = ","), 
        " students across the ", 
        length(years_available),
        " years analyzed."
      )
    } else if (enrollment_change > 0) {
      enrollment_text <- paste0(
        "Total enrollment increased from ", 
        format(first_enrollment, big.mark = ","), 
        " students in ",
        first_year,
        " to ", 
        format(last_enrollment, big.mark = ","), 
        " students in ",
        last_year,
        ", an increase of ",
        format(enrollment_change, big.mark = ","),
        " students."
      )
    } else {
      enrollment_text <- paste0(
        "Total enrollment decreased from ", 
        format(first_enrollment, big.mark = ","), 
        " students in ",
        first_year,
        " to ", 
        format(last_enrollment, big.mark = ","), 
        " students in ",
        last_year,
        ", a decrease of ",
        format(abs(enrollment_change), big.mark = ","),
        " students."
      )
    }
  } else {
    enrollment_text <- paste0("Analysis includes ", current_year, " data.")
  }
  
  # Chronic absence trend analysis
  if (nrow(enrollment_trend) > 1) {
    first_rate   <- enrollment_trend[["CHRONIC_RATE"]][1]
    last_rate    <- enrollment_trend[["CHRONIC_RATE"]][nrow(enrollment_trend)]
    highest_rate <- max(enrollment_trend[["CHRONIC_RATE"]])
    lowest_rate  <- min(enrollment_trend[["CHRONIC_RATE"]])
    
    rate_change <- last_rate - first_rate
    
    if (abs(rate_change) <= 0.005) {
      trend_text <- paste0(
        "The chronic absence rate has remained relatively stable, ranging from ", 
        paste0(round(lowest_rate * 100, 1), "%"), 
        " to ", 
        paste0(round(highest_rate * 100, 1), "%"), 
        " across the period."
      )
    } else if (rate_change > 0) {
      trend_text <- paste0(
        "The chronic absence rate increased from ", 
        paste0(round(first_rate * 100, 1), "%"), 
        " to ", 
        paste0(round(last_rate * 100, 1), "%"), 
        " over the period analyzed."
      )
    } else {
      trend_text <- paste0(
        "The chronic absence rate decreased from ", 
        paste0(round(first_rate * 100, 1), "%"), 
        " to ", 
        paste0(round(last_rate * 100, 1), "%"), 
        " over the period analyzed."
      )
    }
  } else {
    trend_text <- ""
  }
  
  avg_students_per_school <- round(total_students / n_schools, 0)
  
  overview_text <- paste0(
    district_name,
    " serves ",
    total_students_fmt,
    " students across ", 
    school_text,
    " in ",
    current_year,
    ", covering ",
    grade_text, 
    ". The district reported ",
    chronic_students_fmt, 
    " chronically absent students, representing ",
    chronic_rate_pct, 
    " of total enrollment. Schools in the district average ", 
    format(avg_students_per_school, big.mark = ","), 
    " students per school. ",
    enrollment_text
  )
  
  # Add trend text if available
  if (nchar(trend_text) > 0) {
    overview_text <- paste0(overview_text, " ", trend_text)
  }
  
  return(overview_text)
}
