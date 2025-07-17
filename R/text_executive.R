#' Generate executive summary text
#'
#' @param analysis_results Results from analyze_district function
#' @return Character string with executive summary text
#' @keywords internal
text_executive <- function(analysis_results) {
  district_name    <- analysis_results[["district_name"]]
  current_year     <- analysis_results[["current_year"]]
  district_rate    <- analysis_results[["performance"]][["district_rate"]]
  state_rate       <- analysis_results[["performance"]][["state_rate"]]
  gap              <- analysis_results[["performance"]][["gap_from_state"]]
  total_students   <- analysis_results[["overview"]][["total_students"]]
  chronic_students <- analysis_results[["overview"]][["chronic_students"]]
  peak_year        <- analysis_results[["performance"]][["peak_year"]]
  peak_rate        <- analysis_results[["performance"]][["peak_rate"]]
  target_rate      <- analysis_results[["performance"]][["target_rate"]]
  years_to_target  <- analysis_results[["performance"]][["years_to_target"]]
  
  district_rate_pct    <- paste0(round(district_rate * 100, 1), "%")
  state_rate_pct       <- paste0(round(state_rate * 100, 1), "%")
  peak_rate_pct        <- paste0(round(peak_rate * 100, 1), "%")
  target_rate_pct      <- paste0(round(target_rate * 100, 1), "%")
  total_students_fmt   <- format(total_students, big.mark = ",")
  chronic_students_fmt <- format(chronic_students, big.mark = ",")
  
  if (abs(gap) <= 0.005) {
    performance_text <- paste0(
      "performs similarly to the state average"
    )
  } else if (gap > 0.05) {
    performance_text <- paste0(
      "substantially exceeds the state average"
    )
  } else if (gap > 0.02) {
    performance_text <- paste0(
      "moderately exceeds the state average"
    )
  } else if (gap > 0.005) {
    performance_text <- paste0(
      "slightly exceeds the state average"
    )
  } else if (gap < -0.05) {
    performance_text <- paste0(
      "performs substantially better than the state average"
    )
  } else if (gap < -0.02) {
    performance_text <- paste0(
      "performs moderately better than the state average"
    )
  } else {
    performance_text <- paste0(
      "performs slightly better than the state average"
    )
  }
  
  if (is.na(years_to_target)) {
    timeline_text <- paste0(
      "Current trends do not project a clear timeline for reaching the 50% reduction target of ",
      target_rate_pct,
      "."
    )
  } else if (years_to_target == 0) {
    timeline_text <- paste0(
      "The district has already achieved the 50% reduction target of ",
      target_rate_pct,
      "."
    )
  } else if (years_to_target == 1) {
    timeline_text <- paste0(
      "Based on current trends, the district could reach the 50% reduction target of ", 
      target_rate_pct,
      " within 1 year."
    )
  } else if (years_to_target <= 5) {
    timeline_text <- paste0(
      "Based on current trends, the district could reach the 50% reduction target of ", 
      target_rate_pct,
      " within ",
      years_to_target, 
      " years."
    )
  } else if (years_to_target <= 10) {
    timeline_text <- paste0(
      "Based on current trends, reaching the 50% reduction target of ",
      target_rate_pct, 
      " would take approximately ",
      years_to_target, 
      " years."
    )
  } else {
    timeline_text <- paste0(
      "Based on current trends, reaching the 50% reduction target of ",
      target_rate_pct, 
      " would take more than ",
      years_to_target, 
      " years."
    )
  }
  
  if (peak_year == current_year) {
    peak_text <- paste0(
      "The current year represents the peak chronic absence rate of ",
      peak_rate_pct,
      "."
    )
  } else {
    years_since_peak <- current_year - peak_year
    if (years_since_peak == 1) {
      peak_text <- paste0(
        "Chronic absence peaked at ",
        peak_rate_pct, 
        " in ",
        peak_year,
        ", 1 year ago."
      )
    } else {
      peak_text <- paste0(
        "Chronic absence peaked at ",
        peak_rate_pct, 
        " in ",
        peak_year,
        ", ",
        years_since_peak, 
        " years ago."
      )
    }
  }
  
  summary_text <- paste0(
    district_name,
    " served ",
    total_students_fmt,
    " students in ", 
    current_year,
    ", with ",
    chronic_students_fmt,
    " (",
    district_rate_pct, 
    ") experiencing chronic absence. The district ",
    performance_text, 
    " of ",
    state_rate_pct,
    ". ",
    peak_text,
    " ",
    timeline_text
  )
  return(summary_text)
}
