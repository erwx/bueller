#' Generate district metrics text
#'
#' @param analysis_results Results from analyze_district function
#' @return Character string with district metrics text
#' @keywords internal
text_metrics <- function(analysis_results) {
  district_name   <- analysis_results[["district_name"]]
  current_year    <- analysis_results[["current_year"]]
  district_rate   <- analysis_results[["performance"]][["district_rate"]]
  state_rate      <- analysis_results[["performance"]][["state_rate"]]
  gap             <- analysis_results[["performance"]][["gap_from_state"]]
  yoy_change      <- analysis_results[["performance"]][["yoy_change"]]
  peak_year       <- analysis_results[["performance"]][["peak_year"]]
  peak_rate       <- analysis_results[["performance"]][["peak_rate"]]
  target_rate     <- analysis_results[["performance"]][["target_rate"]]
  years_to_target <- analysis_results[["performance"]][["years_to_target"]]
  
  district_rate_pct <- paste0(round(district_rate * 100, 1), "%")
  state_rate_pct    <- paste0(round(state_rate * 100, 1), "%")
  peak_rate_pct     <- paste0(round(peak_rate * 100, 1), "%")
  target_rate_pct   <- paste0(round(target_rate * 100, 1), "%")
  
  gap_abs <- abs(gap)
  if (gap_abs <= 0.005) {
    state_comparison <- paste0(
      "The district's chronic absence rate of ", 
      district_rate_pct,
      " is similar to the state average of ",
      state_rate_pct,
      "."
    )
  } else if (gap > 0) {
    if (gap > 0.05) {
      magnitude <- "substantially higher"
    } else if (gap > 0.02) {
      magnitude <- "moderately higher"
    } else {
      magnitude <- "slightly higher"
    }
    state_comparison <- paste0(
      "The district's chronic absence rate of ", 
      district_rate_pct,
      " is ",
      magnitude, 
      " than the state average of ", 
      state_rate_pct,
      "."
    )
  } else {
    if (gap_abs > 0.05) {
      magnitude <- "substantially lower"
    } else if (gap_abs > 0.02) {
      magnitude <- "moderately lower"
    } else {
      magnitude <- "slightly lower"
    }
    state_comparison <- paste0(
      "The district's chronic absence rate of ", 
      district_rate_pct,
      " is ",
      magnitude, 
      " than the state average of ", 
      state_rate_pct,
      "."
    )
  }
  
  if (is.na(yoy_change)) {
    yoy_text <- ""
  } else {
    yoy_abs <- abs(yoy_change)
    yoy_pct <- paste0(round(yoy_abs * 100, 1), " percentage points")
    
    if (yoy_abs <= 0.005) {
      yoy_text <- paste0(
        "The rate remained stable from the previous year."
      )
    } else if (yoy_change > 0) {
      yoy_text <- paste0(
        "The rate increased by ",
        yoy_pct, 
        " from the previous year."
      )
    } else {
      yoy_text <- paste0(
        "The rate decreased by ",
        yoy_pct, 
        " from the previous year."
      )
    }
  }
  
  if (peak_year == current_year) {
    peak_text <- paste0(
      "The current year represents the highest chronic absence rate observed at ", 
      peak_rate_pct,
      "."
    )
  } else {
    years_since_peak <- current_year - peak_year
    if (years_since_peak == 1) {
      peak_text <- paste0(
        "The highest chronic absence rate of ", 
        peak_rate_pct,
        " occurred in ",
        peak_year, 
        ", 1 year ago."
      )
    } else {
      peak_text <- paste0(
        "The highest chronic absence rate of ", 
        peak_rate_pct,
        " occurred in ",
        peak_year, 
        ", ",
        years_since_peak,
        " years ago."
      )
    }
  }
  
  reduction_needed <- district_rate - target_rate
  if (reduction_needed <= 0) {
    target_text <- paste0(
      "The district has achieved the 50% reduction target rate of ",
      target_rate_pct,
      "."
    )
  } else {
    reduction_pct <- paste0(
      round(reduction_needed * 100, 1), 
      " percentage points")
    
    if (is.na(years_to_target)) {
      target_text <- paste0(
        "To reach the 50% reduction target of ", 
        target_rate_pct,
        ", the district would need to reduce its rate by ",
        reduction_pct, 
        ". Current trends do not indicate a clear timeline for achieving this target."
      )
    } else if (years_to_target <= 1) {
      target_text <- paste0(
        "To reach the 50% reduction target of ", 
        target_rate_pct,
        ", the district would need to reduce its rate by ",
        reduction_pct, 
        ". Based on current trends, this target could be achieved within 1 year."
      )
    } else if (years_to_target <= 5) {
      target_text <- paste0(
        "To reach the 50% reduction target of ", 
        target_rate_pct,
        ", the district would need to reduce its rate by ",
        reduction_pct, 
        ". Based on current trends, this target could be achieved within ",
        years_to_target, 
        " years."
      )
    } else {
      target_text <- paste0(
        "To reach the 50% reduction target of ", 
        target_rate_pct,
        ", the district would need to reduce its rate by ",
        reduction_pct, 
        ". Based on current trends, this would take approximately ",
        years_to_target,
        " years."
      )
    }
  }
  
  metrics_text <- paste0(
    state_comparison, " ", 
    if (nchar(yoy_text) > 0) paste0(yoy_text, " ") else "", 
    peak_text, " ", target_text
  )
  
  return(metrics_text)
}
