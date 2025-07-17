#' Generate trends analysis text
#'
#' @param analysis_results Results from analyze_district function
#' @return Character string with trends analysis text
#' @importFrom stats sd
#' @keywords internal
text_trends <- function(analysis_results) {
  trends <- analysis_results[["trends"]]
  
  if (!trends[["has_trends"]]) {
    return(trends[["message"]])
  }
  
  district_name <- analysis_results[["district_name"]]
  years_analyzed <- trends[["years_analyzed"]]
  num_years <- trends[["num_years"]]
  total_change <- trends[["total_change"]]
  avg_annual_change <- trends[["avg_annual_change"]]
  overall_trends <- trends[["overall_trends"]]
  
  # Overall trend summary
  first_year <- min(years_analyzed)
  last_year <- max(years_analyzed)
  first_rate <- overall_trends$CHRONIC_RATE[1]
  last_rate <- overall_trends$CHRONIC_RATE[nrow(overall_trends)]
  
  total_change_pct <- paste0(ifelse(total_change >= 0, "+", ""), round(total_change * 100, 1), " percentage points")
  avg_change_pct <- paste0(ifelse(avg_annual_change >= 0, "+", ""), round(avg_annual_change * 100, 1), " percentage points per year")
  
  if (abs(total_change) <= 0.01) {
    overall_text <- paste0(
      "Over the ", num_years, "-year period from ", first_year, " to ", last_year, 
      ", the district's chronic absence rate remained relatively stable, changing by only ", 
      total_change_pct, " (from ", round(first_rate * 100, 1), "% to ", round(last_rate * 100, 1), "%)."
    )
  } else if (total_change > 0) {
    overall_text <- paste0(
      "Over the ", num_years, "-year period from ", first_year, " to ", last_year, 
      ", the district's chronic absence rate increased by ", total_change_pct, 
      " (from ", round(first_rate * 100, 1), "% to ", round(last_rate * 100, 1), "%), ",
      "averaging ", avg_change_pct, "."
    )
  } else {
    overall_text <- paste0(
      "Over the ", num_years, "-year period from ", first_year, " to ", last_year, 
      ", the district's chronic absence rate decreased by ", gsub("^\\+", "", total_change_pct), 
      " (from ", round(first_rate * 100, 1), "% to ", round(last_rate * 100, 1), "%), ",
      "averaging ", gsub("^\\+", "", avg_change_pct), "."
    )
  }
  
  # Year-to-year volatility
  yoy_changes <- overall_trends$YOY_RATE_CHANGE[!is.na(overall_trends$YOY_RATE_CHANGE)]
  volatility <- sd(yoy_changes, na.rm = TRUE)
  
  largest_increase_year <- trends[["largest_increase_year"]]
  largest_increase_amount <- trends[["largest_increase_amount"]]
  largest_decrease_year <- trends[["largest_decrease_year"]]
  largest_decrease_amount <- trends[["largest_decrease_amount"]]
  
  if (!is.na(largest_increase_amount) && largest_increase_amount > 0.01) {
    increase_text <- paste0(
      "The largest single-year increase occurred in ", largest_increase_year, 
      " (+", round(largest_increase_amount * 100, 1), " percentage points)."
    )
  } else {
    increase_text <- ""
  }
  
  if (!is.na(largest_decrease_amount) && largest_decrease_amount < -0.01) {
    decrease_text <- paste0(
      "The largest single-year decrease occurred in ", largest_decrease_year, 
      " (", round(largest_decrease_amount * 100, 1), " percentage points)."
    )
  } else {
    decrease_text <- ""
  }
  
  volatility_parts <- c(increase_text, decrease_text)
  volatility_text <- paste(volatility_parts[nchar(volatility_parts) > 0], collapse = " ")
  
  trends_text <- paste0(overall_text, " ", volatility_text)
  
  return(trends_text)
}
