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
  
  overall_trends <- trends[["overall_trends"]]
  years <- overall_trends[["YEAR"]]
  num_years <- length(years)
  
  # Overall trend summary
  first_year <- min(years)
  last_year <- max(years)
  first_rate <- overall_trends$CHRONIC_RATE[1]
  last_rate <- overall_trends$CHRONIC_RATE[nrow(overall_trends)]
  
  total_change <- last_rate - first_rate
  avg_annual_change <- total_change / (num_years - 1)
  
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
  yoy_changes <- overall_trends$RATE_CHANGE[!is.na(overall_trends$RATE_CHANGE)]
  
  if (length(yoy_changes) > 0) {
    largest_increase_idx <- which.max(overall_trends$RATE_CHANGE)
    largest_decrease_idx <- which.min(overall_trends$RATE_CHANGE)
    
    largest_increase_year <- overall_trends$YEAR[largest_increase_idx]
    largest_increase_amount <- overall_trends$RATE_CHANGE[largest_increase_idx]
    largest_decrease_year <- overall_trends$YEAR[largest_decrease_idx]
    largest_decrease_amount <- overall_trends$RATE_CHANGE[largest_decrease_idx]
    
    if (!is.na(largest_increase_amount) && largest_increase_amount > 0.01) {
      increase_text <- paste0(
        " The largest single-year increase occurred in ", largest_increase_year, 
        " (+", round(largest_increase_amount * 100, 1), " percentage points)."
      )
    } else {
      increase_text <- ""
    }
    
    if (!is.na(largest_decrease_amount) && largest_decrease_amount < -0.01) {
      decrease_text <- paste0(
        " The largest single-year decrease occurred in ", largest_decrease_year, 
        " (", round(largest_decrease_amount * 100, 1), " percentage points)."
      )
    } else {
      decrease_text <- ""
    }
  } else {
    increase_text <- ""
    decrease_text <- ""
  }
  
  trends_text <- paste0(overall_text, increase_text, decrease_text)
  
  return(trends_text)
}
