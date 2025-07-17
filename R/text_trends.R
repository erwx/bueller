#' Generate trends analysis text
#'
#' @param analysis_results Results from analyze_district function
#' @return Character string with trends analysis text
#' @keywords internal
text_trends <- function(analysis_results) {
  trends <- analysis_results[["trends"]]
  
  if (!trends[["has_trends"]]) {
    return(trends[["message"]])
  }
  
  district_name <- analysis_results[["district_name"]]
  current_year <- trends[["current_year"]]
  prev_year <- trends[["prev_year"]]
  overall_yoy_change <- trends[["overall_yoy_change"]]
  demo_yoy_changes <- trends[["demo_yoy_changes"]]
  
  # Overall trend text
  yoy_change_pct <- paste0(ifelse(overall_yoy_change >= 0, "+", ""), 
                          round(overall_yoy_change * 100, 1), " percentage points")
  
  if (abs(overall_yoy_change) <= 0.005) {
    overall_text <- paste0(
      "The district's chronic absence rate remained relatively stable from ",
      prev_year, " to ", current_year, ", with minimal change."
    )
  } else if (overall_yoy_change > 0) {
    overall_text <- paste0(
      "The district's chronic absence rate increased by ",
      yoy_change_pct, " from ", prev_year, " to ", current_year, "."
    )
  } else {
    overall_text <- paste0(
      "The district's chronic absence rate decreased by ",
      gsub("^\\+", "", yoy_change_pct), " from ", prev_year, " to ", current_year, "."
    )
  }
  
  # Demographic group trends
  if (nrow(demo_yoy_changes) == 0) {
    demo_text <- "Insufficient data for demographic group trend analysis."
  } else {
    # Find largest increases and decreases
    largest_increase <- demo_yoy_changes[which.max(demo_yoy_changes$YOY_RATE_CHANGE), ]
    largest_decrease <- demo_yoy_changes[which.min(demo_yoy_changes$YOY_RATE_CHANGE), ]
    
    increase_pct <- paste0(round(largest_increase$YOY_RATE_CHANGE * 100, 1), " percentage points")
    decrease_pct <- paste0(round(abs(largest_decrease$YOY_RATE_CHANGE) * 100, 1), " percentage points")
    
    if (largest_increase$YOY_RATE_CHANGE > 0.01) {
      increase_text <- paste0(
        "The largest increase occurred among ", largest_increase$GROUP, 
        " students (", increase_pct, ")."
      )
    } else {
      increase_text <- ""
    }
    
    if (largest_decrease$YOY_RATE_CHANGE < -0.01) {
      decrease_text <- paste0(
        "The largest decrease occurred among ", largest_decrease$GROUP, 
        " students (", decrease_pct, ")."
      )
    } else {
      decrease_text <- ""
    }
    
    # Count groups with increases vs decreases
    increases <- sum(demo_yoy_changes$YOY_RATE_CHANGE > 0.005)
    decreases <- sum(demo_yoy_changes$YOY_RATE_CHANGE < -0.005)
    stable <- nrow(demo_yoy_changes) - increases - decreases
    
    if (increases > decreases) {
      direction_text <- paste0(
        "Most demographic groups (", increases, " of ", nrow(demo_yoy_changes), 
        ") experienced increases in chronic absence rates."
      )
    } else if (decreases > increases) {
      direction_text <- paste0(
        "Most demographic groups (", decreases, " of ", nrow(demo_yoy_changes), 
        ") experienced decreases in chronic absence rates."
      )
    } else {
      direction_text <- paste0(
        "Demographic groups showed mixed trends, with ", increases, 
        " groups increasing and ", decreases, " groups decreasing."
      )
    }
    
    demo_parts <- c(direction_text, increase_text, decrease_text)
    demo_text <- paste(demo_parts[nchar(demo_parts) > 0], collapse = " ")
  }
  
  trends_text <- paste0(overall_text, " ", demo_text)
  
  return(trends_text)
}
