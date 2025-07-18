#' Generate comprehensive trends analysis text with subgroups (300+ words)
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
  
  overall_trends     <- trends[["overall_trends"]]
  demographic_trends <- trends[["demographic_trends"]]
  years              <- overall_trends[["YEAR"]]
  num_years          <- length(years)
  
  # Overall trend summary
  first_year <- min(years)
  last_year  <- max(years)
  first_rate <- overall_trends$CHRONIC_RATE[1]
  last_rate  <- overall_trends$CHRONIC_RATE[nrow(overall_trends)]
  
  total_change      <- last_rate - first_rate
  avg_annual_change <- total_change / (num_years - 1)
  
  total_change_pct <- paste0(
    ifelse(total_change >= 0, "+", ""), 
    round(total_change * 100, 1), 
    " percentage points"
  )
  avg_change_pct <- paste0(
    ifelse(avg_annual_change >= 0, "+", ""), 
    round(avg_annual_change * 100, 1), 
    " percentage points per year"
  )
  
  # Volatility analysis
  rate_changes <- overall_trends$RATE_CHANGE[
    !is.na(overall_trends$RATE_CHANGE)
  ]
  
  if (length(rate_changes) > 0) {
    volatility        <- sd(rate_changes)
    max_increase_idx  <- which.max(overall_trends$RATE_CHANGE)
    max_decrease_idx  <- which.min(overall_trends$RATE_CHANGE)
    
    max_increase_year   <- overall_trends$YEAR[max_increase_idx]
    max_increase_amount <- overall_trends$RATE_CHANGE[max_increase_idx]
    max_decrease_year   <- overall_trends$YEAR[max_decrease_idx]
    max_decrease_amount <- overall_trends$RATE_CHANGE[max_decrease_idx]
    
    if (volatility > 0.03) {
      volatility_desc <- "highly volatile"
    } else if (volatility > 0.015) {
      volatility_desc <- "moderately volatile"
    } else {
      volatility_desc <- "relatively stable"
    }
    
    volatility_text <- paste0(
      "Year-to-year changes have been ", 
      volatility_desc, 
      " with a standard deviation of ", 
      round(volatility * 100, 1), 
      " percentage points."
    )
    
    if (!is.na(max_increase_amount) && max_increase_amount > 0.01) {
      increase_text <- paste0(
        " The largest single-year increase occurred in ", 
        max_increase_year, 
        " (+", 
        round(max_increase_amount * 100, 1), 
        " percentage points)."
      )
    } else {
      increase_text <- ""
    }
    
    if (!is.na(max_decrease_amount) && max_decrease_amount < -0.01) {
      decrease_text <- paste0(
        " The largest single-year decrease occurred in ", 
        max_decrease_year, 
        " (", 
        round(max_decrease_amount * 100, 1), 
        " percentage points)."
      )
    } else {
      decrease_text <- ""
    }
  } else {
    volatility_text <- ""
    increase_text   <- ""
    decrease_text   <- ""
  }
  
  # Overall trend narrative
  if (abs(total_change) <= 0.01) {
    overall_text <- paste0(
      "Over the ", 
      num_years, 
      "-year period from ", 
      first_year, 
      " to ", 
      last_year, 
      ", the district's chronic absence rate remained ", 
      "remarkably stable, changing by only ", 
      total_change_pct, 
      " (from ", 
      round(first_rate * 100, 1), 
      "% to ", 
      round(last_rate * 100, 1), 
      "%). This consistency suggests sustained practices ", 
      "and stable environmental conditions affecting attendance."
    )
  } else if (total_change > 0) {
    if (total_change > 0.05) {
      trend_severity <- "dramatically"
    } else if (total_change > 0.02) {
      trend_severity <- "significantly"
    } else {
      trend_severity <- "modestly"
    }
    
    overall_text <- paste0(
      "Over the ", 
      num_years, 
      "-year period from ", 
      first_year, 
      " to ", 
      last_year, 
      ", the district's chronic absence rate increased ", 
      trend_severity, 
      " by ", 
      total_change_pct, 
      " (from ", 
      round(first_rate * 100, 1), 
      "% to ", 
      round(last_rate * 100, 1), 
      "%), averaging ", 
      avg_change_pct, 
      ". This upward trend indicates emerging or ", 
      "intensifying barriers to student attendance."
    )
  } else {
    if (abs(total_change) > 0.05) {
      trend_severity <- "dramatically"
    } else if (abs(total_change) > 0.02) {
      trend_severity <- "significantly"
    } else {
      trend_severity <- "modestly"
    }
    
    overall_text <- paste0(
      "Over the ", 
      num_years, 
      "-year period from ", 
      first_year, 
      " to ", 
      last_year, 
      ", the district's chronic absence rate improved ", 
      trend_severity, 
      ", decreasing by ", 
      gsub("^\\+", "", total_change_pct), 
      " (from ", 
      round(first_rate * 100, 1), 
      "% to ", 
      round(last_rate * 100, 1), 
      "%), averaging ", 
      gsub("^\\+", "", avg_change_pct), 
      ". This positive trend suggests effective interventions ", 
      "or improving conditions."
    )
  }
  
  # Comprehensive demographic subgroup analysis
  if (length(demographic_trends) > 0) {
    improving_groups <- c()
    worsening_groups <- c()
    stable_groups    <- c()
    group_details    <- c()
    
    for (group_name in names(demographic_trends)) {
      group_data <- demographic_trends[[group_name]]
      if (nrow(group_data) > 1) {
        group_first_rate <- group_data[["CHRONIC_RATE"]][1]
        group_last_rate  <- group_data[["CHRONIC_RATE"]][nrow(group_data)]
        group_change     <- group_last_rate - group_first_rate
        
        group_change_pct <- round(group_change * 100, 1)
        
        if (group_change < -0.02) {
          improving_groups <- c(improving_groups, group_name)
          group_details    <- c(
            group_details, 
            paste0(group_name, " (", group_change_pct, "pp)")
          )
        } else if (group_change > 0.02) {
          worsening_groups <- c(worsening_groups, group_name)
          group_details    <- c(
            group_details, 
            paste0(group_name, " (+", group_change_pct, "pp)")
          )
        } else {
          stable_groups <- c(stable_groups, group_name)
        }
      }
    }
    
    subgroup_intro <- paste0(
      " Examining demographic subgroups reveals important ", 
      "differential patterns that require targeted attention."
    )
    
    if (length(improving_groups) > 0) {
      if (length(improving_groups) == 1) {
        improving_text <- paste0(
          " ", 
          improving_groups[1], 
          " students showed meaningful improvement in attendance rates."
        )
      } else {
        improving_text <- paste0(
          " ", 
          paste(
            improving_groups[1:(length(improving_groups)-1)], 
            collapse = ", "
          ), 
          " and ", 
          improving_groups[length(improving_groups)], 
          " students all demonstrated substantial improvements ", 
          "in attendance."
        )
      }
    } else {
      improving_text <- ""
    }
    
    if (length(worsening_groups) > 0) {
      if (length(worsening_groups) == 1) {
        worsening_text <- paste0(
          " ", 
          worsening_groups[1], 
          " students experienced concerning increases in ", 
          "chronic absence rates, requiring focused intervention."
        )
      } else {
        worsening_text <- paste0(
          " ", 
          paste(
            worsening_groups[1:(length(worsening_groups)-1)], 
            collapse = ", "
          ), 
          " and ", 
          worsening_groups[length(worsening_groups)], 
          " students all showed troubling increases in chronic absence."
        )
      }
    } else {
      worsening_text <- ""
    }
    
    if (length(stable_groups) > 0) {
      stable_text <- paste0(
        " ", 
        paste(stable_groups, collapse = ", "), 
        " students maintained relatively stable attendance patterns."
      )
    } else {
      stable_text <- ""
    }
    
    # Equity implications
    if (length(improving_groups) > 0 && length(worsening_groups) > 0) {
      equity_text <- paste0(
        " These divergent trends suggest that interventions ", 
        "may be having differential impacts across student ", 
        "populations, potentially widening or narrowing ", 
        "achievement gaps."
      )
    } else if (length(improving_groups) > length(worsening_groups)) {
      equity_text <- paste0(
        " The predominantly positive trends across subgroups ", 
        "suggest broad-based improvements in district ", 
        "attendance support systems."
      )
    } else if (length(worsening_groups) > length(improving_groups)) {
      equity_text <- paste0(
        " The concerning trends across multiple subgroups ", 
        "indicate systemic challenges that require ", 
        "comprehensive district-wide response."
      )
    } else {
      equity_text <- paste0(
        " The relatively uniform trends across demographic ", 
        "groups suggest district-wide factors are primary ", 
        "drivers of attendance patterns."
      )
    }
    
    subgroup_text <- paste0(
      subgroup_intro, 
      improving_text, 
      worsening_text, 
      stable_text, 
      equity_text
    )
  } else {
    subgroup_text <- paste0(
      " Insufficient demographic trend data prevents ", 
      "detailed subgroup analysis."
    )
  }
  
  trends_text <- paste0(
    overall_text, 
    " ", 
    volatility_text, 
    increase_text, 
    decrease_text, 
    subgroup_text
  )
  
  return(trends_text)
}
