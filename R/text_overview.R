#' Generate comprehensive district overview text with trends (300+ words)
#'
#' @param analysis_results Results from analyze_district function
#' @return Character string with district overview text
#' @importFrom stats sd
#' @keywords internal
text_overview <- function(analysis_results) {
  district_name     <- analysis_results[["district_name"]]
  current_year      <- analysis_results[["current_year"]]
  years_available   <- analysis_results[["years_available"]]
  total_students    <- analysis_results[["overview"]][["total_students"]]
  chronic_students  <- analysis_results[["overview"]][["chronic_students"]]
  chronic_rate      <- analysis_results[["overview"]][["chronic_rate"]]
  n_schools         <- analysis_results[["overview"]][["n_schools"]]
  grade_span        <- analysis_results[["overview"]][["grade_span"]]
  enrollment_trend  <- analysis_results[["overview"]][["enrollment_trend"]]
  trends            <- analysis_results[["trends"]]
  
  total_students_fmt   <- format(total_students, big.mark = ",")
  chronic_students_fmt <- format(chronic_students, big.mark = ",")
  chronic_rate_pct     <- paste0(round(chronic_rate * 100, 1), "%")
  
  if (grade_span[1] == grade_span[2]) {
    grade_text <- paste0("grade ", grade_span[1])
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
  
  # Enrollment trends
  if (nrow(enrollment_trend) > 1) {
    first_year       <- enrollment_trend[["YEAR"]][1]
    last_year        <- enrollment_trend[["YEAR"]][nrow(enrollment_trend)]
    first_enrollment <- enrollment_trend[["TOTAL_STUDENTS"]][1]
    last_enrollment  <- enrollment_trend[["TOTAL_STUDENTS"]][
      nrow(enrollment_trend)
    ]
    
    enrollment_change     <- last_enrollment - first_enrollment
    enrollment_pct_change <- (enrollment_change / first_enrollment) * 100
    
    if (abs(enrollment_change) < 50) {
      enrollment_text <- paste0(
        "Total enrollment has remained relatively stable at ", 
        "approximately ", 
        format(
          round(mean(enrollment_trend[["TOTAL_STUDENTS"]])), 
          big.mark = ","
        ), 
        " students."
      )
    } else if (enrollment_change > 0) {
      enrollment_text <- paste0(
        "Total enrollment has grown significantly from ", 
        format(first_enrollment, big.mark = ","), 
        " students in ", 
        first_year, 
        " to ", 
        format(last_enrollment, big.mark = ","), 
        " students in ", 
        last_year, 
        ", representing a ", 
        round(enrollment_pct_change, 1), 
        "% increase over the period."
      )
    } else {
      enrollment_text <- paste0(
        "Total enrollment has declined from ", 
        format(first_enrollment, big.mark = ","), 
        " students in ", 
        first_year, 
        " to ", 
        format(last_enrollment, big.mark = ","), 
        " students in ", 
        last_year, 
        ", representing a ", 
        round(abs(enrollment_pct_change), 1), 
        "% decrease over the period."
      )
    }
  } else {
    enrollment_text <- paste0("Analysis includes ", current_year, " data only.")
  }
  
  # Comprehensive trends analysis including subgroups
  if (trends[["has_trends"]] && nrow(trends[["overall_trends"]]) > 1) {
    overall_trends <- trends[["overall_trends"]]
    years          <- overall_trends[["YEAR"]]
    first_year     <- min(years)
    last_year      <- max(years)
    first_rate     <- overall_trends[["CHRONIC_RATE"]][1]
    last_rate      <- overall_trends[["CHRONIC_RATE"]][
      nrow(overall_trends)
    ]
    total_change   <- last_rate - first_rate
    
    # Volatility analysis
    rate_changes <- overall_trends[["RATE_CHANGE"]][
      !is.na(overall_trends[["RATE_CHANGE"]])
    ]
    
    if (length(rate_changes) > 0) {
      volatility   <- sd(rate_changes)
      max_increase <- max(rate_changes, na.rm = TRUE)
      max_decrease <- min(rate_changes, na.rm = TRUE)
      
      if (volatility > 0.03) {
        volatility_text <- "with high year-to-year volatility"
      } else if (volatility > 0.015) {
        volatility_text <- "with moderate year-to-year volatility"
      } else {
        volatility_text <- "with relatively stable year-to-year changes"
      }
      
      volatility_details <- paste0(
        "The largest single-year increase was ", 
        round(max_increase * 100, 1), 
        " percentage points, while the largest decrease was ", 
        round(abs(max_decrease) * 100, 1), 
        " percentage points."
      )
    } else {
      volatility_text    <- ""
      volatility_details <- ""
    }
    
    if (abs(total_change) <= 0.01) {
      trends_summary <- paste0(
        "Over the ", 
        length(years), 
        "-year period from ", 
        first_year, 
        " to ", 
        last_year, 
        ", chronic absence rates remained relatively stable, ", 
        "changing by only ", 
        round(total_change * 100, 1), 
        " percentage points (from ", 
        round(first_rate * 100, 1), 
        "% to ", 
        round(last_rate * 100, 1), 
        "%) ", 
        volatility_text, 
        ". ", 
        volatility_details
      )
    } else if (total_change > 0) {
      trends_summary <- paste0(
        "Over the ", 
        length(years), 
        "-year period from ", 
        first_year, 
        " to ", 
        last_year, 
        ", chronic absence rates increased substantially by ", 
        round(total_change * 100, 1), 
        " percentage points (from ", 
        round(first_rate * 100, 1), 
        "% to ", 
        round(last_rate * 100, 1), 
        "%) ", 
        volatility_text, 
        ". ", 
        volatility_details
      )
    } else {
      trends_summary <- paste0(
        "Over the ", 
        length(years), 
        "-year period from ", 
        first_year, 
        " to ", 
        last_year, 
        ", chronic absence rates improved significantly, ", 
        "decreasing by ", 
        round(abs(total_change) * 100, 1), 
        " percentage points (from ", 
        round(first_rate * 100, 1), 
        "% to ", 
        round(last_rate * 100, 1), 
        "%) ", 
        volatility_text, 
        ". ", 
        volatility_details
      )
    }
    
    # Demographic subgroup trends
    demographic_trends <- trends[["demographic_trends"]]
    if (length(demographic_trends) > 0) {
      improving_groups <- c()
      worsening_groups <- c()
      
      for (group_name in names(demographic_trends)) {
        group_data <- demographic_trends[[group_name]]
        if (nrow(group_data) > 1) {
          group_first_rate <- group_data[["CHRONIC_RATE"]][1]
          group_last_rate  <- group_data[["CHRONIC_RATE"]][
            nrow(group_data)
          ]
          group_change     <- group_last_rate - group_first_rate
          
          if (group_change < -0.02) {
            improving_groups <- c(improving_groups, group_name)
          } else if (group_change > 0.02) {
            worsening_groups <- c(worsening_groups, group_name)
          }
        }
      }
      
      if (length(improving_groups) > 0 || length(worsening_groups) > 0) {
        subgroup_text <- paste0(
          " Examining demographic subgroups reveals ",
          "differential trends:"
        )
        
        if (length(improving_groups) > 0) {
          subgroup_text <- paste0(
            subgroup_text, 
            " ", 
            paste(improving_groups, collapse = ", "), 
            " students showed meaningful improvements in attendance rates."
          )
        }
        
        if (length(worsening_groups) > 0) {
          subgroup_text <- paste0(
            subgroup_text, 
            " ", 
            paste(worsening_groups, collapse = ", "), 
            " students experienced concerning increases in ", 
            "chronic absence rates."
          )
        }
        
        if (length(improving_groups) == 0 && length(worsening_groups) == 0) {
          subgroup_text <- paste0(
            subgroup_text, 
            " Most demographic groups followed similar patterns to ", 
            "the overall district trend."
          )
        }
      } else {
        subgroup_text <- paste0(
          " Demographic subgroups generally followed similar ", 
          "attendance patterns to the overall district trend."
        )
      }
    } else {
      subgroup_text <- ""
    }
    
    trends_text <- paste0(trends_summary, subgroup_text)
  } else {
    trends_text <- "Limited historical data prevents comprehensive trend analysis."
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
    enrollment_text, 
    " ", 
    trends_text
  )
  
  return(overview_text)
}
