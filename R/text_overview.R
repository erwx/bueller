#' Generate comprehensive district overview text (500+ words)
#'
#' @param analysis_results Results from analyze_district function
#' @return Character string with district overview text
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
  
  avg_students_per_school <- round(total_students / n_schools, 0)
  
  # Basic district description
  basic_description <- paste0(
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
    " students per school."
  )
  
  # Enrollment trends analysis
  if (nrow(enrollment_trend) > 1) {
    first_year       <- enrollment_trend[["YEAR"]][1]
    last_year        <- enrollment_trend[["YEAR"]][nrow(enrollment_trend)]
    first_enrollment <- enrollment_trend[["TOTAL_STUDENTS"]][1]
    last_enrollment  <- enrollment_trend[["TOTAL_STUDENTS"]][
      nrow(enrollment_trend)
    ]
    
    enrollment_change     <- last_enrollment - first_enrollment
    enrollment_pct_change <- (enrollment_change / first_enrollment) * 100
    
    # Calculate average enrollment
    avg_enrollment <- round(mean(enrollment_trend[["TOTAL_STUDENTS"]]))
    min_enrollment <- min(enrollment_trend[["TOTAL_STUDENTS"]])
    max_enrollment <- max(enrollment_trend[["TOTAL_STUDENTS"]])
    
    if (abs(enrollment_change) < 50) {
      enrollment_description <- paste0(
        "Total enrollment remained stable across the period, ", 
        "ranging from ", 
        format(min_enrollment, big.mark = ","), 
        " to ", 
        format(max_enrollment, big.mark = ","), 
        " students with an average of ", 
        format(avg_enrollment, big.mark = ","), 
        " students per year."
      )
    } else if (enrollment_change > 0) {
      enrollment_description <- paste0(
        "Total enrollment grew from ", 
        format(first_enrollment, big.mark = ","), 
        " students in ", 
        first_year, 
        " to ", 
        format(last_enrollment, big.mark = ","), 
        " students in ", 
        last_year, 
        ", representing a ", 
        round(enrollment_pct_change, 1), 
        "% increase. The district reached its peak enrollment of ", 
        format(max_enrollment, big.mark = ","), 
        " students during this period."
      )
    } else {
      enrollment_description <- paste0(
        "Total enrollment declined from ", 
        format(first_enrollment, big.mark = ","), 
        " students in ", 
        first_year, 
        " to ", 
        format(last_enrollment, big.mark = ","), 
        " students in ", 
        last_year, 
        ", representing a ", 
        round(abs(enrollment_pct_change), 1), 
        "% decrease. The highest enrollment of ", 
        format(max_enrollment, big.mark = ","), 
        " students occurred during this period."
      )
    }
  } else {
    enrollment_description <- paste0(
      "Analysis includes data for ", 
      current_year, 
      " only."
    )
  }
  
  # Comprehensive year-to-year trends analysis
  if (trends[["has_trends"]] && nrow(trends[["overall_trends"]]) > 1) {
    overall_trends <- trends[["overall_trends"]]
    years          <- overall_trends[["YEAR"]]
    first_year     <- min(years)
    last_year      <- max(years)
    first_rate     <- overall_trends[["CHRONIC_RATE"]][1]
    last_rate      <- overall_trends[["CHRONIC_RATE"]][
      nrow(overall_trends)
    ]
    
    # Calculate descriptive statistics
    total_change      <- last_rate - first_rate
    avg_rate          <- mean(overall_trends[["CHRONIC_RATE"]])
    min_rate          <- min(overall_trends[["CHRONIC_RATE"]])
    max_rate          <- max(overall_trends[["CHRONIC_RATE"]])
    rate_range        <- max_rate - min_rate
    
    # Find peak and lowest years
    peak_year   <- overall_trends[["YEAR"]][
      which.max(overall_trends[["CHRONIC_RATE"]])
    ]
    lowest_year <- overall_trends[["YEAR"]][
      which.min(overall_trends[["CHRONIC_RATE"]])
    ]
    
    # Analyze year-to-year changes
    rate_changes <- overall_trends[["RATE_CHANGE"]][
      !is.na(overall_trends[["RATE_CHANGE"]])
    ]
    
    if (length(rate_changes) > 0) {
      avg_change        <- mean(rate_changes)
      largest_increase  <- max(rate_changes)
      largest_decrease  <- min(rate_changes)
      volatility        <- sd(rate_changes)
      
      increase_year <- overall_trends[["YEAR"]][
        which.max(overall_trends[["RATE_CHANGE"]])
      ]
      decrease_year <- overall_trends[["YEAR"]][
        which.min(overall_trends[["RATE_CHANGE"]])
      ]
      
      # Count years with increases/decreases
      increases <- sum(rate_changes > 0.005)
      decreases <- sum(rate_changes < -0.005)
      stable    <- length(rate_changes) - increases - decreases
      
      trends_description <- paste0(
        "The district's chronic absence rate varied from ", 
        paste0(round(min_rate * 100, 1), "%"), 
        " in ", 
        lowest_year, 
        " to ", 
        paste0(round(max_rate * 100, 1), "%"), 
        " in ", 
        peak_year, 
        ", spanning a range of ", 
        round(rate_range * 100, 1), 
        " percentage points. The average rate across all years was ", 
        paste0(round(avg_rate * 100, 1), "%"), 
        ". From ", 
        first_year, 
        " to ", 
        last_year, 
        ", the overall change was ", 
        ifelse(total_change >= 0, "+", ""), 
        round(total_change * 100, 1), 
        " percentage points. Year-to-year changes averaged ", 
        ifelse(avg_change >= 0, "+", ""), 
        round(avg_change * 100, 1), 
        " percentage points, with a standard deviation of ", 
        round(volatility * 100, 1), 
        " percentage points. The largest single-year increase was ", 
        round(largest_increase * 100, 1), 
        " percentage points in ", 
        increase_year, 
        ", while the largest decrease was ", 
        round(abs(largest_decrease) * 100, 1), 
        " percentage points in ", 
        decrease_year, 
        ". Across all year-to-year transitions, ", 
        increases, 
        " showed increases, ", 
        decreases, 
        " showed decreases, and ", 
        stable, 
        " remained stable."
      )
    } else {
      trends_description <- paste0(
        "The district's chronic absence rate ranged from ", 
        paste0(round(min_rate * 100, 1), "%"), 
        " to ", 
        paste0(round(max_rate * 100, 1), "%"), 
        " across the ", 
        length(years), 
        "-year period."
      )
    }
    
    # Student subgroup trends analysis
    demographic_trends <- trends[["demographic_trends"]]
    if (length(demographic_trends) > 0) {
      total_subgroups <- length(demographic_trends)
      
      # Count subgroups with sufficient data
      subgroups_with_data <- 0
      improving_count     <- 0
      worsening_count     <- 0
      stable_count        <- 0
      
      for (group_name in names(demographic_trends)) {
        group_data <- demographic_trends[[group_name]]
        if (nrow(group_data) > 1) {
          subgroups_with_data <- subgroups_with_data + 1
          
          group_first_rate <- group_data[["CHRONIC_RATE"]][1]
          group_last_rate  <- group_data[["CHRONIC_RATE"]][
            nrow(group_data)
          ]
          group_change     <- group_last_rate - group_first_rate
          
          if (group_change < -0.02) {
            improving_count <- improving_count + 1
          } else if (group_change > 0.02) {
            worsening_count <- worsening_count + 1
          } else {
            stable_count <- stable_count + 1
          }
        }
      }
      
      subgroup_description <- paste0(
        "Student subgroup data are available for ", 
        subgroups_with_data, 
        " demographic groups across multiple years. Over the ", 
        "analysis period, ", 
        improving_count, 
        " subgroups showed decreases in chronic absence rates ", 
        "of 2 percentage points or more, ", 
        worsening_count, 
        " subgroups showed increases of 2 percentage points or more, ", 
        "and ", 
        stable_count, 
        " subgroups remained within 2 percentage points of their ", 
        "starting rates. The complete year-by-year data for all ", 
        "student subgroups are presented in the table below, ", 
        "showing enrollment, chronic absence counts, rates, and ", 
        "year-over-year changes for each demographic group."
      )
    } else {
      subgroup_description <- paste0(
        "Student subgroup trend data are not available for ", 
        "multi-year analysis."
      )
    }
  } else {
    trends_description <- paste0(
      "Multi-year chronic absence trend data are not available. ", 
      "Analysis is limited to ", 
      current_year, 
      " data."
    )
    subgroup_description <- ""
  }
  
  # Combine all sections
  overview_text <- paste0(
    basic_description, 
    " ", 
    enrollment_description, 
    " ", 
    trends_description, 
    " ", 
    subgroup_description
  )
  
  return(overview_text)
}
