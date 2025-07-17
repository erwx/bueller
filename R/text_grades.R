#' Generate grade analysis text
#'
#' @param analysis_results Results from analyze_district function
#' @return Character string with grade analysis text
#' @keywords internal
text_grades <- function(analysis_results) {
  district_name    <- analysis_results[["district_name"]]
  current_year     <- analysis_results[["current_year"]]
  grade_summary    <- analysis_results[["grades"]][["grade_summary"]]
  
  total_students <- sum(grade_summary[["TOTAL_STUDENTS"]])
  total_chronic  <- sum(grade_summary[["CHRONIC_ABSENT"]])
  district_rate  <- total_chronic / total_students
  
  highest_rate_grade <- grade_summary[["GRADE"]][which.max(grade_summary[["CHRONIC_RATE"]])]
  lowest_rate_grade  <- grade_summary[["GRADE"]][which.min(grade_summary[["CHRONIC_RATE"]])]
  
  highest_rate <- max(grade_summary[["CHRONIC_RATE"]])
  lowest_rate  <- min(grade_summary[["CHRONIC_RATE"]])
  rate_range   <- highest_rate - lowest_rate
  
  largest_grade         <- grade_summary[["GRADE"]][which.max(grade_summary[["TOTAL_STUDENTS"]])]
  largest_grade_size    <- max(grade_summary[["TOTAL_STUDENTS"]])
  largest_grade_chronic <- grade_summary[["CHRONIC_ABSENT"]][which.max(grade_summary[["TOTAL_STUDENTS"]])]
  
  most_chronic_grade <- grade_summary[["GRADE"]][which.max(grade_summary[["CHRONIC_ABSENT"]])]
  most_chronic_count <- max(grade_summary[["CHRONIC_ABSENT"]])
  
  district_rate_pct <- paste0(round(district_rate * 100, 1), "%")
  highest_rate_pct  <- paste0(round(highest_rate * 100, 1), "%")
  lowest_rate_pct   <- paste0(round(lowest_rate * 100, 1), "%")
  rate_range_pct    <- paste0(round(rate_range * 100, 1), 
                           " percentage points")
  largest_grade_size_fmt    <- format(largest_grade_size, big.mark = ",")
  largest_grade_chronic_fmt <- format(largest_grade_chronic, big.mark = ",")
  most_chronic_count_fmt    <- format(most_chronic_count, big.mark = ",")
  
  above_avg_grades   <- grade_summary[grade_summary[["CHRONIC_RATE"]] > district_rate + 0.005, ]
  below_avg_grades   <- grade_summary[grade_summary[["CHRONIC_RATE"]] < district_rate - 0.005, ]
  similar_avg_grades <- grade_summary[abs(grade_summary[["CHRONIC_RATE"]] - district_rate) <= 0.005, ]
  
  grade_span <- range(grade_summary[["GRADE"]])
  if (grade_span[1] == grade_span[2]) {
    span_text <- paste0("grade ", grade_span[1])
  } else {
    span_text <- paste0("grades ", grade_span[1], " through ", grade_span[2])
  }
  
  # Performance range text
  if (nrow(grade_summary) == 1) {
    range_text <- paste0(
      "The district serves ",
      span_text,
      " with a chronic absence rate of ",
      highest_rate_pct,
      "."
    )
  } else {
    range_text <- paste0(
      "Chronic absence rates across ",
      span_text,
      " range from ",
      lowest_rate_pct,
      " in grade ",
      lowest_rate_grade,
      " to ",
      highest_rate_pct,
      " in grade ",
      highest_rate_grade,
      ", a spread of ",
      rate_range_pct,
      "."
    )
  }
  
  # Grades above average text
  if (nrow(above_avg_grades) == 0) {
    above_text <- ""
  } else if (nrow(above_avg_grades) == 1) {
    above_text <- paste0(
      "Grade ",
      above_avg_grades[["GRADE"]][1],
      " shows chronic absence rates above the district average."
    )
  } else {
    above_grades_list <- paste(above_avg_grades[["GRADE"]], collapse = ", ")
    above_text <- paste0(
      "Grades ",
      above_grades_list,
      " show chronic absence rates above the district average."
    )
  }
  
  # Grades below average text
  if (nrow(below_avg_grades) == 0) {
    below_text <- ""
  } else if (nrow(below_avg_grades) == 1) {
    below_text <- paste0(
      "Grade ",
      below_avg_grades[["GRADE"]][1],
      " shows chronic absence rates below the district average."
    )
  } else {
    below_grades_list <- paste(below_avg_grades[["GRADE"]], collapse = ", ")
    below_text <- paste0(
      "Grades ",
      below_grades_list,
      " show chronic absence rates below the district average."
    )
  }
  
  # Grades similar to average text
  if (nrow(similar_avg_grades) == 0) {
    similar_text <- ""
  } else if (nrow(similar_avg_grades) == 1) {
    similar_text <- paste0(
      "Grade ",
      similar_avg_grades[["GRADE"]][1],
      " shows chronic absence rates similar to the district average."
    )
  } else {
    similar_grades_list <- paste(similar_avg_grades[["GRADE"]], collapse = ", ")
    similar_text <- paste0(
      "Grades ",
      similar_grades_list,
      " show chronic absence rates similar to the district average."
    )
  }
  
  # Enrollment and impact context
  enrollment_text <- paste0(
    "Grade ",
    largest_grade,
    " has the largest enrollment with ",
    largest_grade_size_fmt,
    " students and ",
    largest_grade_chronic_fmt,
    " chronically absent students."
  )
  
  # Most chronic absence by count
  if (most_chronic_grade == largest_grade) {
    impact_text <- paste0(
      "This grade also accounts for the most chronic absence by count."
    )
  } else {
    impact_text <- paste0(
      "Grade ",
      most_chronic_grade,
      " accounts for the most chronic absence by count with ",
      most_chronic_count_fmt,
      " students."
    )
  }
  
  # Combine grade analysis text
  grades_text <- paste0(
    "The district serves ",
    format(total_students, big.mark = ","),
    " students across ",
    span_text,
    " in ",
    current_year,
    ", with a district average chronic absence rate of ",
    district_rate_pct,
    ". ",
    range_text
  )
  
  # Add performance relative to average
  performance_parts <- c(above_text, below_text, similar_text)
  performance_parts <- performance_parts[nchar(performance_parts) > 0]
  
  if (length(performance_parts) > 0) {
    grades_text <- paste0(
      grades_text,
      " ",
      paste(performance_parts, collapse = " ")
    )
  }
  
  grades_text <- paste0(
    grades_text,
    " ",
    enrollment_text,
    " ",
    impact_text
  )
  
  return(grades_text)
}
