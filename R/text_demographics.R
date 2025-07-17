#' Generate demographic analysis text
#'
#' @param analysis_results Results from analyze_district function
#' @return Character string with demographic analysis text
#' @keywords internal
text_demographics <- function(analysis_results) {
  district_name        <- analysis_results[["district_name"]]
  current_year         <- analysis_results[["current_year"]]
  district_rate        <- analysis_results[["demographics"]][["district_rate"]]
  demographic_summary  <- analysis_results[["demographics"]][["demographic_summary"]]
  
  district_rate_pct <- paste0(round(district_rate * 100, 1), "%")
  
  above_avg   <- demographic_summary[demographic_summary[["GAP_FROM_DISTRICT"]] > 0.005, ]
  below_avg   <- demographic_summary[demographic_summary[["GAP_FROM_DISTRICT"]] < -0.005, ]
  similar_avg <- demographic_summary[abs(demographic_summary[["GAP_FROM_DISTRICT"]]) <= 0.005, ]
  
  largest_pos_gap <- max(demographic_summary[["GAP_FROM_DISTRICT"]])
  largest_neg_gap <- min(demographic_summary[["GAP_FROM_DISTRICT"]])
  
  highest_gap_group <- demographic_summary[["GROUP"]][which.max(demographic_summary[["GAP_FROM_DISTRICT"]])]
  lowest_gap_group  <- demographic_summary[["GROUP"]][which.min(demographic_summary[["GAP_FROM_DISTRICT"]])]
  
  highest_gap_rate <- demographic_summary[["CHRONIC_RATE"]][which.max(demographic_summary[["GAP_FROM_DISTRICT"]])]
  lowest_gap_rate  <- demographic_summary[["CHRONIC_RATE"]][which.min(demographic_summary[["GAP_FROM_DISTRICT"]])]
  
  largest_pos_gap_fmt  <- paste0(round(largest_pos_gap * 100, 1), " percentage points")
  largest_neg_gap_fmt  <- paste0(round(abs(largest_neg_gap) * 100, 1), " percentage points")
  highest_gap_rate_fmt <- paste0(round(highest_gap_rate * 100, 1), "%")
  lowest_gap_rate_fmt  <- paste0(round(lowest_gap_rate * 100, 1), "%")
  
  # Groups above average text
  if (nrow(above_avg) == 0) {
    above_text <- "No demographic groups show chronic absence rates above the district average."
  } else if (nrow(above_avg) == 1) {
    above_text <- paste0(
      "One demographic group shows chronic absence rates above the district average: ",
      above_avg[["GROUP"]][1],
      " students at ",
      paste0(round(above_avg[["CHRONIC_RATE"]][1] * 100, 1), "%"),
      "."
    )
  } else {
    above_groups <- paste(above_avg[["GROUP"]], collapse = ", ")
    above_text <- paste0(
      nrow(above_avg),
      " demographic groups show chronic absence rates above the district average: ",
      above_groups,
      "."
    )
  }
  
  # Groups below average text
  if (nrow(below_avg) == 0) {
    below_text <- "No demographic groups show chronic absence rates below the district average."
  } else if (nrow(below_avg) == 1) {
    below_text <- paste0(
      "One demographic group shows chronic absence rates below the district average: ",
      below_avg[["GROUP"]][1],
      " students at ",
      paste0(round(below_avg[["CHRONIC_RATE"]][1] * 100, 1), "%"),
      "."
    )
  } else {
    below_groups <- paste(below_avg[["GROUP"]], collapse = ", ")
    below_text <- paste0(
      nrow(below_avg),
      " demographic groups show chronic absence rates below the district average: ",
      below_groups,
      "."
    )
  }
  
  # Groups similar to average text
  if (nrow(similar_avg) == 0) {
    similar_text <- ""
  } else if (nrow(similar_avg) == 1) {
    similar_text <- paste0(
      "One demographic group shows chronic absence rates similar to the district average: ",
      similar_avg[["GROUP"]][1],
      " students at ",
      paste0(round(similar_avg[["CHRONIC_RATE"]][1] * 100, 1), "%"),
      "."
    )
  } else {
    similar_groups <- paste(similar_avg[["GROUP"]], collapse = ", ")
    similar_text <- paste0(
      nrow(similar_avg),
      " demographic groups show chronic absence rates similar to the district average: ",
      similar_groups,
      "."
    )
  }
  
  # Gap analysis text
  gap_spread <- largest_pos_gap - largest_neg_gap
  gap_spread_fmt <- paste0(round(gap_spread * 100, 1), " percentage points")
  
  if (largest_pos_gap > 0.005 && largest_neg_gap < -0.005) {
    gap_text <- paste0(
      "The largest gap above the district average is ",
      largest_pos_gap_fmt,
      " for ",
      highest_gap_group,
      " students (",
      highest_gap_rate_fmt,
      "), while the largest gap below the district average is ",
      largest_neg_gap_fmt,
      " for ",
      lowest_gap_group,
      " students (",
      lowest_gap_rate_fmt,
      "). The total spread across demographic groups is ",
      gap_spread_fmt,
      "."
    )
  } else if (largest_pos_gap > 0.005) {
    gap_text <- paste0(
      "The largest gap above the district average is ",
      largest_pos_gap_fmt,
      " for ",
      highest_gap_group,
      " students (",
      highest_gap_rate_fmt,
      ")."
    )
  } else if (largest_neg_gap < -0.005) {
    gap_text <- paste0(
      "The largest gap below the district average is ",
      largest_neg_gap_fmt,
      " for ",
      lowest_gap_group,
      " students (",
      lowest_gap_rate_fmt,
      ")."
    )
  } else {
    gap_text <- "All demographic groups show chronic absence rates similar to the district average."
  }
  
  # Enrollment context for largest groups
  largest_group <- demographic_summary[["GROUP"]][which.max(demographic_summary[["TOTAL_STUDENTS"]])]
  largest_group_size <- max(demographic_summary[["TOTAL_STUDENTS"]])
  largest_group_chronic <- demographic_summary[["CHRONIC_ABSENT"]][which.max(demographic_summary[["TOTAL_STUDENTS"]])]
  
  enrollment_text <- paste0(
    "The largest demographic group by enrollment is ",
    largest_group,
    " students with ",
    format(largest_group_size, big.mark = ","),
    " students, accounting for ",
    format(largest_group_chronic, big.mark = ","),
    " chronically absent students."
  )
  
  # Combine demographic analysis text
  demographics_text <- paste0(
    "Chronic absence patterns vary across demographic groups in ",
    current_year,
    ", with a district average of ",
    district_rate_pct,
    ". ",
    above_text,
    " ",
    below_text
  )
  
  if (nchar(similar_text) > 0) {
    demographics_text <- paste0(demographics_text, " ", similar_text)
  }
  
  demographics_text <- paste0(
    demographics_text,
    " ",
    gap_text,
    " ",
    enrollment_text
  )
  
  return(demographics_text)
}
