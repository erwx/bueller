#' Helper function for geographic aggregation
#'
#' Internal helper function to aggregate student data by geographic levels
#' and calculate chronic absence totals and rates.
#'
#' @param data Student-level data frame
#' @param group_col Column name to group by
#' @param group_name Name for the grouping variable in output
#' @return Data frame with aggregated totals and chronic absence rates
#' @importFrom stats aggregate quantile
#' @importFrom utils head
#' @keywords internal
aggregate_by_group <- function(data, group_col, group_name) {
  totals <- aggregate(
    rep(1, nrow(data)),
    by = list(data[[group_col]]),
    FUN = sum
  )
  names(totals) <- c(group_name, "TOTAL_STUDENTS")
 
  chronic <- aggregate(
    data$CHRONIC_ABSENT,
    by = list(data[[group_col]]),
    FUN = sum
  )
  names(chronic) <- c(group_name, "CHRONIC_ABSENT")
 
  summary <- merge(totals, chronic, by = group_name)
  summary$CHRONIC_RATE <- summary$CHRONIC_ABSENT / 
                         summary$TOTAL_STUDENTS
 
  return(summary)
}



#' Helper function for demographic statistics
#'
#' Internal helper function to calculate chronic absence statistics for
#' demographic groups using indicator variables.
#'
#' @param data Student-level data frame
#' @param group_indicator Binary indicator vector for group membership
#' @return List with total students, chronic absent count, and rate
#' @keywords internal
calculate_demographic_stats <- function(data, group_indicator) {
 total <- sum(group_indicator)
 chronic <- sum(data$CHRONIC_ABSENT * group_indicator)
 rate <- chronic / total
 
 return(list(
   total = total,
   chronic = chronic,
   rate = rate
 ))
}



#' Helper function for geographic aggregation
#'
#' Internal helper function to aggregate student data by geographic levels
#' and calculate chronic absence totals and rates.
#'
#' @param data Student-level data frame
#' @param group_col Column name to group by
#' @param group_name Name for the grouping variable in output
#' @return Data frame with aggregated totals and chronic absence rates
#' @keywords internal
baseline <- function(data, baseline_year) {
 baseline_data <- data[data$YEAR == baseline_year, ]
 
 if (nrow(baseline_data) == 0) {
   stop("No data found for baseline year: ", baseline_year)
 }
 
 n_rows <- nrow(baseline_data)
 n_unique_students <- length(unique(baseline_data$STUDENT_ID))
 
 if (n_rows != n_unique_students) {
   cat("Found", n_rows - n_unique_students, 
       "duplicate student records in baseline year\n")
 }
 
 rm(n_rows, n_unique_students)
 
 baseline_data <- baseline_data[
   !duplicated(baseline_data$STUDENT_ID),
 ]
 
 total_students <- nrow(baseline_data)
 chronic_students <- sum(baseline_data$CHRONIC_ABSENT)
 state_rate <- chronic_students / total_students
 
 county_summary <- aggregate_by_group(baseline_data, 
                                      "COUNTY_NAME", 
                                      "COUNTY_NAME")
 
 district_summary <- aggregate_by_group(baseline_data, 
                                        "DISTRICT_NAME", 
                                        "DISTRICT_NAME")
 
 grade_summary <- aggregate_by_group(baseline_data, 
                                     "GRADE", 
                                     "GRADE")
 
 school_summary <- aggregate_by_group(baseline_data, 
                                      "SCHOOL_NAME", 
                                      "SCHOOL_NAME")
 
 school_district_map <- aggregate(
   baseline_data$DISTRICT_NAME,
   by = list(baseline_data$SCHOOL_NAME),
   FUN = function(x) x[1]
 )
 names(school_district_map) <- c("SCHOOL_NAME", "DISTRICT_NAME")
 
 school_county_map <- aggregate(
   baseline_data$COUNTY_NAME,
   by = list(baseline_data$SCHOOL_NAME),
   FUN = function(x) x[1]
 )
 names(school_county_map) <- c("SCHOOL_NAME", "COUNTY_NAME")
 
 school_summary <- merge(school_summary, school_district_map, 
                         by = "SCHOOL_NAME")
 school_summary <- merge(school_summary, school_county_map, 
                         by = "SCHOOL_NAME")
 
 demographic_cols <- c("MALE", "FEMALE", "HISPANIC", "WHITE", 
                       "ASIAN", "BLACK", "ELL", "DISADVANTAGE", 
                       "DISABILITY")
 
 demographic_names <- c("male", "female", "hispanic", "white",
                        "asian", "black", "ell", "disadvantaged",
                        "disability")
 
 demographics <- list()
 
 for (i in seq_along(demographic_cols)) {
   demographics[[demographic_names[i]]] <- calculate_demographic_stats(
     baseline_data,
     baseline_data[[demographic_cols[i]]]
   )
 }
 
 result <- list(
   baseline_year = baseline_year,
   state = list(
     total_students = total_students,
     chronic_absent_students = chronic_students,
     chronic_rate = state_rate
   ),
   county = county_summary,
   district = district_summary,
   grade = grade_summary,
   school = school_summary,
   demographics = demographics
 )
 
 return(result)
}
