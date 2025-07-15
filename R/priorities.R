#' Identify priority areas for chronic absence interventions
#'
#' Internal function to rank geographic areas and demographic groups by
#' chronic absence burden and identify equity gaps requiring attention.
#'
#' @param baseline_results Output from baseline() function
#' @param target_results Output from targets() function
#' @return List containing priority rankings and equity gap analysis
#' @keywords internal
priorities <- function(baseline_results, target_results) {
 state_rate <- baseline_results[["state"]][["chronic_rate"]]
 
 # Geographic priorities using loop
 geographic_levels <- c("county", "district", "grade")
 name_columns <- c("COUNTY_NAME", "DISTRICT_NAME", "GRADE")
 geographic_priorities <- list()
 
 for (i in seq_along(geographic_levels)) {
   level <- geographic_levels[i]
   name_col <- name_columns[i]
   
   data_frame <- baseline_results[[level]]
   targets_frame <- target_results[[level]]
   
   rate_gap <- data_frame[["CHRONIC_RATE"]] - state_rate
   
   count_rank <- rank(-data_frame[["CHRONIC_ABSENT"]], ties.method = "min")
   rate_rank <- rank(-data_frame[["CHRONIC_RATE"]], ties.method = "min")
   reduction_rank <- rank(-targets_frame[["REDUCTION_NEEDED"]], ties.method = "min")
   
   priorities_df <- data.frame(
     NAME = data_frame[[name_col]],
     TOTAL_STUDENTS = data_frame[["TOTAL_STUDENTS"]],
     CHRONIC_ABSENT = data_frame[["CHRONIC_ABSENT"]],
     CHRONIC_RATE = data_frame[["CHRONIC_RATE"]],
     RATE_GAP_FROM_STATE = rate_gap,
     REDUCTION_NEEDED = targets_frame[["REDUCTION_NEEDED"]],
     COUNT_RANK = count_rank,
     RATE_RANK = rate_rank,
     REDUCTION_RANK = reduction_rank
   )
   
   names(priorities_df)[1] <- name_col
   geographic_priorities[[level]] <- priorities_df
 }
 
 # Demographic group priorities
 demographic_groups <- names(baseline_results[["demographics"]])
 demographic_priorities <- list()
 
 for (group in demographic_groups) {
   baseline_demo <- baseline_results[["demographics"]][[group]]
   target_demo <- target_results[["demographics"]][[group]]
   
   rate_gap <- baseline_demo[["rate"]] - state_rate
   
   demographic_priorities[[group]] <- list(
     total_students = baseline_demo[["total"]],
     chronic_absent = baseline_demo[["chronic"]],
     chronic_rate = baseline_demo[["rate"]],
     rate_gap_from_state = rate_gap,
     reduction_needed = target_demo[["reduction_needed"]],
     equity_concern = ifelse(
       rate_gap > 0.05,
       "HIGH",
       ifelse(rate_gap > 0.02, "MEDIUM", "LOW")
     )
   )
 }
 
 # Identify top priority areas by chronic absence count
 top_counties <- head(geographic_priorities[["county"]][
   order(-geographic_priorities[["county"]][["CHRONIC_ABSENT"]]), 
 ], 10)
 
 top_districts <- head(geographic_priorities[["district"]][
   order(-geographic_priorities[["district"]][["CHRONIC_ABSENT"]]), 
 ], 20)
 
 # Identify equity gaps
 equity_gaps <- list()
 for (group in demographic_groups) {
   demo_data <- demographic_priorities[[group]]
   if (demo_data[["rate_gap_from_state"]] > 0.05) {
     equity_gaps[[group]] <- demo_data
   }
 }
 
 # Return structured results
 result <- list(
   state_average_rate = state_rate,
   county_priorities = geographic_priorities[["county"]],
   district_priorities = geographic_priorities[["district"]],
   grade_priorities = geographic_priorities[["grade"]],
   demographic_priorities = demographic_priorities,
   top_priority_counties = top_counties,
   top_priority_districts = top_districts,
   equity_gaps = equity_gaps,
   summary = list(
     counties_above_state_avg = sum(geographic_priorities[["county"]][["RATE_GAP_FROM_STATE"]] > 0),
     districts_above_state_avg = sum(geographic_priorities[["district"]][["RATE_GAP_FROM_STATE"]] > 0),
     grades_above_state_avg = sum(geographic_priorities[["grade"]][["RATE_GAP_FROM_STATE"]] > 0),
     demographic_groups_with_gaps = length(equity_gaps)
   )
 )
 
 return(result)
}
