#' Calculate chronic absence reduction targets
#'
#' Internal function to calculate target chronic absence rates and annual
#' reduction goals based on baseline metrics and user-specified parameters.
#'
#' @param baseline_results Output from baseline() function
#' @param reduction_percent Numeric percentage reduction goal (e.g., 50 for 50%)
#' @param years_to_goal Numeric number of years to achieve the reduction goal
#' @return List containing target rates, reduction goals, and annual milestones
#' @keywords internal
targets <- function(
  baseline_results,
  reduction_percent,
  years_to_goal
) {
  baseline_year <- baseline_results[["baseline_year"]]
  target_year   <- baseline_year + years_to_goal
 
  reduction_factor <- reduction_percent / 100
 
  baseline_rate <- baseline_results[["state"]][["chronic_rate"]]
  target_rate   <- baseline_rate * (1 - reduction_factor)

  annual_reduction <-
    (baseline_rate - target_rate) / years_to_goal
 
  annual_rates    <- numeric(years_to_goal + 1)
  annual_rates[1] <- baseline_rate
 
  for (i in 2:(years_to_goal + 1)) {
    annual_rates[i] <- annual_rates[i - 1] - annual_reduction
  }
 
  total_students <- baseline_results[["state"]][["total_students"]]

  baseline_chronic_count <-
   baseline_results[["state"]][["chronic_absent_students"]]

  target_chronic_count <- round(total_students * target_rate)

  annual_reduction_count <- round(
   (baseline_chronic_count - target_chronic_count) / years_to_goal
  )
 
  annual_counts    <- numeric(years_to_goal + 1)
  annual_counts[1] <- baseline_chronic_count
 
  for (i in 2:(years_to_goal + 1)) {
    annual_counts[i] <-
      annual_counts[i - 1] - annual_reduction_count
  }
 

  county_targets <- baseline_results[["county"]]
  county_targets[["TARGET_RATE"]] <-
    county_targets[["CHRONIC_RATE"]] * (1 - reduction_factor)
  county_targets[["TARGET_CHRONIC"]] <- round(
    county_targets[["TOTAL_STUDENTS"]] * county_targets[["TARGET_RATE"]])
  county_targets[["REDUCTION_NEEDED"]] <-
    county_targets[["CHRONIC_ABSENT"]] - county_targets[["TARGET_CHRONIC"]]
 
 
  district_targets <- baseline_results[["district"]]
  district_targets[["TARGET_RATE"]] <-
    district_targets[["CHRONIC_RATE"]] * (1 - reduction_factor)
  district_targets[["TARGET_CHRONIC"]] <- round(
    district_targets[["TOTAL_STUDENTS"]] * district_targets[["TARGET_RATE"]])
  district_targets[["REDUCTION_NEEDED"]] <-
    district_targets[["CHRONIC_ABSENT"]] - district_targets[["TARGET_CHRONIC"]]
 

  grade_targets <- baseline_results[["grade"]]
  grade_targets[["TARGET_RATE"]] <-
    grade_targets[["CHRONIC_RATE"]] * (1 - reduction_factor)
  grade_targets[["TARGET_CHRONIC"]] <- round(
    grade_targets[["TOTAL_STUDENTS"]] * grade_targets[["TARGET_RATE"]])
  grade_targets[["REDUCTION_NEEDED"]] <-
    grade_targets[["CHRONIC_ABSENT"]] - grade_targets[["TARGET_CHRONIC"]]
 

  demographic_groups <- names(baseline_results[["demographics"]])
  demographic_targets <- list()
 
  for (group in demographic_groups) {
    baseline_demo <- baseline_results[["demographics"]][[group]]
    target_demo_rate <-
      baseline_demo[["rate"]] * (1 - reduction_factor)
    target_demo_chronic <-
      round(baseline_demo[["total"]] * target_demo_rate)
    reduction_needed <-
      baseline_demo[["chronic"]] - target_demo_chronic
   
    demographic_targets[[group]] <- list(
      baseline_total   = baseline_demo[["total"]],
      baseline_chronic = baseline_demo[["chronic"]],
      baseline_rate    = baseline_demo[["rate"]],
      target_rate      = target_demo_rate,
      target_chronic   = target_demo_chronic,
      reduction_needed = reduction_needed
    )
  }
 
  projection_years <- seq(baseline_year, target_year, by = 1)
 
  result <- list(
    parameters = list(
      baseline_year     = baseline_year,
      target_year       = target_year,
      reduction_percent = reduction_percent,
      years_to_goal     = years_to_goal
    ),
    state = list(
      baseline_rate           = baseline_rate,
      target_rate             = target_rate,
      baseline_chronic_count  = baseline_chronic_count,
      target_chronic_count    = target_chronic_count,
      total_reduction_needed  = baseline_chronic_count - target_chronic_count,
      annual_reduction_needed = annual_reduction_count,
      projection_years        = projection_years,
      annual_rates            = annual_rates,
      annual_counts           = annual_counts
    ),
    county       = county_targets,
    district     = district_targets,
    grade        = grade_targets,
    demographics = demographic_targets
  )
 
  return(result)
}
