#' Run complete chronic absence analysis
#'
#' Main function that orchestrates the full chronic absence analysis pipeline
#' from baseline through demographic concentration analysis.
#'
#' @param data Student-level data frame with chronic absence information
#' @param baseline_year Numeric year for baseline analysis
#' @param reduction_pct Numeric percentage reduction target (e.g., 50 for 50%)
#' @param years_to_goal Numeric years to achieve reduction target
#' @return List containing all analysis results ready for reporting
#' @export
#' @keywords internal
analyze <- function(data, baseline_year, reduction_pct, years_to_goal) {
  # Validate inputs
  if (!baseline_year %in% data[["YEAR"]]) {
    stop("Baseline year ", baseline_year, " not found in data")
  }
  
  if (reduction_pct <= 0 || reduction_pct > 100) {
    stop("Reduction percentage must be between 1 and 100")
  }
  
  if (years_to_goal <= 0) {
    stop("Years to goal must be greater than 0")
  }
  
  # Get current year from data
  current_year <- max(data[["YEAR"]])
  
  # Run analysis pipeline
  baseline_results <- baseline(data, baseline_year)
  current_results <- current(data, current_year)
  trend_results <- trends(data, baseline_year, current_year)
  target_results <- targets(baseline_results, reduction_pct, years_to_goal)
  priority_results <- priorities(baseline_results, target_results)
  hotspot_results <- hotspots(baseline_results)
  demo_results <- demo(data, baseline_year)
  
  # Return combined results
  result <- list(
    baseline_results = baseline_results,
    current_results = current_results,
    trend_results = trend_results,
    target_results = target_results,
    priority_results = priority_results,
    hotspot_results = hotspot_results,
    demo_results = demo_results,
    parameters = list(
      baseline_year = baseline_year,
      current_year = current_year,
      reduction_pct = reduction_pct,
      years_to_goal = years_to_goal
    )
  )
  
  return(result)
}
