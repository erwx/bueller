#' Create Curated JSON for Chat Context
#' 
#' Create JSON summary of analysis results for the chat agent,
#'
#' @param results Output from prep()
#' @return JSON string containing curated analysis data
json <- function(results) {
  analysis_data <- list(
    
    # Basic scope
    metadata = list(
      time_period = paste(results$first_year, "to", results$current_year),
      geography = "California districts (excluding County Offices of Education)",
      total_districts = results$ela_summary$total_districts,
      total_students = results$ela_summary$total_enrollment,
      subjects = c("ELA", "MATH"),
      min_enrollment = 30
    ),
    
    # Current state summary
    current_state = list(
      participation = list(
        ela_median = results$ela_summary$median_participation,
        math_median = results$math_summary$median_participation,
        districts_below_95_pct = results$ela_summary$pct_below_95,
        range = paste0(results$worst_participation$participation_rate, "% to ", 
                      results$best_participation$participation_rate, "%")
      ),
      absenteeism = list(
        ela_median = results$ela_summary$median_absenteeism,
        math_median = results$math_summary$median_absenteeism,
        districts_above_20_pct = results$ela_summary$pct_above_20,
        range = paste0(results$best_absenteeism$chronic_absent_rate, "% to ",
                      results$worst_absenteeism$chronic_absent_rate, "%")
      )
    ),
    
    # Key trend data (sample years)
    trends = list(
      participation_by_year = results$yearly_trends_ela[c("YEAR", "median_rate", "below_95_pct")],
      absenteeism_by_year = results$yearly_absent_ela[c("YEAR", "median_rate", "above_20_pct")]
    ),
    
    # Extreme examples
    district_examples = list(
      worst_participation = list(
        district = results$worst_participation$DISTRICT_NAME,
        rate = results$worst_participation$participation_rate
      ),
      best_participation = list(
        district = results$best_participation$DISTRICT_NAME,
        rate = results$best_participation$participation_rate
      ),
      worst_absenteeism = list(
        district = results$worst_absenteeism$DISTRICT_NAME,
        rate = results$worst_absenteeism$chronic_absent_rate
      ),
      best_absenteeism = list(
        district = results$best_absenteeism$DISTRICT_NAME,
        rate = results$best_absenteeism$chronic_absent_rate
      )
    ),
    
    # Student group disparities
    equity_analysis = list(
      largest_participation_gap = list(
        group = results$largest_participation_gap$DISAGGREGATED_GROUP,
        district = results$largest_participation_gap$DISTRICT_NAME,
        group_rate = results$largest_participation_gap$participation_rate,
        district_average = results$largest_participation_gap$district_participation,
        gap = results$largest_participation_gap$participation_gap
      ),
      largest_absenteeism_gap = list(
        group = results$largest_absenteeism_gap$DISAGGREGATED_GROUP,
        district = results$largest_absenteeism_gap$DISTRICT_NAME,
        group_rate = results$largest_absenteeism_gap$chronic_absent_rate,
        district_average = results$largest_absenteeism_gap$district_absenteeism,
        gap = results$largest_absenteeism_gap$absenteeism_gap
      ),
      significant_gaps = list(
        participation_gaps_15plus = results$significant_participation_gaps,
        absenteeism_gaps_15plus = results$significant_absenteeism_gaps
      )
    ),
    
    # School-level concentration patterns
    school_concentration = list(
      description = "Districts where problems are concentrated in specific schools vs system-wide",
      ela_driven_districts = results$ela_driven_participation,
      math_driven_districts = results$math_driven_participation,
      absenteeism_driven_districts = results$districts_driven_absenteeism,
      explanation = "These are districts where 1/3 or fewer schools drive the problem"
    ),
    
    # Key thresholds for interpretation
    benchmarks = list(
      federal_participation_threshold = "95%",
      high_absenteeism_threshold = "20%",
      severe_absenteeism_threshold = "30%",
      significant_gap_threshold = "15 percentage points"
    )
  )
  
  # Convert to compact JSON
  jsonlite::toJSON(analysis_data, auto_unbox = TRUE, pretty = FALSE)
}
