#' Identify districts with concentrated chronic absence problems
#'
#' Internal function to identify districts where a small number of schools
#' account for a large proportion of chronic absenteeism, indicating
#' opportunities for targeted school-level interventions.
#'
#' @param baseline_results Output from baseline() function
#' @param concentration_threshold Numeric threshold for identifying hotspots
#'   (default 0.35 means top 20% of schools account for 35%+ of chronic absence)
#' @param min_schools Minimum number of schools required for analysis (default 5)
#' @return List containing district concentration analysis and hotspot identification
#' @keywords internal
hotspots <- function(baseline_results, concentration_threshold = 0.25, min_schools = 5) {
 school_data <- baseline_results[["school"]]
 
 # Get unique districts with minimum school count
 district_counts <- table(school_data[["DISTRICT_NAME"]])
 eligible_districts <- names(district_counts[district_counts >= min_schools])
 
 district_analysis <- data.frame()
 
 for (district in eligible_districts) {
   # Get schools in this district
   district_schools <- school_data[school_data[["DISTRICT_NAME"]] == district, ]
   
   # Skip if no chronic absence
   if (sum(district_schools[["CHRONIC_ABSENT"]]) == 0) {
     next
   }
   
   # Sort schools by chronic absence (descending)
   district_schools <- district_schools[order(-district_schools[["CHRONIC_ABSENT"]]), ]
   
   total_schools <- nrow(district_schools)
   total_chronic <- sum(district_schools[["CHRONIC_ABSENT"]])
   
   # Calculate cumulative percentages
   district_schools[["CUMULATIVE_CHRONIC"]] <- cumsum(district_schools[["CHRONIC_ABSENT"]])
   district_schools[["CUMULATIVE_PCT"]] <- district_schools[["CUMULATIVE_CHRONIC"]] / total_chronic
   
   # Calculate concentration metrics
   top_10_pct_count <- max(1, round(total_schools * 0.1))
   top_20_pct_count <- max(1, round(total_schools * 0.2))
   
   top_10_pct_chronic <- sum(district_schools[["CHRONIC_ABSENT"]][1:top_10_pct_count])
   top_20_pct_chronic <- sum(district_schools[["CHRONIC_ABSENT"]][1:top_20_pct_count])
   
   top_10_pct_share <- top_10_pct_chronic / total_chronic
   top_20_pct_share <- top_20_pct_chronic / total_chronic
   
   # Find schools needed for 30% and 50% of chronic absence
   schools_for_30_pct <- which(district_schools[["CUMULATIVE_PCT"]] >= 0.3)[1]
   schools_for_50_pct <- which(district_schools[["CUMULATIVE_PCT"]] >= 0.5)[1]
   
   # Handle cases where thresholds aren't reached
   if (is.na(schools_for_30_pct)) schools_for_30_pct <- total_schools
   if (is.na(schools_for_50_pct)) schools_for_50_pct <- total_schools
   
   # FIXED: Determine if this is a hotspot district (HIGH concentration)
   is_hotspot <- (top_20_pct_share >= concentration_threshold)
   
   # Create district summary
   district_row <- data.frame(
     DISTRICT_NAME = district,
     TOTAL_SCHOOLS = total_schools,
     TOTAL_CHRONIC = total_chronic,
     TOP_10_PCT_SCHOOLS = top_10_pct_count,
     TOP_10_PCT_SHARE = round(top_10_pct_share, 3),
     TOP_20_PCT_SCHOOLS = top_20_pct_count,
     TOP_20_PCT_SHARE = round(top_20_pct_share, 3),
     SCHOOLS_FOR_30_PCT = schools_for_30_pct,
     SCHOOLS_FOR_50_PCT = schools_for_50_pct,
     IS_HOTSPOT = is_hotspot,
     stringsAsFactors = FALSE
   )
   
   district_analysis <- rbind(district_analysis, district_row)
 }
 
 # Sort by concentration (highest top 20% share first)
 district_analysis <- district_analysis[order(-district_analysis[["TOP_20_PCT_SHARE"]]), ]
 
 # Identify top hotspot districts
 hotspot_districts <- district_analysis[district_analysis[["IS_HOTSPOT"]] == TRUE, ]
 top_hotspots <- head(hotspot_districts, 10)
 
 # Create summary statistics
 total_hotspot_districts <- nrow(hotspot_districts)
 avg_concentration <- mean(district_analysis[["TOP_20_PCT_SHARE"]])
 
 # Return results
 result <- list(
   district_analysis = district_analysis,
   hotspot_districts = hotspot_districts,
   top_hotspots = top_hotspots,
   summary = list(
     total_districts = nrow(district_analysis),
     hotspot_districts = total_hotspot_districts,
     avg_top_20_pct_concentration = round(avg_concentration, 3),
     districts_above_threshold = sum(district_analysis[["TOP_20_PCT_SHARE"]] >= concentration_threshold)
   )
 )
 
 return(result)
}
