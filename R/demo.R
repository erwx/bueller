#' Identify demographic concentration within schools
#'
#' Internal function to identify schools where specific demographic groups
#' account for disproportionate shares of chronic absenteeism.
#'
#' @param data Student-level data frame 
#' @param baseline_year Numeric year for analysis
#' @param concentration_threshold Numeric threshold (default 0.4)
#' @return List containing demographic concentration analysis
#' @keywords internal
demo <- function(data, baseline_year, concentration_threshold = 0.2) {
 # Filter to baseline year
 baseline_data <- data[data[["YEAR"]] == baseline_year, ]
 
 # Remove duplicates
 baseline_data <- baseline_data[
   !duplicated(baseline_data[["STUDENT_ID"]]), 
 ]
 
 # Filter to schools with 20+ students
 school_counts <- table(baseline_data[["SCHOOL_NAME"]])
 valid_schools <- names(school_counts[school_counts >= 20])
 baseline_data <- baseline_data[
   baseline_data[["SCHOOL_NAME"]] %in% valid_schools, 
 ]
 
 # Demographic columns
 demo_cols <- c("HISPANIC", "WHITE", "ASIAN", "BLACK", "ELL", 
                "DISADVANTAGE", "DISABILITY")
 demo_names <- c("hispanic", "white", "asian", "black", "ell",
                 "disadvantaged", "disability")
 
 # Calculate school totals
 school_totals <- aggregate(
   rep(1, nrow(baseline_data)),
   by = list(baseline_data[["SCHOOL_NAME"]]),
   FUN = sum
 )
 names(school_totals) <- c("SCHOOL_NAME", "TOTAL")
 
 school_chronic <- aggregate(
   baseline_data[["CHRONIC_ABSENT"]],
   by = list(baseline_data[["SCHOOL_NAME"]]),
   FUN = sum
 )
 names(school_chronic) <- c("SCHOOL_NAME", "CHRONIC")
 
 # Merge school data
 school_summary <- merge(school_totals, school_chronic, 
                        by = "SCHOOL_NAME")
 
 # Remove schools with no chronic absence
 school_summary <- school_summary[school_summary[["CHRONIC"]] > 0, ]
 
 # Calculate demographic concentrations
 hotspot_list <- list()
 
 for (i in seq_along(demo_cols)) {
   col <- demo_cols[i]
   name <- demo_names[i]
   
   # Group enrollment by school and demographic
   group_totals <- aggregate(
     baseline_data[[col]],
     by = list(baseline_data[["SCHOOL_NAME"]]),
     FUN = sum
   )
   names(group_totals) <- c("SCHOOL_NAME", "GROUP_TOTAL")
   
   # Group chronic absence by school and demographic
   group_chronic <- aggregate(
     baseline_data[["CHRONIC_ABSENT"]] * baseline_data[[col]],
     by = list(baseline_data[["SCHOOL_NAME"]]),
     FUN = sum
   )
   names(group_chronic) <- c("SCHOOL_NAME", "GROUP_CHRONIC")
   
   # Merge demographic data
   demo_data <- merge(group_totals, group_chronic, 
                     by = "SCHOOL_NAME")
   demo_data <- merge(school_summary, demo_data, 
                     by = "SCHOOL_NAME")
   
   # Calculate shares
   demo_data[["ENROLLMENT_SHARE"]] <- demo_data[["GROUP_TOTAL"]] / 
                                     demo_data[["TOTAL"]]
   demo_data[["CHRONIC_SHARE"]] <- demo_data[["GROUP_CHRONIC"]] / 
                                  demo_data[["CHRONIC"]]
   
   # Identify hotspots (chronic share > enrollment share + threshold)
   hotspots <- demo_data[
     demo_data[["CHRONIC_SHARE"]] > 
     (demo_data[["ENROLLMENT_SHARE"]] + concentration_threshold) & 
     demo_data[["ENROLLMENT_SHARE"]] >= 0.1 &
     demo_data[["GROUP_TOTAL"]] >= 5, 
   ]
   
   if (nrow(hotspots) > 0) {
     hotspots[["DEMOGRAPHIC"]] <- name
     hotspot_list[[name]] <- hotspots
   }
 }
 
 # Combine all hotspots
 if (length(hotspot_list) > 0) {
   all_hotspots <- do.call(rbind, hotspot_list)
 } else {
   all_hotspots <- data.frame()
 }
 
 # Summary statistics
 if (nrow(all_hotspots) > 0) {
   hotspot_schools <- unique(all_hotspots[["SCHOOL_NAME"]])
   common_demo <- names(sort(table(all_hotspots[["DEMOGRAPHIC"]]), 
                            decreasing = TRUE))[1]
 } else {
   hotspot_schools <- character(0)
   common_demo <- "none"
 }
 
 result <- list(
   schools_analyzed = nrow(school_summary),
   hotspot_details = all_hotspots,
   hotspot_schools = hotspot_schools,
   summary = list(
     total_schools = nrow(school_summary),
     schools_with_hotspots = length(hotspot_schools),
     most_common_group = common_demo,
     hotspot_rate = round(length(hotspot_schools) / 
                         nrow(school_summary), 3)
   )
 )
 
 return(result)
}
