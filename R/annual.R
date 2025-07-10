#' Calculate Year-to-Year Changes in Chronic Absenteeism
#' 
#' Calculates percentage point changes and percentage changes between 
#' consecutive years for all geographic levels and subgroups.
#' 
#' @param state_data Results from group_means()
#' @param county_data Results from county_means() 
#' @param district_data Results from district_means()
#' @param school_data Results from school_means()
#' @return List containing year-to-year changes for all levels
#' @keywords internal
annual <- function(state_data, county_data, district_data, school_data) {
  
  # Helper function using simple vectorized approach
  calculate_consecutive_changes <- function(data, level_name) {
    
    cat("Calculating year-to-year changes for", level_name, "level...\n")
    
    results <- data.frame()
    years <- sort(unique(data$YEAR))
    
    # Loop through consecutive year pairs
    for (i in 1:(length(years) - 1)) {
      year1 <- years[i]
      year2 <- years[i + 1]
      
      # Get data for both years
      data1 <- data[data$YEAR == year1, ]
      data2 <- data[data$YEAR == year2, ]
      
      # Merge on geographic and subgroup identifiers
      if (level_name == "state") {
        merged <- merge(data1, data2, by = "SUBGROUP", suffixes = c("_from", "_to"))
        id_cols <- "SUBGROUP"
      } else if (level_name == "county") {
        merged <- merge(data1, data2, by = c("COUNTY_ID", "SUBGROUP"), suffixes = c("_from", "_to"))
        id_cols <- c("COUNTY_ID", "COUNTY_NAME_from", "SUBGROUP")
      } else if (level_name == "district") {
        merged <- merge(data1, data2, by = c("DISTRICT_ID", "SUBGROUP"), suffixes = c("_from", "_to"))
        id_cols <- c("COUNTY_ID_from", "DISTRICT_ID", "COUNTY_NAME_from", "DISTRICT_NAME_from", "SUBGROUP")
      } else if (level_name == "school") {
        merged <- merge(data1, data2, by = c("SCHOOL_ID", "SUBGROUP"), suffixes = c("_from", "_to"))
        id_cols <- c("COUNTY_ID_from", "DISTRICT_ID_from", "SCHOOL_ID", "COUNTY_NAME_from", "DISTRICT_NAME_from", "SCHOOL_NAME_from", "SUBGROUP")
      }
      
      if (nrow(merged) > 0) {
        # Calculate changes
        merged$year_from <- year1
        merged$year_to <- year2
        merged$rate_from <- merged$absenteeism_rate_from
        merged$rate_to <- merged$absenteeism_rate_to
        merged$point_change <- round(merged$rate_to - merged$rate_from, 2)
        merged$percent_change <- ifelse(merged$rate_from != 0, 
                                       round((merged$rate_to - merged$rate_from) / merged$rate_from * 100, 2), 
                                       NA)
        
        # Select relevant columns
        if (level_name == "state") {
          year_results <- merged[, c("SUBGROUP", "year_from", "year_to", "rate_from", "rate_to", "point_change", "percent_change")]
          year_results$unit <- "STATE"
          year_results <- year_results[, c("unit", "SUBGROUP", "year_from", "year_to", "rate_from", "rate_to", "point_change", "percent_change")]
        } else {
          select_cols <- c(id_cols, "year_from", "year_to", "rate_from", "rate_to", "point_change", "percent_change")
          year_results <- merged[, select_cols]
          
          # Clean up column names (remove _from suffix from geographic names)
          if (level_name == "county") {
            names(year_results)[names(year_results) == "COUNTY_NAME_from"] <- "COUNTY_NAME"
          } else if (level_name == "district") {
            names(year_results)[names(year_results) == "COUNTY_ID_from"] <- "COUNTY_ID"
            names(year_results)[names(year_results) == "COUNTY_NAME_from"] <- "COUNTY_NAME"
            names(year_results)[names(year_results) == "DISTRICT_NAME_from"] <- "DISTRICT_NAME"
          } else if (level_name == "school") {
            names(year_results)[names(year_results) == "COUNTY_ID_from"] <- "COUNTY_ID"
            names(year_results)[names(year_results) == "DISTRICT_ID_from"] <- "DISTRICT_ID"
            names(year_results)[names(year_results) == "COUNTY_NAME_from"] <- "COUNTY_NAME"
            names(year_results)[names(year_results) == "DISTRICT_NAME_from"] <- "DISTRICT_NAME"
            names(year_results)[names(year_results) == "SCHOOL_NAME_from"] <- "SCHOOL_NAME"
          }
        }
        
        results <- rbind(results, year_results)
      }
    }
    
    return(results)
  }
  
  # Calculate for each level
  results <- list()
  results$state_level <- calculate_consecutive_changes(state_data, "state")
  results$county_level <- calculate_consecutive_changes(county_data, "county")
  results$district_level <- calculate_consecutive_changes(district_data, "district")
  results$school_level <- calculate_consecutive_changes(school_data, "school")
  
  cat("Year-to-year changes calculation complete.\n")
  return(results)
}

