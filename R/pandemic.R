#' Calculate Pre-Pandemic Comparison (2018 vs 2023)
#' 
#' Calculates percentage point changes and percentage changes from 
#' pre-pandemic (2018) to current (2023) levels for all geographic levels.
#' 
#' @param state_data Results from group_means()
#' @param county_data Results from county_means()
#' @param district_data Results from district_means() 
#' @param school_data Results from school_means()
#' @return List containing pre-pandemic comparisons for all levels
#' @keywords internal
pandemic <- function(state_data, county_data, district_data, school_data) {
  
  # Helper function using vectorized merge approach
  calculate_pre_pandemic_changes <- function(data, level_name) {
    
    cat("Calculating pre-pandemic comparison for", level_name, "level...\n")
    
    # Get data for 2018 and 2023
    data_2018 <- data[data$YEAR == 2018, ]
    data_2023 <- data[data$YEAR == 2023, ]
    
    # Check if both years exist
    if (nrow(data_2018) == 0) {
      cat("  No 2018 data available for", level_name, "level\n")
      return(data.frame())
    }
    if (nrow(data_2023) == 0) {
      cat("  No 2023 data available for", level_name, "level\n") 
      return(data.frame())
    }
    
    # Merge on geographic and subgroup identifiers
    if (level_name == "state") {
      merged <- merge(data_2018, data_2023, by = "SUBGROUP", suffixes = c("_2018", "_2023"))
      id_cols <- "SUBGROUP"
    } else if (level_name == "county") {
      merged <- merge(data_2018, data_2023, by = c("COUNTY_ID", "SUBGROUP"), suffixes = c("_2018", "_2023"))
      id_cols <- c("COUNTY_ID", "COUNTY_NAME_2018", "SUBGROUP")
    } else if (level_name == "district") {
      merged <- merge(data_2018, data_2023, by = c("DISTRICT_ID", "SUBGROUP"), suffixes = c("_2018", "_2023"))
      id_cols <- c("COUNTY_ID_2018", "DISTRICT_ID", "COUNTY_NAME_2018", "DISTRICT_NAME_2018", "SUBGROUP")
    } else if (level_name == "school") {
      merged <- merge(data_2018, data_2023, by = c("SCHOOL_ID", "SUBGROUP"), suffixes = c("_2018", "_2023"))
      id_cols <- c("COUNTY_ID_2018", "DISTRICT_ID_2018", "SCHOOL_ID", "COUNTY_NAME_2018", "DISTRICT_NAME_2018", "SCHOOL_NAME_2018", "SUBGROUP")
    }
    
    if (nrow(merged) == 0) {
      cat("  No matching records between 2018 and 2023 for", level_name, "level\n")
      return(data.frame())
    }
    
    # Calculate changes vectorized
    merged$pre_pandemic_year <- 2018
    merged$current_year <- 2023
    merged$pre_pandemic_rate <- merged$absenteeism_rate_2018
    merged$current_rate <- merged$absenteeism_rate_2023
    merged$point_change <- round(merged$current_rate - merged$pre_pandemic_rate, 2)
    merged$percent_change <- ifelse(merged$pre_pandemic_rate != 0, 
                                   round((merged$current_rate - merged$pre_pandemic_rate) / merged$pre_pandemic_rate * 100, 2), 
                                   NA)
    
    # Select relevant columns
    if (level_name == "state") {
      results <- merged[, c("SUBGROUP", "pre_pandemic_year", "current_year", "pre_pandemic_rate", "current_rate", "point_change", "percent_change")]
      results$unit <- "STATE"
      results <- results[, c("unit", "SUBGROUP", "pre_pandemic_year", "current_year", "pre_pandemic_rate", "current_rate", "point_change", "percent_change")]
    } else {
      select_cols <- c(id_cols, "pre_pandemic_year", "current_year", "pre_pandemic_rate", "current_rate", "point_change", "percent_change")
      results <- merged[, select_cols]
      
      # Clean up column names (remove _2018 suffix from geographic names)
      if (level_name == "county") {
        names(results)[names(results) == "COUNTY_NAME_2018"] <- "COUNTY_NAME"
      } else if (level_name == "district") {
        names(results)[names(results) == "COUNTY_ID_2018"] <- "COUNTY_ID"
        names(results)[names(results) == "COUNTY_NAME_2018"] <- "COUNTY_NAME"
        names(results)[names(results) == "DISTRICT_NAME_2018"] <- "DISTRICT_NAME"
      } else if (level_name == "school") {
        names(results)[names(results) == "COUNTY_ID_2018"] <- "COUNTY_ID"
        names(results)[names(results) == "DISTRICT_ID_2018"] <- "DISTRICT_ID"
        names(results)[names(results) == "COUNTY_NAME_2018"] <- "COUNTY_NAME"
        names(results)[names(results) == "DISTRICT_NAME_2018"] <- "DISTRICT_NAME"
        names(results)[names(results) == "SCHOOL_NAME_2018"] <- "SCHOOL_NAME"
      }
    }
    
    return(results)
  }
  
  # Calculate pre-pandemic comparisons for each level
  results <- list()
  results$state_level <- calculate_pre_pandemic_changes(state_data, "state")
  results$county_level <- calculate_pre_pandemic_changes(county_data, "county")
  results$district_level <- calculate_pre_pandemic_changes(district_data, "district")
  results$school_level <- calculate_pre_pandemic_changes(school_data, "school")
  
  cat("Pre-pandemic comparison calculation complete.\n")
  return(results)
}

