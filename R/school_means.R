#' Calculate Mean Chronic Absenteeism Rates by School, Year, and Subgroup
#' 
#' Calculates school-level mean chronic absenteeism rates for each year and 
#' student subgroup using the existing demographic columns.
#' 
#' @param data Student-level data frame with demographic columns
#' @return Data frame with mean rates by school, year, and subgroup
#' @keywords internal
school_means <- function(data) {
  
  # Helper function to process a single subgroup
  process_subgroup <- function(subgroup_data, subgroup_name) {
    if (nrow(subgroup_data) == 0) {
      return(NULL)
    }
    
    cat("Processing", subgroup_name, "students...\n")
    
    agg_result <- aggregate(
      CHRONIC_ABSENT ~ COUNTY_ID + DISTRICT_ID + SCHOOL_ID + COUNTY_NAME + DISTRICT_NAME + SCHOOL_NAME + YEAR,
      data = subgroup_data,
      FUN = function(x) c(length(x), sum(x), round(mean(x) * 100, 2))
    )
    
    return(data.frame(
      COUNTY_ID                = agg_result$COUNTY_ID,
      DISTRICT_ID              = agg_result$DISTRICT_ID,
      SCHOOL_ID                = agg_result$SCHOOL_ID,
      COUNTY_NAME              = agg_result$COUNTY_NAME,
      DISTRICT_NAME            = agg_result$DISTRICT_NAME,
      SCHOOL_NAME              = agg_result$SCHOOL_NAME,
      YEAR                     = agg_result$YEAR,
      SUBGROUP                 = subgroup_name,
      total_students           = agg_result$CHRONIC_ABSENT[, 1],
      chronically_absent_count = agg_result$CHRONIC_ABSENT[, 2],
      absenteeism_rate         = agg_result$CHRONIC_ABSENT[, 3],
      stringsAsFactors         = FALSE
    ))
  }
  
  # Pre-allocate results list
  results_list <- list()
  
  # Process ALL students
  results_list[[1]] <- process_subgroup(data, "ALL")
  
  # Process each demographic subgroup
  results_list[[2]] <- process_subgroup(data[data$HISPANIC == 1, ], "HISPANIC")
  results_list[[3]] <- process_subgroup(data[data$WHITE == 1, ], "WHITE")
  results_list[[4]] <- process_subgroup(data[data$ASIAN == 1, ], "ASIAN")
  results_list[[5]] <- process_subgroup(data[data$BLACK == 1, ], "BLACK")
  results_list[[6]] <- process_subgroup(data[data$ELL == 1, ], "ELL")
  results_list[[7]] <- process_subgroup(data[data$DISADVANTAGE == 1, ], "DISADVANTAGE")
  results_list[[8]] <- process_subgroup(data[data$DISABILITY == 1, ], "DISABILITY")
  results_list[[9]] <- process_subgroup(data[data$MALE == 1, ], "MALE")
  results_list[[10]] <- process_subgroup(data[data$FEMALE == 1, ], "FEMALE")
  
  # Remove NULL results (empty subgroups)
  results_list <- results_list[!sapply(results_list, is.null)]
  
  # Combine all results efficiently
  cat("Combining results...\n")
  final_results <- do.call(rbind, results_list)
  rownames(final_results) <- NULL
  
  # Clean up environment
  rm(results_list)
  
  cat("School means calculation complete.\n")
  return(final_results)
}

