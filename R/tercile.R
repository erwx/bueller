#' Analyze Chronic Absenteeism by Demographic Terciles
#' 
#' Groups schools by demographic composition terciles and analyzes
#' chronic absenteeism patterns across combinations of demographics.
#' Uses most recent year only.
#' 
#' @param school_data Results from school_means() function
#' @param demographics_data Results from demo() function
#' @return List containing tercile analysis results
#' @keywords internal
tercile <- function(school_data, demographics_data) {
  
  cat("Preparing tercile analysis of demographic interactions...\n")
  
  # Get ALL students data (not just high-risk schools)
  all_schools <- school_data[
    school_data$SUBGROUP == "ALL" & 
    !is.na(school_data$absenteeism_rate),
  ]
  
  if (nrow(all_schools) == 0) {
    cat("No school data available for tercile analysis.\n")
    return(list(
      summary = "No school data available",
      results = NULL
    ))
  }
  
  # Filter to most recent year only
  most_recent_year <- max(all_schools$YEAR, na.rm = TRUE)
  all_schools <- all_schools[all_schools$YEAR == most_recent_year, ]
  
  cat("Using most recent year:", most_recent_year, "\n")
  cat("Found", nrow(all_schools), "school observations...\n")
  
  # Merge with demographics data
  cat("Merging with demographics data...\n")
  
  analysis_data <- merge(
    all_schools[, c("COUNTY_ID", "DISTRICT_ID", "SCHOOL_ID", "YEAR", 
                   "COUNTY_NAME", "DISTRICT_NAME", "SCHOOL_NAME", 
                   "absenteeism_rate")],
    demographics_data,
    by = c("COUNTY_ID", "DISTRICT_ID", "SCHOOL_ID", "YEAR"),
    all.x = TRUE
  )
  
  # Remove rows with missing demographics
  analysis_data <- analysis_data[complete.cases(analysis_data[, c("absenteeism_rate", 
                                                                 "pct_hispanic", "pct_ell", 
                                                                 "pct_disadvantaged", "pct_disability")]), ]
  
  cat("Complete data for analysis\n")
  
  if (nrow(analysis_data) < 100) {
    cat("Insufficient data for tercile analysis (n =", nrow(analysis_data), ").\n")
    return(list(
      summary = paste("Insufficient data: only", nrow(analysis_data), "observations"),
      results = NULL
    ))
  }
  
  # Create terciles for key demographic variables
  cat("Creating demographic terciles...\n")
  
  # Calculate tercile cutpoints
  hispanic_cuts <- quantile(analysis_data$pct_hispanic, c(0, 1/3, 2/3, 1), na.rm = TRUE)
  ell_cuts <- quantile(analysis_data$pct_ell, c(0, 1/3, 2/3, 1), na.rm = TRUE)
  disadvantaged_cuts <- quantile(analysis_data$pct_disadvantaged, c(0, 1/3, 2/3, 1), na.rm = TRUE)
  disability_cuts <- quantile(analysis_data$pct_disability, c(0, 1/3, 2/3, 1), na.rm = TRUE)
  
  # Create tercile variables
  analysis_data$hispanic_tercile <- cut(analysis_data$pct_hispanic, 
                                       breaks = hispanic_cuts, 
                                       labels = c("Low", "Medium", "High"), 
                                       include.lowest = TRUE)
  
  analysis_data$ell_tercile <- cut(analysis_data$pct_ell, 
                                  breaks = ell_cuts, 
                                  labels = c("Low", "Medium", "High"), 
                                  include.lowest = TRUE)
  
  analysis_data$disadvantaged_tercile <- cut(analysis_data$pct_disadvantaged, 
                                            breaks = disadvantaged_cuts, 
                                            labels = c("Low", "Medium", "High"), 
                                            include.lowest = TRUE)
  
  analysis_data$disability_tercile <- cut(analysis_data$pct_disability, 
                                         breaks = disability_cuts, 
                                         labels = c("Low", "Medium", "High"), 
                                         include.lowest = TRUE)
  
  # Calculate interaction analyses
  cat("Analyzing demographic interactions...\n")
  
  # Hispanic x ELL interaction
  hispanic_ell <- aggregate(
    cbind(absenteeism_rate, enrollment) ~ hispanic_tercile + ell_tercile,
    data = analysis_data,
    FUN = mean
  )
  
  hispanic_ell_results <- data.frame(
    hispanic_tercile = hispanic_ell$hispanic_tercile,
    ell_tercile = hispanic_ell$ell_tercile,
    mean_absenteeism = round(hispanic_ell$absenteeism_rate, 2),
    mean_enrollment = round(hispanic_ell$enrollment, 0),
    combination = paste(hispanic_ell$hispanic_tercile, "Hispanic x", hispanic_ell$ell_tercile, "ELL"),
    stringsAsFactors = FALSE
  )
  
  # Hispanic x Disadvantaged interaction
  hispanic_disadvantaged <- aggregate(
    cbind(absenteeism_rate, enrollment) ~ hispanic_tercile + disadvantaged_tercile,
    data = analysis_data,
    FUN = mean
  )
  
  hispanic_disadvantaged_results <- data.frame(
    hispanic_tercile = hispanic_disadvantaged$hispanic_tercile,
    disadvantaged_tercile = hispanic_disadvantaged$disadvantaged_tercile,
    mean_absenteeism = round(hispanic_disadvantaged$absenteeism_rate, 2),
    mean_enrollment = round(hispanic_disadvantaged$enrollment, 0),
    combination = paste(hispanic_disadvantaged$hispanic_tercile, "Hispanic x", hispanic_disadvantaged$disadvantaged_tercile, "Disadvantaged"),
    stringsAsFactors = FALSE
  )
  
  # ELL x Disadvantaged interaction
  ell_disadvantaged <- aggregate(
    cbind(absenteeism_rate, enrollment) ~ ell_tercile + disadvantaged_tercile,
    data = analysis_data,
    FUN = mean
  )
  
  ell_disadvantaged_results <- data.frame(
    ell_tercile = ell_disadvantaged$ell_tercile,
    disadvantaged_tercile = ell_disadvantaged$disadvantaged_tercile,
    mean_absenteeism = round(ell_disadvantaged$absenteeism_rate, 2),
    mean_enrollment = round(ell_disadvantaged$enrollment, 0),
    combination = paste(ell_disadvantaged$ell_tercile, "ELL x", ell_disadvantaged$disadvantaged_tercile, "Disadvantaged"),
    stringsAsFactors = FALSE
  )
  
  # Disability x Disadvantaged interaction
  disability_disadvantaged <- aggregate(
    cbind(absenteeism_rate, enrollment) ~ disability_tercile + disadvantaged_tercile,
    data = analysis_data,
    FUN = mean
  )
  
  disability_disadvantaged_results <- data.frame(
    disability_tercile = disability_disadvantaged$disability_tercile,
    disadvantaged_tercile = disability_disadvantaged$disadvantaged_tercile,
    mean_absenteeism = round(disability_disadvantaged$absenteeism_rate, 2),
    mean_enrollment = round(disability_disadvantaged$enrollment, 0),
    combination = paste(disability_disadvantaged$disability_tercile, "Disability x", disability_disadvantaged$disadvantaged_tercile, "Disadvantaged"),
    stringsAsFactors = FALSE
  )
  
  # Find extremes across ALL specific combinations
  all_combinations <- rbind(
    hispanic_ell_results[, c("mean_absenteeism", "combination")],
    hispanic_disadvantaged_results[, c("mean_absenteeism", "combination")],
    ell_disadvantaged_results[, c("mean_absenteeism", "combination")],
    disability_disadvantaged_results[, c("mean_absenteeism", "combination")]
  )
  
  # Find highest and lowest rates
  highest_idx <- which.max(all_combinations$mean_absenteeism)
  lowest_idx <- which.min(all_combinations$mean_absenteeism)
  
  highest_rate <- all_combinations[highest_idx, ]
  lowest_rate <- all_combinations[lowest_idx, ]
  
  # Calculate summary statistics
  overall_mean <- round(mean(analysis_data$absenteeism_rate), 2)
  overall_range <- range(analysis_data$absenteeism_rate)
  
  # Compile results
  results <- list(
    # Summary information
    summary = paste("Tercile analysis for", most_recent_year),
    analysis_year = most_recent_year,
    overall_mean_absenteeism = overall_mean,
    overall_range = paste0(round(overall_range[1], 2), "% to ", round(overall_range[2], 2), "%"),
    
    # Tercile cutpoints for reference
    tercile_cutpoints = list(
      hispanic = round(hispanic_cuts, 2),
      ell = round(ell_cuts, 2),
      disadvantaged = round(disadvantaged_cuts, 2),
      disability = round(disability_cuts, 2)
    ),
    
    # Interaction results
    interactions = list(
      hispanic_ell = hispanic_ell_results,
      hispanic_disadvantaged = hispanic_disadvantaged_results,
      ell_disadvantaged = ell_disadvantaged_results,
      disability_disadvantaged = disability_disadvantaged_results
    ),
    
    # Key findings
    key_findings = list(
      highest_absenteeism = list(
        rate = highest_rate$mean_absenteeism,
        combination = highest_rate$combination
      ),
      lowest_absenteeism = list(
        rate = lowest_rate$mean_absenteeism,
        combination = lowest_rate$combination
      ),
      rate_difference = round(highest_rate$mean_absenteeism - lowest_rate$mean_absenteeism, 2)
    )
  )
  
  cat("Tercile analysis complete.\n")
  
  return(results)
}

