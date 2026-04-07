#' Analyze chronic absence patterns for a specific district
#'
#' This function performs comprehensive chronic absence analysis for a single district,
#' including district-level metrics, school-level patterns, hierarchical proportions,
#' and grade-level analysis. It calculates state and peer comparisons efficiently by
#' filtering to the target district after computing necessary benchmarks.
#'
#' @param full_data Data frame containing student-level chronic absence data with
#'   required columns: STUDENT_ID, YEAR, DISTRICT_ID, DISTRICT_NAME, SCHOOL_ID,
#'   SCHOOL_NAME, GRADE, CHRONIC_ABSENT, and demographic indicator columns
#' @param district_name Character string specifying the name of the district to analyze.
#'   Must exactly match a value in the DISTRICT_NAME column of full_data
#' @param subgroups Character vector of demographic subgroup column names to analyze
#'   (e.g., c("MALE", "FEMALE", "HISPANIC", "WHITE", "ELL", "DISADVANTAGE"))
#'
#' @return List containing four data frames:
#'   \describe{
#'     \item{district}{District-level analysis with variance, trends, rankings, and comparisons}
#'     \item{schools}{School-level analysis with rankings and comparisons within district}
#'     \item{proportions}{Hierarchical analysis of school contributions to district totals}
#'     \item{grades}{Grade-level analysis with trends and patterns}
#'   }
#'
#' @examples
#' \dontrun{
#' # Use your own data
#' data <- read.csv("path/to/your/chronic_absence_data.csv")
#' subgroups <- c("MALE", "FEMALE", "HISPANIC", "WHITE", "BLACK", "ELL", "DISADVANTAGE")
#' 
#' # Run analysis for a specific district
#' results <- analysis(sim, "Springfield Elementary School District", subgroups)
#' 
#' # Access different components
#' district_metrics <- results$district
#' school_metrics <- results$schools
#' }
#'
#' @import dplyr
#' @import tidyr
#' @importFrom stats quantile sd weighted.mean
#' @importFrom utils globalVariables 
#'
#' @keywords internal

analysis <- function(full_data, district_name, subgroups) {
  # Helper function to calculate school-level data
  calc_school_data <- function(data, subgroup_name, filter_var = NULL) {
    if (!is.null(filter_var)) {
      data <- data %>% filter(!!sym(filter_var) == 1)
    }
    data %>%
      group_by(YEAR, DISTRICT_ID, DISTRICT_NAME, SCHOOL_ID, SCHOOL_NAME) %>%
      summarise(
        SUBGROUP = subgroup_name,
        ENROLLMENT = n_distinct(STUDENT_ID),
        CHRONIC_COUNT = sum(CHRONIC_ABSENT == 1),
        CHRONIC_RATE = sum(CHRONIC_ABSENT == 1) / n_distinct(STUDENT_ID),
        .groups = "drop"
      )
  }
  
  # Calculate all school data once
  school_data_list <- list(calc_school_data(full_data, "ALL"))
  for (i in seq_along(subgroups)) {
    school_data_list[[i + 1]] <- calc_school_data(full_data, subgroups[i], subgroups[i])
  }
  all_school_data <- bind_rows(school_data_list)
  
  # Calculate all district data once
  all_district_data <- all_school_data %>%
    group_by(YEAR, DISTRICT_ID, DISTRICT_NAME, SUBGROUP) %>%
    summarise(
      ENROLLMENT = sum(ENROLLMENT),
      CHRONIC_COUNT = sum(CHRONIC_COUNT),
      CHRONIC_RATE = sum(CHRONIC_COUNT) / sum(ENROLLMENT),
      .groups = "drop"
    )
  
  # Check if target district exists
  if (!district_name %in% all_district_data$DISTRICT_NAME) {
    stop("District '", district_name, "' not found in data")
  }
  
  # Calculate size bands and determine target district's peer group
  district_sizes <- all_district_data %>%
    filter(SUBGROUP == "ALL") %>%
    group_by(DISTRICT_NAME) %>%
    summarise(avg_enrollment = mean(ENROLLMENT), .groups = "drop") %>%
    mutate(
      size_band = case_when(
        avg_enrollment < quantile(avg_enrollment, 0.33) ~ "SMALL",
        avg_enrollment < quantile(avg_enrollment, 0.67) ~ "MEDIUM",
        TRUE ~ "LARGE"
      )
    )
  
  target_size_band <- district_sizes$size_band[district_sizes$DISTRICT_NAME == district_name]
  peer_districts <- district_sizes$DISTRICT_NAME[district_sizes$size_band == target_size_band]
  
  # Calculate benchmarks
  state_benchmarks <- all_district_data %>%
    group_by(YEAR, SUBGROUP) %>%
    summarise(
      STATE_RATE = weighted.mean(CHRONIC_RATE, ENROLLMENT, na.rm = TRUE),
      .groups = "drop"
    )
  
  peer_benchmarks <- all_district_data %>%
    filter(DISTRICT_NAME %in% peer_districts) %>%
    group_by(YEAR, SUBGROUP) %>%
    summarise(
      PEER_RATE = weighted.mean(CHRONIC_RATE, ENROLLMENT, na.rm = TRUE),
      .groups = "drop"
    )
  
  district_rankings <- all_district_data %>%
    group_by(YEAR, SUBGROUP) %>%
    mutate(
      DISTRICT_RANK = rank(CHRONIC_RATE, ties.method = "min"),
      TOTAL_DISTRICTS = n(),
      DISTRICT_PERCENTILE = round((DISTRICT_RANK / TOTAL_DISTRICTS) * 100, 1)
    ) %>%
    ungroup() %>%
    filter(DISTRICT_NAME == district_name) %>%
    select(YEAR, SUBGROUP, DISTRICT_RANK, TOTAL_DISTRICTS, DISTRICT_PERCENTILE)
  
  # Get target district data
  target_school_data <- all_school_data %>% filter(DISTRICT_NAME == district_name)
  target_district_data <- all_district_data %>% filter(DISTRICT_NAME == district_name)
  target_district_data_raw <- full_data %>% filter(DISTRICT_NAME == district_name)
  
  # DISTRICT ANALYSIS
  district_analysis <- target_school_data %>%
    group_by(YEAR, DISTRICT_ID, DISTRICT_NAME, SUBGROUP) %>%
    summarise(
      ENROLLMENT = sum(ENROLLMENT),
      CHRONIC_COUNT = sum(CHRONIC_COUNT),
      SCHOOL_RATE_SD = sd(CHRONIC_RATE, na.rm = TRUE),
      SCHOOL_RATE_MIN = min(CHRONIC_RATE, na.rm = TRUE),
      SCHOOL_RATE_MAX = max(CHRONIC_RATE, na.rm = TRUE),
      SCHOOL_RATE_Q25 = quantile(CHRONIC_RATE, 0.25, na.rm = TRUE),
      SCHOOL_RATE_Q75 = quantile(CHRONIC_RATE, 0.75, na.rm = TRUE),
      N_SCHOOLS = n(),
      .groups = "drop"
    ) %>%
    mutate(CHRONIC_RATE = CHRONIC_COUNT / ENROLLMENT) %>%
    arrange(DISTRICT_ID, SUBGROUP, YEAR) %>%
    group_by(DISTRICT_ID, SUBGROUP) %>%
    mutate(
      is_consecutive = YEAR == lag(YEAR) + 1,
      across(c(ENROLLMENT, CHRONIC_COUNT, CHRONIC_RATE, SCHOOL_RATE_SD, 
               SCHOOL_RATE_Q25, SCHOOL_RATE_Q75), 
             ~ ifelse(is_consecutive, .x - lag(.x), NA),
             .names = "{.col}_CHANGE")
    ) %>%
    select(-is_consecutive) %>%
    ungroup() %>%
    left_join(state_benchmarks, by = c("YEAR", "SUBGROUP")) %>%
    left_join(peer_benchmarks, by = c("YEAR", "SUBGROUP")) %>%
    left_join(district_rankings, by = c("YEAR", "SUBGROUP")) %>%
    mutate(
      GAP_FROM_STATE = CHRONIC_RATE - STATE_RATE,
      GAP_FROM_PEERS = CHRONIC_RATE - PEER_RATE,
      PEER_SIZE_BAND = target_size_band
    )
  
  # SCHOOL ANALYSIS
  # Calculate variance within schools (how subgroups vary within each school)
  school_variance <- target_school_data %>%
    group_by(YEAR, DISTRICT_ID, SCHOOL_ID, SCHOOL_NAME) %>%
    summarise(
      SUBGROUP_RATE_SD = sd(CHRONIC_RATE, na.rm = TRUE),
      SUBGROUP_RATE_MIN = min(CHRONIC_RATE, na.rm = TRUE),
      SUBGROUP_RATE_MAX = max(CHRONIC_RATE, na.rm = TRUE),
      SUBGROUP_RATE_Q25 = quantile(CHRONIC_RATE, 0.25, na.rm = TRUE),
      SUBGROUP_RATE_Q75 = quantile(CHRONIC_RATE, 0.75, na.rm = TRUE),
      N_SUBGROUPS = n(),
      .groups = "drop"
    )
  
  # Calculate district averages for school comparisons
  district_averages <- target_district_data %>%
    select(YEAR, SUBGROUP, DISTRICT_RATE = CHRONIC_RATE)
  
  school_analysis <- target_school_data %>%
    # Add district averages and variance metrics
    left_join(district_averages, by = c("YEAR", "SUBGROUP")) %>%
    left_join(school_variance, by = c("YEAR", "DISTRICT_ID", "SCHOOL_ID", "SCHOOL_NAME")) %>%
    mutate(GAP_FROM_DISTRICT = CHRONIC_RATE - DISTRICT_RATE) %>%
    
    # Year-over-year changes
    arrange(SCHOOL_ID, SUBGROUP, YEAR) %>%
    group_by(SCHOOL_ID, SUBGROUP) %>%
    mutate(
      is_consecutive = YEAR == lag(YEAR) + 1,
      ENROLLMENT_CHANGE = ifelse(is_consecutive, ENROLLMENT - lag(ENROLLMENT), NA),
      CHRONIC_COUNT_CHANGE = ifelse(is_consecutive, CHRONIC_COUNT - lag(CHRONIC_COUNT), NA),
      RATE_CHANGE = ifelse(is_consecutive, CHRONIC_RATE - lag(CHRONIC_RATE), NA),
      SUBGROUP_RATE_SD_CHANGE = ifelse(is_consecutive, SUBGROUP_RATE_SD - lag(SUBGROUP_RATE_SD), NA),
      SUBGROUP_RATE_Q25_CHANGE = ifelse(is_consecutive, SUBGROUP_RATE_Q25 - lag(SUBGROUP_RATE_Q25), NA),
      SUBGROUP_RATE_Q75_CHANGE = ifelse(is_consecutive, SUBGROUP_RATE_Q75 - lag(SUBGROUP_RATE_Q75), NA)
    ) %>%
    select(-is_consecutive) %>%
    ungroup() %>%
    
    # School rankings within district
    group_by(YEAR, DISTRICT_ID, SUBGROUP) %>%
    mutate(
      SCHOOL_RANK_IN_DISTRICT = rank(CHRONIC_RATE, ties.method = "min"),
      SCHOOLS_IN_DISTRICT = n(),
      SCHOOL_PERCENTILE_IN_DISTRICT = round((SCHOOL_RANK_IN_DISTRICT / SCHOOLS_IN_DISTRICT) * 100, 1)
    ) %>%
    ungroup()
  
  # HIERARCHICAL PROPORTIONS
  hierarchical_proportions <- target_school_data %>%
    group_by(YEAR, SUBGROUP) %>%
    mutate(
      DISTRICT_TOTAL_ENROLLMENT = sum(ENROLLMENT),
      DISTRICT_TOTAL_CHRONIC = sum(CHRONIC_COUNT),
      SCHOOL_ENROLLMENT_PROPORTION = ENROLLMENT / DISTRICT_TOTAL_ENROLLMENT,
      SCHOOL_CHRONIC_PROPORTION = CHRONIC_COUNT / DISTRICT_TOTAL_CHRONIC
    ) %>%
    ungroup() %>%
    select(YEAR, DISTRICT_ID, DISTRICT_NAME, SCHOOL_ID, SCHOOL_NAME, SUBGROUP,
           ENROLLMENT, CHRONIC_COUNT, DISTRICT_TOTAL_ENROLLMENT, DISTRICT_TOTAL_CHRONIC,
           SCHOOL_ENROLLMENT_PROPORTION, SCHOOL_CHRONIC_PROPORTION) %>%
    arrange(YEAR, SUBGROUP, desc(SCHOOL_CHRONIC_PROPORTION))
  
  # GRADE-LEVEL ANALYSIS
  grade_data_list <- list()
  
  # Add ALL students
  grade_data_list[[1]] <- target_district_data_raw %>%
    group_by(YEAR, DISTRICT_ID, DISTRICT_NAME) %>%
    summarise(
      SUBGROUP = "ALL",
      ENROLLMENT = n_distinct(STUDENT_ID),
      CHRONIC_COUNT = sum(CHRONIC_ABSENT == 1),
      CHRONIC_RATE = sum(CHRONIC_ABSENT == 1) / n_distinct(STUDENT_ID),
      .groups = "drop"
    )
  
  # Add each grade
  for (grade in sort(unique(target_district_data_raw$GRADE))) {
    grade_subset <- target_district_data_raw %>% filter(GRADE == grade)
    if (nrow(grade_subset) > 0) {
      grade_data_list[[length(grade_data_list) + 1]] <- grade_subset %>%
        group_by(YEAR, DISTRICT_ID, DISTRICT_NAME) %>%
        summarise(
          SUBGROUP = paste0("GRADE_", grade),
          ENROLLMENT = n_distinct(STUDENT_ID),
          CHRONIC_COUNT = sum(CHRONIC_ABSENT == 1),
          CHRONIC_RATE = sum(CHRONIC_ABSENT == 1) / n_distinct(STUDENT_ID),
          .groups = "drop"
        )
    }
  }
  
  grade_analysis <- bind_rows(grade_data_list) %>%
    arrange(YEAR, SUBGROUP) %>%
    group_by(SUBGROUP) %>%
    mutate(
      is_consecutive = YEAR == lag(YEAR) + 1,
      ENROLLMENT_CHANGE = ifelse(is_consecutive, ENROLLMENT - lag(ENROLLMENT), NA),
      CHRONIC_COUNT_CHANGE = ifelse(is_consecutive, CHRONIC_COUNT - lag(CHRONIC_COUNT), NA),
      RATE_CHANGE = ifelse(is_consecutive, CHRONIC_RATE - lag(CHRONIC_RATE), NA)
    ) %>%
    select(-is_consecutive) %>%
    ungroup()
  
  return(list(
    district = district_analysis,
    schools = school_analysis,
    proportions = hierarchical_proportions,
    grades = grade_analysis
  ))
}
