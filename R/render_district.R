#' Generate district chronic absence report
#'
#' @param data_file Path to student-level chronic absence data file
#' @param district_name Name of district to analyze
#' @importFrom utils read.csv
#' @export
render_district <- function(data_file, district_name) {
  # Check data file exists
  if (!file.exists(data_file)) {
    stop("Data file not found: ", data_file)
  }
  
  # Load data
  if (grepl("\\.csv$", data_file, ignore.case = TRUE)) {
    data <- read.csv(data_file, stringsAsFactors = FALSE)
  } else if (grepl("\\.rds$", data_file, ignore.case = TRUE)) {
    data <- readRDS(data_file)
  } else {
    stop("Unsupported file format. Use .csv or .rds files")
  }
  
  # Validate data structure
  required_cols <- c("STUDENT_ID", "YEAR", "SCHOOL_NAME", 
                     "DISTRICT_NAME", "COUNTY_NAME", "CHRONIC_ABSENT")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", 
         paste(missing_cols, collapse = ", "))
  }
  
  # Check if district exists
  if (!district_name %in% data$DISTRICT_NAME) {
    stop("District '", district_name, "' not found in data")
  }
  
  # Run analysis
  analysis_results <- analyze_district(data, district_name)
  
  # Generate text sections
  executive_text <- text_executive(analysis_results)
  overview_text <- text_overview(analysis_results)
  metrics_text <- text_metrics(analysis_results)
  schools_text <- text_schools(analysis_results)
  demographics_text <- text_demographics(analysis_results)
  grades_text <- text_grades(analysis_results)
  benchmarks_text <- text_benchmarks(analysis_results)
  
  # Generate county context text if available
  if (!is.null(analysis_results$county_context)) {
    county_context_text <- text_county_context(analysis_results)
  } else {
    county_context_text <- "County context data is not available for this analysis."
  }
  
  # Create output filename
  safe_district_name <- gsub("[^A-Za-z0-9]", "_", district_name)
  output_file <- paste0(safe_district_name, "_report.html")
  
  # Create temporary QMD file
  temp_qmd <- file.path(getwd(), "temp_district_report.qmd")
  
  # Generate QMD content with county context section
  qmd_content <- c(
    "---",
    paste0("title: \"", district_name, " Chronic Absence Report\""),
    "format:",
    "  html:",
    "    toc: true",
    "    embed-resources: true",
    "    theme: minty",
    "date: today",
    "execute:",
    "  echo: false",
    "  warning: false",
    "  message: false",
    "---",
    "",
    "# Executive Summary",
    "",
    executive_text,
    "",
    "# District Overview",
    "",
    overview_text,
    "",
    "```{r overview-table}",
    "overview_data <- data.frame(",
    "  Metric = c(",
    "    'Total Students',",
    "    'Chronic Absent Students',",
    "    'Chronic Absence Rate',",
    "    'Number of Schools',",
    "    'Grade Span'",
    "  ),",
    "  Value = c(",
    paste0("    '", format(analysis_results$overview$total_students, big.mark = ","), "',"),
    paste0("    '", format(analysis_results$overview$chronic_students, big.mark = ","), "',"),
    paste0("    '", round(analysis_results$overview$chronic_rate * 100, 1), "%',"),
    paste0("    '", analysis_results$overview$n_schools, "',"),
    paste0("    '", analysis_results$overview$grade_span[1], "-", analysis_results$overview$grade_span[2], "'"),
    "  )",
    ")",
    "knitr::kable(overview_data,",
    "             col.names = c('Metric', 'Value'),",
    "             caption = 'District Overview Summary')",
    "```",
    "",
    "# District Metrics",
    "",
    metrics_text,
    "",
    "```{r metrics-table}",
    "metrics_data <- data.frame(",
    "  Metric = c(",
    "    'District Rate',",
    "    'State Rate',",
    "    'Gap from State',",
    "    'Peak Year',",
    "    'Peak Rate',",
    "    'Target Rate (50% reduction)'",
    "  ),",
    "  Value = c(",
    paste0("    '", round(analysis_results$performance$district_rate * 100, 1), "%',"),
    paste0("    '", round(analysis_results$performance$state_rate * 100, 1), "%',"),
    paste0("    '", ifelse(analysis_results$performance$gap_from_state >= 0, '+', ''), round(analysis_results$performance$gap_from_state * 100, 1), "pp',"),
    paste0("    '", analysis_results$performance$peak_year, "',"),
    paste0("    '", round(analysis_results$performance$peak_rate * 100, 1), "%',"),
    paste0("    '", round(analysis_results$performance$target_rate * 100, 1), "%'"),
    "  )",
    ")",
    "knitr::kable(metrics_data,",
    "             col.names = c('Metric', 'Value'),",
    "             caption = 'District Performance Metrics')",
    "```"
  )
  
  # Add county context section if available
  if (!is.null(analysis_results$county_context)) {
    county_context_section <- c(
      "",
      "# Community Context",
      "",
      county_context_text,
      "",
      "```{r county-context-table}",
      "county_data <- data.frame(",
      "  Metric = c(",
      "    'County',",
      "    'Median Household Income',",
      "    'Poverty Rate',",
      "    'Housing Cost Burden (30%+)',",
      "    'Housing Cost Burden (50%+)',",
      "    'Public Assistance Rate',",
      "    'District Disadvantaged Rate'",
      "  ),",
      "  Value = c(",
      paste0("    '", analysis_results$county_context$county_name, " County',"),
      paste0("    '$", format(analysis_results$county_context$county_median_income, big.mark = ","), "',"),
      paste0("    '", round(analysis_results$county_context$county_poverty_rate * 100, 1), "%',"),
      paste0("    '", round(analysis_results$county_context$county_housing_burden_30 * 100, 1), "%',"),
      paste0("    '", round(analysis_results$county_context$county_housing_burden_50 * 100, 1), "%',"),
      paste0("    '", round(analysis_results$county_context$county_assistance_rate * 100, 1), "%',"),
      paste0("    '", round(analysis_results$county_context$district_disadvantage_rate * 100, 1), "%'"),
      "  )",
      ")",
      "knitr::kable(county_data,",
      "             col.names = c('Metric', 'Value'),",
      "             caption = paste0('", analysis_results$county_context$county_name, " County Economic Context (ACS ", analysis_results$county_context$acs_year, ")'))",
      "```"
    )
    qmd_content <- c(qmd_content, county_context_section)
  }
  
  # Continue with remaining sections
  remaining_sections <- c(
    "",
    "# School Analysis",
    "",
    schools_text,
    "",
    "```{r school-table}",
    "school_data <- data.frame(",
    "  School = c(" ,
    paste0("    \"", analysis_results$schools$school_summary$SCHOOL_NAME, "\"", collapse = ",\n"),
    "  ),",
    "  Students = c(",
    paste0("    ", analysis_results$schools$school_summary$TOTAL_STUDENTS, collapse = ",\n"),
    "  ),",
    "  Chronic_Absent = c(",
    paste0("    ", analysis_results$schools$school_summary$CHRONIC_ABSENT, collapse = ",\n"),
    "  ),",
    "  Rate = c(",
    paste0("    ", round(analysis_results$schools$school_summary$CHRONIC_RATE, 3), collapse = ",\n"),
    "  )",
    ")",
    "school_data$Students <- format(school_data$Students, big.mark = ',')",
    "school_data$Chronic_Absent <- format(school_data$Chronic_Absent, big.mark = ',')",
    "school_data$Rate <- paste0(round(school_data$Rate * 100, 1), '%')",
    "knitr::kable(school_data,",
    "             col.names = c('School', 'Total Students', 'Chronic Absent', 'Rate'),",
    "             caption = 'School-Level Chronic Absence Summary')",
    "```",
    "",
    "# Demographic Analysis",
    "",
    demographics_text,
    "",
    "```{r demographic-table}",
    "demo_data <- data.frame(",
    "  Group = c(",
    paste0("    \"", analysis_results$demographics$demographic_summary$GROUP, "\"", collapse = ",\n"),
    "  ),",
    "  Students = c(",
    paste0("    ", analysis_results$demographics$demographic_summary$TOTAL_STUDENTS, collapse = ",\n"),
    "  ),",
    "  Chronic_Absent = c(",
    paste0("    ", analysis_results$demographics$demographic_summary$CHRONIC_ABSENT, collapse = ",\n"),
    "  ),",
    "  Rate = c(",
    paste0("    ", round(analysis_results$demographics$demographic_summary$CHRONIC_RATE, 3), collapse = ",\n"),
    "  ),",
    "  Gap = c(",
    paste0("    ", round(analysis_results$demographics$demographic_summary$GAP_FROM_DISTRICT, 3), collapse = ",\n"),
    "  )",
    ")",
    "demo_data$Students <- format(demo_data$Students, big.mark = ',')",
    "demo_data$Chronic_Absent <- format(demo_data$Chronic_Absent, big.mark = ',')",
    "demo_data$Rate <- paste0(round(demo_data$Rate * 100, 1), '%')",
    "demo_data$Gap <- paste0(ifelse(demo_data$Gap >= 0, '+', ''), round(demo_data$Gap * 100, 1), 'pp')",
    "knitr::kable(demo_data,",
    "             col.names = c('Group', 'Students', 'Chronic Absent', 'Rate', 'Gap'),",
    "             caption = 'Demographic Group Chronic Absence Summary')",
    "```",
    "",
    "# Grade-Level Patterns",
    "",
    grades_text,
    "",
    "```{r grade-table}",
    "grade_data <- data.frame(",
    "  Grade = c(",
    paste0("    ", analysis_results$grades$grade_summary$GRADE, collapse = ",\n"),
    "  ),",
    "  Students = c(",
    paste0("    ", analysis_results$grades$grade_summary$TOTAL_STUDENTS, collapse = ",\n"),
    "  ),",
    "  Chronic_Absent = c(",
    paste0("    ", analysis_results$grades$grade_summary$CHRONIC_ABSENT, collapse = ",\n"),
    "  ),",
    "  Rate = c(",
    paste0("    ", round(analysis_results$grades$grade_summary$CHRONIC_RATE, 3), collapse = ",\n"),
    "  )",
    ")",
    "grade_data$Students <- format(grade_data$Students, big.mark = ',')",
    "grade_data$Chronic_Absent <- format(grade_data$Chronic_Absent, big.mark = ',')",
    "grade_data$Rate <- paste0(round(grade_data$Rate * 100, 1), '%')",
    "knitr::kable(grade_data,",
    "             col.names = c('Grade', 'Students', 'Chronic Absent', 'Rate'),",
    "             caption = 'Grade-Level Chronic Absence Summary')",
    "```",
    "",
    "# Benchmark Comparisons",
    "",
    benchmarks_text
  )
  
  qmd_content <- c(qmd_content, remaining_sections)
  
  # Write QMD file
  writeLines(qmd_content, temp_qmd)
  
  # Render report
  system(sprintf('quarto render "%s" --output "%s"', temp_qmd, output_file))
  
  # Cleanup
  unlink(temp_qmd)
  
  cat("District report saved to:", output_file, "\n")
  return(output_file)
}
