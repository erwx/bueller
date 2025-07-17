#' Generate district chronic absence report
#'
#' @param data_file Path to student-level chronic absence data file
#' @param district_name Name of district to analyze
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
  
  # Create output filename
  safe_district_name <- gsub("[^A-Za-z0-9]", "_", district_name)
  output_file <- paste0(safe_district_name, "_report.html")
  
  # Create temporary QMD file
  temp_qmd <- file.path(getwd(), "temp_district_report.qmd")
  
  # Generate QMD content (exact same as original render.txt)
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
    "# District Metrics",
    "",
    metrics_text,
    "",
    "# School Analysis",
    "",
    schools_text,
    "",
    "# Demographic Analysis",
    "",
    demographics_text,
    "",
    "# Grade-Level Patterns",
    "",
    grades_text,
    "",
    "# Benchmark Comparisons",
    "",
    benchmarks_text
  )
  
  # Write QMD file
  writeLines(qmd_content, temp_qmd)
  
  # Render report (exact same as original render.txt)
  system(sprintf('quarto render "%s" --output "%s"', temp_qmd, output_file))
  
  # Cleanup
  unlink(temp_qmd)
  
  cat("District report saved to:", output_file, "\n")
  return(output_file)
}
