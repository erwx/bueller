#' Generate chronic absence reports for a district
#'
#' This function orchestrates the complete chronic absence analysis and report
#' generation process. It loads data, runs the analysis for a specified district,
#' and renders HTML reports using parameterized Quarto templates.
#'
#' @param data_file Character string specifying the path to the data file containing
#'   student-level chronic absence data. Can be .rds, .csv, or other supported formats
#' @param district_name Character string specifying the name of the district to analyze.
#'   Must exactly match a value in the DISTRICT_NAME column of the data
#'
#' @return Character string containing the file path of the generated HTML report
#'
#' @examples
#' \dontrun{
#' # Generate report for a district
#' report_path <- bueller("data/chronic_absence_data.rds", "Springfield Elementary")
#' 
#' # View generated file
#' print(report_path)
#' 
#' # Open in browser
#' browseURL(report_path)
#' }
#'
#' @importFrom utils read.csv head
#' @importFrom tools file_ext
#' @export
bueller <- function(data_file, district_name) {
  
  # Validate inputs
  if (!file.exists(data_file)) {
    stop("Data file not found: ", data_file)
  }
  
  if (is.null(district_name) || district_name == "") {
    stop("District name must be provided")
  }
  
  # Load data based on file extension
  cat("Loading data from", data_file, "...\n")
  
  file_ext <- tolower(tools::file_ext(data_file))
  
  if (file_ext == "rds") {
    full_data <- readRDS(data_file)
  } else if (file_ext == "csv") {
    full_data <- read.csv(data_file, stringsAsFactors = FALSE)
  } else {
    stop("Unsupported file format. Please use .rds or .csv files")
  }
  
  # Validate data structure
  required_cols <- c("STUDENT_ID", "YEAR", "DISTRICT_ID", "DISTRICT_NAME", 
                     "SCHOOL_ID", "SCHOOL_NAME", "GRADE", "CHRONIC_ABSENT",
                     "MALE", "FEMALE", "HISPANIC", "WHITE", "ASIAN", "BLACK",
                     "ELL", "DISADVANTAGE", "DISABILITY")
  
  missing_cols <- setdiff(required_cols, names(full_data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  # Check if district exists
  if (!district_name %in% full_data$DISTRICT_NAME) {
    available_districts <- sort(unique(full_data$DISTRICT_NAME))
    stop("District '", district_name, "' not found in data. Available districts:\n", 
         paste(head(available_districts, 10), collapse = "\n"),
         if(length(available_districts) > 10) paste0("\n... and ", length(available_districts) - 10, " more"))
  }
  
  # Run analysis
  cat("Running analysis for", district_name, "...\n")
  
  subgroups <- c("MALE", "FEMALE", "HISPANIC", "WHITE", "ASIAN", "BLACK", 
                 "ELL", "DISADVANTAGE", "DISABILITY")
  
  results <- analysis(full_data, district_name, subgroups)
  
  # Create safe filename
  safe_district_name <- gsub("[^A-Za-z0-9_-]", "_", district_name)
  safe_district_name <- gsub("_{2,}", "_", safe_district_name)
  safe_district_name <- gsub("^_|_$", "", safe_district_name)
  
  output_file <- paste0(safe_district_name, "_chronic_absence_report.html")
  
  # Get template directory
  template_dir <- system.file("templates", package = "chronic")
  
  if (!dir.exists(template_dir)) {
    stop("Template directory not found. Please ensure the chronic package is properly installed.")
  }
  
  # Check if Quarto is available
  if (Sys.which("quarto") == "") {
    stop("Quarto is not installed or not found in PATH. Please install Quarto from https://quarto.org/")
  }
  
  # Render report
  cat("Rendering report...\n")
  
  # Save results as RDS files in template directory
  saveRDS(results$district, file.path(template_dir, "district_data.rds"))
  saveRDS(results$schools, file.path(template_dir, "school_data.rds")) 
  saveRDS(results$proportions, file.path(template_dir, "proportions_data.rds"))
  saveRDS(results$grades, file.path(template_dir, "grades_data.rds"))
  
  # Update index.qmd with parameters
  index_path <- file.path(template_dir, "index.qmd")
  original_content <- readLines(index_path)
  
  # Replace parameter defaults with RDS loading
  updated_content <- gsub(
    'district_data: null',
    'district_data: !expr readRDS("district_data.rds")',
    original_content
  )
  updated_content <- gsub(
    'school_data: null',
    'school_data: !expr readRDS("school_data.rds")',
    updated_content
  )
  updated_content <- gsub(
    'proportions_data: null', 
    'proportions_data: !expr readRDS("proportions_data.rds")',
    updated_content
  )
  updated_content <- gsub(
    'grades_data: null',
    'grades_data: !expr readRDS("grades_data.rds")',
    updated_content
  )
  updated_content <- gsub(
    'district_name: "Default District"',
    paste0('district_name: "', district_name, '"'),
    updated_content
  )
  
  writeLines(updated_content, index_path)
  
  # Change to template directory and render
  old_wd <- getwd()
  setwd(template_dir)
  on.exit({
    setwd(old_wd)
    writeLines(original_content, index_path)
    unlink(file.path(template_dir, "district_data.rds"))
    unlink(file.path(template_dir, "school_data.rds"))
    unlink(file.path(template_dir, "proportions_data.rds"))
    unlink(file.path(template_dir, "grades_data.rds"))
  }, add = TRUE)
  
  render_result <- system2("quarto", 
                          args = c("render", "index.qmd", "--to", "html"),
                          stdout = TRUE, stderr = TRUE)
  
  if (!is.null(attr(render_result, "status")) && attr(render_result, "status") != 0) {
    stop("Report rendering failed. Quarto output:\n", paste(render_result, collapse = "\n"))
  }
  
  # Copy HTML output to working directory
  rendered_file <- file.path(template_dir, "index.html")
  if (!file.exists(rendered_file)) {
    stop("Report generation failed - output file not created")
  }
  
  final_path <- file.path(old_wd, output_file)
  file.copy(rendered_file, final_path, overwrite = TRUE)
  unlink(rendered_file)  # Clean up template directory
  
  cat("Report generated successfully:", output_file, "\n")
  
  return(output_file)
}
