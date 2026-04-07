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
#' # Open in browser
#' browseURL(report_path)
#' }
#'
#' @importFrom utils read.csv
#' @importFrom tools file_ext
#' @export
bueller <- function(data_file = NULL, district_name) {

  if (is.null(data_file)) {
    full_data <- sim
  } else if (is.data.frame(data_file)) {
    full_data <- data_file
  } else if (is.character(data_file)) {
    if (!file.exists(data_file)) stop("Data file not found: ", data_file)
    full_data <- switch(tolower(tools::file_ext(data_file)),
      rds = readRDS(data_file),
      csv = read.csv(data_file, stringsAsFactors = FALSE),
      stop("Unsupported file format. Please use .rds or .csv files")
    )
  } else {
    stop("data_file must be a file path, a data frame, or NULL to use the built-in demo data")
  }

  #
  # Validate inputs
  #if (!file.exists(data_file)) {
  #  stop("Data file not found: ", data_file)
  #}
  #if (is.null(district_name) || district_name == "") {
  #  stop("District name must be provided")
  #}

  # Load data based on file extension
  #cat("Loading data from", data_file, "...\n")
  #full_data <- switch(tolower(tools::file_ext(data_file)),
  #  rds = readRDS(data_file),
  #  csv = read.csv(data_file, stringsAsFactors = FALSE),
  #  stop("Unsupported file format. Please use .rds or .csv files")
  #)

  # Validate required columns
  required_cols <- c(
    "STUDENT_ID", "YEAR", "DISTRICT_ID", "DISTRICT_NAME",
    "SCHOOL_ID", "SCHOOL_NAME", "GRADE", "CHRONIC_ABSENT",
    "MALE", "FEMALE", "HISPANIC", "WHITE", "ASIAN", "BLACK",
    "ELL", "DISADVANTAGE", "DISABILITY"
  )
  missing_cols <- setdiff(required_cols, names(full_data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  # Validate district exists
  if (!district_name %in% full_data$DISTRICT_NAME) {
    available_districts <- paste(sort(unique(full_data$DISTRICT_NAME)), collapse = ", ")
    stop("District '", district_name, "' not found in data. Available districts: ",
         available_districts)
  }

  # Run analysis
  cat("Running analysis for", district_name, "...\n")
  subgroups <- c("MALE", "FEMALE", "HISPANIC", "WHITE", "ASIAN", "BLACK",
                 "ELL", "DISADVANTAGE", "DISABILITY")
  results <- analysis(full_data, district_name, subgroups)

  # Build output filename
  safe_name <- gsub("^_|_$", "", gsub("_{2,}", "_", gsub("[^A-Za-z0-9_-]", "_", district_name)))
  output_file <- paste0(safe_name, "_chronic_absence_report.html")

  # Locate template directory
  template_dir <- system.file("templates", package = "bueller")
  if (!dir.exists(template_dir)) {
    stop("Template directory not found. Please ensure the bueller package is properly installed.")
  }
  if (Sys.which("quarto") == "") {
    stop("Quarto is not installed or not found in PATH. Please install Quarto from https://quarto.org/")
  }

  # Write RDS data files for Quarto to consume
  cat("Rendering report...\n")
  rds_files <- list(
    district_data    = results$district,
    school_data      = results$schools,
    proportions_data = results$proportions,
    grades_data      = results$grades
  )
  for (nm in names(rds_files)) {
    saveRDS(rds_files[[nm]], file.path(template_dir, paste0(nm, ".rds")))
  }

  # Patch index.qmd params and restore on exit
  index_path <- file.path(template_dir, "index.qmd")
  original_content <- readLines(index_path)

  replacements <- c(
    'district_data: null'            = 'district_data: !expr readRDS("district_data.rds")',
    'school_data: null'              = 'school_data: !expr readRDS("school_data.rds")',
    'proportions_data: null'         = 'proportions_data: !expr readRDS("proportions_data.rds")',
    'grades_data: null'              = 'grades_data: !expr readRDS("grades_data.rds")',
    'district_name: "Default District"' = paste0('district_name: "', district_name, '"')
  )
  updated_content <- Reduce(
    function(lines, i) gsub(names(replacements)[i], replacements[i], lines, fixed = TRUE),
    seq_along(replacements),
    init = original_content
  )
  writeLines(updated_content, index_path)

  # Render from template directory (Quarto requires this working directory)
  old_wd <- getwd()
  setwd(template_dir)
  on.exit({
    setwd(old_wd)
    writeLines(original_content, index_path)
    unlink(file.path(template_dir, paste0(names(rds_files), ".rds")))
  }, add = TRUE)

  render_result <- system2(
    "quarto",
    args   = c("render", "index.qmd", "--to", "html"),
    stdout = TRUE,
    stderr = TRUE
  )
  if (!is.null(attr(render_result, "status")) && attr(render_result, "status") != 0) {
    stop("Report rendering failed. Quarto output:\n", paste(render_result, collapse = "\n"))
  }

  # Move rendered file to original working directory
  rendered_file <- file.path(template_dir, "index.html")
  if (!file.exists(rendered_file)) {
    stop("Report generation failed - output file not created")
  }
  file.copy(rendered_file, file.path(old_wd, output_file), overwrite = TRUE)
  unlink(rendered_file)

  cat("Report generated successfully:", output_file, "\n")
  return(output_file)
}
