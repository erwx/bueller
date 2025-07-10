#' Generate Student Absenteeism Report
#'
#' Creates a comprehensive HTML report analyzing chronic absenteeism patterns
#' across geographic levels and student subgroups. This is the main function users should call.
#'
#' @param data A data frame with student-level absenteeism data
#'
#' @return Path to the generated output directory
#' @export
#'
#' @importFrom stats aggregate complete.cases cor quantile sd var median
#' 
#' @examples
#' \dontrun{
#' # Generate HTML report with Word download option
#' bueller(df)
#' }
bueller <- function(data) {
  
  # Prepare all analysis results
  results <- prep(data)
  
  # Get path to the Quarto template
  template_path <- system.file(
    "templates",
    "absenteeism.qmd",
    package = "bueller"
  )

  if (template_path == "") {
    stop("Template not found.")
  }
  
  # Create temporary RDS file for the template
  temp_results_file <- tempfile(fileext = ".rds")
  saveRDS(results, temp_results_file)
  
  # Render the template in its original location
  quarto::quarto_render(
    input          = template_path,
    execute_params = list(results_file = temp_results_file)
  )
  
  # Clean up temp results file
  unlink(temp_results_file)
  
  # Set up output directory in user's home folder
  home_dir  <- path.expand("~")
  base_name <- "absenteeism-files"
  counter   <- 1
  
  # Find next available versioned folder
  while (dir.exists(file.path(home_dir, paste0(base_name, "-", counter)))) {
    counter <- counter + 1
  }
  
  # Create the output directory
  output_dir <- file.path(
    home_dir,
    paste0(base_name, "-", counter)
  )
  dir.create(output_dir, recursive = TRUE)
  
  template_dir  <- dirname(template_path)
  files_to_copy <- c(
    "absenteeism.html",
    "absenteeism.docx"
  )
  
  # Copy each file if it exists
  for (file_name in files_to_copy) {

    source_file <- file.path(template_dir, file_name)

    if (file.exists(source_file)) {
      dest_file <- file.path(output_dir, file_name)
      file.copy(source_file, dest_file, overwrite = TRUE)
      message("Created: ", dest_file)
    }

  }
  
  # Return path to the output directory
  message("Reports generated in: ", output_dir)
  return(output_dir)
}

