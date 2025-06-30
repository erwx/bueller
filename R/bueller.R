#' Generate Student Engagement Report
#'
#' Creates a comprehensive HTML report analyzing assessment participation and 
#' chronic absenteeism patterns. This is the main function users should call.
#'
#' @param data A data frame with the merged engagement data
#' @param output_dir Directory where the report should be saved (default: current directory)
#' @param min_enrollment Minimum enrollment threshold for district inclusion (default: 30)
#'
#' @return Path to the generated HTML report
#' @export
#'
#' @importFrom stats aggregate complete.cases cor quantile sd var median
#' 
#' @examples
#' \dontrun{
#' # Generate HTML report with Word download option
#' bueller(merged_data)
#' 
#' # Specify output directory and parameters
#' bueller(merged_data, output_dir = "reports/", min_enrollment = 50)
#' }
bueller <- function(
  data,
  output_dir = NULL,
  min_enrollment = 30
) {
  
  # Prepare all analysis results
  results <- bueller_prep(data, min_enrollment)
  
  # Get path to the Quarto template
  template_path <- system.file("templates", "engagement-template.qmd", package = "bueller")
  if (template_path == "") {
    stop("Template not found. Make sure the bueller package is properly installed.")
  }
  
  # If no output_dir specified, use template directory
  if (is.null(output_dir)) {
    output_dir <- dirname(template_path)
  } else {
    # Normalize output directory path
    output_dir <- normalizePath(output_dir, mustWork = FALSE)
    
    # Create output directory if it doesn't exist
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE)
    }
  }
  
  # Create temporary RDS file for the template
  temp_results_file <- tempfile(fileext = ".rds")
  saveRDS(results, temp_results_file)
  
  # Check if we're rendering to the same directory as the template
  template_dir <- dirname(template_path)
  if (normalizePath(output_dir) == normalizePath(template_dir)) {
    # Render directly from template location - no copying needed
    quarto::quarto_render(
      input = template_path,
      execute_params = list(
        results_file = temp_results_file
      )
    )
  } else {
    # Save current working directory
    old_wd <- getwd()
    
    # Change to output directory and do all work from there
    setwd(output_dir)
    
    tryCatch({
      # Copy template to output directory so Quarto renders there
      local_template <- file.path(getwd(), "engagement-template.qmd")
      file.copy(template_path, local_template)
      
      # Render the local copy
      quarto::quarto_render(
        input = local_template,
        execute_params = list(
          results_file = temp_results_file
        )
      )
      
      # Clean up temp template copy
      unlink(local_template)
      
    }, finally = {
      # Always restore working directory
      setwd(old_wd)
    })
  }
  
  # Clean up temp results file
  unlink(temp_results_file)
  
  # Return path to the generated file
  final_file <- file.path(output_dir, "engagement-template.html")
  message("HTML report generated: ", final_file)
  return(final_file)
}