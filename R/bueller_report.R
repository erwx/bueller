#' Generate Student Engagement Report
#'
#' Creates a report analyzing assessment participation and chronic absenteeism 
#' patterns across districts in multiple formats.
#'
#' @param data A data frame with the merged engagement data
#' @param output_dir Directory where the report should be saved (default: current directory)
#' @param type Report format: "text" (default), "html", or "revealjs"
#' @param min_enrollment Minimum enrollment threshold for district inclusion (default: 30)
#'
#' @return Path to the generated report (for html/revealjs) or invisible for text
#' @export
#'
#' @importFrom stats median
#' 
#' @examples
#' \dontrun{
#' # Beautiful console output (default)
#' bueller_report(merged_data)
#' 
#' # HTML report with Word download option
#' bueller_report(merged_data, type = "html")
#' 
#' # RevealJS presentation
#' bueller_report(merged_data, type = "revealjs")
#' }
bueller_report <- function(
  data, 
  output_dir = ".", 
  type = "text",
  min_enrollment = 30
) {
  
  # Validate type
  valid_types <- c("text", "html", "revealjs")
  if (!type %in% valid_types) {
    stop("type must be one of: ", paste(valid_types, collapse = ", "))
  }
  
  if (type == "text") {
    # Generate beautiful console-based text report
    pretty_print(data, min_enrollment)
    return(invisible(NULL))
    
  } else if (type == "html") {
    # Get path to the Quarto template
    template_path <- system.file("templates", "engagement-report.qmd", package = "bueller")
    if (template_path == "") {
      stop("Template not found. Make sure the bueller package is properly installed.")
    }
    
    # Create temporary RDS file for the template
    temp_data_file <- tempfile(fileext = ".rds")
    saveRDS(data, temp_data_file)
    
    # Render the report
    output_file <- file.path(output_dir, "engagement-report.html")
    
    quarto::quarto_render(
      input = template_path,
      output_file = output_file,
      execute_params = list(
        data_file = temp_data_file,
        min_enrollment = min_enrollment
      )
    )
    
    # Clean up temp file
    unlink(temp_data_file)
    
    message("HTML report generated: ", output_file)
    return(output_file)
    
  } else if (type == "revealjs") {
    # Generate RevealJS presentation (placeholder for now)
    message("RevealJS presentation format coming soon!")
    return(invisible(NULL))
  }
}