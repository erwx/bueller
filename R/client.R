#' Interactive Chat Interface for Student Engagement Data
#'
#' Start an interactive chat session to ask questions about assessment
#' participation and chronic absenteeism patterns in California
#' districts.
#'
#' @return NULL (runs interactive session)
#' @export
#'
#' @param data A data frame with the merged engagement data
client <- function(data) {
  
  # Check if ellmer is available
  if (!requireNamespace("ellmer", quietly = TRUE)) {
    stop(paste("ellmer package is required.",
               "Install with: install.packages('ellmer')"))
  }
  
  # Check if jsonlite is available
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop(paste("jsonlite package is required.",
               "Install with: install.packages('jsonlite')"))
  }
  
  cat("California Student Engagement Chat Agent\n")
  cat(paste("Analyzing assessment participation and chronic",
            "absenteeism...\n\n"))
  
  # Run analysis on package data
  cat("Loading and analyzing data (this may take a moment)...\n")
  results <- prep(data)
  cat("Analysis complete!\n\n")
  
  # Create curated JSON
  cat("Preparing chat context...\n")
  results_json <- json(results)
  cat("JSON size:", nchar(results_json), "characters\n")
  cat("Estimated tokens:", round(nchar(results_json) / 4), "\n\n")
  
  # Create system prompt with curated data
  system_prompt <- paste0(
    paste("You are a superintelligent assistant specializing in",
          "California K-12 student engagement data."),
    paste("You analyze assessment participation and chronic",
          "absenteeism patterns with both technical precision and",
          "accessible communication.\n\n"),
    "ANALYSIS DATA (JSON):\n",
    results_json,
    paste("\n\nYou have access to the key analysis results above.",
          "Use this data to:\n"),
    "- Answer specific questions about districts, trends, and patterns\n",
    "- Reference exact statistics and district names when relevant\n",
    "- Identify insights about student group disparities\n",
    "- Explain participation compliance and absenteeism thresholds\n",
    "- Compare districts and analyze concentration patterns\n\n",
    paste("Be conversational yet precise. Always reference specific",
          "data points when available. Use the benchmarks provided",
          "to interpret whether rates are concerning or acceptable.",
          "If asked about data not in your context, explain what",
          "information you do have access to.")
  )
  
  chat <- ellmer::chat_anthropic(system_prompt = system_prompt)
  
  cat("Ask me about participation trends\n")
  cat("district comparisons, or student group disparities.\n")
  cat("Type 'quit' to exit.\n\n")
  
  # Chat loop with conversation memory
  repeat {
    user_input <- readline("\U0001F4E3 Prompt here... \n::: ")
    cat("\n")
    
    if (tolower(trimws(user_input)) %in% c("quit", "exit", "q")) {
      cat("Goodbye!\n")
      break
    }
    
    if (trimws(user_input) == "") next
    
    # Get response (chat object maintains conversation history automatically)
    tryCatch({
      cat("\U0001F916 The robot says... \n")
      response <- chat$chat(user_input)
      cat(response, "\n\n")  # Print the response
    }, error = function(e) {
      cat("\nSorry, I encountered an error:", e$message, "\n")
      if (grepl("too long|token", e$message, ignore.case = TRUE)) {
        cat("This might be due to context size. Try a shorter question.\n\n")
      } else {
        cat("Please check your API key setup or try again.\n\n")
      }
    })
  }
}
