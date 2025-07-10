#' Generate School Chronic Absenteeism Distribution Plots
#' 
#' Creates histograms with density curves for school-level chronic absenteeism
#' rates by year and subgroup. Saves all plots as R objects for later selection.
#' 
#' @param school_data Results from school_means() function
#' @return Named list of plot objects organized by year and subgroup
#' @keywords internal
school_distribution_plots <- function(school_data) {
  
  # Initialize nested list structure
  plot_list <- list()
  
  # Get unique years and subgroups from the data
  years <- sort(unique(school_data$YEAR))
  subgroups <- c("ALL", "HISPANIC", "WHITE", "ASIAN", "BLACK", "ELL", 
                 "DISADVANTAGE", "DISABILITY", "MALE", "FEMALE")
  
  total_plots <- length(years) * length(subgroups)
  current_plot <- 0
  
  cat("Generating", total_plots, "distribution plots...\n")
  
  # Loop through each year
  for (year in years) {
    year_key <- paste0("year_", year)
    plot_list[[year_key]] <- list()
    
    # Loop through each subgroup
    for (subgroup in subgroups) {
      current_plot <- current_plot + 1
      
      # Filter data for this year and subgroup
      subset_data <- school_data[school_data$YEAR == year & 
                                school_data$SUBGROUP == subgroup, ]
      
      # Check if we have data for this combination
      if (nrow(subset_data) > 0 && !all(is.na(subset_data$absenteeism_rate))) {
        
        cat("Plot", current_plot, "of", total_plots, ":", year, "-", subgroup, "\n")
        
        # Get the rates for plotting
        rates <- subset_data$absenteeism_rate
        rates <- rates[!is.na(rates)]  # Remove any NA values
        
        if (length(rates) > 1) {  # Need at least 2 points for density
          
          # Create the plot title
          plot_title <- paste("School Chronic Absenteeism Distribution\n", 
                              year, "-", subgroup, 
                              "(n =", length(rates), "schools)")
          
          # Create histogram with density scale
          hist(rates, 
               freq = FALSE,
               main = plot_title,
               xlab = "Chronic Absenteeism Rate (%)",
               ylab = "Density",
               col = "lightblue",
               border = "white",
               breaks = 20)
          
          # Add density curve
          if (length(rates) > 2) {  # density() needs at least 3 points
            lines(density(rates), 
                  col = "red", 
                  lwd = 2)
          }
          
          # Add summary statistics as text
          mean_rate <- round(mean(rates), 2)
          median_rate <- round(median(rates), 2)
          text(x = max(rates) * 0.7, 
               y = par("usr")[4] * 0.9,
               labels = paste("Mean:", mean_rate, "%\nMedian:", median_rate, "%"),
               adj = 0)
          
          # Save the plot as an object
          plot_list[[year_key]][[subgroup]] <- recordPlot()
          
        } else {
          # Not enough data for meaningful plot
          cat("Skipping", year, "-", subgroup, ": insufficient data\n")
          plot_list[[year_key]][[subgroup]] <- NULL
        }
        
      } else {
        # No data for this combination
        cat("Skipping", year, "-", subgroup, ": no data\n")
        plot_list[[year_key]][[subgroup]] <- NULL
      }
    }
  }
  
  # Clean up environment
  rm(years, subgroups, total_plots, current_plot, year, year_key, 
     subgroup, subset_data, rates, plot_title, mean_rate, median_rate)
  
  cat("Distribution plots generation complete.\n")
  cat("Access plots using: plot_list$year_YYYY$SUBGROUP\n")
  cat("Display plots using: replayPlot(plot_list$year_YYYY$SUBGROUP)\n")
  
  return(plot_list)
}

