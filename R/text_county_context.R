#' Generate county context analysis text
#'
#' @param analysis_results Results from analyze_district function
#' @return Character string with county context analysis text
#' @keywords internal
text_county_context <- function(analysis_results) {
  # Check if county context is available
  if (is.null(analysis_results[["county_context"]])) {
    return("County context data is not available for this analysis.")
  }
  
  district_name    <- analysis_results[["district_name"]]
  current_year     <- analysis_results[["current_year"]]
  county_context   <- analysis_results[["county_context"]]
  
  county_name      <- county_context[["county_name"]]
  acs_year         <- county_context[["acs_year"]]
  median_income    <- county_context[["county_median_income"]]
  poverty_rate     <- county_context[["county_poverty_rate"]]
  housing_burden_30 <- county_context[["county_housing_burden_30"]]
  housing_burden_50 <- county_context[["county_housing_burden_50"]]
  assistance_rate  <- county_context[["county_assistance_rate"]]
  
  district_chronic <- county_context[["district_chronic_rate"]]
  district_disadvantage <- county_context[["district_disadvantage_rate"]]
  
  high_poverty     <- county_context[["high_poverty_county"]]
  high_housing     <- county_context[["high_housing_burden"]]
  low_income       <- county_context[["low_income_county"]]
  
  # Format values
  median_income_fmt <- paste0("$", format(median_income, big.mark = ",", scientific = FALSE))
  poverty_rate_pct <- paste0(round(poverty_rate * 100, 1), "%")
  housing_30_pct <- paste0(round(housing_burden_30 * 100, 1), "%")
  housing_50_pct <- paste0(round(housing_burden_50 * 100, 1), "%")
  assistance_pct <- paste0(round(assistance_rate * 100, 1), "%")
  district_chronic_pct <- paste0(round(district_chronic * 100, 1), "%")
  district_disadvantage_pct <- paste0(round(district_disadvantage * 100, 1), "%")
  
  # County economic profile
  economic_text <- paste0(
    county_name,
    " County has a median household income of ",
    median_income_fmt,
    " and a poverty rate of ",
    poverty_rate_pct,
    " based on ",
    acs_year,
    " American Community Survey data. ",
    housing_30_pct,
    " of renter households pay 30% or more of their income on housing costs, with ",
    housing_50_pct,
    " experiencing severe cost burden (50% or more of income). ",
    assistance_pct,
    " of households receive public assistance income."
  )
  
  # Context classification
  context_parts <- c()
  
  if (!is.na(high_poverty) && high_poverty) {
    context_parts <- c(context_parts, "high poverty")
  } else if (!is.na(high_poverty) && !high_poverty) {
    context_parts <- c(context_parts, "relatively low poverty")
  }
  
  if (!is.na(high_housing) && high_housing) {
    context_parts <- c(context_parts, "high housing cost burden")
  } else if (!is.na(high_housing) && !high_housing) {
    context_parts <- c(context_parts, "moderate housing costs")
  }
  
  if (!is.na(low_income) && low_income) {
    context_parts <- c(context_parts, "low median income")
  } else if (!is.na(low_income) && !low_income) {
    context_parts <- c(context_parts, "above-average median income")
  }
  
  if (length(context_parts) > 0) {
    if (length(context_parts) == 1) {
      context_summary <- paste0("This represents a county with ", context_parts[1], ".")
    } else if (length(context_parts) == 2) {
      context_summary <- paste0("This represents a county with ", context_parts[1], " and ", context_parts[2], ".")
    } else {
      context_summary <- paste0("This represents a county with ", 
                                paste(context_parts[-length(context_parts)], collapse = ", "), 
                                ", and ", context_parts[length(context_parts)], ".")
    }
  } else {
    context_summary <- ""
  }
  
  # District vs county comparison
  disadvantage_gap <- district_disadvantage - poverty_rate
  
  if (abs(disadvantage_gap) <= 0.05) {
    disadvantage_text <- paste0(
      "The district's economically disadvantaged student rate of ",
      district_disadvantage_pct,
      " closely aligns with the county's poverty rate."
    )
  } else if (disadvantage_gap > 0) {
    gap_pct <- paste0(round(disadvantage_gap * 100, 1), " percentage points")
    disadvantage_text <- paste0(
      "The district's economically disadvantaged student rate of ",
      district_disadvantage_pct,
      " is ",
      gap_pct,
      " higher than the county's overall poverty rate, suggesting the district serves a more economically vulnerable population than the county average."
    )
  } else {
    gap_pct <- paste0(round(abs(disadvantage_gap) * 100, 1), " percentage points")
    disadvantage_text <- paste0(
      "The district's economically disadvantaged student rate of ",
      district_disadvantage_pct,
      " is ",
      gap_pct,
      " lower than the county's overall poverty rate, suggesting the district serves a relatively more affluent population within the county."
    )
  }
  
  # Chronic absence in context
  if (!is.na(high_poverty) && high_poverty) {
    if (district_chronic > 0.25) {
      chronic_context <- "The district's high chronic absence rate may reflect broader economic challenges in the county."
    } else {
      chronic_context <- "Despite county-level economic challenges, the district maintains a relatively moderate chronic absence rate."
    }
  } else if (!is.na(high_housing) && high_housing) {
    if (district_chronic > 0.25) {
      chronic_context <- "The district's chronic absence challenges may be influenced by housing cost pressures affecting families in the county."
    } else {
      chronic_context <- "Despite high housing costs in the county, the district maintains a relatively moderate chronic absence rate."
    }
  } else {
    if (district_chronic > 0.25) {
      chronic_context <- "The district's chronic absence rate appears elevated relative to the county's generally favorable economic conditions."
    } else {
      chronic_context <- "The district's chronic absence rate aligns with the county's generally favorable economic conditions."
    }
  }
  
  # Combine county context text
  county_context_text <- paste0(
    economic_text,
    " ",
    if (nchar(context_summary) > 0) paste0(context_summary, " ") else "",
    disadvantage_text,
    " ",
    chronic_context
  )
  
  return(county_context_text)
}
