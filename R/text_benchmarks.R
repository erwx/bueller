#' Generate benchmark analysis text
#'
#' @param analysis_results Results from analyze_district function
#' @return Character string with benchmark analysis text
#' @keywords internal
text_benchmarks <- function(analysis_results) {
  district_name    <- analysis_results[["district_name"]]
  current_year     <- analysis_results[["current_year"]]
  state_rate       <- analysis_results[["benchmarks"]][["state_rate"]]
  district_rate    <- analysis_results[["benchmarks"]][["district_rate"]]
  gap_from_state   <- analysis_results[["benchmarks"]][["gap_from_state"]]
  size_band        <- analysis_results[["benchmarks"]][["size_band"]]
  econ_band        <- analysis_results[["benchmarks"]][["econ_band"]]
  peer_group_size  <- analysis_results[["benchmarks"]][["peer_group_size"]]
  peer_rank        <- analysis_results[["benchmarks"]][["peer_rank"]]
  peer_median      <- analysis_results[["benchmarks"]][["peer_median"]]
  
  state_rate_pct    <- paste0(round(state_rate * 100, 1), "%")
  district_rate_pct <- paste0(round(district_rate * 100, 1), "%")
  
  gap_abs <- abs(gap_from_state)
  if (gap_abs <= 0.005) {
    state_text <- paste0(
      "The district's chronic absence rate of ",
      district_rate_pct,
      " is similar to the state average of ",
      state_rate_pct,
      "."
    )
  } else if (gap_from_state > 0) {
    gap_pct <- paste0(round(gap_from_state * 100, 1), " percentage points")
    state_text <- paste0(
      "The district's chronic absence rate of ",
      district_rate_pct,
      " is ",
      gap_pct,
      " higher than the state average of ",
      state_rate_pct,
      "."
    )
  } else {
    gap_pct <- paste0(round(gap_abs * 100, 1), " percentage points")
    state_text <- paste0(
      "The district's chronic absence rate of ",
      district_rate_pct,
      " is ",
      gap_pct,
      " lower than the state average of ",
      state_rate_pct,
      "."
    )
  }
  
  if (size_band == "SMALL") {
    size_text <- "small"
  } else if (size_band == "MEDIUM") {
    size_text <- "medium-sized"
  } else {
    size_text <- "large"
  }
  
  if (econ_band == "LOW") {
    econ_text <- "low economic disadvantage"
  } else if (econ_band == "MEDIUM") {
    econ_text <- "medium economic disadvantage"
  } else {
    econ_text <- "high economic disadvantage"
  }
  
  peer_definition <- paste0(size_text, " districts with ", econ_text)
  
  # Peer comparison text
  if (peer_group_size < 3) {
    if (peer_group_size == 0) {
      peer_text <- paste0(
        "No comparable peer districts exist in the ",
        peer_definition,
        " category for comparison."
      )
    } else if (peer_group_size == 1) {
      peer_text <- paste0(
        "Only 1 district exists in the ",
        peer_definition,
        " category, insufficient for meaningful peer comparison."
      )
    } else {
      peer_text <- paste0(
        "Only ",
        peer_group_size,
        " districts exist in the ",
        peer_definition,
        " category, insufficient for meaningful peer comparison."
      )
    }
  } else {
    if (is.na(peer_rank)) {
      peer_text <- paste0(
        "Among ",
        peer_group_size,
        " ",
        peer_definition,
        " districts, ranking information is not available."
      )
    } else {
      peer_median_pct <- paste0(round(peer_median * 100, 1), "%")
      
      if (peer_rank == 1) {
        rank_text <- "ranks 1st (lowest chronic absence rate)"
      } else if (peer_rank == peer_group_size) {
        rank_text <- paste0(
          "ranks ",
          peer_rank,
          "th (highest chronic absence rate)"
        )
      } else {
        rank_text <- paste0("ranks ", peer_rank, "th")
      }
      
      if (is.na(peer_median)) {
        median_text <- ""
      } else {
        median_gap <- district_rate - peer_median
        if (abs(median_gap) <= 0.005) {
          median_text <- paste0(
            " The district's rate is similar to the peer group median of ",
            peer_median_pct,
            "."
          )
        } else if (median_gap > 0) {
          median_gap_pct <- paste0(round(median_gap * 100, 1), " percentage points")
          median_text <- paste0(
            " The district's rate is ",
            median_gap_pct,
            " higher than the peer group median of ",
            peer_median_pct,
            "."
          )
        } else {
          median_gap_pct <- paste0(round(abs(median_gap) * 100, 1), " percentage points")
          median_text <- paste0(
            " The district's rate is ",
            median_gap_pct,
            " lower than the peer group median of ",
            peer_median_pct,
            "."
          )
        }
      }
      
      peer_text <- paste0(
        "Among ",
        peer_group_size,
        " ",
        peer_definition,
        " districts, ",
        district_name,
        " ",
        rank_text,
        ".",
        median_text
      )
    }
  }
  
  classification_text <- paste0(
    "The district is classified as a ",
    size_text,
    " district with ",
    econ_text,
    " based on enrollment size and student demographics."
  )
  
  benchmarks_text <- paste0(
    state_text,
    " ",
    classification_text,
    " ",
    peer_text
  )
  
  return(benchmarks_text)
}
