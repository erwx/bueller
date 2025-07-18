#' Generate comprehensive benchmark analysis text (100+ words longer)
#'
#' @param analysis_results Results from analyze_district function
#' @return Character string with benchmark analysis text
#' @keywords internal
text_benchmarks <- function(analysis_results) {
  district_name     <- analysis_results[["district_name"]]
  current_year      <- analysis_results[["current_year"]]
  state_rate        <- analysis_results[["benchmarks"]][["state_rate"]]
  district_rate     <- analysis_results[["benchmarks"]][["district_rate"]]
  gap_from_state    <- analysis_results[["benchmarks"]][["gap_from_state"]]
  size_band         <- analysis_results[["benchmarks"]][["size_band"]]
  econ_band         <- analysis_results[["benchmarks"]][["econ_band"]]
  peer_group_size   <- analysis_results[["benchmarks"]][["peer_group_size"]]
  peer_rank         <- analysis_results[["benchmarks"]][["peer_rank"]]
  peer_median       <- analysis_results[["benchmarks"]][["peer_median"]]
  
  state_rate_pct    <- paste0(round(state_rate * 100, 1), "%")
  district_rate_pct <- paste0(round(district_rate * 100, 1), "%")
  
  gap_abs <- abs(gap_from_state)
  
  # State comparison with context
  if (gap_abs <= 0.005) {
    state_comparison <- paste0(
      "The district's chronic absence rate of ", 
      district_rate_pct, 
      " is virtually identical to the state average of ", 
      state_rate_pct, 
      ", suggesting the district faces typical statewide ", 
      "attendance challenges."
    )
  } else if (gap_from_state > 0) {
    gap_pct <- paste0(round(gap_from_state * 100, 1), " percentage points")
    
    if (gap_from_state > 0.05) {
      magnitude <- "substantially higher"
      context   <- paste0(
        "indicating significant attendance challenges ", 
        "requiring urgent attention"
      )
    } else if (gap_from_state > 0.02) {
      magnitude <- "moderately higher"
      context   <- paste0(
        "suggesting notable attendance issues that warrant ", 
        "focused intervention"
      )
    } else {
      magnitude <- "slightly higher"
      context   <- paste0(
        "reflecting minor attendance challenges above typical ", 
        "statewide patterns"
      )
    }
    
    state_comparison <- paste0(
      "The district's chronic absence rate of ", 
      district_rate_pct, 
      " is ", 
      magnitude, 
      " than the state average of ", 
      state_rate_pct, 
      " by ", 
      gap_pct, 
      ", ", 
      context, 
      "."
    )
  } else {
    gap_pct <- paste0(round(gap_abs * 100, 1), " percentage points")
    
    if (gap_abs > 0.05) {
      magnitude <- "substantially lower"
      context   <- paste0(
        "demonstrating exceptional attendance outcomes that ", 
        "significantly exceed state expectations"
      )
    } else if (gap_abs > 0.02) {
      magnitude <- "moderately lower"
      context   <- paste0(
        "showing strong attendance performance above typical ", 
        "statewide results"
      )
    } else {
      magnitude <- "slightly lower"
      context   <- paste0(
        "reflecting marginally better attendance than ", 
        "statewide patterns"
      )
    }
    
    state_comparison <- paste0(
      "The district's chronic absence rate of ", 
      district_rate_pct, 
      " is ", 
      magnitude, 
      " than the state average of ", 
      state_rate_pct, 
      " by ", 
      gap_pct, 
      ", ", 
      context, 
      "."
    )
  }
  
  # Size and economic classification context
  if (size_band == "SMALL") {
    size_text    <- "small"
    size_context <- paste0(
      "typically allowing for more personalized attention but ", 
      "potentially having fewer resources"
    )
  } else if (size_band == "MEDIUM") {
    size_text    <- "medium-sized"
    size_context <- paste0(
      "balancing operational efficiency with manageable scale ", 
      "for targeted interventions"
    )
  } else {
    size_text    <- "large"
    size_context <- paste0(
      "providing extensive resources but facing complex ", 
      "coordination challenges"
    )
  }
  
  if (econ_band == "LOW") {
    econ_text    <- "low economic disadvantage"
    econ_context <- paste0(
      "serving students from relatively affluent families with ", 
      "fewer socioeconomic barriers to attendance"
    )
  } else if (econ_band == "MEDIUM") {
    econ_text    <- "medium economic disadvantage"
    econ_context <- paste0(
      "serving a mixed population with moderate socioeconomic ", 
      "challenges affecting attendance"
    )
  } else {
    econ_text    <- "high economic disadvantage"
    econ_context <- paste0(
      "serving students facing significant economic hardships ", 
      "that may impact school attendance"
    )
  }
  
  peer_definition <- paste0(size_text, " districts with ", econ_text)
  
  classification_text <- paste0(
    "The district is classified as a ", 
    size_text, 
    " district with ", 
    econ_text, 
    " based on enrollment size and student demographics, ", 
    size_context, 
    " while ", 
    econ_context, 
    "."
  )
  
  # Peer comparison with detailed context
  if (peer_group_size < 3) {
    if (peer_group_size == 0) {
      peer_text <- paste0(
        "No comparable peer districts exist in the ", 
        peer_definition, 
        " category, making meaningful comparison impossible. ", 
        "This unique classification suggests the district ", 
        "operates in distinctive circumstances."
      )
    } else if (peer_group_size == 1) {
      peer_text <- paste0(
        "Only 1 district exists in the ", 
        peer_definition, 
        " category, preventing meaningful peer comparison. ", 
        "This limited peer group indicates the district's ", 
        "unique operational context."
      )
    } else {
      peer_text <- paste0(
        "Only ", 
        peer_group_size, 
        " districts exist in the ", 
        peer_definition, 
        " category, providing insufficient data for reliable ", 
        "peer comparison due to the limited sample size."
      )
    }
  } else {
    if (is.na(peer_rank)) {
      peer_text <- paste0(
        "Among ", 
        peer_group_size, 
        " ", 
        peer_definition, 
        " districts, ranking information is not available ", 
        "due to data limitations."
      )
    } else {
      peer_median_pct <- paste0(round(peer_median * 100, 1), "%")
      
      # Performance context based on ranking
      if (peer_rank <= peer_group_size * 0.25) {
        performance_tier    <- "top quartile"
        performance_context <- paste0(
          "demonstrating exceptional attendance outcomes ", 
          "relative to similar districts"
        )
      } else if (peer_rank <= peer_group_size * 0.5) {
        performance_tier    <- "second quartile"
        performance_context <- paste0(
          "showing above-average attendance performance ", 
          "compared to peer districts"
        )
      } else if (peer_rank <= peer_group_size * 0.75) {
        performance_tier    <- "third quartile"
        performance_context <- paste0(
          "performing below the peer group median with ", 
          "room for improvement"
        )
      } else {
        performance_tier    <- "bottom quartile"
        performance_context <- paste0(
          "facing significant attendance challenges relative ", 
          "to comparable districts"
        )
      }
      
      if (peer_rank == 1) {
        rank_text <- "ranks 1st (lowest chronic absence rate)"
      } else if (peer_rank == peer_group_size) {
        rank_text <- paste0(
          "ranks ", 
          peer_rank, 
          "th (highest chronic absence rate)"
        )
      } else {
        rank_text <- paste0("ranks ", peer_rank, "th of ", peer_group_size)
      }
      
      # Median comparison
      if (is.na(peer_median)) {
        median_text <- ""
      } else {
        median_gap <- district_rate - peer_median
        
        if (abs(median_gap) <= 0.005) {
          median_text <- paste0(
            " The district's rate aligns closely with the ", 
            "peer group median of ", 
            peer_median_pct, 
            ", indicating typical performance for similar districts."
          )
        } else if (median_gap > 0) {
          median_gap_pct <- paste0(
            round(median_gap * 100, 1), 
            " percentage points"
          )
          median_text <- paste0(
            " The district's rate exceeds the peer group median of ", 
            peer_median_pct, 
            " by ", 
            median_gap_pct, 
            ", suggesting underperformance relative to districts ", 
            "with similar characteristics."
          )
        } else {
          median_gap_pct <- paste0(
            round(abs(median_gap) * 100, 1), 
            " percentage points"
          )
          median_text <- paste0(
            " The district's rate is ", 
            median_gap_pct, 
            " lower than the peer group median of ", 
            peer_median_pct, 
            ", indicating stronger attendance outcomes than ", 
            "typical for similar districts."
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
        ", placing it in the ", 
        performance_tier, 
        " and ", 
        performance_context, 
        ".", 
        median_text
      )
    }
  }
  
  # National context and implications
  national_context <- paste0(
    " These comparisons provide important context for ", 
    "understanding the district's attendance challenges. ", 
    "Districts with similar demographic and size characteristics ", 
    "face comparable operational realities, making peer ", 
    "performance a more meaningful benchmark than statewide ", 
    "averages alone. "
  )
  
  if (!is.na(peer_rank) && peer_group_size >= 3) {
    if (peer_rank <= peer_group_size * 0.5) {
      improvement_text <- paste0(
        "The district's relative strong performance suggests ", 
        "effective practices that could be documented and sustained."
      )
    } else {
      improvement_text <- paste0(
        "The district's position indicates opportunities to ", 
        "learn from higher-performing peers with similar circumstances."
      )
    }
  } else {
    improvement_text <- paste0(
      "The limited peer comparison data emphasizes the ", 
      "importance of tracking performance against state ", 
      "benchmarks and historical trends."
    )
  }
  
  benchmarks_text <- paste0(
    state_comparison, 
    " ", 
    classification_text, 
    " ", 
    peer_text, 
    national_context, 
    improvement_text
  )
  
  return(benchmarks_text)
}
