
## ---------------------------
##
## Script name: Initial Tipping Analysis
##
## Author: Dr. Joan Dudney
##
## Date Created: 2026-01-14
##
## Copyright (c) Joan Dudney, 2026
## Email: dudney@ucsb.edu
##
## ---------------------------
##
## Notes: This code was first developed by R Fidler to estimate 
## ecosystem transitions and transien transitions
##   
##
## ---------------------------


librarian::shelf(tidyverse, data.table, here)

############################################################
#Create  Dataframes

marine.data <- readRDS(here("Data", "marine.data.raw.complete.rds"))
terrestrial.data.all <- readRDS(here("Data", "terrestrial.data.raw.complete.rds"))

all.dat<-bind_rows(
  marine.data%>%
    select(std_id, latitude, longitude, ecosystem, data, 
           survey1_year, survey2_year, survey1_value, survey2_value, metric_units),
  terrestrial.data.all%>%
    select(std_id, latitude, longitude, ecosystem, data, 
           survey1_year, survey2_year, survey1_value, survey2_value, metric_units))


#Complete data preparation pipeline
test.dat <- all.dat %>%
  # Rename columns to match expected format
  rename(
    year1 = survey1_year,
    year2 = survey2_year, 
    metric1 = survey1_value,
    metric2 = survey2_value
  ) %>%
  
  # Add temporal analysis columns
  group_by(std_id) %>%
  mutate(
    three_check = n_distinct(year2),
    rep1 = min(year2),
    rep2 = max(year2), 
    repdiff = rep2 - rep1
  ) %>%
  ungroup() %>%
  
  # Apply filtering criteria
  filter(three_check > 1) %>%  # Must have multiple time points
  mutate(
    lagcheck = case_when(
      ecosystem %in% c("Forests", "Grasslands") & repdiff < 10 ~ "short",
      !ecosystem %in% c("Forests", "Grasslands") & repdiff < 10 ~ "short", 
      TRUE ~ "keep"
    )
  ) %>%
  filter(lagcheck == "keep") %>%
  
  # Standardize ecosystem names
  mutate(ecosystem = case_when(
    ecosystem == "Marshes" ~ "Salt Marsh",
    ecosystem == "Grassland" ~ "Grasslands",
    TRUE ~ ecosystem
  )) %>%
  
  # Handle Grasslands site IDs (add data source to make unique)
  mutate(std_id = case_when(
    ecosystem == "Grasslands" ~ paste0(std_id, "_x_", data),
    TRUE ~ std_id
  )) %>%
  
  # Select final columns (excluding pp_change and raw_metric_change if not needed)
  select(std_id, latitude, longitude, ecosystem, data, 
         year1, year2, metric1, metric2, metric_units,
         three_check, rep1, rep2, repdiff, lagcheck)


test.dat<-test.dat%>%
  filter(!grepl("BioTIME", std_id))

str(test.dat)

table(test.dat$ecosystem)

############################################################
# ENHANCED PARAMETER STRUCTURE
############################################################

# Create a unified parameter structure that includes all transition types
create_unified_parameters <- function() {
  # Default unified parameters with recovery methods included
  unified_params <- list(
    "Forests" = list(
      alt_state_decline_threshold = 50,   # % drop for alternative state
      transient_decline_threshold = 50,    # % drop for transient change
      sustained_years = 10,                # Number of years decline must be sustained 
      alt_state_recovery_threshold = 50,   # % of max that defines alt state
      transient_recovery_threshold = 75,   # % of max that defines transient
      alt_state_method = "min_plus_percent",  # Method: "max_percent" or "min_plus_percent"
      min_plus_percent = 0.05,              # For alt_state CT: % of max to add to min
      recovery_method = "min_plus_percent", # Method for recovery: "min_plus_percent" or "max_percent"
      recovery_min_plus_percent = 5,       # For recovery: % of max to add to min
      recovery_percent_threshold = 75      # For max_percent recovery: % of max to exceed
    ),
    "Coral Reef" = list(
      alt_state_decline_threshold = 50,
      transient_decline_threshold = 50,
      sustained_years = 5,
      alt_state_recovery_threshold = 50,
      transient_recovery_threshold = 75,
      alt_state_method = "min_plus_percent",
      min_plus_percent = 1,
      recovery_method = "min_plus_percent",
      recovery_min_plus_percent = 15,
      recovery_percent_threshold = 75
    ),
    "Grasslands" = list(
      alt_state_decline_threshold = 40,
      transient_decline_threshold = 40,
      sustained_years = 5,
      alt_state_recovery_threshold = 50,
      transient_recovery_threshold = 75,
      alt_state_method = "min_plus_percent",
      min_plus_percent = 1,
      recovery_method = "min_plus_percent",
      recovery_min_plus_percent = 15,
      recovery_percent_threshold = 75
    ),
    "Salt Marsh" = list(
      alt_state_decline_threshold = 25,
      transient_decline_threshold = 25,
      sustained_years = 5,
      alt_state_recovery_threshold = 50,
      transient_recovery_threshold = 75,
      alt_state_method = "min_plus_percent",
      min_plus_percent = 1,
      recovery_method = "min_plus_percent",
      recovery_min_plus_percent = 20,
      recovery_percent_threshold = 75
    ),
    "Kelp" = list(
      alt_state_decline_threshold = 50,
      transient_decline_threshold = 50,
      sustained_years = 5,
      alt_state_recovery_threshold = 50,
      transient_recovery_threshold = 75,
      alt_state_method = "min_plus_percent",
      min_plus_percent = 1,
      recovery_method = "min_plus_percent",
      recovery_min_plus_percent = 15,
      recovery_percent_threshold = 75
    ),
    "Seagrass" = list(
      alt_state_decline_threshold = 50,
      transient_decline_threshold = 50,
      sustained_years = 5,
      alt_state_recovery_threshold = 50,
      transient_recovery_threshold = 75,
      alt_state_method = "min_plus_percent",
      min_plus_percent = 1,
      recovery_method = "min_plus_percent",
      recovery_min_plus_percent = 15,
      recovery_percent_threshold = 75
    )
  )
  
  return(unified_params)
}

############################################################
# CORE ANALYSIS FUNCTIONS
############################################################

get_parameters <- function(eco_params, ecosystem) {
  defaults <- list(
    alt_state_decline_threshold = 50,
    transient_decline_threshold = 50,
    sustained_years = ifelse(ecosystem == "Forests", 10, 5),
    alt_state_recovery_threshold = 50,
    transient_recovery_threshold = 75,
    alt_state_method = "max_percent",
    min_plus_percent = 5,
    recovery_method = "max_percent",
    recovery_min_plus_percent = 5,
    recovery_percent_threshold = 75
  )
  
  params <- list()
  for (param_name in names(defaults)) {
    params[[param_name]] <- ifelse(
      !is.null(eco_params[[param_name]]),
      eco_params[[param_name]],
      defaults[[param_name]]
    )
  }
  
  return(params)
}

# ============================================================
# CORRECTED: Sustained decline detection now respects method
# ============================================================

detect_sustained_decline_max_percent <- function(time_points, max_idx, max_value, p) {
  # Original threshold-based logic for max_percent method
  # Check if values stay below threshold for sustained_years
  
  alt_state_decline_value <- max_value * (1 - p$alt_state_decline_threshold/100)
  transient_decline_value <- max_value * (1 - p$transient_decline_threshold/100)
  
  alt_state_below_threshold_since <- NA
  transient_below_threshold_since <- NA
  in_alt_state_decline <- FALSE
  in_transient_decline <- FALSE
  
  decline_periods <- data.frame(
    start = numeric(), 
    end = numeric(), 
    decline_type = character(),
    stringsAsFactors = FALSE
  )
  
  for (i in (max_idx+1):nrow(time_points)) {
    current <- time_points[i, ]
    
    # Check for alternative state decline
    if (current$metric <= alt_state_decline_value) {
      if (!in_alt_state_decline) {
        alt_state_below_threshold_since <- current$year
        in_alt_state_decline <- TRUE
        
        if (!in_transient_decline) {
          transient_below_threshold_since <- current$year
          in_transient_decline <- TRUE
        }
      }
    }
    # Check for transient decline
    else if (current$metric <= transient_decline_value) {
      if (!in_transient_decline) {
        transient_below_threshold_since <- current$year
        in_transient_decline <- TRUE
      }
    }
    # Above thresholds - check for recovery
    else {
      if (current$metric / max_value >= 0.95) {
        in_alt_state_decline <- FALSE
        in_transient_decline <- FALSE
        alt_state_below_threshold_since <- NA
        transient_below_threshold_since <- NA
      }
    }
    
    # Check for sustained alt state decline
    if (in_alt_state_decline && !is.na(alt_state_below_threshold_since)) {
      if (i == nrow(time_points) || 
          (i < nrow(time_points) && time_points$metric[i+1] > alt_state_decline_value)) {
        
        if (current$year - alt_state_below_threshold_since >= p$sustained_years) {
          decline_periods <- rbind(decline_periods, data.frame(
            start = alt_state_below_threshold_since,
            end = current$year,
            decline_type = "alt_state",
            stringsAsFactors = FALSE
          ))
        }
        
        if (i < nrow(time_points)) {
          in_alt_state_decline <- FALSE
          alt_state_below_threshold_since <- NA
        }
      }
    }
    
    # Check for sustained transient decline
    if (in_transient_decline && !is.na(transient_below_threshold_since) && 
        (!in_alt_state_decline || is.na(alt_state_below_threshold_since))) {
      
      if (i == nrow(time_points) || 
          (i < nrow(time_points) && time_points$metric[i+1] > transient_decline_value)) {
        
        if (current$year - transient_below_threshold_since >= p$sustained_years) {
          decline_periods <- rbind(decline_periods, data.frame(
            start = transient_below_threshold_since,
            end = current$year,
            decline_type = "transient",
            stringsAsFactors = FALSE
          ))
        }
        
        if (i < nrow(time_points)) {
          in_transient_decline <- FALSE
          transient_below_threshold_since <- NA
        }
      }
    }
  }
  
  return(list(
    decline_periods = decline_periods,
    has_sustained_decline = nrow(decline_periods) > 0
  ))
}

detect_sustained_decline_min_plus_percent <- function(time_points, max_idx, max_value, p) {
  # NEW: Min-plus-percent logic for sustained decline
  # Two-stage process:
  # 1. Must decline below threshold (qualification)
  # 2. Must stay near minimum without recovering for sustained_years
  
  decline_threshold <- max_value * (1 - p$alt_state_decline_threshold/100)
  
  # Track running minimum and sustained decline periods
  running_min <- max_value
  running_min_year <- time_points$year[max_idx]
  in_sustained_decline <- FALSE
  sustained_decline_start <- NA
  
  decline_periods <- data.frame(
    start = numeric(), 
    end = numeric(), 
    decline_type = character(),
    stringsAsFactors = FALSE
  )
  
  for (i in (max_idx+1):nrow(time_points)) {
    current <- time_points[i, ]
    
    # Update running minimum
    if (current$metric < running_min) {
      running_min <- current$metric
      running_min_year <- current$year
    }
    
    # Stage 1: Check if we've declined below threshold (qualification)
    if (current$metric <= decline_threshold) {
      # Stage 2: Check if we're staying near minimum
      # Calculate escape threshold based on current running minimum
      escape_threshold <- running_min + (max_value * p$min_plus_percent / 100)
      
      if (current$metric <= escape_threshold) {
        # We're near the minimum
        if (!in_sustained_decline) {
          # Start tracking sustained decline
          sustained_decline_start <- current$year
          in_sustained_decline <- TRUE
        }
      } else {
        # Value has recovered above the escape threshold
        if (in_sustained_decline) {
          # End the sustained decline period
          duration <- time_points$year[i-1] - sustained_decline_start
          
          if (duration >= p$sustained_years) {
            decline_periods <- rbind(decline_periods, data.frame(
              start = sustained_decline_start,
              end = time_points$year[i-1],
              decline_type = "alt_state",
              stringsAsFactors = FALSE
            ))
          }
          
          in_sustained_decline <- FALSE
          sustained_decline_start <- NA
        }
      }
    } else {
      # Recovered above the decline threshold entirely
      if (in_sustained_decline) {
        # End the sustained decline period
        duration <- time_points$year[i-1] - sustained_decline_start
        
        if (duration >= p$sustained_years) {
          decline_periods <- rbind(decline_periods, data.frame(
            start = sustained_decline_start,
            end = time_points$year[i-1],
            decline_type = "alt_state",
            stringsAsFactors = FALSE
          ))
        }
        
        in_sustained_decline <- FALSE
        sustained_decline_start <- NA
      }
    }
  }
  
  # Check if we end in sustained decline
  if (in_sustained_decline) {
    duration <- time_points$year[nrow(time_points)] - sustained_decline_start
    
    if (duration >= p$sustained_years) {
      decline_periods <- rbind(decline_periods, data.frame(
        start = sustained_decline_start,
        end = time_points$year[nrow(time_points)],
        decline_type = "alt_state",
        stringsAsFactors = FALSE
      ))
    }
  }
  
  return(list(
    decline_periods = decline_periods,
    has_sustained_decline = nrow(decline_periods) > 0
  ))
}

# ============================================================
# MAIN SITE ANALYSIS FUNCTION (with corrected logic)
# ============================================================

analyze_site_common <- function(site_data, params, debug_mode = FALSE) {
  ecosystem <- unique(site_data$ecosystem)[1]
  site_id <- unique(site_data$std_id)[1]
  
  if (length(ecosystem) == 0 || length(site_id) == 0) {
    return(NULL)
  }
  
  if (is.null(params) || is.null(params[[ecosystem]])) {
    return(NULL)
  }
  
  p <- get_parameters(params[[ecosystem]], ecosystem)
  
  # Create chronological sequence
  time_points <- data.table(
    year = c(site_data$year1, site_data$year2),
    metric = c(site_data$metric1, site_data$metric2)
  )
  
  time_points <- time_points[!is.na(metric)]
  setkey(time_points, year)
  time_points <- unique(time_points)
  
  if (nrow(time_points) < 2) {
    return(NULL)
  }
  
  # Find maximum
  max_row <- time_points[metric == max(metric)]
  if (nrow(max_row) == 0) return(NULL)
  
  max_value <- max_row$metric[1]
  max_year <- max_row$year[1]
  max_idx <- which(time_points$year == max_year)
  
  # Handle no post-max data
  if (max_idx >= nrow(time_points)) {
    return(list(
      site_id = site_id,
      ecosystem = ecosystem,
      params = p,
      max_value = max_value,
      max_year = max_year,
      no_post_max_data = TRUE,
      min_value_after_max = NA,
      min_year_after_max = NA,
      max_recovery_value = NA,
      max_recovery_year = NA,
      max_recovery_percent = NA,
      decline_percent = NA,
      time_points = time_points,
      decline_periods = data.frame(),
      alt_state_decline_value = NA,
      transient_decline_value = NA,
      alt_state_recovery_value = NA,
      transient_recovery_value = NA,
      min_plus_percent_threshold = NA,
      recovery_threshold = NA,
      has_sustained_decline = FALSE,
      has_simple_decline = FALSE
    ))
  }
  
  # Calculate thresholds
  alt_state_decline_value <- max_value * (1 - p$alt_state_decline_threshold/100)
  transient_decline_value <- max_value * (1 - p$transient_decline_threshold/100)
  alt_state_recovery_value <- max_value * (p$alt_state_recovery_threshold/100)
  transient_recovery_value <- max_value * (p$transient_recovery_threshold/100)
  
  # Post-max statistics
  post_max_points <- time_points[year > max_year]
  
  if (nrow(post_max_points) == 0) {
    return(list(
      site_id = site_id,
      ecosystem = ecosystem,
      params = p,
      max_value = max_value,
      max_year = max_year,
      no_post_max_data = TRUE,
      min_value_after_max = NA,
      min_year_after_max = NA,
      max_recovery_value = NA,
      max_recovery_year = NA,
      max_recovery_percent = NA,
      decline_percent = NA,
      time_points = time_points,
      decline_periods = data.frame(),
      alt_state_decline_value = alt_state_decline_value,
      transient_decline_value = transient_decline_value,
      alt_state_recovery_value = alt_state_recovery_value,
      transient_recovery_value = transient_recovery_value,
      min_plus_percent_threshold = NA,
      recovery_threshold = NA,
      has_sustained_decline = FALSE,
      has_simple_decline = FALSE
    ))
  }
  
  # Find minimum after max
  min_after_max_row <- post_max_points[metric == min(metric)]
  min_value_after_max <- min_after_max_row$metric[1]
  min_year_after_max <- min_after_max_row$year[1]
  min_idx <- which(time_points$year == min_year_after_max)
  
  decline_percent <- 100 * (1 - min_value_after_max/max_value)
  
  # Find maximum recovery after minimum
  post_min_points <- time_points[year > min_year_after_max]
  
  max_recovery_value <- min_value_after_max
  max_recovery_year <- min_year_after_max
  max_recovery_percent <- (min_value_after_max / max_value) * 100
  
  if (nrow(post_min_points) > 0) {
    recovery_row <- post_min_points[metric == max(metric)]
    max_recovery_value <- recovery_row$metric[1]
    max_recovery_year <- recovery_row$year[1]
    max_recovery_percent <- (max_recovery_value / max_value) * 100
  }
  
  # Calculate thresholds
  min_plus_percent_threshold <- min_value_after_max + (max_value * p$min_plus_percent/100)
  
  if (p$recovery_method == "min_plus_percent") {
    recovery_threshold <- min_value_after_max + (max_value * p$recovery_min_plus_percent/100)
  } else {
    recovery_threshold <- max_value * (p$recovery_percent_threshold/100)
  }
  
  # ============================================================
  # CORRECTED: Use method-specific sustained decline detection
  # ============================================================
  
  if (p$alt_state_method == "max_percent") {
    decline_result <- detect_sustained_decline_max_percent(time_points, max_idx, max_value, p)
  } else {
    decline_result <- detect_sustained_decline_min_plus_percent(time_points, max_idx, max_value, p)
  }
  
  decline_periods <- decline_result$decline_periods
  has_sustained_decline <- decline_result$has_sustained_decline
  
  # Simple decline check (for recovery classification)
  has_simple_decline <- FALSE
  if (decline_percent >= p$alt_state_decline_threshold) {
    has_simple_decline <- TRUE
  }
  
  return(list(
    site_id = site_id,
    ecosystem = ecosystem,
    params = p,
    max_value = max_value,
    max_year = max_year,
    min_value_after_max = min_value_after_max,
    min_year_after_max = min_year_after_max,
    max_recovery_value = max_recovery_value,
    max_recovery_year = max_recovery_year,
    max_recovery_percent = max_recovery_percent,
    decline_percent = decline_percent,
    time_points = time_points,
    decline_periods = decline_periods,
    alt_state_decline_value = alt_state_decline_value,
    transient_decline_value = transient_decline_value,
    alt_state_recovery_value = alt_state_recovery_value,
    transient_recovery_value = transient_recovery_value,
    min_plus_percent_threshold = min_plus_percent_threshold,
    recovery_threshold = recovery_threshold,
    has_sustained_decline = has_sustained_decline,
    has_simple_decline = has_simple_decline,
    no_post_max_data = FALSE
  ))
}

# ============================================================
# CLASSIFICATION FUNCTIONS (unchanged from original)
# ============================================================

classify_ct_tt <- function(analysis) {
  alt_state_ct <- FALSE
  transient_ct <- FALSE
  no_ct <- TRUE
  
  if (!analysis$has_sustained_decline) {
    return(list(
      alt_state_ct = FALSE,
      transient_ct = FALSE,
      no_ct = TRUE
    ))
  }
  
  p <- analysis$params
  
  if (p$alt_state_method == "max_percent") {
    if (analysis$max_recovery_percent < p$alt_state_recovery_threshold) {
      alt_state_ct <- TRUE
      transient_ct <- FALSE
      no_ct <- FALSE
    }
    else if (analysis$max_recovery_percent >= p$alt_state_recovery_threshold && 
             analysis$max_recovery_percent < p$transient_recovery_threshold) {
      alt_state_ct <- FALSE
      transient_ct <- TRUE
      no_ct <- FALSE
    }
    else {
      alt_state_ct <- FALSE
      transient_ct <- FALSE
      no_ct <- TRUE
    }
  } else if (p$alt_state_method == "min_plus_percent") {
    min_idx <- which(analysis$time_points$year == analysis$min_year_after_max)
    
    recovery_threshold <- analysis$min_value_after_max + (analysis$max_value * p$recovery_min_plus_percent/100)
    
    exceeds_min_plus_threshold <- FALSE
    exceeds_recovery_threshold <- FALSE
    
    if (!is.na(min_idx) && min_idx < nrow(analysis$time_points)) {
      for (i in (min_idx+1):nrow(analysis$time_points)) {
        current_value <- analysis$time_points$metric[i]
        if (current_value > analysis$min_plus_percent_threshold) {
          exceeds_min_plus_threshold <- TRUE
        }
        if (current_value > recovery_threshold) {
          exceeds_recovery_threshold <- TRUE
          break
        }
      }
    }
    
    if (!exceeds_min_plus_threshold) {
      alt_state_ct <- TRUE
      transient_ct <- FALSE
      no_ct <- FALSE
    }
    else if (exceeds_min_plus_threshold && !exceeds_recovery_threshold) {
      alt_state_ct <- FALSE
      transient_ct <- TRUE
      no_ct <- FALSE
    }
    else {
      alt_state_ct <- FALSE
      transient_ct <- FALSE
      no_ct <- TRUE
    }
  }
  
  return(list(
    alt_state_ct = alt_state_ct,
    transient_ct = transient_ct,
    no_ct = no_ct
  ))
}

classify_recovery <- function(analysis) {
  if (is.null(analysis) || !analysis$has_simple_decline) {
    return(FALSE)
  }
  
  if (analysis$decline_percent < analysis$params$alt_state_decline_threshold) {
    return(FALSE)
  }
  
  # Ecosystem-specific recovery thresholds
  if (analysis$ecosystem == "Coral Reef") {
    coral_threshold_value <- analysis$max_value * 0.75
    if (analysis$max_recovery_value >= coral_threshold_value) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
  
  if (analysis$ecosystem == "Forests") {
    forest_threshold_value <- analysis$min_value_after_max + (analysis$max_value * 0.05)
    if (analysis$max_recovery_value >= forest_threshold_value) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
  
  if (analysis$max_recovery_value >= analysis$recovery_threshold) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

# ============================================================
# DATA PREPARATION FUNCTIONS
# ============================================================

prepare_aligned_site_data <- function(site_data, analysis) {
  if (is.null(analysis) || is.null(analysis$max_value) || is.null(analysis$max_year)) {
    return(NULL)
  }
  
  if (!is.null(analysis$time_points) && nrow(analysis$time_points) > 0) {
    time_points <- analysis$time_points
    time_points$metric_pct <- (time_points$metric / analysis$max_value) * 100
    time_points$rel_year <- time_points$year - analysis$max_year
    
    zero_idx <- which(time_points$rel_year == 0)
    if (length(zero_idx) == 0) {
      max_point <- data.table(
        year = analysis$max_year,
        metric = analysis$max_value,
        metric_pct = 100,
        rel_year = 0
      )
      time_points <- rbind(time_points, max_point)
    } else {
      time_points$metric_pct[zero_idx] <- 100
    }
    
    time_points$std_id <- analysis$site_id
    return(as.data.frame(time_points))
  }
  
  long_data <- bind_rows(
    site_data %>% select(std_id, year = year1, metric = metric1),
    site_data %>% select(std_id, year = year2, metric = metric2)
  ) %>% 
    arrange(year) %>%
    distinct() %>%
    filter(!is.na(metric))
  
  long_data$metric_pct <- (long_data$metric / analysis$max_value) * 100
  long_data$rel_year <- long_data$year - analysis$max_year
  
  max_point <- data.frame(
    std_id = analysis$site_id,
    year = analysis$max_year,
    metric = analysis$max_value,
    metric_pct = 100,
    rel_year = 0
  )
  
  aligned_data <- bind_rows(long_data, max_point) %>% distinct()
  return(aligned_data)
}

calculate_mean_trend <- function(aligned_data) {
  if (is.null(aligned_data) || nrow(aligned_data) == 0) {
    return(data.frame())
  }
  
  mean_trend <- aligned_data %>%
    group_by(rel_year) %>%
    summarize(
      mean_metric_pct = mean(metric_pct, na.rm = TRUE),
      se_metric = sd(metric_pct, na.rm = TRUE) / sqrt(n()),
      n = n()
    ) %>%
    filter(n >= 2)
  
  if (!any(abs(mean_trend$rel_year) < 0.01)) {
    mean_trend <- bind_rows(
      mean_trend,
      data.frame(rel_year = 0, mean_metric_pct = 100, se_metric = 0, 
                 n = length(unique(aligned_data$std_id)))
    ) %>% arrange(rel_year)
  } else {
    zero_idx <- which.min(abs(mean_trend$rel_year))
    mean_trend$mean_metric_pct[zero_idx] <- 100
    mean_trend$se_metric[zero_idx] <- 0
  }
  
  plot_mean_trend <- mean_trend %>% filter(rel_year >= -5, rel_year <= 20)
  return(plot_mean_trend)
}

# ============================================================
# MAIN ANALYSIS FUNCTION
# ============================================================

run_unified_transition_analysis <- function(data, custom_params = NULL, debug_mode = FALSE) {
  start_time <- Sys.time()
  
  if (is.null(custom_params)) {
    params <- create_unified_parameters()
  } else {
    params <- custom_params
  }
  
  if (!data.table::is.data.table(data)) {
    dt <- data.table::as.data.table(data)
  } else {
    dt <- data
  }
  
  site_ids <- unique(dt$std_id)
  total_sites <- length(site_ids)
  
  all_analyses <- list()
  all_classifications <- list()
  
  result_df <- data.frame(
    site_id = character(),
    ecosystem = character(),
    max_value = numeric(),
    max_year = numeric(),
    min_value_after_max = numeric(),
    min_year_after_max = numeric(),
    max_recovery_value = numeric(),
    max_recovery_year = numeric(),
    max_recovery_percent = numeric(),
    decline_percent = numeric(),
    has_sustained_decline = logical(),
    has_simple_decline = logical(),
    alt_state_ct = logical(),
    transient_ct = logical(),
    recovery = logical(),
    no_ct = logical(),
    stringsAsFactors = FALSE
  )
  
  aligned_data <- list()
  ecosystems <- unique(dt$ecosystem)
  eco_counts <- data.frame(
    ecosystem = ecosystems,
    critical = 0,
    transient = 0,
    recovery = 0,
    total_sites = 0,
    stringsAsFactors = FALSE
  )
  rownames(eco_counts) <- ecosystems
  
  cat("Analyzing all sites (single pass with CORRECTED sustained decline detection)...\n")
  pb <- txtProgressBar(min = 0, max = total_sites, style = 3)
  
  recovery_sites_by_ecosystem <- list()
  for (eco in ecosystems) {
    recovery_sites_by_ecosystem[[eco]] <- character(0)
  }
  
  for (i in seq_along(site_ids)) {
    site_id <- site_ids[i]
    site_data <- dt[std_id == site_id]
    
    if (nrow(site_data) < 2) {
      setTxtProgressBar(pb, i)
      next
    }
    
    ecosystem <- unique(site_data$ecosystem)[1]
    
    if (ecosystem %in% rownames(eco_counts)) {
      eco_counts[ecosystem, "total_sites"] <- eco_counts[ecosystem, "total_sites"] + 1
    }
    
    analysis <- analyze_site_common(site_data, params, debug_mode)
    
    if (is.null(analysis)) {
      setTxtProgressBar(pb, i)
      next
    }
    
    all_analyses[[site_id]] <- analysis
    
    if (!is.null(analysis$no_post_max_data) && analysis$no_post_max_data) {
      classification <- list(
        alt_state_ct = FALSE,
        transient_ct = FALSE,
        recovery = FALSE,
        no_ct = TRUE
      )
      all_classifications[[site_id]] <- classification
      
      result_df <- rbind(result_df, data.frame(
        site_id = analysis$site_id,
        ecosystem = analysis$ecosystem,
        max_value = analysis$max_value,
        max_year = analysis$max_year,
        min_value_after_max = NA,
        min_year_after_max = NA,
        max_recovery_value = NA,
        max_recovery_year = NA,
        max_recovery_percent = NA,
        decline_percent = NA,
        has_sustained_decline = FALSE,
        has_simple_decline = FALSE,
        alt_state_ct = FALSE,
        transient_ct = FALSE,
        recovery = FALSE,
        no_ct = TRUE,
        stringsAsFactors = FALSE
      ))
      
      setTxtProgressBar(pb, i)
      next
    }
    
    ct_tt_class <- classify_ct_tt(analysis)
    
    recovery <- FALSE
    if (ct_tt_class$no_ct) {
      recovery <- classify_recovery(analysis)
      
      if (recovery) {
        recovery_sites_by_ecosystem[[ecosystem]] <- c(recovery_sites_by_ecosystem[[ecosystem]], site_id)
      }
    }
    
    classification <- list(
      alt_state_ct = ct_tt_class$alt_state_ct,
      transient_ct = ct_tt_class$transient_ct,
      recovery = recovery,
      no_ct = !ct_tt_class$alt_state_ct && !ct_tt_class$transient_ct && !recovery
    )
    all_classifications[[site_id]] <- classification
    
    result_df <- rbind(result_df, data.frame(
      site_id = analysis$site_id,
      ecosystem = analysis$ecosystem,
      max_value = analysis$max_value,
      max_year = analysis$max_year,
      min_value_after_max = analysis$min_value_after_max,
      min_year_after_max = analysis$min_year_after_max,
      max_recovery_value = analysis$max_recovery_value,
      max_recovery_year = analysis$max_recovery_year,
      max_recovery_percent = analysis$max_recovery_percent,
      decline_percent = analysis$decline_percent,
      has_sustained_decline = analysis$has_sustained_decline,
      has_simple_decline = analysis$has_simple_decline,
      alt_state_ct = classification$alt_state_ct,
      transient_ct = classification$transient_ct,
      recovery = classification$recovery,
      no_ct = classification$no_ct,
      stringsAsFactors = FALSE
    ))
    
    if (ecosystem %in% rownames(eco_counts)) {
      if (classification$alt_state_ct) {
        eco_counts[ecosystem, "critical"] <- eco_counts[ecosystem, "critical"] + 1
      } else if (classification$transient_ct) {
        eco_counts[ecosystem, "transient"] <- eco_counts[ecosystem, "transient"] + 1
      } else if (classification$recovery) {
        eco_counts[ecosystem, "recovery"] <- eco_counts[ecosystem, "recovery"] + 1
      }
    }
    
    setTxtProgressBar(pb, i)
  }
  
  close(pb)
  
  cat("\nSites by ecosystem and transition type:\n")
  print(eco_counts)
  
  # Prepare plotting data
  cat("\nPreparing data for plotting...\n")
  all_transition_data <- list()
  
  for (ecosystem in ecosystems) {
    eco_sites <- result_df$site_id[result_df$ecosystem == ecosystem]
    eco_key <- tolower(gsub(" ", "_", ecosystem))
    
    critical_key <- paste0(eco_key, "_critical")
    transient_key <- paste0(eco_key, "_transient")
    recovery_key <- paste0(eco_key, "_recovery")
    
    # Critical sites
    critical_sites <- result_df$site_id[result_df$ecosystem == ecosystem & result_df$alt_state_ct]
    
    if (length(critical_sites) > 0) {
      critical_aligned <- data.frame()
      for (site_id in critical_sites) {
        site_data <- dt[std_id == site_id]
        analysis <- all_analyses[[site_id]]
        aligned <- prepare_aligned_site_data(site_data, analysis)
        if (!is.null(aligned)) {
          critical_aligned <- rbind(critical_aligned, aligned)
        }
      }
      
      critical_mean <- calculate_mean_trend(critical_aligned)
      
      all_transition_data[[critical_key]] <- list(
        aligned_data = critical_aligned,
        site_info = result_df[result_df$site_id %in% critical_sites, ],
        mean_trend = critical_mean,
        has_data = nrow(critical_aligned) > 0
      )
    } else {
      all_transition_data[[critical_key]] <- list(
        aligned_data = data.frame(),
        site_info = data.frame(),
        mean_trend = data.frame(),
        has_data = FALSE
      )
    }
    
    # Transient sites
    transient_sites <- result_df$site_id[result_df$ecosystem == ecosystem & result_df$transient_ct]
    
    if (length(transient_sites) > 0) {
      transient_aligned <- data.frame()
      for (site_id in transient_sites) {
        site_data <- dt[std_id == site_id]
        analysis <- all_analyses[[site_id]]
        aligned <- prepare_aligned_site_data(site_data, analysis)
        if (!is.null(aligned)) {
          transient_aligned <- rbind(transient_aligned, aligned)
        }
      }
      
      transient_mean <- calculate_mean_trend(transient_aligned)
      
      all_transition_data[[transient_key]] <- list(
        aligned_data = transient_aligned,
        site_info = result_df[result_df$site_id %in% transient_sites, ],
        mean_trend = transient_mean,
        has_data = nrow(transient_aligned) > 0
      )
    } else {
      all_transition_data[[transient_key]] <- list(
        aligned_data = data.frame(),
        site_info = data.frame(),
        mean_trend = data.frame(),
        has_data = FALSE
      )
    }
    
    # Recovery sites
    recovery_sites <- result_df$site_id[result_df$ecosystem == ecosystem & result_df$recovery]
    
    if (length(recovery_sites) > 0) {
      recovery_aligned <- data.frame()
      for (site_id in recovery_sites) {
        site_data <- dt[std_id == site_id]
        analysis <- all_analyses[[site_id]]
        aligned <- prepare_aligned_site_data(site_data, analysis)
        if (!is.null(aligned)) {
          recovery_aligned <- rbind(recovery_aligned, aligned)
        }
      }
      
      recovery_mean <- calculate_mean_trend(recovery_aligned)
      
      all_transition_data[[recovery_key]] <- list(
        aligned_data = recovery_aligned,
        site_info = result_df[result_df$site_id %in% recovery_sites, ],
        mean_trend = recovery_mean,
        has_data = nrow(recovery_aligned) > 0
      )
    } else {
      all_transition_data[[recovery_key]] <- list(
        aligned_data = data.frame(),
        site_info = data.frame(),
        mean_trend = data.frame(),
        has_data = FALSE
      )
    }
  }
  
  end_time <- Sys.time()
  run_time <- difftime(end_time, start_time, units = "mins")
  
  cat("\nAnalysis completed in", round(run_time, 2), "minutes\n")
  
  summary_stats <- data.frame(
    total_sites = nrow(result_df),
    critical_transitions = sum(result_df$alt_state_ct),
    transient_transitions = sum(result_df$transient_ct),
    recoveries = sum(result_df$recovery),
    no_transitions = sum(result_df$no_ct)
  )
  
  cat("\nOverall Summary:\n")
  print(summary_stats)
  
  eco_summary <- result_df %>%
    group_by(ecosystem) %>%
    summarize(
      total_sites = n(),
      sites_with_sustained_decline = sum(has_sustained_decline, na.rm = TRUE),
      sites_with_simple_decline = sum(has_simple_decline, na.rm = TRUE),
      sites_with_alt_state = sum(alt_state_ct, na.rm = TRUE),
      sites_with_transient = sum(transient_ct, na.rm = TRUE),
      sites_with_recovery = sum(recovery, na.rm = TRUE),
      percent_sustained_decline = sites_with_sustained_decline / total_sites * 100,
      percent_simple_decline = sites_with_simple_decline / total_sites * 100,
      percent_alt_state = sites_with_alt_state / total_sites * 100,
      percent_transient = sites_with_transient / total_sites * 100,
      percent_recovery = sites_with_recovery / total_sites * 100
    )
  
  cat("\nEcosystem Summary:\n")
  print(eco_summary)
  
  return(list(
    result_df = result_df,
    all_analyses = all_analyses,
    all_classifications = all_classifications,
    all_transition_data = all_transition_data,
    summary = summary_stats,
    ecosystem_summary = eco_summary,
    ecosystem_counts = eco_counts,
    parameters = params
  ))
}

# ============================================================
# VERIFICATION FUNCTION
# ============================================================

verify_site_analysis <- function(site_id, original_data, unified_results) {
  cat("\n========== VERIFYING SITE:", site_id, "==========\n")
  
  current_params <- unified_results$parameters
  
  site_raw <- original_data %>% filter(std_id == site_id) %>% arrange(year1)
  cat("\n--- ORIGINAL SURVEY PAIRS ---\n")
  print(site_raw %>% select(year1, year2, metric1, metric2))
  
  time_points_manual <- data.frame(
    year = c(site_raw$year1, site_raw$year2),
    metric = c(site_raw$metric1, site_raw$metric2)
  ) %>% 
    arrange(year) %>%
    distinct() %>%
    filter(!is.na(metric))
  
  cat("\n--- CONSTRUCTED TIME SERIES ---\n")
  print(time_points_manual)
  
  max_idx <- which.max(time_points_manual$metric)
  manual_max_value <- time_points_manual$metric[max_idx]
  manual_max_year <- time_points_manual$year[max_idx]
  
  cat("\n--- MAXIMUM DETECTION ---\n")
  cat("Manual max:", manual_max_value, "in year", manual_max_year, "\n")
  
  post_max <- time_points_manual[time_points_manual$year > manual_max_year, ]
  if(nrow(post_max) > 0) {
    min_idx <- which.min(post_max$metric)
    manual_min_after_max <- post_max$metric[min_idx]
    manual_min_year <- post_max$year[min_idx]
    manual_decline_pct <- 100 * (1 - manual_min_after_max/manual_max_value)
    
    cat("Manual min after max:", manual_min_after_max, "in year", manual_min_year, "\n")
    cat("Manual decline percent:", round(manual_decline_pct, 1), "%\n")
  } else {
    cat("NO DATA AFTER MAXIMUM\n")
    manual_min_after_max <- NA
    manual_min_year <- NA
    manual_decline_pct <- NA
  }
  
  analysis <- unified_results$all_analyses[[site_id]]
  cat("\n--- ANALYSIS COMPARISON ---\n")
  cat("Analysis max:", analysis$max_value, "vs Manual:", manual_max_value, 
      ifelse(analysis$max_value == manual_max_value, "✓", "✗"), "\n")
  cat("Analysis max year:", analysis$max_year, "vs Manual:", manual_max_year,
      ifelse(analysis$max_year == manual_max_year, "✓", "✗"), "\n")
  
  if(!is.na(manual_min_after_max)) {
    cat("Analysis min:", analysis$min_value_after_max, "vs Manual:", manual_min_after_max,
        ifelse(abs(analysis$min_value_after_max - manual_min_after_max) < 0.001, "✓", "✗"), "\n")
    cat("Analysis decline:", round(analysis$decline_percent, 1), "% vs Manual:", round(manual_decline_pct, 1), "%",
        ifelse(abs(analysis$decline_percent - manual_decline_pct) < 0.1, "✓", "✗"), "\n")
  }
  
  ecosystem <- analysis$ecosystem
  eco_params <- current_params[[ecosystem]]
  
  if(!is.null(eco_params)) {
    sustained_years_req <- eco_params$sustained_years
    decline_threshold <- eco_params$alt_state_decline_threshold
    
    cat("\n--- SUSTAINED DECLINE CHECK ---\n")
    cat("Ecosystem:", ecosystem, "\n")
    cat("Method:", eco_params$alt_state_method, "\n")
    cat("Requires", sustained_years_req, "years of decline >", decline_threshold, "%\n")
    cat("Analysis says sustained decline:", analysis$has_sustained_decline, "\n")
    
    cat("\n--- CURRENT THRESHOLDS FOR", ecosystem, "---\n")
    cat("Method:", eco_params$alt_state_method, "\n")
    
    if(eco_params$alt_state_method == "max_percent") {
      cat("Alt state recovery threshold:", eco_params$alt_state_recovery_threshold, "% of max\n")
      cat("Transient recovery threshold:", eco_params$transient_recovery_threshold, "% of max\n")
    } else {
      cat("Min plus percent (CT escape):", eco_params$min_plus_percent, "% of max\n")
      cat("Recovery threshold (min + X%):", eco_params$recovery_min_plus_percent, "% of max\n")
    }
  }
  
  result_row <- unified_results$result_df %>% filter(site_id == !!site_id)
  cat("\n--- FINAL CLASSIFICATION ---\n")
  cat("Critical:", result_row$alt_state_ct, "| Transient:", result_row$transient_ct, 
      "| Recovery:", result_row$recovery, "| No CT:", result_row$no_ct, "\n")
  
  return(list(
    manual_time_series = time_points_manual,
    manual_max = manual_max_value,
    manual_decline_pct = manual_decline_pct,
    current_ecosystem_params = eco_params,
    analysis_matches = list(
      max = analysis$max_value == manual_max_value,
      decline = abs(analysis$decline_percent - manual_decline_pct) < 0.1
    )
  ))
}

# ============================================================
# USAGE NOTES
# ============================================================

# Example usage:
unified_results <- run_unified_transition_analysis(test.dat)
#all_transition_data <- unified_results$all_transition_data

# # Convert all_transition_data list to a single data frame
# all_transition_df <- bind_rows(
#   lapply(names(unified_results$all_transition_data), function(name) {
#     data <- unified_results$all_transition_data[[name]]
#     if (data$has_data && nrow(data$aligned_data) > 0) {
#       data$aligned_data %>%
#         mutate(transition_type = name)
#     }
#   })
# )

# # Add coordinates to results
# results_with_coords <- unified_results$result_df %>%
#   left_join(
#     test.dat %>% 
#       select(std_id, latitude, longitude) %>% 
#       distinct(),
#     by = c("site_id" = "std_id")
#   )


results_final <- unified_results$result_df %>%
  left_join(
    test.dat %>% 
      select(std_id) %>% 
      distinct(),
    by = c("site_id" = "std_id")
  )


