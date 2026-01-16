## ---------------------------
##
## Script name: CT and TT Analysis 
##
## Author: Dr. Joan Dudney
##
## Date Created: 2025-01-15
##
## Copyright (c) Joan Dudney, 2025
## Email: dudney@ucsb.edu
##
## ---------------------------
##
## Notes:
##   Runs unified transition analysis and exports results with climate data
##
## ---------------------------

librarian::shelf(tidyverse, data.table)

# Source the main analysis script
source("Scripts/CalculateCT_TT.R")

# Run the analysis if not already done
if (!exists("unified_results")) {
  unified_results <- run_unified_transition_analysis(test.dat)
}

# Create a dataset with climate variables to merge with transitions data
marine_clean <- marine.data |>
  select(-matches("survey|pp_change_rate")) |>
  distinct(std_id, .keep_all = TRUE) |> 
  rename(historical_temp = baseline_climate_mean_sst,
      historical_sd_temp = baseline_climate_sd_sst,
      period1_max_zscore_temp = period1_max_zscore_sst,
      period2_max_zscore_temp = period2_max_zscore_sst,
      period1_mean_zscore_temp = period1_mean_zscore_bidirectional_sst,
      period2_mean_zscore_temp = period2_mean_zscore_bidirectional_sst) |>
  select(-matches("period.*temp$"))

terr_clean <- terrestrial.data.all |>
  # Add the same std_id modification for Grasslands
  mutate(std_id = case_when(
    ecosystem == "Grasslands" ~ paste0(std_id, "_x_", data),
    TRUE ~ std_id
  )) |> 
  select(-matches("survey|pp_change_rate")) |>
  distinct(std_id, .keep_all = TRUE) |> 
  rename(historical_temp = baseline_climate_mean_temp,
      historical_sd_temp = baseline_climate_sd_temp,
      period1_max_zscore_temp = period1_max_zscore_temp,
      period2_max_zscore_temp = period2_max_zscore_temp,
      period1_mean_zscore_temp = period1_mean_zscore_bidirectional_temp,
      period2_mean_zscore_temp = period2_mean_zscore_bidirectional_temp) |>
  select(-matches("prec|sd_zscore|above")) 
  
  
# Joining data 
all_clean_climate <- bind_rows(marine_clean, terr_clean) |> 
  select(-metric) |>
  distinct(std_id, .keep_all = TRUE)

grass_clim <- all_clean_climate |> 
   filter(ecosystem == "Grasslands")

grass_ct <- results_final |> 
  rename(std_id = site_id) |> 
  filter(ecosystem == "Grasslands")

merge_grass <- grass_ct |> 
  left_join(grass_clim)

# Use unified_results instead of results_final
ct_climate <- unified_results$result_df |>
  rename(std_id = site_id) |> 
  left_join(all_clean_climate, by = c("std_id", "ecosystem"))

# Test with Grasslands
grass_test <- unified_results$result_df |>
  filter(ecosystem == "Grasslands") |>
  rename(std_id = site_id)

# Check if these IDs match
head(grass_test$std_id)
head(grass_clim$std_id)

ct_climate <- results_final |>
  rename(std_id = site_id) |> 
  left_join(all_clean_climate)

grass <- ct_climate |> 
  filter(ecosystem == "Grasslands")

############################################################
# Filter to just places that experienced big declines (50%)
############################################################

big_decline <- ct_climate |> 
  #filter(decline_percent>49.9) |> 
  select(-matches("decline|min|value|percent|year")) |> 
   pivot_longer(
    cols = c(alt_state_ct, transient_ct, recovery, no_ct),
    names_to = "transition_type",
    values_to = "transition_value"
  )

big_decline |> 
  filter(transition_value == TRUE) |>
  ggplot(aes(x = historical_temp, fill = ecosystem, color = transition_type)) +
  geom_density(alpha = 0.6) +
  facet_wrap(~ecosystem, ncol = 1) +
  labs(
    x = "Historical Temperature",
    y = "Density",
    title = "Temperature Distribution by Transition Type (>50% Decline)"
  ) +
  theme_minimal()


big_decline |> 
  filter(transition_value == TRUE) |>
  ggplot(aes(x = historical_temp, fill = transition_type)) +
  geom_histogram(bins = 30, alpha = 0.7) +
  facet_wrap(~transition_type, ncol = 1, scales = "free_y") +
  labs(
    x = "Historical Temperature",
    y = "Count",
    title = "Sites by Temperature and Transition Type (>50% Decline)"
  ) +
  theme_minimal()


big_decline |>
  mutate(temp_bin = cut(historical_temp, breaks = 30)) |>
  group_by(transition_type, temp_bin, ecosystem) |>
  summarise(
    prop_true = mean(transition_value, na.rm = TRUE),
    temp_mid = mean(historical_temp, na.rm = TRUE),
    .groups = "drop"
  ) |>
  ggplot(aes(x = temp_mid, y = prop_true, color = transition_type, size = ecosystem)) +
  geom_point(size = 2) +
  geom_smooth(method = "loess", se = TRUE, alpha = 0.2) +
  labs(
    x = "Historical Temperature (°C)",
    y = "Proportion with Transition",
    title = "Probability of Transition Type by Temperature (>50% Decline)",
    color = "Transition Type"
  ) +
  theme_minimal()
