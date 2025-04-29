get_calculator_table <- function(selected_exercise, calc_weight, results, predictions) {
    # Check if results are available
    if (is.null(results) || nrow(results) == 0) {
      return(DT::datatable(data.frame(`Number of Sets` = integer(0),
                                      `Max Tonnage` = integer(0),
                                      # `Last Tonnage` = integer(0), # Removed
                                      `Reps to Beat PR` = integer(0),
                                      `Reps for Trend` = integer(0)), # Changed column name
                           options = list(pageLength = 5),
                           rownames = FALSE))
    }

    # Summarise historical max tonnage
    summary_data <- results %>%
      group_by(n_sets) %>%
      summarise(
        maxTonnage = max(max_tonnage, na.rm = TRUE), # Ensure NA removal if needed
        .groups = "drop"
      )

    # --- Join with predictions ---
    # Predictions reactive already returns n_sets and predicted_tonnage
    # Ensure predictions has rows for all n_sets (1-5), possibly with NA if prediction failed
    # A full join might be safest if predictions could somehow miss an n_set
    # But left_join should work if predictions always has n_sets 1-5
    table_data <- summary_data %>%
      left_join(predictions, by = "n_sets") %>%
      mutate(
        # Use selected_exercise (synced) for consistency
        bw_multiplier_val = exercise_df$bwMultiplier[exercise_df$Exercise == selected_exercise],
        bw_multiplier_val = ifelse(length(bw_multiplier_val) == 0, 0, bw_multiplier_val),
        latest_bw_ma = tail(merged_df$BodyWeight_MA, 1),
        adjusted_weight = calc_weight + bw_multiplier_val * latest_bw_ma,
        # Calculate Reps to Beat PR
        repsPR = ifelse(adjusted_weight == 0 | is.na(adjusted_weight) | is.na(maxTonnage), NA, maxTonnage / adjusted_weight),
        # Calculate Reps for Trend
        repsTrend = ifelse(adjusted_weight == 0 | is.na(adjusted_weight) | is.na(predicted_tonnage), NA, predicted_tonnage / adjusted_weight)
      ) %>%
      mutate(
        repsPR = as.double(round(repsPR, 2)),
        repsTrend = as.double(round(repsTrend, 2)),
        maxTonnage = as.integer(round(maxTonnage))
      ) %>%
      select(
        `Number of Sets` = n_sets,
        `Max Tonnage` = maxTonnage,
        `Reps to Beat PR` = repsPR,
        `Reps for Trend` = repsTrend
      )

    # Return the data frame as a DT table
    DT::datatable(
      table_data,
      options = list(pageLength = 5),
      rownames = FALSE,
      caption = selected_exercise
    )
}