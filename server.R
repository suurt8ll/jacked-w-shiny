#---- Shiny server ----
server <- function(input, output, session) {
  # Filter the exercise choice list based on chosen date
  observe({
    if (length(input$date) != 0) {
      x <- merged_df %>% filter(date == input$date) %>% pull(name) %>% unique()
    } else {
      x <- unique(merged_df$name)
    }
    updateSelectInput(session, "exercise", choices = x)
    updateSelectInput(session, "calc_exercise", choices = x)
  })

  # Reactive expression for historical max tonnage results
  reactive_tonnage_results <- reactive({
    req(input$exercise) # Ensure input$exercise is available (synced with calc_exercise)

    exercise_data <- merged_df %>%
      filter(name == input$exercise) %>%
      mutate(tonnage = reps * (isBW * (bwMultiplier * BodyWeight_MA) + weight)) %>%
      select(date, tonnage) %>%
      arrange(date)

    if (nrow(exercise_data) == 0) {
      return(NULL)
    }

    lapply(1:5, function(n) {
      calculate_max_tonnage(exercise_data, n) %>%
        mutate(n_sets = n)
    }) %>%
      bind_rows()
  })

  reactive_predicted_tonnage <- reactive({
    results <- reactive_tonnage_results()
    regression_start_date <- input$regressionStartDate # Use the synced date

    # Check if historical results are available
    if (is.null(results) || nrow(results) == 0) {
      return(data.frame(n_sets = integer(), predicted_tonnage = numeric())) # Return empty frame
    }

    # Calculate predictions
    predictions <- results %>%
      group_by(n_sets) %>%
      reframe({
        group_dates <- date
        group_tonnage <- max_tonnage

        # Filter data based on the selected regression start date
        if (!is.null(regression_start_date)) {
          valid_indices <- group_dates >= regression_start_date
          filtered_dates <- group_dates[valid_indices]
          filtered_tonnage <- group_tonnage[valid_indices]
        } else {
          filtered_dates <- group_dates
          filtered_tonnage <- group_tonnage
        }

        # Check if enough points *after filtering*
        if (length(filtered_dates) >= 2) {
          model_data <- data.frame(
            date_numeric = as.numeric(filtered_dates),
            max_tonnage = filtered_tonnage
          )
          model <- lm(max_tonnage ~ date_numeric, data = model_data)
          today_numeric <- as.numeric(Sys.Date())
          prediction_value <- predict(model, newdata = data.frame(date_numeric = today_numeric))

          # Return prediction for this group
          data.frame(predicted_tonnage = as.numeric(prediction_value)) # Only need prediction value
        } else {
          # Return NA if not enough data for prediction
          data.frame(predicted_tonnage = NA_real_)
        }
      }) %>%
      ungroup() # Ungroup after reframe

    return(predictions)
  })

  output$kehakaalPlot <- renderPlotly({
    p <- ggplot(health_log, aes(x = Date)) +
      geom_point(aes(y = BodyWeight), color = "purple", na.rm = TRUE) +
      geom_line(aes(y = BodyWeight_MA), na.rm = TRUE) +
      labs(title = "BodyWeight with 30-Day Moving Average", x = "Date", y = "BodyWeight")
    ggplotly(p)
  })

  output$maxTonnagePlot <- renderPlotly({
    results <- reactive_tonnage_results()
    regression_start_date <- input$regressionStartDate # Get the date for subtitle

    # Check if historical results are available
    if (is.null(results) || nrow(results) == 0) {
      return(plotly_empty(type = "scatter", mode = "markers") %>%
               layout(title = "No data available for this exercise"))
    }

    predicted_points_data <- reactive_predicted_tonnage()

    # Add date and n_sets back for plotting (if predictions exist)
    if (nrow(predicted_points_data) > 0) {
      # Ensure n_sets is present for joining/mapping color
      # Assuming reactive_predicted_tonnage returns rows ordered by n_sets 1 to 5
      # or includes n_sets directly (which it does now)
      predicted_points_plot <- predicted_points_data %>%
        filter(!is.na(predicted_tonnage)) %>% # Don't plot if prediction failed
        mutate(date = Sys.Date()) %>%
        rename(max_tonnage = predicted_tonnage) # Rename for aes consistency
    } else {
      predicted_points_plot <- data.frame() # Empty frame if no predictions
    }

    # --- Plotting ---
    p <- ggplot() +
      geom_line(data = results, aes(x = date, y = max_tonnage, color = as.factor(n_sets))) +
      geom_point(data = results, aes(x = date, y = max_tonnage, color = as.factor(n_sets))) +
      {if(nrow(predicted_points_plot) > 0) geom_point(data = predicted_points_plot,
                                                 aes(x = date, y = max_tonnage, color = as.factor(n_sets)),
                                                 shape = 17, size = 3)} +
      labs(
        title = "Max Tonnage Records (1-5 Sets) with Trend Prediction",
        subtitle = if (!is.null(regression_start_date)) paste("Regression based on data from", format(regression_start_date, "%Y-%m-%d"), "onwards") else "Regression based on all available data",
        x = "Date", y = "Max Tonnage", color = "Number of Sets"
      )

    ggplotly(p)
  })

  output$repTonnagePlot <- renderPlotly({
    req(input$exercise) # Ensure an exercise is selected

    # 1. Filter data for the selected exercise
    exercise_data <- merged_df %>%
      filter(name == input$exercise)

    # Handle case where there's no data
    if (nrow(exercise_data) == 0) {
      return(plotly_empty(type = "scatter", mode = "markers") %>%
               layout(title = paste("No data available for", input$exercise)))
    }

    # 2. Calculate effective_weight and single_set_tonnage for each set
    rep_tonnage_data <- exercise_data %>%
      mutate(
        effective_weight = (isBW * (bwMultiplier * BodyWeight_MA) + weight),
        single_set_tonnage = reps * effective_weight
      ) %>%
      filter(!is.na(single_set_tonnage) & !is.na(effective_weight)) # Ensure calculations are valid

    # Handle case where no valid tonnage could be calculated
    if (nrow(rep_tonnage_data) == 0) {
      return(plotly_empty(type = "scatter", mode = "markers") %>%
               layout(title = paste("Could not calculate tonnage for", input$exercise, "(check data)")))
    }

    # 3. Find the specific record (row) with the max tonnage for each rep count
    rep_tonnage_peaks <- rep_tonnage_data %>%
      group_by(reps) %>%
      # Keep the single row corresponding to the max tonnage for that rep count.
      # If there are ties (same max tonnage on different dates),
      # this keeps the one with the highest tonnage (which will be the same)
      # and slice_max breaks ties by keeping the first one encountered in the data.
      # To keep the *most recent* tie, you could arrange by date descending first
      # or add date to order_by in slice_max. Let's keep it simple for now.
      slice_max(order_by = single_set_tonnage, n = 1, with_ties = FALSE) %>%
      ungroup() %>% # Crucial to ungroup after slicing
      filter(reps > 0) %>% # Optional filter
      arrange(reps) # Order by reps for plotting line correctly

    # Handle case where filtering leaves no data
    if (nrow(rep_tonnage_peaks) == 0) {
      return(plotly_empty(type = "scatter", mode = "markers") %>%
               layout(title = paste("No valid rep records > 0 found for", input$exercise)))
    }

    # 4. Create the plot, mapping tooltip info to the 'text' aesthetic
    p <- ggplot(rep_tonnage_peaks, aes(x = reps, y = single_set_tonnage)) + # Y-axis is the max tonnage
      geom_line(color = "cyan", group = 1) +
      geom_point(
        aes(text = paste( # Define the tooltip text using paste()
          "Reps:", reps,
          "<br>Max Tonnage:", round(single_set_tonnage, 1), "kg", # Use the tonnage from the record row
          "<br>Effective Weight:", round(effective_weight, 1), "kg", # Use effective_weight from the record row
          "<br>Date:", format(date, "%Y-%m-%d") # Use date from the record row, formatted
        )),
        color = "cyan", size = 2.5
      ) +
      labs(
        title = paste("Historical Max Single-Set Tonnage per Rep Count:", input$exercise),
        subtitle = "Hover over points for weight and date details", # Updated subtitle
        x = "Repetitions per Set",
        y = "Max Historical Tonnage for this Rep Count (kg)"
      ) +
      scale_x_continuous(breaks = scales::pretty_breaks(n = max(1, min(10, nrow(rep_tonnage_peaks)))))

    # 5. Convert to Plotly, telling it to use the 'text' aesthetic for tooltips
    ggplotly(p, tooltip = "text")

  })

  output$calc_reps <- DT::renderDataTable({
    # Get historical results
    results <- reactive_tonnage_results()
    # Get predicted tonnage for today based on selected regression start date
    predictions <- reactive_predicted_tonnage() # This now returns n_sets and predicted_tonnage

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
        # Use input$exercise (synced) for consistency
        bw_multiplier_val = exercise_df$bwMultiplier[exercise_df$Exercise == input$exercise],
        bw_multiplier_val = ifelse(length(bw_multiplier_val) == 0, 0, bw_multiplier_val),
        latest_bw_ma = tail(merged_df$BodyWeight_MA, 1),
        adjusted_weight = input$calc_weight + bw_multiplier_val * latest_bw_ma,
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
      caption = input$exercise
    )
  })

  output$rawDataTable <- DT::renderDataTable({
    last_three_months <- Sys.Date() %m-% months(3)
    filtered_data <- merged_df %>%
      filter(date >= last_three_months) %>%
      select(date, name, reps, weight, BodyWeight_interpolated, BodyWeight_MA)
    filtered_data <- filtered_data[rev(seq_len(nrow(filtered_data))), ]
    DT::datatable(filtered_data)
  })
}