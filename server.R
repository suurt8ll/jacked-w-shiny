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

  # Sync the chosen exercise between graph and calc tabs
  observe({
    updateSelectInput(session, "exercise", selected = input$calc_exercise)
  })
  observe({
    updateSelectInput(session, "calc_exercise", selected = input$exercise)
  })

  observe({
    updateDateInput(session, "regressionStartDate", value = input$calc_regressionStartDate)
  })
  observe({
    updateDateInput(session, "calc_regressionStartDate", value = input$regressionStartDate)
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

  output$activityBarPlot <- renderPlotly({
    end_date <- Sys.Date()
    start_date <- switch(input$activity_date_range,
      "1m" = end_date %m-% months(1), "3m" = end_date %m-% months(3),
      "6m" = end_date %m-% months(6), "1y" = end_date %m-% years(1),
      "3y" = end_date %m-% years(3), "5y" = end_date %m-% years(5),
      "all" = min(health_log$Date, na.rm = TRUE), end_date %m-% months(3)
    )

    filtered_health_log <- health_log %>% filter(Date >= start_date & Date <= end_date)
    filtered_health_log$Week <- format(as.Date(filtered_health_log$Date), "%Y-%U")
    weekly_sums <- filtered_health_log %>% group_by(Week) %>% summarise(WeeklySum = sum(ActiveMinutes, na.rm = TRUE))

    p <- ggplot(weekly_sums, aes(x = Week, y = WeeklySum)) +
      geom_bar(stat = "identity", fill = "purple") +
      labs(title = "Weekly Active Minutes", x = "Week", y = "Total Active Minutes") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
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
      rownames = FALSE
    )
  })

  output$rawDataTable <- DT::renderDataTable({
    last_three_months <- Sys.Date() %m-% months(3)
    filtered_data <- merged_df %>%
      filter(date >= last_three_months) %>%
      select(date, name, reps, weight, BodyWeight, BodyWeight_MA)
    filtered_data <- filtered_data[rev(seq_len(nrow(filtered_data))), ]
    DT::datatable(filtered_data)
  })
}