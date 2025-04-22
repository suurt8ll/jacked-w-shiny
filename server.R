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
  # TODO maybe two input fields can share the same variable name? This would be a cleaner solution.
  # Sync the choses exercise between graph and calc tabs
  observe({
    updateSelectInput(session, "exercise", selected = input$calc_exercise)
  })
  # Sync the choses exercise between graph and calc tabs
  observe({
    updateSelectInput(session, "calc_exercise", selected = input$exercise)
  })

  reactive_tonnage_results <- reactive({
    req(input$exercise) # Ensure input$exercise is available

    exercise_data <- merged_df %>%
      filter(name == input$exercise) %>%
      mutate(tonnage = reps * (isBW * (bwMultiplier * BodyWeight_MA) + weight)) %>%
      select(date, tonnage)

    # Handle case where exercise_data might be empty
    if (nrow(exercise_data) == 0) {
      return(NULL) # Return an empty structure that downstream code can handle.
    }

    lapply(1:5, function(n) {
      calculate_max_tonnage(exercise_data, n) %>%
        mutate(n_sets = n)
    }) %>%
      bind_rows()
  })

  output$kehakaalPlot <- renderPlotly({
    p <- ggplot(health_log, aes(x = Date)) +
      geom_point(aes(y = BodyWeight), color = "purple", na.rm = TRUE) +
      geom_line(aes(y = BodyWeight_MA), na.rm = TRUE) +
      labs(title = "BodyWeight with 30-Day Moving Average", x = "Date", y = "BodyWeight")
    ggplotly(p)
  })

  output$activityBarPlot <- renderPlotly({
    # Determine the start date based on selected range
    end_date <- Sys.Date()
    start_date <- switch(input$activity_date_range,
      "1m" = end_date %m-% months(1),
      "3m" = end_date %m-% months(3),
      "6m" = end_date %m-% months(6),
      "1y" = end_date %m-% years(1),
      "3y" = end_date %m-% years(3),
      "5y" = end_date %m-% years(5),
      "all" = min(health_log$Date, na.rm = TRUE),
      end_date %m-% months(3)  # Default fallback is now "Past 3 Months"
    )

    # Filter health_log based on date range
    filtered_health_log <- health_log %>%
      filter(Date >= start_date & Date <= end_date)

    # Create a new column for the week number
    filtered_health_log$Week <- format(as.Date(filtered_health_log$Date), "%Y-%U")

    # Summarize the data to get weekly sums
    weekly_sums <- filtered_health_log %>%
      group_by(Week) %>%
      summarise(WeeklySum = sum(ActiveMinutes, na.rm = TRUE))

    # Plot the data using ggplot2
    p <- ggplot(weekly_sums, aes(x = Week, y = WeeklySum)) +
      geom_bar(stat = "identity", fill = "purple") +
      labs(title = "Weekly Active Minutes", x = "Week", y = "Total Active Minutes") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))

    ggplotly(p)
  })

  output$maxTonnagePlot <- renderPlotly({
    # Get the pre-calculated results from the reactive expression
    results <- reactive_tonnage_results()

    # Check if results are available (e.g., if exercise_data was empty)
    if (is.null(results) || nrow(results) == 0) {
      # Return an empty plot or a message if no data
      # Keep the plotly_empty part as it's a plotly object already
      return(plotly_empty(type = "scatter", mode = "markers") %>%
               layout(title = "No data available for this exercise"))
    }

    p <- ggplot(data = results, aes(x = date, y = max_tonnage)) +
      # Map color to n_sets (as a factor) for lines and points
      geom_line(aes(color = as.factor(n_sets))) +
      geom_point(aes(color = as.factor(n_sets))) +
      # Add titles and labels
      labs(
        title = "Max Tonnage Records for 1-5 Sets",
        x = "Date",
        y = "Max Tonnage",
        color = "Number of Sets" # Set the legend title for the color aesthetic
      )
    ggplotly(p)
  })

  output$calc_reps <- DT::renderDataTable({
    # Get the pre-calculated results from the reactive expression
    results <- reactive_tonnage_results()

    # Check if results are available
    if (is.null(results) || nrow(results) == 0) {
      return(DT::datatable(data.frame(`Number of Sets` = integer(0),
                                      `Max Tonnage` = integer(0),
                                      `Last Tonnage` = integer(0),
                                      `Reps to Beat PR` = integer(0),
                                      `Reps to Beat Last` = integer(0)),
                           options = list(pageLength = 5),
                           rownames = FALSE))
    }

    # Continue with the calculation specific to the table
    table_data <- results %>%
      group_by(n_sets) %>%
      summarise(
        maxTonnage = max(max_tonnage),
        # This assumes results are ordered by date within each n_sets group, which calculate_max_tonnage does.
        lastTonnage = tail(max_tonnage, 1)
      ) %>%
      mutate(
        # Ensure input$calc_exercise is available for lookup
        # Use the synced input$exercise for consistency with the reactive
        bw_multiplier_val = exercise_df$bwMultiplier[exercise_df$Exercise == input$exercise],
        # Handle cases where exercise might not be in exercise_df
        bw_multiplier_val = ifelse(length(bw_multiplier_val) == 0, 0, bw_multiplier_val),
        # Use the latest BodyWeight_MA from the full merged_df as before
        latest_bw_ma = tail(merged_df$BodyWeight_MA, 1),
        adjusted_weight = input$calc_weight + bw_multiplier_val * latest_bw_ma,
        # Handle division by zero if adjusted_weight is 0 or NA
        repsPR = ifelse(adjusted_weight == 0 | is.na(adjusted_weight), NA, maxTonnage / adjusted_weight),
        repsBeatPrev = ifelse(adjusted_weight == 0 | is.na(adjusted_weight), NA, lastTonnage / adjusted_weight)
      ) %>%
      mutate(
        # Apply ceiling logic, handling potential NAs from division
        repsPR = as.integer(ifelse(is.na(repsPR), NA, floor(repsPR) + 1)),
        repsBeatPrev = as.integer(ifelse(is.na(repsBeatPrev), NA, floor(repsBeatPrev) + 1)),
        maxTonnage = as.integer(round(maxTonnage)),
        lastTonnage = as.integer(round(lastTonnage))
      ) %>%
      select(
        `Number of Sets` = n_sets,
        `Max Tonnage` = maxTonnage,
        `Last Tonnage` = lastTonnage,
        `Reps to Beat PR` = repsPR,
        `Reps to Beat Last` = repsBeatPrev
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
    # Reverse the order of the rows to flip the data frame
    filtered_data <- filtered_data[rev(seq_len(nrow(filtered_data))), ]
    DT::datatable(filtered_data)
  })
}