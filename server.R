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

  output$kehakaalPlot <- renderPlotly({
    p <- ggplot(health_log, aes(x = Date)) +
      geom_point(aes(y = BodyWeight), color = "grey", na.rm = TRUE) +
      geom_line(aes(y = BodyWeight_MA),
                color = "black",
                na.rm = TRUE) +
      labs(title = "BodyWeight with 30-Day Moving Average", x = "Date", y = "BodyWeight") +
      theme_minimal()
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
      geom_bar(stat = "identity", fill = "grey") +
      labs(title = "Weekly Active Minutes", x = "Week", y = "Total Active Minutes") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))

    ggplotly(p)
  })

  output$maxTonnagePlot <- renderPlotly({
    # Calculate tonnage per set
    exercise_data <- merged_df %>%
      filter(name == input$exercise) %>%
      mutate(tonnage = reps * (isBW * (bwMultiplier * BodyWeight_MA) + weight)) %>%
      select(date, tonnage)

    # Generate results for multiple `n` values
    results <- lapply(1:5, function(n) {
      calculate_max_tonnage(exercise_data, n) %>%
        mutate(n_sets = n)
    }) %>%
      bind_rows()

    # Create an interactive plot for multiple values of `n`
    plot_ly(
      data = results,
      x = ~date,
      y = ~max_tonnage,
      color = ~as.factor(n_sets), # Use `n_sets` as the grouping variable
      type = "scatter",
      mode = "lines+markers"
    ) %>%
      layout(
        title = "Max Tonnage Records for 1-5 Sets",
        xaxis = list(title = "Date"),
        yaxis = list(title = "Max Tonnage"),
        legend = list(title = list(text = "Number of Sets"))
      )
  })

  output$calc_reps <- DT::renderDataTable({
    # Existing calculation logic remains the same
    exercise_data <- merged_df %>%
      filter(name == input$calc_exercise) %>%
      mutate(tonnage = reps * (isBW * (bwMultiplier * BodyWeight_MA) + weight)) %>%
      select(date, tonnage)

    # Generate results for multiple `n` values
    results <- lapply(1:5, function(n) {
      calculate_max_tonnage(exercise_data, n) %>%
        mutate(n_sets = n)
    }) %>%
      bind_rows() %>%
      group_by(n_sets) %>%
      summarise(
        maxTonnage = max(max_tonnage),
        lastTonnage = tail(max_tonnage, 1)
      ) %>%
      mutate(
        adjusted_weight = input$calc_weight +
          exercise_df$bwMultiplier[exercise_df$Exercise == input$calc_exercise] *
            tail(merged_df$BodyWeight_MA, 1),
        repsPR = maxTonnage / adjusted_weight,
        repsBeatPrev = lastTonnage / adjusted_weight
      ) %>%
      mutate(
        repsPR = as.integer(ifelse(repsPR %% 1 == 0, repsPR + 1, ceiling(repsPR))),
        repsBeatPrev = as.integer(ifelse(repsBeatPrev %% 1 == 0, repsBeatPrev + 1, ceiling(repsBeatPrev))),
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
      results,
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