#---- Shiny server ----
server <- function(input, output, session) {
  # Filter the exercise choice list based on chosen date AND sort by recency
  observe({
    # 1. Calculate the last performed date for ALL exercises
    # Ensure 'date' column is Date type if not already
    exercise_last_performed <- merged_df %>%
      filter(!is.na(date)) %>% # Avoid issues if date is NA
      group_by(name) %>%
      summarise(last_date = max(date, na.rm = TRUE), .groups = 'drop') %>%
      arrange(desc(last_date)) # Sort by most recent date first

    # 2. Get the globally sorted list of exercise names
    sorted_exercise_names <- exercise_last_performed$name

    # 3. Determine the final list based on date filter
    final_exercise_list <- if (length(input$date) != 0 && !is.na(input$date)) {
      # Get unique exercises performed on the selected date
      exercises_on_date <- merged_df %>%
        filter(date == input$date) %>%
        pull(name) %>%
        unique()
      # Filter the globally sorted list, keeping only those performed on the selected date,
      # while maintaining the global recency order.
      sorted_exercise_names[sorted_exercise_names %in% exercises_on_date]
    } else {
      # If no date is selected or it's NA, use the full globally sorted list
      sorted_exercise_names
    }

    # 4. Update the select input for the Tonnage tab
    # Use the calculated 'final_exercise_list' for choices
    updateSelectInput(session, "exercise", choices = final_exercise_list)
  })

  # Reactive expression for historical max tonnage results
  reactive_tonnage_results <- reactive({
    req(input$exercise) # Ensure input$exercise is available

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
    req(input$exercise)

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
      slice_max(order_by = single_set_tonnage, n = 1, with_ties = FALSE) %>%
      ungroup() %>%
      filter(reps > 0) %>% # Keep only reps > 0 for peak data
      arrange(reps)

    # Handle case where filtering leaves no data
    if (nrow(rep_tonnage_peaks) == 0) {
      return(plotly_empty(type = "scatter", mode = "markers") %>%
               layout(title = paste("No valid rep records > 0 found for", input$exercise)))
    }

    # Add Polynomial Regression through (0,0)
    poly_degree <- 2
    add_regression <- FALSE
    r_squared_text <- ""
    regression_model <- NULL # Initialize model variable
    prediction_data <- NULL # Initialize prediction data frame

    if (nrow(rep_tonnage_peaks) > poly_degree) {
      # Construct the formula string for lm (using actual column names)
      formula_terms <- paste0("I(reps^", 1:poly_degree, ")", collapse = " + ")
      formula_str_lm <- paste("single_set_tonnage ~", formula_terms, "+ 0")

      tryCatch({
        regression_model <- lm(as.formula(formula_str_lm), data = rep_tonnage_peaks)

        # Calculate R-squared (0,0) manually
        ssr <- sum(residuals(regression_model)^2)
        tss_00 <- sum(rep_tonnage_peaks$single_set_tonnage^2)

        if (tss_00 > 0) {
          r_squared <- 1 - (ssr / tss_00)
          r_squared_text <- sprintf("R\u00B2 (0,0) = %.2f (Degree %d)", r_squared, poly_degree)
          add_regression <- TRUE

          # Generate Prediction Data for Hover
          # Create a sequence of reps for prediction, starting from 0
          max_rep_plot <- max(rep_tonnage_peaks$reps)
          pred_reps <- seq(0, max_rep_plot, length.out = 100) # Generate 100 points for a smooth line

          # Create newdata frame for prediction
          newdata <- data.frame(reps = pred_reps)

          # Predict tonnage
          pred_tonnage <- predict(regression_model, newdata = newdata)
          # Ensure predicted tonnage at reps=0 is exactly 0 (it should be due to +0 in formula)
          pred_tonnage[newdata$reps == 0] <- 0
          # Prevent negative predicted tonnage (doesn't make physical sense)
          pred_tonnage[pred_tonnage < 0] <- 0

          # Calculate predicted effective weight: tonnage / reps
          # Handle division by zero for reps = 0
          pred_effective_weight <- ifelse(newdata$reps == 0, 0, pred_tonnage / newdata$reps)

          # Create the hover text
          hover_texts <- paste(
            "Predicted Reps:", round(pred_reps, 2),
            "<br>Predicted Tonnage:", round(pred_tonnage, 1), "kg",
            "<br>Predicted Eff. Weight:", round(pred_effective_weight, 1), "kg"
          )

          # Store prediction data
          prediction_data <- data.frame(
            pred_reps = pred_reps,
            pred_tonnage = pred_tonnage,
            hover_text = hover_texts
          )
        } else {
          warning("Total Sum of Squares is zero, cannot calculate R-squared (0,0).")
        }

      }, error = function(e) {
        warning("Could not fit regression model: ", e$message)
        add_regression <- FALSE # Ensure flag is false on error
      })
    } else {
      warning(paste("Not enough data points (",
                    nrow(rep_tonnage_peaks),
                    ") for polynomial degree ",
                    poly_degree,
                    ". Skipping regression.", sep = ""))
    }

    # 4. Create the BASE plot (without geom_smooth)
    p <- ggplot(rep_tonnage_peaks, aes(x = reps, y = single_set_tonnage)) +
      geom_point(data = data.frame(reps = 0, single_set_tonnage = 0), # Add (0,0) point
                 aes(x = reps, y = single_set_tonnage),
                 color = "grey", size = 2, alpha = 0.5, inherit.aes = FALSE) + # Use inherit.aes=FALSE
      geom_line(color = "purple", group = 1) + # Line connecting actual max points
      geom_point( # Actual max points
        aes(text = paste( # Tooltip for actual points
          "Reps:", reps,
          "<br>Max Tonnage:", round(single_set_tonnage, 1), "kg",
          "<br>Effective Weight:", round(effective_weight, 1), "kg",
          "<br>Date:", format(date, "%Y-%m-%d")
        )),
        color = "purple", size = 2.5
      ) +
      labs(
        title = paste("Historical Max Single-Set Tonnage per Rep Count:", input$exercise),
        subtitle = "Hover over points for details, hover over line for predictions",
        x = "Repetitions per Set",
        y = "Max Historical Tonnage for this Rep Count (kg)"
      ) +
      scale_x_continuous(breaks = scales::pretty_breaks(n = max(1, min(10, nrow(rep_tonnage_peaks) + 1))),
                         limits = c(0, NA)) + # Ensure x-axis starts at 0
      scale_y_continuous(limits = c(0, NA)) # Ensure y-axis starts at 0

    # Add R-squared Text Annotation if calculated
    # This annotation is added to the ggplot object before conversion
    if (add_regression) {
      p <- p +
        annotate("text",
                 # Position near origin
                 x = 0,
                 # Position near top, relative to max data OR prediction
                 y = max(rep_tonnage_peaks$single_set_tonnage,
                         max(prediction_data$pred_tonnage, na.rm = TRUE)) * 0.95,
                 label = r_squared_text,
                 hjust = 0, vjust = 1, # Align text to be top-left
                 color = "white",
                 size = 3.5)
    }

    # 5. Convert to Plotly, telling it to use the 'text' aesthetic for the points
    # The base ggplot object 'p' no longer contains geom_smooth
    pl <- ggplotly(p, tooltip = "text")

    # Add Regression Line Trace Manually to Plotly Object
    if (add_regression && !is.null(prediction_data)) {
      pl <- pl %>%
        add_trace(
          data = prediction_data, # Use the generated prediction data
          x = ~pred_reps,
          y = ~pred_tonnage,
          type = "scatter",
          mode = "lines",
          line = list(color = "blue", dash = "dash"), # Style the line
          hoverinfo = "text", # Use the text specified below for hover
          text = ~hover_text, # Assign the pre-formatted hover text
          name = sprintf("Prediction (Deg %d)", poly_degree) # Name for legend
        )
    }

    # 6. Final Layout Adjustments
    pl <- pl %>% layout(hovermode = "closest",
                        legend = list(x = 0.01, y = 0.99, bgcolor = "rgba(255,255,255,0.6)")) # Position legend
    pl

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