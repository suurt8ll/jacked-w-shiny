render_rep_tonnage_plot <- function(exercise_name, poly_degree = 2) {
    # 1. Filter data for the selected exercise
    exercise_data <- merged_df %>%
      filter(name == exercise_name)

    # Handle case where there's no data
    if (nrow(exercise_data) == 0) {
      return(plotly_empty(type = "scatter", mode = "markers") %>%
               layout(title = paste("No data available for", exercise_name)))
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
               layout(title = paste("Could not calculate tonnage for", exercise_name, "(check data)")))
    }

    # 3. Find the specific record (row) with the max tonnage for each rep count
    rep_tonnage_peaks <- rep_tonnage_data %>%
      group_by(reps) %>%
      slice_max(order_by = single_set_tonnage, n = 1, with_ties = FALSE) %>%
      ungroup() %>%
      filter(reps > 0) %>% # Keep only reps > 0 for peak data
      arrange(reps) %>%
      # Add the custom tooltip text as a new column
      mutate(
        tooltip_text = paste(
          "Reps:", reps,
          "<br>Max Tonnage:", round(single_set_tonnage, 1), "kg",
          "<br>Effective Weight:", round(effective_weight, 1), "kg",
          "<br>Date:", format(date, "%Y-%m-%d")
        )
      )


    # Handle case where filtering leaves no data
    if (nrow(rep_tonnage_peaks) == 0) {
      return(plotly_empty(type = "scatter", mode = "markers") %>%
               layout(title = paste("No valid rep records > 0 found for", exercise_name)))
    }

    # Add Polynomial Regression through (0,0)
    add_regression <- FALSE # Flag to control adding the regression line
    regression_model <- NULL # Initialize model variable
    prediction_data <- NULL # Initialize prediction data frame

    if (nrow(rep_tonnage_peaks) > poly_degree) {
      # Construct the formula string for lm (using actual column names)
      formula_terms <- paste0("I(reps^", 1:poly_degree, ")", collapse = " + ")
      formula_str_lm <- paste("single_set_tonnage ~", formula_terms, "+ 0")

      tryCatch({
        regression_model <- lm(as.formula(formula_str_lm), data = rep_tonnage_peaks)

        # If model fits successfully, proceed to generate prediction data
        add_regression <- TRUE # Set flag to TRUE if lm call succeeds

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

    # 4. Create the BASE plot in ggplot
    #    - Include the (0,0) point
    #    - Include the line connecting the actual max points
    #    - DO NOT include geom_point for rep_tonnage_peaks with aes(text=...) here
    p <- ggplot() +
      geom_point(data = data.frame(reps = 0, single_set_tonnage = 0), # Add (0,0) point
                 aes(x = reps, y = single_set_tonnage),
                 color = "grey", size = 2, alpha = 0.5, inherit.aes = FALSE) + # Use inherit.aes=FALSE
      geom_line(data = rep_tonnage_peaks, aes(x = reps, y = single_set_tonnage), # Line connecting actual max points
                color = "purple", group = 1) +
      labs(
        title = paste("Historical Max Single-Set Tonnage per Rep Count:", exercise_name),
        subtitle = "Hover over points for details, hover over line for predictions",
        x = "Repetitions per Set",
        y = "Max Historical Tonnage for this Rep Count (kg)"
      ) +
      scale_x_continuous(breaks = scales::pretty_breaks(n = max(1, min(10, nrow(rep_tonnage_peaks) + 1))),
                         limits = c(0, NA)) + # Ensure x-axis starts at 0
      scale_y_continuous(limits = c(0, NA)) # Ensure y-axis starts at 0

    # 5. Convert to Plotly
    # Do NOT specify tooltip = "text" here. Plotly will infer tooltips for the line
    # and the (0,0) point based on mapped aesthetics (x, y).
    pl <- ggplotly(p)

    # Use the rep_tonnage_peaks data with the new tooltip_text column
    if (nrow(rep_tonnage_peaks) > 0) {
      pl <- pl %>%
        add_trace(
          data = rep_tonnage_peaks,
          x = ~reps,
          y = ~single_set_tonnage,
          type = "scatter",
          mode = "markers", # Add markers
          marker = list(color = "purple", size = 5), # Match original point style
          hoverinfo = "text", # Tell plotly to use the 'text' argument for hover
          text = ~tooltip_text, # Use the pre-calculated tooltip text column
          name = "Max Records" # Name for legend
        )
    }

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
}