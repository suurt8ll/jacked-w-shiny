#  ./R/plot_functions.R

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
      formula_str_lm <- paste("single_set_tonnage ~", formula_terms, "+ 0") # Force through origin

      tryCatch({
        # Fit the model using data including reps > 0
        regression_model <- lm(as.formula(formula_str_lm), data = rep_tonnage_peaks)

        # If model fits successfully, proceed to generate prediction data
        add_regression <- TRUE # Set flag to TRUE if lm call succeeds

        # Generate Prediction Data for Hover
        max_rep_plot <- max(rep_tonnage_peaks$reps)

        # 1. Generate a high-resolution sequence for smoothness (e.g., 200 points)
        smooth_reps <- seq(0, max_rep_plot, length.out = 200)
        # 2. Generate a sequence of integers within the range (including 0 for calculation)
        # Ensure max_rep_plot is at least 0 before flooring
        integer_reps <- seq(0, floor(max(0, max_rep_plot)), by = 1)
        # 3. Combine, remove duplicates, and sort
        pred_reps <- sort(unique(c(smooth_reps, integer_reps)))

        # Create newdata frame for prediction using the combined sequence
        newdata <- data.frame(reps = pred_reps)

        # Predict tonnage
        pred_tonnage <- predict(regression_model, newdata = newdata)
        # Ensure predicted tonnage at reps=0 is exactly 0 (it should be due to +0 in formula)
        # Find the index corresponding to reps=0 (robust to floating point issues)
        zero_rep_index <- which(abs(newdata$reps - 0) < 1e-9)
        if(length(zero_rep_index) > 0) {
            pred_tonnage[zero_rep_index] <- 0
        }
        # Prevent negative predicted tonnage (doesn't make physical sense)
        pred_tonnage[pred_tonnage < 0] <- 0

        # Calculate predicted effective weight: tonnage / reps
        # Handle division by zero for reps = 0
        pred_effective_weight <- ifelse(abs(newdata$reps - 0) < 1e-9, 0, pred_tonnage / newdata$reps)

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
    # We don't use Plotly directy because in this case syncting the Shiny theme with thematic does not work.
    p <- ggplot() +
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
      # NO any theme_*() method here, because it messes up the theme sync.

    # 5. Convert to Plotly
    pl <- ggplotly(p)

    # Add markers for actual max records
    if (nrow(rep_tonnage_peaks) > 0) {
      pl <- pl %>%
        add_trace(
          data = rep_tonnage_peaks,
          x = ~reps,
          y = ~single_set_tonnage,
          type = "scatter",
          mode = "markers",
          marker = list(color = "purple", size = 5),
          hoverinfo = "text",
          text = ~tooltip_text,
          name = "Max Records"
        )
    }

    # Add Regression Line Trace Manually to Plotly Object
    if (add_regression && !is.null(prediction_data)) {
        # Filter prediction data to only include reps >= 1 for plotting the line
        # This filter now acts on the combined sequence, ensuring rep=1 is the start if present
        prediction_data_for_plot <- prediction_data %>%
                                      filter(pred_reps >= 1) # Filter points for plotting

        # Check if there are still points to plot after filtering
        if (nrow(prediction_data_for_plot) > 0) {
            pl <- pl %>%
                add_trace(
                    data = prediction_data_for_plot, # Use the filtered data
                    x = ~pred_reps,
                    y = ~pred_tonnage,
                    type = "scatter",
                    mode = "lines",
                    line = list(color = "blue", dash = "dash"),
                    hoverinfo = "text",
                    text = ~hover_text,
                    name = sprintf("Prediction (Deg %d)", poly_degree)
                )
        } else {
            warning("No prediction points with reps >= 1 to plot for the regression line.")
        }
    }


    # 6. Final Layout Adjustments (no changes here needed)
    pl <- pl %>% layout(hovermode = "closest",
                        legend = list(x = 0.01, y = 0.99, bgcolor = "rgba(255,255,255,0.6)"))
    pl
}