#---- Packages and Functions ----
library("shiny")
library("dplyr")
library("tidyr")
library("stringi")
library("zoo")
library("ggplot2")
library("scales")
library("plotly")
library("lubridate")
library("DT")
library("here")
library("bslib")
library("thematic")
library("readr")

# Change R plot theming defaults for all the plots generated in the app.
thematic_shiny()

# Functions
# Calculate max tonnage for `n` sets
# NOTE: This function expects the input data frame to already have a 'tonnage' column.
# Tonnage calculation (weight * reps, adjusted for BW exercises) should happen
# before calling this function, likely in server.R.
calculate_max_tonnage <- function(data, n) {
  data %>%
    group_by(date) %>%
    summarise(
      # Check if there are at least 'n' sets for the date before attempting combination
      max_tonnage = if (n > n()) NA else max(apply(combn(tonnage, n), 2, sum, na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    # Filter out dates where max_tonnage is NA (because there weren't enough sets)
    filter(!is.na(max_tonnage))
}

#---- Parameters ----
# File paths (now using CSVs)
sets_csv_path <- here("data", "sets.csv")
exercise_definitions_csv_path <- here("data", "exercise_definitions.csv")

# Rolling average window size
rolling_window <- 30

#---- Data loading ----

# TODO User should have an option to reload data directly in app without needing to restart the whole program.
# ^ Hint: this can be done separately in bash script, user needs to press a key to refresh.

# Load data from CSV files using readr
sets_raw <- read_csv(sets_csv_path)
exercise_df <- read_csv(exercise_definitions_csv_path)

#---- Data Cleaning and Transformations ----

# Separate workout sets and bodyweight entries from sets_raw
# Assuming 'Weight' in the 'name' column indicates a bodyweight entry
workout_sets_raw <- sets_raw %>% filter(name != "Weight")
bodyweight_raw <- sets_raw %>% filter(name == "Weight")

# Process Bodyweight Data for health_log
health_log <- bodyweight_raw %>%
  select(created, BodyWeight = weight) %>% # Select relevant columns and rename weight to BodyWeight
  mutate(Date = as.Date(created)) %>%      # Convert created timestamp to Date
  # Handle potential multiple BW entries per day - keep the first one for simplicity
  distinct(Date, .keep_all = TRUE) %>%
  select(Date, BodyWeight) %>%             # Keep only Date and BodyWeight
  arrange(Date)                            # Arrange by date for interpolation/rolling average

# Create a sequence of all dates from the first bodyweight entry to today
# Handle case where bodyweight_raw might be empty
start_date <- if(nrow(health_log) > 0) min(health_log$Date) else Sys.Date()
all_dates <- seq(from = start_date, to = Sys.Date(), by = "day")

# Create a new dataframe with all dates and merge it with health_log
# This ensures all dates are present, with NAs for missing BW entries
health_log <- data.frame(Date = all_dates) %>%
  left_join(health_log, by = "Date")

# Carry last body weight forward to current date if the last entry is NA
# This replicates the original logic
if (nrow(health_log) > 0 && is.na(tail(health_log$BodyWeight, n = 1))) {
  last_non_na_bw <- tail(na.trim(health_log$BodyWeight), n = 1)
  # Only fill if there was a previous non-NA value found
  if (length(last_non_na_bw) > 0 && !is.na(last_non_na_bw)) {
    health_log$BodyWeight[nrow(health_log)] <- last_non_na_bw
  }
}

# Interpolate missing bodyweight data and calculate moving average
# na.approx requires sorted data. Use rule=2 to carry the last observation forward.
health_log$BodyWeight_interpolated <- na.approx(health_log$BodyWeight, na.rm = FALSE, rule = 2)
health_log$BodyWeight_MA <- rollapply(
  health_log$BodyWeight_interpolated,
  width = rolling_window,
  FUN = mean,
  fill = NA,
  align = "right",
  na.rm = TRUE # na.rm=TRUE is safe after interpolation
)
# Ensure MA is numeric (rollapply can sometimes return a matrix)
health_log$BodyWeight_MA <- as.numeric(health_log$BodyWeight_MA)


# Process Exercise Definitions
# Fix missing bodyweight multipliers (same logic as before)
exercise_df$bwMultiplier <- ifelse(is.na(exercise_df$bwMultiplier), 0, exercise_df$bwMultiplier)


# Process Workout Data and Merge
workout_data <- workout_sets_raw %>%
  select(name, reps, weight, created) %>% # Select relevant columns
  mutate(date = as.Date(created)) %>%      # Convert created timestamp to Date, rename to 'date'
  select(-created)                         # Remove original created POSIXct column

# Merge workout data with exercise definitions and health log data
merged_df <- workout_data %>%
  left_join(exercise_df, by = c("name" = "Exercise")) %>%
  # Join with health_log on date, bringing in interpolated and MA bodyweight
  left_join(health_log %>% select(Date, BodyWeight_interpolated, BodyWeight_MA), by = c("date" = "Date"))

# Remove rows where weight or reps is missing (same logic as before)
merged_df <- merged_df %>%
  filter(!is.na(weight) & !is.na(reps))

# FIXME Exercises not in exercise_df should be omitted.
# The left_join results in NA values for exercise_df columns if the exercise name
# from sets.csv is not found in exercise_definitions.csv.
# We can filter these out based on a key column from exercise_df, e.g., 'isBW'.
merged_df <- merged_df %>%
  filter(!is.na(isBW))


# Clean up temporary data frames
rm("sets_raw", "workout_sets_raw", "bodyweight_raw", "workout_data")