#---- Packages and functions ----
required_libs <- c(
  "shiny",
  "shinydashboard",
  "dplyr",
  "tidyr",
  "readODS",
  "stringi",
  "zoo",
  "ggplot2",
  "scales",
  "plotly",
  "TTR",
  "lubridate"
)
# Install missing libraries.
for (lib in required_libs) {
  if (!(lib %in% installed.packages()[, "Package"])) {
    install.packages(lib)
  }
}
# Load all libraries.
lapply(required_libs, require, character.only = TRUE)

# Functions
calculate_max_tonnage <- function(exercise, tonnage_criteria, window="d", fill_missing=FALSE) {
  #TODO Implement the ability to choose a window length (daily, weekly, monthly, yearly).
  #TODO Boolean for telling the function if it should fill in the missing values or not.
  # Select the exercise and calculate tonnage for each set.
  exercise_data <- merged_df %>%
    filter(Exercise == exercise) %>%
    mutate(
      T1 = R1 * (isBW * (bwMultiplier * BodyWeight_MA) + W1),
      T2 = R2 * (isBW * (bwMultiplier * BodyWeight_MA) + W2),
      T3 = R3 * (isBW * (bwMultiplier * BodyWeight_MA) + W3),
      T4 = R4 * (isBW * (bwMultiplier * BodyWeight_MA) + W4),
      T5 = R5 * (isBW * (bwMultiplier * BodyWeight_MA) + W5)
    ) %>%
    select(Date, Exercise, T1, T2, T3, T4, T5)
  
  # Determine the maximum tonnage for each date dynamically.
  max_tonnage_data <- exercise_data %>%
    rowwise() %>%
    mutate(TonnageCriteria = tonnage_criteria, MaxTonnage = {
      sets <- c(T1, T2, T3, T4, T5)
      # Removes the "max" part of the string and converts to numeric.
      criteria <- as.numeric(gsub("max", "", tonnage_criteria))
      
      # Filter out NA values and check the number of valid sets
      valid_sets <- sets[!is.na(sets)]
      if (length(valid_sets) < criteria) {
        NA
      } else {
        if (criteria == 1) {
          max(valid_sets, na.rm = TRUE)
        } else {
          max(apply(combn(valid_sets, criteria), 2, sum, na.rm = TRUE), na.rm = TRUE)
        }
      }
    }) %>%
    ungroup() %>%
    filter(!is.na(MaxTonnage)) %>%
    select(Date, TonnageCriteria, MaxTonnage)
}

#---- Data loading ----

# Load data from Libreoffice Calc file
training_log <- read_ods(path = "./fitness-log.ods", sheet = "TrainingLog")
health_log <- read_ods(path = "./fitness-log.ods", sheet = "HealthLog")
exercise_df <- read_ods(path = "./fitness-log.ods", sheet = "ExerciseDatabase")

# Andmete korrastamine
health_log$Date <- as.Date(health_log$Date, "%m/%d/%y")
training_log$Date <- as.Date(training_log$Date, "%m/%d/%y")
# If exercise is not body-weight then multiplier is missing, convert these to 0.
exercise_df$bwMultiplier <- ifelse(is.na(exercise_df$bwMultiplier),
                                            0,
                                            exercise_df$bwMultiplier)

# Greasy hack, et ma näeks numbreid, kui pole kaua aega kaalnunud
health_log$BodyWeight[nrow(health_log)] <- ifelse(is.na(tail(health_log$BodyWeight, n = 1)),
                                                  tail(na.trim(health_log$BodyWeight), n = 1),
                                                  tail(health_log$BodyWeight, n = 1))
# Use rollapply to calculate the moving average, ignoring NA values
health_log$BodyWeight_interpolated <- na.approx(health_log$BodyWeight, na.rm = FALSE)
health_log$BodyWeight_MA <- rollapply(
  health_log$BodyWeight_interpolated,
  width = 30,
  FUN = mean,
  fill = NA,
  align = "right"
)

# Tekita üks suur juicy dataframe, mis hoomab kõike.
merged_df <- training_log %>%
  left_join(exercise_df, by = "Exercise") %>%
  left_join(health_log, by = "Date")

#--- Graphs ----

# Plot the data using ggplot2
p <- ggplot(health_log, aes(x = Date)) +
  geom_point(aes(y = BodyWeight), color = "grey", na.rm = TRUE) +
  geom_line(aes(y = BodyWeight_MA),
            color = "black",
            na.rm = TRUE) +
  labs(title = "BodyWeight with 30-Day Moving Average", x = "Date", y = "BodyWeight") +
  theme_minimal()
ggplotly(p)


# Create a new column for the week number
health_log$Week <- format(as.Date(health_log$Date), "%Y-%U")

# Summarize the data to get weekly sums
weekly_sums <- health_log %>%
  group_by(Week) %>%
  summarise(WeeklySum = sum(ActiveMinutes, na.rm = TRUE))

# Plot the data using ggplot2
p <- ggplot(weekly_sums, aes(x = Week, y = WeeklySum)) +
  geom_bar(stat = "identity", fill = "grey") +
  labs(title = "Weekly Active Minutes", x = "Week", y = "Total Active Minutes") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggplotly(p)

# Rough logic for the dynamic drop-down menu
date <- as.Date("2024-07-04")
if (length(date) != 0) {
  # Limit choices to the given date's exercises.
  exercise_list <- training_log %>% filter(Date == date) %>% select(Exercise)
  exercise_list <- unique(exercise_list[["Exercise"]])
} else {
  # Whole exercise list gets used if no date is chosen.
  exercise_list <- unique(training_log$Exercise)
}

# This will plot a graph of tonnage records
exercise <- "Pull-up"
# max_tonnage_data_long <- data.frame()
# for (tonnage_criteria in c("max1", "max2", "max3", "max4", "max5")) {
#   max_tonnage_data_long <- rbind(
#     max_tonnage_data_long,
#     calculate_max_tonnage(exercise = exercise, tonnage_criteria = tonnage_criteria)
#   )
# }

#TODO Implement the ability to choose a window length (daily, weekly, monthly, yearly).
#TODO Boolean for telling the function if it should fill in the missing values or not.
selected_window = "w"
date_format <- ifelse(selected_window == "w", "%G-W%V", "%y-%m-%d")
# Select the exercise and calculate tonnage for each set.
exercise_data <- merged_df %>%
  filter(Exercise == exercise) %>%
  mutate(
    ISOWeek = format(Date, date_format),
    T1 = R1 * (isBW * (bwMultiplier * BodyWeight_MA) + W1),
    T2 = R2 * (isBW * (bwMultiplier * BodyWeight_MA) + W2),
    T3 = R3 * (isBW * (bwMultiplier * BodyWeight_MA) + W3),
    T4 = R4 * (isBW * (bwMultiplier * BodyWeight_MA) + W4),
    T5 = R5 * (isBW * (bwMultiplier * BodyWeight_MA) + W5),
  ) %>%
  select(Date, ISOWeek, Exercise, T1, T2, T3, T4, T5)
# Calculate total tonnage for the session, needed for choosing the best session in the future.
exercise_data$SUM_T <- rowSums(exercise_data[, c("T1", "T2", "T3", "T4", "T5")], na.rm = TRUE)

# Determine the maximum tonnage for each date dynamically.
tonnage_criteria = "max1"
max_tonnage_data <- exercise_data %>%
  rowwise() %>%
  mutate(TonnageCriteria = tonnage_criteria, MaxTonnage = {
    sets <- c(T1, T2, T3, T4, T5)
    # Removes the "max" part of the string and converts to numeric.
    criteria <- as.numeric(gsub("max", "", tonnage_criteria))
    
    # Filter out NA values and check the number of valid sets
    valid_sets <- sets[!is.na(sets)]
    if (length(valid_sets) < criteria) {
      NA
    } else {
      if (criteria == 1) {
        max(valid_sets, na.rm = TRUE)
      } else {
        max(apply(combn(valid_sets, criteria), 2, sum, na.rm = TRUE), na.rm = TRUE)
      }
    }
  }) %>%
  ungroup() %>%
  filter(!is.na(MaxTonnage)) %>%
  select(Date, TonnageCriteria, MaxTonnage)

## Following is largely generated by llama-3.1-405b-instruct-fp8.

# Convert Date column to a date object
exercise_data$Date <- as.Date(exercise_data$Date)
# Create a sequence of dates from the minimum to maximum date
date_seq <- seq(min(exercise_data$Date), max(exercise_data$Date), by = "day")
# Merge the date sequence with the exercise data
exercise_data_filled <- merge(data.frame(Date = date_seq), exercise_data, all.x = TRUE)
# Fill in missing values using LOCF
exercise_data_filled <- exercise_data_filled %>%
  arrange(Date) %>%
  mutate(
    SUM_T = zoo::na.locf(SUM_T, na.rm = FALSE),
    T1 = zoo::na.locf(T1, na.rm = FALSE),
    T2 = zoo::na.locf(T2, na.rm = FALSE),
    T3 = zoo::na.locf(T3, na.rm = FALSE),
    T4 = zoo::na.locf(T4, na.rm = FALSE),
    T5 = zoo::na.locf(T5, na.rm = FALSE),
    EMA_T3 = EMA(T3, n = 180)
  )

p <- ggplot(exercise_data_filled, aes(x = Date)) +
  geom_line(aes(y = T3)) +
  geom_line(aes(y = EMA_T3), color = "red")
p

## End of LLM generated code.

p <- ggplot(exercise_data, aes(x = Date, y = T2)) +
  geom_line() +
  geom_point(size = 1)
p

p <- ggplot(exercise_data_filled, aes(x = Date, y = T2)) +
  geom_line() +
  geom_point(size = 1) #+
  #scale_color_brewer() +
  #scale_y_log10() +
  #theme_dark()
p
ggplotly(p)

p <- ggplot(max_tonnage_data_long,
            aes(x = Date, y = MaxTonnage, colour = TonnageCriteria)) +
  geom_line() +
  geom_point(size = 1) +
  scale_color_brewer() +
  scale_y_log10() +
  theme_dark()
ggplotly(p)

# For EMA calculations we need weekly data points. Let's simply fill missing
# tonnage data with last known value.

calc_weight <- 0
max_tonnage_data_long %>%
  group_by(TonnageCriteria) %>%
  summarise(
    maxTonnage = max(MaxTonnage),
    lastTonnage = tail(MaxTonnage, 1)
  ) %>%
  mutate(repsPR = maxTonnage / (calc_weight + exercise_df$bwMultiplier[exercise_df$Exercise == exercise] * tail(merged_df$BodyWeight_MA, 1)),
          repsBeatPrev = lastTonnage / (calc_weight + exercise_df$bwMultiplier[exercise_df$Exercise == exercise] * tail(merged_df$BodyWeight_MA, 1))) %>%
  mutate(repsPR = as.integer(ifelse(repsPR %% 1 == 0, repsPR + 1, ceiling(repsPR))),
          repsBeatPrev = as.integer(ifelse(repsBeatPrev %% 1 == 0, repsBeatPrev + 1, ceiling(repsBeatPrev))))