#---- Packages and Functions ----
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
  "lubridate",
  "RSQLite"
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
# Calculate max tonnage for `n` sets
calculate_max_tonnage <- function(data, n) {
  data %>%
    group_by(date) %>%
    summarise(
      max_tonnage = if (n > n()) NA else max(apply(combn(tonnage, n), 2, sum, na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    filter(!is.na(max_tonnage)) # Exclude dates with fewer than `n` sets
}

#---- Parameters ----
# File paths
ods_path <- "./fitness-log.ods"
# Automatically find the latest massive*.db file
db_files <- list.files(pattern = "^massive( \\(\\d+\\))?\\.db$")
# Function to extract version numbers from filenames
extract_version <- function(filename) {
  if (filename == "massive.db") {
    return(0)
  } else {
    matches <- regmatches(filename, regexec("\\((\\d+)\\)", filename))
    if (length(matches[[1]]) > 1) {
      return(as.numeric(matches[[1]][2]))
    } else {
      return(0)
    }
  }
}
# Apply the function to each filename to get versions
versions <- sapply(db_files, extract_version)
# Check if any files were found
if (length(versions) == 0) {
  stop("No massive*.db files found in the current directory.")
}
# Select the file with the highest version number
sqlite_path <- db_files[which.max(versions)]

# Sheet names
training_log_sheet <- "TrainingLog"
health_log_sheet <- "HealthLog"
exercise_db_sheet <- "ExerciseDatabase"

# Date formats
date_format_ods <- "%m/%d/%y"
date_format_sqlite <- "%Y-%m-%dT%H:%M:%S"

# Rolling average window size
rolling_window <- 30

#---- Data Loading ----

# Load data from LibreOffice Calc file
training_log <- read_ods(path = ods_path, sheet = training_log_sheet)
health_log <- read_ods(path = ods_path, sheet = health_log_sheet)
exercise_df <- read_ods(path = ods_path, sheet = exercise_db_sheet)

# Load data from SQLite database
con <- dbConnect(SQLite(), sqlite_path)
massive_db <- dbReadTable(con, "sets") %>% select(name, reps, weight, created)
massive_db_bw <- dbReadTable(con, "weights") %>% select(created, BodyWeight = value)
dbDisconnect(con)

#---- Data Cleaning ----
# Convert dates
health_log$Date <- as.Date(health_log$Date, date_format_ods)
training_log$Date <- as.Date(training_log$Date, date_format_ods)
massive_db <- massive_db %>%
  mutate(
    name = trimws(name),
    created = as.POSIXct(created, format = date_format_sqlite),
    date = as.Date(created)
  )
massive_db_bw <- massive_db_bw %>%
  mutate(
    created = as.POSIXct(created, format = date_format_sqlite),
    Date = as.Date(created)
  ) %>%
  select(Date, BodyWeight)

# Fix missing bodyweight multipliers
exercise_df$bwMultiplier <- ifelse(is.na(exercise_df$bwMultiplier), 0, exercise_df$bwMultiplier)

# Merge bodyweight data from both sources
health_log <- bind_rows(health_log, massive_db_bw)

# Carry last body weight forward to current date
health_log$BodyWeight[nrow(health_log)] <- ifelse(
  is.na(tail(health_log$BodyWeight, n = 1)),
  tail(na.trim(health_log$BodyWeight), n = 1),
  tail(health_log$BodyWeight, n = 1)
)

#---- Data Transformations ----
# Interpolate missing bodyweight data and calculate moving average
health_log$BodyWeight_interpolated <- na.approx(health_log$BodyWeight, na.rm = FALSE)
health_log$BodyWeight_MA <- rollapply(
  health_log$BodyWeight_interpolated,
  width = rolling_window,
  FUN = mean,
  fill = NA,
  align = "right"
)
# Pivot longer: Combine R1, R2,... and W1, W2,... into rows
training_log_long <- training_log %>%
  pivot_longer(
    cols = starts_with("R") | starts_with("W"),   # Columns to pivot
    names_to = c(".value", "Set"),               # Extract values (R/W) and Set number
    names_pattern = "([RW])(\\d+)"               # Regex to split column names into R/W and set number
  ) %>%
  filter(!(is.na(R) & is.na(W))) %>%      # Drop rows where both Reps and Weight are NA
  rename(
    date = Date,                              # Rename Date to created
    name = Exercise,                             # Rename Exercise to name
    reps = R,                                 # Rename Reps to reps
    weight = W                              # Rename Weight to weight
  ) %>%
  arrange(date, Num, Set) %>%                         # Optional: Arrange rows by date and set
  select(name, reps, weight, date, everything()) # Reorder columns with the renamed ones first
# Merge everything into one big dataframe.
merged_df_libreoffice <- training_log_long %>%
  left_join(exercise_df, by = c("name" = "Exercise")) %>%
  left_join(health_log, by = c("date" = "Date"))
merged_df_massive <- massive_db %>%
  left_join(exercise_df, by = c("name" = "Exercise")) %>%
  left_join(health_log, by = c("date" = "Date"))
# Bind rows with only common columns
merged_df <- bind_rows(
  merged_df_libreoffice %>% select(all_of(intersect(names(merged_df_libreoffice), names(merged_df_massive)))),
  merged_df_massive %>% select(all_of(intersect(names(merged_df_libreoffice), names(merged_df_massive))))
)
rm("con", "exercise_df", "massive_db", "merged_df_libreoffice", "merged_df_massive", "training_log", "training_log_long")

#---- BW Graph ----
p <- ggplot(health_log, aes(x = Date)) +
  geom_point(aes(y = BodyWeight), color = "grey", na.rm = TRUE) +
  geom_line(aes(y = BodyWeight_MA),
            color = "black",
            na.rm = TRUE) +
  labs(title = "BodyWeight with 30-Day Moving Average", x = "Date", y = "BodyWeight") +
  theme_minimal()
ggplotly(p)

#---- Weekly Activity Graph ----
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

#---- Tonnage Graph w/ drop-down ----

# Logic for the dynamic drop-down menu, filtered based on date.
selected_date <- as.Date("2024-11-23")
if (!is.null(selected_date)) {
  # Limit choices to the given date's exercises.
  exercise_list <- merged_df %>% filter(date == selected_date) %>% pull(name) %>% unique()
} else {
  # Whole exercise list gets used if no date is chosen.
  exercise_list <- unique(merged_df$name)
}

# Select the exercise and calculate tonnage per set
selected_exercise <- "Chin-up"
exercise_data <- merged_df %>%
  filter(name == selected_exercise) %>%
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
  type = 'scatter',
  mode = 'lines+markers'
) %>%
  layout(
    title = "Max Tonnage Records for 1-5 Sets",
    xaxis = list(title = "Date"),
    yaxis = list(title = "Max Tonnage"),
    legend = list(title = list(text = "Number of Sets"))
  )
