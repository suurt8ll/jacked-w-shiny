# --- Setup ---
# Load necessary libraries
library(readODS)
library(RSQLite)
library(dplyr)
library(tidyr)
library(lubridate)
library(readr)
library(here)

cat("Starting data migration script...\n")

# --- Configuration ---
# Define file paths using here() for portability
ods_path <- here("data", "fitness-log.ods") # Your ODS file name

# Find the latest massive*.db file
db_files <- list.files(here("data"), pattern = "^massive( \\(\\d+\\))?\\.db$")
extract_version <- function(filename) {
  if (grepl("^massive\\.db$", filename)) return(0) # Base file is version 0
  matches <- regmatches(filename, regexec("\\((\\d+)\\)", filename))
  if (length(matches[[1]]) > 1) return(as.numeric(matches[[1]][2])) # Extract number from parentheses
  return(-1) # Should not happen with the pattern, but safer
}
versions <- sapply(db_files, extract_version)
if (length(versions) == 0) {
  stop("FATAL: No massive*.db files found in the 'data' directory.")
}
sqlite_path <- here("data", db_files[which.max(versions)])

# Output file paths (using distinct names initially, can be changed later)
output_sets_csv_path <- here("data", "sets.csv")
output_definitions_csv_path <- here("data", "exercise_definitions.csv")

# ODS Sheet names
training_log_sheet <- "TrainingLog"
health_log_sheet <- "HealthLog"
exercise_db_sheet <- "ExerciseDatabase" # Not used in this part, but kept for context

# Date formats
date_format_ods <- "%m/%d/%y" # Format in your ODS Date columns
# SQLite format is handled by lubridate::ymd_hms

# Timezone (IMPORTANT: Set to your local timezone where the data was recorded)
local_timezone <- "UTC"

cat("Configuration set.\n")
cat("Input ODS:", basename(ods_path), "\n")
cat("Input SQLite DB:", basename(sqlite_path), "\n")

# Check if input files exist
stopifnot("ODS file not found at specified path." = file.exists(ods_path))
stopifnot("SQLite DB file not found at specified path." = file.exists(sqlite_path))

# --- Load Data ---
cat("Loading data from sources...\n")
# Load ODS data
tryCatch({
  ods_training_log <- read_ods(path = ods_path, sheet = training_log_sheet)
  ods_health_log <- read_ods(path = ods_path, sheet = health_log_sheet)
  ods_exercise_db <- read_ods(path = ods_path, sheet = exercise_db_sheet)
}, error = function(e) {
  stop("FATAL: Error reading ODS file '", basename(ods_path), "'. Check sheet names and file integrity. Error: ", e$message)
})

# Load SQLite data
tryCatch({
  con <- dbConnect(SQLite(), sqlite_path)
  # Select only the necessary columns directly from the DB
  sqlite_sets <- dbGetQuery(con, "SELECT name, reps, weight, created FROM sets")
  sqlite_weights <- dbGetQuery(con, "SELECT created, value FROM weights")
  dbDisconnect(con)
}, error = function(e) {
  stop("FATAL: Error reading SQLite DB '", basename(sqlite_path), "'. Check table names ('sets', 'weights') and file integrity. Error: ", e$message)
})
cat("Data loading complete.\n")

# --- Process ODS Exercise Set Data ---
cat("Processing ODS exercise set data...\n")
ods_sets_processed <- ods_training_log %>%
  select(Date, Exercise, starts_with("R"), starts_with("W")) %>%
  pivot_longer(
    cols = starts_with("R") | starts_with("W"),
    names_to = c(".value", "Set"), # .value captures R/W, Set captures the number
    names_pattern = "([RW])(\\d+)", # Regex to separate R/W prefix from set number
    values_drop_na = TRUE # Drop rows where BOTH R and W are NA for a given set number
  ) %>%
  filter(!is.na(R) & !is.na(W)) %>% # Ensure both Reps and Weight have values after pivot
  rename(
    created_date_ods = Date,
    name = Exercise,
    reps = R,
    weight = W,
    set_number_ods = Set # Keep the original ODS set number temporarily for duplicate check
  ) %>%
  mutate(
    # Attempt to parse ODS date
    created_date_ods = suppressWarnings(as.Date(created_date_ods, format = date_format_ods)),
    # Create POSIXct timestamp - Assume midday (12:00:00) for ODS entries
    # Use the specified local timezone
    created = if_else(
      is.na(created_date_ods),
      NA_POSIXct_,
      as.POSIXct(paste(created_date_ods, "12:00:00"), tz = local_timezone)
    ),
    name = trimws(name),
    # Ensure numeric types
    reps = as.numeric(reps),
    weight = as.numeric(weight),
    set_number_ods = as.numeric(set_number_ods) # Ensure set number is numeric
  ) %>%
  filter(!is.na(created) & !is.na(reps) & !is.na(weight)) %>% # Remove rows with parsing errors or missing core data
  # Select columns needed for combining and duplicate check
  select(name, reps, weight, created, set_number_ods)

# --- Process SQLite Exercise Set Data ---
cat("Processing SQLite exercise set data...\n")
sqlite_sets_processed <- sqlite_sets %>%
  mutate(
    # Parse SQLite timestamp string, specify timezone
    created = ymd_hms(created, tz = local_timezone),
    name = trimws(name),
    # Ensure numeric types (might be redundant if DB types are correct, but safe)
    reps = as.numeric(reps),
    weight = as.numeric(weight)
  ) %>%
  filter(!is.na(created) & !is.na(reps) & !is.na(weight)) %>% # Remove rows with parsing errors or missing core data
  # Select columns needed for combining (no set_number_ods here)
  select(name, reps, weight, created)

# --- Combine ODS and SQLite Sets (Raw) ---
cat("Combining exercise set data sources...\n")
# Use bind_rows; set_number_ods column will exist, containing NAs for SQLite rows
sets_combined_raw <- bind_rows(ods_sets_processed, sqlite_sets_processed)

# --- Check for Duplicates in Combined Exercise Sets ---
# A duplicate here means the exact same exercise, reps, weight, timestamp,
# AND (for ODS entries) the same original set number.
cat("Checking for duplicate exercise set entries...\n")

duplicates_sets_check <- sets_combined_raw %>%
  group_by(name, reps, weight, created, set_number_ods) %>%
  summarise(n = n(), .groups = 'drop') %>%
  filter(n > 1)

if (nrow(duplicates_sets_check) > 0) {
  warning("Duplicate exercise set entries detected based on Exercise, Reps, Weight, Timestamp, and (for ODS) original Set Number. Review these entries:")
  # Show the actual duplicate rows from the raw combined data for context
  duplicate_sets_details <- sets_combined_raw %>%
    semi_join(duplicates_sets_check, by = c("name", "reps", "weight", "created", "set_number_ods")) %>%
    arrange(name, created, set_number_ods, reps, weight) # Arrange for easier review
  print(duplicate_sets_details)
  stop("FATAL: Duplicate exercise sets found. Please resolve overlaps in source data or adjust script logic before proceeding.")
} else {
  cat("No duplicate exercise set entries found.\n")
}

# --- Final Processing: Select Columns and Sort Exercise Sets ---
# This part runs only if no duplicates were found and the script didn't stop.
cat("Performing final exercise set processing (selecting columns and sorting)...\n")
sets_final <- sets_combined_raw %>%
  # Select only the desired final columns, EXCLUDING set_number_ods
  select(name, created, reps, weight) %>%
  # Arrange the final dataset by timestamp
  arrange(created)

cat("Exercise set data processing and combining complete.\n")
cat("Final exercise set data frame 'sets_final' created with", nrow(sets_final), "rows.\n")


# --- Process ODS Bodyweight Data ---
cat("Processing ODS bodyweight data...\n")
ods_bw_processed <- ods_health_log %>%
  select(Date, BodyWeight) %>%
  filter(!is.na(BodyWeight)) %>% # Remove rows with missing bodyweight
  rename(
    created_date_ods = Date,
    weight = BodyWeight
  ) %>%
  mutate(
    created_date_ods = suppressWarnings(as.Date(created_date_ods, format = date_format_ods)),
    # Create POSIXct timestamp - Assume morning (06:00:00) for ODS bodyweight entries
    # Use the specified local timezone
    created = if_else(
      is.na(created_date_ods),
      NA_POSIXct_,
      as.POSIXct(paste(created_date_ods, "06:00:00"), tz = local_timezone)
    ),
    weight = as.numeric(weight) # Ensure numeric type
  ) %>%
  filter(!is.na(created) & !is.na(weight)) %>% # Remove rows with parsing errors or missing core data
  select(created, weight) # Select final columns

# --- Process SQLite Bodyweight Data ---
cat("Processing SQLite bodyweight data...\n")
sqlite_bw_processed <- sqlite_weights %>%
  rename(weight = value) %>% # Rename value to weight for consistency
  mutate(
    # Parse SQLite timestamp string, specify timezone
    created = ymd_hms(created, tz = local_timezone),
    weight = as.numeric(weight) # Ensure numeric type
  ) %>%
  filter(!is.na(created) & !is.na(weight)) %>% # Remove rows with parsing errors or missing core data
  select(created, weight) # Select final columns

# --- Combine ODS and SQLite Bodyweight Data ---
cat("Combining bodyweight data sources...\n")
# Use bind_rows as columns match
bw_combined_raw <- bind_rows(ods_bw_processed, sqlite_bw_processed)

# --- Check for Duplicates in Bodyweight Data ---
# A duplicate here means an EXACT duplicate row (same timestamp and weight).
# Multiple entries on the same day are considered distinct weigh-ins.
cat("Checking for exact duplicate bodyweight entries...\n")

# Count exact duplicates
duplicates_bw_check <- bw_combined_raw %>%
  group_by(created, weight) %>%
  summarise(n = n(), .groups = 'drop') %>%
  filter(n > 1)

if (nrow(duplicates_bw_check) > 0) {
  warning("Exact duplicate bodyweight entries detected (same timestamp and weight). Review these entries:")
  # Show the actual duplicate rows
  duplicate_bw_details <- bw_combined_raw %>%
    semi_join(duplicates_bw_check, by = c("created", "weight")) %>%
    arrange(created, weight) # Arrange for easier review
  print(duplicate_bw_details)
  # Do NOT stop for bodyweight duplicates, just warn.
  # Exact duplicates will be handled by distinct() in the next step.
} else {
  cat("No exact duplicate bodyweight entries found.\n")
}

# --- Final Processing: Remove Exact Duplicates and Sort Bodyweight Data ---
cat("Performing final bodyweight processing (removing exact duplicates and sorting)...\n")
bw_final <- bw_combined_raw %>%
  distinct() %>% # Remove any exact duplicate rows
  arrange(created) # Sort the final dataset by timestamp

cat("Bodyweight data processing and combining complete.\n")
cat("Final bodyweight data frame 'bw_final' created with", nrow(bw_final), "rows.\n")

# --- Format Data for `sets.csv` Output ---
cat("Formatting data for the consolidated sets CSV...\n")

# 1. Process Exercise Sets (sets_final already exists and is sorted by created)
sets_formatted <- sets_final %>%
  # Add placeholder columns to match Massive export format
  mutate(
    unit = NA_character_,
    hidden = "0", # Default seems to be "0"
    image = NA_character_,
    sets = NA_integer_, # These seem specific to workout summary, not individual sets
    minutes = NA_integer_,
    seconds = NA_integer_,
    steps = NA_integer_
  ) %>%
  # Select and order columns to match the target CSV structure (excluding id for now)
  select(name, reps, weight, created, unit, hidden, image, sets, minutes, seconds, steps)

# 2. Process Bodyweight Data (bw_final already exists and is sorted by created)
bw_formatted <- bw_final %>%
  mutate(
    name = "Weight",
    reps = 1, # Convention to fit the structure
    # Add placeholder columns to match Massive export format
    unit = "kg", # Assuming kg based on example, adjust if needed
    hidden = "0",
    image = NA_character_,
    sets = NA_integer_,
    minutes = NA_integer_,
    seconds = NA_integer_,
    steps = NA_integer_
  ) %>%
  # Select and order columns to match the target CSV structure (excluding id for now)
  # Ensure columns are in the same order as sets_formatted for bind_rows
  select(name, reps, weight, created, unit, hidden, image, sets, minutes, seconds, steps)


# 3. Combine Exercise Sets and Formatted Bodyweight Data
# Exercise sets come first, then bodyweight.
# The order within sets_formatted and bw_formatted is already by 'created'.
final_combined_data <- bind_rows(sets_formatted, bw_formatted)

# 4. Add unique ID based on the combined order
final_combined_data <- final_combined_data %>%
  mutate(id = row_number()) %>%
  # Select and reorder columns to put 'id' first
  select(id, everything()) # Use everything() to keep all other columns in their current order

# The final data frame 'final_combined_data' is now ready for export.
# It contains sets followed by bodyweight, with sequential IDs.

cat("Data formatting for CSV complete.\n")
cat("Final data frame 'final_combined_data' created with", nrow(final_combined_data), "rows.\n")

# --- Process Exercise Definitions (Using Full Structure) ---
cat("Processing exercise definitions using full structure...\n")

# Assume ods_exercise_db is loaded and available from earlier steps
# Ensure column names match the provided structure exactly
stopifnot(all(c("Exercise", "isBW", "bwMultiplier", "MovementPattern", "PrimaryMover", "Notes") %in% names(ods_exercise_db)))

definitions_processed <- ods_exercise_db %>%
  rename(name = Exercise) %>% # Use 'name' internally for joining
  select(name, isBW, bwMultiplier, MovementPattern, PrimaryMover, Notes) %>% # Select all relevant columns
  mutate(
    name = trimws(name),
    # Ensure bwMultiplier is numeric, default NA to 0
    bwMultiplier = suppressWarnings(as.numeric(bwMultiplier)),
    bwMultiplier = if_else(is.na(bwMultiplier), 0, bwMultiplier),
    # Ensure isBW is numeric (0 or 1), default NA to 0
    isBW = suppressWarnings(as.numeric(isBW)),
    isBW = if_else(is.na(isBW), 0, isBW),
    # Ensure other text columns are character.
    MovementPattern = if_else(is.na(MovementPattern), NA, as.character(MovementPattern)),
    PrimaryMover = if_else(is.na(PrimaryMover), NA, as.character(PrimaryMover)),
    Notes = if_else(is.na(Notes), "", as.character(Notes)) # Default NA notes to empty string
  ) %>%
  distinct(name, .keep_all = TRUE) # Ensure unique definitions based on name

# Find exercises in logs that are NOT in the definitions
all_logged_exercise_names <- sets_final %>%
  distinct(name)

missing_definitions <- all_logged_exercise_names %>%
  anti_join(definitions_processed, by = "name")

if (nrow(missing_definitions) > 0) {
  cat("INFO: Found", nrow(missing_definitions), "exercises in logs that are missing from the ODS ExerciseDatabase:\n")
  print(missing_definitions$name)
  # Create placeholder rows for these missing exercises with all columns
  missing_defs_placeholders <- missing_definitions %>%
    mutate(
      isBW = 0, # Default: Assume not body-weight
      bwMultiplier = 0, # Default: Assume 0 multiplier
      MovementPattern = NA,
      PrimaryMover = NA,
      Notes = "Auto-added needs review!" # Placeholder note
    )
  # Combine original definitions with placeholders
  final_definitions <- bind_rows(definitions_processed, missing_defs_placeholders)
} else {
  cat("INFO: All exercises found in logs have corresponding entries in the ODS ExerciseDatabase.\n")
  final_definitions <- definitions_processed
}

# Final cleanup for definitions CSV
final_definitions <- final_definitions %>%
  rename(Exercise = name) %>% # Rename back to 'Exercise' for the output file
  arrange(Exercise) %>%
  # Select and order all columns for the output CSV
  select(Exercise, isBW, bwMultiplier, MovementPattern, PrimaryMover, Notes)

# --- Write CSV Files ---
cat("Writing output CSV files...\n")
# Write the consolidated sets file
tryCatch({
  # Format POSIXct 'created' column to ISO8601 format with 'T' separator for compatibility
  final_sets_data_to_write <- final_combined_data %>%
    mutate(created = format(created, "%Y-%m-%dT%H:%M:%S")) # Explicit format
  write_csv(final_sets_data_to_write, output_sets_csv_path, na = "") # Use "" for NA values
  cat("Successfully wrote consolidated sets data to:", basename(output_sets_csv_path), "\n")
}, error = function(e) {
  stop("FATAL: Error writing sets CSV file. Check permissions and path. Error: ", e$message)
})

# Write the consolidated definitions file
tryCatch({
  write_csv(final_definitions, output_definitions_csv_path, na = "") # Use "" for NA values
  cat("Successfully wrote consolidated definitions data to:", basename(output_definitions_csv_path), "\n")
}, error = function(e) {
  stop("FATAL: Error writing definitions CSV file. Check permissions and path. Error: ", e$message)
})

cat("--- Data Migration Script Finished Successfully ---\n")
cat("Please review the output files:\n")
cat("- ", basename(output_sets_csv_path), "\n")
cat("- ", basename(output_definitions_csv_path), "\n")
if (nrow(missing_definitions) > 0) {
  cat("ACTION REQUIRED: Manually update '", basename(output_definitions_csv_path), "' to fill in correct details (isBW, bwMultiplier, MovementPattern, PrimaryMover, Notes) for the exercises marked 'Auto-added'.\n")
}