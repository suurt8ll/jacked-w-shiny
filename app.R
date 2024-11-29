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
  "RSQLite",
  "lubridate",
  "DT"
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
sqlite_path <- "./massive (1).db"

# Sheet names
training_log_sheet <- "TrainingLog"
health_log_sheet <- "HealthLog"
exercise_db_sheet <- "ExerciseDatabase"

# Date formats
date_format_ods <- "%m/%d/%y"
date_format_sqlite <- "%Y-%m-%dT%H:%M:%S"

# Rolling average window size
rolling_window <- 30

#---- Data loading ----

# TODO User should have an option to reload data directly in app without needing to restart the whole program.
# ^ Hint: this can be done separately in bash script, user needs to press a key to refresh.

# Load data from LibreOffice Calc file
training_log <- read_ods(path = ods_path, sheet = training_log_sheet)
health_log <- read_ods(path = ods_path, sheet = health_log_sheet)
exercise_df <- read_ods(path = ods_path, sheet = exercise_db_sheet)

# Load data from SQLite database
con <- dbConnect(SQLite(), sqlite_path)
massive_db <- dbReadTable(con, "sets")
dbDisconnect(con)

#---- Data Cleaning ----
# Convert dates
health_log$Date <- as.Date(health_log$Date, date_format_ods)
training_log$Date <- as.Date(training_log$Date, date_format_ods)
massive_db <- massive_db %>%
  select(name, reps, weight, created) %>%
  mutate(
    name = trimws(name),
    created = as.POSIXct(created, format = date_format_sqlite),
    date = as.Date(created)
  )

# Fix missing bodyweight multipliers
exercise_df$bwMultiplier <- ifelse(is.na(exercise_df$bwMultiplier), 0, exercise_df$bwMultiplier)

# Carry last body weight forward to current date
health_log$BodyWeight[nrow(health_log)] <- ifelse(
  is.na(tail(health_log$BodyWeight, n = 1)),
  tail(na.trim(health_log$BodyWeight), n = 1),
  tail(health_log$BodyWeight, n = 1)
)

#---- Data Transformations ----

# FIXME Exercises not in exercise_df should be omitted.

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
# Remove rows where weight or reps is missing
merged_df <- merged_df %>%
  filter(!is.na(weight) & !is.na(reps))
rm("con", "massive_db", "merged_df_libreoffice", "merged_df_massive", "training_log", "training_log_long")

#---- Shiny ui ----
ui <- dashboardPage(
  dashboardHeader(title = "GAINZ"),
  
  dashboardSidebar(sidebarMenu(
    menuItem(
      "BodyWeight",
      tabName = "kaal",
      icon = icon("bar-chart")
    ),
    menuItem("Activity", tabName = "activity", icon = icon("bar-chart")),
    menuItem("Tonnage", tabName = "maxtonnage", icon = icon("bar-chart")),
    menuItem("Calculator", tabName = "calc", icon = icon("bar-chart")),
    menuItem("Raw Data", tabName = "rawdata", icon = icon("table"))
  )),
  
  dashboardBody(tabItems(
    tabItem(
      tabName = "kaal",
      tags$style(type = "text/css", "#kehakaalPlot {height: calc(100vh - 160px) !important;}"),
      plotlyOutput("kehakaalPlot", height = "100%", width = "100%")
    ),
    tabItem(
      tabName = "activity",
      fluidRow(
        selectInput(
          "activity_date_range",
          "Select Date Range:",
          choices = c(
            "Past 1 Month" = "1m",
            "Past 3 Months" = "3m",
            "Past 6 Months" = "6m",
            "Past 1 Year" = "1y",
            "Past 3 Years" = "3y",
            "Past 5 Years" = "5y",
            "All Time" = "all"
          ),
          selected = "3m"  # Default selection set to "Past 3 Months"
        )
      ),
      tags$style(
        type = "text/css",
        "#activityBarPlot {height: calc(100vh - 160px) !important;}"
      ),
      plotlyOutput("activityBarPlot", height = "100%", width = "100%")
    ),
    tabItem(
      tabName = "maxtonnage",
      fluidRow(column(
        width = 6, dateInput(
          "date",
          "Date",
          min = min(merged_df$date),
          max = max(merged_df$date)
        )
      ), column(
        width = 6, selectInput("exercise", "Exercise", choices = unique(merged_df$name))
      )),
      fluidRow(
        tags$style(
          type = "text/css",
          "#maxTonnagePlot {height: calc(100vh - 160px) !important;}"
        ),
        plotlyOutput("maxTonnagePlot", height = "100%", width = "100%")
      )
    ),
    tabItem(
      tabName = "calc",
      fluidRow(selectInput(
        "calc_exercise", "Exercise", choices = unique(merged_df$name)
      )),
      # FIXME step should be customizable based on users own situation and wants.
      fluidRow(numericInput("calc_weight", "Weight (kg):", value = 60, min = 2.5, step = 2.5)),
      fluidRow(DT::dataTableOutput("calc_reps"))
    ),
    tabItem(
      tabName = "rawdata",
      fluidRow(
        DT::dataTableOutput("rawDataTable")
      )
    )
  )),
  skin = "blue"
)

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
    # Ensure lubridate is loaded
    library(lubridate)
    
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
      type = 'scatter',
      mode = 'lines+markers'
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
        adjusted_weight = input$calc_weight + exercise_df$bwMultiplier[exercise_df$Exercise == input$calc_exercise] * tail(merged_df$BodyWeight_MA, 1),
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
    DT::datatable(filtered_data)
  })
}

#---- Run the Shiny app ----
app <- shinyApp(ui = ui, server = server)

#runApp(app, port = 6006, host = "0.0.0.0")
# Use this if you don't want to expose your dashboard to the LAN.
#runApp(app, port=6006)

# Comment out runApp() and uncomment this to get it working inside RStudio.
app
