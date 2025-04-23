#---- Shiny ui ----
ui <- fluidPage(
  tags$head(
    tags$link(rel = "icon", type = "image/png", sizes = "32x32", href = "favicon-32x32.png"),
    tags$link(rel = "icon", type = "image/png", sizes = "16x16", href = "favicon-16x16.png"),
    tags$link(rel = "shortcut icon", href = "favicon.ico"),
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  navbarPage(
    title = "GAINZ",
    theme = bslib::bs_theme(
      version = 5, # Good practice to specify Bootstrap version
      bg = "#000000", # Black background
      fg = "#FFFFFF", # White text
      primary = "purple", # Primary color (buttons, highlights)
      "input-bg" = "#333333", # Dark grey for input backgrounds (slightly lighter than pure black)
      "input-fg" = "#FFFFFF", # White text for inputs
      "input-border-color" = "#555555", # Grey border for inputs
      # Explicitly theme dropdowns (might be needed for selectize)
      "dropdown-bg" = "#222222", # Dark background for dropdown menu
      "dropdown-link-color" = "#FFFFFF", # White text for dropdown items
      "dropdown-link-hover-bg" = "purple", # Primary color on hover
      "dropdown-link-hover-color" = "#FFFFFF" # White text on hover
    ),
    # Replace sidebarMenu/menuItem/tabItems/tabItem with tabPanel
    tabPanel(
      title = tagList("BodyWeight", icon("bar-chart")), # Combine title and icon
      value = "kaal", # Use tabName as value for potential bookmarking/linking
      plotlyOutput("kehakaalPlot", height = "100%", width = "100%")
    ),
    tabPanel(
      title = tagList("Activity", icon("bar-chart")),
      value = "activity",
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
          selected = "3m"
        )
      ),
      plotlyOutput("activityBarPlot", height = "100%", width = "100%")
    ),
    tabPanel(
      title = tagList("Tonnage", icon("bar-chart")),
      value = "maxtonnage",
      fluidRow(
        column(
          width = 4,
          dateInput(
            "date",
            "Filter by Date (Optional):",
            min = min(merged_df$date),
            max = max(merged_df$date),
            value = NULL # Start with no date selected for filtering
          )
        ),
        column(
          width = 4,
          selectInput("exercise", "Exercise:", choices = unique(merged_df$name))
        ),
        column(
          width = 4,
          dateInput(
            "regressionStartDate", # ID for Tonnage tab
            "Regression Start Date:",
            min = min(merged_df$date), # Use global min for simplicity
            max = max(merged_df$date), # Use global max for simplicity
            value = min(merged_df$date) # Default to earliest date in dataset
          )
        )
      ),
      plotlyOutput("maxTonnagePlot", height = "100%", width = "100%")
    ),
    tabPanel(
      title = tagList("Calculator", icon("bar-chart")),
      value = "calc",
      # --- NEW: Row for inputs ---
      fluidRow(
        column(
          width = 4,
          selectInput(
            "calc_exercise", "Exercise:", choices = unique(merged_df$name)
          )
        ),
        column(
          width = 4,
          numericInput("calc_weight", "Weight (kg):", value = 60, min = 2.5, step = 2.5)
        ),
        column(
          width = 4,
          dateInput(
            "calc_regressionStartDate", # Different ID for Calc tab
            "Regression Start Date:",
            min = min(merged_df$date),
            max = max(merged_df$date),
            value = min(merged_df$date)
          )
        )
      ),
      DT::dataTableOutput("calc_reps")
    ),
    tabPanel(
      title = tagList("Raw Data", icon("table")),
      value = "rawdata",
      DT::dataTableOutput("rawDataTable")
    )
  )
)