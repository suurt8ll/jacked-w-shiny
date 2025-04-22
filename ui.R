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

  dashboardBody(
    tags$head(
      tags$link(rel = "icon", type = "image/png", sizes = "32x32", href = "favicon-32x32.png"),
      tags$link(rel = "icon", type = "image/png", sizes = "16x16", href = "favicon-16x16.png"),
      tags$link(rel = "shortcut icon", href = "favicon.ico"),
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
    ),
    tabItems(
      tabItem(
        tabName = "kaal",
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
    )
  ),
  skin = "blue"
)