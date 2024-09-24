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
  "plotly"
)
# Vaatab, kas vajalikud paketid on olemas, kui ei ole siis installib need.
for (lib in required_libs) {
  if (!(lib %in% installed.packages()[, "Package"])) {
    install.packages(lib)
  }
}
# Pakettide laadimine
lapply(required_libs, require, character.only = TRUE)

# Funktsioonid
calculate_max_tonnage <- function(exercise, tonnage_criteria) {
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
  
  # Determine the maximum tonnage for each date dynamically
  max_tonnage_data <- exercise_data %>%
    rowwise() %>%
    mutate(TonnageCriteria = tonnage_criteria, MaxTonnage = {
      sets <- c(T1, T2, T3, T4, T5)
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
# TODO User should have an option to reload data directly in app without needing to restart the whole program.
# ^ Hint: this can be done separately in bash script, user needs to press a key to refresh.
# Load data from Libreoffice Calc file
training_log <- read_ods(path = "./fitness-log.ods", sheet = "TrainingLog")
health_log <- read_ods(path = "./fitness-log.ods", sheet = "HealthLog")
exercise_df <- read_ods(path = "./fitness-log.ods", sheet = "ExerciseDatabase")

# Format data
health_log$Date <- as.Date(health_log$Date, "%y-%m-%d")
training_log$Date <- as.Date(training_log$Date, "%y-%m-%d")
# If exercise is not body-weight then the multiplier is missing, convert these to 0.
exercise_df$bwMultiplier <- ifelse(is.na(exercise_df$bwMultiplier), 0, exercise_df$bwMultiplier)
# Put the last known body-weight as weight if it's missing.
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

# FIXME Exercises not in exercise_df should be omitted.
# Merge everything to one big, juicy dataframe
merged_df <- training_log %>%
  left_join(exercise_df, by = "Exercise") %>%
  left_join(health_log, by = "Date")

# FIXME all graphs should follow green theme like the page.
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
    menuItem("Calculator", tabName = "calc", icon = icon("bar-chart"))
  )),
  
  dashboardBody(tabItems(
    tabItem(
      tabName = "kaal",
      tags$style(type = "text/css", "#kehakaalPlot {height: calc(100vh - 160px) !important;}"),
      plotlyOutput("kehakaalPlot", height = "100%", width = "100%")
    ),
    tabItem(
      tabName = "activity",
      tags$style(
        type = "text/css",
        "#activityBarPlot {height: calc(100vh - 80px) !important;}"
      ),
      plotlyOutput("activityBarPlot", height = "100%", width = "100%")
    ),
    tabItem(
      tabName = "maxtonnage",
      fluidRow(column(
        width = 6, dateInput(
          "date",
          "Date",
          min = min(training_log$Date),
          max = max(training_log$Date)
        )
      ), column(
        width = 6, selectInput("exercise", "Exercise", choices = unique(training_log$Exercise))
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
        "calc_exercise", "Exercise", choices = unique(training_log$Exercise)
      )),
      # FIXME step should be customizable based on users own situation and wants.
      fluidRow(numericInput("calc_weight", "Weight (kg):", value = 60, min = 2.5, step = 2.5)),
      fluidRow(tableOutput("calc_reps"))
    )
  )),
  skin = "green"
)

#---- Shiny server ----
server <- function(input, output, session) {
  # Filter the exercise choice list based on chosen date
  observe({
    if (length(input$date) != 0) {
      x <- unique(training_log %>% filter(Date == input$date) %>% select(Exercise) %>% unlist())
    } else {
      x <- unique(training_log$Exercise)
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
    # Plot the data using ggplot2
    p <- ggplot(health_log, aes(x = Date)) +
      geom_point(aes(y = BodyWeight),
                 color = "grey",
                 na.rm = TRUE) +
      geom_line(aes(y = BodyWeight_MA),
                color = "black",
                na.rm = TRUE) +
      labs(title = "BodyWeight with 30-Day Moving Average", x = "Date", y = "BodyWeight") +
      theme_minimal()
    ggplotly(p)
  })
  
  output$activityBarPlot <- renderPlotly({
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
  })
  
  output$maxTonnagePlot <- renderPlotly({
    max_tonnage_data_long <- data.frame()
    for (tonnage_criteria in c("max1", "max2", "max3", "max4", "max5")) {
      max_tonnage_data_long <- rbind(
        max_tonnage_data_long,
        calculate_max_tonnage(
          exercise = input$exercise,
          tonnage_criteria = tonnage_criteria
        )
      )
    }
    
    p <- ggplot(max_tonnage_data_long,
                aes(x = Date, y = MaxTonnage, colour = TonnageCriteria)) +
      geom_line() +
      geom_point() +
      scale_color_brewer() +
      #scale_y_log10() +
      theme_dark()
    ggplotly(p)
  })
  
  output$calc_reps <- renderTable({
    # TODO this shares alot with the maxTonnagePlot logic, merge them somehow?
    max_tonnage_data_long <- data.frame()
    for (tonnage_criteria in c("max1", "max2", "max3", "max4", "max5")) {
      max_tonnage_data_long <- rbind(
        max_tonnage_data_long,
        calculate_max_tonnage(
          exercise = input$calc_exercise,
          tonnage_criteria = tonnage_criteria
        )
      )
    }
    max_tonnage_data_long %>%
      group_by(TonnageCriteria) %>%
      summarise(
        maxTonnage = max(MaxTonnage),
        lastTonnage = tail(MaxTonnage, 1)
      ) %>%
      mutate(repsPR = maxTonnage / (input$calc_weight + exercise_df$bwMultiplier[exercise_df$Exercise == input$calc_exercise] * tail(merged_df$BodyWeight_MA, 1)),
             repsBeatPrev = lastTonnage / (input$calc_weight + exercise_df$bwMultiplier[exercise_df$Exercise == input$calc_exercise] * tail(merged_df$BodyWeight_MA, 1))) %>%
      mutate(repsPR = as.integer(ifelse(repsPR %% 1 == 0, repsPR + 1, ceiling(repsPR))),
             repsBeatPrev = as.integer(ifelse(repsBeatPrev %% 1 == 0, repsBeatPrev + 1, ceiling(repsBeatPrev))),
             maxTonnage = as.integer(round(maxTonnage)),
             lastTonnage = as.integer(round(lastTonnage)))
  })
}

#---- Run the Shiny app ----
app <- shinyApp(ui = ui, server = server)

#runApp(app, port = 6006, host = "0.0.0.0")
# Use this if you don't want to expose your dashboard to the LAN.
#runApp(app, port=6006)

# Comment out runApp() and uncomment this to get it working inside RStudio.
app
