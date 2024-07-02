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
calculate_max_tonnage <- function(harjutus, tonnage_criteria) {
  exercise_data <- merged_df %>%
    filter(Exercise == harjutus) %>%
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

# Andmete laadimine
trenni_logi <- read_ods(path = "./fitness-log.ods",
                        sheet = "TrainingLog")
tervise_logi <- read_ods(path = "./fitness-log.ods",
                         sheet = "WeightLog")
harjutuste_andmebaas <- read_ods(path = "./fitness-log.ods",
                                 sheet = "ExerciseDatabase")

# Andmete korrastamine
tervise_logi$Date <- as.Date(tervise_logi$Date, "%m/%d/%y")
# FIXME: Intermediary dates are missing, but they are needed for calculations. A solution is required.
trenni_logi$Date <- as.Date(trenni_logi$Date, "%m/%d/%y")
# If exercise is not body-weight then multiplier is missing, convert these to 0.
harjutuste_andmebaas$bwMultiplier <- ifelse(
  is.na(harjutuste_andmebaas$bwMultiplier),
  0,
  harjutuste_andmebaas$bwMultiplier
)

# Greasy hack, et ma näeks numbreid, kui pole kaua aega kaalnunud
tervise_logi$BodyWeight[nrow(tervise_logi)] <- ifelse(is.na(tail(tervise_logi$BodyWeight, n = 1)),
                                                    tail(na.trim(tervise_logi$BodyWeight), n = 1),
                                                    tail(tervise_logi$BodyWeight, n = 1))
# Use rollapply to calculate the moving average, ignoring NA values
tervise_logi$BodyWeight_interpolated <- na.approx(tervise_logi$BodyWeight, na.rm = FALSE)
tervise_logi$BodyWeight_MA <- rollapply(
  tervise_logi$BodyWeight_interpolated,
  width = 30,
  FUN = mean,
  fill = NA,
  align = "right"
)

# Tekita üks suur juicy dataframe, mis hoomab kõike.
merged_df <- trenni_logi %>%
  left_join(harjutuste_andmebaas, by = "Exercise") %>%
  left_join(tervise_logi, by = "Date")

ui <- dashboardPage(
  dashboardHeader(title = "GAINZ"),
  
  dashboardSidebar(sidebarMenu(
    menuItem("BodyWeight", tabName = "kaal", icon = icon("bar-chart")),
    menuItem("Aktiivus", tabName = "aktiivsus", icon = icon("bar-chart")),
    menuItem("Tonnage", tabName = "maxtonnage", icon = icon("bar-chart")),
    menuItem(
      "Kalkulaator",
      tabName = "calc",
      icon = icon("bar-chart")
    )
  )),
  
  dashboardBody(tabItems(
    tabItem(
      tabName = "kaal",
      tags$style(type = "text/css", "#kehakaalPlot {height: calc(100vh - 160px) !important;}"),
      plotlyOutput("kehakaalPlot", height = "100%", width = "100%")
    ),
    tabItem(
      tabName = "aktiivsus",
      tags$style(
        type = "text/css",
        "#aktiivsusBarPlot {height: calc(100vh - 80px) !important;}"
      ),
      plotlyOutput("aktiivsusBarPlot", height = "100%", width = "100%")
    ),
    tabItem(
      tabName = "maxtonnage",
      fluidRow(
        selectInput(
          "harjutus",
          "Exercise",
          choices = unique(trenni_logi$Exercise),
          selected = "Pull-up"
        )
      ),
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
      fluidRow(
        selectInput(
          "calc_harjutus",
          "Exercise",
          choices = unique(trenni_logi$Exercise),
          selected = "Pull-up"
        )
      ),
      fluidRow(
        sliderInput(
          "calc_sets",
          "Calculate PR for n sets:",
          min = 1,
          max = 5,
          value = 1
        )
      ),
      fluidRow(textInput("calc_weight", "Weight (kg):", value = 60)),
      fluidRow(textOutput("calc_reps")),
      fluidRow(textOutput("calc_reps_prev"))
    )
  )),
  skin = "green"
)

server <- function(input, output, session) {
  output$kehakaalPlot <- renderPlotly({
    # Plot the data using ggplot2
    p <- ggplot(tervise_logi, aes(x = Date)) +
      geom_point(aes(y = BodyWeight), color = "grey", na.rm = TRUE) +
      geom_line(aes(y = BodyWeight_MA),
                color = "black",
                na.rm = TRUE) +
      labs(title = "BodyWeight with 30-Day Moving Average", x = "Date", y = "BodyWeight") +
      theme_minimal()
    ggplotly(p)
  })
  
  output$aktiivsusBarPlot <- renderPlotly({
    # FIXME Exercise duration has been moved to TrainingLog, this does not work anymore.
    
    # Create a new column for the week number
    tervise_logi$Week <- format(as.Date(tervise_logi$Date), "%Y-%U")
    
    # Summarize the data to get weekly sums
    weekly_sums <- tervise_logi %>%
      group_by(Week) %>%
      summarise(WeeklySum = sum(Aktiivsed.minutid, na.rm = TRUE))
    
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
          harjutus = input$harjutus,
          tonnage_criteria = tonnage_criteria
        )
      )
    }
    
    p <- ggplot(max_tonnage_data_long,
                aes(x = Date, y = MaxTonnage, colour = TonnageCriteria)) +
      geom_line() +
      geom_point() +
      scale_color_brewer() +
      scale_y_log10() +
      theme_dark()
    ggplotly(p)
  })
  
  output$calc_reps <- renderText({
    # Calculate reps
    reps_alltime <- max(calculate_max_tonnage(input$calc_harjutus, input$calc_sets)$MaxTonnage) / as.numeric(input$calc_weight)
    reps_alltime <- ceiling(reps_alltime)
    paste("Reps needed for all time PR for", input$calc_sets, "sets:", reps_alltime, sep = " ")
  })
  
  output$calc_reps_prev <- renderText({
    # Calculate reps
    reps_prev <- tail(calculate_max_tonnage(input$calc_harjutus, input$calc_sets)$MaxTonnage, 1) / as.numeric(input$calc_weight)
    reps_prev <- ceiling(reps_prev)
    paste("Reps needed to outdo previous workout sesh for", input$calc_sets, "sets:", reps_prev, sep = " ")
  })
}

app <- shinyApp(ui = ui, server = server)
runApp(app, port=6006)