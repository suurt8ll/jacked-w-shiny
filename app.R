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
    filter(Harjutus == harjutus) %>%
    mutate(
      T1 = R1 * (BW. * (Kehakaalu.kordaja * Kehakaal_MA) + W1),
      T2 = R2 * (BW. * (Kehakaalu.kordaja * Kehakaal_MA) + W2),
      T3 = R3 * (BW. * (Kehakaalu.kordaja * Kehakaal_MA) + W3),
      T4 = R4 * (BW. * (Kehakaalu.kordaja * Kehakaal_MA) + W4),
      T5 = R5 * (BW. * (Kehakaalu.kordaja * Kehakaal_MA) + W5)
    ) %>%
    select(Kuupäev, Harjutus, T1, T2, T3, T4, T5)
  
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
    select(Kuupäev, TonnageCriteria, MaxTonnage)
}

# Andmete laadimine
trenni_logi <- read_ods(path = "/home/madis/documents/tervis/fitness-log.ods",
                        sheet = "Trenni logi",
                        .name_repair = "universal")
tervise_logi <- read_ods(path = "/home/madis/documents/tervis/fitness-log.ods",
                         sheet = "Tervisenäitajate logi",
                         .name_repair = "universal")
harjutuste_andmebaas <- read_ods(path = "/home/madis/documents/tervis/fitness-log.ods",
                                 sheet = "Andmebaas",
                                 .name_repair = "universal")
harjutuste_andmebaas <- harjutuste_andmebaas[, 1:6]

# Andmete korrastamine
tervise_logi$Kuupäev <- as.Date(tervise_logi$Kuupäev, "%d/%m/%y")
trenni_logi$Kuupäev <- as.Date(trenni_logi$Kuupäev, "%d/%m/%y")
harjutuste_andmebaas$Kehakaalu.kordaja <- ifelse(
  is.na(harjutuste_andmebaas$Kehakaalu.kordaja),
  0,
  harjutuste_andmebaas$Kehakaalu.kordaja
)

# Greasy hack, et ma näeks numbreid, kui pole kaua aega kaalnunud
tervise_logi$Kehakaal[nrow(tervise_logi)] <- ifelse(is.na(tail(tervise_logi$Kehakaal, n = 1)),
                                                    tail(na.trim(tervise_logi$Kehakaal), n = 1),
                                                    tail(tervise_logi$Kehakaal, n = 1))
# Use rollapply to calculate the moving average, ignoring NA values
tervise_logi$Kehakaal_interpolated <- na.approx(tervise_logi$Kehakaal, na.rm = FALSE)
tervise_logi$Kehakaal_MA <- rollapply(
  tervise_logi$Kehakaal_interpolated,
  width = 30,
  FUN = mean,
  fill = NA,
  align = "right"
)

# Tekita üks suur juicy dataframe, mis hoomab kõike.
merged_df <- trenni_logi %>%
  left_join(harjutuste_andmebaas, by = "Harjutus") %>%
  left_join(tervise_logi, by = "Kuupäev")

ui <- dashboardPage(
  dashboardHeader(title = "GAINZ"),
  
  dashboardSidebar(sidebarMenu(
    menuItem("Kehakaal", tabName = "kaal", icon = icon("bar-chart")),
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
          "Harjutus",
          choices = unique(trenni_logi$Harjutus),
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
          "Harjutus",
          choices = unique(trenni_logi$Harjutus),
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
    p <- ggplot(tervise_logi, aes(x = Kuupäev)) +
      geom_point(aes(y = Kehakaal), color = "grey", na.rm = TRUE) +
      geom_line(aes(y = Kehakaal_MA),
                color = "black",
                na.rm = TRUE) +
      labs(title = "Kehakaal with 30-Day Moving Average", x = "Date", y = "Kehakaal") +
      theme_minimal()
    ggplotly(p)
  })
  
  output$aktiivsusBarPlot <- renderPlotly({
    # Create a new column for the week number
    tervise_logi$Week <- format(as.Date(tervise_logi$Kuupäev), "%Y-%U")
    
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
                aes(x = Kuupäev, y = MaxTonnage, colour = TonnageCriteria)) +
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

#options(shiny.port = 6006)
shinyApp(ui = ui, server = server)