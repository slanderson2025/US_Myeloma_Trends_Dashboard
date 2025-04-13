# Set the working directory
setwd('/Users/stephanie/Desktop/BHDS2010/myeloma_dashboard')
# 
# # Confirms the working directory
getwd()

# Load required libraries
# install.packages(c("shiny", "dplyr", "ggplot2", "plotly", "DT", "readr", 
#   "viridis", "rmarkdown", "shinycssloaders", "shinyWidgets", "reactable","shinyjs"))

# ============================================
# 📦 Myeloma Trends Dashboard - Full Shiny App
# ============================================

# ---- Load Required Libraries ----
library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)
library(readr)
library(viridis)
library(rmarkdown)
library(shinycssloaders)
library(shinyWidgets)
library(reactable)
library(shinyjs)

# ---- Load and Preprocess Data ----
data <- read_csv("myeloma_data.csv", show_col_types = FALSE)

# Convert relevant columns to numeric
cols_to_numeric <- c("count", "crude_rate", "crude_rate_lower_95", "crude_rate_upper_95", "crude_rate_standard_error")
data[cols_to_numeric] <- lapply(data[cols_to_numeric], function(col) suppressWarnings(as.numeric(col)))

# Clean year and age group variables
data <- data %>% filter(!is.na(year) & year != "Total")
data$year <- as.factor(data$year)
data$age_group_in_years <- as.factor(data$age_group_in_years)

# ---- UI Layout ----
ui <- fluidPage(
  useShinyjs(),  # 👈 Initialize shinyjs
  
  titlePanel("📈 Myeloma Trends Dashboard"),
  
  # Toggle Button to show/hide sidebar
  actionButton("toggleSidebar", "☰ Show/Hide Filters", class = "btn btn-primary mb-2"),
  
  # Wrap sidebarPanel in a div for toggle control
  div(id = "sidebarContainer",
      sidebarPanel(
        pickerInput("years", "Select Years:", 
                    choices = levels(data$year), 
                    selected = levels(data$year),
                    multiple = TRUE, 
                    options = list(`actions-box` = TRUE)),
        
        pickerInput("age_groups", "Select Age Groups:",
                    choices = levels(data$age_group_in_years),
                    selected = levels(data$age_group_in_years),
                    multiple = TRUE,
                    options = list(`actions-box` = TRUE)),
        
        checkboxInput("log_scale", "Log scale for counts", FALSE),
        br(),
        
        downloadButton("download_data", "⬇ Download Filtered CSV"),
        br(), br(),
        downloadButton("download_report", "📝 Download PDF Report")
      )
  ),
  
  # Main Panel with tabbed outputs
  mainPanel(
    tabsetPanel(
      tabPanel("📉 Line Chart: By Age Group", withSpinner(plotlyOutput("line_age"))),
      tabPanel("📈 Line Chart: Yearly Totals", withSpinner(plotlyOutput("line_year"))),
      tabPanel("📊 Histogram", withSpinner(plotlyOutput("hist_plot"))),
      tabPanel("📦 Boxplot", withSpinner(plotlyOutput("box_plot"))),
      tabPanel("🌡 Heatmap", withSpinner(plotlyOutput("heatmap"))),
      tabPanel("🧮 Summary Stats", verbatimTextOutput("summary_stats")),
      tabPanel("📋 Reactable Table", reactableOutput("data_table"))
    )
  )
)

# ---- Server Logic ----
server <- function(input, output, session) {
  
  # Toggle sidebar visibility
  observeEvent(input$toggleSidebar, {
    toggle("sidebarContainer")  # shinyjs::toggle
  })
  
  # Reactive data filter
  filtered_data <- reactive({
    req(input$years, input$age_groups)
    data %>% filter(year %in% input$years, age_group_in_years %in% input$age_groups)
  })
  
  # Line Chart by Age Group
  output$line_age <- renderPlotly({
    gg <- ggplot(filtered_data(), aes(x = age_group_in_years, y = count, color = year, group = year)) +
      geom_line() + geom_point() +
      labs(title = "Myeloma Counts by Age Group", x = "Age Group", y = "Count") +
      theme_minimal()
    if (input$log_scale) gg <- gg + scale_y_log10()
    ggplotly(gg)
  })
  
  # Line Chart by Year Totals
  output$line_year <- renderPlotly({
    totals <- filtered_data() %>% group_by(year) %>% summarise(total_count = sum(count, na.rm = TRUE))
    gg <- ggplot(totals, aes(x = year, y = total_count, group = 1)) +
      geom_line() + geom_point() +
      labs(title = "Total Myeloma Counts by Year", x = "Year", y = "Total Count") +
      theme_minimal()
    if (input$log_scale) gg <- gg + scale_y_log10()
    ggplotly(gg)
  })
  
  # Histogram
  output$hist_plot <- renderPlotly({
    gg <- ggplot(filtered_data(), aes(x = count)) +
      geom_histogram(binwidth = 50, fill = "skyblue", color = "black") +
      labs(title = "Histogram of Myeloma Case Counts", x = "Case Count", y = "Frequency") +
      theme_minimal()
    ggplotly(gg)
  })
  
  # Boxplot
  output$box_plot <- renderPlotly({
    gg <- ggplot(filtered_data(), aes(x = age_group_in_years, y = count, fill = age_group_in_years)) +
      geom_boxplot() +
      labs(title = "Boxplot of Myeloma Case Counts by Age Group", x = "Age Group", y = "Count") +
      theme_minimal()
    ggplotly(gg)
  })
  
  # Heatmap
  output$heatmap <- renderPlotly({
    heat_data <- filtered_data() %>%
      group_by(year, age_group_in_years) %>%
      summarise(mean_rate = mean(crude_rate, na.rm = TRUE)) %>%
      ungroup()
    
    gg <- ggplot(heat_data, aes(x = year, y = age_group_in_years, fill = mean_rate)) +
      geom_tile() +
      scale_fill_viridis_c(option = "C") +
      labs(title = "Heatmap of Crude Rates", x = "Year", y = "Age Group", fill = "Crude Rate") +
      theme_minimal()
    ggplotly(gg)
  })
  
  # Summary Statistics
  output$summary_stats <- renderPrint({
    summary(select(filtered_data(), count, crude_rate, crude_rate_lower_95, crude_rate_upper_95, crude_rate_standard_error))
  })
  
  # Reactable Table
  output$data_table <- renderReactable({
    reactable(filtered_data(), searchable = TRUE, pagination = TRUE, highlight = TRUE)
  })
  
  # CSV Download
  output$download_data <- downloadHandler(
    filename = function() paste0("myeloma_filtered_", Sys.Date(), ".csv"),
    content = function(file) {
      write_csv(filtered_data(), file)
    }
  )
  
  # PDF Report Download via R Markdown
  output$download_report <- downloadHandler(
    filename = function() paste0("myeloma_report_", Sys.Date(), ".pdf"),
    content = function(file) {
      tempReport <- file.path(tempdir(), "report_template.Rmd")
      file.copy("report_template.Rmd", tempReport, overwrite = TRUE)
      
      params <- list(
        data = filtered_data(),
        years = input$years,
        age_groups = input$age_groups
      )
      
      rmarkdown::render(tempReport,
                        output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv()))
    }
  )
}

# ---- Run App ----
shinyApp(ui, server)