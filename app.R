# ---- Set Working Directory ----
setwd('/Users/stephanie/Desktop/BHDS2010/myeloma_dashboard/Myeloma_Data')

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
library(readxl)

# ---- Load and Preprocess Data ----
df1 <- read_excel("myeloma_data_region_state_year_age.xlsx")
df2 <- read_excel("myeloma_data_age_year.xlsx")
df3 <- read_excel("myeloma_data_year_state.xlsx")

cols_to_numeric <- c("count", "population", "crudert", "crudertlow95", "crudertupp95", "crudertse")

ordered_year <- c("1999","2000", "2001","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019","2020","2021")
ordered_agegroup <- c("30-34 years", "35-39 years", "40-44 years", "45-49 years", "50-54 years", "55-59 years", "60-64 years", "65-69 years", "70-74 years", "75-79 years", "80-84 years", "85+ years")
ordered_region <- c("Midwest", "Northeast", "South", "West")
ordered_state <- c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", "Delaware", "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming")

##########
df1[cols_to_numeric] <- lapply(df1[cols_to_numeric], function(col) {
  col <- ifelse(tolower(trimws(as.character(col))) %in% c("missing", "na", ""), NA, col)
  suppressWarnings(as.numeric(col))
})

df1_clean <- df1 %>%
  mutate(
    year = factor(year, levels = ordered_year),
    agegroup = factor(agegroup, levels = ordered_agegroup),
    region = factor(region, levels = ordered_region),
    state = factor(state, levels = ordered_state)
  )


##########

df2[cols_to_numeric] <- lapply(df2[cols_to_numeric], function(col) suppressWarnings(as.numeric(col)))

df2_clean <- df2 %>%
  filter(!is.na(year) & year != "Total") %>%
  mutate(
    year = factor(year),
    agegroup = factor(agegroup)
  )




#######
merged_df <- left_join(df1_clean, df2_clean, by = c("year", "agegroup"))
#######

# ---- UI Layout ----
ui <- fluidPage(
  useShinyjs(),
  titlePanel("Myeloma Trends Dashboard"),
  
  actionButton("toggleSidebar", "☰ Show/Hide Filters", class = "btn btn-primary mb-2"),
  
  div(id = "sidebarContainer",
      sidebarPanel(
        pickerInput("years", "Select Years:", 
                    choices = levels(df1_clean$year), 
                    selected = levels(df1_clean$year),
                    multiple = TRUE, 
                    options = list(`actions-box` = TRUE)),
        
        pickerInput("age_groups", "Select Age Groups:",
                    choices = levels(df1_clean$agegroup),
                    selected = levels(df1_clean$agegroup),
                    multiple = TRUE,
                    options = list(`actions-box` = TRUE)),
        
        checkboxInput("show_all_states", "Show All States", value = FALSE),
        
        pickerInput("region", "Select Region:",
                    choices = sort(unique(df1_clean$region)),
                    multiple = TRUE,
                    options = list(`actions-box` = TRUE)),
        
        pickerInput("state", "Select State:",
                    choices = NULL,
                    multiple = TRUE,
                    options = list(`actions-box` = TRUE)),
        
        checkboxInput("log_scale", "Log scale for counts", FALSE),
        br(),
        
        downloadButton("download_data", "Download Filtered CSV"),
        br(), br(),
        downloadButton("download_report", "Download PDF Report")
      )
  ),
  
  ####Need hover labels

  mainPanel(
    tabsetPanel(
      tabPanel("Line Chart: By Age Group", withSpinner(plotlyOutput("line_age"))),
      tabPanel("Line Chart: Yearly Totals", withSpinner(plotlyOutput("line_year"))),
      tabPanel("Histogram", withSpinner(plotlyOutput("hist_plot"))),
      tabPanel("Boxplot", withSpinner(plotlyOutput("box_plot"))),
      tabPanel("Heatmap", withSpinner(plotlyOutput("heatmap"))),
      tabPanel("Summary Stats", verbatimTextOutput("summary_stats")),
      tabPanel("Reactable Table", reactableOutput("data_table"))
    )
  )
)

# ---- Server Logic ----
server <- function(input, output, session) {
  
  observeEvent(input$toggleSidebar, {
    toggle("sidebarContainer")
  })
  
  # Reactive data filter
  filtered_data <- reactive({
    req(input$years, input$age_groups)
    df1_clean %>% filter(year %in% input$years, agegroup %in% input$age_groups) %>%
      mutate(
        agegroup = factor(agegroup, levels = ordered_agegroup),
        year = factor(year, levels = ordered_year)
      )
  })
  
  observe({
    if (input$show_all_states) {
      updatePickerInput(session, "state",
                        choices = ordered_state,
                        selected = ordered_state)
    } else {
      req(input$region)
      available_states <- df1_clean %>%
        filter(region %in% input$region) %>%
        pull(state) %>%
        unique() %>%
        sort()
      
      updatePickerInput(session, "state",
                        choices = available_states,
                        selected = available_states)
    }
  })
  
  output$line_age <- renderPlotly({
    summary_data_line_age <- filtered_data() %>%
      group_by(agegroup, year) %>%
      summarise(countyearage = sum(count, na.rm = TRUE), .groups = "drop") %>%
      mutate(
        agegroup = factor(agegroup, levels = ordered_agegroup),
        year = factor(year, levels = ordered_year)
      )
    
    gg <- ggplot(summary_data_line_age, aes(x = agegroup, y = countyearage, color = year, group = year)) +
      geom_line() +
      geom_point() +
      labs(title = "Myeloma Counts by Age Group", x = "Age Group", y = "Count") +
      theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
    if (input$log_scale) gg <- gg + scale_y_log10()
    ggplotly(gg)
  })
  
  output$line_year <- renderPlotly({
    totals <- filtered_data() %>% group_by(year) %>% summarise(total_count = sum(count, na.rm = TRUE))
    gg <- ggplot(totals, aes(x = year, y = total_count, group = 1)) +
      geom_line(color = "steelblue", size = 1) +
      geom_point(color = "steelblue", size = 2) +
      labs(title = "Total Myeloma Counts by Year", x = "Year", y = "Total Count") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    if (input$log_scale) gg <- gg + scale_y_log10()
    ggplotly(gg)
  })
  
    #add facted young has lower counts
    #k1: year & age
  output$hist_plot <- renderPlotly({
    hist_data <- filtered_data() %>%
      filter(!is.na(count)) %>%
      summarise(countyearage = sum(count, na.rm = TRUE), .groups = "drop") %>%
      mutate(agegroup = factor(agegroup, levels = ordered_agegroup))
    
    gg <- ggplot(hist_data, aes(x = countyearage, fill = agegroup, text = paste("Age Group:", as.character(agegroup), "<br>Count:", countyearage))) +
      geom_histogram(position = "identity", alpha = 0.4, binwidth = 100, color = "turquoise") +
      labs(
        title = "Overlayed Histogram of Myeloma Case Counts by Age Group",
        x = "Case Count",
        y = "Frequency",
        fill = "Age Group"
      ) +
      theme_minimal()
    ggplotly(gg,tooltip = "text")
  })
  
  # output$box_plot <- renderPlotly({
  #   summary_data <- filtered_data() %>%
  #     group_by(agegroup, year) %>%
  #    summarise(count = sum(count, na.rm =TRUE))
  #   
  #   gg <- ggplot(filtered_data(), aes(x = agegroup, y = count, fill = agegroup)) +
  #     geom_boxplot() +
  #     labs(title = "Boxplot of Myeloma Case Counts by Age Group", x = "Age Group", y = "Count") +
  #     theme_minimal()+
  #     theme(axis.text.x = element_text(angle = 45, hjust = 1))
  #   ggplotly(gg)
  # })
  
  # output$boxplot <- renderPlotly({
  #   box_data <- filtered_data() %>%
  #     filter(!is.na(count), !is.na(agegroup)) %>%
  #     mutate(agegroup = factor(agegroup, levels = ordered_agegroup))
  #   
  #   gg <- ggplot(box_data, aes(x = agegroup, y = count, fill = agegroup)) +
  #     geom_boxplot(outlier.shape = NA) +
  #     labs(
  #       title = "Myeloma Case Count Distribution by Age Group",
  #       x = "Age Group",
  #       y = "Case Count"
  #     ) +
  #     theme_minimal() +
  #     theme(axis.text.x = element_text(angle = 45, hjust = 1))
  #     theme(legend.position = "none")
  #   ggplotly(gg)
  # })
  
  # Boxplot of counts by age group
  output$boxplot_plot <- renderPlotly({
    data <- filtered_data() %>%
      filter(!is.na(count)), !is.na(agegroup)) %>%
    mutate(agegroup = factor(agegroup, levels = ordered_agegroup))
    p <- ggplot(data, aes(x = agegroup, y = count, fill = agegroup)) +
      geom_boxplot(outlier.shape = NA) +
      labs(title = "Boxplot of Case Counts by Age Group", x = "Age Group", y = "Case Count") +
      theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")
    
    ggplotly(p)
  })
  
  
  output$heatmap <- renderPlotly({
    heat_data <- filtered_data() %>%
      group_by(year, agegroup) %>%
      summarise(mean_rate = mean(crudert, na.rm = TRUE)) %>%
      ungroup()
    
    gg <- ggplot(heat_data, aes(x = year, y = agegroup, fill = mean_rate)) +
      geom_tile() +
      scale_fill_viridis_c(option = "C") +
      labs(title = "Heatmap of Crude Rates", x = "Year", y = "Age Group", fill = "Crude Rate") +
      theme_minimal()+
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    ggplotly(gg)
  })
  
  output$summary_stats <- renderPrint({
    summary(select(filtered_data(), count, crudert, crudertlow95, crudertupp95, crudertse))
  })
  
  output$data_table <- renderReactable({
    reactable(filtered_data(), searchable = TRUE, pagination = TRUE, highlight = TRUE)
  })
  
  output$download_data <- downloadHandler(
    filename = function() paste0("myeloma_filtered_", Sys.Date(), ".csv"),
    content = function(file) {
      write_csv(filtered_data(), file)
    }
  )
  
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

# Run the App
shinyApp(ui = ui, server = server)
