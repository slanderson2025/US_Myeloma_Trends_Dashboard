# ---- Set Working Directory ----
#setwd('/Set/Your/Working/Directory/Here')

# install.packages(c(
# "shiny", "dplyr", "ggplot2", "plotly", "DT",
# "readr", "viridis", "rmarkdown", "shinycssloaders",
# "shinyWidgets", "reactable", "shinyjs", "readxl"
# ))

# ---- Load Required Libraries ----
#library(shiny)
library(car)
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
library(shinythemes)
library(tinytex)
library(DiagrammeR)
library(knitr)
library(kableExtra)
library(tibble)


# ---- Load and Preprocess Data ----
df2 <- read_excel("myeloma_data_age_year.xlsx")
df3 <- read_excel("myeloma_data_year_state.xlsx")

cols_to_numeric <- c("count", "population", "crudert", "crudertlow95", "crudertupp95", "crudertse")

ordered_year <- c("1999","2000", "2001","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019","2020","2021")
ordered_agegroup <- c("30-34 years", "35-39 years", "40-44 years", "45-49 years", "50-54 years", "55-59 years", "60-64 years", "65-69 years", "70-74 years", "75-79 years", "80-84 years", "85+ years")
ordered_region <- c("Midwest", "Northeast", "South", "West")
ordered_state <- c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", "Delaware", "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming")


######
# df2 (below) information:
# Dataset: United States and Puerto Rico Cancer Statistics, 1999–2021 Incidence  
# Cancer Site: Myeloma  
# Query Parameters:  
# - Age Groups: 30–34 through 85+ years  
# - Grouped By: Year, Age Group  
# - Rates per 100,000 (Standard Population: 2000 U.S. Std. Million)  
# - Show Totals: Disabled  
# - Show Zero Values: Enabled  
# - Show Suppressed: Enabled  
# 
# For more details, see: [CDC WONDER Help](http://wonder.cdc.gov/wonder/help/cancer-v2021.html)  
# Query Date: April 18, 2025
# ")
df2[cols_to_numeric] <- lapply(df2[cols_to_numeric], 
                               function(col) suppressWarnings(as.numeric(col)))

df2_clean <- df2 %>%
  mutate(
    year = factor(year, levels = ordered_year),
    agegroup = factor(agegroup, levels = ordered_agegroup)
  )

######
# df3 (below) information:
# Dataset: United States and Puerto Rico Cancer Statistics, 1999–2021 Incidence  
# Cancer Site: Myeloma  
# Query Parameters:  
# - Age Groups: 30–34 through 85+ years  
# - Grouped By: Year, States  
# - Rates per 100,000 (Standard Population: 2000 U.S. Std. Million)  
# - Show Totals: Disabled  
# - Show Zero Values: Enabled  
# - Show Suppressed: Enabled  
# 
# For more details, see: [CDC WONDER Help](http://wonder.cdc.gov/wonder/help/cancer-v2021.html)  
# Query Date: April 18, 2025
# ")
# ####
# 
cols_to_numeric <- c("Count", "Population", "Crude Rate", "Crude Rate Lower 95% Confidence Interval", "Crude Rate Upper 95% Confidence Interval", "Crude Rate Standard Error")

df3[cols_to_numeric] <- lapply(df3[cols_to_numeric], 
                               function(col) suppressWarnings(as.numeric(col)))

df3_clean <- df3 %>%
  mutate(
    Year = as.integer(Year),
    States = as.factor(States),
    `Crude Rate` = suppressWarnings(as.numeric(`Crude Rate`))
  ) %>%
  filter(!is.na(`Crude Rate`))



# ---- UI Layout ----
ui <- fluidPage(
  useShinyjs(),
  
  titlePanel("U.S. Myeloma Trends Dashboard"),
  
  actionButton("toggleSidebar", "☰ Show/Hide Filters", class = "btn btn-primary mb-2"),
  
  # ---- Main layout with sidebar and content side-by-side ----
  fluidRow(
    # ---- Sidebar Column (Left) ----
    column(
      width = 3,
      div(id = "sidebarContainer",
          # Box 1: Age Group Filters
          conditionalPanel(
            condition = "input.main_tabs != 'State Comparison'",
            div(
              style = "background-color: #f5f5f5; padding: 15px; border-radius: 5px; margin-bottom: 15px;",
              h4("Trends by Age Group"),
              pickerInput("years", "Select Years:",
                          choices = levels(df2_clean$year),
                          selected = levels(df2_clean$year),
                          multiple = TRUE,
                          options = list(`actions-box` = TRUE)),
              pickerInput("age_groups", "Select Age Groups:",
                          choices = levels(df2_clean$agegroup),
                          selected = levels(df2_clean$agegroup),
                          multiple = TRUE,
                          options = list(`actions-box` = TRUE)),
              checkboxInput("log_scale", "Log Scale for Counts", FALSE)
            )
          ),
          br(),
          
          # Box 2: State Comparison Filters
          conditionalPanel(
            condition = "input.main_tabs == 'State Comparison'",
            div(
              style = "background-color: #f5f5f5; padding: 15px; border-radius: 5px; margin-bottom: 15px;",
              h4("Myeloma Crude Rate Distribution by State"),
              pickerInput("selected_states_box", "Select 2–5 States to Compare:",
                          choices = NULL,
                          multiple = TRUE,
                          options = list(`max-options` = 5, `actions-box` = TRUE))
            )
          ),
          
          # Box 3: Downloads
          div(
            style = "background-color: #f5f5f5; padding: 15px; border-radius: 5px;",
            h4("Download"),
            downloadButton("download_data", "Download: Year–Age Data"),
            br(), br(),
            downloadButton("download_state_data", "Download: Year–State Data"),
            br(), br(),
            downloadButton("download_report", "Download PDF Report"),
            br(),br(),
            tags$small(
              style = "color: red;",
              "Please select 2–5 states in State Comparison tab to enable Year–State data and PDF report downloads."
            )
          )
      )
    ),
    
    # ---- Main Panel Column (Right) ----
    column(
      width = 9,
      tabsetPanel(
        id = "main_tabs",
        
        tabPanel("Line Graph: Myeloma Incidence by Age Group",
                 withSpinner(plotlyOutput("line_age"))),
        
        tabPanel("Line Graph: Myeloma Incidence by Year",
                 withSpinner(plotlyOutput("line_year"))),
        
        tabPanel("Histograms",
                 fluidPage(
                   radioButtons("histogram_view", "Select Histogram View:",
                                choices = c("Overlay" = "overlay", "Faceted" = "faceted"),
                                inline = TRUE,
                                selected = "overlay"),
                   br(),
                   h4("Histogram of Myeloma Incidence"),
                   withSpinner(plotlyOutput("histogram_plot", height = "600px"))
                 )
        ),
        
        tabPanel("Box Plot",
                 fluidPage(
                   h4("Myeloma Counts by Age Group"),
                   withSpinner(plotlyOutput("box_plot", height = "600px")),
                   hr(),
                   h4("Summary Statistics Table"),
                   reactableOutput("box_summary_table")
                 )
        ),
        
        tabPanel("Heatmap",
                 withSpinner(plotlyOutput("heatmap"))),
        
      #  tabPanel("Summary Statstics",
      #          tableOutput("UI_summary_stats")),
        
      #  tabPanel("Data Table",
      #           reactableOutput("data_table")),
        
        tabPanel("State Comparison",
                 fluidPage(
                   plotlyOutput("state_box_plot"),
                   br(),
                   div(id = "state_test_outputs",
                       conditionalPanel(
                         condition = "input.selected_states_box.length == 2",
                         h4("Shapiro–Wilk Normality Test"),
                         reactableOutput("shapiro_table"),
                         h4("Levene’s Test for Equal Variance"),
                         reactableOutput("levene_table"),
                         h4("Selected Test Result"),
                         reactableOutput("main_test_table")
                       ),
                       conditionalPanel(
                         condition = "input.selected_states_box.length > 2",
                         h4("Shapiro–Wilk Normality Test by State"),
                         reactableOutput("multi_shapiro_table"),
                         h4("Levene’s Test for Equal Variance"),
                         reactableOutput("multi_levene_table"),
                         h4("Selected Test Result"),
                         reactableOutput("multi_test_table"),
                         uiOutput("tukey_section")
                       )
                   ),
                   hr(),
                   h4("Test Selection Logic"),
                   fluidRow(
                     column(
                       width = 6,
                       h5("For 2-State Comparison"),
                       grVizOutput("decision_tree_2", height = "350px")
                     ),
                     column(
                       width = 6,
                       h5("For 3–5 State Comparison"),
                       grVizOutput("decision_tree_multi", height = "350px")
                     )
                   )
                 )
        )
      )
    )
  ),
  
  # ---- Footer ----
  tags$footer(
    style = "text-align: center; padding: 10px; font-size: 12px; color: #666;",
    "Data Source: CDC WONDER. U.S. Cancer Statistics Public Use Database (1999–2021), ",
    tags$a(href = "https://wonder.cdc.gov/cancer.html",
           "https://wonder.cdc.gov/cancer.html", target = "_blank")
  )
)
# ---- Server Logic ----
server <- function(input, output, session) {
  
  observeEvent(input$toggleSidebar, {
    toggle("sidebarContainer")
  })
  
  # Reactive data filter (data by year and age group only)
  filtered_data <- reactive({
    req(input$years, input$age_groups)
    df2_clean %>% filter(year %in% input$years, agegroup %in% input$age_groups) %>%
      mutate(
        agegroup = factor(agegroup, levels = ordered_agegroup),
        year = factor(year, levels = ordered_year)
      )
  })
  
  
  
  ###############################################  
  #### Line Chart: Myeloma Incidence by Age Group 
  ###############################################  
  # 1. Create a reactive expression for the plot and the file path
  plot_and_path <- reactive({
    summary_data_line_age <- filtered_data() %>%
      group_by(agegroup, year) %>%
      summarise(count = sum(count, na.rm = TRUE), .groups = "drop") %>%
      mutate(
        agegroup = factor(agegroup, levels = ordered_agegroup),
        year = factor(year, levels = ordered_year)
      )
    
    gg_line_age_plot <- ggplot(summary_data_line_age, aes(
      x = agegroup,
      y = count,
      color = year,
      group = year,
      text = paste0("Year: ", year, "<br>Age Group: ", agegroup, "<br>Count: ", count)
    )) +
      geom_line() +
      geom_point() +
      #  labs(title = "Line Graph: Myeloma Incidence by Age Group", 
      labs( 
        x = "Age Group", 
        y = "Count") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            plot.title = element_text(hjust = 0.5))  # Center the title
    
    if (input$log_scale) gg_line_age_plot <- gg_line_age_plot + scale_y_log10()
    
    # Save to PNG for PDF use
    path <- file.path(tempdir(), "gg_line_age_plot.png")
    ggsave(path, plot = gg_line_age_plot, width = 6.5, height = 5)
    
    list(
      plot = gg_line_age_plot,
      path = path
    )
  })
  
  # 2. Render to UI (interactive plot)
  output$line_age <- renderPlotly({
    ggplotly(plot_and_path()$plot, tooltip = "text")
  })
  ###############################################  
  
  
  
  ###############################################  
  #### Line Chart: Myeloma Incidence by Year  
  ###############################################  
  plot_and_path_year <- reactive({
    totals <- filtered_data() %>% 
      group_by(year) %>% 
      summarise(total_count = sum(count, na.rm = TRUE), .groups = "drop")
    
    gg_line_year_plot <- ggplot(totals, aes(x = year, y = total_count, group = 1)) +
      geom_line(color = "steelblue", size = 1) +
      geom_point(color = "steelblue", size = 2) +
      labs(
        #   title = "Line Graph: Total Myeloma Incidence by Year",
        x = "Year",
        y = "Total Count"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5)  # Center the title
      )
    
    if (input$log_scale) {
      gg_line_year_plot <- gg_line_year_plot + scale_y_log10()
    }
    
    # Save PNG to temporary path for use in PDF
    path <- file.path(tempdir(), "gg_line_year_plot.png")
    ggsave(path, plot = gg_line_year_plot, width = 6.5, height = 5)
    
    list(
      plot = gg_line_year_plot,
      path = path
    )
  })
  
  output$line_year <- renderPlotly({
    ggplotly(plot_and_path_year()$plot)
  })
  
  output$histogram_plot <- renderPlotly({
    ggplotly(plot_and_path_hist()$plot, tooltip = "text")
  })
  ###############################################  
  
  ###############################################  
  #### Histograms 
  ###############################################  
  
  # Define the data for the histograms (reactive):
  hist_data <- reactive({
    req(filtered_data())
    
    filtered_data() %>%
      filter(!is.na(count) & !is.na(agegroup)) %>%
      mutate(agegroup = factor(agegroup, levels = ordered_agegroup))
  })
  
  # Create the overlay histogram (reactive):
  hist_overlay_plot <- reactive({
    req(hist_data())
    
    ggplot(hist_data(), aes(
      x = count,
      fill = agegroup,
      text = paste("Age Group:", as.character(agegroup), "<br>Count:", count)
    )) +
      geom_histogram(binwidth = 500, color = "darkturquoise") +
      labs(
        #title = "Overlay Histogram of Myeloma Incidence by Age Group",
        x = "Case Count",
        y = "Frequency",
        fill = "Age Group"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            plot.title = element_text(hjust = 0.5))
  })
  
  # Create the faceted histogram (reactive):
  hist_faceted_plot <- reactive({
    req(hist_data())
    
    ggplot(hist_data(), aes(
      x = count,
      fill = agegroup,
      text = paste("Age Group:", as.character(agegroup), "<br>Count:", count)
    )) +
      geom_histogram(binwidth = 500, color = "darkturquoise") +
      facet_wrap(~ agegroup, scales = "free_y") +
      labs(
        # title = "Faceted Histogram of Myeloma Incidence by Age Group",
        x = "Case Count",
        y = "Frequency",
        fill = "Age Group"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            plot.title = element_text(hjust = 0.5))
  })
  
  #Render the appropriate plot based on the radio button selection:
  output$histogram_plot <- renderPlotly({
    req(input$histogram_view)
    
    if (input$histogram_view == "overlay") {
      ggplotly(hist_overlay_plot(), tooltip = "text")
    } else {
      ggplotly(hist_faceted_plot(), tooltip = "text")
    }
  })
  
  # Overlay Histogram + PNG Path
  hist_overlay_plot_and_path <- reactive({
    req(hist_overlay_plot())
    
    path <- file.path(tempdir(), "hist_overlay_plot.png")
    ggsave(path, plot = hist_overlay_plot(), width = 6.5, height = 5)
    
    list(
      plot = hist_overlay_plot(),
      path = path
    )
  })
  
  # Faceted Histogram + PNG Path
  hist_faceted_plot_and_path <- reactive({
    req(hist_faceted_plot())
    
    path <- file.path(tempdir(), "hist_faceted_plot.png")
    ggsave(path, plot = hist_faceted_plot(), width = 6.5, height = 5)
    
    list(
      plot = hist_faceted_plot(),
      path = path
    )
  })
  ###############################################  
  
  
  
  ###############################################  
  #### Boxplot
  ###############################################  
  box_plot_and_path <- reactive({
    req(filtered_data())
    
    box_data <- filtered_data() %>%
      group_by(agegroup, year) %>%
      summarise(countyearage = sum(count, na.rm = TRUE), .groups = "drop") %>%
      mutate(agegroup = factor(agegroup, levels = ordered_agegroup))
    
    gg_box <- ggplot(box_data, aes(
      x = agegroup,
      y = countyearage,
      fill = agegroup
      #,
      # text = paste(
      #   "Age Group:", as.character(agegroup),
      #   "<br>Year:", year,
      #   "<br>Count:", countyearage
      # )
    )) +
      geom_boxplot() +
      # (outlier.shape = NA) +
      labs(
        #title = "Myeloma Counts by Age Group",
        x = "Age Group",
        y = "Count"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    if (input$log_scale) {
      gg_box <- gg_box + scale_y_log10()
    }
    
    # Save plot to a temporary PNG file
    path <- file.path(tempdir(), "gg_box_plot.png")
    ggsave(path, plot = gg_box, width = 6.5, height = 5)
    
    list(
      plot = gg_box,
      path = path
    )
  })
  
  
  output$box_plot <- renderPlotly({
    ggplotly(box_plot_and_path()$plot, tooltip = c("x", "y", "fill"))
  })
  
  output$box_summary_table <- renderReactable({
    summary_stats <- filtered_data() %>%
      group_by(agegroup) %>%
      summarise(
        Mean = round(mean(count, na.rm = TRUE), 2),
        Median = round(median(count, na.rm = TRUE), 2),
        Min = min(count, na.rm = TRUE),
        Max = max(count, na.rm = TRUE),
        SD = round(sd(count, na.rm = TRUE), 2),
        .groups = "drop"
      ) %>%
      mutate(agegroup = factor(agegroup, levels = ordered_agegroup)) %>%
      arrange(agegroup)
    
    reactable::reactable(summary_stats,
                         columns = list(
                           agegroup = colDef(name = "Age Group"),
                           Mean = colDef(align = "center"),
                           Median = colDef(align = "center"),
                           Min = colDef(align = "center"),
                           Max = colDef(align = "center"),
                           SD = colDef(name = "SD", align = "center")
                         ),
                         bordered = TRUE,
                         striped = TRUE,
                         highlight = TRUE,
                         defaultPageSize = 10
    )
  })
  
  summary_stats_data <- reactive({
    filtered_data() %>%
      group_by(agegroup) %>%
      summarise(
        Mean = round(mean(count, na.rm = TRUE), 2),
        Median = round(median(count, na.rm = TRUE), 2),
        Min = min(count, na.rm = TRUE),
        Max = max(count, na.rm = TRUE),
        SD = round(sd(count, na.rm = TRUE), 2),
        .groups = "drop"
      ) %>%
      mutate(`Age Group` = factor(agegroup, levels = ordered_agegroup)) %>%
      select(`Age Group`, everything(), -agegroup) %>%
      arrange(`Age Group`)
  })
  
  ###############################################  
  
  
  ###############################################  
  #### Heatmap
  ###############################################  
  # 1. Create a reactive expression for heatmap and its PNG file path
  heatmap_and_path <- reactive({
    heat_data <- filtered_data() %>%
      group_by(year, agegroup) %>%
      summarise(mean_rate = mean(crudert, na.rm = TRUE), .groups = "drop") %>%
      mutate(
        agegroup = factor(agegroup, levels = ordered_agegroup),
        year = factor(year, levels = ordered_year)
      )
    
    gg_heat <- ggplot(heat_data, aes(x = year, y = agegroup, fill = mean_rate)) +
      geom_tile(color = "white") +
      scale_fill_viridis_c(option = "C") +
      labs(
        # title = "Heatmap of Crude Rates",
        x = "Year",
        y = "Age Group",
        fill = "Crude Rate"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5)
      )
    
    # Save to PNG for PDF
    path <- file.path(tempdir(), "gg_heatmap_plot.png")
    ggsave(path, plot = gg_heat, width = 6.5, height = 5)
    
    list(
      plot = gg_heat,
      path = path
    )
  })
  
  # 2. Render interactive plot in UI
  output$heatmap <- renderPlotly({
    ggplotly(heatmap_and_path()$plot)
  })
  ###############################################  
  
  ###############################################  
  #### Summary Stats
  ###############################################  
  # 1. Reactive summary for Shiny UI
  output$summary_stats <- renderPrint({
    summary(select(filtered_data(), count, crudert, crudertlow95, crudertupp95, crudertse))
  })
  
  # 2. Reactive summary as data frame for display or downstream use
  summary_stats_df <- reactive({
    filtered_data() %>%
      select(count, crudert, crudertlow95, crudertupp95, crudertse) %>%
      summary() %>%
      as.data.frame.matrix()
  })
  
  
  output$UI_summary_stats <- renderTable({
    # Create formatted summary data
    UI_summary_stats <- filtered_data() %>%
      dplyr::select(count, crudert, crudertlow95, crudertupp95, crudertse) %>%
      summary() %>%
      as.data.frame.matrix()
    
    # Set appropriate row names (not used anymore since we're dropping the column)
    stat_labels <- c("Min.", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max.")
    rownames(UI_summary_stats) <- stat_labels
    
    # Rename columns
    colnames(UI_summary_stats) <- c(
      "Count Summary", "Crude Rate", "Lower 95% CI", "Upper 95% CI", "Standard Error"
    )
    
    # Do NOT convert rownames to column — just return the matrix without Statistic column
    UI_summary_stats
  }, striped = TRUE, bordered = TRUE, spacing = "xs", align = "c")
  
  UI_summary_stats_df <- reactive({
    filtered_data() %>%
      dplyr::select(count, crudert, crudertlow95, crudertupp95, crudertse) %>%
      summary() %>%
      as.data.frame.matrix() %>%
      `colnames<-`(c(
        "Count Summary", "Crude Rate", "Lower 95% CI", "Upper 95% CI", "Standard Error"
      ))
  })
  ###############################################  
  
  ###############################################  
  #### React Table
  ###############################################  
  output$data_table <- renderReactable({
    reactable(
      filtered_data() %>% select(-agegroupcd),
      columns = list(
        agegroup    = colDef(name = "Age Group"),
        year        = colDef(name = "Year"),
        count       = colDef(name = "Incidence"),
        population  = colDef(name = "Population"),
        crudert     = colDef(name = "Crude Rate"),
        crudertlow95 = colDef(name = "95% CI Lower"),
        crudertupp95 = colDef(name = "95% CI Upper"),
        crudertse   = colDef(name = "Standard Error")
      ),
      bordered = TRUE,
      searchable = TRUE, 
      pagination = TRUE, 
      highlight = TRUE)
  })
  ###############################################  
  
  
  ###############################################  
  #### Sate Comparison Boxplots
  ###############################################  
  # Populate state picker for Side by Side boxplots
  observe({
    updatePickerInput(session, "selected_states_box", choices = 
                        sort(unique(df3_clean$States)))
  })
  
  
  # ---- State Side by Side Box Plots ----
  output$state_box_plot <- renderPlotly({
    box_states_data <- df3_clean %>% filter(States %in% input$selected_states_box)
    req(nrow(box_states_data) > 1)
    
    gg <- ggplot(box_states_data, aes(
      x = States,
      y = `Crude Rate`,
      fill = States,
      text = paste("State:", States, "<br>Year:", Year, "<br>Crude Rate:", round(`Crude Rate`, 2))
    )) +
      geom_boxplot(outlier.shape = NA, alpha = 0.7) +
      theme_minimal() +
      labs(
        title = "Crude Rate Distribution by State",
        x = "State",
        y = "Crude Rate per 100,000"
      )
    
    ggplotly(gg, tooltip = "text")
  })
  
  # ---- Two-State Shapiro Test ----
  output$shapiro_table <- renderReactable({
    if (length(input$selected_states_box) != 2) return(NULL)
    
    state1 <- input$selected_states_box[1]
    state2 <- input$selected_states_box[2]
    test_data <- df3_clean %>% filter(States %in% c(state1, state2))
    
    group1 <- test_data %>% filter(States == state1) %>% pull(`Crude Rate`)
    group2 <- test_data %>% filter(States == state2) %>% pull(`Crude Rate`)
    
    sw1 <- shapiro.test(group1)
    sw2 <- shapiro.test(group2)
    
    data.frame(
      State = c(state1, state2),
      `Shapiro-Wilk p-value` = round(c(sw1$p.value, sw2$p.value), 4)
    ) %>% reactable(bordered = TRUE, striped = TRUE, highlight = TRUE)
  })
  
  # ---- Two-State Levene's Test ----
  output$levene_table <- renderReactable({
    if (length(input$selected_states_box) != 2) return(NULL)
    
    state1 <- input$selected_states_box[1]
    state2 <- input$selected_states_box[2]
    test_data <- df3_clean %>% filter(States %in% c(state1, state2))
    
    group1 <- test_data %>% filter(States == state1) %>% pull(`Crude Rate`)
    group2 <- test_data %>% filter(States == state2) %>% pull(`Crude Rate`)
    
    sw1 <- shapiro.test(group1)
    sw2 <- shapiro.test(group2)
    
    levene_df <- data.frame(
      group = factor(c(rep(state1, length(group1)), rep(state2, length(group2)))),
      value = c(group1, group2)
    )
    
    levene_result <- car::leveneTest(value ~ group, data = levene_df)
    p_levene <- levene_result$`Pr(>F)`[1]
    
    data.frame(
      `F value` = round(levene_result$`F value`[1], 4),
      `p-value` = round(p_levene, 4)
    ) %>% reactable(bordered = TRUE, striped = TRUE)
  })
  
  
  # ---- Two-State Main Test Table ----
  output$main_test_table <- renderReactable({
    if (length(input$selected_states_box) != 2) return(NULL)
    
    state1 <- input$selected_states_box[1]
    state2 <- input$selected_states_box[2]
    test_data <- df3_clean %>% filter(States %in% c(state1, state2))
    
    group1 <- test_data %>% filter(States == state1) %>% pull(`Crude Rate`)
    group2 <- test_data %>% filter(States == state2) %>% pull(`Crude Rate`)
    
    sw1 <- shapiro.test(group1)
    sw2 <- shapiro.test(group2)
    
    if (sw1$p.value >= 0.05 && sw2$p.value >= 0.05) {
      levene_df <- data.frame(
        group = factor(c(rep(state1, length(group1)), rep(state2, length(group2)))),
        value = c(group1, group2)
      )
      p_levene <- car::leveneTest(value ~ group, data = levene_df)$`Pr(>F)`[1]
      
      ttest <- t.test(group1, group2, var.equal = (p_levene >= 0.05))
      test_label <- ifelse(p_levene >= 0.05, "Student's t-test", "Welch's t-test")
      
      data.frame(
        Test = test_label,
        `t Statistic` = round(ttest$statistic, 4),
        `p-value` = round(ttest$p.value, 4)
      ) %>% reactable(bordered = TRUE, striped = TRUE)
    } else {
      wtest <- wilcox.test(group1, group2)
      data.frame(
        Test = "Wilcoxon Rank-Sum Test",
        `W Statistic` = round(wtest$statistic, 4),
        `p-value` = round(wtest$p.value, 4)
      ) %>% reactable(bordered = TRUE, striped = TRUE)
    }
  })
  
  # ---- Multi-State Shapiro Test ----
  output$multi_shapiro_table <- renderReactable({
    if (length(input$selected_states_box) <= 2) return(NULL)
    
    test_data <- df3_clean %>% filter(States %in% input$selected_states_box)
    
    normality_results <- test_data %>%
      group_by(States) %>%
      summarise(p_value = round(shapiro.test(`Crude Rate`)$p.value, 4), .groups = "drop")
    
    normality_results %>%
      rename(`Shapiro-Wilk p-value` = p_value) %>%
      reactable(bordered = TRUE, striped = TRUE)
  })
  
  # ---- Multi-State Levene's Test ----
  output$multi_levene_table <- renderReactable({
    if (length(input$selected_states_box) <= 2) return(NULL)
    
    test_data <- df3_clean %>% filter(States %in% input$selected_states_box)
    levene_result <- car::leveneTest(`Crude Rate` ~ States, data = test_data)
    
    data.frame(
      `F value` = round(levene_result$`F value`[1], 4),
      `p-value` = round(levene_result$`Pr(>F)`[1], 4)
    ) %>% reactable(bordered = TRUE, striped = TRUE)
  })
  
  
  # ---- Multi-State Main Test Table ----
  output$multi_test_table <- renderReactable({
    if (length(input$selected_states_box) <= 2) return(NULL)
    
    test_data <- df3_clean %>% filter(States %in% input$selected_states_box)
    normality_results <- test_data %>%
      group_by(States) %>%
      summarise(p_value = shapiro.test(`Crude Rate`)$p.value, .groups = "drop")
    levene_result <- car::leveneTest(`Crude Rate` ~ States, data = test_data)
    
    if (any(normality_results$p_value < 0.05) || levene_result$`Pr(>F)`[1] < 0.05) {
      kruskal <- kruskal.test(`Crude Rate` ~ States, data = test_data)
      data.frame(
        Test = "Kruskal–Wallis Test",
        `Chi-squared` = round(kruskal$statistic, 4),
        `p-value` = round(kruskal$p.value, 4)
      ) %>% reactable(bordered = TRUE, striped = TRUE)
    } else {
      anova_result <- aov(`Crude Rate` ~ States, data = test_data)
      data.frame(
        Test = "One-Way ANOVA",
        `F value` = round(summary(anova_result)[[1]]$`F value`[1], 4),
        `p-value` = round(summary(anova_result)[[1]]$`Pr(>F)`[1], 4)
      ) %>% reactable(bordered = TRUE, striped = TRUE)
    }
  })
  
  # ---- Multi-State Tukey Results Table ---- 
  
  output$tukey_table <- renderReactable({
    if (length(input$selected_states_box) <= 2) return(NULL)
    
    test_data <- df3_clean %>% filter(States %in% input$selected_states_box)
    
    normality_results <- test_data %>%
      group_by(States) %>%
      summarise(p_value = shapiro.test(`Crude Rate`)$p.value, .groups = "drop")
    
    levene_p <- car::leveneTest(`Crude Rate` ~ States, data = test_data)$`Pr(>F)`[1]
    
    # Only proceed with Tukey test if ANOVA assumptions are met
    if (all(normality_results$p_value >= 0.05) && levene_p >= 0.05) {
      anova_result <- aov(`Crude Rate` ~ States, data = test_data)
      
      tukey_df <- as.data.frame(TukeyHSD(anova_result)$States) %>%
        tibble::rownames_to_column(var = "Comparison") %>%
        rename(
          `Mean Difference` = diff,
          `Lower CI` = lwr,
          `Upper CI` = upr,
          `Adjusted p-value` = `p adj`
        ) %>%
        mutate(across(where(is.numeric), ~ round(.x, 4)))
      
      reactable(
        tukey_df,
        columns = list(
          Comparison = colDef(name = "State Comparison"),
          `Adjusted p-value` = colDef(cell = function(value) {
            if (value < 0.05) htmltools::strong(paste0(value, " *")) else value
          })
        ),
        bordered = TRUE,
        striped = TRUE,
        highlight = TRUE
      )
    } else {
      return(NULL)  # Don't show the table at all
    }
  })
  
  #section below only confirms that Tukey table will only be shown if assumptions are met to proceed with ANOVA
  
  output$tukey_section <- renderUI({
    req(length(input$selected_states_box) > 2)
    
    test_data <- df3_clean %>% filter(States %in% input$selected_states_box)
    
    normality_results <- test_data %>%
      group_by(States) %>%
      summarise(p_value = shapiro.test(`Crude Rate`)$p.value, .groups = "drop")
    
    levene_p <- car::leveneTest(`Crude Rate` ~ States, data = test_data)$`Pr(>F)`[1]
    
    # Only show Tukey section if ANOVA assumptions are met
    if (all(normality_results$p_value >= 0.05) && levene_p >= 0.05) {
      tagList(
        h4("Tukey HSD Post-hoc Comparison"),
        reactableOutput("tukey_table")
      )
    } else {
      return(NULL)
    }
  })
  ##  ---- Two-State Decision Tree ---- 
  
  output$decision_tree_2 <- renderGrViz({
    grViz("
    digraph tree_2 {
      graph [layout = dot, rankdir = TB]
      node [shape = box, style=filled, fillcolor=white, fontname = Helvetica]

      A [label = 'Normality Test\n(Shapiro–Wilk)']
      B1 [label = 'Both Normal']
      B2 [label = 'One or Both Not Normal']
      C1 [label = 'Variance Test\n(Levene’s)']
      D1 [label = 'Equal Variance']
      D2 [label = 'Unequal Variance']
      E1 [label = 'Student’s t-test', fillcolor = lightblue]
      E2 [label = 'Welch’s t-test', fillcolor = lightblue]
      E3 [label = 'Wilcoxon Rank-Sum Test', fillcolor = lightblue]

      A -> B1
      A -> B2
      B1 -> C1
      B2 -> E3
      C1 -> D1
      C1 -> D2
      D1 -> E1
      D2 -> E2
    }
  ")
  })
  
  # ---- Multi-State Decision Tree ---- 
  
  output$decision_tree_multi <- renderGrViz({
    grViz("
    digraph decision_tree_multi {
      graph [layout = dot, rankdir = TB]
      node [shape = box, style = filled, fillcolor = white, fontname = Helvetica]

      A [label = 'Normality Test\\n(Shapiro–Wilk)']
      B1 [label = 'All Groups Normal']
      B2 [label = 'One or More Not Normal']
      C [label = 'Variance Test\\n(Levene’s)']
      D1 [label = 'Equal Variance']
      D2 [label = 'Unequal Variance']
      E1 [label = 'One-Way ANOVA', fillcolor = lightblue]
      E2 [label = 'Kruskal–Wallis Test', fillcolor = lightblue]
      F [label = 'Tukey HSD Post-hoc', fillcolor = lightblue]

      A -> B1
      A -> B2
      B1 -> C
      B2 -> E2
      C -> D1
      C -> D2
      D1 -> E1
      D2 -> E2
      E1 -> F
    }
  ")
  })
  ###############################################  
  ###### Outputs
  ###############################################  
  # CSV Download
  output$download_data <- downloadHandler(
    filename = function() paste0("myeloma_age_data_", Sys.Date(), ".csv"),
    content = function(file) {
      req(filtered_data())
      write_csv(filtered_data(), file)
    }
  )
  
  output$download_state_data <- downloadHandler(
    filename = function() {
      paste0("myeloma_state_data_", Sys.Date(), ".csv")
    },
    content = function(file) {
      # Require at least one selected state
      req(input$selected_states_box)
      
      # Filter and write the data
      box_states_data <- df3_clean %>% filter(States %in% input$selected_states_box)
      write_csv(box_states_data, file)
    }
  )
  
  # PDF Report Download
  output$download_report <- downloadHandler(
    filename = function() {
      paste0("myeloma_report_", Sys.Date(), ".pdf")
    },
    content = function(file) {
      tempReport <- file.path(tempdir(), "myeloma_report.Rmd")
      file.copy("myeloma_report.Rmd", tempReport, overwrite = TRUE)
      
      box_states_data <- df3_clean %>% filter(States %in% input$selected_states_box)
      state_boxplot_static <- ggplot(box_states_data, aes(
        x = States,
        y = `Crude Rate`,
        fill = States)) +
        geom_boxplot(outlier.shape = NA, alpha = 0.7) +
        theme_minimal() +
        labs(
          # title = "Myeloma Crude Rate Distribution by State",
          x = "State",
          y = "Crude Rate per 100,000"
        )
      
      plot_file <- file.path(tempdir(), "state_boxplot_static.png")
      ggsave(plot_file, plot = state_boxplot_static, width = 8, height = 5)
      
      if (length(input$selected_states_box) == 2) {
        state1 <- input$selected_states_box[1]
        state2 <- input$selected_states_box[2]
        
        test_data <- df3_clean %>% filter(States %in% c(state1, state2))
        group1 <- test_data %>% filter(States == state1) %>% pull(`Crude Rate`)
        group2 <- test_data %>% filter(States == state2) %>% pull(`Crude Rate`)
        
        sw1 <- shapiro.test(group1)
        sw2 <- shapiro.test(group2)
        
        shapiro_df <- data.frame(
          State = c(state1, state2),
          `Shapiro-Wilk p-value` = signif(c(sw1$p.value, sw2$p.value), 4)
        )
      } else {
        shapiro_df <- NULL
      }
      
      box_summary_table <- filtered_data() %>%
        group_by(agegroup) %>%
        summarise(
          Mean = round(mean(count, na.rm = TRUE), 2),
          Median = round(median(count, na.rm = TRUE), 2),
          Min = min(count, na.rm = TRUE),
          Max = max(count, na.rm = TRUE),
          SD = round(sd(count, na.rm = TRUE), 2),
          N = n(),
          .groups = "drop"
        ) %>%
        mutate(agegroup = factor(agegroup, levels = ordered_agegroup)) %>%
        arrange(agegroup)
      
      data_table_df <- filtered_data() %>%
        select(-agegroupcd) %>%
        rename(
          `Age Group`      = agegroup,
          `Year`           = year,
          `Incidence`      = count,
          `Population`     = population,
          `Crude Rate`     = crudert,
          `95% CI Lower`   = crudertlow95,
          `95% CI Upper`   = crudertupp95,
          `Standard Error` = crudertse
        )
      
      params <- list(
        selected_years = input$years,
        selected_agegroups = input$age_groups,
        log_scale = input$log_scale,
        hist_view = input$histogram_view,
        box_summary_table = box_summary_table,
        filtered_data = filtered_data(),
        selected_states = input$selected_states_box,
        gg_line_age_plot_path = plot_and_path()$path,
        gg_line_year_plot_path = plot_and_path_year()$path,
        hist_overlay_plot_path = hist_overlay_plot_and_path()$path,
        hist_faceted_plot_path = hist_faceted_plot_and_path()$path,
        box_plot_path = box_plot_and_path()$path,
        heatmap_and_path = heatmap_and_path()$path,
        summary_stats_df = summary_stats_df(),
        summary_stats_data = summary_stats_data(),
        state_plot_path = plot_file,
        state_data = box_states_data,
        shapiro_results = shapiro_df,
        data_table_df = data_table_df,
        UI_summary_stats = summary_stats_df()
      )
    
      rmarkdown::render(
        input = tempReport,
        params = params,
        output_format = "pdf_document", 
        output_file = file,
        envir = new.env(parent = globalenv())
      )
    }
  )
}

# Launch the app
shinyApp(ui = ui, server = server)
