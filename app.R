library(shiny)
library(DT)
library(purrr)
library(tidyverse)
library(bslib)
library(patchwork)
library(later)
here::here()

#  --dark-moss-green: #606c38ff;
#  --pakistan-green: #283618ff;
#  --cornsilk: #fefae0ff;
#  --earth-yellow: #dda15eff;
#  --tigers-eye: #bc6c25ff;

# Source external functions
if (file.exists("functions.R")) {
  source("functions.R")
} else if (file.exists("fish_optimization_functions.R")) {
  source("fish_optimization_functions.R")
} else {
  stop("Cannot find functions.R or fish_optimization_functions.R file. Please check file location.")
}

# Import data
df <- rio::import(here::here("data/fish_simple.csv"))
# Don't remove pos_number for the Critterpedia view
df_full <- df  # Keep full data with pos_number
df <- df %>% 
  select(-c(pos_number))

# Define UI
ui <- fluidPage(
  theme = bs_theme(version = 4),
  
  # Custom CSS styling with the color scheme and tooltip styling
  tags$head(
    tags$style(HTML("
      /* Color scheme variables */
      :root {
        --dark-moss-green: #606c38;
        --pakistan-green: #283618;
        --cornsilk: #fefae0;
        --earth-yellow: #dda15e;
        --tigers-eye: #bc6c25;
      }
      
      /* Body and main background */
      body {
        background-color: var(--cornsilk);
        color: var(--pakistan-green);
        font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
      }
      
      /* Title styling */
      .container-fluid h1 {
        color: var(--pakistan-green);
        text-align: center;
        margin-bottom: 30px;
        font-weight: bold;
        text-shadow: 1px 1px 2px rgba(0,0,0,0.1);
      }
      
      /* Tab styling */
      .nav-tabs > li > a {
        background-color: var(--earth-yellow);
        color: var(--pakistan-green);
        border: 1px solid var(--tigers-eye);
        font-weight: bold;
      }
      
      .nav-tabs > li.active > a,
      .nav-tabs > li.active > a:hover,
      .nav-tabs > li.active > a:focus {
        background-color: var(--dark-moss-green);
        color: var(--cornsilk);
        border: 1px solid var(--tigers-eye);
      }
      
      .nav-tabs > li > a:hover {
        background-color: var(--tigers-eye);
        color: var(--cornsilk);
      }
      
      /* Tab content styling */
      .tab-content {
        background-color: white;
        border: 2px solid var(--dark-moss-green);
        border-top: none;
        padding: 20px;
        border-radius: 0 0 8px 8px;
      }
      
      /* Well panel styling */
      .well {
        background-color: white;
        border: 2px solid var(--dark-moss-green);
        border-radius: 8px;
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
      }
      
      /* Button styling */
      .btn-default {
        background-color: var(--earth-yellow);
        border-color: var(--tigers-eye);
        color: var(--pakistan-green);
        font-weight: bold;
      }
      
      .btn-default:hover {
        background-color: var(--tigers-eye);
        border-color: var(--pakistan-green);
        color: var(--cornsilk);
      }
      
      .btn-primary {
        background-color: var(--dark-moss-green);
        border-color: var(--pakistan-green);
        color: var(--cornsilk);
        font-weight: bold;
      }
      
      .btn-primary:hover {
        background-color: var(--pakistan-green);
        border-color: var(--tigers-eye);
        color: var(--cornsilk);
      }
      
      /* Checkbox styling */
      .checkbox label {
        color: var(--pakistan-green);
        font-weight: 500;
      }
      
      /* Input styling */
      .form-control {
        border: 2px solid var(--earth-yellow);
        border-radius: 4px;
      }
      
      .form-control:focus {
        border-color: var(--dark-moss-green);
        box-shadow: 0 0 0 0.2rem rgba(96, 108, 56, 0.25);
      }
      
      /* Headers */
      h4, h5 {
        color: var(--pakistan-green);
        font-weight: bold;
        margin-bottom: 15px;
      }
      
      /* DataTable styling */
      .dataTables_wrapper {
        color: var(--pakistan-green);
      }
      
      .dataTables_wrapper .dataTables_length,
      .dataTables_wrapper .dataTables_filter,
      .dataTables_wrapper .dataTables_info,
      .dataTables_wrapper .dataTables_paginate {
        color: var(--pakistan-green);
      }
      
      /* Conditional panel placeholder */
      .placeholder-content {
        background-color: var(--earth-yellow);
        border: 2px dashed var(--tigers-eye);
        border-radius: 8px;
        color: var(--pakistan-green);
      }
      
      /* Slider styling */
      .irs-bar {
        background: var(--dark-moss-green);
      }
      
      .irs-bar-edge {
        background: var(--dark-moss-green);
      }
      
      .irs-single {
        background: var(--tigers-eye);
      }
      
      .irs-handle {
        border-color: var(--tigers-eye);
        background: var(--earth-yellow);
      }
      
      /* Tooltip styling */
      .tooltip {
        pointer-events: none;
      }
      .tooltip > .tooltip-inner {
        pointer-events: none;
        background-color: var(--dark-moss-green);
        color: var(--cornsilk);
        border: 2px solid var(--tigers-eye);
        padding: 15px;
        font-size: 14px;
        text-align: left;
        margin-left: 0;
        max-width: 400px;
        border-radius: 8px;
      }
      .tooltip > .arrow::before {
        border-right-color: var(--dark-moss-green);
      }
      
      /* Info icon styling */
      .info-icon {
        color: var(--tigers-eye);
        margin-left: 5px;
        cursor: help;
      }
      
      .info-icon:hover {
        color: var(--dark-moss-green);
      }
      
            /* Loading modal styling */
      .loading-modal .modal-content {
        background-color: var(--cornsilk);
        border: 3px solid var(--dark-moss-green);
        border-radius: 15px;
        text-align: center;
      }
      
      .loading-modal .modal-header {
        background-color: var(--dark-moss-green);
        color: var(--cornsilk);
        border-bottom: 2px solid var(--tigers-eye);
        border-radius: 12px 12px 0 0;
      }
      
      .loading-modal .modal-body {
        padding: 40px;
        color: var(--pakistan-green);
      }
      
      .loading-modal .close {
        display: none; /* Hide the close button */
      }
      
      /* Spinner animation */
      .spinner {
        border: 4px solid var(--earth-yellow);
        border-top: 4px solid var(--dark-moss-green);
        border-radius: 50%;
        width: 50px;
        height: 50px;
        animation: spin 1s linear infinite;
        margin: 20px auto;
      }
      
      @keyframes spin {
        0% { transform: rotate(0deg); }
        100% { transform: rotate(360deg); }
      }
      
      .loading-text {
        font-size: 18px;
        font-weight: bold;
        color: var(--pakistan-green);
        margin-top: 15px;
      }
      
      
      
    ")),
    tags$script(HTML("
      $(function () {
        $('[data-toggle=tooltip]').tooltip()
      })
    "))
  ),
  
  titlePanel("Optimizing Fish Catching in Animal Crossing Using Monte Carlo Simulation"),
  
  fluidRow(
    # Left sidebar: fish selection
    column(3,
           div(class = "fish-selection-box",
               h4("What're we catching?"),
               
               # Single tabsetPanel with all fish selection options
               tabsetPanel(
                 id = "fish_tabs",
                 
                 # River Fish Tab
                 tabPanel("River Fish",
                          br(),
                          fluidRow(
                            column(6, actionButton("select_all_river", "Select All", 
                                                   class = "btn-default btn-sm")),
                            column(6, actionButton("deselect_all_river", "Deselect All", 
                                                   class = "btn-default btn-sm"))
                          ),
                          br(),
                          checkboxGroupInput("river_fish", 
                                             NULL,
                                             choices = setNames(df$name[df$location_collapsed == "River"], 
                                                                df_full$full_name[match(df$name[df$location_collapsed == "River"], df_full$name)]),
                                             selected = df$name[df$location_collapsed == "River"])
                 ),
                 
                 # Ocean Fish Tab
                 tabPanel("Ocean Fish",
                          br(),
                          fluidRow(
                            column(6, actionButton("select_all_ocean", "Select All", 
                                                   class = "btn-default btn-sm")),
                            column(6, actionButton("deselect_all_ocean", "Deselect All", 
                                                   class = "btn-default btn-sm"))
                          ),
                          br(),
                          checkboxGroupInput("ocean_fish", 
                                             NULL,
                                             choices = setNames(df$name[df$location_collapsed == "Ocean"], 
                                                                df_full$full_name[match(df$name[df$location_collapsed == "Ocean"], df_full$name)]),
                                             selected = df$name[df$location_collapsed == "Ocean"])
                 ),
                 
                 # Pond Fish Tab
                 tabPanel("Pond Fish",
                          br(),
                          fluidRow(
                            column(6, actionButton("select_all_pond", "Select All", 
                                                   class = "btn-default btn-sm")),
                            column(6, actionButton("deselect_all_pond", "Deselect All", 
                                                   class = "btn-default btn-sm"))
                          ),
                          br(),
                          checkboxGroupInput("pond_fish", 
                                             NULL,
                                             choices = setNames(df$name[df$location_collapsed == "Pond"], 
                                                                df_full$full_name[match(df$name[df$location_collapsed == "Pond"], df_full$name)]),
                                             selected = df$name[df$location_collapsed == "Pond"])
                 ),
                 
                 # Critterpedia Tab (moved here as 4th tab)
                 tabPanel("Critterpedia",
                          br(),
                          fluidRow(
                            column(6, actionButton("select_all_critterpedia", "Select All", 
                                                   class = "btn-default btn-sm")),
                            column(6, actionButton("deselect_all_critterpedia", "Deselect All", 
                                                   class = "btn-default btn-sm"))
                          ),
                          br(),
                          checkboxGroupInput("critterpedia_fish", 
                                             NULL,
                                             choices = setNames(df_full$name[order(df_full$pos_number)], 
                                                                df_full$full_name[order(df_full$pos_number)]),
                                             selected = df_full$name[order(df_full$pos_number)])
                 )
               )
           )
    ),
    
    
    # Right column
    column(6,
           # Top-level tabs for results and visualizations
           tabsetPanel(
             id = "main_results_tabs",
             
             tabPanel("Optimal Fishing Dates",
                      br(),
                      
                      # Placeholder
                      conditionalPanel(
                        condition = "output.analysis_complete == false || output.analysis_complete == null",
                        div(
                          class = "placeholder-content",
                          style = "text-align: center; padding: 50px;",
                          h5("Run optimization to see results"),
                          p("Select fish from the left panel and configure optimization parameters below, then click 'Optimize Fish Catching' to generate schedules.")
                        )
                      ),
                      
                      # Results tables - SIMPLIFIED STRUCTURE
                      conditionalPanel(
                        condition = "output.analysis_complete == true",
                        
                        # Single tabsetPanel with all schedule types
                        tabsetPanel(
                          id = "results_tabs",
                          
                          tabPanel("River Schedule",
                                   br(),
                                   DT::dataTableOutput("river_table")
                          ),
                          
                          tabPanel("Ocean Schedule",
                                   br(),
                                   DT::dataTableOutput("ocean_table")
                          ),
                          
                          tabPanel("Pond Schedule",
                                   br(),
                                   DT::dataTableOutput("pond_table")
                          ),
                          
                          tabPanel("Critterpedia Schedule",
                                   br(),
                                   DT::dataTableOutput("critterpedia_table")
                          )
                        )
                      )
             ),
             
             # Tab 2: Understanding the Monte Carlo Process (moved here)
             tabPanel("Monte Carlo Insights",
                      br(),
                      
                      # Only show if analysis is complete
                      conditionalPanel(
                        condition = "output.analysis_complete == false || output.analysis_complete == null",
                        div(
                          class = "placeholder-content",
                          style = "text-align: center; padding: 50px;",
                          h5("Run optimization to see simulation insights"),
                          p("These visualizations will show what happens 'under the hood' during the Monte Carlo simulation process.")
                        )
                      ),
                      
                      conditionalPanel(
                        condition = "output.analysis_complete == true",
                        fluidRow(
                          column(12,
                                 h5("Understanding the Monte Carlo Process"),
                                 p("These visualizations show what happened 'under the hood' during simulation:")
                          )
                        ),
                        
                        fluidRow(
                          column(6,
                                 h6("Total Fishing Attempts Distribution"),
                                 plotOutput("draws_plot", height = "300px"),
                                 p(style = "font-size: 12px; color: #606c38;", 
                                   "Shows variability in efficiency across successful simulations")
                          ),
                          column(6,
                                 h6("Time Periods Used Distribution"), 
                                 plotOutput("columns_plot", height = "300px"),
                                 p(style = "font-size: 12px; color: #606c38;",
                                   "Shows how many different time slots were needed")
                          )
                        ),
                        
                        fluidRow(
                          column(12,
                                 h6("Exploration vs Exploitation Analysis"),
                                 plotOutput("exploration_plot", height = "350px"),
                                 p(style = "font-size: 12px; color: #606c38;",
                                   "Shows relationship between exploration choices and efficiency")
                          )
                        )
                      )
             ),
             # Analysis Results tab
             tabPanel("Analysis Results",
                      br(),
                      verbatimTextOutput("analysis_output")
             )
           )
    ),
    column(3,
           wellPanel(
                      tabsetPanel(
                        id = "optimization_tabs",
                        
                        # Parameters tab
                        tabPanel("Parameters",
                                 br(),
                                 div(
                                   numericInput("N", 
                                                span("Simulations",
                                                     span(`data-toggle` = "tooltip", `data-placement` = "right",
                                                          title = "How many simulations should we run? More simulations (especially with lots of fish) will take a long time.",
                                                          icon("info-circle", class = "info-icon"))),
                                                value = 5000, 
                                                min = 0, 
                                                max = 100000, 
                                                step = 1)
                                 ),
                                 
                                 div(
                                   sliderInput("exploration_weight", 
                                               span("Exploration",
                                                    span(`data-toggle` = "tooltip", `data-placement` = "right",
                                                         title = "High numbers encourage exploration (finding optimal solutions) but take more time. Low numbers encourage exploitation (sticking with tried-and-true options)",
                                                         icon("info-circle", class = "info-icon"))),
                                               min = 0, 
                                               max = 1.0, 
                                               value = 0.3, 
                                               step = 0.01)
                                 ),
                                 
                                 div(
                                   sliderInput("column_minimization_weight", 
                                               span("Minimize time traveling:",
                                                    span(`data-toggle` = "tooltip", `data-placement` = "right",
                                                         title = "Minimize number of encounters but with more time-traveling closer to (0.0) or minimize time-traveling but need more encounters each day closer to (1.0)",
                                                         icon("info-circle", class = "info-icon"))),
                                               min = 0, 
                                               max = 1.0, 
                                               value = 0.5, 
                                               step = 0.01)
                                 ),
                                 
                                 div(
                                   numericInput("emergency_brake", 
                                                span("Emergency Brake",
                                                     span(`data-toggle` = "tooltip", `data-placement` = "right",
                                                          title = "Maximum attempts per simulation before giving up. Higher values ensure all simulations succeed but may be very slow for difficult problems.",
                                                          icon("info-circle", class = "info-icon"))),
                                                value = 10000, 
                                                min = 1000, 
                                                max = 100000, 
                                                step = 1000)
                                 ),
                                 
                                 br(),
                                 
                                 actionButton("analyze_btn", 
                                              "Optimize Fish Catching", 
                                              class = "btn-primary btn-block")
                        )
                      )
                    )
             )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Track whether analysis has been completed
  analysis_complete <- reactiveVal(FALSE)
  
  # Output for conditional panel
  output$analysis_complete <- reactive({
    analysis_complete()
  })
  outputOptions(output, "analysis_complete", suspendWhenHidden = FALSE)
  
  # Flag to prevent infinite loops during programmatic updates
  programmatic_update <- reactiveVal(FALSE)
  
  # Helper function to safely update checkboxes
  safe_update <- function(input_id, selected_values) {
    programmatic_update(TRUE)
    updateCheckboxGroupInput(session, input_id, selected = selected_values)
    # Use invalidateLater to reset the flag after a short delay
    invalidateLater(100, session)
    later::later(function() {
      programmatic_update(FALSE)
    }, delay = 0.1)
  }
  
  # CRITTERPEDIA SELECT/DESELECT BUTTONS
  observeEvent(input$select_all_critterpedia, {
    all_fish <- df_full$name[order(df_full$pos_number)]
    safe_update("critterpedia_fish", all_fish)
    
    # Also update location tabs
    safe_update("river_fish", df$name[df$location_collapsed == "River"])
    safe_update("ocean_fish", df$name[df$location_collapsed == "Ocean"])
    safe_update("pond_fish", df$name[df$location_collapsed == "Pond"])
  })
  
  observeEvent(input$deselect_all_critterpedia, {
    safe_update("critterpedia_fish", character(0))
    safe_update("river_fish", character(0))
    safe_update("ocean_fish", character(0))
    safe_update("pond_fish", character(0))
  })
  
  # LOCATION SELECT/DESELECT BUTTONS  
  observeEvent(input$select_all_river, {
    river_fish <- df$name[df$location_collapsed == "River"]
    safe_update("river_fish", river_fish)
    
    # Update critterpedia to include these fish
    current_critterpedia <- isolate(input$critterpedia_fish)
    new_critterpedia <- unique(c(current_critterpedia, river_fish))
    safe_update("critterpedia_fish", new_critterpedia)
  })
  
  observeEvent(input$deselect_all_river, {
    river_fish <- df$name[df$location_collapsed == "River"]
    safe_update("river_fish", character(0))
    
    # Remove river fish from critterpedia
    current_critterpedia <- isolate(input$critterpedia_fish)
    new_critterpedia <- setdiff(current_critterpedia, river_fish)
    safe_update("critterpedia_fish", new_critterpedia)
  })
  
  observeEvent(input$select_all_ocean, {
    ocean_fish <- df$name[df$location_collapsed == "Ocean"]
    safe_update("ocean_fish", ocean_fish)
    
    current_critterpedia <- isolate(input$critterpedia_fish)
    new_critterpedia <- unique(c(current_critterpedia, ocean_fish))
    safe_update("critterpedia_fish", new_critterpedia)
  })
  
  observeEvent(input$deselect_all_ocean, {
    ocean_fish <- df$name[df$location_collapsed == "Ocean"]
    safe_update("ocean_fish", character(0))
    
    current_critterpedia <- isolate(input$critterpedia_fish)
    new_critterpedia <- setdiff(current_critterpedia, ocean_fish)
    safe_update("critterpedia_fish", new_critterpedia)
  })
  
  observeEvent(input$select_all_pond, {
    pond_fish <- df$name[df$location_collapsed == "Pond"]
    safe_update("pond_fish", pond_fish)
    
    current_critterpedia <- isolate(input$critterpedia_fish)
    new_critterpedia <- unique(c(current_critterpedia, pond_fish))
    safe_update("critterpedia_fish", new_critterpedia)
  })
  
  observeEvent(input$deselect_all_pond, {
    pond_fish <- df$name[df$location_collapsed == "Pond"]
    safe_update("pond_fish", character(0))
    
    current_critterpedia <- isolate(input$critterpedia_fish)
    new_critterpedia <- setdiff(current_critterpedia, pond_fish)
    safe_update("critterpedia_fish", new_critterpedia)
  })
  
  # BIDIRECTIONAL SYNCHRONIZATION (with proper debouncing)
  
  # Sync: Location tabs → Critterpedia
  # Use debounce to prevent rapid-fire updates
  location_changes <- reactive({
    c(input$river_fish, input$ocean_fish, input$pond_fish)
  })
  
  location_changes_debounced <- debounce(location_changes, 200)  # 200ms delay
  
  observeEvent(location_changes_debounced(), {
    if (!programmatic_update()) {
      new_selection <- location_changes_debounced()
      current_critterpedia <- isolate(input$critterpedia_fish)
      
      if (!setequal(new_selection, current_critterpedia)) {
        safe_update("critterpedia_fish", new_selection)
      }
    }
  }, ignoreInit = TRUE)
  
  # Sync: Critterpedia → Location tabs
  critterpedia_changes_debounced <- debounce(reactive(input$critterpedia_fish), 200)
  
  observeEvent(critterpedia_changes_debounced(), {
    if (!programmatic_update()) {
      selected_fish <- critterpedia_changes_debounced()
      
      # Calculate what should be selected in each location
      river_should_be <- intersect(selected_fish, df$name[df$location_collapsed == "River"])
      ocean_should_be <- intersect(selected_fish, df$name[df$location_collapsed == "Ocean"])
      pond_should_be <- intersect(selected_fish, df$name[df$location_collapsed == "Pond"])
      
      # Update only if different
      if (!setequal(river_should_be, isolate(input$river_fish))) {
        safe_update("river_fish", river_should_be)
      }
      if (!setequal(ocean_should_be, isolate(input$ocean_fish))) {
        safe_update("ocean_fish", ocean_should_be)
      }
      if (!setequal(pond_should_be, isolate(input$pond_fish))) {
        safe_update("pond_fish", pond_should_be)
      }
    }
  }, ignoreInit = TRUE)
  
  
  # Reactive expression for filtered data
  filtered_data <- reactive({
    # Combine all selected fish from all tabs
    selected_fish <- c(input$river_fish, input$ocean_fish, 
                       input$pond_fish, input$critterpedia_fish)
    # Remove duplicates
    selected_fish <- unique(selected_fish)
    
    if (length(selected_fish) == 0) {
      return(data.frame()) # Return empty dataframe if no fish selected
    }
    
    # Use df to ensure we have the right columns
    filtered_df <- df[df$name %in% selected_fish, ]
    return(filtered_df)
  })
  
  
  # Reactive values to store schedule data
  river_schedule <- reactiveVal(data.frame())
  ocean_schedule <- reactiveVal(data.frame())
  pond_schedule <- reactiveVal(data.frame())
  critterpedia_schedule <- reactiveVal(data.frame())
  
  # Handle button click for fish catching optimization

  observeEvent(input$analyze_btn, {
    current_data <- filtered_data()
    
    # Check if data is available
    if (nrow(current_data) == 0) {
      output$analysis_output <- renderText({
        "No fish selected! Please select at least one fish from the left panel."
      })
      analysis_complete(FALSE)
      return()
    }
    
    # Show loading modal
    showModal(modalDialog(
      div(class = "loading-modal",
          div(class = "spinner"),
          div(class = "loading-text", "Gone fishin'..."),
          div(style = "margin-top: 10px; color: var(--tigers-eye); font-size: 14px;", 
              paste("Running", input$N, "simulations"))),
      title = div(style = "text-align: center; font-weight: bold;", "🎣 Optimizing Fish Catching 🎣"),
      footer = NULL,
      easyClose = FALSE,
      fade = TRUE,
      size = "s"
    ))
    
    # Show processing message
    output$analysis_output <- renderText({
      "Gone fishin' (simulating...)"
    })
    
    # Run optimization (wrapped in tryCatch for error handling)
    tryCatch({
      result <- optimize_fish_catching_with_viz_data(
        current_data,
        N = input$N,
        exploration_weight = input$exploration_weight,
        column_minimization_weight = input$column_minimization_weight,
        emergency_brake = input$emergency_brake  
      )
      
      # Create visualizations
      viz_plots <- create_monte_carlo_visualizations(result)
      
      # Store plots for rendering
      output$draws_plot <- renderPlot({ viz_plots$draws_histogram })
      output$exploration_plot <- renderPlot({ viz_plots$exploration_effect })
      output$columns_plot <- renderPlot({ viz_plots$columns_histogram })
      
      optimal_times <- result$best_solution$columns_used
      schedule <- create_optimal_fishing_schedule(
        current_data, 
        optimal_times, 
        wide_format = TRUE
      )
      
      # Split schedule by location and remove location_collapsed column
      river_data <- schedule[schedule$name %in% df$name[df$location == "River"], ]
      ocean_data <- schedule[schedule$name %in% df$name[df$location == "Ocean"], ]
      pond_data <- schedule[schedule$name %in% df$name[df$location == "Pond"], ]
      
      # Remove location_collapsed column from all tables
      if("location_collapsed" %in% names(river_data)) river_data <- river_data[, !names(river_data) %in% "location_collapsed"]
      if("location_collapsed" %in% names(ocean_data)) ocean_data <- ocean_data[, !names(ocean_data) %in% "location_collapsed"]
      if("location_collapsed" %in% names(pond_data)) pond_data <- pond_data[, !names(pond_data) %in% "location_collapsed"]
      
      # Replace name column with full_name for display
      if("name" %in% names(river_data) && "full_name" %in% names(river_data)) {
        river_data$name <- river_data$full_name
        river_data <- river_data[, !names(river_data) %in% "full_name"]
      }
      if("name" %in% names(ocean_data) && "full_name" %in% names(ocean_data)) {
        ocean_data$name <- ocean_data$full_name
        ocean_data <- ocean_data[, !names(ocean_data) %in% "full_name"]
      }
      if("name" %in% names(pond_data) && "full_name" %in% names(pond_data)) {
        pond_data$name <- pond_data$full_name
        pond_data <- pond_data[, !names(pond_data) %in% "full_name"]
      }
      
      # Create critterpedia order table (sorted by pos_number)
      critterpedia_data <- schedule
      if("location_collapsed" %in% names(critterpedia_data)) {
        critterpedia_data <- critterpedia_data[, !names(critterpedia_data) %in% "location_collapsed"]
      }
      # Replace name column with full_name for display
      if("name" %in% names(critterpedia_data) && "full_name" %in% names(critterpedia_data)) {
        critterpedia_data$name <- critterpedia_data$full_name
        critterpedia_data <- critterpedia_data[, !names(critterpedia_data) %in% "full_name"]
      }
      
      # Create a mapping from fish names to pos_numbers
      name_to_pos <- setNames(df_full$pos_number, df_full$name)
      
      # Get pos_numbers for fish in our schedule (using original names before display transformation)
      original_names <- schedule$name
      pos_numbers <- name_to_pos[original_names]
      
      # Sort critterpedia_data by pos_number
      critterpedia_data <- critterpedia_data[order(pos_numbers, na.last = TRUE), ]
      
      # Update the schedule data
      river_schedule(river_data)
      ocean_schedule(ocean_data)
      pond_schedule(pond_data)
      critterpedia_schedule(critterpedia_data)
      
      # Mark analysis as complete
      analysis_complete(TRUE)
      
      # Update analysis output with results summary
      output$analysis_output <- renderText({
        paste(
          "=== OPTIMIZATION COMPLETE ===",
          paste("Parameters used:"),
          paste("- Total draws:", input$N),
          paste("- Exploration Weight:", input$exploration_weight),
          paste("- Minimization Weight:", input$column_minimization_weight),
          paste("- Emergency Brake:", input$emergency_brake),  # ADD THIS LINE
          paste(""),
          paste("Results:"),
          paste("- Total fish caught:", nrow(current_data)),
          paste("- River fish:", nrow(river_data)),
          paste("- Ocean fish:", nrow(ocean_data)), 
          paste("- Pond fish:", nrow(pond_data)),
          paste("- Optimal times selected:", length(optimal_times)),
          paste("- Success rate:", round(result$summary_stats$success_rate * 100, 1), "%"),  # ADD THIS LINE
          sep = "\n"
        )
      })
      
      # Remove loading modal on success
      removeModal()
      
    }, error = function(e) {
      output$analysis_output <- renderText({
        paste("Error in optimization:", e$message)
      })
      analysis_complete(FALSE)
      
      # Remove loading modal on error
      removeModal()
    })
  })
  
  
  # Location-specific tables
  output$river_table <- DT::renderDataTable({
    if (nrow(river_schedule()) > 0) {
      DT::datatable(river_schedule(), 
                    options = list(pageLength = 80, scrollX = TRUE),
                    rownames = FALSE)
    }
  })
  
  output$ocean_table <- DT::renderDataTable({
    if (nrow(ocean_schedule()) > 0) {
      DT::datatable(ocean_schedule(), 
                    options = list(pageLength = 80, scrollX = TRUE),
                    rownames = FALSE)
    }
  })
  
  output$pond_table <- DT::renderDataTable({
    if (nrow(pond_schedule()) > 0) {
      DT::datatable(pond_schedule(), 
                    options = list(pageLength = 80, scrollX = TRUE),
                    rownames = FALSE)
    }
  })
  
  # Render the critterpedia table with 80 entries per page
  output$critterpedia_table <- DT::renderDataTable({
    if (nrow(critterpedia_schedule()) > 0) {
      DT::datatable(critterpedia_schedule(), 
                    options = list(pageLength = 80, scrollX = TRUE),
                    rownames = FALSE)
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)