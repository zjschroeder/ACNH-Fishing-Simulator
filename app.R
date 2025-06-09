library(shiny)
library(DT)
library(purrr)
library(tidyverse)
here::here()

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
df <- df %>% 
  select(-pos_number)

# Define UI
ui <- fluidPage(
  titlePanel("Optimizing Fish Catching in Animal Crossing Using Monte Carlo Simulation"),
  
  fluidRow(
    # Left sidebar: fish selection
    column(3,
           wellPanel(
             h4("What're we fishin' for?"),
             
             # River  
             h5("River Fish"),
             fluidRow(
               column(6, actionButton("select_all_river", "Select All", size = "sm")),
               column(6, actionButton("deselect_all_river", "Deselect All", size = "sm"))
             ),
             br(),
             checkboxGroupInput("river_fish", 
                                NULL,
                                choices = df$name[df$location == "river"],
                                selected = df$name[df$location == "river"]),
             
             hr(),
             
             # Ocean fish 
             h5("Ocean Fish"),
             fluidRow(
               column(6, actionButton("select_all_ocean", "Select All", size = "sm")),
               column(6, actionButton("deselect_all_ocean", "Deselect All", size = "sm"))
             ),
             br(),
             checkboxGroupInput("ocean_fish", 
                                NULL,
                                choices = df$name[df$location == "ocean"],
                                selected = df$name[df$location == "ocean"]),
             
             hr(),
             
             # Pond fish 
             h5("Pond Fish"),
             fluidRow(
               column(6, actionButton("select_all_pond", "Select All", size = "sm")),
               column(6, actionButton("deselect_all_pond", "Deselect All", size = "sm"))
             ),
             br(),
             checkboxGroupInput("pond_fish", 
                                NULL,
                                choices = df$name[df$location == "pond"],
                                selected = df$name[df$location == "pond"])
           )
    ),
    
    # Main
    column(6,
           h4("Optimal Fishing Dates"),
           
           # Placeholder
           conditionalPanel(
             condition = "output.analysis_complete == false || output.analysis_complete == null",
             div(
               style = "text-align: center; padding: 50px; color: #666;",
               h5("Run optimization to see results"),
               p("Select fish from the left panel and configure optimization parameters on the right, then click 'Optimize Fish Catching' to generate schedules.")
             )
           ),
           
           # Results tables 
           conditionalPanel(
             condition = "output.analysis_complete == true",
             
             h5("River Schedule"),
             DT::dataTableOutput("river_table"),
             
             br(),
             
             h5("Ocean Schedule"),
             DT::dataTableOutput("ocean_table"),
             
             br(),
             
             h5("Pond Schedule"),
             DT::dataTableOutput("pond_table")
           )
    ),
    
    # Right sidebar: optimization
    column(3,
           wellPanel(
             h4("Optimization Parameters"),
             
             numericInput("N", 
                          "How many times should we simulate?", 
                          value = 100, 
                          min = 0, 
                          max = 1000, 
                          step = 1),
             
             sliderInput("exploration_weight", 
                         "Should the algorithm prioritize the full year (higher) or\nstay in dates and times closer together (lower)?", 
                         min = 0, 
                         max = 1.0, 
                         value = 0.3, 
                         step = 0.01),
             
             sliderInput("column_minimization_weight", 
                         "Would you rather have fewer casts (lower number) or\nless time travelling (higher number)", 
                         min = 0, 
                         max = 1.0, 
                         value = 0.5, 
                         step = 0.01),
             
             br(),
             
             actionButton("analyze_btn", 
                          "Optimize Fish Catching", 
                          class = "btn-primary btn-block"),
             
             br(),
             
             h5("Analysis Results"),
             verbatimTextOutput("analysis_output")
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
  
  # Select/Deselect all buttons for River
  observeEvent(input$select_all_river, {
    updateCheckboxGroupInput(session, "river_fish", 
                             selected = df$name[df$location == "river"])
  })
  
  observeEvent(input$deselect_all_river, {
    updateCheckboxGroupInput(session, "river_fish", selected = character(0))
  })
  
  # Select/Deselect all buttons for Ocean
  observeEvent(input$select_all_ocean, {
    updateCheckboxGroupInput(session, "ocean_fish", 
                             selected = df$name[df$location == "ocean"])
  })
  
  observeEvent(input$deselect_all_ocean, {
    updateCheckboxGroupInput(session, "ocean_fish", selected = character(0))
  })
  
  # Select/Deselect all buttons for Pond
  observeEvent(input$select_all_pond, {
    updateCheckboxGroupInput(session, "pond_fish", 
                             selected = df$name[df$location == "pond"])
  })
  
  observeEvent(input$deselect_all_pond, {
    updateCheckboxGroupInput(session, "pond_fish", selected = character(0))
  })
  
  # Reactive expression for filtered data
  filtered_data <- reactive({
    selected_fish <- c(input$river_fish, input$ocean_fish, input$pond_fish)
    
    if (length(selected_fish) == 0) {
      return(data.frame()) # Return empty dataframe if no fish selected
    }
    
    filtered_df <- df[df$name %in% selected_fish, ]
    return(filtered_df)
  })
  
  # Reactive values to store schedule data
  river_schedule <- reactiveVal(data.frame())
  ocean_schedule <- reactiveVal(data.frame())
  pond_schedule <- reactiveVal(data.frame())
  
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
    
    # Show processing message
    output$analysis_output <- renderText({
      "Gone fishin' (simulating...)"
    })
    
    # Run optimization (wrapped in tryCatch for error handling)
    tryCatch({
      result <- optimize_fish_catching(
        current_data,
        N = input$N,
        exploration_weight = input$exploration_weight,
        column_minimization_weight = input$column_minimization_weight
      )
      
      optimal_times <- result$best_solution$columns_used
      schedule <- create_optimal_fishing_schedule(
        current_data, 
        optimal_times, 
        wide_format = TRUE
      )
      
      # Split schedule by location
      river_data <- schedule[schedule$name %in% df$name[df$location == "river"], ]
      ocean_data <- schedule[schedule$name %in% df$name[df$location == "ocean"], ]
      pond_data <- schedule[schedule$name %in% df$name[df$location == "pond"], ]
      
      # Update the schedule data
      river_schedule(river_data)
      ocean_schedule(ocean_data)
      pond_schedule(pond_data)
      
      # Mark analysis as complete
      analysis_complete(TRUE)
      
      # Update analysis output with results summary
      output$analysis_output <- renderText({
        paste(
          "=== OPTIMIZATION COMPLETE ===",
          paste("Parameters used:"),
          paste("- Total draws:", input$N),
          paste("- Exploration Weight:", input$exploration_weight),
          paste("-  Minimization Weight:", input$column_minimization_weight),
          paste(""),
          paste("Results:"),
          paste("- Total fish caught:", nrow(current_data)),
          paste("- River fish:", nrow(river_data)),
          paste("- Ocean fish:", nrow(ocean_data)), 
          paste("- Pond fish:", nrow(pond_data)),
          paste("- Optimal times selected:", length(optimal_times)),
          sep = "\n"
        )
      })
      
    }, error = function(e) {
      output$analysis_output <- renderText({
        paste("Error in optimization:", e$message)
      })
      analysis_complete(FALSE)
    })
  })
  
  # Render the location-specific tables
  output$river_table <- DT::renderDataTable({
    if (nrow(river_schedule()) > 0) {
      DT::datatable(river_schedule(), 
                    options = list(pageLength = 5, scrollX = TRUE),
                    rownames = FALSE)
    }
  })
  
  output$ocean_table <- DT::renderDataTable({
    if (nrow(ocean_schedule()) > 0) {
      DT::datatable(ocean_schedule(), 
                    options = list(pageLength = 5, scrollX = TRUE),
                    rownames = FALSE)
    }
  })
  
  output$pond_table <- DT::renderDataTable({
    if (nrow(pond_schedule()) > 0) {
      DT::datatable(pond_schedule(), 
                    options = list(pageLength = 5, scrollX = TRUE),
                    rownames = FALSE)
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)