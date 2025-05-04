library(shiny)
library(shinydashboard)
library(DBI)
library(RSQLite)
library(DT)
library(shinyWidgets)
library(bslib)

# Connect to SQLite DB
db_file <- "coffee_data.db"
con <- dbConnect(SQLite(), db_file)

# Define a custom theme
my_theme <- bs_theme(
  version = 4,                
  primary = "#007bff",         
  secondary = "#6c757d",       
  success = "#28a745",        
  info = "#17a2b8",            
  warning = "#ffc107",        
  danger = "#dc3545",         
  light = "#f8f9fa",         
  dark = "#343a40",           
  base_font = font_google("Roboto"),  
  heading_font = font_google("Merriweather")
)

# Define UI title
header <- dashboardHeader(title = "Coffee Rating Dashboard")

# Define sidebar
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("About", tabName = "about", icon = icon("info-circle")),
    menuItem("Cupping Results", tabName = "results", icon = icon("coffee")),
    menuItem("Enter New Cupping", tabName = "new_entry", icon = icon("plus-circle"))
  ),
  
  # Create filters
  hr(),
  selectInput("farm_filter", "Farm", choices = NULL),
  selectInput("bean_filter", "Coffee Bean Origin", choices = NULL),
  selectInput("roaster_filter", "Roaster", choices = NULL),
  selectInput("species_filter", "Species", choices = NULL),
  selectInput("method_filter", "Processing Method", choices = NULL),
  
  # Create a Slider for Cupping Total Score
  sliderInput("score_range", 
              "Total Cupping Score Range", 
              min = 0, max = 100, value = c(0, 100), step = 0.1, 
              animate = TRUE)
)

# Define body
body <- dashboardBody(
  tabItems(
    tabItem(
      tabName = "results",
      fluidRow(
        box(
          title = "Cupping Details",
          width = 12,
          status = "primary",
          solidHeader = TRUE,
          DTOutput("cupping_table")
        )
      )
    ),
    tabItem(
      tabName = "about",
      h3("About This Dashboard"),
      p("This dashboard displays coffee cupping data that were collected from the Coffee Quality Institute's review pages in Jnuary 2018. This data is for both Arabica and Robusta beans, across many countries and professionally rated on a 0-100 scale. All sorts of scoring/ratings for things like acidity, sweetness, fragrance, balance, etc."),
      # Add image to the About tab
      img(src = "www/istockphoto.jpg", height = "300px", width = "auto", alt = "Coffee Image"),
      p("Love Coffee~~~.")
    ),
    
    
    # Create New Entry Tab
    tabItem(
      tabName = "new_entry",
      fluidRow(
        box(
          title = "Add New Cupping Record", width = 12, status = "success", solidHeader = TRUE,
          fluidRow(
            column(4,
                   textInput("input_farm", "Farm Name"),
                   textInput("input_roaster", "Roaster"),
                   textInput("input_origin", "Country of Origin"),
                   selectInput("input_species", "Species", choices = NULL),
                   selectInput("input_method", "Processing Method", choices = NULL)
            ),
            column(4,
                   numericInput("input_aroma", "Aroma", value = NA, min = 0, max = 10),
                   numericInput("input_flavor", "Flavor", value = NA, min = 0, max = 10),
                   numericInput("input_aftertaste", "Aftertaste", value = NA, min = 0, max = 10),
                   numericInput("input_acidity", "Acidity", value = NA, min = 0, max = 10),
                   numericInput("input_body", "Body", value = NA, min = 0, max = 10),
                   numericInput("input_balance", "Balance", value = NA, min = 0, max = 10)
            ),
            column(4,
                   numericInput("input_uniformity", "Uniformity", value = NA, min = 0, max = 10),
                   numericInput("input_clean_cup", "Clean Cup", value = NA, min = 0, max = 10),
                   numericInput("input_sweetness", "Sweetness", value = NA, min = 0, max = 10),
                   numericInput("input_cupper_points", "Cupper Points", value = NA, min = 0, max = 10),
                   numericInput("input_moisture", "Moisture", value = NA, min = 0, max = 10),
                   actionButton("submit_entry", "Submit Record", class = "btn-success")
            )
          )
        )
      )
    )
  )
)

# Define UI
ui <- dashboardPage(header, sidebar, body)

# Define server
server <- function(input, output, session) {
  
  # Query to fetch all cupping data
  full_data <- reactive({
    query <- "
      SELECT
        cf.farm_name,
        r.producer AS roaster,
        cb.country_of_origin,
        s.species,
        pm.processing_method,
        c.total_cup_points,
        c.grading_date,
        c.aroma, c.flavor, c.aftertaste, c.acidity, c.body, c.balance,
        c.uniformity, c.clean_cup, c.sweetness, c.cupper_points, c.moisture,
        c.category_one_defects, c.quakers, c.category_two_defects
      FROM Result res
      LEFT JOIN Cupping c ON res.cupping_id = c.cupping_id
      LEFT JOIN CoffeeFarm cf ON res.farm_id = cf.farm_id
      LEFT JOIN Roaster r ON res.roaster_id = r.roaster_id
      LEFT JOIN CoffeeBean cb ON cf.coffee_bean_id = cb.coffee_bean_id
      LEFT JOIN Species s ON cb.species_id = s.species_id
      LEFT JOIN ProcessingMethod pm ON cb.method_id = pm.method_id
    "
    dbGetQuery(con, query)
  })
  
  
  # Update filter choices
  observe({
    data <- full_data()
    updateSelectInput(session, "farm_filter", choices = c("All", unique(data$farm_name)))
    updateSelectInput(session, "bean_filter", choices = c("All", unique(data$country_of_origin)))
    updateSelectInput(session, "roaster_filter", choices = c("All", unique(data$roaster)))
    species_list <- dbGetQuery(con, "SELECT DISTINCT species FROM Species")
    updateSelectInput(session, "species_filter", choices = c("All", species_list$species))
    method_list <- dbGetQuery(con, "SELECT DISTINCT processing_method FROM ProcessingMethod")
    updateSelectInput(session, "method_filter", choices = c("All", method_list$processing_method))
    updateSelectInput(session, "input_species", choices = unique(data$species))
    updateSelectInput(session, "input_method", choices = unique(data$processing_method))
  })
  
  # Reactive filtered data
  filtered_data <- reactive({
    data <- full_data()
    
    if (input$farm_filter != "All") {
      data <- subset(data, farm_name == input$farm_filter)
    }
    if (input$bean_filter != "All") {
      data <- subset(data, country_of_origin == input$bean_filter)
    }
    if (input$roaster_filter != "All") {
      data <- subset(data, roaster == input$roaster_filter)
    }
    if (input$species_filter != "All") {
      data <- subset(data, species == input$species_filter)
    }
    if (input$method_filter != "All") {
      data <- subset(data, processing_method == input$method_filter)
    }
    
    # Filter by Total Cupping Score Range
    data <- subset(data, total_cup_points >= input$score_range[1] & total_cup_points <= input$score_range[2])
    
    # Only keep cupping fields on Cupping details
    data <- data[, c(
      "grading_date", "total_cup_points", "aroma", "flavor", "aftertaste",
      "acidity", "body", "balance", "uniformity", "clean_cup", "sweetness",
      "cupper_points", "moisture"
    )]
    
    # Rename headers
    colnames(data) <- c(
      "Date", "Total Points", "Aroma", "Flavor", "Aftertaste", "Acidity",
      "Body", "Balance", "Uniformity", "Clean Cup", "Sweetness",
      "Cupper Points", "Moisture"
    )
    data
  })
  
  # ChatGPT suggested to add "server = TRUE" to improve performance for large datasets.
  output$cupping_table <- renderDT({
    datatable(filtered_data(), options = list(pageLength = 10, server = TRUE))
  })
  
  # Save new cupping record
  observeEvent(input$submit_entry, {
    # Calculate total points
    total_points <- sum(input$input_aroma, input$input_flavor, input$input_aftertaste,
                        input$input_acidity, input$input_body, input$input_balance, 
                        input$input_uniformity, input$input_clean_cup, input$input_sweetness,
                        input$input_cupper_points, input$input_moisture, na.rm = TRUE)
    #vChatGPT helped me to fix issue - "9 values for 13 columns"
    dbExecute(con, "
      INSERT INTO Cupping (
        aroma, flavor, aftertaste, acidity, body, balance, uniformity, clean_cup, sweetness,
        cupper_points, moisture, total_cup_points, grading_date
      ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, DATE('now')) 
    ",
              params = list(
                input$input_aroma, input$input_flavor, input$input_aftertaste,
                input$input_acidity, input$input_body, input$input_balance,
                input$input_uniformity, input$input_clean_cup, input$input_sweetness,
                input$input_cupper_points, input$input_moisture, total_points
              ))
    
    showModal(modalDialog(
      title = "Success",
      "New cupping record has been submitted.",
      easyClose = TRUE
    ))
  })  
  
  # Disconnect from DB when session ends
  session$onSessionEnded(function() {
    dbDisconnect(con)
  })
}

# Run the app
shinyApp(ui, server)
