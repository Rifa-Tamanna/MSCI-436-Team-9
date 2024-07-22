library(shiny)
library(randomForest)

# Loading the model  
rf_model <- readRDS("rds_files/rf_model.rds")
plot_genre <- readRDS("rds_files/plot_genre.rds")
plot_rating <- readRDS("rds_files/plot_rating.rds")


# Defining the UI components
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .output-box {
        background-color: #f8f8f8;
        border: 1px solid #ddd;
        border-radius: 5px;
        padding: 15px;
        margin-bottom: 15px;
      }
      .output-title {
        font-weight: bold;
        margin-bottom: 10px;
        color: #333;
      }
      .output-content pre {
        font-family: 'Arial';
        white-space: normal;
        word-wrap: normal;
        background-color: transparent;
        border: none;
        word-break: keep-all;
        overflow-wrap: normal;
        overflow: hidden;
      }
    "))
  ),
  
  # UI for title
  titlePanel(div(
    h2("Hit or Flop", style = "color: #333; font-weight: bold;"),
    h3("Movie Gross Revenue Predictor", style = "color: #666;")
  )),
  
  # UI for sidebar
  sidebarLayout(
    sidebarPanel(
      numericInput("runtime", "Movie Runtime (in minutes)", value = 120, min = 1),
      
      numericInput("budget", "Movie Budget (in $1000)", value = 50000, min = 1),
      
      selectInput("rating", "Movie Rating", choices = c("G", "PG", "PG-13"="PG.13", "R"), selected = "G"),
      
      selectInput("genre", "Genre",
                  choices = c("Action","Adventure", "Animation", "Biography", "Comedy", "Crime", "Drama", "Fantasy", "Horror"), 
                  selected = "Action"),
      
      tags$head(tags$style(".sidebar-panel {
                        padding: 5px;
                        background-color: #f5f5f5;
                        border: 1px solid #ddd;
                        border-radius: 2px;
                        margin-bottom: 10px;
                      }")),
      
      checkboxInput("show_genre", "Show Genre Histogram", value = TRUE),
      
      checkboxInput("show_budget", "Show Budget Histogram", value = TRUE),
      
      checkboxInput("show_rating", "Show Rating Histogram", value = TRUE)

    ),
    
    # UI for prediction, suggestions, and graphs
    mainPanel(
        h3("Predicted Movie Success:"),
        
        div(class = "output-box",
            div(class = "output-title", "Prediction"),
            div(class = "output-content", verbatimTextOutput("prediction")),
            div(class = "output-content", verbatimTextOutput("bin")),
            div(class = "output-title", "Budget Suggestions"),
            div(class = "output-content", verbatimTextOutput("suggestions_budget")),
            div(class = "output-title", "Runtime Suggestions"),
            div(class = "output-content", verbatimTextOutput("suggestions_runtime"))
        ),
        
      br(), 
  
      fluidRow(
        column(width = 12,
               conditionalPanel(
                 condition = "input.show_budget == true",
                 plotOutput("budget_hist", height = "300px")
               )
        )
      ),
      
      tags$div(style = "height: 30px;"),
      
      fluidRow(
        column(width = 6,
               conditionalPanel(
                 condition = "input.show_genre == true",
                 plotOutput("genre_hist", height = "300px")
               )
        ),
        column(width = 6,
               conditionalPanel(
                 condition = "input.show_rating == true",
                 plotOutput("rating_hist", height = "300px")
               )
        )
      )
      
    )
  )
)

# Defining the server
server <- function(input, output) {
  
  # Expression for genre data 
  genre_data <- reactive({
    genres <- c("Action", "Adventure", "Animation", "Biography", "Comedy", "Crime", "Drama", "Fantasy", "Horror")
    data <- as.data.frame(matrix(0, nrow = 1, ncol = length(genres)))
    colnames(data) <- paste0("genre", genres)
    data[, paste0("genre", input$genre)] <- 1
    return(data)
  })
  
  # Expression for rating data
  rating_data <- reactive({
    ratings <- c("G", "PG", "PG.13", "R")
    data <- as.data.frame(matrix(0, nrow = 1, ncol = length(ratings)))
    colnames(data) <- paste0("rating", ratings)
    data[, paste0("rating", input$rating)] <- 1
    print(data)
    return(data)
  })
  
  # Function to prepare input data for prediction
  prepare_input <- reactive({
    genre_df <- genre_data()
    rating_df <- rating_data()
    
    new_data <- data.frame(
      name = "prediction",
      runtime = input$runtime,
      budget = input$budget *1000,
      genre_df,
      rating_df
    )
    
    saveRDS(new_data, file = "rds_files/test_data.rds")
    print(new_data$budget)
    
    print(str(new_data))
    
    return(new_data)
  })
  

  # Make prediction 
  prediction <- reactive({
    new_data <- prepare_input()
    tryCatch({
      predict(rf_model, newdata = new_data)
    }, error = function(e) {
      # Print the error message
      print(e)
      NA
    })
  })
  
  # Output prediction
  output$prediction <- renderText({
    pred <- prediction()
    if (is.na(pred)) {
      "Error in prediction. Check the console for details."
    } else {
      paste("Predicted Gross Revenue: $", format(round(pred, 2), big.mark = ","))
    }
  })
  
  # Finding the bin 
  output$bin <- renderText({
    pred <- prediction()
    if (is.na(pred)) {
      "Unable to determine bin due to prediction error."
    } else {
      bins <- c(0, 7500000, 25000000, 50000000, 150000000, Inf)
      labels <- c("Very low", "Low", "Medium", "High", "Very high")
      bin <- cut(pred, breaks = bins, labels = labels, include.lowest = TRUE)
      paste("Predicted Revenue Bin:", as.character(bin))
    }
  })
  

  # Finding the bin
  predicted_bin <- reactive({
    pred <- prediction()
    if (is.na(pred)) {
      NA
    } else {
      bins <- c(0, 7500000, 25000000, 50000000, 150000000, Inf)
      labels <- c("Very low", "Low", "Medium", "High", "Very high")
      as.character(cut(pred, breaks = bins, labels = labels, include.lowest = TRUE))
    }
  })
  
  # Outputting genre histogram
  output$genre_hist <- renderPlot({
    plot_genre(input$genre, predicted_bin())
  })
  
  # Outputting budget scatterplot
  output$budget_hist <- renderPlot({
    pred <- prediction()
    if (is.na(pred)) {
      plot_budget(input$genre, input$budget*1000, NA)
    } else {
      plot_budget(input$genre, input$budget*1000, pred)
    }
  })

  # Outputting rating histogram
  output$rating_hist <- renderPlot({
    rating <- input$rating
    if (rating == "PG.13"){
      rating <- "PG-13"
    }
    plot_rating(rating, predicted_bin())
  })
  
  output$suggestions_budget <- renderText({
    new_data <- prepare_input()
    budget_suggestion(new_data)
  })
  
  output$suggestions_runtime <- renderText({
    new_data <- prepare_input()
    runtime_suggestion(new_data)
  })
}

# Create the Shiny app object
shinyApp(ui = ui, server = server)

