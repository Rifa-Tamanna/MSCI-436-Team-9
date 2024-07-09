library(shiny)
library(randomForest)

# Load Model 
rf_model <- readRDS("rf_model.rds")
plot_genre <- readRDS("plot_genre.rds")



# Load Data

# Define UI

ui <- fluidPage(
  
  titlePanel("Hit or Flop | Movie Gross Revenue Predictor"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("runtime", "Movie Runtime (in minutes)", value = 120, min = 1),
      
      numericInput("budget", "Movie Budget (in $)", value = 50000000, min = 1),
      
      selectInput("rating", "Movie Rating", choices = c("G", "PG", "PG-13" ="PG.13", "R"), selected = "G"),
      
      selectInput("genre", "Genre",
                  choices = c("Action","Adventure", "Animation", "Biography", "Comedy", "Crime", "Drama", "Fantasy", "Horror"), 
                  selected = "Action"),
      
      checkboxInput("show_genre", "Show Genre Histogram", value = TRUE),
      
      checkboxInput("show_budget", "Show Budget Histogram", value = TRUE)
    ),
    
    mainPanel(
      h3("Predicted Movie Success:"),
      verbatimTextOutput("prediction"),
      verbatimTextOutput("bin"),
      
      conditionalPanel(
        condition = "input.show_genre == true",
        plotOutput("genre_hist")
      ),
      
      conditionalPanel(
        condition = "input.show_budget == true",
        plotOutput("budget_hist")
      )
    )
  )
)

# Define Server
server <- function(input, output) {
  
  # Create a reactive expression for genre data
  genre_data <- reactive({
    genres <- c("Action", "Adventure", "Animation", "Biography", "Comedy", "Crime", "Drama", "Fantasy", "Horror")
    data <- as.data.frame(matrix(0, nrow = 1, ncol = length(genres)))
    colnames(data) <- paste0("genre", genres)
    data[, paste0("genre", input$genre)] <- 1
    return(data)
  })
  
  # Create a reactive expression for rating data
  rating_data <- reactive({
    ratings <- c("G", "PG", "PG.13", "R")
    data <- as.data.frame(matrix(0, nrow = 1, ncol = length(ratings)))
    colnames(data) <- paste0("rating", ratings)
    data[, paste0("rating", input$rating)] <- 1
    return(data)
  })
  
  # Function to prepare input data for prediction
  prepare_input <- reactive({
    genre_df <- genre_data()
    rating_df <- rating_data()
    
    new_data <- data.frame(
      name = "prediction",
      runtime = input$runtime,
      budget = input$budget,
      genre_df,
      rating_df
    )
    
    # Print the structure of new_data
    print(str(new_data))
    
    return(new_data)
  })
  
  # Make prediction (as a reactive expression)
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
  
  # Find the bin 
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
  
  # Find the bin (as a reactive expression)
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
  
  # Genre histogram
  output$genre_hist <- renderPlot({
    plot_genre(input$genre, predicted_bin())
  })
  
  # Budget histogram
  output$budget_hist <- renderPlot({
    pred <- prediction()
    if (is.na(pred)) {
      # Handle the case when prediction fails
      plot_budget(input$genre, input$budget, NA)
    } else {
      plot_budget(input$genre, input$budget, pred)
    }
  })
 
}

# Create the Shiny app object
shinyApp(ui = ui, server = server)

