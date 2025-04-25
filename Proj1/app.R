
#Importing libraries
library(shiny)
library(shinythemes)
library(shinydashboard)
library(data.table)
library(caret)
library(xgboost)

# Reading data

hous <- read.csv("h-5M.csv", stringsAsFactors = FALSE)

# Defining predictor and response variables in training set
hous_x = data.matrix(hous[, -1])
hous_y = hous[,1]
# Defining final training set
xgb_hous = xgb.DMatrix(data = hous_x, label = hous_y)

# Building a model
model <- xgboost(data = xgb_hous, 
                 nrounds = 20,
                 eta = 0.5,
                 max_depth = 3,
                 gamma = 0,
                 subsample=1,
                 colsample_bytree=1,
                 min_child_weight=2
)

# Save model to RDS file
saveRDS(model, "model.rds")

# Read in the RF model
model <- readRDS("model.rds")



# User interface    
ui <- dashboardPage(
  dashboardHeader(title = "House Valuation App"),
  dashboardSidebar(
                   sidebarMenu(
                        menuItem(" About", tabName = "about", icon=icon("house")),
                        menuItem(" House Value Calculator", tabName = "valuation", icon=icon("calculator")),
                        menuItem(" Housing Market Analysis", tabName = "analysis", icon=icon("chart-line"))
                              )
                   ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "about",
              fluidPage(
                h1("How much is your house worth?"),
                h2("Get an instant home valuation."),
                tags$img(src="Houses on coin stacks.jpg", height=195, width=420),
                h5("Following the publication of the Local Property Tax Bill on 7th April 2025, 
                   property owners must self-assess the market value of their house as of 1st November 2025. "),
                h5("Head over to our House Value Calculator to get an instant property valuation.")
              )
            ),
      
      # Second tab content
      tabItem(tabName = "valuation",
              h2("Property Valuation Tool"),
              fluidPage(
                
                # Input values
                sidebarPanel(
                  h3("Please specify the parameters"),
                  
                  selectInput("region", label = "Region:", 
                              choices = list("Dublin" = "dublin", "Cork" = "cork", "Galway" = "galway", 
                                             "Dublin Inner Ring" = "dub_inner_ring", "Dublin Outer Ring" = "dub_outer_ring", 
                                             "Urban South" = "urban_south", "Other" = "other"), 
                              selected = "Dublin"),
                  selectInput("house_type", label = "House Type:", 
                              choices = list("Detached/Bungalow"="detached", "Semi-D/End of Terrace"="semi_d", 
                                             "Terrace/Townhouse"="terrace", "Apartment/Duplex"="apartment"),
                              selected = "Detached/Bungalow"),
                  sliderInput("bed_no", "No. of Bedrooms:",
                              min = 1, max =7,
                              value = 3),
                  sliderInput("bath_no", "No. of Bathrooms:",
                              min = 1, max =7,
                              value = 2),
                  textInput("size", "House Size in Sq M", "100"),
                  
                  actionButton("submitbutton", "Submit", class = "btn btn-primary")
                  ),
                
                mainPanel(
                  tags$label(h3('Result')), # Output Text Box
                  verbatimTextOutput('contents'),
                  tableOutput('tabledata') # Prediction results table
                  
                )
              )
        ),
      
      # Third tab content
      tabItem(tabName = "analysis",
              h2("Analysis"),
              fluidPage(
                
                # sidebar for slider
                sidebarLayout(
                  sidebarPanel(
                    sliderInput(inputId = "bins",
                                label = "number of bins:",
                                min = 1,
                                max = 50,
                                value = 30
                      
                    )
                  ),
                  mainPanel(
                    plotOutput(outputId = "distPlot")
                  )
                
                )
              )
      )
    )
  )
)

# Server                           
server <- function(input, output, session) {
  
  # Input Data
  datasetInput <- reactive({  
    
    # Data Frame of variables
    df <- data.frame(
      Name = c("region",
               "house_type",
               "bed_no",
               "bath_no",
               "size"),
      Value = as.character(c(input$region,
                             input$house_type,
                             input$bed_no,
                             input$bath_no,
                             input$size)),
      stringsAsFactors = FALSE)
    
    play <- "play"
    df <- rbind(df, play)
    input <- transpose(df)
    write.table(input,"input.csv", sep=",", quote = FALSE, row.names = FALSE, col.names = FALSE)
    
    test <- read.csv(paste("input", ".csv", sep=""), header = TRUE)
    
    # test$price <- factor(test$outlook, levels = c("overcast", "rainy", "sunny"))
    
    
    Output <- data.frame(Prediction=predict(model,test))
    print(Output)
    
  })
  
  # Output Text Box
  output$contents <- renderPrint({
    if (input$submitbutton>0) { 
      isolate("Calculation complete.") 
    } else {
      return("Server is ready for calculation.")
    }
  })
  
  # Prediction results table
  output$tabledata <- renderTable({
    if (input$submitbutton>0) { 
      isolate(datasetInput()) 
    } 
  })
  
  output$distPlot <- renderPlot({
    
    x <- hous$sold_price_eur
    bins <- seq(min(x), max(x), length.out = input$bins +1)
    
    hist(x, main="Histogram of Sold Price", xlab="Price (mln)", ylab="Count", col="#75AADB", breaks = bins)
  })
  
}

# Creating the shiny app             
shinyApp(ui = ui, server = server)