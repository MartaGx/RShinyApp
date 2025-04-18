
#Importing libraries
library(shiny)
library(shinythemes)
library(data.table)
library(caret)
library(xgboost)

# Reading data

hous <- read.csv("h-5M.csv", stringsAsFactors = FALSE)
hous <- data.matrix(hous)

# Building a model
model <- xgboost(data = hous, 
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
ui <- fluidPage(theme = shinytheme("united"), # or slate
                
                # Page header
                headerPanel('House Value Calculator'),
                
                # Input values
                sidebarPanel(
                  HTML("<h3>Input parameters</h3>"),
                  
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
    
    test$outlook <- factor(test$outlook, levels = c("overcast", "rainy", "sunny"))
    
    
    Output <- data.frame(Prediction=predict(model,test), round(predict(model,test,type="prob"), 3))
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
  
}

# Creating the shiny app             
shinyApp(ui = ui, server = server)