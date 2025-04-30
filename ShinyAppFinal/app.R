
#Importing libraries
library(shiny)
library(shinythemes)
library(shinydashboard)
library(data.table)
library(caret)
library(xgboost)
library(shinyWidgets)


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

# Function for a hyperlink navigating from Tab 1 to Tab 2
shinyLink <- function(to, label) {
  tags$a(
    class = "shiny__link",
    href = to,
    label
  )
}



##################
# User Interface #
#################
ui <- dashboardPage(skin = "blue",
  dashboardHeader(title = "House Valuation App",
                  tags$li(class = "dropdown",
                          tags$a(icon("magnifying-glass"), icon("circle-user"))) 
                  ),
  dashboardSidebar(
                   sidebarMenu(
                        menuItem("Home", tabName = "home", icon = icon("house")),
                        menuItem("House Value Calculator", tabName = "valuation", icon = icon("calculator")),
                        menuItem("Housing Market Analysis", tabName = "analysis", icon = icon("chart-line"))
                              )
                   ),
  dashboardBody(
    tabItems(
      
      ################
      # Tab 1 content
      tabItem(tabName = "home",
              fluidPage(
                h1("How much is your house worth?"),
                h2("Get an instant home valuation."),
                # image
                tags$img(src="Houses on coin stacks.jpg", height=195, width=420),
                br(),
                br(),
                tags$p("Following the publication of the Local Property Tax Bill on 7th April 2025, 
                   property owners must self-assess the market value of their house as of 1st November 2025. "),
                tags$p("Head over to our ", shinyLink(to = "valuation", label = "House Value Calculator "), "to get an instant property valuation."),
                tags$p("According to the Property Price Register data*, the majority of residential properties in Ireland sold
                   for a price below €1,000,000. However, there are a good few properties with a sold price between €1mln and €2mln, 
                   and four properties, which sold for prices between €2mln and €3.1mln. 
                       To find out more, go to the", shinyLink(to = "analysis", label = "Housing Market Analysis"), 
                       "page, where you will be able to explore the distribution of house prices in Ireland."),
                h6("* source: propertypriceregister.ie, July 2023"),
              )
            ),
      
      ################
      # Tab 2 content
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
                  textInput("size", "House Size in Sq M:", "100"),
                  
                  actionButton("submitbutton", "Submit", class = "btn btn-primary")
                  ),
                
                mainPanel(
                  tags$label(h3('Your house valuation result')), # Output Text Box
                  verbatimTextOutput('contents'),
                  tableOutput('tabledata') # Prediction results table
                  
                )
              )
        ),
      
      ################
      # Tab 3 content
    
      
      tabItem(tabName = "analysis",
              h2("Analysis"),
              h4("Adjust the sliders below to take a closer look at the distribution of the houses with sold price below €1mln."),
              
              
              fluidRow(
                valueBoxOutput("min_", width = 4), 
                valueBoxOutput("median_", width = 4), 
                valueBoxOutput("max_", width = 4)),
              
              fluidRow(
                box(plotOutput(outputId = "histPlot")), 
                box(plotOutput(outputId = "boxPlot"))),
              
              fluidRow(
                chooseSliderSkin(skin = "Modern", color = "#75AADB"),
                box(sliderInput(inputId = "x_range",
                                label= "Select sold price range:",
                                min = 50000,
                                max = 3100000,
                                value = c(50000,3100000), 
                                step = 50000,
                                pre = "€",
                                sep = ","),
                    sliderInput(inputId = "bins",
                                label = "Select the density of the bars on histogram:",
                                min = 1,
                                max = 50,
                                value = 30))), 
      )
    )
  )
)

##################
#     Server    #
#################
                          
server <- function(input, output, session) {
  
  ################################
  # Tab 1 - About
  
  # Hyperlink to the House Value Calculator tab
   observeEvent(input$shinyLink, {
    updateTabItems(session, "tabs", "valuation") # couldn't get this one to work...
  }
  )
  
  ################################
  # Tab 2 - House Value Calculator
  
  #Input Data
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
    
    Output <- data.frame(Prediction=predict(model,test))
    print(Output)
  })
  
  # Output Text Box
  output$contents <- renderPrint({
    if (input$submitbutton>0) { 
      isolate("Valuation complete.") 
    } else {
      return("Server is ready for valuation.")
    }
  })
  
  # Prediction results table
  output$tabledata <- renderTable({
    if (input$submitbutton>0) { 
      isolate(datasetInput()) 
    } 
  })
  
  ###################
  # Tab 3 - Histogram
  
  # Creating function to print currency in valueBox()
  euro_format <- scales::dollar_format(
    prefix = "\u20ac", # the euro symbol
    suffix = "",
    big.mark = ",",
    decimal.mark = ".",
    accuracy = 1)
  
  
  output$min_ <- renderValueBox({
    valueBox(value = euro_format(min(hous$sold_price_eur)),
             subtitle = "minimum sold price",
             icon("caret-down"),
             color = "light-blue")
  })
  
  output$median_ <- renderValueBox({
    valueBox(value = euro_format(median(hous$sold_price_eur)),
             subtitle = "median sold price",
             icon("coins"),
             color = "orange")
  })

  
  output$max_ <- renderValueBox({
    valueBox(value = euro_format(max(hous$sold_price_eur)),
             subtitle = "maximum sold price",
             icon("caret-up"),
             color = "maroon")
  })
  
  options(scipen=999)
  output$histPlot <- renderPlot({
    
    x <- hous$sold_price_eur
    bins <- seq(min(x), max(x), length.out = input$bins +1)
    
    hist(x, main="Histogram of Sold Price", xlab="Price €", ylab="Count", col="#75AADB", breaks = bins, xlim = c(input$x_range[1], input$x_range[2]))
  })
  
  output$boxPlot <- renderPlot({
    
    boxplot(hous$sold_price_eur, main="Boxplot of Sold Price", ylab="Price €", col="#75AADB", ylim = c(input$x_range[1], input$x_range[2]))
  })
  
  
}


###########################
# Creating the shiny app  # 
##########################
shinyApp(ui = ui, server = server)