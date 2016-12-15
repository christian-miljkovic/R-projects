library(shiny)
source("/Users/christianmiljkovic/Desktop/Data_Analytics/app/Rental_Analytics.R")

ui = fluidPage( 
  #  Application title
  titlePanel("Rent Pricer"),
  
  # Sidebar with sliders that demonstrate various available
  # options
    selectInput(inputId = "beds",
                label = "Number of bedrooms:",
                choices = c(1:6),
                selected = 2),
      

    selectInput(inputId = "bath",
                label = "Number of bathrooms",
                choices = c(1:6),
                selected = 1),
      
      # Specification of range within an interval
    selectInput(inputId = "sqft",
                label = "Number of Square feet:",
                choices = seq(500,5000,by=100),
                selected = 500),
      
      # Provide a custom currency format for value display,
      # with basic animation
      selectInput("dataset", "Choose a Location:", 
                  choices = c("Upper West Side", "West Village", "Tribeca",
                              "Central Harlem","Washington Heights","Battery Park City",
                              "Chelsea","East Village","Financial District","Greenwich Village",
                              "Midtown","Murray Hill","Soho","Upper East Side","Lincoln Square"))
)
  
  
    
    # Show a table summarizing the values entered
    mainPanel(
      tableOutput("values")
    )
  



server = function(input,output) {
  
  
  selectedData <- reactive({
    prediction = my_prediction(input$beds,input$bath,input$dataset,input$sqft)
  })
  
  
  
  output$main_plot <- renderPlot({
    
    output$distPlot <- renderPlot({
      x   <- seq(500,2000000,length=1000)
      
      dnorm(x,mean=prediction, sd=100)
      
    })
    
    
  })
  
}

shinyApp(ui = ui, server = server)


