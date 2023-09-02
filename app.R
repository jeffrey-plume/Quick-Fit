library(shiny)
library(drc)
library(plotly)

# UI
ui <- fluidPage(
  titlePanel("Quick Fit"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Choose CSV File",
                placeholder = "File must have 'Dose' and 'Response' columns",
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      tags$hr(),
      
      numericInput("slope", 'Slope', value = 1, min = 0, max = NA, step = 0.1),
      numericInput("min", 'Min', value = 0, min = 0, max = NA, step = 1),
      numericInput("max", 'Max', min = 0, value = 100, max = NA, step = 1)
      
      ),
    
    mainPanel(
      plotlyOutput("dosePlot")
    )
  )
)

# Server
server <- function(input, output, session = session) {
  
  dataInput <- reactive({
    inFile <- input$file1
    
    if (is.null(inFile)) {
      data <- data.frame(Dose = 100 * 10^(-0.5*(0:9)),
                         Response = c(5, 10, 20, 35, 50, 65, 75, 85, 92, 95))
    } else{
      data <- read.csv(inFile$datapath, header = input$header)
    }
    
    return(data)
  })
  
  model <- reactive({
    drm(Response~Dose, data = dataInput(), fct = LL.4(),
        lowerl = c(-Inf, 0, 0, -Inf))
  })
  
  observe({
    if (is.null(model())) {
      return(NULL)
    }
    
    isolate({
      
      updateNumericInput(session = session, inputId = 'slope', value = round(coef(model())[[1]], digits =  1))
      updateNumericInput(session = session, inputId = 'min', value = round(coef(model())[[2]]))
      updateNumericInput(session = session, inputId = 'max', value = round(coef(model())[[3]]))
      
    })

  })

  
  output$dosePlot <- renderPlotly({
    if (is.null(dataInput())) {
      return(NULL)
    }
    
    
    newModel <- drm(Response~Dose, 
                    data = dataInput(), 
                    fct = LL.4(fixed = c(input$slope, input$min, input$max, NA)))
    
    p <- ggplot(dataInput(), aes(x = Dose, y = Response)) +
      geom_point() +
      geom_line(aes(y = predict(newModel, newdata = dataInput())), color = "blue") +
      annotate('text', label = paste("EC50 =", round(coef(newModel)[[1]], digits = 3)),
                                     x = as.numeric(coef(newModel)[[1]])*1.5, y =  55) +
      labs(title = "Dose-Response Curve", x = "Dose", y = "Response") +
      scale_x_log10()
    
    ggplotly(p)
  })
}

# Run App
shinyApp(ui, server)
