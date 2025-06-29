library(shiny)
source("R/analysis.R")

ui <- fluidPage(
  titlePanel("Análisis Estadístico con Shiny"),
  sidebarLayout(
    sidebarPanel(
      fileInput("datafile", "Carga tu archivo CSV", accept = ".csv"),
      selectInput("variable", "Selecciona variable", choices = NULL),
      actionButton("analizar", "Analizar")
    ),
    mainPanel(
      verbatimTextOutput("summary"),
      plotOutput("histplot")
    )
  )
)

server <- function(input, output, session) {
  dataset <- reactiveVal()
  
  observeEvent(input$datafile, {
    df <- read.csv(input$datafile$datapath)
    dataset(df)
    updateSelectInput(session, "variable", choices = names(df))
  })
  
  analysis <- eventReactive(input$analizar, {
    req(dataset(), input$variable)
    variable <- input$variable
    data <- dataset()[[variable]]
    list(
      summary = summary_statistics(data),
      histdata = data
    )
  })
  
  output$summary <- renderPrint({
    req(analysis())
    analysis()$summary
  })
  
  output$histplot <- renderPlot({
    req(analysis())
    hist(analysis()$histdata, main = paste("Histograma de", input$variable), col = "skyblue")
  })
}

shinyApp(ui, server)