library(shiny)
library(DT)

data <- read_tsv("../../../resources/RNASeq/amit/amit.palantir.public.rnaseq", comment = "#")

ui <- fluidPage(
  
  titlePanel("Expression Data"),
  
  sidebarLayout(
    sidebarPanel(
      
      selectInput(inputId = "dataset",
                  label = "Choose a dataset:",
                  choices = c("amit")), 
      uiOutput("celllineSelection"), 
      uiOutput("geneSelection")
    ),
    
    mainPanel(
      DT::dataTableOutput("expression")
    )
  )
)

# Server
server <- function(input, output) {

  datasetInput <- reactive({
    switch(input$dataset,
           "amit" = data)
  })
  
  output$celllineSelection <- renderUI({
    selectInput(inputId = "cellline", 
                label = "Choose a cellline:", 
                choices = names(datasetInput())[-c(1:2)], 
                selected = names(datasetInput())[3], 
                multiple = TRUE,
                selectize = TRUE)
  })
  
  output$geneSelection <- renderUI({
    selectInput(inputId = "symbol", 
                label = "Choose a gene symbol:", 
                choices = sort(data$Symbol), 
                selected = sort(data$Symbol)[1], 
                multiple = TRUE,
                selectize = TRUE)
  })

  output$expression <- renderDataTable({
    dataset <- datasetInput() %>%
      dplyr::select_at(c("Symbol", "entrezID", input$cellline)) %>%
      dplyr::filter(Symbol %in% input$symbol)
  })
  
}

shinyApp(ui, server)