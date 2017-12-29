# Palantir-shiny

# Copyright (c) 2017 Tobias Neumann, Jesse Lipp.
# 
# Palantir-shiny is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as
# published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.
# 
# Palantir-shiny is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Affero General Public License for more details.
# 
# You should have received a copy of the GNU Affero General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

########################################
########################################
# Tab creations
########################################
########################################

fillPublicExpressionTab <- function() {
  tabItem(tabName = "publicBrowse",
          h2("Samples"),
          fluidRow(
            column(width = 9,
                   box(width = NULL, solidHeader = TRUE, status = "primary",
                       div(style = 'overflow-x: scroll', DT::dataTableOutput("publicExpressionDT"))
                   )
            ),
            column(width = 3,
                   box(width = NULL, solidHeader = TRUE,
                       selectInput(inputId = "selectPublicExpression",
                                   label = "Public Expression Repository:",
                                   choices = names(publicExpression),
                                   selectize = TRUE),
                       uiOutput("symbolFilter"),
                       uiOutput("cellLineFilter")
                   ),
                   box(width = NULL, solidHeader = TRUE,
                       infoBoxOutput(width = NULL, "publicExpressionBoxGenesTotal"),
                       infoBoxOutput(width = NULL, "publicExpressionBoxSamplesTotal"),
                       div(style="display:inline-block", downloadButton("downloadSelection", label = "Get selection")),
                       div(style="display:inline-block",downloadButton("downloadAll", label = "Download all"))
                   )
            )
          )
  )
}

########################################
########################################
# Server
########################################
########################################

server <- function(input, output) {
  set.seed(122)
  histdata <- rnorm(500)
  
  publicExpressionReactive <- reactive({
    publicExpression[[input$selectPublicExpression]] %>% arrange(Symbol)
  })
  
  essentialomeInput <- reactive({
    essentialomes[[input$essentialomeInput]]
  })
  
  output$private <- renderMenu({
    sidebarMenu(
      menuItem("Samples", tabName = "samples", icon = icon("files-o")),
      menuItem("Genes", tabName = "genes", icon = icon("empire"))
    )
  })
  
  output$symbolFilter <- renderUI({
    selectInput("symbolFilter",
                label = "Gene",
                choices = sort(publicExpressionReactive()$Symbol),
                multiple = TRUE,
                selectize = TRUE
    )
  })
  
  output$cellLineFilter <- renderUI({
    selectInput("cellLineFilter",
                label = "Cell line",
                choices = names(publicExpressionReactive())[-c(1:2)],
                multiple = TRUE,
                selectize = TRUE
    )
  })
  
  output$public <- renderMenu({
    sidebarMenu(
      menuItem("RNA-seq", icon = icon("connectdevelop"), tabName = "publicBrowse"),
      menuItem("Essentialomes", icon = icon("barcode"),tabName = "essentialomeBrowse")
    )
  })
  
  output$ui <- renderUI({
    tabItems(
    fillPublicExpressionTab(),
    tabItem(tabName = "genes",
                            h2("Genes")
                    )#,fillEssentialomeTab()
    )
  })
  
  output$essentialomeTable <- DT::renderDataTable({
    
    cellLineChoice <- input$cellLineFilter
    geneChoice <- input$symbolFilter
    
    if (is.null(cellLineChoice)) {
      cellLineChoice <- names(essentialomeInput())[-c(1:2)]
    }
    if (is.null(geneChoice)) {
      dataset <- essentialomeInput() %>%
        dplyr::select_at(c("Symbol", "EntrezID", cellLineChoice))
    } else {
      dataset <- essentialomeInput() %>%
        dplyr::select_at(c("Symbol", "EntrezID", cellLineChoice)) %>%
        dplyr::filter(Symbol %in% input$symbolFilter)
    }
    
  }, options = list(dom = 'ltipr'), rownames= FALSE
  )
  
  output$publicExpressionDT <- DT::renderDataTable({
    
    cellLineChoice <- input$cellLineFilter
    geneChoice <- input$symbolFilter
    if (is.null(cellLineChoice)) {
      cellLineChoice <- names(publicExpressionReactive())[-c(1:2)]
    }
    if (is.null(geneChoice)) {
      dataset <- publicExpressionReactive() %>%
        dplyr::select_at(c("Symbol", "EntrezID", cellLineChoice))
    } else {
      dataset <- publicExpressionReactive() %>%
        dplyr::select_at(c("Symbol", "EntrezID", cellLineChoice)) %>%
        dplyr::filter(Symbol %in% input$symbolFilter)
    }
    
  }, options = list(dom = 'ltipr'), rownames= FALSE
  )
  
  output$downloadSelection <- downloadHandler(
    filename = function() {
      "download.tsv"
    },
    content = function(file) {
      
      cellLineChoice <- input$cellLineFilter
      geneChoice <- input$symbolFilter
      if (is.null(cellLineChoice)) {
        cellLineChoice <- names(publicExpressionReactive())[-c(1:2)]
      }
      if (is.null(geneChoice)) {
        dataset <- publicExpressionReactive() %>%
          dplyr::select_at(c("Symbol", "EntrezID", cellLineChoice))
      } else {
        dataset <- publicExpressionReactive() %>%
          dplyr::select_at(c("Symbol", "EntrezID", cellLineChoice)) %>%
          dplyr::filter(Symbol %in% input$symbolFilter)
      }
      
      write.table(dataset, file, row.names = FALSE, quote=FALSE, sep ="\t")
    }
  )
  
  output$downloadAll <- downloadHandler(
    filename = function() {
      "download.tsv"
    },
    content = function(file) {
      write.table(publicExpressionReactive(), file, row.names = FALSE, quote=FALSE, sep ="\t")
    }
  )
  
  output$publicExpressionBoxGenesTotal <- renderInfoBox({
    infoBox(title = "Number of Genes", 
            value = publicExpressionReactive() %>% nrow())
  })
  
  output$publicExpressionBoxSamplesTotal <- renderInfoBox({
    infoBox(title = "Number of Samples", 
            value = publicExpressionReactive() %>% ncol() - 2)
  })
}