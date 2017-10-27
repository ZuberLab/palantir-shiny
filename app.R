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

library(shiny)
library(DT)
library(shinydashboard)

########################################
########################################
# Load public data
########################################
########################################

# RNA-seq

publicRNASeqLoc = 'testdata/PublicExpression/'
publicRNASeq = list()
publicRNASeqMenuItemList <- list()
publicRNASeqTabItemList <- list()

for (resource in list.files(publicRNASeqLoc, pattern=".palantir.public.rnaseq")) {
  name = sub(".palantir.public.rnaseq","",resource)
  publicRNASeq[[name]] = readr::read_tsv(paste(publicRNASeqLoc, resource, sep=""), comment = "#")
  publicRNASeqMenuItemList[[length(publicRNASeqMenuItemList)+1]] <- menuSubItem(name, tabName = name, icon = icon('caret-square-o-right'))
  publicRNASeqTabItemList[[length(publicRNASeqTabItemList)+1]] <- tabItem(tabName = name, h2(name))
}

# Essentialomes

essentialomeLoc = 'testdata/Essentialomes/'
essentialomes = list()
essentialomesMenuItemList <- list()

for (resource in list.files(essentialomeLoc, pattern=".palantir.public.essentialome")) {
  name = sub(".palantir.public.essentialome","",resource)
  essentialomes[[name]] = readr::read_tsv(paste(essentialomeLoc, resource, sep=""), comment = "#")
  essentialomesMenuItemList[[length(essentialomesMenuItemList)+1]] <- menuSubItem(name, tabName = name, icon = icon('caret-square-o-right'))
}

########################################
########################################
# UI
########################################
########################################

sideBar <- dashboardSidebar(
  h3("Lab data"),
  sidebarMenuOutput("private"),
  h3("Public resources"),
  sidebarMenuOutput("public")
)

body <- dashboardBody(
  uiOutput("ui")
)

ui <- dashboardPage(
  dashboardHeader(title = "Palantir"),
  sideBar,
  body,
  skin = "green"
)

########################################
########################################
# Server
########################################
########################################

# Create RNASeq dynamic tab

fillRNASeqTab <- function() {
  tabItem(tabName = "samples",
          h2("Samples"),
          fluidRow(
            box(
                column(6,
                       uiOutput("symbolFilter")
                ),
                column(6,
                       uiOutput("cellLineFilter")
                )
            )
          ),
          fluidRow(
            column(6,
              DT::dataTableOutput("mytable1")
            )
          ),
          fluidRow(
            column(2,
              downloadButton("downloadSelection", label = "Download selection", class = "fa fa-filter")
            ),
            column(2,
              downloadButton("downloadAll", label = "Download all")
            )
          )
  )
}


server <- function(input, output) {
  set.seed(122)
  histdata <- rnorm(500)
  
  datasetInput <- reactive({
    publicRNASeq[[1]]
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
                choices = sort(datasetInput()$Symbol),
                multiple = TRUE,
                selectize = TRUE
    )
  })
  
  output$cellLineFilter <- renderUI({
    selectInput("cellLineFilter",
                label = "Cell line",
                choices = names(datasetInput())[-c(1:2)],
                multiple = TRUE,
                selectize = TRUE
    )
  })
  
  output$public <- renderMenu({
    sidebarMenu(
      menuItem("RNA-seq", icon = icon("connectdevelop"),
               publicRNASeqMenuItemList
      ),
      menuItem("Essentialomes", icon = icon("barcode"),
               essentialomesMenuItemList
      )
    )
  })
  
  output$ui <- renderUI({
    tabList <- list(fillRNASeqTab(),
                    tabItem(tabName = "genes",
                            h2("Genes")
                    )
    )
    
    tags$div( c(tabList, publicRNASeqTabItemList) , class = "tab-content" )
  })

  output$mytable1 <- DT::renderDataTable({
    
    cellLineChoice <- input$cellLineFilter
    geneChoice <- input$symbolFilter
    if (is.null(cellLineChoice)) {
      cellLineChoice <- names(datasetInput())[-c(1:2)]
    }
    if (is.null(geneChoice)) {
    dataset <- datasetInput() %>%
      dplyr::select_at(c("Symbol", "entrezID", cellLineChoice))
    } else {
      dataset <- datasetInput() %>%
        dplyr::select_at(c("Symbol", "entrezID", cellLineChoice)) %>%
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
        cellLineChoice <- names(datasetInput())[-c(1:2)]
      }
      if (is.null(geneChoice)) {
        dataset <- datasetInput() %>%
          dplyr::select_at(c("Symbol", "entrezID", cellLineChoice))
      } else {
        dataset <- datasetInput() %>%
          dplyr::select_at(c("Symbol", "entrezID", cellLineChoice)) %>%
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
      write.table(datasetInput(), file, row.names = FALSE, quote=FALSE, sep ="\t")
    }
  )
}

shinyApp(ui, server)