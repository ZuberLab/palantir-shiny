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
            box(title = "Box title",
                column(4,
                       selectInput("entrezFilter",
                                   "EntrezID",
                                   unique(as.character(publicRNASeq[[1]]$entrezID)))
                       ),
                column(4,
                       selectInput("symbolFilter",
                                   "Gene",
                                   unique(as.character(publicRNASeq[[1]]$Symbol)))
                )
            )
          ),
          fluidRow(
            DT::dataTableOutput("mytable1")
          )
  )
}


server <- function(input, output) {
  set.seed(122)
  histdata <- rnorm(500)
  
  output$private <- renderMenu({
    sidebarMenu(
      menuItem("Samples", tabName = "samples", icon = icon("files-o")),
      menuItem("Genes", tabName = "genes", icon = icon("empire"))
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
    DT::datatable(publicRNASeq[[1]])
  })
}

shinyApp(ui, server)