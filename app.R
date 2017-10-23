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

for (resource in list.files(publicRNASeqLoc, pattern=".palantir.public.rnaseq")) {
  name = sub(".palantir.public.rnaseq","",resource)
  publicRNASeq[[name]] = readr::read_tsv(paste(publicRNASeqLoc, resource, sep=""), comment = "#")
  publicRNASeqMenuItemList[[length(publicRNASeqMenuItemList)+1]] <- menuSubItem(name, tabName = name, icon = icon('caret-square-o-right'))
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
  tabItems(
    # First tab content
    tabItem(tabName = "dashboard",
            fluidRow(
              box(plotOutput("plot1", height = 250)),
              
              box(
                title = "Controls",
                sliderInput("slider", "Number of observations:", 1, 100, 50)
              )
            )
    ),
    
    # Second tab content
    tabItem(tabName = "widgets",
            h2("Widgets tab content")
    )
  )
)

ui <- dashboardPage(
  dashboardHeader(title = "Palantir"),
  sideBar,
  body
)

########################################
########################################
# Server
########################################
########################################

server <- function(input, output) {
  set.seed(122)
  histdata <- rnorm(500)
  
  output$private <- renderMenu({
    sidebarMenu(
      menuItem("Samples", icon = icon("files-o")),
      menuItem("Genes", icon = icon("empire"))
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
  
  

  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
}

shinyApp(ui, server)