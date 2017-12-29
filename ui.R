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

# Tab creation functions to be moved to server.R

fillPublicTab <- function() {
  tabItem(tabName = "essentialomeBrowse",
          h2("Essentialomes"),
          fluidRow(
            column(width = 9,
                   box(width = NULL, solidHeader = TRUE, 
                       DT::dataTableOutput("essentialomeTable")
                   )
            ),
            column(width = 3,
                   box(width = NULL, solidHeader = TRUE,
                       selectInput(inputId = "selectEssentialome",
                                   label = "Essentialome:",
                                   choices = names(essentialomes),
                                   selectize = TRUE)
                   )
            )
          )
  )
}


ui <- dashboardPage(
  dashboardHeader(title = "Palantir"),
  sideBar,
  body,
  skin = "green"
)