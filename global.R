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
library(shinydashboard)
library(DT)
library(tidyverse)

########################################
########################################
# Load public data
########################################
########################################

# RNA-seq

publicExpressionLoc = 'testdata/PublicExpression/'
publicExpression = list()

for (resource in list.files(publicExpressionLoc, pattern=".palantir.public.rnaseq")) {
  name = sub(".palantir.public.rnaseq","",resource)
  publicExpression[[name]] = readr::read_tsv(paste(publicExpressionLoc, resource, sep=""), comment = "#")
}

# Essentialomes

essentialomeLoc = 'testdata/Essentialomes/'
essentialomes = list()

for (resource in list.files(essentialomeLoc, pattern=".palantir.public.essentialome")) {
  name = sub(".palantir.public.essentialome","",resource)
  essentialomes[[name]] = readr::read_tsv(paste(essentialomeLoc, resource, sep=""), comment = "#")
}