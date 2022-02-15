#setwd("working dir")

#load custom functions

library(ggplot2)

source("function development.R")
source("function dimensions.R")
source("function dynamic.R")

source("function whichid.R")

source("function nxeldist.R")
source("function nxteltyp.R")

#read PP slides
source("function extract_slides.R")

#assign id to elements
source("function assign_id.R")

#create table for dashboard
source("function dsbTable.R")

#general used vectors
Jahr   <- c("SQ2015", "SQ2016", "SQ2017", "SQ2018", "SQ2020", "Z2015", "Z2016", "Z2017", "Z2018")
farben <- c("gold", "lightskyblue", "darkolivegreen1", "coral")