library(pdftools)
library(tidyverse)
library(rvest)
library(RCurl)
library(XML)
library(lubridate)
library(scales)
library(plotly)
library(rmarkdown)
#library(rsconnect)

graphics.off()

# render
rmarkdown::render("app/mass.Rmd",
                  envir = globalenv()) # make sure global env is referenced
