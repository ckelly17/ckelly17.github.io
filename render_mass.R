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

file.copy("app/mass.html", "output/mass.html")


setwd("/Users/conorkelly/Documents/Heroku Apps/covid_Rmd/output")


system("git remote add tiger https://github.com/ckelly17/ckelly17.github.io.git")
system("git status")
system("git add 'app/mass.html' ")
system("git commit -m 'updating wastewater tracker' ")
system("git push")




