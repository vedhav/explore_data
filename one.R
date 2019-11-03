library(shiny)
library(shinyWidgets)
library(shinyjs)
library(shinyalert)
library(bs4Dash)
library(DT)
library(plotly)
library(psych)
library(tidyverse)
library(corrplot)

analysisData <<- read.csv("mtcars.csv")

numericAnalysisData <- analysisData %>% select_if(is.numeric)
corrplot(
    cor(numericAnalysisData), method = "circle", type = "circle",
    type = "full", order = "original"
)



