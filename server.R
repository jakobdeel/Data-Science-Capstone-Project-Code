#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)
library(tidyverse)


# Data loading and processing
#reads in ngrams list
ngramslist <- readRDS("ngramslist.RDS")
source("txtpredict.R", local=TRUE)

# Define server logic required to output word prediction list and plot
shinyServer(function(input, output) {

  #runs txtpredict function and outputs bar chart to show likelihood of each next word
  output$plot <- renderPlotly({
     prediction <- txtpredict(input$inputstring)
     prediction <- prediction[1:10,]
     prediction$next_word <- factor(prediction$next_word,
                                    levels = unique(prediction$next_word[order(prediction$prop, decreasing = FALSE)]))
     plot <- plot_ly(data = prediction, y = prediction$next_word, x = prediction$prop*100, 
                     name = "Prediction Visualization", type = "bar")
     plot <- layout(plot, 
                    xaxis = list(title = "Likelihood of Prediction (in percent)"),
                    showlegend = FALSE)
     plot
})
  
})

