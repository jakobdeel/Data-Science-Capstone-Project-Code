#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#


library(shiny)
library(plotly)
library(tidyverse)

#reads in ngrams list
ngramslist <- readRDS("ngramslist.RDS")
source("txtpredict.R", local=TRUE)

# Define UI for application
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Jakob Deel - Text Prediction Algorithm"),
  
  # Sidebar with a text input for input text string 
  sidebarLayout(
    sidebarPanel(
      h4("In the text box below, please enter a text string."),
      textInput("inputstring",
                  label = NULL,
                  value = "Would you like to",
                  placeholder = "Click here and enter a text string!"),
      submitButton("Predict Next Word"),
      h5("Thank you for your patience as the prediction loads.")
    ),
    
    # Shows predicted next words as list and plot
    mainPanel(
      h4("Next word predictions:"),
      plotlyOutput("plot")
    )
  )
))
