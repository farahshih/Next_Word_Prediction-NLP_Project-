library(shiny)
library(tm)
library(quanteda)
library(stringr)

source("func_and_data.R")

shinyServer(
  function(input, output){
    output$outputWord<-renderPrint({ngram_model(input$InputS,4)})
  })