
library(shiny)


shinyUI(fluidPage(
  titlePanel("N-gram Prediction Model"),
  fluidRow(
    column(12, h5("Instructions: please enter a sentence that you are interested in.
                  And select the previous N words to predict the next word.")),
    column(6,
           wellPanel(textInput("InputS",label="Enter a sentence:",
                               value="Can you follow me please? It would mean the"))),
    column(6,
           wellPanel(numericInput("ChooseN", label="Choose last N words to predict:",
                                  4, min=1, max=4)))
    ),
  mainPanel(h3('Next Word Prediction'),
            h5("Note1: Wait for a few seconds for the app to load for the first time."),
            h5("Note2: Higher frequency means higher possiblity."),
            p("- If there is no predicted answer, try different n."),
            verbatimTextOutput("outputWord"))
))

# shinyUI(fluidPage(
#   titlePanel("N-gram Prediction Model"),
#   fluidRow(
#     column(12, h5("Instructions: please enter a sentence that you are interested 
#                   in (At leaset four words). The app will use last four words to
#                   predict the next word")),
#     column(6, 
#            wellPanel(textInput("InputS",label="Enter a sentence:", 
#                      value="Can you follow me please? It would mean the")))
#   ),
#   mainPanel(h3('Next Word Prediction'),
#             h5("Note1: Wait for a few seconds for the app to load for the first time."),
#             h5("Note2: Higher frequency means higher possiblity."),
#             verbatimTextOutput("outputWord"))
# ))
# 
