
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel("Quantification on Reuters-21578 Data"),

  # Sidebar with a slider input for number of bins
  #sidebarLayout(
    
    sidebarPanel(
      
      # Choose category
      selectInput("category", "Choose a category:",
                  choices = c("acq", "corn", "crude", "earn", "grain",
                              "interest", "money-fx", "ship", "trade", "wheat"),
                  width = "100%"),
      
      sliderInput("kFolds",
                  "number of folds:",
                  min = 2,
                  max = 10,
                  value = 5,
                  step = 1,
                  width = "100%"),

      fluidRow(column(width = 6, actionButton("switch", label = "Switch")), 
               column(width = 6, actionButton("resample", label = "Resample"))),
      
      #actionButton("switch", label = "Switch"),
      
      sliderInput("features",
                  "Select Features:",
                  min = 100,
                  max = 35000,
                  value = 35000,
                  step = 100,
                  width = "100%"),

#       sliderInput("alpha",
#                   "Alpha (prior):",
#                   min = 1e-05,
#                   max = 2,
#                   value = 0.5,
#                   step = 1e-05,
#                   width = "100%"),
# 
#       sliderInput("beta",
#                   "Beta (prior):",
#                   min = 0.5,
#                   max = 300,
#                   value = 0.5,
#                   step = 0.5,
#                   width = "100%"),
      
      sliderInput("m",
                  "Angular coefficient (m):",
                  min = 0.5,
                  max = 2,
                  value = 1.0,
                  step = 0.05,
                  width = "100%"),

      sliderInput("q",
                  "Intercept (q):",
                  min = -100,
                  max = 300,
                  value = 0,
                  step = 10,
                  width = "100%"),
      
      actionButton("reset", label = "Reset")
      
#       actionButton("bestTraining", label = "Best Train"),
#       
#       actionButton("bestValidation", label = "Best Valid")
      
    ),

    # Show a plot of the generated distribution
    mainPanel(
      
      fluidRow(column(width = 6, verbatimTextOutput("validObjects")),
               column(width = 6, verbatimTextOutput("testObjects"))
               ),
      fluidRow(column(width = 6, tableOutput("validMeasures")),
               column(width = 6, tableOutput("testMeasures"))
               ),
      fluidRow(column(width = 6, plotOutput("plotValid")),
               column(width = 6, plotOutput("plotExplored"))
               )
      
#       fluidRow(column(width = 6, verbatimTextOutput("trainObjects")),
#                #column(width = 6, verbatimTextOutput("validObjects")),
#                column(width = 6, verbatimTextOutput("validObjects"))
#                ),
#       fluidRow(column(width = 6, tableOutput("trainMeasures")), 
#                #column(width = 6, tableOutput("validMeasures")), 
#                column(width = 6, tableOutput("validMeasures"))),
#       fluidRow(column(width = 6, plotOutput("plotTrain")), 
#                #column(width = 6, plotOutput("plotValid")),
#                column(width = 6, plotOutput("plotValid")))

    )
  #)
))
