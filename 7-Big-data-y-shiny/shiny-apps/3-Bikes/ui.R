rm(list=ls())
library(shiny)

shinyUI(fluidPage(
  titlePanel("..."),
  
  tabsetPanel(
    tabPanel(strong("..."),

             fluidRow(column(3,selectInput('y','Respuesta',choices=c('Normal','Logaritmo'),selected = 'Normal'))),
             
             fluidRow(column(2,checkboxInput("year","year",value = TRUE)),
                      column(2,checkboxInput("season","season",value = TRUE)),
                      column(2,checkboxInput("holiday","holiday",value = TRUE)),
                      column(2,checkboxInput("workingday","workingday",value = TRUE)),
                      column(2,checkboxInput("weather","weather",value = TRUE))),
             
             # fluidRow(column(2,checkboxInput("hour","hour",value = TRUE)),
             #          ...
             #          ...
             #          ...
             #          ...
             
             # fluidRow(column(2,numericInput("phour","Grado hour",value = 1,min=1,max=3)),
             #          ...
             #          ...
             #          ...
             #          ...
             
             # fluidRow(column(2,actionButton("go", "Run model"))),br(),
             
             
             
             fluidRow(column(6,verbatimTextOutput("resumen")))
             
    ),
    
    tabPanel(strong("...")
             # ,plotOutput(...)
    )
  )
))



