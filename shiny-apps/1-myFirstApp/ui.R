library(shiny)

# Definir la interfaz que vera el usuario 
shinyUI(fluidPage(
  
  # Titulo
  titlePanel("Histograma aleatorio"),
  
  sidebarLayout(
    
    # Menu de seleccion lateral
    sidebarPanel(
      sliderInput("obs","Numero de observaciones:",min = 1,max = 100,value = 50),
      textInput("inputId", "MyInput"),
      selectInput("selectInputId", "Select Input", c('blue', 'green', 'red'))
    ),
    
    # Salida de resultados
    mainPanel(
      plotOutput("distPlot")
    )
  )
))