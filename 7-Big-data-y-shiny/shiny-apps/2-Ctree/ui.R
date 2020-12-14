library(shiny)

# Definir la interfaz que vera el usuario 
shinyUI(fluidPage(
  
  # Titulo
  titlePanel("Ctrees"),
  
  sidebarLayout(
    
    # Menu de seleccion lateral
    sidebarPanel(
      textInput("tit","Titulo:")
    ),
    
    # Salida de resultados
    mainPanel(
      plotOutput("tree")
    )
  )
))