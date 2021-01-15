library(shiny)

# Definir en el server como realizar el histograma
shinyServer(function(input, output) {
  print(input)
  output$distPlot <- renderPlot({
    set.seed(12345)
    x <- rnorm(input$obs)
    hist(x, col = input$selectInputId , border = 'white', main = input$inputId)
  })
})