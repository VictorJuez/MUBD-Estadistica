library(shiny)
library(party)

train <- read.table('products_train.csv', header=TRUE, sep=';', nrows=2000, stringsAsFactors = TRUE)                         # Especifica el nombre del fichero
test <- read.table('products_test.csv', header=TRUE, sep=';', nrows=2000, stringsAsFactors = TRUE)


# Definir en el server como realizar el histograma
shinyServer(function(input, output) {
  output$tree <- renderPlot({
    ct <- ctree(target~.,train,controls = ctree_control(maxdepth=3))
    plot(ct,main=input$tit)
  })
})