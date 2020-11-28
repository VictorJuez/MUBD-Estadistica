library(shiny)

########################################################################################
##-- Read data
########################################################################################
# setwd('...')
# train <- read.csv2(...)
# train$countLog <- ...


shinyServer(function(input, output) {
  
    
    ##----------------------------------- Ajuste del modelo
    output$resumen <- renderPrint({
      # input$go

      var.pred <- character(0)

      if(input$year) var.pred <- c(var.pred,"year")
      if(input$season) var.pred <- c(var.pred,"season")
      if(input$holiday) var.pred <- c(var.pred,"holiday")
      if(input$workingday) var.pred <- c(var.pred,"workingday")
      if(input$weather) var.pred <- c(var.pred,"weather")

      # if(input$hour) var.pred <- c(var.pred,paste0("poly(hour,",input$phour,")"))
      # ...
      # ...
      # ...
      # ...

      # res <- ifelse(...,...,...)
      # form <- as.formula(paste(res,'~',paste(var.pred,collapse = "+")))
      # mod.lm <- lm(form, train)
      # mod.lm <<- mod.lm
      # summary(mod.lm)
      
      paste("Aqui ira el modelo")
      
    }) 
    
    ##----------------------------------- Grafico de validacion
    output$val <- renderPlot({
      # input$go
      # par(mfrow=c(...,...))
      # plot(...)
    }) 

})

