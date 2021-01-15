##-- 0. Ejecuta la app como esta con el boton "Run App"

##-- 1. Anyade un titulo a la app y a las pestanyas en el ui.R. Ejecuta
# Pista: titlePanel("..."),strong("...") y strong("...")

##-- 2. Lee los datos de entrenamiento en el server.R y crea la variable countLog. Ejecuta
# Pista: setwd, read.csv2 (o read.table) y instrucción log

##-- 3. Igual que para las variables categoricas, create un checkbox para cada variable numérica en el ui.R. Ejecuta
# Pista: Descomenta el primer parrafo  comentado en el ui [el que empieza por # fluidRow(column(2,checkboxInput("hour","hour",value = TRUE))] 
# y anyade todas las numericas aparte de la hora

##-- 4. Crea un input para cada variable numerica en el ui.R que indique el grado del polinomio a ajustar. Ejecuta
# Pista: Descomenta el segundo parrafo  comentado en el ui [el que empieza por # fluidRow(column(2,numericInput("phour","Grado hour",value = 1,min=1,max=3))] 
# y anyade todas las numericas aparte de la hora


##-- 5. Igual que para las variables categoricas, anyade en el server.R al vector var.pred las variables numericas con el polinomio correspondiente. Ejecuta
# Pista: Descomenta el primer parrafo  comentado en el server [el que empieza por # if(isolate(input$hour)) var.pred <- c(var.pred,paste0("poly(hour,",isolate(input$phour),")"))] 
# y anyade todas las numericas aparte de la hora

##-- 6. Crea el objeto res que valga "count" si el input$y es distinto de "Logaritmo" y "countLog" si es igual a logaritmo en el server.R. Ejecuta
# Pista: Descomenta la linea que empieza por # res <- ifelse(...,...,...) y completala en los puntos suspensivos

##-- 7. Descomenta las lineas que hay por debajo de la linea anterior en el server y elimina la ultima linea "paste("Aqui ira el modelo")". Ejecuta 

##-- 8. Anyade un boton en el ui.R para que solo se ejecute el modelo cuando lo apretemos. Ejecuta
# Pista: Descomenta la linea # fluidRow(column(2,actionButton("go", "Run model"))),br(),

##-- 9. Haz que solo se ejecute el modelo al apretar el boton en el server.R Ejecuta
# Pista: Descomenta la linea # input$go y pon todos los inputs$loquesea excepto el input$go dentro de un isolate()

##-- 10. Crea en el server.R los graficos de validacion. Ejecuta
# Pista: Descomenta todas las lineas dentro del renderplot y sustituye los puntos suspensivos

##-- 11. Haz que se vea el grafico de validacion en el ui.R. Ejecuta
# Pista: Descomenta la linea # ,plotOutput(...) y sustituye los puntos suspensivos

##-- 12. Ya esta la app hecha. Ya eres un Shiny Master. Haz pruebas con distintos modelos 
