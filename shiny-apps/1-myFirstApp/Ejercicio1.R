##-- 1. Instala y carga el paquete shiny


##-- 2. Abre los ficheros ui.R y server.R de la carpeta myFirstApp

##-- 3. Ejecuta la aplicacion en local con uno de los siguientes sistemas: 
##-- a) clicando en el boton "Run App" que aparece en la parte superior derecha de cualquiera de los 2 scripts
##-- b) define el directorio donde esta la app con setwd('...') y ejecuta la instrucción runApp()

##-- 4. Cambiale el titulo a la aplicacion en el ui: "Histograma aleatorio" en vez de "Mi primera aplicacion"

##-- 5. Vuelve a ejecutar la aplicacion despues de haber cerrado la ventana del navegador previamente

##-- 6. Anyade en el ui un nuevo widget de tipo texto para poner un titulo al histograma
##-- Ponlo despues del sliderInput y separa los distintos widgets por una coma
# Pista: textInput


##-- 7. Si el input que has creado se llama "miinput", para acceder desde el server, lo llamaremos
##-- con input$miinput. Anyade el titulo al histograma en el server
# Pista: parametro main de la funcion hist


##-- 8. Vuelve a ejecutar la app

##-- 9. Crea un selector para escoger el color del histograma en el ui. Que pueda escoger entre "blue","green" o "red"
##-- Recuerde poner comas entre widgets
# Pista: selectInput. En el parametro choices, pon las opciones que quieras


##-- 10. Pon el color en el codigo del histograma del server
# Pista: Parametro "col" de la funcion hist


##-- 11. Ejecuta la aplicación por ultima vez 