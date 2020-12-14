##-- 1. Instala (si no lo has hecho ya) y carga el paquete shiny

##-- 2. Abre los ficheros ui.R y server.R de la carpeta Ctrees

##-- 3. En el server indica el directorio y el nombre del fichero de entrenamiento y test de los datos products.csv 

##-- 4. Ejecuta la aplicacion en local con uno de los siguientes sistemas: 
##-- a) clicando en el boton "Run App" que aparece en la parte superior derecha de cualquiera de los 2 scripts
##-- b) define el directorio donde esta la app con setwd('...') y ejecuta la instrucción runApp()

##-- 5. Anyade en el ui un nuevo widget de tipo númerico para decidir la profundidad 
##-- del arbol. Llámalo "depth". Haz que vaya de 1 a 5 y que por defecto valga 1 


##-- 6. El input que has creado se llama "depth". Cambia la profundidad del árbol
##-- que hay en el server por input$depth

##-- 7. Vuelve a ejecutar la app

##-- 8. Introduce un widget con identificador "data" de tipo choice pare decidir que juego de datos se utiliza
##-- Ponle como opciones "train" o "text"

##-- 9. La instruccion get transforma un character en un objeto con ese nombre
##-- Usa esta instruccion para obetener los datos en el server y modifica lo que creas
##-- conveniente en la intruccion ctree


##-- 10. Vuelve a ejecutar la app

##-- 11. Crea un boton en el ui con identificador "go" para enviar los inputs cuando se apriete

##-- 12. Haz dos cosas en el server
##-- (1) En la primera linea despues de renderPlot pon input$go (Lo que venga a continuacion se ejecutará cada vez que se apriete)
##-- (2) Alguno de los input$xxx que tienes en el server ponlo dentro de la función isolate(). P.ej. isolate(input$tit) o isolate(input$data)
##-- La instruccion isolate hará que ese input solo se envie cuando apretemos el boton

##-- 13. Vuelve a ejecutar la app. ¿Funciona?




