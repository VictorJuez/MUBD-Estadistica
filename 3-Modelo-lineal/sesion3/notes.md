- Queremos estimar las Betas y tambien la Sigma (el error). Buscamos la sigma más pequena posible. Esto indicará que nuesto modelo es lo mas acurado posible.
- Tantos coeficientes como categorias menos 1.

## Formulacion modelo
- Por cada unidad de cemento que yo anada la dureza va a incrementar en 0.11391
- Por cada unidad de agua que yo anada la dureza va a decrementar 0.169
- Intercept: Cual seria el valor de la variable respuesta si todas las otras las ponemos a 0
  - Puede no tener mucho sentido y no ser interpretable

## Estimacion de los parametros
- Cuando hagamos la estimacion obtenedremos algun error. La diferencia de los valores reales a los estimados es el residuo (e)
- Escogeremos la recta que minimice al maximo la suma de los errores al cuadrado

## Interpretacion de los coeficientes
- B0: valor de la variable respuesta cuando el resto de las variables estan a 0

## Seleccion de variables
seleccion de las variables basada en el AIC o BIC
- forward: Cuando sea forward, parten de un modelo sin variables y van anadiendo aquellas que hacen que mejore el AIC o el BIC -> raramente utilizado
- backward: Lo mismo pero al reves -> el mas utilizado
- forward-backward: o anadir o quitar, depende

## Seleccion manual
- Es el mejor metodo (en comparacion al automático). Si tenemos mucho timepo mejor hacer seleccion manual. Pero si tenemos muchas variables escoger el automatico

## Metodos automaticos
- Se mira cuan bueno es el modelo y por otra parte el numero de parámetros que tiene el modelo. Nos interesa que el modelo sea lo más bueno posible (verosimilitud) con el mínimo de variables posibles.
 - L: verosimilitud del modelo. Cuanto más alta mejor
 - K: numero de variables
 - Buscamos el valor AIC más pequeno posible
 - El BIC va a penalizar maas el tener mas variables

 - -2log(L) es la devianza

 ## Metodologia manual
- Mirar todas las variables, calcular todos los p valores y elminar las que lo tienen mayor
- Una variable es confusora cuando la introducimos en un modelo hace variar el coeficiente de las otras. Estas variables las necesitamos.
- (VIF) Nos interesa tener variables predictoras que no estan muy relacionadas entre si
  - VIF > 5 o 8 serian demasiado grandes

## Validación
- homesticidad: la variabilidad de los residuos a lo largo de la recta no es constante
- independencia: Cuando el tiempo esté involucrado desconfiar del modelo.


## Premisas - Linealidad
- Buscamos que los residuos se distribuyen por igual por encima y por debajo de los valores predichos y de forma constante 

## Observaciones influyentes

## R2
- R cercano a 0 el modelo muy malo, al 1 es muy bueno. El porcentaje de variablilidad que explica el modelo

## Division de la muestra
- Muestra entrenamiento, muestra test
  - Se dedica un 70$ al entrenamiento, y 30 al test
