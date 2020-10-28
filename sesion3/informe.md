MUBD - Estadistica - Sesion 3: Modelo Lineal
================

# 1\. Lectura de datos y descriptiva

## Lectura e inspección de los datos

``` r
datos <- read.table('Concrete_train.txt',sep="\t",header=TRUE)
dim(datos)                  # num filas y columnas
```

    ## [1] 706   9

``` r
datos[1:5,]                 # ver datos
```

    ##   Cement BlastFurnaceSlag FlyAsh Water Superplasticizer CoarseAggregate
    ## 1  198.6            132.4      0   192                0           978.4
    ## 2  266.0            114.0      0   228                0           932.0
    ## 3  380.0             95.0      0   228                0           932.0
    ## 4  380.0             95.0      0   228                0           932.0
    ## 5  198.6            132.4      0   192                0           978.4
    ##   FineAggregate Age Strength
    ## 1         825.5 360    44.30
    ## 2         670.0  90    47.03
    ## 3         594.0 365    43.70
    ## 4         594.0  28    36.45
    ## 5         825.5  90    38.07

``` r
summary(datos)              # descriptiva de todas las variables 
```

    ##      Cement      BlastFurnaceSlag     FlyAsh           Water      
    ##  Min.   :102.0   Min.   :  0.00   Min.   :  0.00   Min.   :121.8  
    ##  1st Qu.:190.7   1st Qu.:  0.00   1st Qu.:  0.00   1st Qu.:167.9  
    ##  Median :269.3   Median : 22.00   Median :  0.00   Median :185.7  
    ##  Mean   :278.1   Mean   : 73.11   Mean   : 54.06   Mean   :182.5  
    ##  3rd Qu.:347.2   3rd Qu.:140.97   3rd Qu.:118.30   3rd Qu.:192.0  
    ##  Max.   :540.0   Max.   :359.40   Max.   :200.10   Max.   :247.0  
    ##  Superplasticizer CoarseAggregate  FineAggregate        Age        
    ##  Min.   : 0.000   Min.   : 801.0   Min.   :594.0   Min.   :  1.00  
    ##  1st Qu.: 0.000   1st Qu.: 932.0   1st Qu.:728.2   1st Qu.: 14.00  
    ##  Median : 6.400   Median : 968.0   Median :778.5   Median : 28.00  
    ##  Mean   : 6.092   Mean   : 972.8   Mean   :773.0   Mean   : 45.44  
    ##  3rd Qu.:10.000   3rd Qu.:1030.0   3rd Qu.:824.8   3rd Qu.: 56.00  
    ##  Max.   :32.200   Max.   :1145.0   Max.   :992.6   Max.   :365.00  
    ##     Strength    
    ##  Min.   : 2.33  
    ##  1st Qu.:23.43  
    ##  Median :33.71  
    ##  Mean   :35.21  
    ##  3rd Qu.:44.61  
    ##  Max.   :82.60

``` r
boxplot(datos, las=2)       # boxplot de todas las variables
```

![](informe_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

En este último gráfico vemos como se distribuyen las distintas
características. Podemos observar como algunas lo hacen de forma normal
(Cement, CoarseAggregate, FineAggregate). Y otras no (FlyAsh, Age,
Superplasticizer). Posteriormente veremos diferentes filtros para
intentar normalizarlas todas.

## Explorar todos los pares de datos

``` r
pairs(datos) # descriptiva bivariante
```

![](informe_files/figure-gfm/unnamed-chunk-3-1.png)<!-- --> Con este
gráfico, podemos ver como se relacionan todas las características entre
ellas una a una. De todas ellas nos interesa ver como se relaciona cada
una de las características con la variable resultado (Strenght). A
simple vista, la que parece tener la relación lineal más clara con
Strenght es el Cemento (cuando este incrementa, también lo hace la
Dureza)

## Descriptiva bivariante para la variable Cemento

``` r
plot(Strength~Cement,datos)                       # puntos
with(datos,lines(lowess(Strength~Cement),col=2))  # estimacion no parametrica de la relacion
```

![](informe_files/figure-gfm/unnamed-chunk-4-1.png)<!-- --> Con esta
estimación (linea roja), afirmamos pues, que la relación es lineal

## Descriptiva bivariante para todas las variables

``` r
par(mfrow=c(2,4))
for(i in 1:8){
  plot(datos$Strength~datos[,i],main=names(datos)[i],xlab=names(datos)[i],ylab="Strength")
  with(datos,lines(lowess(Strength~datos[,i]),col=2))
}
```

![](informe_files/figure-gfm/unnamed-chunk-5-1.png)<!-- --> Ahora, con
todas las otras características, vemos que algunas tinenen curvatura
(Age, Water, BlastFurnaceSlag), por ende no presentan una relación
lineal. Y otras sí, como el superplasticizer o el CoarseAggregate

# 2\. Ajuste del Modelo

## Ajuste del Modelo lineal simple

``` r
mod.lm0 <- lm(Strength~Cement,datos)
summary(mod.lm0)
```

    ## 
    ## Call:
    ## lm(formula = Strength ~ Cement, data = datos)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -37.618 -10.903  -0.733   9.856  41.291 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 12.620040   1.560795   8.086  2.7e-15 ***
    ## Cement       0.081215   0.005271  15.407  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 14.22 on 704 degrees of freedom
    ## Multiple R-squared:  0.2521, Adjusted R-squared:  0.2511 
    ## F-statistic: 237.4 on 1 and 704 DF,  p-value: < 2.2e-16

  - **Residuals**: descriptiva de los residuos (errores).
  - **Coeficientes**:
      - *Estimate*: coeficientes del termino independiente (intercept) y
        el cemento. Por cada unidad de cemento que yo agrego, la dureza
        augmenta en 0.08 uds.
      - *Std. Error*: Error estándar de la estimación, en el caso del
        Cemento, estimamos un valor de 0,08 pero con un error de +-
        0.005
      - *t value*: Es la relación que hay entre la Estimación y el Std.
        Error (Estimación/Error). Nos interesa que sea lo mayor posible,
        lo que signfica que hay un error pequeno
      - *Pr(\>|t|)*: P-valor de los coeficientes. Utiliza el t-valor
        para hacerlo. Un valor muy pequeno de este (\< 0.05) implica que
        descartamos la hipótesis nula, es decir, que el coeficiente sea
        0. Dicho de otra forma, en el caso del cemento, implica que éste
        está influyendo sobre la variable respuesta (Dureza). Nos
        aseguramos que no es un coeficiente igual a 0, lo que por
        contra, significaría que el cemento no tiene ningún impacto
        sobre la Dureza (por cada unidad de cemento que agrego, la
        dureza varia +- 0) y por tanto no deberíamos utilizarla como
        característica del modelo.
  - **Signif. codes**: el R nos ayuda y directamente nos califica cada
    coeficiente según si son más o menos significativos: `0 ‘***’ 0.001
    ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1`.
      - p-valor entre (0 y 0.001): `***`
      - p-valor entre (0.001 y 0.01): `**`,
      - p-valor entre (0.01 y 0.05): `*`,
      - p-valor entre (0.05 y 0.1): `.`,
      - p-valor entre (0.1 y 1): `  `
  - **Residual standard error**: Como la media de los residuos, lo que
    espero equivocarme utilizando el modelo
  - **Multiple R-squared**: \(R^2\) Es el porcentaje de variabilidad que
    explica el modelo. Cerca de 0 no explica nada, cerca del 1 explica
    mucho. En este caso, considerando que tenemos una única variable
    (Cemento), un valor de 0.25 está bastante bien, podemos explicar un
    25% de la dureza utilizando el cemento.
  - **F-static**: si el modelo en general explica algo o no

<!-- end list -->

``` r
par(mfrow=c(1,1))
plot(Strength~Cement,datos)
abline(mod.lm0,col="red")
```

![](informe_files/figure-gfm/unnamed-chunk-7-1.png)<!-- --> La línea
roja es el modelo lineal

## Ajuste del modelo multivariado

``` r
mod.lm1 <- lm(Strength~Cement + BlastFurnaceSlag + FlyAsh +      
                       Water + Superplasticizer + CoarseAggregate +
                       FineAggregate + Age,datos)                   # Ajuste del modelo
# mod.lm1 <- lm(Strength~.,datos)                                     # Instruccion equivalente a la anterior
mod.lm1                                                             # Ver coeficientes
```

    ## 
    ## Call:
    ## lm(formula = Strength ~ Cement + BlastFurnaceSlag + FlyAsh + 
    ##     Water + Superplasticizer + CoarseAggregate + FineAggregate + 
    ##     Age, data = datos)
    ## 
    ## Coefficients:
    ##      (Intercept)            Cement  BlastFurnaceSlag            FlyAsh  
    ##        -32.90148           0.12537           0.10706           0.09151  
    ##            Water  Superplasticizer   CoarseAggregate     FineAggregate  
    ##         -0.13137           0.34550           0.02139           0.02127  
    ##              Age  
    ##          0.11199

``` r
summary(mod.lm1)                                                    # Resumen del modelo
```

    ## 
    ## Call:
    ## lm(formula = Strength ~ Cement + BlastFurnaceSlag + FlyAsh + 
    ##     Water + Superplasticizer + CoarseAggregate + FineAggregate + 
    ##     Age, data = datos)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -26.657  -6.159   0.600   6.645  31.919 
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)      -32.901477  33.369889  -0.986  0.32449    
    ## Cement             0.125369   0.010608  11.818  < 2e-16 ***
    ## BlastFurnaceSlag   0.107063   0.012488   8.573  < 2e-16 ***
    ## FlyAsh             0.091511   0.015440   5.927 4.85e-09 ***
    ## Water             -0.131372   0.050861  -2.583  0.01000 ** 
    ## Superplasticizer   0.345505   0.115764   2.985  0.00294 ** 
    ## CoarseAggregate    0.021390   0.011625   1.840  0.06620 .  
    ## FineAggregate      0.021270   0.013206   1.611  0.10772    
    ## Age                0.111992   0.006377  17.562  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 10.11 on 697 degrees of freedom
    ## Multiple R-squared:  0.6253, Adjusted R-squared:  0.621 
    ## F-statistic: 145.4 on 8 and 697 DF,  p-value: < 2.2e-16

  - Análisis rápido de la estimación: por cada unidad de cemento que
    agreguemos, la dureza augmentará 0.125. En cambio, por cada unidad
    de agua que agreguemos, la dureza se reducirá en 0.13
  - A simple vista ya vemos las variables más significativas, con tres
    estrellas (Cemento, FlyAsh, Age..) y las menos (FineAggregate,
    CoarseAggregate).
  - Importante destacar el R-squared: ha augmentado considerablemente
    (0.6253) en comparación al 0.25 que teníamos en el modelo anterior
    con solo el Cemento como variable. Ésto nos indica que ahora con
    éste modelo que contiene todas las variables podemos explicar un
    63% la dureza

## Selección automática de variables

``` r
mod.lm2 <- step(mod.lm1)                   # Seleccionar variables
```

    ## Start:  AIC=3276.1
    ## Strength ~ Cement + BlastFurnaceSlag + FlyAsh + Water + Superplasticizer + 
    ##     CoarseAggregate + FineAggregate + Age
    ## 
    ##                    Df Sum of Sq    RSS    AIC
    ## <none>                           71288 3276.1
    ## - FineAggregate     1     265.3  71553 3276.7
    ## - CoarseAggregate   1     346.2  71634 3277.5
    ## - Water             1     682.4  71970 3280.8
    ## - Superplasticizer  1     911.1  72199 3283.1
    ## - FlyAsh            1    3592.9  74881 3308.8
    ## - BlastFurnaceSlag  1    7517.1  78805 3344.9
    ## - Cement            1   14285.8  85574 3403.0
    ## - Age               1   31544.5 102833 3532.8

  - AIC: Se mira cuan bueno es el modelo y por otra parte el numero de
    parámetros que tiene el modelo. Nos interesa que el modelo sea lo
    más bueno posible (verosimilitud) con el mínimo de variables
    posibles. Cuanto más pequeno es el AIC mejor.
  - En la tabla anterior observamos el AIC resultante por cada variable
    que quitamos al modelo. En este caso, si NO quitamos ninguna,
    tenemos el valor más pequeno del AIC, que es lo que buscamos.

<!-- end list -->

``` r
summary(mod.lm2)                           # Modelo con variables seleccionadas
```

    ## 
    ## Call:
    ## lm(formula = Strength ~ Cement + BlastFurnaceSlag + FlyAsh + 
    ##     Water + Superplasticizer + CoarseAggregate + FineAggregate + 
    ##     Age, data = datos)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -26.657  -6.159   0.600   6.645  31.919 
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)      -32.901477  33.369889  -0.986  0.32449    
    ## Cement             0.125369   0.010608  11.818  < 2e-16 ***
    ## BlastFurnaceSlag   0.107063   0.012488   8.573  < 2e-16 ***
    ## FlyAsh             0.091511   0.015440   5.927 4.85e-09 ***
    ## Water             -0.131372   0.050861  -2.583  0.01000 ** 
    ## Superplasticizer   0.345505   0.115764   2.985  0.00294 ** 
    ## CoarseAggregate    0.021390   0.011625   1.840  0.06620 .  
    ## FineAggregate      0.021270   0.013206   1.611  0.10772    
    ## Age                0.111992   0.006377  17.562  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 10.11 on 697 degrees of freedom
    ## Multiple R-squared:  0.6253, Adjusted R-squared:  0.621 
    ## F-statistic: 145.4 on 8 and 697 DF,  p-value: < 2.2e-16

Al no quitar ninguna variable, tenemos el mismo modelo que el anterior
(lm1)

## Validación de las premisas

Premisas: - **Linealidad**: Una recta/plano/hiperplano se ajusta bien a
los datos - **Homoscedasticidad**: Variabilidad constante - **Normalidad
de los residuos**: Los errores son normales - **Independencia**: La
muestra es aleatoria simple y el resultado de una observación no
condiciona el resto

``` r
par(mfrow=c(2,2))                          # ventana para 4 gr?ficos
plot(mod.lm2)                              # graficos para valorar premisas
```

![](informe_files/figure-gfm/unnamed-chunk-11-1.png)<!-- --> -
**Residuals vs Fitted**: Nos indica la Homoscedasticidad, para
cumplirla, los residuos, deberían distribuirse de la misma forma por
todo el rango de los Fitted values. Así pues, vemos que no es el caso,
ya que cuando éstos son pequenos, los residuos oscilan menos (muy
cercanos a la linea roja). Y en cambio cuando mayor son los Fitted
values, más dispersos son los residuos
