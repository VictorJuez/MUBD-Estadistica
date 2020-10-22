MUBD - Estadistica - Sesion 1
================

## Ejercicio 1.1

1.  Crear un data.frame con los siguientes datos

| Marca     | Modelo | Consumo |
| --------- | ------ | ------- |
| audi      | A3     | 6.8     |
| volswagen | Polo   | 6.8     |
| bmw       | X5     | 12.5    |
| renault   | megane | 4.7     |
| fiat      | punto  | 5.0     |

2.  Modificar el consumo del fiat punto a 4.5

<!-- end list -->

``` r
#1
marcas <- c("audi","volkswagen","bmw", "renault", "fiat")
modelos <- c("A3", "Polo", "X5", "megane", "punto")
consumos <- c(6.8, 6.8, 12.5, 4.7, 5.0)
df1 <- data.frame(marcas, modelos, consumos)

#2
df1$consumos[5] <- 4.5
df1
```

``` bg-info
##       marcas modelos consumos
## 1       audi      A3      6.8
## 2 volkswagen    Polo      6.8
## 3        bmw      X5     12.5
## 4    renault  megane      4.7
## 5       fiat   punto      4.5
```

## Ejercicio 1.2

1.  Leer el fichero flights.txt cambiando los parametros de lectura
    oportunos y guardar en datos2 (OJO con el simbolo de los missings)
2.  Inspeccionar los datos: num. filas y columnas, visualizar los datos
    y descriptiva global
3.  ¿Cuanto tiempo estuvo en el aire el vuelo 251 y con numero de cola
    N855UA?

<!-- end list -->

``` r
#1
datos2 = read.table('flights.txt',header=TRUE,sep='\t',dec = '.', na.strings = "@", stringsAsFactors = TRUE)
```

``` r
#2
dim(datos2)
```

``` bg-info
## [1] 336776     16
```

``` r
# View(datos2) # No se ejecuta porque el resultado es  demasiado grande para imprimirlo en el informe
summary(datos2)
```

``` bg-info
##       year          month             day           dep_time   
##  Min.   :2013   Min.   : 1.000   Min.   : 1.00   Min.   :   1  
##  1st Qu.:2013   1st Qu.: 4.000   1st Qu.: 8.00   1st Qu.: 907  
##  Median :2013   Median : 7.000   Median :16.00   Median :1401  
##  Mean   :2013   Mean   : 6.549   Mean   :15.71   Mean   :1349  
##  3rd Qu.:2013   3rd Qu.:10.000   3rd Qu.:23.00   3rd Qu.:1744  
##  Max.   :2013   Max.   :12.000   Max.   :31.00   Max.   :2400  
##                                                  NA's   :8255  
##    dep_delay          arr_time      arr_delay           carrier     
##  Min.   : -43.00   Min.   :   1   Min.   : -86.000   UA     :58665  
##  1st Qu.:  -5.00   1st Qu.:1104   1st Qu.: -17.000   B6     :54635  
##  Median :  -2.00   Median :1535   Median :  -5.000   EV     :54173  
##  Mean   :  12.64   Mean   :1502   Mean   :   6.895   DL     :48110  
##  3rd Qu.:  11.00   3rd Qu.:1940   3rd Qu.:  14.000   AA     :32729  
##  Max.   :1301.00   Max.   :2400   Max.   :1272.000   MQ     :26397  
##  NA's   :8255      NA's   :8713   NA's   :9430       (Other):62067  
##     tailnum           flight     origin            dest           air_time    
##         :  2512   Min.   :   1   EWR:120835   ORD    : 17283   Min.   : 20.0  
##  N725MQ :   575   1st Qu.: 553   JFK:111279   ATL    : 17215   1st Qu.: 82.0  
##  N722MQ :   513   Median :1496   LGA:104662   LAX    : 16174   Median :129.0  
##  N723MQ :   507   Mean   :1972                BOS    : 15508   Mean   :150.7  
##  N711MQ :   486   3rd Qu.:3465                MCO    : 14082   3rd Qu.:192.0  
##  N713MQ :   483   Max.   :8500                CLT    : 14064   Max.   :695.0  
##  (Other):331700                               (Other):242450   NA's   :9430   
##     distance         hour           minute     
##  Min.   :  17   Min.   : 0.00   Min.   : 0.00  
##  1st Qu.: 502   1st Qu.: 9.00   1st Qu.:16.00  
##  Median : 872   Median :14.00   Median :31.00  
##  Mean   :1040   Mean   :13.17   Mean   :31.76  
##  3rd Qu.:1389   3rd Qu.:17.00   3rd Qu.:49.00  
##  Max.   :4983   Max.   :24.00   Max.   :59.00  
##                 NA's   :8255    NA's   :8255
```

``` r
#3
sel.flight = which(datos2$flight==251 & datos2$tailnum=="N855UA")
datos2[sel.flight, "air_time"]
```

``` bg-info
## [1] 246
```

## Ejercicio 1.3

1.  Copiar los vuelos que estuvieron volando menos de 1 hora en un
    fichero llamado short\_flights\_test.txt \[El fichero sin usar
    comillas para las categorias,sin nombre de columnas ni de filas y
    columnas separadas por tabuladores\]

<!-- end list -->

``` r
datos <- read.table('Mobiles.txt',header=TRUE,sep=";",dec=".",na.strings=c("NA",""), stringsAsFactors = TRUE) 
write.table(subset(datos,air_time < 60), "Flights_test.txt", quote=FALSE, row.names = FALSE, sep = "\t")
```

## Ejercicio 1.4

1.  Realiza la descriptiva de la variable tiempo de vuelo (air\_time)
    del conjunto de datos flights.txt
2.  Realiza la descriptiva de la variable categorica origen (origin) del
    conjunto de datos flights.txt

<!-- end list -->

``` r
#1
summary(datos2$air_time)
sd(datos2$air_time,na.rm=TRUE)
```

``` r
hist(datos2$air_time, col="blue")
```

![](informe_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
boxplot(datos2$air_time, main="Air time (minutes)")
```

![](informe_files/figure-gfm/unnamed-chunk-3-2.png)<!-- -->

``` r
#2
originTable = table(datos2$origin)      # Tabla de frecuencias
prop.table(originTable)                 # Tabla proporciones
round(100*prop.table(originTable),1)    # Tabla de proporciones redondeados
```

## Ejercicio 1.5

1.  Realiza la descriptiva del retraso en la llegada segun el retraso en
    la salida.
2.  Calcula la correlacion y haz el diagrama bivariante. Tambien usa la
    instruccion smoothScatter para hacer un plot alternativo

<!-- end list -->

``` r
#1
#- Descriptiva global de cada variable (previo al ejercicio, no necesario)
View(datos2)
summary(datos2$arr_delay)
summary(datos2$dep_delay)
```

``` r
#- Correlacion entre las variables
cor(datos2$arr_delay, datos2$dep_delay, use="complete.obs")
```

``` bg-info
## [1] 0.9148028
```

0.91 -\> observamos que hay una relacion directa (\>1) y con alta
intensidad (muy cerca del 1)

``` r
#- Diagrama bivariante 
smoothScatter(datos2$arr_delay, datos2$dep_delay, xlab="Retraso llegada", ylab="Retraso salida", main="Relacion del retraso de llegada y el retraso de salida")  # Grafico bivariante
```

![](informe_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

3.  Realiza la descriptiva del retraso en la llegada segun el mes del
    a?o
4.  Haz el summary del retraso segun el mes y el boxplot estratificado

<!-- end list -->

``` r
# Analisi de variables
  # arr_delay: variable Numerica Continua
  # month: variable Categorica Ordinal?

tapply(datos2$arr_delay, datos2$month, summary)
```

``` r
boxplot(datos2$arr_delay~datos2$month, 
        xlab="Mes",
        ylab="Retraso de llegada (min.)",
        main="Retraso de llegada segun el mes")
```

![](informe_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->
