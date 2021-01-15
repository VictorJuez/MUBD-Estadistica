############################################################
# Ejercicio 6-1
############################################################

##-- 1. Lee los datos "bank-additional-full.csv"
rm(list=ls())
setwd('...')
datos <- read.csv('bank-additional-full.csv',header=TRUE,sep=';')
summary(datos)


##-- 2. Realiza una depuracion
# a. Ordena las categoricas ordinales
# b. Convierte en dummies las categoricas nominales
# c. Todas las variables deben ser de clase "numeric"
# d. Elimina varible pdays
datos$pdays <- NULL
to.dummy <- function(x,ordinal,ord=NULL){
  if(ordinal){
    y <- as.numeric(factor(x,levels=ord))
  }else{
    nl <- nlevels(x)
    y <- matrix(ncol=nl-1,nrow=length(x))
    for(i in 1:(nl-1)){
      y[,i] <- ifelse(x==levels(x)[i],1,0)
    }
    colnames(y) <- paste0(levels(x)[1:(nl-1)],'.dummy')
  }
  return(y)
}
job2 <- to.dummy(datos$job,FALSE)
marital2 <- to.dummy(datos$marital,FALSE)
education2 <- to.dummy(datos$education,TRUE,
                       c('unknown','illiterate','basic.4y','basic.6y','basic.9y',
                         'professional.course','high.school','university.degree'))
default2 <- to.dummy(datos$default,FALSE)
housing2 <- to.dummy(datos$housing,TRUE,c('unknown','no','yes'))
loan2 <- to.dummy(datos$loan,TRUE,c('unknown','no','yes'))
month2 <- to.dummy(datos$month,FALSE)
day_of_week2 <- to.dummy(datos$day_of_week,FALSE)
poutcome2 <- to.dummy(datos$poutcome,FALSE)

datos$job <- NULL
datos$marital <- NULL
datos$default <- NULL
datos$housing <- NULL
datos$loan <- NULL
datos$month <- NULL
datos$day_of_week <- NULL
datos$poutcome <- NULL

datos <- cbind(datos,job2,marital2,education2,default2,housing2,loan2,month2,day_of_week2,poutcome2)
dim(datos)
for(i in 1:length(datos)) datos[,i] <- as.numeric(datos[,i])

##-- 3.Train + test en 10000 primeros
p <- 0.7                 # Proporcion en muestra de entrenamiento
n <- 10000               # numero de observaciones 
set.seed(12345)
train.sel <- sample(c(FALSE,TRUE),n,rep=TRUE,prob=c(1-p,p))
train <- datos[which(train.sel),-which(names(datos)=='y')]
test <- datos[which(!train.sel),-which(names(datos)=='y')]
y.train <- datos[which(train.sel),'y']
y.test <- datos[which(!train.sel),'y']

##-- 3. Estudia el numero de grupos idoneo en una submuestra de 10000
K <- seq(1,21,2)
p <- c()
for(k in K){
  cat('Iteration:',k,'of',max(K),'\n')
  knn1 <- knn(train, test, cl=y.train, k = k)
  t <- table(knn1,y.test)
  p[(k+1)/2] <- sum(diag(t))/sum(t)
}
cbind(K,p)


############################################################
# Ejercicio 6-2
############################################################

##----------------------------------------------------------------------------------------------------
##-- 1. Reduce el numero de variables predictoras a 20 y aplica el KNN con diferente n�mero de vecinos
## �Mejora la capacidad predictiva medida por la proporci�n de aciertos? (15')
############################################################
# Cargar paquetes
############################################################
# install.packages('e1071')
# install.packages('ineq')
# install.packages('class')
library(e1071)
library(ineq)
library(class)

############################################################
# seleccionar variables relevantes (hecho en clase)
############################################################
setwd('...')
datos <- read.csv2('products.csv',header=TRUE)
##-- Dividir muestra
d <- datos[,-1]                            # Eliminamos identificador
p <- 0.7                                   # Proporcion en muestra de entrenamiento
n <- nrow(d)                               # Numero de observaciones 
set.seed(12345)
train.sel <- sample(c(FALSE,TRUE),n,rep=TRUE,prob=c(1-p,p))
train <- d[train.sel,]
test <- d[!train.sel,]

##-- Aplicar Bayes
nb <- naiveBayes(target ~ ., train)

##-- Importancia de las variables
nbt <- nb$tables                # Tablas con medias
var.imp <- c()                  # GINI
for (i in 1:(ncol(d)-1)){var.imp[i] <- ineq(nbt[[i]][,1])}

##-- Seleccionar las 20 mas importantes
var.imp20 <- which(order(var.imp,decreasing=TRUE)<=20)
datos.knn <- d[,c(paste0('feat_',var.imp20),'target')]

##-- Aplicar knn con diferentes vecinos
train1 <- datos.knn[train.sel,]
test1 <- datos.knn[!train.sel,]

VECINOS <- seq(1,21,2)
prop.acierto <- c()
j <- 1
for(k in VECINOS){
  mod.knn <- knn(train1[,-21],test1[,-21],train1$target, k=k)
  t <- table(mod.knn,test1$target)
  prop.acierto[j] <- sum(diag(t))/sum(t)   
  j <- j+1
}
cbind(VECINOS,prop.acierto)  # k=15 KNN = 0.5830218 vs. 0.5840343

# se obtiene mejor capacidad predictiva con NB

##-- 2. Obten una medida probab�listica del KNN y promediala con la obtenida con el NB. Asigna a un grupo segun esta nueva probabilidad
## �Mejora la capacidad predictiva medida por la proporci�n de aciertos? (15')
pred.nb0 <- predict(nb, newdata = test,type='raw')
pred.nb <- apply(pred.nb0,1,max)

pred.knn0 <- knn(train1[,-21],test1[,-21],train1$target, k=15, prob=TRUE)
pred.knn <- attributes(pred.knn0)$prob

sel.knn <- pred.knn>pred.nb 
table(sel.knn)

pred.nb1 <- predict(nb, newdata = test)
pred.knn1 <- knn(train1[,-21],test1[,-21],train1$target, k=15)

pred <- ifelse(sel.knn,as.character(pred.knn1),as.character(pred.nb1))
t <- table(pred,test$target)
sum(diag(t))/sum(t) # 0.5922118 --> Mejora la capacidad predictiva


