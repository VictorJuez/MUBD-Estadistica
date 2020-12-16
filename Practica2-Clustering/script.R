library(scatterplot3d)  
library(flexclust)
library(NbClust)        # NbClust
library(cluster)
library(factoextra)     # fviz_***
library(kernlab)        # kkmeans
library(clValid)        # clValid
library(cluster)        # pam
library(Amelia)

datos <- read.table('Datos de entrenamiento.txt',header=TRUE,sep='\t', dec = '.')
datos = datos[1:600,]

datos2 = datos;
datos2$subject = NULL;
datos2$activity = NULL;
datos2 = scale(datos2)

summary(datos)
head(datos)

## Regla del codo
VE <- c()
for (k in 1:10){
  km <- kmeans(datos2,centers=k,nstart=10)
  VE[k] <- km$betweenss/km$totss       
}
plot(VE,type="b",pch=19,xlab="Numero de grupos",ylab="Variabilidad explicada")
## K = 3


## ACP
#missing(datos)
#cor(datos)
cli = prcomp(datos2, scale=TRUE, center=TRUE)
#plot(cli)
principales = cli$x
principales = principales[,1:50]

# Analisi de clusters
set.seed(12345)
ncluster = NbClust(principales, min.nc=2, max.nc=10, method="kmeans")
ncluster
par(mfrow=c(1,1))
barplot(table(ncluster$Best.n[1,]))
# k=2


############################################################
# Representacion grafica
############################################################
##-- 2 grupos
km2 <- kmeans(datos2,centers=2,nstart=10)
ve2 <- km2$betweenss/km2$totss

##-- En las 2 primeras componentes
pr.comp <- princomp(datos2)
x <- pr.comp$scores[,1]
y <- pr.comp$scores[,2]
plot(x,y,pch=19,col=km2$cluster)

##-- 3 grupos
km3 <- kmeans(datos2,centers=3,nstart=10)
ve3 <- km3$betweenss/km3$totss

##-- En las 2 primeras componentes
pr.comp <- princomp(datos2)
x <- pr.comp$scores[,1]
y <- pr.comp$scores[,2]
plot(x,y,pch=19,col=km3$cluster)

## -- 4 grupos
km4 <- kmeans(datos2,centers=4,nstart=10)

## -- 6 grupos (solo para verificar con enunciado)
km6 <- kmeans(datos2,centers=6,nstart=10)

##-- En las 2 primeras componentes
pr.comp <- princomp(datos2)
x <- pr.comp$scores[,1]
y <- pr.comp$scores[,2]
plot(x,y,pch=19,col=km4$cluster)

# Nos quedamos con 3 clusters

############################################################
# Similitud con etiquetas iniciales
############################################################
randIndex(table(km2$cluster,datos$activity))



### PARTE 2

###### KNN
library(deldir)
library(kknn)
library(class)

############################################################
# Leer e inspeccionar los datos
############################################################
d0 = read.table('Datos de entrenamiento.txt',header=TRUE,sep='\t', dec = '.')
dim(d0)
summary(d0)

############################################################
# Eliminar variables irrelevantes y transformar algunas
############################################################
d = d0
d$subject = NULL

############################################################
# Dividir la muestra
############################################################
##-- Dividir en muestra de entrenamiento y muestra test
p <- 0.7                 # Proporcion en muestra de entrenamiento
n <- dim(d)[1]           # numero de observaciones 
set.seed(12345)
train.sel <- sample(c(FALSE,TRUE),n,rep=TRUE,prob=c(1-p,p))
train <- d[train.sel,]
test <- d[!train.sel,]

############################################################
# Comparar capacidad predictiva 
############################################################
##-- 1-NN Train + Test
knn1 <- knn(train[,-ncol(d)], test=test[,-ncol(d)], cl=train$activity, k = 1)
t <- table(knn1,test$activity)
t
sum(diag(t))/sum(t)

##-- 1-NN Cross-validation
knn2 <- knn.cv(d[,-ncol(d)], cl=d$activity, k = 1)
t <- table(knn2,d$activity)
sum(diag(t))/sum(t)

##-- Opcion Naive (Asignar a la categoria mayoritaria)
table(test$activity)
max(prop.table(table(test$activity)))

############################################################
# Numero de grupos
############################################################
p <- c()
K <- seq(1,21,2)
for(k in K){
  cat('Iteration:',k,'of',max(K),'\n')
  knn <- knn(train[,-ncol(d)], test=test[,-ncol(d)], cl=train$activity, k = k)
  t <- table(knn,test$activity)
  p[(k+1)/2] <- sum(diag(t))/sum(t)
}
plot(K,p,pch=19,type='b')
cbind(K,p)

############################################################
# Usar ACP --> Reduccion dimensionalidad previo a KNN
############################################################
res.acp0 <- princomp(d[,-ncol(d)])
screeplot(res.acp0,type='lines')
res.acp <- res.acp0$scores[,1]
train2 <- data.frame(c1=res.acp[train.sel])
test2 <- data.frame(c1=res.acp[!train.sel])


K <- seq(1,21,2)
p <- c()
for(k in K){
  cat('Iteration:',k,'of',max(K),'\n')
  knn <- knn(train2, test2, cl=train$activity, k = k)
  t <- table(knn,test$activity)
  p <- c(p,sum(diag(t))/sum(t))
}
plot(K,p,pch=19,type='b')
cbind(K,p)

##-- 1-NN
knn1 <- knn(train2, test2, cl=train$activity, k = 1)
t <- table(knn1,test$activity)
t
sum(diag(t))/sum(t)

############################################################
# Anyadir un minimo de votos (parametro l)
############################################################
knn2 <- knn(train2, test2, cl=train$activity, l=2, k = 2)
t <- table(knn2,test$activity)
sum(diag(t))/sum(t)
tmiss <- table(knn2,test$activity,useNA = 'always')
tmiss

############################################################
# Kernel
############################################################
##-- Con datos originales
kknn1 <- kknn(factor(activity)~., train, test,k=1)
fit <- fitted(kknn1)
t <- table(fit,test$activity)
sum(diag(t))/sum(t)     

p <- c()
K <- seq(1,21,2)
for(k in K){
  cat('Iteration:',k,'of',max(K),'\n')
  kknn <- kknn(factor(activity)~., train, test,k=k)
  t <- table(fitted(kknn),test$activity)
  p <- c(p,sum(diag(t))/sum(t))
}
plot(K,p,pch=19,type='b')
cbind(K,p)

##-- Con ACP --> NO mejora
kknn3 <- kknn(factor(train$activity)~., train2, test2,k=2)
fit <- fitted(kknn3)
t <- table(fit,test$activity)
sum(diag(t))/sum(t)

K <- seq(1,21,2)
for(k in K){
  cat('Iteration:',k,'of',max(K),'\n')
  kknn <- kknn(factor(train$activity)~., train2, test2,k=k)
  t <- table(fitted(kknn),test$activity)
  p[(k+1)/2] <- sum(diag(t))/sum(t)
}
plot(K,p,pch=19,type='b')
cbind(K,p)

############################################################
#
# Naive Bayes
#
############################################################

############################################################
# Cargar paquetes
############################################################
library(e1071)
library(ineq)

############################################################
# Leer datos
############################################################
datos <- read.table('Datos de entrenamiento.txt',header=TRUE,sep='\t', dec = '.', stringsAsFactors = TRUE)

############################################################
# Inspeccionar datos
############################################################
dim(datos)                                 # Dimension
summary(datos)                             # Descriptiva
table(apply(apply(datos,2,is.na),2,sum))   # Tabla con numero de missings

############################################################
# Premisa de independencia
############################################################
cor.matrix <- cor(datos[,-c(1,length(datos))])              # Matriz de correlaciones
cor.num <- as.numeric(cor.matrix)                           # Todas las correlaciones en un vector
t.cor <- table(cut(cor.num[cor.num!=1],br=seq(-1,1,0.1)))/2 # Categorazion de las correlaciones en intervalos de 0.1
t.cor
barplot(t.cor)

############################################################
# Dividir la muestra
############################################################
d = datos
d$subject = NULL
p <- 0.7                                   # Proporcion en muestra de entrenamiento
n <- nrow(d)                               # Numero de observaciones 
set.seed(12345)
train.sel <- sample(c(FALSE,TRUE),n,rep=TRUE,prob=c(1-p,p))
train <- d[train.sel,]
test <- d[!train.sel,]

############################################################
# Aplicar bayes
############################################################
nb <- naiveBayes(activity ~ ., train)
nb$levels       # Clases
nb$apriori      # A priori
nb$tables       # A posteriori

############################################################
# Capacidad predictiva
############################################################
##-- Global
preds <- predict(nb, newdata = test)
t <- table(preds, test$activity)
t
p.acierto <- sum(diag(t))/sum(t)
p.acierto

##-- Se pueden pedir las probabilidades de cada clase para inspecci?n visual
preds2 <- predict(nb, newdata = test,type = "raw")
head(preds2)
heatmap(preds2[1:50,],Rowv=NA,Colv=NA,col = cm.colors(256))

##-- Proporcion de acierto por clase
barplot(diag(prop.table(t,2)))

############################################################
# Importancia de las variables
############################################################
# Medir la importancia de las variables en funcion del indice Gini
graphics.off()                  # Cerrar ventanas
nbt <- nb$tables                # Tablas con medias
var.imp <- c()                  # Gini

##-- Calcular indice y graficar distribucion
for (i in 1:(ncol(d)-1)){
  ##-- Crear ventana grafica cada 9 graficos
  if((i-1) %% 9==0){
    par(mfrow=c(3,3))
  }
  
  x <- nbt[[i]][,1]
  barplot(x,main=names(d)[i])  # grafico
  var.imp[i] <- ineq(x)        # GINI         
}

##-- Los mas predictores
sel.pr <- order(var.imp, decreasing=TRUE)[1:9]
par(mfrow=c(3,3))
for (i in sel.pr){
  x <- nbt[[i]][,1]
  barplot(x,main=names(d)[i])
}

##-- Los menos predictores
sel.pr <- order(var.imp, decreasing=FALSE)[1:9]
par(mfrow=c(3,3))
for (i in sel.pr){
  x <- nbt[[i]][,1]
  barplot(x,main=names(d)[i])
}

############################################################
# Intento de mejora 1: Seleccionar mejores predictores
############################################################
##-- Selecionar los mejores predictores
hist(var.imp)
sel.pr <- which(var.imp>0.5)
train1 <- train[,c(paste0('feat',sel.pr),'activity')]

##-- Aplicar bayes nuevamente
nb1 <- naiveBayes(activity ~ ., train1, type='class')
preds <- predict(nb1, newdata = test)
t <- table(preds, test$activity)
p.acierto1 <- sum(diag(t))/sum(t)
p.acierto1

# TODO: INTENTO DE MEJORA 2 

#-----------------------------------------------------------
#
# Arboles condicionales
#
#-----------------------------------------------------------
library(randomForest)
library(party)
library(e1071)
############################################################
# Dividir la muestra
############################################################
datos <- read.table('Datos de entrenamiento.txt',header=TRUE,sep='\t', dec = '.', stringsAsFactors = TRUE)
d = datos
d$subject = NULL
p <- 0.7                                   # Proporcion en muestra de entrenamiento
n <- nrow(d)                               # Numero de observaciones 
set.seed(12345)
train.sel <- sample(c(FALSE,TRUE),n,rep=TRUE,prob=c(1-p,p))
train <- d[train.sel,]
test <- d[!train.sel,]

############################################################
# Visualizacion
############################################################
##-- Construirlo
ct.mod <- ctree(activity ~ ., train,controls=ctree_control(maxdepth=3)) # Poco profundo para poder graficarlo

##-- Visualizarlo
plot(ct.mod,type='extended')
plot(ct.mod,type='simple')

############################################################
# Evaluar capacidad predictiva
############################################################
ct.mod <- ctree(activity ~ ., train,controls=ctree_control(maxdepth=0)) # Profundidad maxima
pred <- predict(ct.mod,test,type="response")                          # prediccion de la respuesta
(t <- table(pred,test$activity))                                        # tabla de predicciones vs respuesta real
sum(diag(t))/sum(t)

############################################################
# Capacidad predictiva por clase
############################################################
barplot(diag(prop.table(t,2)))

############################################################
# Mejora: Podar el arbol --> No mejora
############################################################
##-- Con libreria partykit
library(partykit)
ct.mod <- ctree(activity ~ ., train,control=ctree_control(maxdepth=Inf))
nid <- nodeids(ct.mod)                                                               # id de los nodos
iid <- nid[!(nid %in% nodeids(ct.mod, terminal = TRUE))]                             # Prune manual: eliminar los nodos terminales

# P valores
pval <- unlist(nodeapply(ct.mod, ids = iid, FUN = function(n) info_node(n)$p.value)) # p-valores resultantes del test de los nodos no terminales
quantile(log(pval,10),seq(0,1,0.1))

# Modelo podado
ct.mod2 <- nodeprune(ct.mod, ids = iid[log(pval,10) > -95])
pred2 <- predict(ct.mod2,test,type="response")                                       
(t2 <- table(pred2,test$activity))                                                     
sum(diag(t2))/sum(t2) 

table(pred2,test$activity,useNA='alw')

##-- Con libreria tree --> No mejora
library(tree)
tr <- tree(activity ~ ., train)
cv.tr <- cv.tree(tr, FUN = prune.misclass)
plot(cv.tr)

# Modelo podado
ct.prune <- prune.misclass (tr ,best = 10)              # modelo
plot(ct.prune); text(ct.prune,pretty =0)                # arbol podado
pred3 <- predict (ct.prune ,test ,type="class")         # prediccion
t3 <- table(pred3,test$activity)                          # tabla de confusion 
sum(diag(t3))/sum(t3)                                   # porcentaje de acierto 


#-----------------------------------------------------------
#
# Random forest
#
#-----------------------------------------------------------
set.seed(12345)
rf.mod <- randomForest(activity~.,train,importance=TRUE,ntree=50,do.trace=TRUE)  
rf.mod
pred.rf <- predict(rf.mod,test)
(t <- table(pred.rf,test$activity))                         # tabla de predicciones vs respuesta real
sum(diag(t))/sum(t)    

############################################################
# Es necesario el conjunto de entrenamiento?
############################################################
##-- Comparacion de errores de clasificacion
EE1 <- c(1-sum(diag(t))/sum(t),1-diag(prop.table(t,2)))    # Error de prediccion observado con muestra test (Global y por clase) --> En random forest no se necesita muestra test
EE2 <- rf.mod$err.rate[50,]                                # Error de prediccion estimado con modelo (Global y por clase)
plot(EE1,type='b',col=1,lwd=2,pch=15,xlab='',ylab='OOB',ylim=0:1,lty=1)
lines(EE2,type='b',col=2,lwd=2,pch=15,xlab='',lty=2)
legend('topright',c('OBB test','OBB RF'),col=1:2,lty=1:2,lwd=2) 

##-- Comparacion de la matriz de confusion
t2 <- rf.mod$confusion
t2
round(prop.table(t(t),1),2)
round(prop.table(t2,1),2)

############################################################
# Necesitamos mas arboles?
############################################################
plot(rf.mod, type="l")

############################################################
# Importancia de las variables --> Interpretabilidad
############################################################
varImpPlot(rf.mod)
v.imp0 <- importance(rf.mod)

##-- Importancia Global
ord <- order(v.imp0[,'MeanDecreaseAccuracy'],decreasing=TRUE)
v.imp0[ord,c('MeanDecreaseAccuracy','MeanDecreaseGini')]

##-- Importancia para una clase concreta
ord <- order(v.imp0[,'walk'],decreasing=TRUE)
v.imp0[ord,c('walk')]

############################################################
# "Tunear" el parametro mtry
############################################################
mtry.par <- tuneRF(d[,1:93],d$activity)
set.seed(12345)
rf.mod1 <- randomForest(activity~.,train,importance=TRUE,ntree=50,do.trace=TRUE,mtry=18)
pred.rf1 <- predict(rf.mod1,test)
(t <- table(pred.rf1,test$activity))                        
sum(diag(t))/sum(t)   

## TODO: Ajustar random forest con mas arboles 500

#-----------------------------------------------------------
#
# SVM
#
#-----------------------------------------------------------

############################################################
# Escoger solo 4000 registros
# Transformar para solo tener 2 clases
# Dividir la muestra 50/50
############################################################
##-- Dividir en muestra de entrenamiento y muestra test (iguales)
p <- 0.5
set.seed(12345)
n0 <- 4000
sel <- sample(1:nrow(d),n0)
d1 <- d[sel,]
d1$activity2 <- factor(d1$activity=='walk') # Class_1
d1$activity <- NULL
train.sel <- sample(c(FALSE,TRUE),n0,rep=TRUE,prob=c(1-p,p))
train <- d1[train.sel,]
test <- d1[!train.sel,]

############################################################
# Ajustar modelo
############################################################
mod.svm <- svm(activity2~.,data = train,cost=1)

############################################################
# Capacidad predictiva
############################################################
pr <- predict(mod.svm,test)
t <- table(pr,test$target2)
sum(diag(t))/sum(t)
t

############################################################
# Tunear
############################################################
##-- Tunear el parametro de sobreajuste (cost)
mod.tune <- tune(svm,activity2~.,data=train,kernel="linear",ranges=list(cost=c(0.01,0.2,0.1,1,5,10,100)))
summary(mod.tune)
mod.tune$best.parameters

##-- Escoger el mejor modelo
mod.best <- mod.tune$best.model
summary(mod.best)

############################################################
# Capacidad predictiva
############################################################
pr <- predict(mod.best,test)
t <- table(pr,test$activity2)
t
sum(diag(t))/sum(t)

############################################################
# Kernels polynomial
############################################################
mod.tune1 <- tune(svm,activity2~.,data=train,kernel="polynomial",ranges=list(cost=c(0.01,0.2,0.1,1,5,10,100)))
summary(mod.tune1)
mod.best1 <- mod.tune1$best.model

############################################################
# Capacidad predictiva
############################################################
pr <- predict(mod.best1,test)
t <- table(pr,test$activity2)
sum(diag(t))/sum(t)

############################################################
# Kernel radial
############################################################
mod.tune2 <- tune(svm,activity2~.,data=train,kernel="radial",ranges=list(cost=c(0.01,0.2,0.1,1,5,10,100)))
summary(mod.tune2)
mod.best2 <- mod.tune2$best.model

############################################################
# Capacidad predictiva
############################################################
pr <- predict(mod.best2,test)
t <- table(pr,test$target2)
sum(diag(t))/sum(t)

############################################################
# Que kernel escoger?
############################################################
mod.tune3 <- tune(svm,activity2~.,data=train,cost=1,ranges=list(kernel=c('linear','polynomial','radial','sigmoid')))
summary(mod.tune3)
mod.best3 <- mod.tune3$best.model


############################################################
# Capacidad predictiva
############################################################
pr <- predict(mod.best3,test)
t <- table(pr,test$activity2)
t
sum(diag(t))/sum(t)


#### FINAL PREDICT
datosTest <- read.table('Datos Test.txt',header=TRUE,sep='\t', dec = '.', stringsAsFactors = TRUE)
datosTest$subject = NULL
pr <- predict(rf.mod1,datosTest)

nombre_objeto <- data.frame(activity=pr) # pr: predicciones 
write.table(nombre_objeto, 'p2.txt', row.names = FALSE, col.names = TRUE, sep='\t', quote = FALSE)

########################################################################################################################
########################################################################################################################
########################################################################################################################
########################################################################################################################
########################################################################################################################
########################################################################################################################

# Extra
#######
# Escalamos los datos
minValue = min(datos) * -1
datos2 = scale(log(datos+ minValue + 1))


## Imputacion multiple para los datos ausentes
#library(mice)
#tempData = mice(datos, m=5,maxit=50,meth='pmm',seed=500)
#summary(tempData)

# K=5

# ACP
library(Amelia)
#missing(datos)
#cor(datos)
cli = prcomp(datos, scale=TRUE, center=TRUE)
#plot(cli)
principales = cli$x
principales = principales[,1:50]
#head(principales)