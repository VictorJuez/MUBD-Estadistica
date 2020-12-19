library(scatterplot3d)  
library(flexclust)
library(NbClust)        # NbClust
library(cluster)
library(factoextra)     # fviz_***
library(kernlab)        # kkmeans
library(clValid)        # clValid
library(cluster)        # pam
library(Amelia)
##################################
saveResult = function(result, fileName) {
  nombre_objeto <- data.frame(activity=result) # pr: predicciones 
  write.table(nombre_objeto, fileName, row.names = FALSE, col.names = TRUE, sep='\t', quote = FALSE)
}

getD = function() {
  datos <- read.table('Datos de entrenamiento.txt',header=TRUE,sep='\t', dec = '.', stringsAsFactors = TRUE)
  d = datos
  d$subject = NULL
  return(d)
}

#################################

datos <- read.table('Datos de entrenamiento.txt',header=TRUE,sep='\t', dec = '.')
#datos = datos[1:600,]

datos2 = datos;
datos2$subject = NULL;
datos2$activity = NULL;

## Regla del codo
VE <- c()
for (k in 1:10){
  km <- kmeans(datos2,centers=k,nstart=10)
  VE[k] <- km$betweenss/km$totss       
}
plot(VE,type="b",pch=19,xlab="Numero de grupos",ylab="Variabilidad explicada")
round(VE, 2)
## K = 2


## ACP
pr.comp <- princomp(datos2)

# Analisi de clusters
set.seed(12345)
ncluster = NbClust(pr.comp$scores[1:500, 1:10], min.nc=2, max.nc=10, method="kmeans")
ncluster
par(mfrow=c(1,1))
barplot(table(ncluster$Best.n[1,]))
# k=2


############################################################
# Representacion grafica
############################################################
##-- 2 grupos
km <- kmeans(datos2,centers=2,nstart=10)
ve <- km$betweenss/km$totss

##-- En las 2 primeras componentes
x <- pr.comp$scores[,1]
y <- pr.comp$scores[,2]
plot(x,y,pch=19,col=km$cluster)

## Vemos en el grafico que la particion en dos clusters es coherente, a simple vista no se identifican mas de estos dos clusteres

##-- 6 grupos (Para comparar con las actividades)
km6 <- kmeans(datos2,centers=6,nstart=10)
ve6 <- km6$betweenss/km6$totss

##-- En las 2 primeras componentes
x <- pr.comp$scores[,1]
y <- pr.comp$scores[,2]
plot(x,y,pch=19,col=km6$cluster)

############################################################
# Similitud con etiquetas iniciales
############################################################
randIndex(table(km$cluster,datos$activity))
# Observamos un valor bajo, esto se debe a que la variable activity realmente consta de 6 categorias distintas de actividad y nosotros solo hemos sido capaces de identificar dos clusteres dado el conjunto de datos
# Entonces solo somos capaces de discernir dos categorias mientras deberiamos discernir 6

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
## TODO: que validamos?

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
## K = 3 da el mejor resultado

############################################################
# K = 3
############################################################
##-- 1-NN Train + Test
knn1 <- knn(train[,-ncol(d)], test=test[,-ncol(d)], cl=train$activity, k = 3, l=2)
t <- table(knn1,test$activity)
t
sum(diag(t))/sum(t)
## KNN k=3 => 0.9622016
## KNN k=3 l=2 => 0.9628401
## KNN k=3 l=3 => 0.9930982

##-- 1-NN Cross-validation
knn2 <- knn.cv(d[,-ncol(d)], cl=d$activity, k = 3)
t <- table(knn2,d$activity)
sum(diag(t))/sum(t)

## Predict results
datosTest <- read.table('Datos Test.txt',header=TRUE,sep='\t', dec = '.', stringsAsFactors = TRUE)
datosTest$subject = NULL
pr <- knn(d[,-ncol(d)], test=datosTest, cl=d$activity, k = 3)
pr2 <- knn(d[,-ncol(d)], test=datosTest, cl=d$activity, k = 3)
pr3 = pr
## KNN3 96% acierto
## KNN3L3 95.85

totalNulls = 0
for (i in 1:length(pr3)) {
  x = pr3[i]
  if (is.na(x)) {
    totalNulls = totalNulls+1
    pr3[i] = pr2[i]
  }
}

t <- table(pr3,test$activity)
t
sum(diag(t))/sum(t)

nombre_objeto <- data.frame(activity=pr3) # pr: predicciones 
write.table(nombre_objeto, 'p2_knn3.txt', row.names = FALSE, col.names = TRUE, sep='\t', quote = FALSE)


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
knn1 <- knn(train2, test2, cl=train$activity, k = 3)
t <- table(knn1,test$activity)
t
sum(diag(t))/sum(t)
## RESULTADOS MUY MALOS, NO NOS SIRVE

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
## Maximum 0.9522546

#### PRUEBAS
kknn1 <- kknn(factor(activity)~., train, test,k=15)
fit <- fitted(kknn1)
t <- table(fit,test$activity)
sum(diag(t))/sum(t)   
## KKNN k=3 => 0.9436340
## KKNN k=15 => 0.9522546

kknn <- kknn(factor(activity)~., d, datosTest,k=3)
fit = fitted(kknn)
saveResult(fit, 'kknn3.txt')
## KKNN k=3 => 95.25
## KKNN k=15 => 95.45

##-- Con ACP --> NO mejora
kknn3 <- kknn(factor(train$activity)~., train2, test2,k=15)
fit <- fitted(kknn3)
t <- table(fit,test$activity)
sum(diag(t))/sum(t)

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
# 0.8023873

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
# 0.8322281

############################################################
# Intento de mejora 2: Quitar variables correlacionadas
############################################################
##-- Sistema 1: Correlaciones grandes
high.corr <- which(cor.matrix>0.8,arr.ind = TRUE)
t.high.corr <- sort(table(as.numeric(high.corr))/2,decreasing=TRUE)
t.high.corr
sel.rm <- names(t.high.corr)[t.high.corr>=2] 
train2 <- train[,-which(names(train) %in% paste0('feat',sel.rm))]

##-- Aplicar bayes nuevamente
nb2 <- naiveBayes(activity ~ ., train2, type="class")
preds <- predict(nb2, newdata = test)
t <- table(preds, test$activity)
p.acierto2 <- sum(diag(t))/sum(t)
p.acierto2
# 0.7758621

##-- Sistema 2: Suma de correlaciones
cor.sum <- sort(apply(cor.matrix,2,sum),decreasing=TRUE)
cor.sum
sel.rm <- names(cor.sum)[1:30]
train3 <- train[,-which(names(train) %in% sel.rm)]

##-- Aplicar bayes nuevamente
nb3 <- naiveBayes(activity ~ ., train3, type="class")
preds <- predict(nb3, newdata = test)
t <- table(preds, test$activity)
p.acierto3 <- sum(diag(t))/sum(t)
p.acierto3
# 0.8083554

##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### 
##### IGNORADO! DEMASIADO TIEMPO DE EJECUCION
##-- Sistema 3: R2 --> No mejora (0.57) --> Muy costoso.
R2.max <- 0.5
d <- train[,-94]
ite <- 1
while(R2.max>=0.5){
  p <- ncol(d)
  R2 <- c()
  for(i in 1:p){
    res <- names(d)[i]
    mod.lm <- lm(as.formula(paste(res,'~.')),d)
    s.mod.lm <- summary(mod.lm)
    R2[i] <- s.mod.lm$r.squared
    cat('Iteration:',ite,'Model:',i,'R2:',R2[i],'\n')
  }
  R2.max <- max(R2)
  d <- d[,-which.max(R2)]
  
  ite <- ite + 1
}

##-- Aplicar bayes nuevamente
train4 <- d
train4$target <- train$target
test4 <- test[,names(train4)]
nb4 <- naiveBayes(target ~ ., train4, type="class")
preds <- predict(nb4, newdata = test4)
t <- table(preds, test$target)
p.acierto4 <- sum(diag(t))/sum(t)
p.acierto4

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
ct.mod <- ctree(activity ~ ., train,controls=ctree_control(maxdepth=2)) # Poco profundo para poder graficarlo

##-- Visualizarlo
plot(ct.mod,type='extended')
plot(ct.mod,type='simple')

############################################################
# Evaluar capacidad predictiva
############################################################
ct.mod <- ctree(activity ~ ., train) # Profundidad maxima
pred <- predict(ct.mod,test,type="response")                          # prediccion de la respuesta
(t <- table(pred,test$activity))                                        # tabla de predicciones vs respuesta real
sum(diag(t))/sum(t)
# 0.8885942

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
# 0.8580902 - NO MEJORA

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
# 0.882626

#-----------------------------------------------------------
#
# Random forest
#
#-----------------------------------------------------------
set.seed(12345)
rf.mod <- randomForest(activity~.,train,importance=TRUE,ntree=500,do.trace=TRUE)  
rf.mod
pred.rf <- predict(rf.mod,test)
(t <- table(pred.rf,test$activity))                         # tabla de predicciones vs respuesta real
sum(diag(t))/sum(t)
# 0.9602122

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
mtry.par <- tuneRF(d,d$activity)
set.seed(12345)
rf.mod1 <- randomForest(activity~.,train,importance=TRUE,ntree=200,do.trace=TRUE,mtry=184)
pred.rf1 <- predict(rf.mod1,test)
(t <- table(pred.rf1,test$activity))                        
sum(diag(t))/sum(t)   


#-----------------------------------------------------------
#
# SVM
#
#-----------------------------------------------------------

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

######## all classes
##-- 1. Mejora la capacidad predictiva en la clasificacion de todas las clases usando la funcion "tune" 
## (Prueba con un numero ligeramente mayor de filas)
d = getD()
p <- 0.5
set.seed(12345)
train.sel <- sample(c(FALSE,TRUE),nrow(d),rep=TRUE,prob=c(1-p,p))
test <- d[train.sel,]
train <- d[!train.sel,]

## Generacion modelo
mod.svm <- svm(activity~.,data = train,cost=1)

## Capacidad predictiva
pr <- predict(mod.svm,test)
t <- table(pr,test$activity)
sum(diag(t))/sum(t)
t


## Tunear modelo
mod.tune <- tune(svm,activity~.,data=train,kernel="linear",ranges=list(cost=c(0.01,0.2,0.1,1,5,10,100)))
summary(mod.tune)
mod.tune$best.parameters

##-- Escoger el mejor modelo
mod.best <- mod.tune$best.model
summary(mod.best)

## Capacidad predictiva
pr <- predict(mod.best,test)
t <- table(pr,test$activity)
sum(diag(t))/sum(t)
# 0.9798658
t

############################################################
# Que kernel escoger?
############################################################
mod.tune2 <- tune(svm,activity~.,
                  data = train,
                  ranges = list(kernel = c('linear','polynomial','radial','sigmoid'),
                                cost = c(0.01,0.2,0.1,1,5,10,100)))
summary(mod.tune2)
mod.best <- mod.tune2$best.model


############################################################
# Capacidad predictiva
############################################################
pr <- predict(mod.best,test)
t <- table(pr,test$activity)
t
sum(diag(t))/sum(t)
# 0.9790762

## Modelo manual
mod.svm <- svm(activity~.,data = train, cost=10, kernel='linear')

pr <- predict(mod.svm,test)
t <- table(pr,test$activity)
sum(diag(t))/sum(t)
# d kernel = radial cost = 10 => 0.9988
# d kernel = linear cost = 10 => 0.9948
# d kernel = radial cost = 15 => 0.9992

# train kernel = radial cost = 10 => 0.984739 
# cost = 15 => 0.9839357
# cost = 5 => 0.9831325


mod.tune <- tune(svm,activity~.,data=train,kernel="radial",ranges=list(cost=c(0.01,0.2,0.1,1,5,10,100)))


#### FINAL PREDICT
datosTest <- read.table('Datos Test.txt',header=TRUE,sep='\t', dec = '.', stringsAsFactors = TRUE)
datosTest$subject = NULL
pr <- predict(mod.svm,datosTest)

nombre_objeto <- data.frame(activity=pr) # pr: predicciones 
write.table(nombre_objeto, 'svm_radial_15.txt', row.names = FALSE, col.names = TRUE, sep='\t', quote = FALSE)

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