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
screeplot(pr.comp,type='lines')

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
d = read.table('Datos de entrenamiento.txt',header=TRUE,sep='\t', dec = '.', stringsAsFactors = TRUE)
d$subject = NULL

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
##-- 1-NN Cross-validation
knn2 <- knn.cv(d[,-ncol(d)], cl=d$activity, k = 1)
t <- table(knn2,d$activity)
sum(diag(t))/sum(t)
## TODO: que validamos?

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
## K = 3 da el mejor resultado => 0.9622016

############################################################
# Usar ACP --> Reduccion dimensionalidad previo a KNN
############################################################
res.acp <- pr.comp$scores[,1:50]
train2 <- res.acp[train.sel,]
test2 <- res.acp[!train.sel,]


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

## Mejor resultado con k = 7 => 0.9429708 ==> NO MEJORA

############################################################
# Anyadir un minimo de votos (parametro l) => Out of scope
############################################################
# knn2 <- knn(train2, test2, cl=train$activity, l=2, k = 2)
# t <- table(knn2,test$activity)
# sum(diag(t))/sum(t)
# tmiss <- table(knn2,test$activity,useNA = 'always')
# tmiss

############################################################
# Kernel KNN
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
## Maximum k=15 => 0.9522546 ==> NO mejora

############################################################
#
# Naive Bayes
#
############################################################
library(e1071)
library(ineq)

############################################################
# Premisa de independencia
############################################################
cor.matrix <- cor(d[,-c(1,length(d))])              # Matriz de correlaciones
cor.num <- as.numeric(cor.matrix)                           # Todas las correlaciones en un vector
t.cor <- table(cut(cor.num[cor.num!=1],br=seq(-1,1,0.1)))/2 # Categorazion de las correlaciones en intervalos de 0.1
t.cor
barplot(t.cor)

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
  x <- nbt[[i]][,1]
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
nb1 <- naiveBayes(activity ~ ., train1)
preds1 <- predict(nb1, newdata = test)
t <- table(preds1, test$activity)
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

#-----------------------------------------------------------
#
# Arboles condicionales
#
#-----------------------------------------------------------
library(randomForest)
library(party)
library(e1071)

##-- Construirlo
ct.mod <- ctree(activity ~ ., train, control = ctree_control(maxdepth=3)) # Poco profundo para poder graficarlo

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
# 0.9124668

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
# 0.882626 --> No mejora

#-----------------------------------------------------------
#
# Random forest
#
#-----------------------------------------------------------
set.seed(12345)
rf.mod <- randomForest(activity~.,train,importance=TRUE,ntree=100,do.trace=TRUE)  
rf.mod
pred.rf <- predict(rf.mod,test)
(t <- table(pred.rf,test$activity))                         # tabla de predicciones vs respuesta real
sum(diag(t))/sum(t)
# 0.9602122

############################################################
# Es necesario el conjunto de entrenamiento? -> Probablemente no
############################################################
##-- Comparacion de errores de clasificacion
EE1 <- c(1-sum(diag(t))/sum(t),1-diag(prop.table(t,2)))    # Error de prediccion observado con muestra test (Global y por clase) --> En random forest no se necesita muestra test
EE2 <- rf.mod$err.rate[50,]                                # Error de prediccion estimado con modelo (Global y por clase)
plot(EE1,type='b',col=1,lwd=2,pch=15,xlab='',ylab='OOB',ylim=0:1,lty=1)
lines(EE2,type='b',col=2,lwd=2,pch=15,xlab='',lty=2)
legend('topright',c('OOB test','OOB RF'),col=1:2,lty=1:2,lwd=2) 

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
rf.mod1 <- randomForest(activity~.,train,importance=TRUE,ntree=200,do.trace=TRUE,mtry=92)
pred.rf1 <- predict(rf.mod1,test)
(t <- table(pred.rf1,test$activity))                        
sum(diag(t))/sum(t)   
# 0.9496021 -> No mejora

#-----------------------------------------------------------
#
# SVM
#
#-----------------------------------------------------------

############################################################
# Que kernel escoger?
############################################################
pr.comp = princomp(d[,-ncol(d)])
d2 = as.data.frame(pr.comp$scores[,1:10])
d2$activity = d$activity[1:nrow(d2)]

p <- 0.7                 # Proporcion en muestra de entrenamiento
n <- dim(d2)[1]           # numero de observaciones 
set.seed(12345)
train.sel <- sample(c(FALSE,TRUE),n,rep=TRUE,prob=c(1-p,p))
train2 <- d2[train.sel,]
test2 <- d2[!train.sel,]

mod.tune <- tune(svm,activity~.,
                  data = d2,
                  ranges = list(kernel = c('linear','polynomial','radial','sigmoid'),
                                cost = c(0.01,0.2,0.1,1,5,10,100)))
summary(mod.tune)
mod.best <- mod.tune$best.model


############################################################
# Capacidad predictiva
############################################################
mod.svm <- svm(activity~.,data = train, cost=10, kernel='radial')

pr <- predict(mod.svm,test)
t <- table(pr,test$activity)
sum(diag(t))/sum(t)
# 0.9801061


#### FINAL PREDICT
datosTest <- read.table('Datos Test.txt',header=TRUE,sep='\t', dec = '.', stringsAsFactors = TRUE)
datosTest$subject = NULL
pr <- predict(mod.svm,datosTest)

nombre_objeto <- data.frame(activity=pr) # pr: predicciones 
write.table(nombre_objeto, 'svm_radial_15.txt', row.names = FALSE, col.names = TRUE, sep='\t', quote = FALSE)