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
# k=3


############################################################
# Representacion grafica
############################################################
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
randIndex(table(km3$cluster,datos$activity))

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