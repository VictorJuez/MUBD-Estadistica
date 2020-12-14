library(NbClust)
library(clValid)
library(flexclust)
library(cluster)
library(ggplot2)

datos <- read.table('Datos de entrenamiento.txt',header=TRUE,sep='\t', dec = '.')
datos$subject = NULL;
datos$activity = NULL;

datos = datos[1:100,]

summary(datos)
head(datos)

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

## Regla del codo
VE <- c()
for (k in 1:10){
  km <- kmeans(principales,centers=k,nstart=10)
  VE[k] <- km$betweenss/km$totss       
}
plot(VE,type="b",pch=19,xlab="Numero de grupos",ylab="Variabilidad explicada")

# Analisi de clusters
library(NbClust)
set.seed(12345)
ncluster = NbClust(principales, min.nc=2, max.nc=10, method="kmeans")
ncluster
par(mfrow=c(1,1))
barplot(table(ncluster$Best.n[1,]))
# k=3



#######
# Escalamos los datos
minValue = min(datos) * -1
datos2 = scale(log(datos+ minValue + 1))

DUN = SIL = matrix(nrow=6, ncol=4)
set.seed(12345)
D = dist(datos2)
for (i in 3:8){
  ##-- Clusterizacion jerarquica para obtener los centros
  hc <- hclust(D,method = 'ward.D') 
  cl <- cutree(hc,i)
  centers <- apply(datos2,2,tapply,cl,mean)
  
  ##-- 4 metodos
  km_1 <- kmeans(datos2,i,algorithm='Lloyd',nstart=1)       # Metodo 1
  km_2 <- kmeans(datos2,i)                                  # Metodo 2
  km_3 <- kmeans(datos2,i,nstart=10)                        # Metodo 3
  km_4 <- kmeans(datos2,centers=centers)                    # Metodo 4
  
  ##-- Calcular indices y guardarlos en una matrix
  DUN[i-2,] <- c(dunn(D,km_1$cluster),
                 dunn(D,km_2$cluster),
                 dunn(D,km_3$cluster),
                 dunn(D,km_4$cluster))
  SIL[i-2,] <- c(summary(silhouette(km_1$cluster,D))[[4]],
                 summary(silhouette(km_2$cluster,D))[[4]],
                 summary(silhouette(km_3$cluster,D))[[4]],
                 summary(silhouette(km_4$cluster,D))[[4]])
  
  print(DUN)
  print(SIL)
  print('-----------------------')
  
}
round(DUN,3)
m = which.max(DUN)
arrayInd(m, .dim = dim(DUN))
# Nos da el mejor resultado utilizando 8 clusters y el algoritmo de Lloyd

round(SIL,3)
m2 = which.max(SIL)
arrayInd(m2, .dim = dim(SIL))
# Nos da el mejor resultado utilizando 3 clusters y el metodo 3

