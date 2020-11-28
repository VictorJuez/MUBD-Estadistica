############################################################
#
# MUBD - Clusterizacion no supervisada
#
############################################################

############################################################
# Ejercicio 5-1
############################################################
# Aplicar la clusterizacion jerarquica al conjunto de datos de 
# wine2.txt para clasificarlos segun tipologia (variable Type)
# 1. Lee los datos
setwd('C:/Users/jcortes/Google Drive/Docencia/Master/Material/Datasets')
wine <- read.table('wine2.txt',sep=',',header=TRUE)

# 2. Haz una descriptiva de los datos sin escalar
summary(wine)
pairs(wine)

# 3. Escala los datos y haz un heatmap
wine2 <- scale(wine)
heatmap(as.matrix(wine2))

# 4. Compara los 4 dendogramas resultantes de usar la distancia euclidea/manhattan y el metodo de wald y completo
# Que influye mas el tipo de distancia o el tipo de agrupacion?
d1 <- dist(wine2,method='euclidean')
d2 <- dist(wine2,method='manhattan')
hc1 <- hclust(d1,method = "ward")    
hc2 <- hclust(d1,method = "complete")    
hc3 <- hclust(d2,method = "ward")    
hc4 <- hclust(d2,method = "complete")
par(mfrow=c(4,1),las=1)
plot(hc1,cex=0.7)
plot(hc2,cex=0.7)
plot(hc3,cex=0.7)
plot(hc4,cex=0.7)

# 5. Segun los 4 dendogramas, ?Cuantos grupos de vino crees que hay? Haz la particion segun 
# el numero de grupos que creas conveniente para cada uno de los sistemas
ct1 <- cutree(hc1,k=3)
ct2 <- cutree(hc2,k=3)
ct3 <- cutree(hc3,k=3)
ct4 <- cutree(hc4,k=3)

# 6. Escoge una partici?n de las anteriores y calcula el % de variabilidad explicada
##-- Inercia entre (con wine)
IB <- 0
for(i in 1:3){
  w <- wine[ct1==i,]                       # Seleccionar los datos segun la clasificacion escogida
  n <- sum(ct1==i)                         # Calcular cuantos hay en ese cluster
  ymean <- apply(wine,2,mean)              # Calcular la media global de cada variable
  ymeangroup <- apply(w,2,mean)            # Calcular la media de este cluster para cada cluster
  ib <- n*sum((ymeangroup-ymean)^2)        # Inercia debida a este cluster
  IB <- IB + ib                            # Inercia entre acumalada
}

##-- Inercia global (con wine)
IT <- sum(apply(scale(wine,scale=FALSE)^2,2,sum,na.rm=TRUE))

##-- Varianza explicada
VE <- round(100*IB/IT,2)
VE
cor(d1, cophenetic(hc1)) 

# 7. Define las caracteristicas mas relevantes de cada grupo de vinos segun la clasificacion escogida. 
# En que caracteristica difieren menos?
apply(wine,2,tapply,ct1,summary)

par(mfrow=c(4,4),las=1)
for(i in 1:13) boxplot(wine2[,i]~ct1,main=colnames(wine2)[i])

pca <- PCA(wine,graph=FALSE)
par(mfrow=c(1,2))
plot(pca,col.ind=ct1,label='none')
plot(pca,choix = "var",cex=0.7)



############################################################
# Ejercicio 5-2
############################################################
##-- Variables
# FRESH: annual spending (m.u.) on fresh products (Continuous); 
# MILK: annual spending (m.u.) on milk products (Continuous); 
# GROCERY: annual spending (m.u.)on grocery products (Continuous); 
# FROZEN: annual spending (m.u.)on frozen products (Continuous) 
# DETERGENTS_PAPER: annual spending (m.u.) on detergents and paper products (Continuous) 
# DELICATESSEN: annual spending (m.u.)on and delicatessen products (Continuous); 

##-- Cargar algunas librerias
rm(list=ls())
library(NbClust)
library(clValid)
library(flexclust)
library(cluster)

##-- 1.Lee el fichero Wholesale customers data.csv
datos <- read.table('datos/Wholesale customers data.csv',header=TRUE,sep='\t')

##-- 2.Haz una descriptiva de los datos
##-- Sin logaritmos
summary(datos)               
datos2 <- scale(datos)            # Se saca el logaritmo para trabajar en escala multiplicativa
heatmap(datos2)                   
pairs(datos2)
boxplot(datos2)

##-- Con logaritmos
datos2 <- scale(log(datos+1))     # Se saca el logaritmo para trabajar en escala multiplicativa
heatmap(datos2)                   
pairs(datos2)
boxplot(datos2)


##-- 3. Decide el numero de clusteres segun alg?n criterio. ?Cuantos clusteres son los ideales?
##-- Regla del codo
VE <- c()
for (k in 1:10){
  km <- kmeans(datos2,centers=k,nstart=10)
  VE[k] <- km$betweenss/km$totss       
}
plot(VE,type="b",pch=19,xlab="N?mero de grupos",ylab="Variabilidad explicada")
# numero ideal = 5

# Paquete NbClust
set.seed(12345)
ncluster <- NbClust(datos2, min.nc=2, max.nc=10, method="kmeans")
ncluster
par(mfrow=c(1,1))
barplot(table(ncluster$Best.n[1,]))
# numero ideal = 2

##-- 4. Compara los siguientes 4 metodos:
# kmeans con algoritmo Lloyd y n-start=1
# kmeans con algoritmo Hartigan-Wong y n-start=1
# kmeans con algoritmo Hartigan-Wong y n-start=10
# kmeans despues de hacer una clusterizacion jerarquica y fijando los centros segun esta

# Para un numero de clusters de 5 a 8. Obten los siguientes indicadores/graficos
# El indice de Dunn (funcion dunn del paquete clValid)
# El indice silhouette (funcion del silhouette paquete cluster)

# Contesta las siguientes preguntas
# a. Segun los indices de Dunn y de silhouette, Cuantos clusteres se deberan escoger entre 5 y 8?
# b. La decision es independiente del sistema empleado?? La decision es independiente del indice empleado?


##-- Matrices para guardar resultados
DUN <- SIL <- matrix(nrow=4,ncol=4)                 

##-- Bucle para un numero de clusteres de 5 a 8
set.seed(12345)                                             # Semilla para replicar los resultados
D <- dist(datos2)                                           # Calcular distancias
for (i in 5:8){
  
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
  DUN[i-4,] <- c(dunn(D,km_1$cluster),
                 dunn(D,km_2$cluster),
                 dunn(D,km_3$cluster),
                 dunn(D,km_4$cluster))
  SIL[i-4,] <- c(summary(silhouette(km_1$cluster,D))[[4]],
                 summary(silhouette(km_2$cluster,D))[[4]],
                 summary(silhouette(km_3$cluster,D))[[4]],
                 summary(silhouette(km_4$cluster,D))[[4]])
  
}
round(DUN,3)
round(SIL,3)

##-- 5. Compara la similitud de los clusteres con el RAND INDEX para escoger 2 y 5 
# clusteres con el sistema m?s simple (Lloyd,nstart=1) y el sistema por defecto 
# con 10 inicios (Hartigan-Wong,nstart=10)

# 2 Clusteres
set.seed(1234)
km1 <- kmeans(datos2,2,algorithm='Lloyd',nstart=1)
km2 <- kmeans(datos2,2,nstart=10)
randIndex(x = km1$cluster, y= km2$cluster) 

# 5 Clusteres
set.seed(1234)
km1 <- kmeans(datos2,5,algorithm='Lloyd',nstart=1,iter.max = 30)
km2 <- kmeans(datos2,5,nstart=10)
randIndex(x = km1$cluster, y= km2$cluster) 


##-- 6. Describe los grupos segun la agrupacion escogida
# Hacer descriptiva, por ejemplo, segun boxplot
par(mfrow=c(2,3),mar=c(5,5,5,1))
for(i in 1:6) boxplot(datos2[,i]~km2$cluster,main=colnames(datos2)[i])