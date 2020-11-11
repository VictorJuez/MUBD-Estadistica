# Leemos los datos
data = read.table('p1a_train.csv', header = TRUE, sep = ';', dec = '.', stringsAsFactors = TRUE)

dim(data)
summary(data)

# Ver de que tipo son las variables
sapply(data, class)

# Establecemos variables categoricas
data$year = as.factor(data$year)
data$hour = as.factor(data$hour)
data$season = as.factor(data$season)
data$holiday = as.factor(data$holiday)
data$workingday = as.factor(data$workingday)
data$weather = as.factor(data$weather)

# Comparacion variables
pairs(numericas)
plot(atemp~count, numericas)
with(numericas,lines(lowess(atemp~count),col=2))

# Descriptiva bivariante para todas las variables NUMERICAS
var.num <- which(sapply(data,class)!="factor" & names(data)!="count" & names(data)!="id")
par(mfrow=c(2,2))
for(vn in var.num){
  plot(data$count~data[,vn],main=names(data)[vn],xlab=names(data)[vn],ylab="Count")
  with(data,lines(lowess(count~data[,vn]),col=2))
}

# Descriptiva para todas las variables CATEGORICAS
var.cat <- which(sapply(data,class)=="factor")
par(mfrow=c(2,2))
for(vc in var.cat)  {
  plot(data$count~data[,vc],main=names(data)[vc],xlab=names(data)[vc],ylab="Count")
  with(data,lines(lowess(count~data[,vc]),col=2))
}

# Establecer categorias de referencia en variables categoricas
data$year = relevel(data$year, ref="2011")
data$hour = relevel(data$hour, ref="0")
data$season = relevel(data$season, ref="1")
data$holiday = relevel(data$holiday, ref="0")
data$workingday = relevel(data$workingday, ref="0")
data$weather = relevel(data$weather, ref="1")


mod.lm1 = lm(count~., data)
summary(mod.lm1)

# Seleccion automatica de variables
mod.lm2 = step(mod.lm1)
summary(mod.lm2)

par(mfrow=c(2,2))
plot(mod.lm2)

library(car)
residualPlots(mod.lm2)