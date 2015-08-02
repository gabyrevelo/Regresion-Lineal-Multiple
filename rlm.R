#2.1 Leer los archivos poblacion1.xlsx y poblacion2.xlsx , y analizar sus dimensiones.

install.packages("readxl",dependencies=TRUE)
library(readxl)
library(ggplot2)
library(DT)
po1<-sdata <- read_excel("poblacion1.xlsx",sheet = 1,col_names = TRUE, na=c(""))
po2<-sdata <- read_excel("poblacion2.xlsx",sheet = 1,col_names = TRUE, na=c(""))

dim(po1)
#La data población1 contiene 44 observaciones de 4 variables
dim(po2)
#La data población2 contiene 40 observaciones de 7 variables

#2.2 Una los archivos leídos en un mismo objeto llamado poblacion.

pobla<-match(po1$identificador,po2$identificador)
View(cbind(po1,po2[pobla,]))
poblacion1<-cbind(po1,po2[pobla,])
poblacion1
poblacion<-poblacion1[c(-24,-25,-30,-43),]
View(poblacion)

# Otra manera de unir los archivos leídos en un mismo objeto, es utilizando el comando merge
options(warn=-1)
data1 <- read_excel("poblacion1.xlsx",sheet=1,na="")
data2 <- read_excel("poblacion2.xlsx",sheet=1,na="")

poblacion <- merge(x = data1 ,y = data2, by = "identificador", suffixes = c("","")) 
poblacion <- poblacion[,-1]
datatable(poblacion)

#2.3 Cree un código que identifique la clase de cada variable y genere diagramas de cajas
#para variables continuas y diagramas de barras para variables discretas.

for(i in 1:ncol(poblacion)) {
     print(class(poblacion[,i]))
}

for(i in 1:ncol(poblacion)) {
if(is.numeric(poblacion[,i])){
boxplot(poblacion[,i], xlab = "", ylab=names(poblacion)[i], main=paste("Diag cajas de",names(poblacion)[i]),
        col="steelblue", border="gray1") 
}else{
    barplot(table(poblacion[,i]), xlab = names(poblacion)[i], ylab="Frecuencia", main="Diagrama de barras",
            col="steelblue", border="gray1")  
 }
}

#2.3 Cree un código que calcule automáticamente el mínimo, media, máximo, desviación
#estándar, primer cuartil de cada variable numérica y la frecuencia en el caso de variables categóricas.

install.packages("agricolae", dependencies=TRUE)
library(agricolae)

for(i in 1:ncol(poblacion)){
    if(is.numeric(poblacion[,i])){
        print( paste("El minimo, el maximo, la media,la desviacion estandar y el primer cuartil de ",names(poblacion)[i],"son"))
              print( c(min(poblacion[,i]),
                       max(poblacion[,i]),
                       mean(poblacion[,i]),
                       sd(poblacion[,i]),
                       
                       
                       quantile(poblacion[,i], probs = seq(0, 1, 0.25), na.rm = FALSE)))
    }else{
        print(paste("La frecuencia de ",names(poblacion)[i]))
        print(table(poblacion[,i]))
    }
}

#2.4 Calcule la correlación entre la variable dependiente poblacion y cada una de las
#variables explicativas (numéricas).

r<-numeric(ncol(poblacion))
for(i in 1:ncol(poblacion)){
    if(is.numeric(poblacion[,i])){
    r[i]<-cor(poblacion$poblacion,poblacion[,i])
  
    }
}
r

#2.5 Considere la variable categórica serv.bas.compl con una confiabilidad del 90%, ¿Puede asumirse que la media de la variable poblacion en el grupo serv.bas.compl: SI
#es distinta a la media del grupo serv.bas.compl: NO ? Utilice la función:

sb1<-subset()
t.test(x, y, alternative = c("two.sided", "less", "greater"),
       mu = 0, paired = FALSE, var.equal = FALSE,
       conf.level = 0.90)

#2.6 Considerando los cálculos anteriores genere el modelo de regresión lineal m?ltiple que mejor se ajuste a los
# datos. Interprete los coeficientes obtenidos

clase<-sapply(poblacion,class)
poblacion_reg<-poblacion[,clase=="numeric"]
regresion<-lm(poblacion~.,poblacion_reg)
summary(regresion)
(regre<-step(regresion,direction="backward"))
confint(regre,level = 0.95)

#2.7 Interprete el R

summary(regre)

#2.8 Analice la significancia de la regresión y de cada uno de los parámetros
#individuales.

qt(0.975,40)
#Ahora veremos si se rechaza o no se rechaza la H0, para ello utilizaremos
#la Razón t de Student. Tenemos que tj=8.617002 y el fractil de rden 1-α/2 es 2.021075
#luego, como el valor absoluto de tj es mayor que el fractil de orden 1-α/2, se rechaza Ho al nivel α
# Por lo tanto podemos decir que la regresión lineal es significativa.

#2.9 Realice un análisis detallado de los residuos

residuo <- regre[["residuals"]]
prediccion <- regre[["fitted.values"]]
data<- data.frame(poblacion[,"poblacion"],poblacion[,"tasa.crimen"], prediccion,residuo)
datatable(data,filter="top", options = list(
  searching = TRUE,
  pageLength = 5,
  lengthMenu = c(5, 10, 15)
))
hist(residuo,15)
mean(residuo)
qqnorm(residuo)
qqline(residuo,col="red")
plot(poblacion[,"tasa.crimen"],poblacion[,"poblacion"])
plot(residuo,prediccion)
plot(residuo,poblacion[,"tasa.crimen"])

#3 Genere un informe dinámico (archivo rlm.Rmd) en el cual se detalle cada uno
# de los pasos ejecutados en la generaci?n del modelo de regresión lineal múltiple,
# las conclusiones y resultados obtenidos.

