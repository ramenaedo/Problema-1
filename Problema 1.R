library(tidyverse)
library(tidytext)
library(naivebayes)
library(tm)
library(caret)
library(car)
library(ggplot2)
library(class)

#Comenzaremos leyendo nuestro datos
#https://rpubs.com/jboscomendoza/naive_nayes_con_r_clasificacion_texto
#Especial agradecimiento a esa pagina
download.file(url = "https://raw.githubusercontent.com/ramenaedo/iris.data/main/datos", destfile = "flores.csv")
flores_df<-read_csv("flores.csv")
#Realizamos los scatterplots pertinentes
#https://elartedeldato.com/blog/como-cambiar-el-color-de-un-grafico-en-ggplot/#:~:text=Para%20a%C3%B1adir%20la%20paleta%20de,de%20categor%C3%ADas%20de%20la%20variable.
#https://www.youtube.com/watch?v=Zza_zf8tVY8&ab_channel=Lic.LourdesCuellar

#Contemos la cantidad de observaciones en nuestra muestra
table(flores_df$genero)

#Realizamos el primer scatterplots
attach(flores_df)
names(flores_df)
grafica1=ggplot(flores_df,aes(longitudsepalo,anchosepalo,color=genero))
grafica1+geom_point()
#Realizamos el segundo scatterplots
grafica2=ggplot(flores_df,aes(longitudsepalo,longitudpetalo, color=genero))
grafica2+geom_point()
#Realizamos el tercer scatterplots
grafica3=ggplot(flores_df,aes(longitudsepalo,anchopetalo,color=genero))
grafica3+geom_point()

#Entrenando a bayes
flores_df$genero<-(as.factor(flores_df$genero))
attach(flores_df)
modelo <- naive_bayes(genero~ longitudpetalo+anchopetalo+longitudsepalo+anchosepalo,  data = flores_df, usekernel=T)

#Para ver uno por uno mas grande
par(mfrow=c(1,1))
plot(modelo)
#Para ver los 4 juntos
par(mfrow=c(2,4))
plot(modelo)

#Veamos la Correlacion entre los datos
cor(flores_df[,c("longitudsepalo","longitudpetalo","anchosepalo","anchopetalo")])

#Prediciendo Naive Bayes
predict(modelo,newdata = data.frame(longitudpetalo=3,anchopetalo=2,longitudsepalo=3,anchosepalo=2))

#Prediciendo Vecinos Cercanos
attach(flores_df)
knn(train=cbind(longitudpetalo,anchopetalo,longitudsepalo,anchosepalo),test=cbind(3,2,3,2),cl=genero,k=1,prob=T)
knn(train=cbind(longitudpetalo,anchopetalo,longitudsepalo,anchosepalo),test=cbind(3,2,3,2),cl=genero,k=5,prob=T)
knn(train=cbind(longitudpetalo,anchopetalo,longitudsepalo,anchosepalo),test=cbind(3,2,3,2),cl=genero,k=10,prob=T)
knn(train=cbind(longitudpetalo,anchopetalo,longitudsepalo,anchosepalo),test=cbind(3,2,3,2),cl=genero,k=25,prob=T)
knn(train=cbind(longitudpetalo,anchopetalo,longitudsepalo,anchosepalo),test=cbind(3,2,3,2),cl=genero,k=50,prob=T)
knn(train=cbind(longitudpetalo,anchopetalo,longitudsepalo,anchosepalo),test=cbind(3,2,3,2),cl=genero,k=75,prob=T)
knn(train=cbind(longitudpetalo,anchopetalo,longitudsepalo,anchosepalo),test=cbind(3,2,3,2),cl=genero,k=100,prob=T)
knn(train=cbind(longitudpetalo,anchopetalo,longitudsepalo,anchosepalo),test=cbind(3,2,3,2),cl=genero,k=125,prob=T)
knn(train=cbind(longitudpetalo,anchopetalo,longitudsepalo,anchosepalo),test=cbind(3,2,3,2),cl=genero,k=150,prob=T)
