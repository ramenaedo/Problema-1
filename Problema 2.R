library(tidyverse)
library(tidytext)
library(tm)
library(caret)
library(car) #para recodificar
library(class)
library(rpart)
library(rpart.plot)
#Gracias a:
#https://rpubs.com/jboscomendoza/arboles_decision_clasificacion
#https://rpubs.com/dsulmont/475705

#Comenzaremos leyendo nuestros datos

download.file(url = "https://raw.githubusercontent.com/ramenaedo/tae.data/main/datos", destfile = "datos.csv")

datos_df<-read_csv("datos.csv")

#Veamos si nuestros datos son balanceados
table(datos_df$Calificacion)

#Recodificamos
datos_df$Calificacion1<-recode(datos_df$Calificacion, "1=1 ; 2:3=0")

#Vemaos si sigue siendo balanceado
table(datos_df$Calificacion1)

#Creamos el arbol
datos_df$Calificacion1<-as.factor(datos_df$Calificacion1)
attach(datos_df)
modelo<-rpart(Calificacion1~Idioma+Instructor+Curso+Semestre+Estudiantes,data=datos_df)
rpart.plot(modelo) 
