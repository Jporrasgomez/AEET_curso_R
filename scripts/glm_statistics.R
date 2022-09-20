#Estadística Gaussiana. N muestral grande!!
#Plantearse siempre si se está hablando de los datos observados, o del modelo predictorio. 

#PASOS: 
  #Visualizar data
  #Entender el modelo
  #Visualizar el modelo
  #Chequear el modelo
  #Hacer predicciones


library(tidyverse)
library(ggplot2)

trees <- read.csv("data/trees.csv")
str(trees)
trees$site <- as.factor(trees$site)

head(trees)

# What is the relationship between DBH and height?
#Do taller trees have bigger trunks?
#Can we predict height from DBH? How well?



########### Visualizar data ###########

plot(trees$height)
plot(trees$dbh)

hist(trees$height)
hist(trees$dbh)

plot(height ~ dbh, data = trees, las = 1)

#o

ggplot(trees) +
  geom_point(aes(dbh, height))



######## Entender modelo #############

#first: always plot the data. La variable predictora y respuesta no tienen por qué tener distribución normal, sí los residuos
#del modelo

#modelo regresión lineal
m1 <- lm(height ~ dbh, data = trees)


library(equatiomatic)
equatiomatic::extract_eq(m1)

summary(m1)


#Es esencial primero mirar los residuos. Minimo y máximo parecido, y 1q y 3q parecidos. Es simétrico, distribución normal. 


plot(m1)


hist(m1$residuals)

#Intercept: valor de y cuando x = 0. No tiene sentio biológico, porque un árbol sin tronco no puede tener un diámetro de 0, 
#pero es el punto de anclaje o partido del modelo. El error t del intercepto es bajo (error estándar, incertidumbre del
#intercepto) intercepto = 19.34 +- 0.31. ¿es este parámetro significativamente distinto de 0? La t de student es el
#estadistico utilizado para responder a esta pregunta y refleja la magnitud de la diferencia (en este caso, entre intercept
#y 0) y el p-value el rango de confianza del estadístico. Dar el p-value. En este caso, podemos decir que el intercepto es
#muy significativamente distinto de 0. 
#sin el resto de información descontextualiza los datos. 

#dbh, la variable predictora, da nombre a la pendiente. Por cada unidad de dhb aumentada, la altura aumenta en 0.6 unidades.
#La intertidumbre es muy baja (0.01). T de student y p-value responden que la pendiente SI es diferente de 0. 

#Residual standard error.
#Es el error del modelo. La media de la distancia debe ser próxima a 0. Pero la desviación
#típica de los residuos es de 4 metros. Es decir, lo que se desvia la altura del árbol respecto a la predicción que hace
#el modelo en función del dbh, es de 4 metros

#Grados de libertad (998). Número de observaciones menos el número de parámetros. Parámetros son 2 (a y b / dbh y height). 
#Reprsenta un poco la complejidad del model (si hay muchos parámetros) y la viabilidad del mismo (si hay pocas observaciones)

#R-squared. Define el % con el que el modelo puede predecir la altura del árbol. No es reflejo en sí mismo de la calidad del
#modelo, ya que hay que contextualizarlo. Hay veces que con un 10% podemos aceptarlo. Aaquí es muy alto porque la dbh
#sí que puede explicar de manera muy aislada la altura del árbol, ya que están muy relacionadas. Pero en humanos, la altura
#depende de multitud de factores (Raza, estilo de vida, etc)



#El error estándar es para parámetros (media, intercept, slope), la desviación media para datos. El error de 

#otras formas a parte del summary: 

coef(m1)

library("broom")
tidy(m1)

library(parameters)
parameters(m1)

library("report")
report(m1)

library("xtable")

xtable(m1, digits = 2)

#no funciona lo d ela tabla :(


####### Visualización del modelo ################

library(effects)
plot(allEffects(m1))


library(visreg)
visreg(m1)


library(see)
plot(parameters(m1), show_intercept = TRUE) +
  labs(title = "Height ~ Diameter") # ggplot2



######### Chequear modelo #########

library(performance)
check_model(m1)

#todo slos gráficos son modelo de lo que tiene que salir. Fijarse en esto en el future


library(easystats)
model_dashboard(m1)



#####PREDeCIR#######

#Predecir cuál seria el valor esperado de altura para un valor dado de dbh
new.dbh <- data.frame(dbh = c(39))
predict(m1, new.dbh, se.fit = TRUE)

#Ver la predicción en función del intervalo de confianza al 95%. Es el valor medio esperado. Aquí solo se tiene la incertidumbre
#de los parámetros (intercepto y pendiente // a y b)
predict(m1, new.dbh, interval = "confidence")

#En cambio, si vemos entre que valores está la altura para 39 cm de dbh basándonos en el "intervalo de predicción", nos fijamos
#en el 95% de los datos de altura que nuestra muestra contiene para el valor de 39 cm. Aquí tenemos en cuanta la varianza residual
#(epsilon)
predict(m1, new.dbh, interval = "prediction")




######### Variables predictoras categóricas ##########

plot(height ~ as.factor(sex), data = trees)
m2 <- lm(height ~ sex, data = trees)

summary(m2)

#como es un variable categórica de dos niveles, R coge que el intercepto hace referencia a sexfemale y el slope a sexmale. 
#Esto es por orden alfabético (female > male)

#Por tanto, el summary nos muestra que la altura media de las hemrbas des de 36.9312 con un error de 0.3981. Para el macho, 
#nos indica que la media es de 0.8432 m por debajo de las hembras. 

#Que no haya diferencia significativas en nuestro modelo quiere decir que no hemos encontrado diferencias significativas
#dentro de lo establecido por nuestro modelo en la muestra de árboles que hemos cogido. 

#El error estandar residual del modelo es de 8.86. Por lo tanto, el sexo no es buen predictor de la altura porque nos da una 
#desviacion de hasta 8.865 m. El dbh por ejemplo daba una desviación de 4 y algo. 
#Además, la R nos indica que el modelo explica un 0.2% de la altura. 

#Otra forma a parte del summary: 

library(modelbased)
estimate_means(m2)
estimate_contrasts(m2)


#Visualizar el modelo: 

plot(allEffects(m2))
visreg(m2)

#Ver residuos y chequear modelo

check_model(m2)
plot(m2)




###### Varias variables categóricas ######

plot(height ~ as.factor(site), data = trees)
plot(height ~ site, data = trees)

m3 <- lm(height ~ (site), data = trees)

summary(m3)


#el error estandar es cada vez mas grande e n los sitios porque cadaevez se muestrearon menos arboles
#(ver plot(height ~site, data = trees)

plot(m3)
hist(m3$residuals)

plot(simulate_parameters(m3), stack = FALSE)


#comparaciones: 
estimate_means(m3)
estimate_contrasts(m3)

kable(xtable::xtable(m3), digits = 2)

#visualizar el modelo

plot(allEffects(m3))
visreg(m3)

#chequear modelo
check_model(m3)



###### 2 variables predictoras a la vez #####

m4 <- lm(height ~ site + dbh, data = trees)
summary(m4)

#Muestra la altura esperada en cada sitio con dbh = 0. dbh tiene un valor de 0.6 ya que es la pendiente, igual que en m1

##Cambian los efectos bastante en los sitios al incorporar el dbh y el error standar residual es el menor de todos. 

plot(allEffects(m4))
visreg(m4)

plot_model(m4, type = "eff")
plot_model(m4, type = "est")
plot(parameters(m4))




###### otro modelo ######

m5 <- lm(formula = height ~ site * dbh, data = trees)
summary(m5)



############ EVALUAR CAPACIDAD PREDICTORA DEL MODELO #############
#mirar pdf





##### COMPARAR MODELOS  #########
#Hay que tener cuidado a la hora de comparar modleos. Puede hacer que tendamos a generar demasiados modelos que no
#tengan sentido biologico. 


library(performance)
compare_performance(m1, m2, m3, m4)

library(see)
plot(compare_performance(m1, m2, m3, m4))

library(parameters)
compare_parameters(m1, m2, m3, m4)

plot(compare_parameters(m1, m2, m3, m4))

