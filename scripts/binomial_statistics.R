##Ejemplo con el titanic

titanic <- read.csv("data/titanic_long.csv")
names(titanic)
str(titanic)
summary(titanic)



mtitanic <- lm(survived ~ class, data = titanic)
summary(mtitanic)

plot(mtitanic)
plot(mtitanic$residuals)
hist(mtitanic$residuals)


library(performance)
check_model(mtitanic)

library(effects)
plot(allEffects(mtitanic))

#el modelo es una mierda, no se ajusta un modelo linear a estos datos. Los residuos no tienen una distribución normal
#esto es porque supervivencia, nuestra variable respuestaes binaria . 
#Se necesita una ecuación de enlace (binomial, gaussiana...) Mirar pdf 246
#Una función de enlace permite que ajustemos el modelo lineal al tipo de variables que tenemos. 
# Para este caso, la link function es "logit" que es una probabilidad en una escala diferente. De -5 a 5 habitualmente. 


table(titanic$class, titanic$survived)

#La forma de visualizarlO: 
plot(factor(survived) ~ factor(class), data = titanic)

#creamos un modelo binomiaL: 
tit.glm <- glm(survived ~ class,
               data = titanic,
               family = binomial)
summary(tit.glm)
hist(tit.glm$residuals)

# los residuos no cambian, pero ya lo asumismo
# En summary, lo que vemos en estimate es la función "logit". La primera es la que menos probabilidad en escala logit tiene
# de sobrevivir (crew, en orden alfabáetico). El resto, classfirst, classsecond y classthird son las diferencias respecto al 
# intercept. 

allEffects(tit.glm)
plot(allEffects(tit.glm))

modelbased::estimate_means(tit.glm)
modelbased::estimate_contrasts(tit.glm)

performance::r2(tit.glm)

binned_residuals(tit.glm)

check_model(tit.glm)


library(DHARMa)
simulateResiduals(tit.glm, plot = T)


#Compares predicted vs observed probabilities
install.packages("predtools")
library(predtools)
titanic$surv.pred <- predict(tit.glm, type = "response")
calibration_plot(data = titanic, obs = "survived", pred = "surv.pred",
                 x_lim = c(0,1), y_lim = c(0,1))

#en el ultimo g´rafico lo que podemos ver es que puede haber cofounding effects en la predicción, que la clase no explica todo


# Los hombres sobrevivieron más que las mujeres?
str(titanic)

table(titanic$sex, titanic$survived)
  
plot(factor(survived) ~ factor(sex), data = titanic)

#creamos un modelo binomiaL: 
tit.glm2 <- glm(survived ~ sex,
               data = titanic,
               family = binomial)
summary(tit.glm2)
plot(allEffects(tit.glm2))

modelbased::estimate_means(tit.glm2)
modelbased::estimate_contrasts(tit.glm2)
performance::r2(tit.glm2)
binned_residuals(tit.glm2)
check_model(tit.glm2)

DHARMa::simulateResiduals(tit.glm2, plot = T)




#las mujeres sobreivieron más por ser mujeres o por ir más en primera clase?

table(titanic$class, titanic$survived, titanic$sex)

tit.glm3 <- glm(survived ~ sex*class, 
                data = titanic, 
                family = binomial)

summary(tit.glm3)
allEffects(tit.glm3)
plot(allEffects(tit.glm3))

plot_model(tit.sex.class.int, type = "int")



## proporciones titanic

tit.prop <- read.csv("data/titanic_prop.csv")

prop.glm1 <- glm(cbind(Yes, No) ~ Class,
                data = tit.prop,
                family = binomial)
summary(prop.glm)

prop.glm2 <- glm(cbind(Yes, No) ~ Sex,
                data = tit.prop,
                family = binomial)
summary(prop.glm2)

prop.glm3 <- glm(cbind(Yes, No) ~ Age,
                 data = tit.prop,
                 family = binomial)
summary(prop.glm3)

prop.glm4 <- glm(cbind(Yes, No) ~ Class*Sex*Age,
                 data = tit.prop,
                 family = binomial)
summary(prop.glm4)



#quasibinomial
