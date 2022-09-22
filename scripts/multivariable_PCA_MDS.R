#This scripts introduce multivariate analysis

library(vegan)
library(mvabund)

########## PCA #############
data(iris)
head(iris)

#prepare and explore the data. Nos quedamos SOLO CON LAS VARIABLES CUANTITATIVAS
ir <- iris[, 1:4]
ir_species <- iris[, 5]
pairs(ir)
pairs(iris,
      lower.panel = NULL, 
      col = as.numeric(iris$Species))
cor(ir)
#run pca
#princomp
pca <- prcomp(ir, center = TRUE,
              scale. = TRUE)
#FORMAS PARA HACER PCA

iris.pca <- princomp(ir)
iris.pca
summary(iris.pca)

#la ultima linea del summary explica el porcentaje que explica de la varianza. El comp1 explica un 92% y el comp 3 el 97%

iris.pca2 <- prcomp(ir)
iris.pca2 

#La PCA explica la DISPERSION DE LA VARIANZA. En el summary, "proportion of variance" es donde vemos la 
#% de varianza que explica cada eje. Los ejes PC1 y PC2 son los más importantes. 

##Centrar y escalar la PCA es importante. Visualmente, si no escalamos las variables (estandarizamos su peso), nos puede
#salir una dispersión de la varianza muy alta para alguna variable respecto a las otras, 
#pero no ser algo real. SImplemente por no escalar. 
#Si una variable va de 0 a 100 y otra de 0 a 10 y las dos explican el mismo porcentaje de varianza, pero no escalamos el 
#plot, veremos que la de 0 a 100 tiene una influencia mucho mayor. R, por defecto, no escala. 

#Por eso: 

iris.pca <- prcomp(ir, center = TRUE, scale = TRUE)
iris.pca
#Cambian levemente los resultados
summary(iris.pca)

plot(iris.pca, type = "l")
biplot(iris.pca)

library(devtools)
install_github("vqv/ggbiplot")
library(ggbiplot)
g <- ggbiplot(iris.pca, obs.scale = 1, var.scale = 1,
              groups = ir_species, ellipse = TRUE,
              circle = TRUE)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal',
               legend.position = 'top')
g

#La pca se lee de tal manera que las flechas que indican cada variable están señaando hacia donde va la variable 
#de forma creciente. En su sentido contrario iria la variable en sentido decreciente. 

predict(iris.pca, newdata = (ir[1:2,]))


######## NMDS y PERMANOVA #######
#Es el concepto del ANOVA, si hay diferencia entre grupos, pero utiliza permutaciones. 
#NMDS es una forma de visualizar los datos, no un test estadístico. 

#load data
Herb <- read.csv(file = "data/Herbivore_specialisation.csv")
head(Herb)

library(reshape2)
#Nos permite hacer una tabla dinámica. Lo que hacemos aqui es una MATRIZ DE DISTANCIAS. Preparamos los
#los datos para el NMD
names(Herb)
Herbivores <- dcast(data = Herb,formula = Habitat + DayNight + Replicate + Mass ~ species, 
                    value.var = "abundance", fun.aggregate = sum)
head(Herbivores)
#Hemos desplegado la variable de "especies" al poner "~species" y hemos dicho a R que ponga en el valor de las nuevas celdas
#el sumatorio de las abundancias

#simplify objects
Habitat <- Herbivores$Habitat
DayNight <- Herbivores$DayNight
Herb_community <- Herbivores[,5:11]

#Indices de similaridad y disimilaridad

#A few words:
#binary:
    #Jackard
    #Sorensen (This coefficient weights matches in species composition
              #between the two samples more heavily than mismatches)
#quantitative
    #Euclidian: simple distance, good for e.g. distance between sites
    #bray: 0-1 The Bray-Curtis measure ignores cases in which the species
            #is absent in both community samples, and it is dominated
            #by the abundant species so that rare species add very little to the
            #value of the coefficient
#morisita: 0-1 independent of sample size. Only for counts. Recomended.
#kulczynski: Weigth more rare species.
#gower (allows factors!)

#Best is Legendre book numerical ecology
#(only found Krebs online): http://www.zoology.ubc.ca/~krebs/downloads/krebs_chapter_12_2014.pdf

#Ver indices en paquetes de R: 
?dist
?vegdist
?betadiver

#EL NMDS va a dibujar en 2 dimensiones las relaciones que hay entra cada uno de los puntos. Relfelja lo más fielmente posible, 
#de forma apoximada, la distancia entre puntos. 
#Estrés: diferencia entre

#NMDS: 
#mirar ?metaMDS
Herb_community.mds <- metaMDS(Herb_community, distance = "bray", trymax = 50)

#Cada RUn es una configuración distinta. Cuanto más bajo sea el stress mejor. Ha hecho 20 iteraiones (try = 20) 
#CUando dice "No convergence" quiere decir que no es combinación muy buena. Lo que podemos hacer es, como es un proceso
#random, podemos volver a correr el código. Si no, podemos cambiar "trymax" y subirlo más de 20 para que haga mas combinaciones
# o cambiar también K (el número de dimensiones) para aumentar la precisión del nmds

plot(Herb_community.mds$points, col = as.factor(Habitat), pch = 16)

#Lo que se ve aqui es la diferencia entre cada 2 puntos. Hay que leer el gráfico en función del indice que hemos elegido. 
#En este caso hemos elegido "Brays Courtis". Vemos que el habitat negro es más diferente que el resto. 

plot(Herb_community.mds$points, col = as.factor(Habitat), pch = 16)
Habitat.uni <- unique(Habitat)
legend(x = -2.5, y = -1.5, Habitat.uni, pch=16, col = 1:5, cex = 0.8)

DayNight <- as.factor(DayNight)
plot(Herb_community.mds$points, col = DayNight, pch = 16)
legend(x = -2.5, y = -1.5, c("Day","Night"), pch=16, col = 1:5, cex = 0.8)


#Si el stress es menor de 0.2 podemos decir que el plot refleja de forma aproximada a la matriz de disimilaridades 
#Si es más de 0.3, hay que intentar hacer más trys o añadir una 3a dimensión
Herb_community.mds$stress

#Dependiendo de los datos que ten
#PCA: correlación entre variables. 
#NMDS: mapa de equidistancias y enfasis en qué distasncia hay entre distintos puntos. En el nmds no hay muchas variables. 
#Se centra en los casos de estudio y la diferencia que hay entre ellos. 



##
##PERMANOVA ############

#Se hace con el paquete vegan y la función "adonis2"
a <- adonis2(Herb_community ~ Habitat* DayNight, method = "bray")
summary(a)
a

#Lo que nos die al correr "a" es mas útil que el summary. R2 indica el porcentaje de la varianza que explica cada variable. 

#Con la siguiente función podemos ves la dispersión de puntos dentro de los distintos niveles de una variable categórica. 
#EN español, si tenemos distintos habitats y numero de especies , podemos ver la dispersión dentro de cada habitat. Esto es la 
#beta diversidad. 

b <- betadisper(vegdist(Herb_community), group = Habitat)
anova(b)
#No tiene ninguna logica que se pida al modelo que nos de los resultados con "anova" pero el autor del paquete
#lo puso asi. (?)
#Groups: 4 habitats, No hay diferencias entre ellos. Es decir, son igual de heterogeneos 
boxplot(b)

#ADONIS: hay diferencias entre habitats?
#BEtadisperse: hay diferencias en la homegenieidad dentro de los habitats


########### The mvabund way ##########

## estos análisis han sido la dorma clasica de hacer análisis multivariable. No obstante, ahora hay formas
#cuyo objetivo es responder a la misma pregutna pero su mecanismo interno es más eficiente. Ejemplo: GLM. 
#Suele depender de nuestra pregunta. Si queremos hacer estudios más en profundidad y con mayor nivel de detalle 
# lo mejor es utilizar glm. De esta forma no tenemos que trabajar con matriz de distancias. 

#Libreria mvabund

library(mvabund)

head(Herbivores)
Herb_spp <- mvabund(Herbivores[, 5:11])
Herb_spp

#explorar los datos
boxplot(Herb_spp)
meanvar.plot(Herb_spp)

#fit manyglm
#Cogemos poisson porque son conteos
mod1 <- manyglm(Herb_spp ~ Habitat, family = "poisson")
plot(mod1)

#Hay mucha mas varianza con valores grandes que con pequeños segun los residuales. Es mejor hacer otro modelo. 

mod2 <- manyglm(Herb_spp ~ Habitat, family = "negative_binomial")
plot(mod2)
#aquí los residuales son homogeneos

anova(mod2, p.uni = "adjusted")
