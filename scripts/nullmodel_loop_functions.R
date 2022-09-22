
## ¿LAS CORRELACIONES QUE HEMOS OBSERVADO SON REALES O FRUTO DEL AZAR?

#A simple null model used to test correlations----
#La idea del modelo nulo es crear un montón de valores de posibles correlaciones al relacionar nuestras observaciones 
#al azar. Si por ejemplo vamos al campo y muestreamos interacciones polinizador-planta, podemos hace un modelo nulo que luego
#distribuya al azar esas interacciones (entre todas las plantas y polinizadores que hemos observado). De esta manera podemos
#descartar que la correlación que hemos observado es azarosa. O también podemos comparar lo observado con nuestros datos
#con lo esperado. 

#The data

abundances <- c(1,3,4,7,8,13)
body_size <- c(9,6,3,3,1,1)
plot(abundance ~ body_size)
(corr <- cor(abundance,body_size))
cor.test(abundance,body_size)
original_correlation <- cor(abundance, body_size)

#COn estas formulas hemos visto si hay correlacion en los datos tal y como estan ordenados (1-9, 3-6, 4-3...) pero 
#tenemos que descartar que LA CORRELACIÓN SEA PURO AZAR. 
#se puede reordenar el orden de las variables para crear un MODELO NULO 


sampled_body_size <- sample(body_size, size = length(body_size), replace = F, prob = NULL)

#Para obtener muchos datos distintos, podemos correr muchas veces la siguiente linea: 

cor(abundances, sample(body_size, size = length(body_size), replace = F, prob = NULL)

#Modelo nulo: creamos una distribución de valores para observar que la correlación es diferente
#Lo ideal seria correr la linea de arriba muchas veces y guardar el resultado de las correlaciones y ver si hay
#una diferencia con la origina 

################# LOOPS #################

out <- c()

for(i in 1:10){
  print(paste("numero", i))
}

for(i in 1:10){
  out[i] <- (paste("numero", i))
}

# Out es el nombre que le damos a un vector vacio donde vamos a querer guardar los resultados del loop
# i es el nombre de una variable
# 1:10 el vector que indica el valor de i en cada vez que se corra el loop. La primera valdrá 1,
# después 2... Puedes poner un vector
# categórica y que cada vez i valga distintos factore
# Entre las llaves {} ponemos lo que queremos obtener del loop. 



## Hace loops ahora para calcular la distribución de correlaciones
null_model_corr <- rep(NA, 99)

for(i in 1:99){
  null_model_corr[i] <- cor(abundances, sample(body_size, size = length(body_size), replace = F, prob = NULL))
}

null_model_corr
hist(null_model_corr)
lines(c(original_correlation, original_correlation), c(0,20), col = "red")

#Vemos que nuestra correlacion original está fuera de la distribucion del histograma 

#P-value
#Si es 0 es que nuestra correlación es diferente a lo esperado por azar.

length(which(null_model_corr < original_correlation))/length(null_model_corr)



## MODELO NULO CON MÁS CHICHA

# is our community uneven?
#Perfect evenes means that all species have same abundances. That's impossible!

abundances

# let's calculate Pielou's Index (Mirar el documento word de recursos y links de course contents)

n_species <- length(abundances)

p <- abundances/sum(abundances)
S <- -sum(p * log2(p))
J <- S / log2(n_species)

#Funcionalizar: 

J <- function(x){
  p <- x/sum(x)
  S <- -sum(p * log2(p))
  J <- S / log2(length(x))
  J
}

#Las funciones nos devuelven la ultima línea que hayamos escrito. En este caso ponemos J para qu enos devuelva el resultado. 
#Ahora podemos incluir cualquier vector para J
J(abundances)

#Is the eveness higher than expected?
# Dado el numero de especies (6) y la abundancia de cada una (abundances), ¿cuál es la distribución de eveness 
# que podemos esperar?


rand <- sample(x = c(1:6), size = sum(abundances), replace = T, prob = NULL)
null_abundances <- table(rand)
J(null_abundances

#EN este caso hay reemplazp porque el size dle vector que queremos crear es de 36, y el original de 6

#Let's create the distribution

null_model_eve <- c()
for(k in 1:99){
  rand <- sample(x = c(1:6), size = sum(abundances), replace = T, prob = NULL)
  null_abundances <- table(rand)
  null_model_eve[k] <- J(null_abundances)
}
null_model_eve
hist(null_model_eve)

eve_original <- J(abundances)

lines(c(eve, eve), c(0, 60), col = "red")

#p-value
length(which(null_model_eve < eve_original))/length(null_model_eve)
