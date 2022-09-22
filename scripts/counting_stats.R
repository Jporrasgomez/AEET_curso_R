##CONTEOS


seedl <- read.csv("data/seedlings.csv")

seedl.glm <- glm(count ~ light,
                 data = seedl,
                 family = poisson)

summary(seedl.glm)

allEffects(seedl.glm)
plot(allEffects(seedl.glm))

check_model(seedl.glm)

seedl.glm2 <- glm(count ~ light,
                 data = seedl,
                 family = quasipoisson)

summary(seedl.glm2)

allEffects(seedl.glm2)
plot(allEffects(seedl.glm2))

check_model(seedl.glm2)



#Obsrvar esfuerzo de muestreo
seedl.offset <- glm(count ~ light,
                    offset = log(area),
                    data = seedl,
                    family = poisson)
summary(seedl.offset)
