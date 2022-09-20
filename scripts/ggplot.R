## Aqui es esencial mirar el pdf, ya que no me ha dado tiempo a copiar todas las funciones y formas de gráficos 



install.packages(c("here",
                   "tidyverse",
                   "patchwork",
                   "RColorBrewer"))
library(here) #refiere la ruta a la carpeta del proyecto
library(tidyverse) #incluye la librería ggplot2
library(ggplot2) #VISUALIZACIÓN DE DATOS
library(RColorBrewer) #paletas de color
library(plotly) #hacer los gráficos interactivos
library(patchwork) #combinar gráficos de ggplot
library(readr)

but <- read_csv(here("data/butterfly_dataset.csv"))
glimpse(but)
but <- but %>%
  mutate(date = str_c(year, "-" , month, "-", day)) %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d"))

but_sum <- but %>%
  group_by(plot, habitat, hab_type, date) %>%
  summarise(n_species = n(),
            abundance = sum(abundance))

#Pruebas de gráficos

ggplot(but_sum, aes(x = date,
                    y = abundance)) + geom_point()


ggplot(but_sum, aes(x = date,
                    y = abundance)) + geom_point(color = "orange")


ggplot(but_sum, aes(x = date,
                    y = abundance)) +
  geom_point(color = "purple", shape = "triangle", size = 3.8, alpha = 0.5)


ggplot(but_sum, aes(x = date,
                    y = abundance,
                    color = n_species)) +
  geom_point()


#Graficar número de especies vs. tiempo, donde el tamaño del punto represente la abundancia de mariposas en el muestreo

ggplot(data = but_sum, aes(x = date, y = n_species, size = abundance)) + geom_point()

#es lo mismo que: 

ggplot(data = but_sum, aes(x = date, y = n_species)) + geom_point(aes(size = abundance))

#se puede cambiar la escale: 

ggplot(data = but_sum, aes(x = date, y = n_species)) + geom_point(aes(size = abundance)) +
  scale_size(breaks = c(10, 20, 30 40, 50))


ggplot(data = but_sum, aes(x = date, y = abundance, color = n_species)) + geom_point() +
         scale_color_gradient(low = "gold", high = "red")

ggplot(data = but_sum, aes(x = date, y = abundance, color = n_species)) + geom_point() +
  scale_color_viridis_b()

ggplot(data = but_sum, aes(x = date, y = abundance, color = n_species)) + geom_point() +
  scale_color_viridis_c()



## Paletas de colores 

library(RColorBrewer)
display.brewer.all(
  colorblindFriendly = TRUE) #scale_color_

install.packages("rcartocolor")
library(rcartocolor)
rcartocolor::display_carto_all(
  colorblind_friendly = TRUE) #scale_color_

rcartocolor::display_carto_all(
  colorblind_friendly = FALSE)

library(MetBrewer)
MetBrewer::display_all() #scale_color_met_c("nombrede

#siempre se llama a los colores con "scale_color_met_b/c" , siendo X el nombre de la paleta y c o b si es binary o continous. 


ggplot(data = but_sum, aes(x = date, y = abundance, color = n_species)) + geom_point() +
  scale_color_met_c("Monet")


ggplot(but_sum, aes(x = date,
                    y = abundance,
                    color = n_species)) +
  geom_point() +
  scale_color_distiller(palette = "RdYlBu")




#Asignar de forma manual unos colores a variables categóricas: 

hab_cols <- c("agriculture" = "yellow", "disturbed_forest" ="brown",
             "closed_forest" = "dark green","road" = "grey" ,"secondary_forest" = "light green",
              "bamboo_forest" = "light blue", "unknown" = "black")
ggplot(but_sum, aes(x = date,
                    y = abundance,
                    color = habitat)) +
  geom_point() +
  scale_color_manual(values = hab_cols)



#Escalas y ejes

ggplot(but_sum, aes(x = date,
                    y = abundance,
                    color = n_species)) + geom_point() + scale_y_log10()

ggplot(but_sum, aes(x = date,
                    y = abundance,
                    color = n_species)) + geom_point() + scale_y_reverse()


#Graficar numero de especies vs. abundancia, donde el color del punto 
#represente el tipo de hábitat (forest vs.open)
hab_type_cols <- c("open" = "brown", "forest" = "green")

ggplot(but_sum, aes(x = n_species, y = abundance, color = hab_type)) + 
  geom_point() + scale_color_manual(values = hab_type_cols)

ggplot(but_sum, aes(x = n_species, y = abundance, color = hab_type)) + 
  geom_point() + scale_color_manual(values = hab_type_cols) +
  scale_x_log10() + scale_y_log10()


#linea de tendencia

ggplot(but_sum, aes(x = n_species, y = abundance)) + 
  geom_point()  + geom_smooth()

ggplot(but_sum, aes(x = n_species, y = abundance, color = hab_type)) + 
  geom_point() + scale_color_manual(values = hab_type_cols) + geom_smooth()

ggplot(but_sum, aes(x = n_species, y = abundance)) + 
  geom_point(aes(color = hab_type))  + geom_smooth()

#geom_rug() da el numero de observaciones
ggplot(but_sum, aes(x = n_species, y = abundance)) + 
  geom_point()  + geom_smooth() + geom_rug()


####Limitar escalas

#Aqui, la funcion lineal solo cuuenta los puntos que aparecen en el gráfico: 
ggplot(but_sum, aes(x = n_species,
                    y = abundance,
                    color = hab_type)) +
  geom_point() +
  geom_smooth(method="lm") +
  scale_y_continuous(limits = c(0, 300))

#Aqui, la lm considera los puntos que quedan fuera del gráfico
ggplot(but_sum, aes(x = n_species,
                    y = abundance,
                    color = hab_type)) +
  geom_point() +
  geom_smooth(method ="lm") +
  coord_cartesian(ylim = c(0, 300))


ggplot(but_sum, aes(x = n_species,
                    y = abundance)) +
  geom_point() +
  labs(x = "No. de especies", y = "Abundancia", title = "Muestreos de mariposas en Vietnam",
  caption = "cada punto representa un mues")



ggplot(but_sum, aes(x = date,
                    y = n_species,
                    color = abundance)) +
  geom_point() +
  scale_color_distiller(palette = "Spectral") +
  labs(x = "Año"
       ,
       y = "Número de especies"
       ,
       title = "Muestreos de mariposas en Vietnam",
subtitle = "Datos de Bonebrake et al. 2020",
       color = "Abundancia")

### GGPLOT INTERACTIVO ###
library(plotly)
ggplotly()

#movidas
install.packages("GGally")
GGally::ggpairs(but_sum)

