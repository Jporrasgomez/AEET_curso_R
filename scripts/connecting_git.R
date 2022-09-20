
install.packages("usethis")
library("usethis")



#conectar con nuestro git user
use_git_config(user.name = "Jporrasgomez",
               user.email = "jporrasgomez95@gmail.com")
git_sitrep()

#conectar R con git
usethis::create_github_token()

#Una vez creado y copiado el token y que estñe en la memoria del portapapeles del ordenador, 
#ejectuamos la siguiente funcion: 
gitcreds::gitcreds_set()

#se copia el token en la pregunta que hace la consola "? Enter password or token:"

#mi token: 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

#dudas: ¿como acceder a mi token? ¿se puede poner en varios ordenadores?


