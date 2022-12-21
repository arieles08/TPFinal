#Instala el paquete que tiene el dataset del titanic
install.packages("archivist")

#Carga el Dataset train: titanic_imputed
titanic_imputed <- archivist::aread("pbiecek/models/27e5c")

#Explora Dataset Train
dim(titanic_imputed) #2207 filas y 9 cols

str(titanic_imputed)

#Genera el Henry 
(henry <- archivist::aread("pbiecek/models/a6538"))

#Genero a Johnny_D:
johnny_d <- henry
johnny_d$gender   <- 'male'
johnny_d$age      <- 8
johnny_d$class    <- '1st'
johnny_d$embarked <- 'Southampton'
johnny_d$fare     <- 72 
johnny_d$sibsp    <- 0 
johnny_d$parch    <- 0 



#Modelo Random Forest -> titanic_rf 
titanic_rf <- archivist:: aread("pbiecek/models/4e0fc")

#Carga la librerias para usar el BreakDown:
library("randomForest")
library("DALEX")

#Paso 1: explain al random forest
explain_rf <- DALEX::explain(model = titanic_rf,  
                             data = titanic_imputed[, -9],
                             y = titanic_imputed$survived == "yes", 
                             label = "Random Forest")

#Paso 2.1: breakdown a Johnny_D:
bd_rf1 <- predict_parts(explainer = explain_rf,
                       new_observation = johnny_d,
                       type = "break_down",
                       order = c("age", "class", "fare", "gender", "embarked", "sibsp", "parch"))
bd_rf1

plot(bd_rf1)

#Paso 2.2: breakdown a henry
bd_rf2 <- predict_parts(explainer = explain_rf,
                       new_observation = henry,
                       type = "break_down")
bd_rf2

plot(bd_rf2)

