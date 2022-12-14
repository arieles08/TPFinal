#previamente (se hace una sola vez):
install.packages("fastDummies")
install.packages("stringr")

#Inicia
library(fastDummies)
require(data.table)
library("stringr") 

#carga:
datos_properati <- read.csv("properati_preprocesado_2022.csv")
datos_properati = datos_properati %>%
  mutate(surface_uncovered = surface_total - surface_covered)



#3 Quito las columnas que NO voy a usar: l3, precio_en_miles, surface_total
#datos_properati_d <- datos_properati_d %>% select(-c(l3, surface_total, precio_en_miles))

datos_properati_d <- datos_properati %>% select(-c(surface_total, precio_en_miles))


# fijamos semilla
set.seed(22)
# Partición Train y Test, indicando proporción
train_test_d <- initial_split(datos_properati_d, prop = 0.75)
train_data_d <- training(train_test_d)
test_data_d <- testing(train_test_d)


#rf
#opcion: modelo_randomForest <- randomForest(price ~.-id -l3 -precio_en_miles -surface_total, data = train_data_d)
modelo_randomForest <- randomForest(price ~.-id, data = train_data_d)

importance(modelo_randomForest)

#explain
explain_rf <- DALEX::explain(model = modelo_randomForest,  
                             label = "Random Forest")

#selecciono observacion
#variables_test <- test_data_d %>% select(-c(l3, surface_total, price, precio_en_miles))

variable_ya <- filter(test_data_d, l3 == "Puerto Madero") #filtra el barrio

variable_y<-variable_ya[9,] #selecciona la n° 9 de ese barrio
str(variable_y)

#breakdown
bd_rf <- predict_parts(explainer = explain_rf,
                        new_observation = variable_y,
                        type = "break_down")
bd_rf
plot(bd_rf)

#------------------------------------
# Replicamos los pasos del BreakDown para johnny_d:
data <- explain_rf[["data"]] #21579 registros y 8 cols
pred <- explain_rf[["y_hat"]]

#0: intercept              -> 0.235
mean(predict(titanic_rf, data, type = "prob")[,'yes']) #-> 0.2353095


#1: age = 8:               -> 0.505 = +0.27 + (0.235)
data$age <- johnny_d[,3]
mean(predict(titanic_rf, data, type = "prob")[,'yes']) #-> 0.505121


#2: class = 1st            -> 0.591 = +0.086 + (0.235 + 0.27)    
data$class <- johnny_d[,1]
mean(predict(titanic_rf, data, type = "prob")[,'yes']) #-> 0.5906969


#3: fare = 72              -> 0.544 = -0.046 + (0.235 + 0.27 + 0.086)
data$fare <- johnny_d[,6]
mean(predict(titanic_rf, data, type = "prob")[,'yes']) #-> 0.5443561


#4: gender = male          -> 0.461 = -0.083 + (0.235 + 0.27 + 0.086 - 0.046)
data$gender <- johnny_d[,2]
mean(predict(titanic_rf, data, type = "prob")[,'yes']) #-> 0.4611518


#5: embarked = Southampton -> 0.458 = -0.003 + (0.235 + 0.27 + 0.086 - 0.046 - 0.083)
data$embarked <- johnny_d[,7]
mean(predict(titanic_rf, data, type = "prob")[,'yes']) #-> 0.4584422


#6: sibsp = 0              -> 0.452 = -0.006 + (0.235 + 0.27 + 0.086 - 0.046 - 0.083 - 0.003)
data$sibsp <- johnny_d[,4]
mean(predict(titanic_rf, data, type = "prob")[,'yes']) #-> 0.4523398


#6: parch = 0              -> 0.422 = -0.03 + (0.235 + 0.27 + 0.086 - 0.046 - 0.083 - 0.003 - 0.006)
data$parch <- johnny_d[,5]
mean(predict(titanic_rf, data, type = "prob")[,'yes']) #-> 0.422 --> LLEGAMOS AL VALOR DE LA PREDICCION

#----------------------------------


#ibreakdown
ibd_rf <- predict_parts(explainer = explain_rf,
                        new_observation = variable_y,
                        type = "break_down_interactions")
ibd_rf 
