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


#breakdown
bd_rf <- predict_parts(explainer = explain_rf,
                        new_observation = variable_y,
                        type = "break_down")
bd_rf
plot(bd_rf)



#------------------------------------
# Replicamos los pasos del BreakDown para caso 9 de Puerto Madero (Properati):
data <- explain_rf[["data"]] #21579 registros y 8 cols
pred <- explain_rf[["y_hat"]]

#0: intercept              -> 204643.23
mean(predict(modelo_randomForest, data)) #-> 204643.2


#1: l3 = Puerto Madero :               -> 177093.3
data$l3 <- variable_y[,2]
mean(predict(modelo_randomForest, data)) #-> 177093.3 > 204643.23 + -27549.96


#2: surface_uncovered = 55              ->  205625.5 > 204643.23 + -27549.96  + 28532.21  
data$surface_uncovered <- variable_y[,8]
mean(predict(modelo_randomForest, data)) #-> 205625.5


#3: surface_covered = 90              -> 227686.8 > 204643.23 + -27549.96  + 28532.21 + 22061.34
data$surface_covered <- variable_y[,5]
mean(predict(modelo_randomForest, data)) #-> 227686.8


#4: bathrooms = 2            -> 248816.3 > 204643.23 + -27549.96  + 28532.21 + 22061.34 + 21129.48
data$bathrooms <- variable_y[,4]
mean(predict(modelo_randomForest, data)) #-> 248816.3


#5: property_type = Departamento -> 254239.4 > 204643.23 + -27549.96  + 28532.21 + 22061.34 + 21129.48 + 5423.07
data$property_type <- variable_y[,7]
mean(predict(modelo_randomForest, data)) #-> 254239.4


#6: rooms = 3              -> 240551.4 > 204643.23 + -27549.96  + 28532.21 + 22061.34 + 21129.48 + 5423.07  + -13687.97
data$rooms <- variable_y[,3]
mean(predict(modelo_randomForest, data)) #-> 240551.4 --> LLEGAMOS AL VALOR DE LA PREDICCION

#verificación:
predict(modelo_randomForest, variable_y) #-> 240551.4 --> VALOR PREDICCION COINCIDE

#----------------------------------


#ibreakdown
ibd_rf <- predict_parts(explainer = explain_rf,
                        new_observation = variable_y,
                        type = "break_down_interactions")
ibd_rf 
