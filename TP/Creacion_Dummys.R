#previamente (se hace una sola vez):
install.packages("fastDummies")
install.packages("stringr")

#Inicia
library(fastDummies)
require(data.table)
library("stringr") 
library("dplyr")
library("rsample")
library(randomForest)
library(DALEX)
library(iBreakDown)

#carga:
datos_properati <- read.csv("properati_preprocesado_2022.csv")
datos_properati = datos_properati %>%
  mutate(surface_uncovered = surface_total - surface_covered)

# Create dummy variable
datos_properati_d <- dummy_cols(datos_properati, 
                                select_columns = "l3")
str(datos_properati_d)

#Corrijo los nombres de las columnas, para que los tome RandomForest:
#1 Elimino espacioes en Blanco:
for (col in colnames(datos_properati_d)){
  setnames(datos_properati_d, col , str_remove_all(col, " "))
} 

#2 Elimino /:
setnames(datos_properati_d, "l3_Centro/Microcentro" , "l3_CentroMicrocentro")

#3 Quito las columnas que NO voy a usar: l3, precio_en_miles, surface_total
datos_properati_d <- datos_properati_d %>% select(-c(l3, surface_total, precio_en_miles))

#crear dummy:
# fijamos semilla
set.seed(22)
# Partición Train y Test, indicando proporción
train_test_d <- initial_split(datos_properati_d, prop = 0.75)
train_data_d <- training(train_test_d)
test_data_d <- testing(train_test_d)


#rf
#opcion: modelo_randomForest <- randomForest(price ~.-id -l3 -precio_en_miles -surface_total, data = train_data_d)
modelo_randomForest <- randomForest(price ~.-id, data = train_data_d)

#Feature Importance
FI <- data.frame(importance(modelo_randomForest))
FI <- cbind(rownames(FI),FI)
FI[order(FI$IncNodePurity, decreasing = TRUE), ][2]


#explain
explain_rf <- DALEX::explain(model = modelo_randomForest,  
                             label = "Random Forest")

#selecciono observacion_1: en Puerto Madero
#variables_test <- test_data_d %>% select(-c(l3, surface_total, price, precio_en_miles))

variable_ya<-filter(test_data_d, l3_PuertoMadero == 1) #filtra el barrio

variable_y<-variable_ya[9,] #selecciona la n° 9 de ese barrio


#selecciono observacion_2: en Almagro
variable_ya2 <-filter(variables_test, l3_Almagro == 1 ) 
variable_y2 <-variable_ya2[5,]
variable_y2

#prediccion para la observacion seleccionada:
predict(modelo_randomForest, variable_y) #420695.9
predict(modelo_randomForest, var_y_small)


#breakdown
bd_rf <- predict_parts(explainer = explain_rf,
                        new_observation = variable_y,
                        type = "break_down")
bd_rf
plot(bd_rf)


#ibreakdown
ibd_rf <- predict_parts(explainer = explain_rf,
                        new_observation = variable_y,
                        type = "break_down_interactions")
ibd_rf 

plot(ibd_rf)
