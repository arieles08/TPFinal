#previamente (se hace una sola vez):
install.packages("fastDummies")
install.packages("stringr")

#Inicia
library(fastDummies)
require(data.table)
library("stringr") 

#carga:
datos_properati <- read.csv("properati_preprocesado_2022.csv")

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

importance(modelo_randomForest)

#explain
explain_rf <- DALEX::explain(model = modelo_randomForest,  
                             label = "Random Forest")

#selecciono observacion
variables_test <- test_data_d %>% select(-c(l3, surface_total, price, precio_en_miles))

variable_ya<-filter(variables_test, l3_PuertoMadero == 1) #filtra el barrio

variable_y<-variable_ya[9,] #selecciona la n° 9 de ese barrio
str(variables_test)


#ibreakdown
ibd_rf <- predict_parts(explainer = explain_rf,
                        new_observation = variable_y,
                        type = "break_down_interactions")
ibd_rf 