#librerias:
require(data.table)
library("randomForest")
library("DALEX")

#Instala el paquete que tiene el dataset del titanic
#install.packages("archivist")

#Carga el Dataset train: titanic_imputed
titanic_imputed <- archivist::aread("pbiecek/models/27e5c") #2207 filas y 9 cols

#Modelo Random Forest -> titanic_rf 
titanic_rf <- archivist:: aread("pbiecek/models/4e0fc")


#--------------Johnny D----------------------------
#Crea Johnny D (identico al libro):
johnny_d <- data.frame(
  class = factor("1st", levels = c("1st", "2nd", "3rd", 
                                   "deck crew", "engineering crew", 
                                   "restaurant staff", "victualling crew")),
  gender = factor("male", levels = c("female", "male")),
  age = 8, sibsp = 0, parch = 0, fare = 72,
  embarked = factor("Southampton", levels = c("Belfast",
                                              "Cherbourg","Queenstown","Southampton")))

#prediccion para johnny_d --> tiene que dar  yes= 0.422
predict(titanic_rf, johnny_d, type = "prob")

#Paso 1: explain al random forest
explain_rf <- DALEX::explain(model = titanic_rf,  
                             data = titanic_imputed[, -9],
                             y = titanic_imputed$survived == "yes", 
                             label = "Random Forest")

#Paso 2.1: breakdown a Johnny_D:
bd_rf1 <- predict_parts(explainer = explain_rf,
                        new_observation = johnny_d,
                        type = "break_down")
                        #,order = c("age", "class", "fare", "gender", "embarked", "sibsp", "parch"))
bd_rf1

plot(bd_rf1)


#Paso 2.2: Grafico de distribuciones (violin)
bd_rf1 <- predict_parts(explainer = explain_rf,
                        new_observation = johnny_d,
                        type = "break_down",
                        keep_distributions=TRUE)
#,order = c("age", "class", "fare", "gender", "embarked", "sibsp", "parch"))

plot(bd_rf1, plot_distributions=TRUE)


#Paso 2.3: breakdown a Johnny_D (escenario2):
bd_rf2 <- predict_parts(explainer = explain_rf,
                        new_observation = johnny_d,
                        type = "break_down",
                        order = c("class", "fare", "gender","age", "embarked", "sibsp", "parch"))
plot(bd_rf2)

#Paso 3: ibreakdown a Johnny_D:
ibd_rf1 <- predict_parts(explainer = explain_rf,
                        new_observation = johnny_d,
                        type = "break_down_interactions")
                        #,order = c("age", "class", "fare", "gender", "embarked", "sibsp", "parch"))
ibd_rf1

plot(ibd_rf1)



#----------------------------------
# Replicamos los pasos del BreakDown para johnny_d:
data <- explain_rf[["data"]] #2207 registros y 9 cols
pred <- explain_rf[["y_hat"]]

#0: intercept              -> 0.235
mean(predict(titanic_rf, data, type = "prob")[,'yes']) #-> 0.2353095

0.3785383 - 0.2353095 = 0.1432288
#1: age = 8:               -> 0.505 = +0.27 + (0.235)
data$age <- johnny_d[,3]
mean(predict(titanic_rf, data, type = "prob")[,'yes']) #-> 0.505121


#2: class = 1st            -> 0.591 = +0.086 + (0.235 + 0.27)    
data$class <- johnny_d[,1]
data$fare <- johnny_d[,6]
mean(predict(titanic_rf, data, type = "prob")[,'yes']) #-> 0.5906969

0.4844749 - 0.2353095 = 0.2491654
#3: fare = 72              -> 0.544 = -0.046 + (0.235 + 0.27 + 0.086)
data$fare <- johnny_d[,6]
data$age <- johnny_d[,3]
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
# Entendiendo la interpretación de las predicciones que usa para las contribuciones
dt <- explain_rf[["data"]]
dt <- data.table(dt)
data <- explain_rf[["data"]] #21579 registros y 8 cols

#0: intercept              -> 0.235
mean(predict(titanic_rf, data, type = "prob")[,'yes']) #-> 0.2353095
dt[,pred0 := predict(titanic_rf, data, type = "prob")[,'yes']]

#1: age = 8:               -> 0.505 = +0.27 + (0.235)
data$age <- johnny_d[,3]
mean(predict(titanic_rf, data, type = "prob")[,'yes']) #-> 0.505121

dt[,age_2 := johnny_d[,3]]
dt[,pred1 := predict(titanic_rf, data, type = "prob")[,'yes']]

#2: class = 1st            -> 0.591 = +0.086 + (0.235 + 0.27)    
data$class <- johnny_d[,1]
mean(predict(titanic_rf, data, type = "prob")[,'yes']) #-> 0.5906969
dt[,class_2 := johnny_d[,1]]
dt[,pred2 := predict(titanic_rf, data, type = "prob")[,'yes']]

#3: fare = 72              -> 0.544 = -0.046 + (0.235 + 0.27 + 0.086)
data$fare <- johnny_d[,6]
mean(predict(titanic_rf, data, type = "prob")[,'yes']) #-> 0.5443561
dt[,fare_2 := johnny_d[,6]]
dt[,pred3 := predict(titanic_rf, data, type = "prob")[,'yes']]

#4: gender = male          -> 0.461 = -0.083 + (0.235 + 0.27 + 0.086 - 0.046)
data$gender <- johnny_d[,2]
mean(predict(titanic_rf, data, type = "prob")[,'yes']) #-> 0.4611518
dt[,gender_2 := johnny_d[,2]]
dt[,pred4 := predict(titanic_rf, data, type = "prob")[,'yes']]


#5: embarked = Southampton -> 0.458 = -0.003 + (0.235 + 0.27 + 0.086 - 0.046 - 0.083)
data$embarked <- johnny_d[,7]
mean(predict(titanic_rf, data, type = "prob")[,'yes']) #-> 0.4584422
dt[,embarked_2 := johnny_d[,7]]
dt[,pred5 := predict(titanic_rf, data, type = "prob")[,'yes']]

#6: sibsp = 0              -> 0.452 = -0.006 + (0.235 + 0.27 + 0.086 - 0.046 - 0.083 - 0.003)
data$sibsp <- johnny_d[,4]
mean(predict(titanic_rf, data, type = "prob")[,'yes']) #-> 0.4523398
dt[,sibsp_2 := johnny_d[,4]]
dt[,pred6 := predict(titanic_rf, data, type = "prob")[,'yes']]


#7: parch = 0              -> 0.422 = -0.03 + (0.235 + 0.27 + 0.086 - 0.046 - 0.083 - 0.003 - 0.006)
data$parch <- johnny_d[,5]
mean(predict(titanic_rf, data, type = "prob")[,'yes']) #-> 0.422 --> LLEGAMOS AL VALOR DE LA PREDICCION
dt[,parch_2 := johnny_d[,5]]
dt[,pred7 := predict(titanic_rf, data, type = "prob")[,'yes']]

#exporto archivo:
fwrite(dt, file= "BD_johnnyd_step-by-step.csv", sep = ",")

getwd()

#----------------------------------
# Replicamos los pasos ver calculo de interaccion con iBreakDown para johnny_d:
dt <- explain_rf[["data"]]
dt <- data.table(dt)
data <- explain_rf[["data"]] #2207 registros y 9 cols
pred <- explain_rf[["y_hat"]]

#0: intercept              -> 0.235
mean(predict(titanic_rf, data, type = "prob")[,'yes']) #-> 0.2353095
dt[,pred0 := predict(titanic_rf, data, type = "prob")[,'yes']]

0.3785383 - 0.2353095 = 0.1432288
#1: age = 8:               -> +0.27 = 0.505 - 0.235 (intercept)
data$age <- johnny_d[,3]
mean(predict(titanic_rf, data, type = "prob")[,'yes']) - 0.235  #-> +0.27
dt[,pred_age8 := predict(titanic_rf, data, type = "prob")[,'yes']]

#2: fare:class   #Contribución INTERACCIÓN      -> -0.2308 = 0.0981 - 0.1854 - 0.1435 (CONJUNTA - Separada1 - Separada2)  
data <- explain_rf[["data"]] #se calcula siempre en el 1er paso, no es secuencial como antes
data$class <- johnny_d[,1]
data$fare <- johnny_d[,6]
mean(predict(titanic_rf, data, type = "prob")[,'yes']) - 0.235 #-> 0.09814545 #Contribución1 CONJUNTA
dt[,pred_fareclass := predict(titanic_rf, data, type = "prob")[,'yes']]
0.0981 - 0.1854 - 0.1435

#2.2:class = 1st  #Contribución1 por Separado    -> +0.185 = 0.420 - 0.235 (intercept)  
data <- explain_rf[["data"]] #se calcula siempre en el 1er paso, no es secuencial como antes
data$class <- johnny_d[,1]
mean(predict(titanic_rf, data, type = "prob")[,'yes']) - 0.235 #-> 0.1854449
dt[,pred_class1 := predict(titanic_rf, data, type = "prob")[,'yes']]

#3: fare:age   #Contribución INTERACCIÓN      -> -0.164 = 0.2495 - 0.27 - 0.1435 (CONJUNTA - Separada1 - Separada2)  
data <- explain_rf[["data"]] #se calcula siempre en el 1er paso, no es secuencial como antes
data$age <- johnny_d[,3]
data$fare <- johnny_d[,6]
mean(predict(titanic_rf, data, type = "prob")[,'yes']) - 0.235 #-> 0.2494749 #Contribución1 CONJUNTA
0.2495 - 0.27 - 0.1435

#2.1: fare = 72  #Contribución1 por Separado      ->  +0.143 = 0.378 - 0.235 (intercept)
data <- explain_rf[["data"]] #se calcula siempre en el 1er paso, no es secuencial como antes
data$fare <- johnny_d[,6]
mean(predict(titanic_rf, data, type = "prob")[,'yes']) - 0.235 #-> 0.1435383 
dt[,pred_fare72 := predict(titanic_rf, data, type = "prob")[,'yes']]

0.2495 - 0.27 - 0.1435
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


#exporto archivo:
fwrite(dt, file= "iBD_johnnyd_1st_step.csv", sep = ",")

getwd()


