#librerias:
require(data.table)
library("randomForest")
library("DALEX")

#Instala el paquete que tiene el dataset del titanic
#install.packages("archivist")

#Carga el Dataset train: titanic_imputed
titanic_imputed <- archivist::aread("pbiecek/models/27e5c")

#Explora Dataset Train
dim(titanic_imputed) #2207 filas y 9 cols

str(titanic_imputed)

#Modelo Random Forest -> titanic_rf 
titanic_rf <- archivist:: aread("pbiecek/models/4e0fc")


#Genera el Henry 
henry <- archivist::aread("pbiecek/models/a6538")

#Create johnny_d from book:
johnny_d <- data.frame(
  class = factor("1st", levels = c("1st", "2nd", "3rd", 
                                   "deck crew", "engineering crew", 
                                   "restaurant staff", "victualling crew")),
  gender = factor("male", levels = c("female", "male")),
  age = 8, sibsp = 0, parch = 0, fare = 72,
  embarked = factor("Southampton", levels = c("Belfast",
                                              "Cherbourg","Queenstown","Southampton")))

#prediccion para johnny_d --> tiene que dar  yes= 0.422
predict(titanic_rf, johnny_d2, type = "prob")

johnny_d2 <- johnny_d
johnny_d2$age <- 90

table(titanic_imputed$age)

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

#----------------------------------
# Replicamos los pasos del BreakDown para johnny_d:
data <- explain_rf[["data"]] #2207 registros y 9 cols
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


#Paso 2.2: breakdown a henry
bd_rf2 <- predict_parts(explainer = explain_rf,
                       new_observation = henry,
                       type = "break_down")
bd_rf2

plot(bd_rf2)



#-----------------------------------------------------------------
#pruebas previas:

#Genero a Johnny_D:
johnny_d <- henry
johnny_d$gender   <- 'male'
johnny_d$age      <- 8
johnny_d$class    <- '1st'
johnny_d$embarked <- 'Southampton'
johnny_d$fare     <- 72 
johnny_d$sibsp    <- 0 
johnny_d$parch    <- 0 

filtro1 = (explain_rf[["data"]]$age == 8)  
df_fitro1 <- explain_rf[["data"]][filtro1,]

mean(explain_rf[["y_hat"]][filtro1]) #-> 0.472 (-0.033) || 10 registros #--> contribucion: +0.2366905

#2: class = 1st:  0.591 = 0.235 + 0.27 + 0.086
filtro2 = (explain_rf[["data"]]$class == '1st')  
df_fitro2 <- explain_rf[["data"]][filtro2,]

mean(explain_rf[["y_hat"]][filtro2]) #-> 0.5762963 (-0.0147) || 324 registros #--> contribucion: +0.2366905


sum(filtro2)