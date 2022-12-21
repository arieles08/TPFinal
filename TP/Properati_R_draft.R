library(tidyverse)
library(tidymodels)
library(rsample)
library(ggplot2)
library(GGally)
library(DALEX)
library(randomForest)
library(iBreakDown)

datos_properati <- read.csv("properati_preprocesado_2022.csv")
# creamos nueva variable de superficie descubierta

datos_properati = datos_properati %>%
  mutate(surface_uncovered = surface_total - surface_covered)

dt <- datos_properati[2:10]

dt$surface_covered <- cut(dt$surface_covered, 10)
dt$surface_uncovered <- cut(dt$surface_uncovered, 10)

table(dt$l3, dt$surface_covered)


install.packages("fastDummies")

#-----------------------
$ l3                <chr> "Palermo", "Caballito", "Flores", "Flores", "Retiro", "Almagro", "Palermo", "Palermo", "Palermo", "Floresta", "C…
$ rooms             <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2…
$ bathrooms         <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1…
$ surface_covered   <int> 38, 55, 37, 37, 29, 41, 36, 31, 30, 36, 32, 34, 27, 30, 31, 40, 35, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, …
$ surface_uncovered <int> 3, 19, 5, 4, 3, 3, 4, 2, 2, 3, 0, 7, 0, 0, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 3, 1, 5, 0, 0, 7, 3, 2, …
$ property_type     <chr> "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", "Departamento", …
#----------------------------
surface_covered + surface_uncovered + rooms + bathrooms + property_type + l3
