library(tidyverse)
library(readxl)
library(janitor)
library(spatstat)

#GUARDA: REINICIAR
# Todo el preprocesamiento de datos que querramos hacer offline. 




eval(parse('preprocesamiento/levantar_ECH.R', encoding="UTF-8"))
eval(parse('preprocesamiento/funciones_procesamientos_ech.R', encoding="UTF-8"))
eval(parse('preprocesamiento/genero_tablas_ech.R', encoding="UTF-8"))



saveRDS(tabla_resultados,"www/tabla_resultados.RDS")




