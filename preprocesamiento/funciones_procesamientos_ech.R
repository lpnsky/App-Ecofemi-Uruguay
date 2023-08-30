library(tidyverse)
library(httr)
library(stringr)
library(haven)
library(readxl)
library(foreign)
library(spatstat)

# Función de tasas por sexo (14 años y más)
tasas_por_sexo <- function(base){
  
  tabla <- base %>% 
    filter(e27 >= 14) %>% 
    group_by(anio, trim, Sexo) %>% 
    summarise(Poblacion         = sum(pesoano),
              Ocupados          = sum(pesoano[pobpcoac == 2]),
              Desocupados       = sum(pesoano[pobpcoac %in% c(3,4,5)]),
              PEA               = Ocupados + Desocupados,
              'Tasa de Actividad'                  = round(PEA/Poblacion*100, 1),
              'Tasa de Empleo'                     = round(Ocupados/Poblacion*100, 1),
              'Tasa de Desocupación'               = round(Desocupados/PEA*100, 1)) %>% 
    gather(indicador, valor, 4:ncol(.))
  
  return(tabla)
  
}


# Función de tasas por sexo y grupos de edad (14 años y más)
tasas_por_sexo_edad <- function(base){
  
  tabla <- base %>% 
    filter(e27 >= 14) %>% 
    group_by(anio, trim, Sexo,GRUPO_EDAD) %>% 
    summarise(Poblacion         = sum(pesoano),
              Ocupados          = sum(pesoano[pobpcoac == 2]),
              Desocupados       = sum(pesoano[pobpcoac %in% c(3,4,5)]),
              PEA               = Ocupados + Desocupados,
              'Tasa de Actividad'                  = round(PEA/Poblacion*100, 1),
              'Tasa de Empleo'                     = round(Ocupados/Poblacion*100, 1),
              'Tasa de Desocupación'               = round(Desocupados/PEA*100, 1)) %>% 
    gather(indicador, valor, 5:ncol(.)) %>% 
    filter(!is.na(GRUPO_EDAD))
  
  return(tabla)
  
}



# Función de brecha del ingreso total individual (perceptores)
brecha_ITI <- function(base){
  
  tabla <- base %>% 
    filter(PT4 > 0) %>% 
    group_by(anio, trim, Sexo) %>% 
    summarise(Media.ITI = round(weighted.mean(PT4, pesoano), 2)) %>% 
    spread(., Sexo, Media.ITI) %>% 
    mutate(brecha.ITI_corrientes = round(((Varones-Mujeres)/Varones)*100, 1)) %>% 
    select(anio, trim, media.mujeres = Mujeres, media.varones = Varones, brecha.ITI_corrientes)
  
  tabla_reg <- base %>% 
    filter(PT4 > 0) %>% 
    group_by(anio, trim, Sexo, REGION) %>% 
    summarise(Media.ITI = round(weighted.mean(PT4, pesoano), 2),
              cte_Media.ITI = round(weighted.mean(PT4_cons, pesoano), 2)) %>% 
    group_by(anio, trim, Sexo) %>%
    summarise(Media.ITI = mean(cte_Media.ITI, na.rm = T)) %>% 
    spread(., Sexo, Media.ITI) %>% 
    mutate(brecha.ITI = round(((Varones-Mujeres)/Varones)*100, 1)) %>% 
    select(anio, trim, cte_media.mujeres = Mujeres, cte_media.varones = Varones,brecha.ITI)
  
  tabla <- tabla %>% 
    left_join(.,tabla_reg, by = c("anio","trim"))
  
  return(tabla)
  
}



# Función de brecha del ingreso de la ocupación principal (ocupades)
brecha_IOP <- function(base){
  
  tabla <- base %>% 
    filter(pobpcoac == 2) %>% 
    group_by(anio, trim, Sexo) %>% 
    summarise(Media.IOP = round(weighted.mean(PT2, pesoano), 2)) %>%
    spread(., Sexo, Media.IOP) %>% 
    mutate(brecha.IOP_corrientes = round(((Varones-Mujeres)/Varones)*100, 1)) %>% 
    select(anio, trim, media.mujeres = Mujeres, media.varones = Varones, brecha.IOP_corrientes)
  
  tabla_reg <- base %>% 
    filter(pobpcoac == 2) %>%
    group_by(anio, trim, Sexo, REGION) %>% 
    summarise(Media.ITI = round(weighted.mean(PT2, pesoano), 2),
              cte_Media.ITI = round(weighted.mean(PT2_cons, pesoano), 2)) %>% 
    group_by(anio, trim, Sexo) %>%
    summarise(Media.ITI = mean(cte_Media.ITI, na.rm = T)) %>% 
    spread(., Sexo, Media.ITI) %>% 
    mutate(brecha.IOP = round(((Varones-Mujeres)/Varones)*100, 1)) %>% 
    select(anio, trim, cte_media.mujeres = Mujeres, cte_media.varones = Varones,brecha.IOP)
  
  tabla <- tabla %>% 
    left_join(.,tabla_reg, by = c("anio","trim"))
  
  
  return(tabla)  
  
}
