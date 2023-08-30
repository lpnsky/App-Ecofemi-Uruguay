library(tidyverse)
library(httr)
library(stringr)




# Función que crea las tablas
creador_tablas <- function(base){
  
  tasas_por_sexo_df              <- tasas_por_sexo(base)
  tasas_por_sexo_edad_df         <- tasas_por_sexo_edad(base)
  brecha_ITI_df                  <- brecha_ITI(base)
  brecha_IOP_df                  <- brecha_IOP(base)
  


  lista <- list("tasas_por_sexo_df" = tasas_por_sexo_df,
                 "tasas_por_sexo_edad_df" = tasas_por_sexo_edad_df,
                 "brecha_ITI_df" = brecha_ITI_df,
                "brecha_IOP_df" = brecha_IOP_df

  )
  
  return(lista)
  
}


tabla_resultados <- creador_tablas(bases)

#Al final borro la base cruda y las funciones que ya usé

rm(list=names(Filter(is.function, mget(ls(all=T)))))
rm(bases)

#un par de ajustes mas

###nombres ramas (tildes y etc)


###reacomodos de tasas sexo para mostrar brechas y tasa de no registro


tabla_resultados[["tasas_por_sexo_df"]] <- bind_rows(tabla_resultados[["tasas_por_sexo_df"]], tabla_resultados$tasas_no_registro_df )

tabla_resultados[["tabla_brechas_tasas"]] <- tabla_resultados[["tasas_por_sexo_df"]] %>% 
  spread(.,key = "Sexo", value = "valor") %>% 
  mutate("Brecha (%)" = round(((Varones-Mujeres)/Varones)*100, 1))

tabla_resultados[["tasas_por_sexo_df"]] <- tabla_resultados[["tasas_por_sexo_df"]] %>% 
  left_join(.,tabla_resultados[["tabla_brechas_tasas"]], by =c("anio", "trim","indicador")) %>% 
  select(-Mujeres,-Varones)
