library(haven)
library(tidyverse)
library(srvyr)
library(readxl)
library(magrittr)

ech11 <- read_sav("C:/Users/tlapunov/Documents/ECH/ECH 2011/FUSIONADO_2011_TERCEROS.sav",col_select = c(dpto,anio,mes,pesoano,numero,nper,e26,e27,PT2,PT4,pobpcoac,HT11,HT19))
ech12 <- read_sav("C:/Users/tlapunov/Documents/ECH/ECH 2012/FUSIONADO_2012_TERCEROS.sav",col_select = c(dpto,anio,mes,pesoano,numero,nper,e26,e27,PT2,PT4,pobpcoac,HT11,ht19))
ech13 <- read_sav("C:/Users/tlapunov/Documents/ECH/ECH 2013/Fusionado_2013_Terceros.sav",col_select = c(dpto,anio,mes,pesoano,numero,nper,e26,e27,PT2,PT4,pobpcoac,HT11,ht19))
ech14 <- read_sav("C:/Users/tlapunov/Documents/ECH/ECH 2014/Fusionado_2014_Terceros.sav",col_select = c(dpto,anio,mes,pesoano,numero,nper,e26,e27,PT2,PT4,pobpcoac,HT11,ht19))
ech15 <- read_sav("C:/Users/tlapunov/Documents/ECH/ECH 2015/HyP_2015_Terceros.sav",col_select = c(dpto,anio,mes,pesoano,numero,nper,e26,e27,pt2,pt4,pobpcoac,ht11,ht19))
ech16 <- read_sav("C:/Users/tlapunov/Documents/ECH/ECH 2016/HyP_2016_Terceros.sav",col_select = c(dpto,anio,mes,pesoano,numero,nper,e26,e27,PT2,PT4,pobpcoac,ht11,ht19))
ech17 <- read_sav("C:/Users/tlapunov/Documents/ECH/ECH 2017/HyP_2017_Terceros.sav",col_select = c(dpto,anio,mes,pesoano,numero,nper,e26,e27,PT2,PT4,pobpcoac,ht11,ht19))
ech18 <- read_sav("C:/Users/tlapunov/Documents/ECH/ECH 2018/HyP_2018_Terceros.sav",col_select = c(dpto,anio,mes,pesoano,numero,nper,e26,e27,PT2,PT4,pobpcoac,ht11,ht19))
ech19 <- read_sav("C:/Users/tlapunov/Documents/ECH/ECH 2019/HyP_2019_Terceros.sav",col_select = c(dpto,anio,mes,pesoano,numero,nper,e26,e27,PT2,PT4,pobpcoac,HT11,ht19))

# paso a mayúsculas variables que me interesan en bases donde no lo están
ech15 <- ech15 %>% rename(PT2=pt2,
                          PT4=pt4)


# genero función que me cambia los nombres de ht11 y ht19 en caso de que estén en minúsculas
renombro <- function(base) {
  if ("ht11" %in% colnames(base)) {
    base <- base %>% rename(HT11=ht11)
  }
  if ("ht19" %in% colnames(base)) {
    base <- base %>% rename(HT19=ht19)
  }
  return(base)
}

# genero lista con todas las bases
lista <- mget(ls(pattern="ech"))

# genero vector de los nombres de las bases
nombres <- ls(pattern="ech")

# aplio la función a las bases
res <- lapply(lista,renombro)

# las reasigno en sus respectivos nombres
for (i in 1:length(nombres)) {
  assign(nombres[i], res[[i]])  
}

# uno las bases
bases <- bind_rows(mget(ls(pattern="ech")))

# elimino las individuales
rm(list=ls(pattern="ech"))

# genero variable de trimestre
bases %<>% mutate(trim = case_when(mes<4 ~ 1, 
                             mes %in% c(4,5,6) ~ 2,
                             mes %in% c(7,8,9) ~ 3,
                             mes>9 ~ 4))

# tomo la serie del IPC
ipc <- read_xlsx("preprocesamiento/Serie IPC.xlsx",sheet="serie_ipc")

# genero variable de trimestre y promedio de ipc por  trimestre
ipc %<>% mutate(trim=case_when(mes<4 ~ 1, 
                               mes %in% c(4,5,6) ~ 2,
                               mes %in% c(7,8,9) ~ 3,
                               mes>9 ~ 4)) 

ipc %<>% group_by(anio,trim) %>% mutate(ipc_prom_trim_mdeo = mean(coef_mdeo),
                                        ipc_prom_trim_int = mean(coef_int)) %>% 
  ungroup()


# le agrego columna de trim+1 y de anio+1 (ya que para el análisis de un determinado trim, debo tomar el trim del mes anterior)
ipc$trim_sig <- ifelse(ipc$trim==4,1,ipc$trim+1)
ipc$anio_trim_sig <- ifelse(ipc$trim_sig==1,ipc$anio+1,ipc$anio)

# solo agarro las variables que necesito
ipc_join <- ipc %>% 
  select(-c(anio,mes,trim,coef_int,coef_mdeo)) %>% 
  mutate(anio_trim_sig=as.character(anio_trim_sig)) %>% 
  rename(trim=trim_sig,anio=anio_trim_sig)

# le pego el ipc a la base
bases %<>% left_join(ipc_join) 

# genero un único ipc
bases %<>% 
  mutate(coef_ipc=ifelse(dpto==1, ipc_prom_trim_mdeo, ipc_prom_trim_int)) %>% 
  select(-c(ipc_prom_trim_mdeo, ipc_prom_trim_int)) 


# paso el ht11 a todos los miembros en caso de que no lo tuvieran
bases %<>% group_by(numero) %>% mutate(HT11 = max(HT11)) %>% ungroup()

# creo variables pc y constantes
bases %<>% mutate(ingPC = HT11/HT19,
                  ingPC_cons = ingPC/coef_ipc,
                  PT2_cons = PT2/coef_ipc,
                  PT4_cons = PT4/coef_ipc,
                  HT11_cons = HT11/coef_ipc)


# creo variable de grupos de edad, region y sexo
bases %<>% mutate(GRUPO_EDAD=case_when(e27 >= 14 & e27 <= 29 ~ "de 14 a 29 años",
                                       e27 >= 30 & e27 <= 64 ~ "de 30 a 64 años"),
                  Sexo=ifelse(e26==1,"Varones","Mujeres"),
                  REGION=ifelse(dpto==1,"Montevideo","Interior")) 


