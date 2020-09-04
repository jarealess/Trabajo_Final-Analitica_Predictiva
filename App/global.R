# - Lectura de archivos de datos
load("www/data/df_clean.RData")
seleccion_comunas <- c(as.vector(unique(df$COMUNA)), "--")
seleccion_clase <- c(as.character(unique(df$CLASE)), "TODOS")
df_tabla <- df
load("www/data/clustering.RData")
load("www/data/accidentalidad.RData")
load("www/data/festivos.RData")
load("www/models/modeloRF_dia_2.rda")
load("www/models/modeloRF_sem_2.rda")
load("www/models/modeloRF_mes_2.rda")
load("www/functions/acc_funtion.RData")
load("www/functions/pred_function.RData")


