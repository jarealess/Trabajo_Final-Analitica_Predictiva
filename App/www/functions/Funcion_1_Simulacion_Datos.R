### Función para predecir accidentalidad para un intervalo de tiempo
### La función toma el intervalo introducido y crea un data frame con las columnas
### FECHA, MES, SEMANA, DIA, COMUNA, BARRIO, CLASE y FECHAS_ESP
### Necesarias para predecir la accidentalidad por mes, semana o día


### se debe introducir ventana de tiempo en formato año/mes/dia
### resolución temporal: mes, semana, dia

acc_function <- function(Fecha_inicial, Fecha_final, resolucion_temp) {
  
  
  ### Librerías necesarias  
  library(stringi)
  library(lubridate)
  library(dplyr)
  library(plyr)
  #set.seed(12345)
  
  ### función que recibe fecha como entrada y la generaliza
  resolucion_temp <- tolower(resolucion_temp)
  freq_date = ifelse(resolucion_temp=="mes", "month", 
                     ifelse(resolucion_temp=="semana", "week", "day"))
  
  M <- seq(as.Date(Fecha_inicial), as.Date(Fecha_final), by=freq_date) 
  
  ## copiamos data requerida de dataframe df
  df2 <- select(accidentalidad, COMUNA, BARRIO, CLASE)
  
  ## se distribuyen las comunas y barrios para cada dia del rango
  ## Para esto toca hacer un proceso iterativo ya que los barrios
  ## también son diferentes para cada comuna
  ## de igual forma se distribuyen las clases de accidente de acuerdo a la
  ## proporción de accidentes de cada comuna
  
  data_for_prediction <- data.frame()
  h = 0  ## contador
  ah=0   ## contador
  
  for (i in 1:(length(unique(df2$COMUNA)))) {
    
    ## filtro comuna i 
    comuna_filter <- subset(df2, COMUNA==unique(df2$COMUNA)[i])
    
    ## rango de fechas
    dates_range <- sort(rep(M, length(unique(comuna_filter$BARRIO))))
    
    
    ## filtro de barrios
    prob_bar <- table(comuna_filter$BARRIO)/length(comuna_filter$BARRIO) ## probabilidad accidentalidad barrio
    
    barrio0 <- sample(sort(unique(comuna_filter$BARRIO)), 
                      length(dates_range), replace = TRUE,
                      prob = prob_bar)
    
    
    
    ## filtro para clase
    prob_clas <- table(comuna_filter$CLASE)/length(comuna_filter$CLASE)  ## probabilidad de cada clase
    prob_clas <- prob_clas[which(prob_clas>0)]
    clases_acc <- sample(sort(unique(comuna_filter$CLASE)), length(dates_range), 
                         replace=TRUE, prob = prob_clas) ## se asignan las clases con su probabilidad
    
    for (j in 1:length(dates_range)) {
      data_for_prediction[j+h, "FECHA"] <- dates_range[j]
      data_for_prediction[j+h,"COMUNA"] <- comuna_filter$COMUNA[1]
      data_for_prediction[j+h,"BARRIO"] <- barrio0[j]
      data_for_prediction[j+h,"CLASE"] <- clases_acc[j]
      
      ah = ah+1
    }
    
    h = h+ah
    ah=0
    
    
  }
  
  ## BALANCEO DE LOS DATOS
  ### se debe hacer para que la dimensión de los conjuntos simulados para
  ### dias, mes, y año sea lo más cercana a la de los datos reales de accidentalidad
  ### por año, y la predicción no de valores muy desfasados.  
  
  n <- dim(data_for_prediction)[1]
  
  if (resolucion_temp=="dia") {
    frac <- sample(1:n, 0.33*n, replace=TRUE)
    data_for_prediction <- data_for_prediction[frac,]
    
  } else {
    if (resolucion_temp=="semana") {
      frac <- sample(1:n, 0.28*n, replace=TRUE)
      data_for_prediction1 <- data_for_prediction
      data_for_prediction2 <- data_for_prediction[frac,]
      data_for_prediction <- rbind(data_for_prediction1, data_for_prediction2)
      
    } else {
      frac <- sample(1:n, 0.32*n, replace=TRUE)
      frac1 <- sample(1:n, 1.73*n, replace=TRUE)
      
      data_for_prediction1 <- data_for_prediction
      data_for_prediction2 <- data_for_prediction[frac,]
      
      data_for_prediction3 <- subset(data_for_prediction, CLASE %in% c(1,2,4,5))
      data_for_prediction3 <- data_for_prediction3[frac1, ]
      
      
      data_for_prediction <- rbind(data_for_prediction1, 
                                   data_for_prediction2,
                                   data_for_prediction3)
    }
  }
  
  ## se ordena por fecha
  data_for_prediction <- subset(data_for_prediction, CLASE %in% c(1,2,3,4,5))
  data_for_prediction <- data_for_prediction[order(data_for_prediction$FECHA),]
  
  
  ############## PARTE 2 ####################################
  ## Se adecuan los datos
  
  ## nombre del día
  data_for_prediction$DIA_NOMBRE <- weekdays(data_for_prediction$FECHA)
  data_for_prediction$DIA_NOMBRE <- toupper(stri_trans_general(data_for_prediction$DIA_NOMBRE,"Latin-ASCII"))
  
  ## fechas especiales
  data_for_prediction$FECHAS_ESP1 <- ifelse(data_for_prediction$DIA_NOMBRE %in% c("SABADO","DOMINGO"), "NO LABORAL", "LABORAL")
  data_for_prediction$FECHAS_ESP <- ifelse(data_for_prediction$FECHA %in% festivos$FECHA, "FESTIVO", data_for_prediction$FECHAS_ESP1)
  
  ## dia, semana, mes, periodo
  data_for_prediction$DIA_ANNO <- yday(data_for_prediction$FECHA)
  data_for_prediction$SEMANA <- week(data_for_prediction$FECHA)
  data_for_prediction$MES <- month(data_for_prediction$FECHA)
  data_for_prediction$PERIODO <- year(data_for_prediction$FECHA)
  
  
  ## variables a factores
  data_for_prediction$MES <- as.factor(data_for_prediction$MES)
  
  ## niveles
  levels(data_for_prediction$FECHAS_ESP) <- levels(accidentalidad$FECHAS_ESP)
  levels(data_for_prediction$CLASE) <- levels(accidentalidad$CLASE)
  levels(data_for_prediction$COMUNA) <- levels(accidentalidad$COMUNA)
  levels(data_for_prediction$MES) <- levels(accidentalidad$MES)
  
  ## SEMANA a numéricas
  data_for_prediction$SEMANA <- as.numeric(data_for_prediction$SEMANA)
  
  ## nombre de las clases
  data_for_prediction$CLASE_TEXT <- ifelse(data_for_prediction$CLASE==1, "ATROPELLO",
                                           ifelse(data_for_prediction$CLASE==2, "CAIDA OCUPANTE",
                                                  ifelse(data_for_prediction$CLASE==3, "CHOQUE",
                                                         ifelse(data_for_prediction$CLASE==4, "OTRO",
                                                                "VOLCAMIENTO"))))
  
  ## data final
  data_for_prediction <- select(data_for_prediction, FECHA, PERIODO, MES, FECHAS_ESP, CLASE_TEXT,
                                SEMANA, DIA_ANNO, COMUNA, BARRIO, CLASE)
  
  data_for_prediction
  
}