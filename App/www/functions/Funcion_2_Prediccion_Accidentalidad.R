## Función para predicción 
##a esta función se le indican los datos y la resolución para la que se quiere predecir

pred_function <- function(datos, resolucion_temp) {
  
  library(randomForest)
  
  resolucion_temp <- tolower(resolucion_temp)
  modelo_pred <- ifelse(resolucion_temp=="mes", 3, 
                        ifelse(resolucion_temp=="semana", 2, 1))
  
  if (modelo_pred == 1) {
    new_pred <- predict(modeloRF_dia_2, newdata=datos)
    new_pred <- ifelse(new_pred<=0, 0, new_pred)
    datos$PRED <- round(new_pred)
    salida = datos[,c("FECHA", "CLASE_TEXT", "PRED")]
    
  } else {
    
    if (modelo_pred == 2) {
      new_pred <- predict(modeloRF_sem_2, newdata=datos)
      new_pred <- ifelse(new_pred<=0, 0, new_pred)
      #new_pred <- abs(new_pred)
      datos$PRED <- round(new_pred)
      salida = datos[,c("SEMANA", "CLASE_TEXT", "PRED")]
      
    } else {
      
      new_pred <- predict(modeloRF_mes_2, newdata=datos)
      new_pred <- ifelse(new_pred<0, 0, new_pred)
      #new_pred <- abs(new_pred)
      datos$PRED <- round(new_pred)
      salida = datos[,c("MES", "CLASE_TEXT","PRED")]
    }
  }
  
  
  ## salida
  library(reshape2)
  library(dplyr)
  
  if (resolucion_temp == "dia") {
    predicciones <- dcast(salida, FECHA~CLASE_TEXT, value.var="PRED", fun.aggregate = sum)
  } else {
    if (resolucion_temp == "semana") {
      predicciones <- dcast(salida, SEMANA~CLASE_TEXT, value.var="PRED", fun.aggregate = sum)
    } else {
      predicciones <- dcast(salida, MES~CLASE_TEXT, value.var="PRED", fun.aggregate = sum)
    }
  }
  
  predicciones
  
}

save(pred_function, file = "")