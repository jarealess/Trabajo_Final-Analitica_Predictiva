library(shiny)
library(tidyverse)
library(leaflet.extras)
library(rvest)
library(htmltools)
library(stringi)
library(lubridate)
library(dplyr)
library(plyr)


# - Lectura de archivos 
df <- read.csv("www/df_limpio.csv")
accidentalidad <- read.csv("www/accidentalidad.csv")
festivos <- read.csv("www/festivos.csv")
df$FECHA <- as.Date(df$FECHA) 

load("www/modeloRF_dia_2.rda")
load("www/modeloRF_sem_2.rda")
load("www/modeloRF_mes_2.rda")

load("www/acc_funtion.RData")


###########################################################
#### - Funcion para simulacion de conjunto de datos - #####
###########################################################




#############################################################################################################


shinyServer(function(input, output, session) {
  
  df_tabla <- df
  
  ############################################
  # - Filtro para el barrio del mapa
  output$barriosOutput <- renderUI({
    selectInput("select_barrio", "BARRIO:",
                c(as.vector(unique(df_fil_comuna()$BARRIOS)), "--"), selected = "--")
  }) 
  ############################################
  
  
  # - legenda de la gravedad - ajuste de color
  levels_gravedad <- unique(df_tabla$GRAVEDAD)
  pal <- colorFactor(palette =c("blue", "orange", "red"), 
                     levels  = levels_gravedad)
  
  # - Filtro comunas y barrios
  df_fil_comuna <- reactive({
    df_tabla %>%
      filter(
        COMUNAS == input$select_comuna,
        FECHA >= input$select_rango_mapa[1],
        FECHA <= input$select_rango_mapa[2],
        CLASE %in% input$select_clase
      )
  })
  
  
  
  
  df_fil_barrio <- reactive({
    
    if (is.null(input$select_barrio)) {
      return (df_fil_comuna())
    }
    
    if (input$select_barrio == "--"){
      return (df_fil_comuna())
    }
    
    df_tabla %>%
      filter(
        COMUNAS == input$select_comuna,
        BARRIOS == input$select_barrio,
        FECHA >= input$select_rango_mapa[1],
        FECHA <= input$select_rango_mapa[2],
        CLASE %in% input$select_clase
      )
  })
  
  
  
  #Mapa - Inicial
  output$mapa <- renderLeaflet({
    leaflet() %>% 
      addProviderTiles(providers$CartoDB.Positron, group = "Positron", options = providerTileOptions(noWrap = TRUE)) %>%
      addFullscreenControl() %>%
      setView(lng = -75.567, lat = 6.217, zoom = 12)
  })
  
  
  
  # - Cambios en el mapa 1
  observe({
    
    
    if(input$select_comuna == "--"){
      leafletProxy("mapa", data = df_fil_barrio()) %>%
        clearShapes()%>%
        clearControls%>%
        setView(lng = -75.567, lat = 6.217, zoom = 12)
    }
    
    if(input$select_comuna != "--"){
      leafletProxy("mapa", data = df_fil_barrio()) %>%
        clearShapes()%>%
        clearControls%>%
        addCircles(~LONGITUD, ~LATITUD, color = ~pal(GRAVEDAD), popup = ~CLASE)%>%
        fitBounds(~min(LONGITUD), ~min(LATITUD), ~max(LONGITUD), ~max(LATITUD))%>%
        addLegend(position = "bottomright", pal = pal, values = levels_gravedad, title = "GRAVEDAD", opacity = 1)
    }
  })
  
  # FILTRO TABLA DE DATOS POR CLASE Y VENTANA DE TIEMPO
  output$tabla_datos <- DT::renderDataTable(DT::datatable({
    
    df_tabla <- df %>%
      filter(FECHA  >= input$tabla_rango[1] &
               FECHA <= input$tabla_rango[2])
    
    if (input$tabla_clase != "TODOS") {
      df_tabla <- df_tabla[df_tabla$CLASE == input$tabla_clase, ]
    }
    df_tabla
  }, options = list(searching = FALSE,
                    initComplete = JS(
                      "function(settings, json) {",
                      "$(this.api().table().header()).css({'background-color': '#3C8DBC', 'color': '#fff'});",
                      "}"),
                    language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')
  ))
  
  )
  
  
  prediccion <- reactive({
    acc_function(input$select_rango_prediccion[1], input$select_rango_prediccion[2], input$select_pre)
  })
  
 
  
   output$t_prediccion <- DT::renderDataTable(DT::datatable({
     prediccion()
    
  },options = list(searching = FALSE,
                   initComplete = JS(
                     "function(settings, json) {",
                     "$(this.api().table().header()).css({'background-color': '#3C8DBC', 'color': '#fff'});",
                     "}"),
                   language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')
  )))
  
  
  
  ####################################################################   
  
})
    