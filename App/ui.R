library(shinydashboard)
library(dplyr)
library(leaflet)
library(leaflet.extras)
library(shinycssloaders)
library(DT)
library(shinyjs)
library(stringi)
library(lubridate)
library(plyr)
library(shiny)
library(tidyverse)
library(rvest)
library(htmltools)
library(randomForest)
library(reshape2)
library(sf)

shinyUI(fluidPage(
    includeCSS("www/style.css"),
    dashboardPage(
        skin = "blue",

        
        dashboardHeader(title="Accidentalidad Medellin", titleWidth = 300),
        dashboardSidebar(width = 300,
            sidebarMenu(
                HTML(paste0(
                    "<br>",
                    "<a target='_blank'><img style = 'display: block; margin-left: auto; margin-right: auto;' src='LogoAPP_Predictiva.svg' width = '220'></a>",
                    "<br>"
                )),
                menuItem("Inicio", tabName = "inicio", icon = icon("home")),
                menuItem("Mapa Accidentes", tabName = "mapa", icon = icon("map marked alt")),
                menuItem("Tabla de Datos", tabName = "tabla", icon = icon("table")),
                menuItem("Modelo Predictivo", tabName = "modelo", icon = icon("random", lib = "glyphicon")),
                menuItem("Agrupamiento", tabName = "agrupamiento", icon = icon("map marked alt")),
                menuItem("Informe Tecnico", tabName = "informe", icon = icon("stats", lib = "glyphicon")),
                menuItem("Creditos", tabName = "creditos", icon = icon("tasks")),
                HTML(paste0(
                    "<br>",
                    "<table style='margin-left:auto; margin-right:auto;'>",
                    "<tr>",
                    "<td style='padding: 7px;'><a href='https://github.com/jarealess/Trabajo_Final-Analitica_Predictiva' target='_blank'><i class='fa fa-github'></i></a></td>",
                    "</tr>",
                    "</table>",
                    " "))

            )),
        dashboardBody(         
            tabItems(
                tabItem(tabName = "inicio",
                        includeMarkdown("www/inicio.html")
                        ),
                
                tabItem(tabName = "mapa",
                        leafletOutput("mapa") %>% withSpinner(color = "#3C8DBC", type = 1),
                        
                        absolutePanel(id = "control_filtros", class = "panel panel-default", fixed = TRUE,
                                      draggable = TRUE, top = 90, left = "auto", right = 700, bottom = "auto",
                                      width = 400, height = "auto",
                                      
                                      h3("CONTROL DE FILTROS"),
                                      
                                      dateRangeInput("select_rango_mapa",
                                                     "RANGO DE FECHAS:",
                                                     language = 'es', 
                                                     weekstart = 1,
                                                     startview = 'year',
                                                     start  = min(df$FECHA),
                                                     end    = max(df$FECHA)
                                      ),
                                      
                                      selectInput("select_clase",
                                                  "CLASE:",
                                                  seleccion_clase, multiple = FALSE,
                                                  selected = "TODOS"),

                                      selectInput("select_comuna",
                                                  "COMUNA:",
                                                  seleccion_comunas, selected = "--"
                                                  ),
                                      conditionalPanel("input.select_comuna != '--'",
                                                       # Solo muestra el selector barrio si hay una comuna seleccionada
                                                       uiOutput("barriosOutput")
                                                       )

                                      )
                        ),
                
                tabItem(
                    tabName = "tabla",
                    h1("Visualizacion de los datos utilizados"),
                    
                    fluidRow(
                        column(6, offset = 1,
                               selectInput("tabla_clase",
                                           "CLASE:",
                                           c("TODOS", unique(df$CLASE))
                                           
                               )
                        ),
                        
                        column(4,
                               dateRangeInput("tabla_rango",
                                           "RANGO DE FECHAS:",
                                           language = 'es', 
                                           weekstart = 1,
                                           format = "dd/mm/yy",
                                           startview = 'year',
                                           start  = min(df$FECHA),
                                           end    = max(df$FECHA))
                               )
                        

                        ),
                        
                    DT::dataTableOutput("tabla_datos") %>% withSpinner(color = "#3C8DBC",
                                                                       type = 1)
                    
                ),

                tabItem(tabName = "modelo",
                        
                        h1("Modelo Predictivo"),
                        fluidRow(
                            column(6, offset = 1,
                                   dateRangeInput("select_rango_prediccion",
                                                  "RANGO DE FECHAS:",
                                                  language = 'es',
                                                  weekstart = 1,
                                                  startview = 'year',
                                                  start  = "2019-01-01",
                                                  end    = "2019-01-31")),
                            column(4,
                                   selectInput("select_pre", label = "REALIZAR PREDICCIONES", 
                                               choices = list("--", "DIA", "SEMANA", "MES"), 
                                               selected = "--"))
                                   
                                   ),
                        
                        h2("RESULTADOS"),
                        
                        #if(input$select_pre != "--"){
                            DT::dataTableOutput("t_prediccion") %>% withSpinner(color = "#3C8DBC",
                                                                                type = 1)
                        #}
                        

                     
                        ),
                
                tabItem(tabName = "agrupamiento",
                        
                        h1("Agrupamiento de la Accidentalidad por Barrios"),
                        
                        fluidRow(
                            column(6,
                                   h3("Visualizacion del agrupamiento en mapa"),
                                   leafletOutput("mapa_agrupamiento") %>% withSpinner(color = "#3C8DBC", type = 1)
                                   
                                   ),
                            column(6,
                                   h3("Resultados"),
                                   includeHTML("www/clustering.html"))
                                   )
                        
                      #  includeHTML("www/background.html")
                        ),
                
                
                tabItem(tabName = "informe",
                        includeHTML("www/informe.html")
                        
                        
                        
                        
                ),
                tabItem("creditos",
                         includeHTML("www/creditos.html"))
                            
                ))
        )))