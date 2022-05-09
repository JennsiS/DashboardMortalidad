library(ggplot2)
library(dplyr)
library(maps)
library(plotly)
library(hrbrthemes)
library(rjson)
library(DT)
library(leaflet)
library(sf)
library(RColorBrewer)
library(lubridate)
library(stringr)
library(writexl)
library(shinydashboard)
library(shinydashboardPlus)
library(readxl)
library(shiny)
library(stats)
library(graphics)
library(utils)
library(datasets)
load(file = "preloadData.RData")

# # Package names
# packages <- c("ggplot2", "readxl", "dplyr", "tidyr", "DT", "lubridate", "maps", 
#               "plotly", "hrbrthemes", "rjson", "leaflet", "sf", "RColorBrewer",
#               "stringr", "writexl", "shinydashboard", "shinydashboardPlus", "shiny",
#               "stats", "graphics", "utils", "datasets", "methods", "base", "grDevices")
# 
# # Install packages not yet installed
# installed_packages <- packages %in% rownames(installed.packages())
# if (any(installed_packages == FALSE)) {
#   install.packages(packages[!installed_packages])
# }
# 
# # Packages loading
# invisible(lapply(packages, library, character.only = TRUE))
# 
# #getwd()
# 
# #setwd(dirname(rstudioapi::getSourceEditorContext()$path))
# all_data <- read.csv(file = 'all_data.csv')
# poblaciones <- read_excel( "PoblacionesINE.xlsx")




####################################UI#########################################

ui <- dashboardPage( title="Tablero de Mortalidad Guatemala",
                     dashboardHeader(title = HTML("Tablero de Mortalidad de Guatemala"), 
                                     disable = FALSE, 
                                     titleWidth  = 550),
                     dashboardSidebar(
                       sidebarMenu(
                         style = "position: relative; overflow: visible;",
                         menuItem("Principal", tabName = "principal"),
                         menuItem("Fechas", tabName = "fechas"),
                         menuItem("Edad, Sexo y Causas", tabName = "categorias"),
                         menuItem("Localización", tabName = "localizacion"),
                         menuItem("Datos", tabName = "datos"),
                         menuItem(h4("Años seleccionados: ")),
                         menuItem(checkboxInput("todos","Todos/Ninguno",value=TRUE)),
                         menuItem(
                           checkboxGroupInput("years", "", years_options,
                                              selected =  c("2015",
                                                            "2016",
                                                            "2017",
                                                            "2018",
                                                            "2019",
                                                            "2020",
                                                            "2021"
                                              ))
                         )
                       )
                     ),
                     dashboardBody(
                       tags$head(tags$style(HTML('
            @import url("https://fonts.googleapis.com/css2?family=Catamaran:wght@100;300&display=swap");
            body{ 
              font-family: "Catamaran", sans-serif !important;
              
            }
            
            h3 {
            	font-family: "Catamaran", sans-serif !important;
            }
            h4{
              font-family: "Catamaran", sans-serif !important;
            }
        '))),
                       div(""),
                       tabItems(
                         tabItem("principal",
                                 h3("Mortalidad Guatemala 2015-2021"),
                                 p('Según la Organización de las Naciones Unidas (ONU) la mortalidad es: "Es la desaparición permanente de todo signo de vida, cualquiera que sea el tiempo transcurrido desde el nacimiento con vida"'),
                                 p("Este tablero resume las estadistícas de mortalidad de Guatemala, 
                      comprendiendo los registros de defunciones desde el Year 2015 hasta el 2021
                      obtenidos en las bases de datos de RENAP."),
                                 p("Este tablero ha sido desarrollado como un producto del proyecto 
                    “PHR-MODELS-C19“ como una iniciativa para fortalecer la disponibilidad de la información para Guatemala."),
                                 p("En el menú desplazable del lado izquierdo se encuentra organizado por pestañas según diferentes categorías las estadísticas más 
                      relevantes en cuanto a mortalidad, presentadas de forma gráfica y acompañadas de un cuadro de datos que puede ser descargado."),
                                 
                                 br(),  
                                 fluidRow(
                                   # valueBox(nrow(all_data), "Defunciones registradas", icon = icon("fa-regular fa-display-medical")),
                                   valueBox(nrow(all_data), "Defunciones registradas"),
                                   valueBox(format(round(tasa_mortalidad, 2), nsmall = 2), "Tasa bruta de mortalidad", icon = icon("percent")),
                                   valueBox(
                                     tags$p(head(causas$Causas,1), style = "font-size: 80%;"), "Causa de muerte más frecuente")
                                 ),
                                 
                         ),
                         tabItem("fechas",
                                 h3("Mortalidad por años y semanas epidemiológicas"),
                                 fluidRow(
                                   column(width = 12, offset=0.5,
                                          tabBox(
                                            selected = "Mortalidad por semanas epidemiologicas", height = "100%", width = "50%",
                                            tabPanel("Mortalidad por semanas epidemiologicas",  plotlyOutput(outputId = "mortalidad_fecha")),
                                            tabPanel("Datos", dataTableOutput("dataMortalidadFecha"))
                                          )
                                   )
                                 ),
                                 h3("Causas de mortalidad más frecuentes por años y semanas epidemiológicas"),
                                 fluidRow(
                                   column(width = 12, offset=0.5,
                                          tabBox(
                                            selected = "Causas de mortalidad mas frecuentes en el tiempo", side = "left", height = "100%", width = "50%",
                                            tabPanel("Causas de mortalidad mas frecuentes en el tiempo",  plotlyOutput(outputId = "mortalidad_causas_fecha")),
                                            tabPanel("Datos", dataTableOutput("dataCausasFecha"))
                                          )
                                   )
                                 )
                         ),
                         tabItem("categorias",
                                 fluidRow(h3("Mortalidad por grupo etario y sexo")),
                                 fluidRow(
                                   column(width = 8, offset = 1,
                                          tabBox(
                                            selected = "Mortalidad por Grupo etario y Sexo", height = "100%", width = "50%",
                                            tabPanel("Mortalidad por Grupo etario y Sexo", plotlyOutput(outputId = "mortalidad_edad")),
                                            tabPanel("Datos", dataTableOutput("dataEdadSexo"))
                                          )
                                   )
                                 ),
                                 fluidRow(h3("Cantidad de defunciones por causas más frecuentes de mortalidad")),
                                 fluidRow(
                                   column(width = 8, offset = 1,
                                          tabBox(
                                            selected = "Causas de mortalidad mas frecuentes", side = "left", height = "100%", width = "50%",
                                            tabPanel("Causas de mortalidad mas frecuentes", plotlyOutput(outputId = "mortalidad_causas")),
                                            tabPanel("Datos", dataTableOutput("dataCausas"))
                                          )
                                   )
                                 )
                         ),
                         tabItem("localizacion",
                                 fluidRow(h3("Mortalidad por departamentos")),
                                 fluidRow(
                                   column(width = 12,offset = 0.5,
                                          tabBox(
                                            selected = "Tasa de mortalidad por departamento", side = "left", height = "100%", width = "50%",
                                            tabPanel("Tasa de mortalidad por departamento", leafletOutput(outputId = "mortalidad_mapa")),
                                            tabPanel("Datos", dataTableOutput("dataDepartamentos"))
                                          )
                                   )
                                   
                                 ),
                                 fluidRow(
                                   column(width= 8, offset = 2,
                                          p("Este mapa describe la tasa de mortalidad cruda por Year. Como denominador, se ha utilizado la
                              población proyectada por el Instituto Nacional de Estadística (INE) para cada Year por
                              departamento. Las tasas se expresan por cada 100,000 habitantes.")
                                   )
                                 )
                                 
                         ),
                         tabItem("datos",
                                 fluidRow(
                                   column(width = 4, offset = 0.5,
                                          downloadButton('DescargarCsv',"CSV"), downloadButton('DescargarXlsx',"Excel")),
                                 ),
                                 br(),
                                 dataTableOutput("dataTable")
                         )
                       )
                     ) # body
) #dashboard page



##################################SERVER#########################################  

server <- function(input, output, session) {
  
  
  muertes_fecha_plot <- reactive(subset(muertes_fecha, muertes_fecha$Year %in% input$years))
  causas_fecha_plot <- reactive(subset(causas_fecha, causas_fecha$Year %in% input$years))
  
  observe({
    updateCheckboxGroupInput(
      session, "years", choices=years_options,
      selected = if(input$todos) years_options
    )
  })
  
  output$mortalidad_fecha <- renderPlotly({
    #plot_ly(muertes_fecha, x = ~Semana_epidemiologica, y = ~n, type = 'scatter', mode = 'lines', colors ='#00A6A6') %>%
    plot_ly(muertes_fecha_plot(), x = ~Semana_epidemiologica, y = ~n, type = 'scatter', mode = 'lines', colors ='#00A6A6') %>%
      config(displaylogo = FALSE) %>%
      layout(xaxis= list(title = "Semana epidemiológica"),
             yaxis= list(title = "Cantidad de muertes"))
    
  })
  
  output$mortalidad_causas_fecha <- renderPlotly({
    plot_ly(causas_fecha_plot(), x = ~Semana_epidemiologica, y = ~n, color = ~Causa_1, type = 'scatter', mode = 'line', colors = "Set2") %>% 
      config(displaylogo = FALSE)%>%
      layout(xaxis = list(title = 'Semana epidemiológica'),
             yaxis = list(title = 'Cantidad de muertes'),
             legend = list(title=list(text='<b> Causas de muerte </b>')))
  })
  
  
  
  output$mortalidad_edad <- renderPlotly({
    plot_ly(mujeres, x = mujeres$Grupo_etario, y= ~n, type = 'bar', name = "Femenino", marker=list(color ='#FFAAA7'))%>%
      add_trace(hombres,x = hombres$Grupo_etario, y = hombres$n, name = "Masculino", marker=list(color ='#98DDCA')) %>%
      layout(xaxis = list(title = 'Grupos etarios'),
             yaxis = list(title = 'Cantidad de muertes'),
             barmode = 'stack',
             plot_bgcolor='#e5ecf6',
             xaxis = list(
               zerolinecolor = '#ffff',
               zerolinewidth = 2,
               gridcolor = 'ffff'),
             yaxis = list(
               zerolinecolor = '#ffff',
               zerolinewidth = 2,
               gridcolor = 'ffff'))
  })
  
  output$mortalidad_mapa <- renderLeaflet({
    leaflet(mortalidad_dep) %>%
      setView(-94, 39, 5) %>%
      addProviderTiles("MapBox", options = providerTileOptions(
        id = "mapbox.light",
        accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>%
      addPolygons(
        fillColor = ~pal(TASA),
        weight = 1,
        opacity = 1,
        color = "white", #linea contorno
        dashArray = "3",
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 5,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE),
        label = dep_labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto")
      ) %>%
      addLegend(pal = pal, values = ~TASA, opacity = 0.7, title = "Cantidad de muertes",# titulo leyenda
                position = "bottomright", group = "Departamentos")%>% 
      setView(lat=16,lng=-90.522713, zoom = 7) %>% 
      addProviderTiles(providers$CartoDB.Positron)
    
  })
  
  output$mortalidad_causas <- renderPlotly({
    plot_ly(causas, x = ~n, y = ~Causas, type = 'bar', orientation = 'h', marker=list(color ='#FE6D73')) %>%
      config(displaylogo = FALSE)%>%
      layout(xaxis = list(title = 'Cantidad de muertes'),
             yaxis = list(title = 'Causas de muerte'))
  })
  
  output$dataTable <- renderDataTable({
    datatable(allDataTable)
  })
  
  output$DescargarCsv <- downloadHandler(
    filename = function() {
      paste("datos_mortalidad-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(allDataTable, file)
    }
  )
  
  output$DescargarXlsx <- downloadHandler(
    filename = function() {
      paste("datos_mortalidad-", Sys.Date(), ".xlsx", sep="")
    },
    content = function(file) {
      write_xlsx(allDataTable, path = file)
    }
  )
  
  output$dataEdadSexo <- renderDataTable({
    datatable(grupos_etarios,
              extensions = 'Buttons', options = list(
                dom = 'Bfrtip',
                buttons = 
                  list('copy', 'print', list(
                    extend = 'collection',
                    buttons = c('csv', 'excel', 'pdf'),
                    text = 'Download'
                  ))
                
              )
    )
  })
  
  output$dataCausas <- renderDataTable({
    datatable(causas,
              extensions = 'Buttons', options = list(
                dom = 'Bfrtip',
                buttons = 
                  list('copy', 'print', list(
                    extend = 'collection',
                    buttons = c('csv', 'excel', 'pdf'),
                    text = 'Download'
                  ))
                
              )
    )
  })
  
  output$dataCausasFecha <- renderDataTable({
    datatable(causas_fecha,
              extensions = 'Buttons', options = list(
                dom = 'Bfrtip',
                buttons = 
                  list('copy', 'print', list(
                    extend = 'collection',
                    buttons = c('csv', 'excel', 'pdf'),
                    text = 'Download'
                  ))
                
              )
    )
  })
  
  output$dataMortalidadFecha <- renderDataTable({
    datatable(muertes_fecha,
              extensions = 'Buttons', options = list(
                dom = 'Bfrtip',
                buttons = 
                  list('copy', 'print', list(
                    extend = 'collection',
                    buttons = c('csv', 'excel', 'pdf'),
                    text = 'Download'
                  ))
                
              )
    )
  })
  
  output$dataDepartamentos <- renderDataTable({
    datatable(muertes_departamentos,
              extensions = 'Buttons', options = list(
                dom = 'Bfrtip',
                buttons = 
                  list('copy', 'print', list(
                    extend = 'collection',
                    buttons = c('csv', 'excel', 'pdf'),
                    text = 'Download'
                  ))
                
              )
    )
  })
  
  
} # server


# Create Shiny object
shinyApp(ui = ui, server = server)
