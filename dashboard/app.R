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
library(shinyBS)
library(fontawesome)
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

valueBoxText <- paste("Defunciones registradas en el período ", years_options[1] , sep ="")
valueBoxText <- paste(valueBoxText, years_options[length(years_options)] , sep ="-")

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
                         menuItem("Ubicación", tabName = "ubicacion"),
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
                                 p('La mortalidad es: "Es la desaparición permanente de todo signo de vida, cualquiera que sea el tiempo transcurrido desde el nacimiento con vida"'),
                                 p("Este tablero resume las estadistícas de mortalidad de Guatemala, 
                      comprendiendo los registros de defunciones desde el Año 2015 hasta el 2021
                      obtenidos en las bases de datos del Registro Nacional de Personas de Guatemala (RENAP)."),
                                 p("Este tablero ha sido desarrollado como un producto del proyecto  PHR-MODELS-C19 del Centro de Estudios en Salud de la Universidad del Valle de Guatemala, a través de un acuerdo cooperativo con los Centros para la Prevención y Control de Enfermedades de Estados Unidos (US CDC), como una iniciativa para fortalecer la disponibilidad de información en salud pública para Guatemala."),
                                 p("El menú desplazable del lado izquierdo se encuentra organizado por pestañas según diferentes categorías de interés. Las pestañas presentan estadísticas  
                      relevantes en cuanto a mortalidad, presentadas de forma gráfica acompañadas de un cuadro de datos que puede descargarse oprimiendo el botón correspondiente en cada sección"),
                                 
                                 br(),  
                                 fluidRow(
                                   # valueBox(nrow(all_data), "Defunciones registradas", icon = icon("fa-regular fa-display-medical")),
                                   valueBox(format(nrow(all_data),big.mark=",",scientific=FALSE), valueBoxText, icon = icon("list")),
                                   popify(
                                    valueBox(format(round(tasa_mortalidad, 2), nsmall = 2, id='tasa', icon=icon("people")), "Tasa bruta de mortalidad por 1,000 habitantes"),
                                    "Cálculo tasa bruta de mortalidad", "El calculo de la tasa bruta de mortalidad se calcula con el total de defunciones registradas en los años registrados dividido por el total de la población correspondiente a estos mismos años y todo esto se multiplica por el amplificador que en este caso es 1,000 habitantes.", placement = "top", trigger = "hover"
                                    ),
                                    valueBox(
                                     tags$p(head(causas$Causas,1), style = "font-size: 80%;"), "Causa de muerte más frecuente", icon = icon("wave-pulse") )
                                 ),
                                 
                                 fluidRow(
                                   column(width=12,
                                         box(p(" El tablero se ha preparado utilizando datos disponibles del Registro Nacional de las Personas (RENAP). Dado el retraso en registro de algunos datos, los mismos pueden cambiar con frecuencia según se actualicen las bases de datos fuente."),
                                             width=12)
                                   )
                                 )
                                 
                                 
                         ),
                         tabItem("fechas",
                                 h3("Número de muertes totales por semana epidemiológica, Guatemala"),
                                 fluidRow(
                                   column(width = 12, offset=0.5,
                                          tabBox(
                                            selected = "Mortalidad por semanas epidemiologicas", height = "100%", width = "50%",
                                            tabPanel("Mortalidad por semanas epidemiologicas",  plotlyOutput(outputId = "mortalidad_fecha")),
                                            tabPanel("Datos", dataTableOutput("dataMortalidadFecha"))
                                          )
                                   )
                                 ),
                                 h3("Número de muertes totales de las causas de muerte más frecuentes por semana epidemiológica, Guatemala"),
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
                                 fluidRow(
                                   column(width =10,offset = 1,
                                          h3("Número de muertes totales por grupo etario y sexo, Guatemala,"))),
                                 fluidRow(
                                   column(width = 10, offset = 1,
                                          tabBox(
                                            selected = "Mortalidad por Grupo etario y Sexo", height = "100%", width = "50%",
                                            tabPanel("Mortalidad por Grupo etario y Sexo", plotlyOutput(outputId = "mortalidad_edad")),
                                            tabPanel("Datos", dataTableOutput("dataEdadSexo"))
                                          )
                                   )
                                 ),
                                 fluidRow(
                                   column(width =10,offset = 1,
                                          h3("Número de muertes totales por causas más frecuentes de mortalidad, Guatemala,"))),
                                 fluidRow(
                                   column(width = 10, offset = 1,
                                          tabBox(
                                            selected = "Causas de mortalidad mas frecuentes", side = "left", height = "100%", width = "50%",
                                            tabPanel("Causas de mortalidad mas frecuentes", plotlyOutput(outputId = "mortalidad_causas")),
                                            tabPanel("Datos", dataTableOutput("dataCausas"))
                                          )
                                   )
                                 ),
                                 fluidRow(
                                   column(width =10,offset = 1,
                                          h3("Tasa de mortalidad por sexo, Guatemala"))),
                                 fluidRow(
                                   column(width = 10, offset = 1,
                                          tabBox(
                                            selected = "Tasa de mortalidad por sexo", side = "left", height = "100%", width = "50%",
                                            tabPanel("Tasa de mortalidad por sexo", plotlyOutput(outputId = "mortalidad_sexo_tasa")),
                                          )
                                   )
                                 ),
                                 fluidRow(
                                   column(width =10,offset = 1,
                                          h3("Tasa mortalidad por grupo etario, Guatemala"))),
                                 fluidRow(
                                   column(width = 10, offset = 1,
                                          tabBox(
                                            selected = "Tasa de mortalidad por edad", side = "left", height = "100%", width = "50%",
                                            tabPanel("Tasa de mortalidad por edad", plotlyOutput(outputId = "mortalidad_edad_tasa")),
                                          )
                                   )
                                 )
                         ),
                         tabItem("ubicacion",
                                 fluidRow(
                                   column(width =10,offset = 1,
                                     h3("Tasa de mortalidad por departamentos, Guatemala"))
                                   ),
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
                                          p("Este mapa describe la tasa de mortalidad cruda por año. Como denominador, se ha utilizado la
                              población proyectada por el Instituto Nacional de Estadística (INE) para cada año por
                              departamento. Las tasas se expresan por cada 100,000 habitantes."),
                                          a("Referencia de poblaciones INE", href="https://www.ine.gob.gt/ine/proyecciones/")
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
  hombres_plot <- reactive({
        data_anios <- subset(all_data, year(all_data$FECHA) %in% input$years)
        hombres_data <- groupby_edad_sexo(data_anios,"h")

     })
  mujeres_plot <- reactive({
    data_anios <- subset(all_data, year(all_data$FECHA) %in% input$years)
    mujeres_data <- groupby_edad_sexo(data_anios,"m")

  })
  
  causas_plot <- reactive({
    data_anios <- subset(all_data, year(all_data$FECHA) %in% input$years)
    causas_data <- groupby_causas(data_anios)
  })
  
  grupos_etarios <- reactive({
    data_anios <- subset(all_data, year(all_data$FECHA) %in% input$years)
    grupos_edad <- groupby_edad_sexo(data_anios,"edad")
  })
  
  departamentos_plot <- reactive ({
    data_anios <- subset(all_data, year(all_data$FECHA) %in% input$years)
    departamentos_data <-groupby_departamento(data_anios,poblaciones, departamentos_locacion)
  })
  
  
  
  tasas_sexo_plot <- reactive ({
    data_anios <- subset(all_data, year(all_data$FECHA) %in% input$years)
    data_sexo_tasa <- groupby_sexo_tasa(data_anios,poblacionesSexo)
  })
  
  tasas_edad_plot <- reactive ({
    data_anios <- subset(all_data, year(all_data$FECHA) %in% input$years)
    data_edad_tasa <- groupby_edad_tasa(data_anios,poblacionesEdad)
  })
  
  
  observe({
    updateCheckboxGroupInput(
      session, "years", choices=years_options,
      selected = if(input$todos) years_options
    )
  })
  
  output$mortalidad_fecha <- renderPlotly({
    plot_ly(muertes_fecha_plot(), x = ~Semana_epidemiologica, y = ~n, type = 'scatter', mode = 'lines', colors ='#00A6A6') %>%
      config(displaylogo = FALSE) %>%
      layout(#title= paste("Número de muertes totales por semana epidemiológica, Guatemala, ", ~Year ),
             xaxis= list(title = "Año y semana epidemiológica"),
             yaxis= list(title = "Número de muertes"))
    
  })
  
  output$mortalidad_causas_fecha <- renderPlotly({
    plot_ly(causas_fecha_plot(), x = ~Semana_epidemiologica, y = ~n, color = ~Causa_1, type = 'scatter', mode = 'line', colors = "Set2") %>% 
      config(displaylogo = FALSE)%>%
      layout(
            xaxis = list(title = 'Año y semana epidemiológica'),
             yaxis = list(title = 'Número de muertes'),
             legend = list(title=list(text='<b> Causas de muerte </b>')))
  })
  
  
  
  output$mortalidad_edad <- renderPlotly({
    plot_ly(mujeres_plot(), x = ~Grupo_etario, y= ~n, type = 'bar', name = "Femenino", marker=list(color ='#FFAAA7'))%>%
      add_trace(hombres_plot(),x = ~Grupo_etario, y = ~n, name = "Masculino", marker=list(color ='#98DDCA')) %>%
      layout(xaxis = list(title = 'Grupos etarios'),
             yaxis = list(title = 'Número de muertes'),
             barmode = 'stack',
             plot_bgcolor='#e5ecf6',
             xaxis = list(
               zerolinecolor = '#ffff',
               zerolinewidth = 2,
               gridcolor = 'ffff',
               categoryorder = "array",
               categoryarray = ~n
               ),
             yaxis = list(
               zerolinecolor = '#ffff',
               zerolinewidth = 2,
               gridcolor = 'ffff'
               )
             )
  })
  
  output$mortalidad_sexo_tasa <- renderPlotly({
    plot_ly(tasas_sexo_plot(),
            x = ~SEXO,
            y = ~TASA,
            type = "bar",
            color = ~SEXO
    ) %>%
      layout(xaxis = list(title = 'Sexo'),
             yaxis = list(title = 'Tasa de mortalidad por 1,000 habitantes'),
             plot_bgcolor='#e5ecf6',
             xaxis = list(
               zerolinecolor = '#ffff',
               zerolinewidth = 2,
               gridcolor = 'ffff'
             ),
             yaxis = list(
               zerolinecolor = '#ffff',
               zerolinewidth = 2,
               gridcolor = 'ffff'
             )
      )
  })
  
  output$mortalidad_edad_tasa <- renderPlotly({
    plot_ly(tasas_edad_plot(),
            x = ~Grupo_etario,
            y = ~TASA,
            type = "bar",
            color = "#7D6B91"
            #color = as.numeric(as.character(~TASA))
    ) %>%
      layout(xaxis = list(title = 'Grupo etario'),
             yaxis = list(title = 'Tasa de mortalidad por 1,000 habitantes'),
             plot_bgcolor='#e5ecf6',
             xaxis = list(
               zerolinecolor = '#ffff',
               zerolinewidth = 2,
               gridcolor = 'ffff'
             ),
             yaxis = list(
               zerolinecolor = '#ffff',
               zerolinewidth = 2,
               gridcolor = 'ffff'
             )
      )
  })
  
  output$mortalidad_mapa <- renderLeaflet({
    mapa_data <- departamentos_plot()
    pal <- colorNumeric( colorRampPalette(brewer.pal(8,"Reds"))(5), domain = c(0,max(mapa_data$TASA)))
    dep_labels <-sprintf(
      "<strong>%s</strong><br/>Tasa de mortalidad por 1,000 habitantes: %g",
      mapa_data$nombre, mapa_data$TASA
    ) %>% lapply(htmltools::HTML)
    
    
    leaflet(mapa_data) %>%
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
      addLegend(pal = pal, values = ~TASA, opacity = 0.7, title = "Tasa",# titulo leyenda
                position = "bottomright", group = "Departamentos")%>% 
      setView(lat=16,lng=-90.522713, zoom = 7) %>% 
      addProviderTiles(providers$CartoDB.Positron)
    
  })
  
  output$mortalidad_causas <- renderPlotly({
    plot_ly(causas_plot(), x = ~n, y = ~Causas, type = 'bar', orientation = 'h', marker=list(color ='#FE6D73')) %>%
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
    datatable(grupos_etarios(),
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
    datatable(causas_plot(),
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
    datatable(causas_fecha_plot(),
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
    datatable(muertes_fecha_plot(),
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
    departamentos_table_data <- departamentos_plot()
    names (departamentos_table_data)[1] = "DEPARTAMENTO"
    names (departamentos_table_data)[11] = "POBLACION_TOTAL"
    departamentos_table_data <- departamentos_table_data[c("DEPARTAMENTO", "POBLACION_TOTAL", "DEFUNCIONES")]
    departamentos_table_data$geometry <- NULL
    datatable(departamentos_table_data,
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
