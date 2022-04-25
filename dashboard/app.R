library(ggplot2)
library(dplyr)
library(maps)
library(plotly)
library(hrbrthemes)
library(rjson)
library(viridis)
library(rgdal)
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

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
all_data <- read.csv(file = 'all_data.csv')
poblaciones <- read_excel( "PoblacionesINE.xlsx")



###################DATA######################


p_2015<- sum(poblaciones$`2015`)
p_2016<- sum(poblaciones$`2016`)
p_2017<- sum(poblaciones$`2017`)
p_2018<- sum(poblaciones$`2018`)
p_2019<- sum(poblaciones$`2019`)
p_2020<- sum(poblaciones$`2020`)
p_2021<- sum(poblaciones$`2021`)
p_total <- p_2015 + p_2016 + p_2017 + p_2018 + p_2019 + p_2020 + p_2021
tasa_mortalidad <- (nrow(all_data)/p_total) * 1000


#Muertes por semana epidemiologica
muertes_fecha <- all_data
muertes_fecha$epiWeek <- epiweek(muertes_fecha$FECHA)
muertes_fecha$year <- year(muertes_fecha$FECHA)
muertes_fecha$fullDate <- str_c(muertes_fecha$year,"-",muertes_fecha$epiWeek)
muertes_fecha$n <- 1
muertes_fecha <- muertes_fecha %>% group_by(fullDate) %>% summarise(n=sum(n))
names (muertes_fecha)[1] = "Semana_epidemiologica"
muertes_fecha$Año <- sub("\\-.*", "", muertes_fecha$Semana_epidemiologica)
# muertes_fecha$Año <- sub(".*-\\.*", "", muertes_fecha$Semana_epidemiologica)
#subset(muertes_fecha, muertes_fecha$Año %in% c("2021"))


#Agrupacion de causas de muerte
causas <- all_data
causas$n <- 1
causas <- causas %>% group_by(CAUSA1) %>% summarise(n=sum(n))
causas <- subset(causas) %>% filter(CAUSA1!="DESCONOCIDA")

causas <- head(arrange(causas,desc(n)), n = 10)
names (causas)[1] = "Causas"

causas_fecha <- all_data
causas_fecha$n <- 1
causas_fecha <- causas_fecha %>% group_by(CAUSA1, FECHA) %>%summarise(n=sum(n))
causas_fecha <- subset(causas_fecha, CAUSA1 %in% causas$Causas)
causas_fecha$epiweek <- epiweek(causas_fecha$FECHA)
causas_fecha$year <- year(causas_fecha$FECHA)
causas_fecha$fullDate <- str_c(causas_fecha$year,"-",causas_fecha$epiweek)
causas_fecha <-causas_fecha %>% group_by(fullDate, CAUSA1) %>% summarise(n=sum(n))
names (causas_fecha)[1] = "Semana_epidemiologica"
names (causas_fecha)[2] = "Causa_1"
causas_fecha$Año <- sub("\\-.*", "", causas_fecha$Semana_epidemiologica)



#Agrupacion por grupo etario y sexo 
grupos_etarios <- all_data %>%
  mutate(GRUPO_ETARIO = case_when(
    between(EDAD,0,9) ~ "0-9",
    between(EDAD,10,19) ~ "10-19",
    between(EDAD,20,29) ~ "20-29",
    between(EDAD,30,39) ~ "30-39",
    between(EDAD,40,49) ~ "40-49",
    between(EDAD,50,59) ~ "50-59",
    between(EDAD,60,69) ~ "60-69",
    between(EDAD,70,79) ~ "70-79",
    between(EDAD,80,89) ~ "80-89",
    between(EDAD,90,99) ~ "90-99",
    between(EDAD,100,109) ~ "100-109",
    between(EDAD,110,119) ~ "110-119",
    between(EDAD,120,150) ~ "120+",
    EDAD == "DESCONOCIDA" ~ "DESCONOCIDA"
  ))

grupos_etarios$n <- 1
grupos_etarios$Año <- year(grupos_etarios$FECHA)
grupos_etarios <- grupos_etarios %>% group_by(GRUPO_ETARIO, SEXO) %>% summarise(n=sum(n))
grupos_etarios$GRUPO_ETARIO <- factor(grupos_etarios$GRUPO_ETARIO, levels = 
                                      c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69",
                                        "70-79", "80-89", "90-99", "100-109", "110-119", "120+", "DESCONOCIDA"))


grupos_etarios <- subset(grupos_etarios) %>% filter(GRUPO_ETARIO!="DESCONOCIDA")
mujeres <- grupos_etarios%>% filter(SEXO=="F")
hombres <- grupos_etarios%>% filter(SEXO=="M")
names(grupos_etarios)[1] = "Grupo_etario"
names(grupos_etarios)[2] = "Sexo"
names (mujeres)[1] = "Grupo_etario"
names (mujeres)[2] = "Sexo"
names (hombres)[1] = "Grupo_etario"
names (hombres)[2] = "Sexo"

#desconocidos <- grupos_etarios%>% filter(SEXO=="DESCONOCIDO")
colors_grapo <- c('#348AA7', '#525174', '#5DD39E','#BCE784')

#Agrupacion por departamento

muertes_departamentos <- all_data
muertes_departamentos$DEFUNCIONES <- 1
muertes_departamentos <- muertes_departamentos %>% group_by(DEPARTAMENTO) %>% summarise(DEFUNCIONES=sum(DEFUNCIONES))
muertes_departamentos$DEPARTAMENTO[muertes_departamentos$DEPARTAMENTO == "SOLOLÁ"] ="SOLOLA"
muertes_departamentos <- merge(muertes_departamentos, poblaciones, by = "DEPARTAMENTO")
muertes_departamentos$TASA <- (muertes_departamentos$DEFUNCIONES/muertes_departamentos$PROMEDIO) * 100000


departamentos_locacion <- st_read("departamentos_gtm",layer="departamentos_gtm")
departamentos_locacion  <- st_transform(departamentos_locacion, "+proj=longlat +datum=WGS84")
departamentos_locacion$nombre <- as.character(departamentos_locacion$nombre)

names (muertes_departamentos)[1] = "nombre"
mortalidad_dep<- merge(muertes_departamentos, departamentos_locacion, by = "nombre")
mortalidad_dep$nombre <- as.character(mortalidad_dep$nombre)
mortalidad_dep <- st_as_sf(mortalidad_dep)

pal <- colorNumeric( colorRampPalette(brewer.pal(9,"Reds"))(5),
                     domain = c(0,max(mortalidad_dep$TASA)))

dep_labels <-sprintf(
  "<strong>%s</strong><br/>Tasa de mortalidad: %g",
  muertes_departamentos$nombre, muertes_departamentos$TASA
) %>% lapply(htmltools::HTML) 

#Agrupación por causas de muerte más comunes
causas <- all_data
causas$n <- 1
causas <- causas %>% group_by(CAUSA1) %>% summarise(n=sum(n))
causas <- subset(causas) %>% filter(CAUSA1!="DESCONOCIDA")

causas <- head(arrange(causas,desc(n)), n = 10)
names (causas)[1] = "Causas"

head(causas$Causas,1)

#Datos
allDataTable <- all_data
allDataTable <- subset(allDataTable, select=-ID)

years_options <-      c("2015" = "2015",
                        "2016" = "2016",
                        "2017" = "2017",
                        "2018" = "2018",
                        "2019" = "2019",
                        "2020" = "2020",
                        "2021" = "2021" )

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
                      comprendiendo los registros de defunciones desde el año 2015 hasta el 2021
                      obtenidos en las bases de datos de RENAP."),
                    p("Este tablero ha sido desarrollado como un producto del proyecto 
                    “PHR-MODELS-C19“ como una iniciativa para fortalecer la disponibilidad de la información para Guatemala."),
                    p("En el menú desplazable del lado izquierdo se encuentra organizado por pestañas según diferentes categorías las estadísticas más 
                      relevantes en cuanto a mortalidad, presentadas de forma gráfica y acompañadas de un cuadro de datos que puede ser descargado."),

                  br(),  
                  fluidRow(
                      valueBox(nrow(all_data), "Defunciones registradas", icon = icon("fa-regular fa-display-medical")),
                      valueBox(format(round(tasa_mortalidad, 2), nsmall = 2), "Tasa bruta de mortalidad", icon = icon("percent")),
                      valueBox(
                        tags$p(head(causas$Causas,1), style = "font-size: 80%;"), "Causa de muerte más frecuente", icon = icon("wave-pulse"))
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
                           p("Este mapa describe la tasa de mortalidad cruda por año. Como denominador, se ha utilizado la
                              población proyectada por el Instituto Nacional de Estadística (INE) para cada año por
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
  
  
  muertes_fecha_plot <- reactive(subset(muertes_fecha, muertes_fecha$Año %in% input$years))
  causas_fecha_plot <- reactive(subset(causas_fecha, causas_fecha$Año %in% input$years))
 
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
