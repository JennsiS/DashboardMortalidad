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
library(shinydashboard)
library(shinydashboardPlus)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
all_data <- read.csv(file = 'all_data.csv')

###################DATA######################

#Muertes por semana epidemiologica
muertes_fecha <- all_data
muertes_fecha$epiWeek <- epiweek(muertes_fecha$FECHA)
muertes_fecha$year <- year(muertes_fecha$FECHA)
muertes_fecha$fullDate <- str_c(muertes_fecha$year,"-",muertes_fecha$epiWeek)
muertes_fecha$n <- 1
muertes_fecha <- muertes_fecha %>% group_by(fullDate) %>% summarise(n=sum(n))

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



#Agrupacion por grupo etario y sexo 
grupos_etarios <- all_data %>%
  mutate(grupo_edad = case_when(
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
grupos_etarios <- grupos_etarios %>% group_by(grupo_edad, SEXO) %>% summarise(n=sum(n))
grupos_etarios$grupo_edad <- factor(grupos_etarios$grupo_edad, levels = 
                                      c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69",
                                        "70-79", "80-89", "90-99", "100-109", "110-119", "120+", "DESCONOCIDA"))


grupos_etarios <- subset(grupos_etarios) %>% filter(grupo_edad!="DESCONOCIDA")
mujeres <- grupos_etarios%>% filter(SEXO=="F")
hombres <- grupos_etarios%>% filter(SEXO=="M")
#desconocidos <- grupos_etarios%>% filter(SEXO=="DESCONOCIDO")
colors_grapo <- c('#348AA7', '#525174', '#5DD39E','#BCE784')

#Agrupacion por departamento

muertes_departamentos <- all_data
muertes_departamentos$n <- 1
muertes_departamentos <- muertes_departamentos %>% group_by(DEPARTAMENTO) %>% summarise(n=sum(n))
muertes_departamentos$DEPARTAMENTO[muertes_departamentos$DEPARTAMENTO == "SOLOLÁ"] ="SOLOLA"
departamentos_locacion <- st_read("departamentos_gtm",layer="departamentos_gtm")
departamentos_locacion  <- st_transform(departamentos_locacion, "+proj=longlat +datum=WGS84")
departamentos_locacion$nombre <- as.character(departamentos_locacion$nombre)

names (muertes_departamentos)[1] = "nombre"
mortalidad_dep<- merge(muertes_departamentos, departamentos_locacion, by = "nombre")
mortalidad_dep$nombre <- as.character(mortalidad_dep$nombre)
mortalidad_dep <- st_as_sf(mortalidad_dep)

pal <- colorNumeric( colorRampPalette(brewer.pal(9,"Reds"))(5),
                     domain = c(0,max(mortalidad_dep$n)))

dep_labels <-sprintf(
  "<strong>%s</strong><br/>Cantidad de muertes: %g",
  muertes_departamentos$nombre, muertes_departamentos$n
) %>% lapply(htmltools::HTML) 

#Agrupación por causas de muerte más comunes
causas <- all_data
causas$n <- 1
causas <- causas %>% group_by(CAUSA1) %>% summarise(n=sum(n))
causas <- subset(causas) %>% filter(CAUSA1!="DESCONOCIDA")

causas <- head(arrange(causas,desc(n)), n = 10)
names (causas)[1] = "Causas"

#Datos
data_table <- all_data
data_table <- subset(data_table, select=-ID)

####################################UI#########################################

ui <- dashboardPage(
      dashboardHeader(title = "Tablero mortalidad Guatemala"),
      dashboardSidebar(
        sidebarMenu(
          menuItem("Fechas", tabName = "fechas"),
          menuItem("Categorias", tabName = "categorias"),
          menuItem("Localizacion", tabName = "localizacion"),
          menuItem("Datos", tabName = "datos")
        )
      ),
      dashboardBody(
        tags$head(
          tags$link(rel = "stylesheet", type = "text/css", href = "customStyles.css")
        ),
        tabItems(
          tabItem("fechas",  
                  h2("Mortalidad por fecha"),
                  fluidRow(
                    column (width=12,
                            plotlyOutput(outputId = "mortalidad_fecha")
                    )
                  ),
                  h2("Mortalidad por causas de muerte"),
                  fluidRow(
                    column (width=12,
                            plotlyOutput(outputId = "mortalidad_causas_fecha"))
                  )
          ),
          tabItem("categorias",
                    mainPanel(
                      fluidRow(
                        column(width=12,offset = 4,plotlyOutput(outputId = "mortalidad_edad"))
                      ),
                      fluidRow(
                        column(width=12,offset = 4,plotlyOutput(outputId = "mortalidad_causas"))
                      )
                    )
                  ),
          tabItem("localizacion",
                    mainPanel(
                      h2("Mortalidad por departamentos"),
                      fluidRow(
                        column(width = 12,offset = 4,leafletOutput(outputId = "mortalidad_mapa"))
                      )
                      
                    )
                  ),
          tabItem("datos",
                  downloadButton('Descargar',"Descargar datos"),
                  dataTableOutput("dataTable")
                  )
          )
        ) # body
      ) #dashboard page



##################################SERVER#########################################  

server <- function(input, output) {
  
  output$mortalidad_fecha <- renderPlotly({
    plot_ly(muertes_fecha, x = ~fullDate, y = ~n, type = 'scatter', mode = 'lines', colors ='#00A6A6') %>%
      config(displaylogo = FALSE) %>%
      layout(xaxis= list(title = "Semana epidemiológica"),
             yaxis= list(title = "Cantidad de muertes"))
    
  })
  
  output$mortalidad_causas_fecha <- renderPlotly({
    plot_ly(causas_fecha, x = ~fullDate, y = ~n, color = ~CAUSA1, type = 'scatter', mode = 'line', colors = "Set2") %>% 
    config(displaylogo = FALSE)%>%
      layout(xaxis = list(title = 'Fecha'),
             yaxis = list(title = 'Cantidad de muertes'),
             legend = list(title=list(text='<b> Causas de muerte </b>')))
  })
  
 
  output$mortalidad_edad <- renderPlotly({
    plot_ly(mujeres, x = mujeres$grupo_edad, y= ~n, type = 'bar', name = "Femenino", marker=list(color ='#FFAAA7'))%>%
      add_trace(hombres,x = hombres$grupo_edad, y = hombres$n, name = "Masculino", marker=list(color ='#98DDCA')) %>%
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
               gridcolor = 'ffff'),
             title ="Mortalidad por grupo etario y sexo")
  })
  
  output$mortalidad_mapa <- renderLeaflet({
    leaflet(mortalidad_dep) %>%
      setView(-94, 39, 5) %>%
      addProviderTiles("MapBox", options = providerTileOptions(
        id = "mapbox.light",
        accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>%
      addPolygons(
        fillColor = ~pal(n),
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
      addLegend(pal = pal, values = ~n, opacity = 0.7, title = "Cantidad de muertes",# titulo leyenda
                position = "bottomright", group = "Departamentos")%>% 
      setView(lat=16,lng=-90.522713, zoom = 7) %>% 
      addProviderTiles(providers$CartoDB.Positron)
    
  })
  
  output$mortalidad_causas <- renderPlotly({
    plot_ly(causas, x = ~n, y = ~Causas, type = 'bar', orientation = 'h', marker=list(color ='#FE6D73')) %>%
      config(displaylogo = FALSE)%>%
      layout(xaxis = list(title = 'Cantidad de muertes'),
             yaxis = list(title = 'Causas de muerte'),
             title ="Causas más frecuentes de mortalidad")
  })
  
  output$dataTable <- renderDataTable({
    datatable(data_table)
  })
  
  output$Descargar <- downloadHandler(
    filename = function() {
      paste("datos_mortalidad-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(all_data, file)
    }
  )
  
  
} # server


# Create Shiny object
shinyApp(ui = ui, server = server)
