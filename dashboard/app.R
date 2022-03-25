library(ggplot2)
library(dplyr)
library(maps)
library(plotly)
library(hrbrthemes)
library(rjson)
library(viridis)
library(rgdal)
library(leaflet)
library(sf)
library(RColorBrewer)
library(lubridate)
library(stringr)

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
causas_fecha <- causas_fecha %>% filter(FECHA>'2021-05-01')

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


mujeres <- grupos_etarios%>% filter(SEXO=="F")
hombres <- grupos_etarios%>% filter(SEXO=="M")
desconocidos <- grupos_etarios%>% filter(SEXO=="DESCONOCIDO")
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
                     domain = c(0,max(newdf$n)))

#Agrupación por causas de muerte más comunes
causas <- all_data
causas$n <- 1
causas <- causas %>% group_by(CAUSA1) %>% summarise(n=sum(n))
causas <- subset(causas) %>% filter(CAUSA1!="DESCONOCIDA")

causas <- head(arrange(causas,desc(n)), n = 10)
names (causas)[1] = "Causas"

####################################UI#########################################

ui <- #fluidPage(theme = shinythemes::shinytheme("united"),
      navbarPage(
      #theme = "cerulean",  # <--- To use a theme, uncomment this
        "Dashboard de mortalidad",
      tabPanel("Fechas",
                mainPanel(
                          h1("Mortalidad por fecha"),
                          plotlyOutput(outputId = "mortalidad_fecha"),
                          h1("Mortalidad por causas de muerte"),
                          plotlyOutput(outputId = "mortalidad_causas_fecha")
                          
                ) # mainPanel
                           
       ), # Navbar 1, tabPanel
      tabPanel("Categorías", 
               mainPanel(
                 plotlyOutput(outputId = "mortalidad_edad"),
                 plotlyOutput(outputId = "mortalidad_causas")
               )
               
        
      ),
      tabPanel("Localización", 
                 mainPanel(
                  leafletOutput(outputId = "mortalidad_mapa")  
                 )
      ),
      tabPanel("Datos","Aqui colocar una tabla con los datos")
      
      ) # navbarPage
#) # fluidPage


##################################SERVER#########################################  

server <- function(input, output) {
  
  output$mortalidad_fecha <- renderPlotly({
    plot_ly(muertes_fecha, x = ~fullDate, y = ~n, type = 'scatter', mode = 'lines', colors ='#00A6A6') %>%
      config(displaylogo = FALSE) %>%
      layout(xaxis= list(title = "Semana epidemiológica"),
             yaxis= list(title = "Cantidad de muertes"))
    
  })
  
  output$mortalidad_causas_fecha <- renderPlotly({
    plot_ly(causas_fecha, x = ~FECHA, y = ~n, color = ~CAUSA1, type = 'scatter', mode = 'line', colors = "Set2") %>% 
    config(displaylogo = FALSE)%>%
      layout(xaxis = list(title = 'Fecha'),
             yaxis = list(title = 'Cantidad de muertes'),
             legend = list(title=list(text='<b> Causas de muerte </b>')),
             title = "Evolucion de mortalidad por causa de muerte")
  })
  
 
  output$mortalidad_edad <- renderPlotly({
    plot_ly(grupos_etarios, x = ~grupo_edad, y= ~n, type = 'bar', name = "Total",marker=list(color ='#348AA7') )%>%
      add_trace(mujeres,x = mujeres$grupo_edad, y = mujeres$n,  name = "Femenino", marker=list(color ='#525174')) %>%
      add_trace(hombres,x = hombres$grupo_edad, y = hombres$n, name = "Masculino", marker=list(color ='#5DD39E')) %>%
      add_trace(desconocidos,x = desconocidos$grupo_edad, y = desconocidos$n, name = "Desconocido", marker=list(color ='#BCE784')) %>%
      layout(xaxis = list(title = 'Grupos etarios'),
             yaxis = list(title = 'Cantidad de muertes'),
             barmode = 'group',
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
        #label = labels,
        # labelOptions = labelOptions(
        #   style = list("font-weight" = "normal", padding = "3px 8px"),
        #   textsize = "15px",
        #   direction = "auto")
      ) %>%
      addLegend(pal = pal, values = ~n, opacity = 0.7, title = NULL,# titulo leyenda
                position = "bottomright", group = "Departamentos")%>% 
      setView(lat=14.628434,lng=-90.522713, zoom = 7)
    
  })
  
  output$mortalidad_causas <- renderPlotly({
    plot_ly(causas, x = ~n, y = ~Causas, type = 'bar', orientation = 'h', marker=list(color ='#FE6D73')) %>%
      config(displaylogo = FALSE)%>%
      layout(xaxis = list(title = 'Cantidad de muertes'),
             yaxis = list(title = 'Causas de muerte'),
             title ="Causas más frecuentes de mortalidad")
  })
  
  
} # server


# Create Shiny object
shinyApp(ui = ui, server = server)
