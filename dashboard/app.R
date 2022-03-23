library(shiny)
library(shinythemes)

library(ggplot2)
library(dplyr)
library(maps)
library(plotly)
library(hrbrthemes)
library(rjson)
library(viridis)
library(rgdal)
library(leaflet)
library(RColorBrewer)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
all_data <- read.csv(file = 'all_data.csv')



ui <- #fluidPage(theme = shinythemes::shinytheme("united"),
      navbarPage(
      #theme = "cerulean",  # <--- To use a theme, uncomment this
        "Dashboard de mortalidad",
      tabPanel("Fechas",
                mainPanel(
                          h1("Mortalidad por fecha"),
                          plotlyOutput(outputId = "mortalidad_fecha"),
                          plotlyOutput(outputId = "mortalidad_causas_fecha")
                          
                ) # mainPanel
                           
       ), # Navbar 1, tabPanel
      tabPanel("Navbar 2", "This panel is intentionally left blank"),
      tabPanel("Navbar 3", "This panel is intentionally left blank")
      ) # navbarPage
#) # fluidPage


# Define server function  
server <- function(input, output) {
  
  muertes_fecha <- all_data %>% count(FECHA)
  output$mortalidad_fecha <- renderPlotly({
    plot_ly(muertes_fecha, x = ~FECHA, y = ~n, type = 'scatter', mode = 'lines', colors ='#00A6A6') %>%
      config(displaylogo = FALSE) %>%
      layout(xaxis= list(title = 'Fecha'),
             yaxis= list(title = 'Cantidad de muertes')
    )
    
  })
  
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
  
  output$mortalidad_causas_fecha <- renderPlotly({
    plot_ly(causas_fecha, x = ~FECHA, y = ~n, color = ~CAUSA1, type = 'scatter', mode = 'line', colors = "Set2") %>% 
    config(displaylogo = FALSE)%>%
      layout(xaxis = list(title = 'Fecha'),
             yaxis = list(title = 'Cantidad de muertes'),
             legend = list(title=list(text='<b> Causas de muerte </b>')),
             title = "Evolucion de mortalidad por causa de muerte")
  })
  
  
} # server


# Create Shiny object
shinyApp(ui = ui, server = server)
