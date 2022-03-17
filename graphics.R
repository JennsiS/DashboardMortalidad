library(ggplot2)
library(dplyr)
library(maps)
library(plotly)
library(hrbrthemes)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
all_data <- read.csv(file = 'all_data.csv')
Sys.setlocale(locale = "es_ES.UTF-8")

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
    between(EDAD,120,150) ~ "120+"
  ))

grupos_etarios$n <- 1
grupos_etarios <- grupos_etarios %>% group_by(grupo_edad, SEXO) %>% summarise(n=sum(n))

#data_edad <-na.omit(all_data)
grupos_etarios %>% group_by(gr=cut(all_data, breaks= seq(0, 123, by = 10)) )
grupos_etarios
sexo <- all_data %>% count(SEXO)
registros <- data_edad
data <- data_edad

fig <- plot_ly(grupos_etarios%>% filter(SEXO=="F"),x = ~grupo_edad, y = ~n, type = 'bar', name = "femenino") %>%
  add_trace(grupos_etarios%>% filter(SEXO=="M"),x = ~grupo_edad, y = ~n, name = "masculino", color = "#16a085") %>%
  layout(xaxis = list(title = 'x'),
         yaxis = list(title = 'y'),
         plot_bgcolor='#e5ecf6',
         xaxis = list(
           zerolinecolor = '#ffff',
           zerolinewidth = 2,
           gridcolor = 'ffff'),
         yaxis = list(
           zerolinecolor = '#ffff',
           zerolinewidth = 2,
           gridcolor = 'ffff'))


fig

muertes_fecha <- all_data %>% count(FECHA)
muertes_fecha$FECHA
muertes_fecha$n

fig <- plot_ly(muertes_fecha, x = ~FECHA, y = ~n, type = 'scatter', mode = 'lines')


fig



