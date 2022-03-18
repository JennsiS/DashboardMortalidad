library(ggplot2)
library(dplyr)
library(maps)
library(plotly)
library(hrbrthemes)
library(rjson)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
all_data <- read.csv(file = 'all_data.csv')
Sys.setlocale(locale = "es_ES.UTF-8")

###################Grafica muertes por grupo etario y sexo ###################
#Colocando una columna adicional clasifique los registros en grupo etario
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
mujeres$n


fig <- plot_ly(grupos_etarios, x = ~grupo_edad, y= ~n, type = 'bar', name = "TOTAL")%>%
  add_trace(mujeres,x = mujeres$grupo_edad, y = mujeres$n,  name = "femenino", color = "#EB5160") %>%
  add_trace(hombres,x = hombres$grupo_edad, y = hombres$n, name = "masculino", color = "#16a085") %>%
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
           gridcolor = 'ffff'))
fig


#####################Grafica de muertes por fecha#############################

muertes_fecha <- all_data %>% count(FECHA)
muertes_fecha$FECHA
muertes_fecha$n

fig <- plot_ly(muertes_fecha, x = ~FECHA, y = ~n, type = 'scatter', mode = 'lines')
fig

