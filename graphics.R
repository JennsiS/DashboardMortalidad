library(ggplot2)
library(dplyr)
library(maps)
library(plotly)
library(hrbrthemes)

setwd("C:/Users/bff_n/Documents/GitHub/Dashboard-Mortalidad/")
all_data <- read.csv(file = 'all_data.csv')

#GT <- map_data("world") %>% filter(region=="GT")
#data <- world.cities %>% filter(country.etc=="GT")

#data_edad <-na.omit(all_data)
grupos_etarios %>% group_by(gr=cut(all_data, breaks= seq(0, 123, by = 10)) )
grupos_etarios
sexo <- all_data %>% count(SEXO)
registros <- data_edad
data <- data_edad

fig <- plot_ly(x = sexo$SEXO, y = sexo$n, type = 'bar') %>%
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


p <- muertes_fecha %>%
  ggplot( aes(x=muertes_fecha$FECHA, y=muertes_fecha$n)) +
  geom_area(fill="#69b3a2", alpha=0.5) +
  geom_line(color="#69b3a2") +
  ylab("Cantidad de muertes") +
  xlab("fecha") +
  theme_ipsum() 
  #scale_x_date(limit=c(as.Date("2021-01-01"),as.Date("2021-02-11"))) 

# Turn it interactive with ggplotly
p <- ggplotly(p)
p