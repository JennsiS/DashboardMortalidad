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
library(readxl)
library(stats)
library(graphics)
library(utils)
library(datasets)
library(epitools)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
all_data <- read.csv(file = 'all_data.csv')
poblaciones <- read_excel( "PoblacionesINE.xlsx")
poblacionesSexo <- read_excel( "PoblacionesSexo.xlsx")
poblacionesEdad <- read_excel( "PoblacionesEdades.xlsx")

allDataTable <- all_data
allDataTable <- subset(allDataTable, select=-ID)

years_options <-      c("2015",
                        "2016",
                        "2017",
                        "2018",
                        "2019",
                        "2020",
                        "2021" )

data_filtrada <- all_data
data_filtrada$anio <- year(data_filtrada$FECHA)

causas <- all_data
causas$n <- 1
causas <- causas %>% group_by(CAUSA1) %>% summarise(n=sum(n))
causas <- subset(causas) %>% filter(CAUSA1!="DESCONOCIDA")

causas <- head(arrange(causas,desc(n)), n = 10)
names (causas)[1] = "Causas"
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
muertes_fecha$epiWeek <- paste0(year(muertes_fecha$FECHA),"-", as.week(muertes_fecha$FECHA)$week)

muertes_fecha$fullDate <- muertes_fecha$epiWeek
muertes_fecha$n <- 1
muertes_fecha <- muertes_fecha %>% group_by(fullDate) %>% summarise(n=sum(n))
names (muertes_fecha)[1] = "Semana_epidemiologica"
muertes_fecha$Year <- sub("\\-.*", "", muertes_fecha$Semana_epidemiologica)


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
causas_fecha$epiWeek <- paste0(year(causas_fecha$FECHA),"-", as.week(causas_fecha$FECHA)$week)

causas_fecha$fullDate <- causas_fecha$epiWeek
causas_fecha <-causas_fecha %>% group_by(fullDate, CAUSA1) %>% summarise(n=sum(n))
names (causas_fecha)[1] = "Semana_epidemiologica"
names (causas_fecha)[2] = "Causa_1"
causas_fecha$Year <- sub("\\-.*", "", causas_fecha$Semana_epidemiologica)



tasas_sexo <- all_data
tasas_sexo$DEFUNCIONES <- 1
selected_years <- unique(tasas_sexo$Year)
tasas_sexo <- tasas_sexo %>% group_by(SEXO)  %>% summarise(DEFUNCIONES=sum(DEFUNCIONES))
tasas_sexo <- subset(tasas_sexo) %>% filter(SEXO!="DESCONOCIDO")
tasas_sexo <- merge(tasas_sexo, poblacionesSexo, by = "SEXO")
tasas_sexo$poblacion_total <- 0

for (column in names(tasas_sexo)) {
  if(column %in% selected_years){
    poblacion_actual <- tasas_sexo[length(names(tasas_sexo))]
    total <- poblacion_actual + tasas_sexo[column]
    tasas_sexo[length(names (tasas_sexo))] <- total
  }
}
names (tasas_sexo)[length(names (tasas_sexo))] = "poblacion_total"

tasas_sexo$TASA <- (tasas_sexo$DEFUNCIONES/tasas_sexo$poblacion_total) * 100000



#Creando dataframe para crear los poligonos correspondientes en el mapa

departamentos_locacion <- st_read("departamentos_gtm",layer="departamentos_gtm")
departamentos_locacion  <- st_transform(departamentos_locacion, "+proj=longlat +datum=WGS84")
departamentos_locacion$nombre <- as.character(departamentos_locacion$nombre)

##################################### Funciones ######################################

groupby_edad_tasa <-function (data, poblacionesEdad){
  grupos_etarios <- data
  grupos_etarios$Year <- year(grupos_etarios$FECHA)
  selected_years <- unique(grupos_etarios$Year)
  grupos_etarios$EDAD <- as.numeric(grupos_etarios$EDAD)
  grupoes_etarios <- na.omit(grupos_etarios)
  grupos_etarios <- grupoes_etarios %>%
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
  
  grupos_etarios$DEFUNCIONES <- 1
  grupos_etarios <- grupos_etarios %>% group_by(GRUPO_ETARIO) %>% summarise(DEFUNCIONES=sum(DEFUNCIONES))
  grupos_etarios$GRUPO_ETARIO <- factor(grupos_etarios$GRUPO_ETARIO, levels =
                                          c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69",
                                            "70-79", "80-89", "90-99", "100-109", "110-119", "120+", "DESCONOCIDA"))
  
  
  grupos_etarios <- subset(grupos_etarios) %>% filter(GRUPO_ETARIO!="DESCONOCIDA")
  names(grupos_etarios)[1] = "Grupo_etario"
  tasas_edades <- merge(grupos_etarios, poblacionesEdad, by = "Grupo_etario")
  tasas_edades$poblacion_total <- 0
  
  for (column in names(tasas_edades)) {
    if(column %in% selected_years){
      poblacion_actual <- tasas_edades[length(names(tasas_edades))]
      total <- poblacion_actual + tasas_edades[column]
      tasas_edades[length(names (tasas_edades))] <- total
    }
  }
  names (tasas_edades)[length(names (tasas_edades))] = "poblacion_total"
  
  tasas_edades$TASA <- (tasas_edades$DEFUNCIONES/tasas_edades$poblacion_total) * 100000
  return(tasas_edades)
  
}

groupby_sexo_tasa <-function (data, poblacionesSexo){
  tasas_sexo <- data
  tasas_sexo$Year <- year(tasas_sexo$FECHA)
  tasas_sexo$DEFUNCIONES <- 1
  selected_years <- unique(tasas_sexo$Year)
  tasas_sexo <- tasas_sexo %>% group_by(SEXO)  %>% summarise(DEFUNCIONES=sum(DEFUNCIONES))
  tasas_sexo <- subset(tasas_sexo) %>% filter(SEXO!="DESCONOCIDO")
  tasas_sexo <- merge(tasas_sexo, poblacionesSexo, by = "SEXO")
  tasas_sexo$poblacion_total <- 0
  
  for (column in names(tasas_sexo)) {
    if(column %in% selected_years){
      poblacion_actual <- tasas_sexo[length(names(tasas_sexo))]
      total <- poblacion_actual + tasas_sexo[column]
      tasas_sexo[length(names (tasas_sexo))] <- total
    }
  }
  names (tasas_sexo)[length(names (tasas_sexo))] = "poblacion_total"
  
  tasas_sexo$TASA <- (tasas_sexo$DEFUNCIONES/tasas_sexo$poblacion_total) * 100000
  return(tasas_sexo)
  
}

groupby_departamento <- function (data, poblaciones, departamentos_locacion){
  #Agrupacion por departamento
  
  muertes_departamentos <- data
  muertes_departamentos$Year <- year(data$FECHA)
  selected_years <- unique(muertes_departamentos$Year)
  
  muertes_departamentos$DEFUNCIONES <- 1
  muertes_departamentos <- muertes_departamentos %>% group_by(DEPARTAMENTO) %>% summarise(DEFUNCIONES=sum(DEFUNCIONES))
  muertes_departamentos$DEPARTAMENTO[muertes_departamentos$DEPARTAMENTO == "SOLOLÁ"] ="SOLOLA"
  muertes_departamentos <- merge(muertes_departamentos, poblaciones, by = "DEPARTAMENTO")
  muertes_departamentos$poblacion_total <- 0
  
  
  for (column in names(muertes_departamentos)) {
    if(column %in% selected_years){
      poblacion_actual <- muertes_departamentos[length(names (muertes_departamentos))]
      total <- poblacion_actual + muertes_departamentos[column]
      muertes_departamentos[length(names (muertes_departamentos))] <- total
    }
  }
  names (muertes_departamentos)[length(names (muertes_departamentos))] = "poblacion_total"
  
  muertes_departamentos$TASA <- (muertes_departamentos$DEFUNCIONES/muertes_departamentos$poblacion_total) * 100000
  
  names (muertes_departamentos)[1] = "nombre"
  mortalidad_dep<- merge(muertes_departamentos, departamentos_locacion, by = "nombre")
  mortalidad_dep$nombre <- as.character(mortalidad_dep$nombre)
  mortalidad_dep <- st_as_sf(mortalidad_dep)
  
  return(mortalidad_dep)
  
}

groupby_causas <- function (data){
  causas <- data
  causas$n <- 1
  causas <- causas %>% group_by(CAUSA1) %>% summarise(n=sum(n))
  causas <- subset(causas) %>% filter(CAUSA1!="DESCONOCIDA")
  
  causas <- head(arrange(causas,desc(n)), n = 10)
  names (causas)[1] = "Causas"
  return(causas)
}


groupby_edad_sexo <- function(data,opcion) {
  #Agrupacion por grupo etario y sexo
  grupos_etarios <- data
  grupos_etarios$EDAD <- as.numeric(grupos_etarios$EDAD)
  grupoes_etarios <- na.omit(grupos_etarios)
  grupos_etarios <- grupoes_etarios %>%
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
  grupos_etarios$Year <- year(grupos_etarios$FECHA)
  grupos_etarios <- grupos_etarios %>% group_by(GRUPO_ETARIO, SEXO) %>% summarise(n=sum(n))
  grupos_etarios$GRUPO_ETARIO <- factor(grupos_etarios$GRUPO_ETARIO, levels =
                                          c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69",
                                            "70-79", "80-89", "90-99", "100-109", "110-119", "120+", "DESCONOCIDA"))
  
  
  grupos_etarios <- subset(grupos_etarios) %>% filter(GRUPO_ETARIO!="DESCONOCIDA")
  names(grupos_etarios)[1] = "Grupo_etario"
  names(grupos_etarios)[2] = "Sexo"
  
  if (opcion =="h") {
    hombres <- grupos_etarios%>% filter(Sexo=="M")
    names (hombres)[1] = "Grupo_etario"
    names (hombres)[2] = "Sexo"
    return(hombres)
    
  } else if (opcion =="m"){
    mujeres <- grupos_etarios%>% filter(Sexo=="F")
    names (mujeres)[1] = "Grupo_etario"
    names (mujeres)[2] = "Sexo"
    return(mujeres)
  
  } else if (opcion=="edad"){
    #grupos_etarios <- subset(grupos_etarios, select=-ID)
    return(grupos_etarios)
  }

  
}


