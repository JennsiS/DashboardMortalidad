#Importacion de las librerias necesarias
library("readxl")
library(dplyr)
library(stringr)

#referenciando el directorio de trabajo
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

#Lectura de datos
data_1519 <- read_excel( "b1519_renap.xlsx")
data_1921 <- read_excel("baseCES2021_renap.xlsx")

#Obteniendo los valores unicos presentados en la columna de departamento
unique(data_1519$DEPARTAMENTO)

#Filtrando los registros que tienen departamentos de Guatemala
departamentos_validos <- c("BAJA VERAPAZ", "HUEHUETENANGO","GUATEMALA", "ALTA VERAPAZ",
                           "CHIQUIMULA", "QUICHE", "QUETZALTENANGO", "ESCUINTLA", "ZACAPA",
                           "IZABAL", "SAN MARCOS", "TOTONICAPAN", "JUTIAPA", "SACATEPEQUEZ",
                           "PETEN", "RETALHULEU", "SUCHITEPEQUEZ", "SANTA ROSA", "SOLOLÁ",
                           "CHIMALTENANGO", "EL PROGRESO", "JALAPA")

data_1519 <- subset(data_1519, DEPARTAMENTO %in% departamentos_validos)
unique(data_1519$DEPARTAMENTO)

#Verificando los valores unicos de las demas columnas
unique(data_1519$SEXO)
data_1519$SEXO[data_1519$SEXO == "I"] ="DESCONOCIDO"
unique(data_1519$EDAD)
data_1519$EDAD[is.na(data_1519$EDAD)] = "DESCONOCIDA"

unique(data_1519$CAUSA1)
unique(data_1519$CAUSA2)
unique(data_1519$CAUSA3)
unique(data_1519$CAUSA4)
unique(data_1519$FECHA)
data_1519 <- data_1519[,-3]

#Eliminando columnas y modificando nombres para hacer coincidir bases de datos 
data_1921
data_1921 <- data_1921[,c(-12,-5,-3)]
names(data_1921)
names (data_1921)[4] = "SEXO"
names (data_1921)[3] = "DEPARTAMENTO"
names (data_1921)[1] = "ID"
names (data_1921)[6] = "CAUSA1"
names (data_1921)[7] = "CAUSA2"
names (data_1921)[8] = "CAUSA3"
names (data_1921)[9] = "CAUSA4"

unique(data_1921$DEPARTAMENTO)
data_1921 <- subset(data_1921, DEPARTAMENTO %in% departamentos_validos)

#Cambiando los valores faltantes por "DESCONOCIDOS"
unique(data_1921$SEXO)
data_1921$SEXO[is.na(data_1921$SEXO)] = "DESCONOCIDO"

unique(data_1921$EDAD)
data_1921$EDAD[is.na(data_1921$EDAD)] = "DESCONOCIDA"

data_1921 <- data_1921 %>% filter(FECHA >= "2015-01-01")

#merge de las bases de datos
all_data <- merge(data_1519, data_1921, all = TRUE)

all_data$ID <- seq.int(nrow(all_data))
all_data <- all_data[,c("ID", names(all_data)[names(all_data) != "ID"])]

all_data[['FECHA']] <- strptime(all_data[['FECHA']], format = "%Y-%m-%d")



unique(all_data$EDAD)
unique(all_data$SEXO)
unique(all_data$FECHA)
all_data$CAUSA1[is.na(all_data$CAUSA1)] = "DESCONOCIDA"
all_data$CAUSA1[all_data$CAUSA1=='"Ignorado'] = "DESCONOCIDA"
all_data$CAUSA1[all_data$CAUSA1=='"Ignorado"'] = "DESCONOCIDA"
all_data$CAUSA1[all_data$CAUSA1=='"ignorado"'] = "DESCONOCIDA"
all_data$CAUSA1<- str_replace_all(all_data$CAUSA1, "^-", "DESCONOCIDA")
all_data$CAUSA1<- str_replace_all(all_data$CAUSA1, "^\\s", "DESCONOCIDA")
all_data$CAUSA2[is.na(all_data$CAUSA2)] = "DESCONOCIDA"
all_data$CAUSA3[is.na(all_data$CAUSA3)] = "DESCONOCIDA"
all_data$CAUSA4[is.na(all_data$CAUSA4)] = "DESCONOCIDA"
all_data$EDAD[is.na(all_data$EDAD)] = "DESCONOCIDA"
#all_data$CAUSA2<- str_replace_all(all_data$CAUSA2, "^-", "DESCONOCIDA")
#all_data$CAUSA3<- str_replace_all(all_data$CAUSA3, "^-", "DESCONOCIDA")
#all_data$CAUSA4<- str_replace_all(all_data$CAUSA4, "^-", "DESCONOCIDA")
all_data

write.csv(all_data,"all_data.csv", row.names = FALSE)

