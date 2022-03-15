#Importacion de las librerias necesarias
library("readxl")

#referenciando el directorio de trabajo
setwd("C:/Users/bff_n/Documents/GitHub/Dashboard-Mortalidad/")

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
unique(data_1519$CAUSA1)



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
unique(data_1921$DEPARTAMENTO)

#merge de las bases de datos
all_data <- merge(data_1519, data_1921, all = TRUE)
all_data
