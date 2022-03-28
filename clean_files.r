#Importacion de las librerias necesarias
library(readxl)
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

all_data$CAUSA1[all_data$CAUSA1=='Paro Cardio Respiratorio'] = "Paro Cardiorrespiratorio"
all_data$CAUSA1[all_data$CAUSA1=='Paro Cardiorespiratorio'] = "Paro Cardiorrespiratorio"
#Segun la literatura consultado un paro cardiaco y un paro cardiorrespiratorio son lo mismo
all_data$CAUSA1[all_data$CAUSA1=='Paro Cardiaco'] = "Paro Cardiorrespiratorio"
all_data$CAUSA1[all_data$CAUSA1=='Infarto Agudo del Miocardio'] = "Infarto Agudo al Miocardio"
all_data$CAUSA1[all_data$CAUSA1=='Neumonua'] = "Neumonía"
all_data$CAUSA1[all_data$CAUSA1=='Neumonia'] = "Neumonía"
all_data$CAUSA1[all_data$CAUSA1=='Choque Septico'] = "Choque Séptico"
all_data$CAUSA1[all_data$CAUSA1=='Choque Suptico'] = "Choque Séptico"
AllData$CAUSA1<-str_replace_all(AllData$CAUSA1, "_", "")
AllData$CAUSA1<-str_replace_all(AllData$CAUSA1,"1", "")
AllData$CAUSA1<-str_replace_all(AllData$CAUSA1,"-", "")
AllData$CAUSA1<-str_replace_all(AllData$CAUSA1,"Causa directa:", "")
AllData$CAUSA1<-str_replace_all(AllData$CAUSA1,"causa directa:", "")
AllData$CAUSA1<-str_replace(AllData$CAUSA1,"  Choque Septico Pulmonar", "Choque Septico")
AllData$CAUSA1<-str_replace(AllData$CAUSA1,"  Choque Septico pulmonar,", "Choque Septico")
AllData$CAUSA1<-str_replace(AllData$CAUSA1,"Choque S???ptico", "Choque Septico")
AllData$CAUSA1<-str_replace(AllData$CAUSA1,"  Bronco Neumon???a Bilateral", "Bronconeumonia bilateral")
AllData$CAUSA1<-str_replace(AllData$CAUSA1,"bronconeumon???a bilateral", "Bronconeumonia bilateral")
AllData$CAUSA1<-str_replace(AllData$CAUSA1, " . Causa Directa: ","")
AllData$CAUSA1<-str_replace(AllData$CAUSA1, "Dengue hemorr???gico", "Dengue hemorragico")
AllData$CAUSA1<-str_replace(AllData$CAUSA1, "ICC", "Insuficiencia cardiaca congestiva")
AllData$CAUSA1<-str_replace(AllData$CAUSA1, " S???psis del Recien Nacido", "Sepsis del recien nacido")
AllData$CAUSA1<-str_replace(AllData$CAUSA1, "Alteraci???n del Estado de Conciencia ", "Alteracion del estado de conciencia")
AllData$CAUSA1<-str_replace(AllData$CAUSA1, "CA DE ESTOMAGO", "Cancer Gastrico")
AllData$CAUSA1<-str_replace(AllData$CAUSA1,"CA cervix estudio estudio 3B metastasis","Cancer de cervix")
AllData$CAUSA1<-str_replace(AllData$CAUSA1, "Choque neurog???nico refractorio a aminas", "Choque neurogenico refractorio a aminas")
AllData$CAUSA1<-str_replace(AllData$CAUSA1, "Cirrosis hep???tica", "Cirrosis hepatica")
AllData$CAUSA1<-str_replace(AllData$CAUSA1, " Causa Directa: ", "")
AllData$CAUSA1<-str_replace(AllData$CAUSA1, "Choque Septico pulmonar,", "Choque Septico")
AllData$CAUSA1<-str_replace(AllData$CAUSA1, "COMA DIABETICO", "Coma diabetico")
AllData$CAUSA1<-str_replace(AllData$CAUSA1, "Coma Diab???tico", "Coma diabetico")
AllData$CAUSA1<-str_replace(AllData$CAUSA1, "C???ncer de mama diseminado grado IV.","Cancer de mama diseminado")
AllData$CAUSA1<-str_replace(AllData$CAUSA1, "ECN hemorr???gico", "Evento cerebrovascular hemorragico")
AllData$CAUSA1<-str_replace(AllData$CAUSA1, " Estado de Choque", "Estado de choque")
AllData$CAUSA1<-str_replace(AllData$CAUSA1, "Estado de choque Hipovolemico", "Choque hipovolemico")
AllData$CAUSA1<-str_replace(AllData$CAUSA1, "Estado de Choque Hipovol???mico", "Choque hipovolemico")
AllData$CAUSA1<-str_replace(AllData$CAUSA1, "Estado de Choque Septico,", "Choque Septico")
AllData$CAUSA1<-str_replace(AllData$CAUSA1, "Estado de choque septico. NAC,", "Choque Septico")
AllData$CAUSA1<-str_replace(AllData$CAUSA1, "Estado de choque septico. NAC,", "Choque Septico")
AllData$CAUSA1<-str_replace(AllData$CAUSA1, "Infecci???n Gastrointestinal", "Infeccion gastrointestinal")
AllData$CAUSA1<-str_replace(AllData$CAUSA1, "Infarto del Miocardio ", "Infarto al miocardio")
AllData$CAUSA1<-str_replace(AllData$CAUSA1, "infarto del miocardio ", "Infarto al miocardio")
AllData$CAUSA1<-str_replace(AllData$CAUSA1, "Shock Septico","Choque Septico")
AllData$CAUSA1<-str_replace(AllData$CAUSA1, "Neumon???a", "Neumonia")
AllData$CAUSA1<-str_replace(AllData$CAUSA1, "Senilidad", "Senectud")
AllData$CAUSA1<-str_replace(AllData$CAUSA1, " Paro cardiorespiratorio", "Paro Cardiorrespiratorio")
AllData$CAUSA1<-gsub('[0-9]*','',AllData$CAUSA1)
AllData$CAUSA1<-gsub("\\.","", AllData$CAUSA1)
AllData$CAUSA1<-gsub("a)", "", AllData$CAUSA1)
AllData$CAUSA1<-str_replace(AllData$CAUSA1,"Alcoholismo nivel de intoxicaci???n no especificada","Alchoholismo")
AllData$CAUSA1<-str_replace(AllData$CAUSA1, "Alcoholismo nivel de intoxicaci???n no espeficada","Alcoholismo")
AllData$CAUSA1<-str_replace(AllData$CAUSA1, "Alcoholismo nivel de intoxicaci???n, no especificado","Alcoholismo")
AllData$CAUSA1<-str_replace(AllData$CAUSA1, "Alcoholismo, nivel de intoxicaci???n no especificada","Alcoholismo")
AllData$CAUSA1<-str_replace(AllData$CAUSA1, "Alcoholismo, nivel de Intoxicaci???n no especificada","Alcoholismo")
AllData$CAUSA1<-str_replace(AllData$CAUSA1, "Alcoholismo, Nivel de Intoxicaci???n no especificada","Alcoholismo")
AllData$CAUSA1<-str_replace(AllData$CAUSA1, "Alcoholismo, nivel de intoxicaci???n no especificado","Alcoholismo")
AllData$CAUSA1<-str_replace(AllData$CAUSA1, "Alcoholismo, nivel de intoxicaci???n, no especificada","Alcoholismo")
AllData$CAUSA1<-str_replace(AllData$CAUSA1, "Alcoholismo, nivel de Intoxicaci???n, no especificado","Alcoholismo")
AllData$CAUSA1<-str_replace(AllData$CAUSA1, "Alcohol???smo Cr???nico","Alcoholismo")
AllData$CAUSA1<-str_replace(AllData$CAUSA1, "Alcoh???lismo Cr???nica","Alcoholismo")
AllData$CAUSA1<-str_replace(AllData$CAUSA1, "alcoholismo Agudo","Alcoholismo")
AllData$CAUSA1<-str_replace(AllData$CAUSA1, "Alcoholismo Cronico","Alcoholismo")
AllData$CAUSA1<-str_replace(AllData$CAUSA1, "Alcoholismo Cr???nico","Alcoholismo")
AllData$CAUSA1<-str_replace(AllData$CAUSA1, "Alchoholismo","Alcoholismo")
AllData$CAUSA1<-str_replace(AllData$CAUSA1, " Accidente vascular encefalico","Evento Cerebrovascular")
AllData$CAUSA1<-str_replace(AllData$CAUSA1, " Accidente Vascular encefalico","Evento Cerebrovascular")
AllData$CAUSA1<-str_replace(AllData$CAUSA1, "Accidente vascular encef???tico agudo no especificado como hemorragico o esqu???mico", "Evento Cerebrovascular")
AllData$CAUSA1<-str_replace(AllData$CAUSA1, "Bronco Neumonia Neumonia", "Bronconeumonia")
AllData$CAUSA1<-str_replace(AllData$CAUSA1, "Bronconeumonia Neumonia","Bronconeumonia")
AllData$CAUSA1<-str_replace(AllData$CAUSA1, "Bronconeumonia Neumonia", "Bronconeumonia")
AllData$CAUSA1<-str_replace(AllData$CAUSA1, "Bronconeumonia no especificada", "Bronconeumonia")
AllData$CAUSA1<-str_replace(AllData$CAUSA1, "Bronconeumon???a","Bronconeumonia")
AllData$CAUSA1<-str_replace(AllData$CAUSA1, "Bronconeumon???a Neumonia", "Bronconeumonia")
AllData$CAUSA1<-str_replace(AllData$CAUSA1," Bronconeumon???a Neumonia","Bronconeumonia")
AllData$CAUSA1<-str_trim(AllData$CAUSA1, side = "both")
AllData$CAUSA1<-gsub("\\,", "", AllData$CAUSA1)
AllData$CAUSA1<-gsub("\\:", "", AllData$CAUSA1)
AllData$CAUSA1<-gsub("\\;","", AllData$CAUSA1)
AllData$CAUSA1<-gsub(";;","", AllData$CAUSA1)
AllData$CAUSA1<-gsub("  Asfixia","Asfixia", AllData$CAUSA1)


#all_data$CAUSA2<- str_replace_all(all_data$CAUSA2, "^-", "DESCONOCIDA")
#all_data$CAUSA3<- str_replace_all(all_data$CAUSA3, "^-", "DESCONOCIDA")
#all_data$CAUSA4<- str_replace_all(all_data$CAUSA4, "^-", "DESCONOCIDA")
all_data

write.csv(all_data,"all_data.csv", row.names = FALSE)

