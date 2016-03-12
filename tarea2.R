#########################################################################

##PARTE 1

setwd("C:/Users/WIN/Desktop/tarea2DataMining")
#Cargando la Data 
data= read.csv("minable.csv",stringsAsFactors = FALSE)
stringsAsFactors = FALSE
str(data)

#########################################################################
#Eliminando algunas caracteristicas sin importancia

data[,"cIdentidad"] = NULL
data[,"fNacimiento"] = NULL
data[,"jReprobadas"] = NULL
data[,"cDireccion"] = NULL

data = data[-1,] #Deleting outlier


#prc <- prc[-1]  #remueve la primera variable(id) desde el set de datos.
table(data$sexo)  # Nos ayuda a obtener el num de personas(dependiendo de la variable)
data$porSex <- factor(data$sexo, levels = c("0","1"), labels = c("femenino", "Masculino"))
round(prop.table(table(data$porSex)) * 100, digits = 1) #para mostrar en porcentaje

#prc <- prc[-1] #elimine diferentes variables no numéricas

normalize <- function(x) { 
  return ((x - min(x)) / (max(x) - min(x))) 
} #normalizamos la data

data_n <- as.data.frame(lapply(data[2:43], normalize)) #set de datos normalizados
summary(data_n$eCivil) #verificamos que el set de datos esta normalizado
summary(data_n$escuela) #probamos con algunas variables del set de datos

data_entrenamiento <- data_n[1:65,]
data_prueba <- data_n[66:100,]

data_entrenamiento_labels <- data[1:65, 1] #Este codigo toma el factor sexo en la columna 1 del prc data frame
data_prueba_labels <- data[66:100, 1] #y crea prc_entrenamiento_labels y prc_prueba_labels data frame.

install.packages("class") #paquete instalado exitosamente
library(class)

data_prueba_pred <- knn(train = data_entrenamiento, test = data_prueba,cl = data_entrenamiento_labels, k=12)
install.packages("gmodels") #paquete instalado exitosamente
library(gmodels)

CrossTable(x=data_prueba_labels, y=data_prueba_pred,prop.chisq = FALSE)



#########################################################################
#PARTE 2

############################################################################


##install.packages("xlsx", dep = T)      # If you have not installed it before
##library(xlsx)

## You need to specifiy the sheetIndex (sheet number)
#data <- read.xlsx("hogares.xlsx", sheetIndex = 1)

##library(readxl)

# read_excel reads both xls and xlsx files
#read_excel("my-old-spreadsheet.xls")
##read_excel("hogares.xlsx")



########################################################
#Usando Usage.R y google_api.R
#Calcular distancias de origen a destino

#################################

datos 
data= parse_data(datos)
data # data es tipo datos [[]]

mat= as.matrix(data)

mat[[1]] #lista de "elements" desde origen 1 a todos destinos
mat[[2]] #lista de "elements" desde origen 2 a todos destinos
mat[[3]] #lista de "elements" desde origen 1 a todos destinos


mat[[1]][1,1][2] #obtener Valor de DISTANCIA de origen 1 al destino 1

mat[[2]][1,1][1]=='ZERO_RESULTS'  #Valor de DISTANCIA de origen 1 al destino 2

mat[[3]][1,1][3]  #Valor de DISTANCIA de origen 1 al destino 3


direcciones= array(c(0,0,0),dim=length(destino))



#as.dataframe.data[1]$distance

#Calcular para cada Origen la distancia/tiempo hacia todos los destinos
for(i in 1:length(destino) )
  ##
  if ((mat[[i]][1,1][1]=='ZERO_RESULTS')==TRUE)
    #Si se cumple esta condicion es por que no se obtuvieron resultados
    #lo que significa q no hay manera de llegar de origen a destino
    
    direcciones[i]= mat[[1]][1,1][3] 
else
  direcciones[i]=mat[[2]][1,1][1]



#datos_json=toJSON(datos)


names(data)


dplyr
cheatsheet

####################################NO

b<- list(a = list(var.1 = 1, var.2 = 2, var.3 = 3)
         , b = list(var.1 = 4, var.2 = 5, var.3 = 6)
         , c = list(var.1 = 7, var.2 = 8, var.3 = 9)
         , d = list(var.1 = 10, var.2 = 11, var.3 = 12)
)


library (plyr)
df <- ldply (b, data.frame)

library(jsonlite)
library(httr)

#get data
data1 <- fromJSON(api_url)
names(data1)



data1$destination_addresses
#install
install.packages("jsonlite", repos="http://cran.r-project.org")

#load
library(jsonlite)

#convert object to json
myjson <- toJSON(iris, pretty=TRUE)
cat(myjson)

#convert json back to object
iris2 <- fromJSON(myjson)
print(iris2)

###
datos = get_data(api_url)
datos

toJSON( get_data(api_url))
parse_data(get_data(api_url))

########################################################NO

datos2 <- read.csv("hogares.csv",head=TRUE,sep=",")
names(datos2)

datos2$Tipo.de.Inmueble
attributes(datos2)

##Elimino la Columna de Foto
datos2[8]=NULL
datos2


row1<-datos2[1]
row2<-datos2[2]
row3<-datos2[3]
row4<-datos2[4]
row5<-datos2[5]
row6<-datos2[6]
row7<-datos2[7]
row8<-datos2[8]

library(jsonlite)
library(httr)

#get data
data1 <- fromJSON(api_url)

#it's a data frame
names(data1)
data1$login