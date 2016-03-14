
#########################################################################
#PARTE 2

#Cargando los datos de "hogares.csv"
############################################################################
datos2<- read.csv("hogares.csv", stringsAsFactors = FALSE)
stringsAsFactors = FALSE
str(datos2)
names(datos2)

#Eliminar Columnas que aportan poca informacion 
datos2$Distrito=NULL
datos2$Foto=NULL
datos2$Piso=NULL

#corrijiendo Datos del dataset "hogares.csv"
datos2$Dirección[61]="Via di Monte Verde"
datos2$Dirección[33]="Via San Roberto Bellarmino"

datos2$Tipo.de.Inmueble[7]="Appartamento"   #Appartameno
datos2$Tipo.de.Inmueble[27]="Appartamento"  #Appartamenti
datos2$Tipo.de.Inmueble[61]="Appartamento"  #Apartamento
datos2$Tipo.de.Inmueble[85]="Appartamento"  #Aparrtamento
datos2$Tipo.de.Inmueble[37]="Mini Appartamento"# Mini\nAppartamento
datos2$Tipo.de.Inmueble[103]="Mini Appartamento"  #  Mini\nAppartamento

datos2$Tipo.de.Inmueble[62]="Mini Appartamento"# Mini appartamento
datos2$Tipo.de.Inmueble[65]="Mini Appartamento"# Mini appartamento


#Agrego la Columnas Distancia para guardar la distancia de direccion Origen a la Universidad* 
datos2$Distancia= 0

#Agrego Columnas que usare para obtener la informacion mas facil sobre NOTAS
datos2$Disponible=0

#Agrego columnas para obtener mas facil informacion de DESCRIPCION
#0 si No posee la caracteristica 1 o mas depende cuantos posea  
datos2$Camera=0
datos2$Bagno=0
datos2$Cucina=0
datos2$Salone=0
datos2$Ingresso=0
datos2$Internet=0
datos2$Terrazzo=0
datos2$Balcone=0
datos2$Rispotiglio=0

#Agrego columnas para obtener mas facil informacion de PRECIO MENSUAL
datos2$Condominio=0
datos2$Riscaldamento=0
datos2$Acqua=0


names(datos2)

#Se Manipula La Columna Direccion para poder usar su contenido
#y poder guardar sin espacios la direccion en Destino

datos2$Dirección= strsplit(as.character(datos2$Dirección), "\n")

datos2$Dirección
for(i in (seq(1:nrow(datos2)) ))
{
  datos2$Dirección[i]= paste(datos2$Dirección[[i]], collapse=" ")
}

datos2$Dirección= as.character(datos2$Dirección)

#datos2$Distancia

#datos2$Dirección[1]
#datos2$Dirección[[1]]
#names(datos2)

###########################################################

#Usando los archivos usage.R y google_api.R

###########################################################
#install.package('curl')
# Seleccionar google_api.R en su sistema de archivos

########################################################

#Calcular distancias de origen a destino

#################################

#Llenar la Columna de Distancia:

origen=c('Via Amerigo Vespucci')
#Calcular para cada Origen en Distancia hacia el destino

for(i in (1:nrow(datos2))  ){
  
  destino = datos2$Dirección[i]
 
  print(destino)
  # Colocar su API Key 
  api_key = "AIzaSyDwbFK39CVJF4woGAgJlssOUxBYgc6fh98"
  
  api_url = get_url(origen, destino, api_key)
  datos = get_data(api_url)
 
  data= parse_data(datos)
  
  
  mat= as.matrix(data)
  
 if ((mat[[1]][1,1][1]=='ZERO_RESULTS')){
    #Si se cumple esta condicion es por que  se obtuvieron resultados
    #lo que significa q  hay manera de llegar de origen a destino
   
   datos2$Distancia[[i]]=0
   print(datos2$Distancia[[i]])
 }else{
   datos2$Distancia[[i]]=mat[[1]][1,1][2] 
   print(datos2$Distancia[[i]])
   
 }
}

#Transformar Columnas:
for(i in (1:nrow(datos2))  ){
  ###########
  #numerizando la columna Habitaciones.Disponibles
  #
  #0 = Doppia,  1 = singola  , 2 = Appartamento Intero
  ###########
  if( (grepl("(singola)",datos2$Habitaciones.Disponibles[i]  ))|
      (grepl("(singole)",datos2$Habitaciones.Disponibles[i]  ))|
      (grepl("(Singole)",datos2$Habitaciones.Disponibles[i]  ))|
      (grepl("(Singola)",datos2$Habitaciones.Disponibles[i]  ))
    ){
    datos2$Habitaciones.Disponibles[i]='1'
  }
  if( (grepl("(doppia)",datos2$Habitaciones.Disponibles[i]  ))|
      (grepl("(doppie)",datos2$Habitaciones.Disponibles[i]  ))|  
      (grepl("(posto letto)",datos2$Habitaciones.Disponibles[i]  ))
  ){
    datos2$Habitaciones.Disponibles[i]='0'
  }
  if( (grepl("(Intero)",datos2$Habitaciones.Disponibles[i]  ))|
      (grepl("(intero)",datos2$Habitaciones.Disponibles[i]  ))|
        (grepl("(Mini)",datos2$Habitaciones.Disponibles[i]  ))|
        (grepl("(monolocale)",datos2$Habitaciones.Disponibles[i]  ))
  ){
    datos2$Habitaciones.Disponibles[i]='2'
  }
  ###########
  #numerizando la columna Tipo.de.Inmueble
  #
  #0 = MiniApartamento, 1 = Apartamento, 2 = Monolocale
  ###########
  if( datos2$Tipo.de.Inmueble[i] =="Mini Appartamento" ){
    datos2$Tipo.de.Inmueble[i]='0'
  }
  if( datos2$Tipo.de.Inmueble[i] =="Appartamento"){
    datos2$Tipo.de.Inmueble[i]='1'
  }
  if( datos2$Tipo.de.Inmueble[i] =="Monolocale"){
    datos2$Tipo.de.Inmueble[i]='2'
  }
  
  ###########
  #Agregando Disponibilidad (ragazze o ragazzi )->a las columna disponible
  #
  #0 disponible para ragazze, 1 ragazzi, y 2 para ambos
  ###########
  if( (grepl("(ragazze/ragazzi)", datos2$Notas[i]))| (grepl ("(ragazzi/ragazze)", datos2$Notas[i]))
    | (grepl("(ragazze/i)", datos2$Notas[i])) | (grepl("(ragazzi/e)", datos2$Notas[i])) ){
     
    datos2$Disponible[i]='2'
  }else{
     if (grepl("(ragazzi)", datos2$Notas[i])){
        datos2$Disponible[i]='1'
      }
      else{
        datos2$Disponible[i]='0'
  
      }
  }
  ###########
  #Agregando Caracteristicas de Columna Descripcion ->a las columnas 
  #Camera,Bagno,Cucina, Salone,Balcone, Terrazza, Ingresso 
  ###########
  if( (grepl("(cucina)", datos2$Descripción[i]))|
      (grepl("(cucna)", datos2$Descripción[i]))|
      (grepl("(cottura)", datos2$Descripción[i]))
    ){
    datos2$Cucina[i]=1
  }
  if( (grepl("(sala)", datos2$Descripción[i]))| 
    (grepl("(salone)", datos2$Descripción[i]))|
     (grepl("(salotto)", datos2$Descripción[i]))|
      (grepl("(salottino)", datos2$Descripción[i]))
    ){
    datos2$Salone[i]=1
  }
  if( (grepl("(Ingresso)", datos2$Descripción[i])) |
      (grepl("(ingresso)", datos2$Descripción[i]))
    ){
    datos2$Ingresso[i]=1
  }
  
  if( (grepl("(Ingresso)", datos2$Descripción[i])) |
      (grepl("(ingresso)", datos2$Descripción[i]))
  ){
    datos2$Ingresso[i]=1
  }
  if( (grepl("(balcone)", datos2$Descripción[i])) |
      (grepl("(balconcino)", datos2$Descripción[i]))
  ){
    datos2$Balcone[i]=1
  }
  if (grepl("(terrazzo)", datos2$Descripción[i])) {
    datos2$Terrazzo[i]=1
  }
  if (grepl("(rispotiglio)", datos2$Descripción[i])) {
    datos2$Rispotiglio[i]=1
  }
  if (grepl("(internet)", datos2$Descripción[i])) {
    datos2$Internet[i]=1
  }
  
  #Camera e Bagno pueden ser mas de 1
  if( (grepl("(bagno)", datos2$Descripción[i])) ) {
    datos2$Bagno[i]=1
  }
  if( (grepl("(2 bagni)", datos2$Descripción[i])) ) {
    datos2$Bagno[i]=2
  }
  if( (grepl("(3 bagni)", datos2$Descripción[i])) ) {
    datos2$Bagno[i]=3
  }
  
  if (grepl("(camera)", datos2$Descripción[i])) {
    datos2$Camera[i]=1
  }
  if( (grepl("(2 camere)", datos2$Descripción[i])) |
      (grepl("(2 camer)", datos2$Descripción[i])) 
    ){
    datos2$Camera[i]=2
  }
  if( (grepl("(tre camere)", datos2$Descripción[i])) |
      (grepl("(3 camere)", datos2$Descripción[i])) ){
    datos2$Camera[i]=3
  }
  if( (grepl("(4 camere)", datos2$Descripción[i])) |
        (grepl("(4 stanze)", datos2$Descripción[i])) 
    ){
    datos2$Camera[i]=4
  }
  if (grepl("(5 camere)", datos2$Descripción[i])) {
    datos2$Camera[i]=5
  }
  ##############################
  #Fin de Descripcion 
  ##############################
  
  ##############################
  #Agregando Caracteristicas de Precio.Mensual a ->Internet, Condominio y Riscaldamento
  # y cambiando columna de Precio solo con el precio de la vivienda
  ##############################
  if( (grepl("(internet)", datos2$Precio.Mensual[i])) |
      (grepl("(TUTTO)", datos2$Precio.Mensual[i]))   
    ){
    datos2$Internet[i]=1##MALO
  }
  if( (grepl("(condominio)", datos2$Precio.Mensual[i]))|
      (grepl("(conominio)", datos2$Precio.Mensual[i])) |
      (grepl("(TUTTO)", datos2$Precio.Mensual[i]))
    ){
    datos2$Condominio[i]=1
  } 
  if( (grepl("(riscaldamento)", datos2$Precio.Mensual[i])) |
        (grepl("(TUTTO)", datos2$Precio.Mensual[i]))
    ){
    datos2$Riscaldamento[i]=1
  } 
  if( (grepl("(acqua)", datos2$Precio.Mensual[i])) |
      (grepl("(TUTTO)", datos2$Precio.Mensual[i]))
    ){
    datos2$Acqua[i]=1
  } 

}
#END FOR

#Eliminar Columnas que ya no usaremos

datos2$Notas= NULL
datos2$Descripción=NULL

#Transformar Columna de precio y guardar los precios de las viviendas
#
for(i in (seq(1:nrow(datos2)) ))
{
  x<- datos2$Precio.Mensual[i]
  datos2$Precio.Mensual[i]=as.numeric(gsub("\\D", "", x))
}

#names(datos2)


#Generando el Modelo para Elegir vivienda segun sea el caso per"Ragazzi" o "Ragazze"
#Modelo para Ragazzi:
#Tiene que ser o Singola o Compartida, tener Cocina, Bagno y HAbitacion 
# y tiene que tener acceso a la ruta Destino que en este caso es la Universidad
for(i in (seq(1:nrow(datos2)) ))
{
#Dividir la data entre las viviendas disponibles para muchachos 
  if( ((datos2$Disponible[i]=='2')|( datos2$Disponible[i]=='1') )&
        (datos2$Distancia[i]!=='0') & (datos2$Camera=='1') & (datos2$Bagno=='1') & (datos2$Cucina==1) & 
      (datos2$Habitaciones.Disponibles==1|datos2$Habitaciones.Disponibles==0 ) 
      ){
    print(datos2$Dirección[i])
        print(datos2$Distancia[i])
        print(datos2$Precio.Mensual[i])
  }
  
}
#Modelo para Ragazze
for(i in (seq(1:nrow(datos2)) ))
{
  #Dividir la data entre las viviendas disponibles para muchachos 
  if( ((datos2$Disponible[i]=='2')|( datos2$Disponible[i]=='0')) & (datos2$Distancia[i]!=='0') ){
    print(datos2$Dirección[i])
    print(datos2$Distancia[i])
    print(datos2$Precio.Mensual[i])
  }
}
#Funcion para buscar Numero dentro de in string
#x<-datos2$Precio.Mensual[1]
#as.numeric(gsub("\\D", "", x))

#Funcion Grepl para buscar coincidencias dentro de Descripcion**
#if(grepl("ab", "aacabd")) print("found") else print("not found")