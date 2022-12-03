#una funcion que te de archivo de entrada  y una lista de salida

#cargamos el archivo
Secuencias_file <- readLines('Datos/5mSIPHEX1_0_short.faa')

#La funcion genera a partir de un archivo de secuencias una lista con los identificadores
#Ejemplo : 
# ">5mSIPHEX1_0-scaffold_1104_c1_1" ">5mSIPHEX1_0-scaffold_1104_c1_2"

Idlist<- function(file){
  #identificamos todas las lineas que contienen ">" 
  grep_index <-grep(">",file)
  #le pedimos esas lineas al archivo
  ID_lines <- file[grep_index]
  #recortamos por el  " #"
  Id <-strsplit(ID_lines, " #")
  #de cada seleccion elegir la linea [1]
  ID <-sapply(Id, `[`, 1, simplify = FALSE)
  #el valor numerico de cada linea 
  #grep_index <-grep(">",file)
  #ID<-Id[1][1]
  return (ID)
}
#Corremos la funcion con el
type <- Idlist(Secuencias_file)
type

class(type)
str(type)

#Esta función sirve para correr la funcion con una lista de los Id
library(plyr)
llply(.data = Secuencias_file,
      .fun= function(x) Idlist(Secuencias_file))


