#una funcion que te de archivo de entrada  y una lista de salida

#cargamos el archivo
Secuencias_file <- readLines('Datos/5mSIPHEX1_0_short.faa')

#creamos una funcion
Idlist<- function(file){
  #identificamos todas las lineas que contienen ">" 
  grep_index <-grep(">",file)
  #le pedimos esas lineas al archivo
  ID_lines <- file[grep_index]
  #recortamos por el  " #"
  Id <-strsplit(ID_lines, " #")
  #de cada seleccion elegir la linea [1]
  ID <-sapply(Id, `[`, 1)
  #el valor numerico de cada linea 
  #grep_index <-grep(">",file)
  #ID<-Id[1][1]
  return (ID)
}
#Corremos la funcion con el
Idlist(Secuencias_file)

#Esta función sirve para correr la funcion con una lista de los Id
library(plyr)
llply(.data = Secuencias_file,
      .fun= function(x) Idlist(Secuencias_file))


