#un archivo de entrada  y una lista de salida
#cargamos el archivo
Secuencias_file <- readLines('Datos/5mSIPHEX1_0_short.faa')
#creamos una funcion
Idlist<- function(file){
  #identificamos todas las lineas que contienen el ID
  grep_line <- grep(value = TRUE, ">",file)
  #recortamos por el espacio
  Id <-strsplit(grep_line, " #")
  #el valor numerico de cada linea 
  grep_index <-grep(">",file)
  ID<-Id[[grep_index]][1]
  
  return (ID)
}
Idlist(Secuencias_file)

