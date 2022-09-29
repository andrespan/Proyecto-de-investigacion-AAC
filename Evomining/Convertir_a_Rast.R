
#creamos una variable que tiene un solo un id, el identificador 2
identificador2 <- ">5mSIPHEX1_0-scaffold_1104_c1_2"
#identificador2
#--------------------------------------------------------------------------------
#cargamos el archivo de las secuencias
Secuencias_file <- readLines('Datos/5mSIPHEX1_0_short.faa')
#class(Secuencias_file)
#-------------------------------------------------
#una lista de 2 identificadores
#Identificadores <- list(">5mSIPHEX1_0-scaffold_1104_c1_1",">5mSIPHEX1_0-scaffold_1104_c1_2")
#class(Identificadores)
#Identificadores
#--------------------------------------------------------------------------------
#Con esta funcion Obtenemos una lista con todos los IDs del archivo
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
  return (ID)
}
#Corremos la funcion con el archivo
listof_ids<- Idlist(Secuencias_file)
listof_ids
#-------------------------------------------------------------------------------
# Esta funcion tiene un archivo y te regresa un dataframe con ID, coordendas 1 y 2 y aminoacidos en orden
#function
archivo_txt <- function(file,id){ 
  #Encontrar la linea completa  del identificador2 
  #Ejemplo: ">5mSIPHEX1_0-scaffold_1104_c1_2 # 235 # 1749 # 1 # ID=11_2;
  #partial=00;start_type=ATG;rbs_motif=AGGAG;rbs_spacer=5-10bp;gc_cont=0.637"
  grep_line <- grep(value = TRUE, id,file)
  #Encontrar solo el ID
  Id <-strsplit(grep_line, " #")
  ID<-Id[[1]][1]
  #encontrar el index de identificador2
  grep_index <-grep(id,Secuencias_file)
  #obtener secuencia de aminoacidos
  aminoacid_sec <- Secuencias_file[grep_index+1]
  #cordenada1
  element <-strsplit(grep_line, "#")
  element[[1]][2]
  coordenada1 <- as.integer( element[[1]][2])
  #cordenada2
  element[[1]][3]
  coordenada2 <- as.integer( element[[1]][3])
  #Hacer un dataframe vacio
  df <- data.frame(matrix(ncol = 4, nrow = 0))
  colnames(df) <-c("ID", "coordenada1", "coordenada2", "aminoacid_sec")
  #rellenar las filas de el df
  df[1,] <-c(ID, coordenada1,coordenada2,aminoacid_sec)
  return (df)
}
#-------------------------------------------------------------------------------
#probamos la funcion para Identificador2 
archivo_txt(Secuencias_file,identificador2)

#-------------------------------------------------------------------------------
#con ldply corremos la funcion para una lista que contiene todos los Id
#obtieniendo un una tabla con el ID, las cordenadas ,la anotacion y la secuencia
library(plyr)
ldply(.data = listof_ids,
      .fun= function(x) archivo_txt(Secuencias_file,x))

#-------------------------------------------------------------------------------
#Agregamos la columna function al df entre la columna 3 y 4
#incluimos la funcion ID_to_metabolic

library("rbims")
#dataframe K0 es e output de la funcion read_ko de rbims
k0<-read_ko("Datos/5mSIPHEX1_0_short.txt")
k0
#variable id de prueba
id_prueba <- "5mSIPHEX1_0_scaffold_1104_c1_2"

#prueba grep
#grep("5mSIPHEX1_0_scaffold_1104_c1_2", My.Data$Scaffold_name)
grep_id <-k0[grep("5mSIPHEX1_0_scaffold_1104_c1_2", k0$Scaffold_name), ]
grep_id

#subset(My.Data, startsWith(as.character(Scaffold_name), "5mSIPHEX1_0_scaffold_1104_c1_2"))
#library(dplyr)
#library(stringr)
#My.Data %>% filter(str_detect(Scaffold_name, '5mSIPHEX1_0_scaffold_1104_c1_2'))

#funcion atomo 
# doy un ID y regresar una function metabolica
#5mSIPHEX1_0-scaffold_1104_c1_2 K02056
ID_to_metabolic <-function(id,df){
  grep_id <-df[grep(id, df$Scaffold_name), ]
  metabolic<-grep_id[3]
  return(metabolic)
}

ID_to_metabolic(id_prueba,k0)

#ahora lo voy a aplicar a una lista de IDs 
Lista_IDs <- k0[,1]
column1
#aplicamos funcion para todos los ids
library(plyr)
ldply(.data =column1,
      .fun= function(x) ID_to_metabolic(x,k0))


