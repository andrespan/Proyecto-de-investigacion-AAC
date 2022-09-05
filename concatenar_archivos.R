
#buscar con grep el identicficador 2
identificador2 <- ">5mSIPHEX1_0-scaffold_1104_c1_2"
identificador2
#--------------------------------------------------------------------------------
#cargar el archivo de secuencias
Secuencias_file <- readLines('Datos/5mSIPHEX1_0_short.faa')
class(Secuencias_file)
#-------------------------------------------------
#una lista de 2 identificadores
Identificadores <-
class(Identificadores)
#Obtener ordenadamente ID, coordendas 1 y 2 y aminoacidos
#-------------------------------------------------------------------------------
#function
archivo_txt <- function(file){ 
 
  #Encontrar la linea completa  del identificador2 
  #Ejemplo
  #">5mSIPHEX1_0-scaffold_1104_c1_2 # 235 # 1749 # 1 # ID=11_2;partial=00;
  #start_type=ATG;rbs_motif=AGGAG;rbs_spacer=5-10bp;gc_cont=0.637"
  grep_line <- grep(value = TRUE, identificador2,file)
  #Encontrar solo el ID
  Id <-strsplit(grep_line, " #")
  ID<-Id[[1]][1]
  #encontrar el index de identificador2
  grep_index <-grep(identificador2,Secuencias_file)
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

archivo_txt(Secuencias_file)

grep_line <- grep(value = TRUE, identificador2,file)
grep_line

#Prueba
library(plyr)
ldply(.data = Secuencias_file,
      .fun= function(x) archivo_txt(x))

#obtieniendo un archivo txt con el ID, las cordenadas ,la anotacion y la secuencia
#Y un archivo fasta con identificadores de RAST



#imprime las lineas desde el primero hasta el segundo ID
#path[grep(identificador1,path):grep(identificador1,path)]
#imprime hasta la segunda linea del archivo
#name <-Secuencias_fileSecuencias_filegrep(identificador1,path)]
#name
#ID <- substring (name, 1, 32)
#coordenadas <- substring (name, 32, 56)  
#ID
#coordenadas
#name_numeric  <- as.numeric(as.factor(name))
#sequence <- for (name in path) {nchar(identificador1)+1}
#sequence

#path[grep(identificador1,path)]




#name 
#path
#identificador1
#setwd() y getwd()


#identificadortotal <-">*"
#identificadortotal
#path <- readLines('Datos/5mSIPHEX1_0_short.faa')

#name2 <- path[grep(identificadortotal,path)]
#ID2 <- substring (name2, 1, 32)
#ID2
#path <- read.delim('Datos'/'5mSIPHEX1_0_short.faa',header=TRUE,sep = " ")
#path <- read.table(file('Datos'/'5mSIPHEX1_0_short.faa'),row.names=0,sep='\t')
#name_numeric  <- grep(identificador1,path) 

##buscar con grep un identificador 
#identificador1 <- ">*-scaffold_*_*_*"
#identificador1
#as.numeric(as.factor(identificador1))