
#buscar con grep el identicficador 2
identificador2 <- ">5mSIPHEX1_0-scaffold_1104_c1_2"
#cargar el archivo de secuencias
Secuencias_file <- readLines('Datos/5mSIPHEX1_0_short.faa')
Secuencias_file
#file <- readLines('Datos/*.faa')
#Obtener ordenadamente ID, coordendas 1 y 2 y aminoacidos

archivo_txt <- function(file){ 
  identificador2 <- ">5mSIPHEX1_0-scaffold_1104_c1_2"
  grep_line <- grep(value = TRUE, identificador2,file)
  #cordenada1
  element <-strsplit(grep_line, "#")
  element[[1]][2]
  coordenada2 <- as.integer( element[[1]][2])
  #cordenada 2
  element[[1]][3]
  coordenada1 <- as.integer( element[[1]][3])
  df <- data.frame(matrix(ncol = 2, nrow = 0))
  colnames(df) <-c("coordenada1", "coordenada2")
  #rellenar las filas de el df
  df[1,] <-c(coordenada1,coordenada2)
  return (df)
}

archivo_txt(Secuencias_file)

#Encontrar al identificador2 
#Ejemplo
#">5mSIPHEX1_0-scaffold_1104_c1_2 # 235 # 1749 # 1 # ID=11_2;partial=00;start_type=ATG;rbs_motif=AGGAG;rbs_spacer=5-10bp;gc_cont=0.637"
grep_line <- grep(value = TRUE, identificador2,Secuencias_file)
#encontrar el index
grep_index <-grep(identificador2,Secuencias_file)
#Obtener coordenadas y ID
#cordenada 2


#cordenada 1
element[[1]][2]
coordenada1 <- as.integer( element[[1]][2])



#obtener secuencia de aminoacidos
aminoacid_sec <- Secuencias_file[grep_index+1]
aminoacid_sec

#obtieniendo un archivo txt con el ID, las cordenadas ,la anotacion y la secuencia
#Y un archivo fasta con identificadores de RAST

#Ejemplo de identificador 5mSIPHEX1_0-scaffold_1104_c1_1
#5m es la profundidad
#SI Enriquecimiento
#PHEX es el  la molecula que se uso
#1_0 es el ID 
#-scaffold
#1104 es el identificador del scaffold
#c1_2  es el ID del gen

#haciendo un for para todos los IDs
for (">" in Secuencias_file) {
  
} 




#imprime las lineas desde el primero hasta el segundo ID
#path[grep(identificador1,path):grep(identificador1,path)]
#imprime hasta la segunda linea del archivo
name <-Secuencias_fileSecuencias_filegrep(identificador1,path)]
name
ID <- substring (name, 1, 32)
coordenadas <- substring (name, 32, 56)  
ID
coordenadas
#name_numeric  <- as.numeric(as.factor(name))
#sequence <- for (name in path) {nchar(identificador1)+1}
sequence

path[grep(identificador1,path)]




#name 
#path
identificador1
#setwd() y getwd()


identificadortotal <-">*"
identificadortotal
path <- readLines('Datos/5mSIPHEX1_0_short.faa')

name2 <- path[grep(identificadortotal,path)]
ID2 <- substring (name2, 1, 32)
ID2
#path <- read.delim('Datos'/'5mSIPHEX1_0_short.faa',header=TRUE,sep = " ")
#path <- read.table(file('Datos'/'5mSIPHEX1_0_short.faa'),row.names=0,sep='\t')
#name_numeric  <- grep(identificador1,path) 

##buscar con grep un identificador 
identificador1 <- ">*-scaffold_*_*_*"
identificador1
#as.numeric(as.factor(identificador1))