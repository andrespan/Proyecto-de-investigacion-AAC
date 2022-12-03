
#creamos una variable que tiene un solo un id, el identificador 2
identificador2 <- ">5mSIPHEX1_0-scaffold_1104_c1_2"
#identificador2
#--------------------------------------------------------------------------------
#cargamos el archivo de las secuencias
Secuencias_file <- readLines('Datos/5mSIPHEX1_0.faa')
#class(Secuencias_file)
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
  ID <-sapply(Id, `[`, 1)
  #el valor numerico de cada linea 
  #grep_index <-grep(">",file)
  return (ID)
}

#Corremos la funcion con el archivo
listof_ids<- Idlist(Secuencias_file)
listof_ids


#-------------------------------------------------------------------------------
#con la funcion read_ko buscar en la tabla la columna 3 (el numero de KO)
#install.packages("devtools")
#library(devtools)
#install_github("mirnavazquez/RbiMs")
library("rbims")
#dataframe proviene del output de la funcion read_ko 
k0<-read_ko("Datos/02.KO_results/5mSIPHEX1_0.faa.txt")
k0
#-------------------------------------------------------------------------------
# Esta funcion tiene un archivo y te regresa un dataframe con ID, coordendas 1 y 2 y aminoacidos en orden
#function
archivo_txt <- function(file,df,id){ 
  #Encontrar la linea completa  del identificador2 
  #Ejemplo: ">5mSIPHEX1_0-scaffold_1104_c1_2 # 235 # 1749 # 1 # ID=11_2;
  #partial=00;start_type=ATG;rbs_motif=AGGAG;rbs_spacer=5-10bp;gc_cont=0.637"
  grep_line <- grep(value = TRUE, id,file)
  #Encontrar solo el ID
  Id <-strsplit(grep_line, " #")
  ID<-Id[[1]][1]
  #cambiar el - por _
  id_completo<-gsub("-", "_", ID)
  #quitar el > al id_completo
  id_mod<-gsub('>', '',id_completo)
  #encontrar el index de identificador2
  grep_index <-grep(id,Secuencias_file)
  #obtener secuencia de aminoacidos
  aminoacid_sec <- Secuencias_file[grep_index+1]
  aminoacid<-gsub('[*]', '',aminoacid_sec)
  #cordenada1
  element <-strsplit(grep_line, "#")
  element[[1]][2]
  coordenada1 <- as.integer( element[[1]][2])
  #cordenada2
  element[[1]][3]
  coordenada2 <- as.integer( element[[1]][3])
  #Calcular columna "strand" restando las columnas de coordenadas stop-start y 
  #arrojar una columna con + o -  si el resultado es positivo o negativo
  resta_dir <- coordenada2 - coordenada1
  strand <- as.character(ifelse(resta_dir < 0, "-", "+"))
  #agregar funcion a partir del df k0
  grep_id <-df[grepl(id_mod, df$Scaffold_name)|is.na(df$Scaffold_name), ]
  #regresar la fila 3 que es annotation = metabolic
  metabolic<-grep_id[3]
  #Hacer un dataframe vacio
  df <- data.frame(matrix(ncol = 14, nrow = 0))
  colnames(df) <-c("contig_id",	"feature_id",	"type",	"location", "start",	"stop", "strand","function",	"locus_tag",	"figfam",	"species",	"nucleotide_sequence",	"amino_acid",	"sequence_accession")
  #rellenar las filas de el df
  df[1,] <-c("contig_id",	"feature_id",	"type",	"location", coordenada1, coordenada2, strand, metabolic,	"unknown",	"figfam",	"species",	"nuc",	aminoacid, id_completo)
  return (df)
}
#-------------------------------------------------------------------------------
#probamos la funcion para Identificador2 
archivo_txt(Secuencias_file,k0,identificador2)
#-------------------------------------------------------------------------------
#lista de 2 ids
lista_prueba <- c(">5mSIPHEX1_0-scaffold_1104_c1_2", ">5mSIPHEX1_0-scaffold_23_c1_3")
class(lista_prueba)
#-------------------------------------------------------------------------------
#con ldply corremos la funcion para una lista que contiene todos los Id
#obtieniendo un una tabla con el ID, las cordenadas ,la anotacion y la secuencia
library(plyr)
df_1235<-ldply(.data = listof_ids,
               .fun= function(x) archivo_txt(Secuencias_file,k0,x))

df_1235
#-------------------------------------------------------------------------------
#Agregamos la columna Funcion al dataframe que resulta de archivo_txt entre la columna 3 y 4

#incluimos la funcion ID_to_metabolic

#con la funcion read_ko buscar en la tabla la columna 3 (el numero de KO)
#install.packages("devtools")
library(devtools)
install_github("mirnavazquez/RbiMs")
library("rbims")
#dataframe proviene del output de la funcion read_ko 
k0<-read_ko("Datos/02.KO_results/5mSIPHEX1_0.faa.txt")
k0
class(k0)

#variable id de prueba
id_prueba <- "5mSIPHEX1_0_scaffold_1104_c1_2"
id_prueba

#prueba grep
#grep("5mSIPHEX1_0_scaffold_1104_c1_2", My.Data$Scaffold_name)
grep_id <-k0[grep("5mSIPHEX1_0_scaffold_1104_c1_2", k0$Scaffold_name), ]
grep_id

#funcion atomo 
# doy un ID y regresar una function metabolica
#5mSIPHEX1_0-scaffold_1104_c1_2 K02056
ID_to_metabolic <-function(id,df){
  grep_id <-df[grep(id, df$Scaffold_name), ]
  #regresar un dataframe que contenga ID y metabolic
  metabolic<-grep_id[3]
  #agregar un > al id
  id_completo <- paste(">",id,sep = "")
  dataframe <- data.frame(id_completo,metabolic)
  return(dataframe)
}

ID_to_metabolic(id_prueba,k0)

#ahora lo voy a aplicar a una lista de IDs 
Lista_IDs <- k0$Scaffold_name
#Lista_IDs

#aplicamos funcion para todos los ids
library(plyr)
ko_df<- ldply(.data =Lista_IDs,
              .fun= function(x) ID_to_metabolic(x,k0))

ko_df
#Unir el df de la funcion ID_to_metabolic y archivo_txt con merge

df = merge(x = df_1235 , y = ko_df, by.y = 1, by.x=14, all.x = TRUE)
df
#cambiar la columna 5 a la 4 con select()
library(dplyr)
dat_2 <- select(df,ID, coordenada1, coordenada2,KO,aminoacid_sec)
dat_2


#-------------------------------------------------------------------------------
#creamos un archivo fasta que tenga el id y las secuencia de aminoacidos
#creamos una variable de tipo caracter

Xfasta <- character(nrow(dat_2) * 2)
Xfasta[c(TRUE, FALSE)] <- dat_2$ID
Xfasta[c(FALSE, TRUE)] <- dat_2$aminoacid_sec
head(Xfasta)
#cramos el archivo 
file_fasta <- writeLines(Xfasta, "Archivos_convertidos/aminoacid_file.fasta")
file_fasta
#--------------------------------------------------------------------------------
#Para crear el archivo de rast

write.table(dat_2, "Archivos_convertidos/rast_file.tsv", append = TRUE, sep = '\t', dec = ".",
            row.names = FALSE, col.names = TRUE)
#write.csv(dat_2, file="file.csv")
#Xrast <- character(nrow(dat_2) * 2)
#Xrast[c(TRUE, FALSE)] <- dat_2
#Xrast
#write.table(Xrast, "rast_file.fasta")

#-------------------------------------------------------------------------------


#Hacer una funcion que convierta el identificador de la secuencia del
#archivo fasta en la columna 2 del tsv de rast
library("readr")
Rast_tsv <- read_tsv("Ejemplos/BINSIP5_0_rast_file.tsv")
Xfasta2 <- character(nrow(dat_2) * 2)
Xfasta2[c(TRUE, FALSE)] <- paste(">", Rast_tsv$feature_id, sep = "")
Xfasta2[c(FALSE, TRUE)] <- dat_2$aminoacid_sec
head(Xfasta2)

feat_id_faa <- writeLines(Xfasta2, "Archivos_convertidos/featureid_file.faa")
feat_id_faa
#-------------------------------------------------------------------------------
#Calcular columna "strand" restando las columnas de coordenadas stop-start y 
#arrojar una columna con + o -  si el resultado es positivo o negativo

resta_dir <- Rast_tsv$stop - Rast_tsv$start
head(resta_dir)    
class(resta_dir)
resta_dir

Rast_tsv$strand <- as.character(ifelse(resta_dir < 0, "-", "+"))
write.table(Rast_tsv, "Archivos_convertidos/rast_wstrand.tsv", append = TRUE, sep = '\t', dec = ".",
            row.names = FALSE, col.names = TRUE)

#creamos una variable que tiene un solo un id, el identificador 2
identificador2 <- ">5mSIPHEX1_0-scaffold_1104_c1_2"
#identificador2
#--------------------------------------------------------------------------------
#cargamos el archivo de las secuencias
Secuencias_file <- readLines('Datos/5mSIPHEX1_0.faa')
#class(Secuencias_file)
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
  ID <-sapply(Id, `[`, 1)
  #el valor numerico de cada linea 
  #grep_index <-grep(">",file)
  return (ID)
}

#Corremos la funcion con el archivo
listof_ids<- Idlist(Secuencias_file)
listof_ids


#-------------------------------------------------------------------------------
#con la funcion read_ko buscar en la tabla la columna 3 (el numero de KO)
#install.packages("devtools")
#library(devtools)
#install_github("mirnavazquez/RbiMs")
library("rbims")
#dataframe proviene del output de la funcion read_ko 
k0<-read_ko("Datos/02.KO_results/5mSIPHEX1_0.faa.txt")
k0
#-------------------------------------------------------------------------------
# Esta funcion tiene un archivo y te regresa un dataframe con ID, coordendas 1 y 2 y aminoacidos en orden
#function
archivo_txt <- function(file,df,id){ 
  #Encontrar la linea completa  del identificador2 
  #Ejemplo: ">5mSIPHEX1_0-scaffold_1104_c1_2 # 235 # 1749 # 1 # ID=11_2;
  #partial=00;start_type=ATG;rbs_motif=AGGAG;rbs_spacer=5-10bp;gc_cont=0.637"
  grep_line <- grep(value = TRUE, id,file)
  #Encontrar solo el ID
  Id <-strsplit(grep_line, " #")
  ID<-Id[[1]][1]
  #cambiar el - por _
  id_completo<-gsub("-", "_", ID)
  #quitar el > al id_completo
  id_mod<-gsub('>', '',id_completo)
  #encontrar el index de identificador2
  grep_index <-grep(id,Secuencias_file)
  #obtener secuencia de aminoacidos
  aminoacid_sec <- Secuencias_file[grep_index+1]
  aminoacid<-gsub('[*]', '',aminoacid_sec)
  #cordenada1
  element <-strsplit(grep_line, "#")
  element[[1]][2]
  coordenada1 <- as.integer( element[[1]][2])
  #cordenada2
  element[[1]][3]
  coordenada2 <- as.integer( element[[1]][3])
  #Calcular columna "strand" restando las columnas de coordenadas stop-start y 
  #arrojar una columna con + o -  si el resultado es positivo o negativo
  resta_dir <- coordenada2 - coordenada1
  strand <- as.character(ifelse(resta_dir < 0, "-", "+"))
  #agregar funcion a partir del df k0
  grep_id <-df[grepl(id_mod, df$Scaffold_name)|is.na(df$Scaffold_name), ]
  #regresar la fila 3 que es annotation = metabolic
  metabolic<-grep_id[3]
  #Hacer un dataframe vacio
  df <- data.frame(matrix(ncol = 14, nrow = 0))
  colnames(df) <-c("contig_id",	"feature_id",	"type",	"location", "start",	"stop", "strand","function",	"locus_tag",	"figfam",	"species",	"nucleotide_sequence",	"amino_acid",	"sequence_accession")
  #rellenar las filas de el df
  df[1,] <-c("contig_id",	"feature_id",	"type",	"location", coordenada1, coordenada2, strand, metabolic,	"unknown",	"figfam",	"species",	"nuc",	aminoacid, id_completo)
  return (df)
}
#-------------------------------------------------------------------------------
#probamos la funcion para Identificador2 
archivo_txt(Secuencias_file,k0,identificador2)
#-------------------------------------------------------------------------------
#lista de 2 ids
lista_prueba <- c(">5mSIPHEX1_0-scaffold_1104_c1_2", ">5mSIPHEX1_0-scaffold_23_c1_3")
class(lista_prueba)
#-------------------------------------------------------------------------------
#con ldply corremos la funcion para una lista que contiene todos los Id
#obtieniendo un una tabla con el ID, las cordenadas ,la anotacion y la secuencia
library(plyr)
df_1235<-ldply(.data = listof_ids,
               .fun= function(x) archivo_txt(Secuencias_file,k0,x))

df_1235
#-------------------------------------------------------------------------------
#Agregamos la columna Funcion al dataframe que resulta de archivo_txt entre la columna 3 y 4

#incluimos la funcion ID_to_metabolic

#con la funcion read_ko buscar en la tabla la columna 3 (el numero de KO)
#install.packages("devtools")
library(devtools)
install_github("mirnavazquez/RbiMs")
library("rbims")
#dataframe proviene del output de la funcion read_ko 
k0<-read_ko("Datos/02.KO_results/5mSIPHEX1_0.faa.txt")
k0
class(k0)

#variable id de prueba
id_prueba <- "5mSIPHEX1_0_scaffold_1104_c1_2"
id_prueba

#prueba grep
#grep("5mSIPHEX1_0_scaffold_1104_c1_2", My.Data$Scaffold_name)
grep_id <-k0[grep("5mSIPHEX1_0_scaffold_1104_c1_2", k0$Scaffold_name), ]
grep_id

#funcion atomo 
# doy un ID y regresar una function metabolica
#5mSIPHEX1_0-scaffold_1104_c1_2 K02056
ID_to_metabolic <-function(id,df){
  grep_id <-df[grep(id, df$Scaffold_name), ]
  #regresar un dataframe que contenga ID y metabolic
  metabolic<-grep_id[3]
  #agregar un > al id
  id_completo <- paste(">",id,sep = "")
  dataframe <- data.frame(id_completo,metabolic)
  return(dataframe)
}

ID_to_metabolic(id_prueba,k0)

#ahora lo voy a aplicar a una lista de IDs 
Lista_IDs <- k0$Scaffold_name
#Lista_IDs

#aplicamos funcion para todos los ids
library(plyr)
ko_df<- ldply(.data =Lista_IDs,
              .fun= function(x) ID_to_metabolic(x,k0))

ko_df
#Unir el df de la funcion ID_to_metabolic y archivo_txt con merge

df = merge(x = df_1235 , y = ko_df, by.y = 1, by.x=14, all.x = TRUE)
df
#cambiar la columna 5 a la 4 con select()
library(dplyr)
dat_2 <- select(df,ID, coordenada1, coordenada2,KO,aminoacid_sec)
dat_2


#-------------------------------------------------------------------------------
#creamos un archivo fasta que tenga el id y las secuencia de aminoacidos
#creamos una variable de tipo caracter

Xfasta <- character(nrow(dat_2) * 2)
Xfasta[c(TRUE, FALSE)] <- dat_2$ID
Xfasta[c(FALSE, TRUE)] <- dat_2$aminoacid_sec
head(Xfasta)
#cramos el archivo 
file_fasta <- writeLines(Xfasta, "Archivos_convertidos/aminoacid_file.fasta")
file_fasta
#--------------------------------------------------------------------------------
#Para crear el archivo de rast

write.table(dat_2, "Archivos_convertidos/rast_file.tsv", append = TRUE, sep = '\t', dec = ".",
            row.names = FALSE, col.names = TRUE)
#write.csv(dat_2, file="file.csv")
#Xrast <- character(nrow(dat_2) * 2)
#Xrast[c(TRUE, FALSE)] <- dat_2
#Xrast
#write.table(Xrast, "rast_file.fasta")

#-------------------------------------------------------------------------------


#Hacer una funcion que convierta el identificador de la secuencia del
#archivo fasta en la columna 2 del tsv de rast
library("readr")
Rast_tsv <- read_tsv("Ejemplos/BINSIP5_0_rast_file.tsv")
Xfasta2 <- character(nrow(dat_2) * 2)
Xfasta2[c(TRUE, FALSE)] <- paste(">", Rast_tsv$feature_id, sep = "")
Xfasta2[c(FALSE, TRUE)] <- dat_2$aminoacid_sec
head(Xfasta2)

feat_id_faa <- writeLines(Xfasta2, "Archivos_convertidos/featureid_file.faa")
feat_id_faa
#-------------------------------------------------------------------------------
#Calcular columna "strand" restando las columnas de coordenadas stop-start y 
#arrojar una columna con + o -  si el resultado es positivo o negativo

resta_dir <- Rast_tsv$stop - Rast_tsv$start
head(resta_dir)    
class(resta_dir)
resta_dir

Rast_tsv$strand <- as.character(ifelse(resta_dir < 0, "-", "+"))
write.table(Rast_tsv, "Archivos_convertidos/rast_wstrand.tsv", append = TRUE, sep = '\t', dec = ".",
            row.names = FALSE, col.names = TRUE)
