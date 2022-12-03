######Funcion convertir a rast integrada a lapply_con_funcion/"funcion final"###################
#creamos una variable que tiene un solo un id, el identificador 2
identificador2 <- ">5mSIPHEX1_0-scaffold_1104_c1_2"
#identificador2
#n<-df_1235$species[1]
#name<-rast_ids[rast_ids$V4 == n,]
#id_num<-name$V1
#--------------------------------------------------------------------------------
#cargamos el archivo de las secuencias
Secuencias_file <- readLines('Datos/5mSIPHEX1_0.faa')
#class(Secuencias_file)
#-------------------------------------------------------------------------------
#cargamos el archivo de los rastids 
rast_ids<-read.table("Datos/rast_namesid.IDs",colClasses = "character")
rast_ids$V3
#feature_id<-"666666.100326"
x<-rast_ids[rast_ids$V3 == ig,]
x
1:nrow(rast_ids)
xvalue<-which(x == ig, arr.ind = TRUE)
xvalue
sp<-x$V4
specie<-lapply(sp,`[[`, 1)
specie
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
xvalue<-which(listof_ids == identificador2, arr.ind = TRUE)
xvalue
#-------------------------------------------------------------------------------
# Esta funcion tiene un archivo y te regresa un dataframe con ID, coordendas 1 y 2 y aminoacidos en orden
#function
archivo_txt <- function(file2,file,list,id){ 
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
  #grep_id <-df[grepl(id_mod, df$Scaffold_name)|is.na(df$Scaffold_name), ]
  #regresar la fila 3 que es annotation = metabolic
  #metabolic<-grep_id[3]
  #agregar a contig_id el id_completo hasta el numero despues de "scaffold"
  contig <-strsplit(id_mod, "_")
  contig_id<-contig[[1]][4]
  #hacer feature id
  identif_g<-strsplit(gsub("-","_",id),"_")
  ig<-gsub(">","",gsub(" ","_",paste(identif_g[[1]][1],identif_g[[1]][2])))
  #feature_id<-"666666.100326"
  x<-rast_ids[file2$V3 == ig,]
  #xvalue<-which(file == grep_line)
  xvalue<-which(list == id, arr.ind = TRUE)
  x_row<-xvalue[1]
  feature<-paste("fig|",x$V2,".peg.",x_row,sep = "")
  #calcular especie
  specie<-lapply(x$V4,`[[`, 1)
  speciesin_<-gsub("_"," ",specie)
  #Hacer un dataframe vacio
  df <- data.frame(matrix(ncol = 13, nrow = 0))
  colnames(df) <-c("contig_id",	"feature_id",	"type",	"location", "start",	"stop", "strand",	"locus_tag",	"figfam",	"species",	"nucleotide_sequence",	"amino_acid",	"sequence_accession")
  #rellenar las filas de el df
  df[1,] <-c(contig_id,	feature,	"type",	"location", coordenada1, coordenada2, strand,	"unknown",	"figfam",	speciesin_,	"nuc",	aminoacid, id_mod)
  return (df)
}
#-------------------------------------------------------------------------------
#probamos la funcion para Identificador2 

archivo_txt(rast_ids,Secuencias_file,listof_ids,identificador2)
#-------------------------------------------------------------------------------
#lista de 2 ids
lista_prueba <- c(">5mSIPHEX1_0-scaffold_1104_c1_1", ">5mSIPHEX1_0-scaffold_1104_c1_2", ">5mSIPHEX1_0-scaffold_1104_c1_3", ">5mSIPHEX1_0-scaffold_1104_c1_4")
#class(lista_prueba)
#-------------------------------------------------------------------------------
#con ldply corremos la funcion para una lista que contiene todos los Id
#obtieniendo un una tabla con el ID, las cordenadas ,la anotacion y la secuencia
library(plyr)
df_1235<-ldply(.data = lista_prueba,
               .fun= function(x) archivo_txt(rast_ids,Secuencias_file,listof_ids,x))


df_1235
#-------------------------------------------------------------------------------
#Agregamos la columna "Function" al dataframe que resulta de archivo_txt entre la columnas

#incluimos la funcion ID_to_metabolic

#con la funcion read_ko buscar en la tabla la columna 3 (el numero de KO)
#install.packages("devtools")
library(devtools)
#install_github("mirnavazquez/RbiMs")
library("rbims")
#dataframe proviene del output de la funcion read_ko 
#rast_ids$V3
n<-strsplit(lista_prueba[[1]][1],"-")[[1]][1]
w<-gsub(">","",n)
k0<-read_ko(paste("Datos/02.KO_results/",w,".faa.txt",sep = ""))

#variable id de prueba
id_prueba <- "5mSIPHEX1_0_scaffold_1104_c1_2"
#id_prueba

#prueba grep
#grep("5mSIPHEX1_0_scaffold_1104_c1_2", My.Data$Scaffold_name)
#grep_id <-k0[grep("5mSIPHEX1_0_scaffold_1104_c1_2", k0$Scaffold_name), ]
#grep_id

#funcion atomo 
# doy un ID y regresar una function metabolica
#5mSIPHEX1_0-scaffold_1104_c1_2 K02056
ID_to_metabolic <-function(id,df){
  grep_id <-df[grep(id, df$Scaffold_name), ]
  #regresar un dataframe que contenga ID y metabolic
  metabolic<-grep_id[3]
  #agregar un > al id
  #id_completo <- paste(">",id,sep = "")
  dataframe <- data.frame(id,metabolic)
  return(dataframe)
}

ID_to_metabolic(id_prueba,k0)

#ahora lo voy a aplicar a una lista de IDs 
Lista_IDs <- k0$Scaffold_name
Lista_IDs

#aplicamos funcion para todos los ids
library(plyr)
ko_df<- ldply(.data =Lista_IDs,
              .fun= function(x) ID_to_metabolic(x,k0))

ko_df
colnames(ko_df)[2] <- "function"

#Unir el df de la funcion ID_to_metabolic y archivo_txt con merge

df = merge(x = df_1235 , y = ko_df, by.y = 1, by.x=13, all.x = TRUE)
df
#df$sequence_accession<-gsub('>', '',df$sequence_accession)
#df

#cambiar la columna 5 a la 4 con select()
library(dplyr)
dat_2 <- select(df,"contig_id",	"feature_id",	"type",	"location", "start",	"stop", "strand","function",	"locus_tag",	"figfam",	"species",	"nucleotide_sequence",	"amino_acid",	"sequence_accession")
dat_2


#-------------------------------------------------------------------------------
#creamos un archivo fasta que tenga el feature_id y las secuencia de aminoacidos
#la columna 2 "feature_id" del tsv de rast y amino_acid


#Xfasta <- character(nrow(dat_2) * 2)
#>gi|666666.100335.1|666666.100335|BINSIP5_0_Pacificitalea|NA|BINSIP5_0_Pacificitalea
#LAEGANAKVLYGAGWVGVTKDNMADYDF
#Xfasta[c(TRUE, FALSE)] <- paste(">gi",gsub(".peg","",dat_2$feature_id),"|",dat_2$species,"|NA|",dat_2$species,sep = "")
#Xfasta[c(FALSE, TRUE)] <- dat_2$amino_acid

#cramos el archivo 
#file_fasta <- writeLines(Xfasta, "Archivos_convertidos/aminoacid_file.fasta")
#file_fasta
#--------------------------------------------------------------------------------
#Para crear el archivo de rast
#usar el nombre de numero
#n<-dat_2$species[1]
#name<-rast_ids[rast_ids$V4 == n,]
#id_num<-name$V1
#write.table(dat_2, paste("Archivos_convertidos/",id_num,".tsv"), append = TRUE, sep = '\t', dec = ".",
#           row.names = FALSE, col.names = TRUE, quote=FALSE)



#-------------------------------------------------------------------------------
#--------------------------------------------------------------------------------
#Para crear el archivo de rast
#usar el nombre de numero

#n<-dat_2$species[1]
rast_ids$V3
name<-rast_ids[rast_ids$V3 == w,]
name
id_num<-name$V1
rastable<-write.table(dat_2, paste("Archivos_convertidos_prueba/",id_num,".txt",sep = ""), append = TRUE, sep = '\t', dec = ".",
                      row.names = FALSE, col.names = TRUE, quote=FALSE)
rastable
#------------------------------------------------------------------------------
#Para crear el .faa
Xfasta <- character(nrow(dat_2) * 2)
#>fig|666666.100001.peg.4834
#MNGTDVFASQAFARVMDRTREIYDIVVIDTPPVLVVPDARVIAQLADAVLFVVRWDSTLK
Xfasta[c(TRUE, FALSE)] <- paste(">", dat_2$feature_id, sep = "")
Xfasta[c(FALSE, TRUE)] <- dat_2$amino_acid
file_fasta <- writeLines(Xfasta, paste("Archivos_convertidos_prueba/",id_num,".faa",sep = ""))

file_fasta



