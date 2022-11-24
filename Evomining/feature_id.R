#crear columna feature id

# los feature_id van a ir 100000 20000000....420000
library("readr")
gtdbK_file <- read_tsv('Datos/gtdbtk.bac120.summary.tsv')
colnames(gtdbK_file)
gtdbK_file$classification
gtdbK_file$user_genome
gtdbK_file$fastani_taxonomy
gtdbK_file$closest_placement_taxonomy
gtdbK_file$pplacer_taxonomy
gtdbK_file$

genome_example<-"700mSIPHEX1-concot_0"

user_id <- function(file,id){ 
  grep_line <- grep(value = TRUE, id,file)
  #cambiar el - por _
  id_completo<-gsub("-", "_", grep_line)
  #cortar el id
  id_split <-strsplit(id_completo, "_")
  ID<-paste(id_split[[1]][1],"_",id_split[[1]][3],sep = "")
  #Encontrar la asignacion de gtdbk
  gtdbk_name <- file$classification
  splitname <- strsplit(gtdbk_name, "__")
  genera <- splitname[[1]][1]
  #ifelse( < 0, "-", "+")
  #separar por __ y quedarnos con genero o specie
  #Ejemplo: ">5mSIPHEX1_0-scaffold_1104_c1_2 # 235 # 1749 # 1 # ID=11_2;
  #partial=00;start_type=ATG;rbs_motif=AGGAG;rbs_spacer=5-10bp;gc_cont=0.637"
  #grep_line <- grep(value = TRUE, id,file)
  #ID<-Id[[1]][1]
  #quitar el > al id_completo
  #id_mod<-gsub('>', '',id_completo)
  #encontrar el index de identificador2
  #grep_index <-grep(id,Secuencias_file)
  #obtener secuencia de aminoacidos
  #aminoacid_sec <- Secuencias_file[grep_index+1]
  #aminoacid<-gsub('[*]', '',aminoacid_sec)
  #cordenada1
  #element <-strsplit(grep_line, "#")
  #element[[1]][2]
  #coordenada1 <- as.integer( element[[1]][2])
  #cordenada2
  #element[[1]][3]
  #coordenada2 <- as.integer( element[[1]][3])
  #Calcular columna "strand" restando las columnas de coordenadas stop-start y 
  #arrojar una columna con + o -  si el resultado es positivo o negativo
  #resta_dir <- coordenada2 - coordenada1
  #strand <- as.character(ifelse(resta_dir < 0, "-", "+"))
  #agregar funcion a partir del df k0
  #grep_id <-df[grepl(id_mod, df$Scaffold_name)|is.na(df$Scaffold_name), ]
  #regresar la fila 3 que es annotation = metabolic
  #metabolic<-grep_id[3]
  #agregar a contig_id el id_completo hasta el numero despues de "scaffold"
  #contig <-strsplit(id_mod, "_")
  #contig_id<-contig[[1]][4]
  #Hacer un dataframe vacio
  df <- data.frame(matrix(ncol = 4, nrow = 0))
  colnames(df) <-c("id_numero",	"feature_id","user_genome","gtdbk")
  #rellenar las filas de el df
  df[1,] <-c("id_num", "feature_id",	ID,	"gtdbk_name")
  return (df)
}

user_id(gtdbK_file,genome_example)
