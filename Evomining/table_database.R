library("readr")
#cargamos archivo
gtdbK_file <- read_tsv('Datos/gtdbtk.bac120.summary.tsv')
genome_example<-"700mSIPHEX2-concot_9"
#--------------------------------------------------------------------------------
#Funcion da un dataframe con id_numero",	"feature_id","user_genome", "gtdbk" y "database
#id_numero     feature_id   user_genome                                gtdbk  database
#1   2200000 666666.2200000 700mSIPHEX2_9 Paracoccus_sp000967825_700mSIPHEX2_9 family
user_id <- function(table,id){ 
  #busco la fila del id en gtdbk_file en la columna user_genome
  #Ejemplo: A tibble: 1 × 21
  #user_genome    class…¹ fasta…² fasta…³ fasta…⁴ fasta…⁵ fasta…⁶ close…⁷ close…⁸ close…⁹ close…˟ pplac…˟ class…˟ note  other…˟ aa_pe…˟
  #<chr>          <chr>   <chr>   <chr>   <chr>   <chr>   <chr>   <chr>   <chr>   <chr>   <chr>   <chr>   <chr>   <chr> <chr>     <dbl>
  #700mSIPHEX1-c… d__Bac… GCF_00… 95.0    d__Bac… 96.36   0.87    GCF_00… d__Bac… 96.36   0.87    d__Bac… ANI/Pl… topo… GCF_90…    54.6
  x<-table[table$user_genome == id,]
  #Encontrar en la fila la asignacion de gtdbk en clasification y separar por ";". Ejemplo:
  #[[1]]
  #[1] "d__Bacteria"                  "p__Proteobacteria"            "c__Alphaproteobacteria"       "o__Rhodobacterales"          
  #[5] "f__Rhodobacteraceae"          "g__Sulfitobacter"             "s__Sulfitobacter sp001634775"
  w<-strsplit(x$classification, ";")
  #En un if anidado, encontrar la especie correspondiente si está asignada, si no asignar genero, familia. Ejemplo:
  #[[1]]
  #[1] "s__Sulfitobacter sp001634775"
  specie<-lapply(w,`[[`, 7)
  f<-specie
  if(specie=="s__") {
    f<-lapply(w,`[[`, 6)
    if(f=="g__"){
      f<-lapply(w,`[[`, 5)
    }
  }
  #recortar solo la especie y y separarla pro guion bajo. Ejemplo
  #[1] "Sulfitobacter_sp001634775"
  s<-strsplit(as.character(f), "__")
  sp<-s[[1]][2]
  g_sp<- gsub(" ", "_", sp)
  #cambiar el - por _ en el id que esta en user_genome
  id_completo<-gsub("-", "_",x$user_genome)
  #cortar el id para que coincida con los archivos de genomas. Ejemplo:
  #[1] "700mSIPHEX1_0"
  id_split <-strsplit(id_completo, "_")
  bin_id<-paste(id_split[[1]][1],"_",id_split[[1]][3],sep = "")
  #Agregar bin_id al nombre de especie para diferenciar entre genomas. Ejemplo:
  #[1] "Sulfitobacter_sp001634775_5mSIPHEX2_25"
  sp_rast<-paste(g_sp,bin_id,sep = "_")
  #Asignamos un numero de id basado en el index
  xvalue<-which(table == id, arr.ind = TRUE)
  x_row<-xvalue[1]
  row_size<- nchar(x_row)
  if(nchar(x_row) == 1 ){
    row_size<- as.numeric(x_row)*100000
  } else {
    if(nchar(x_row) == 2 ){
      row_size<- as.numeric(x_row)*11000
    }
  }
  value_id<-format(row_size, scientific = FALSE)
  #value_id<-format(as.numeric(x_row)*100000, scientific = FALSE)
  feature_id<-paste("666666.",value_id,sep = "")
  #sacar la familia y clasificarla en la db
  family<-lapply(w,`[[`, 5)
  orden<-lapply(w,`[[`, 4)
  #Hacer dataframe vacio y llenarlo.
  df <- data.frame(matrix(ncol = 6, nrow = 0))
  colnames(df) <-c("id_numero",	"feature_id","user_genome","gtdbk_specie", "database", "orden")
  #rellenar las filas de el df
  df[1,] <-c( value_id, feature_id, bin_id,	sp_rast, family, orden)
  return (df)
}

user_id(gtdbK_file,genome_example)
#-----------------------------------------------------------------------------------
#aplicar para una lista de ids
#lista:
userg_list<-gtdbK_file$user_genome

library(plyr)
taxon_assig<- ldply(.data =userg_list,
                    .fun= function(x) user_id(gtdbK_file,x))

taxon_assig

newdata <- taxon_assig[order(taxon_assig$database),]
newdata
#rast_ids<-select(taxon_assig, "id_numero", "feature_id", "gtdbk")
#rast_ids
#------------------------------------------------------------------------------------
famis<-function(file){
  x<-file$classification
  w<-strsplit(x, ";")
  fam<-lapply(w,`[[`, 5)
  specie<-lapply(w,`[[`, 7)
  e<-lapply(w,`[[`, 3)
  ug<-file$user_genome
  ret<-paste(fam, specie,ug)
  return(sort(ret))
}
famis(gtdbK_file)

#----------------------------------------------------------------------------------
#simular un archivo de rast ids sin nombres de columnas
#100000	666666.100000	700mSIPHEX1_15	Oleibacter_sp002733645_700mSIPHEX1_15
write.table(taxon_assig , file =  "Datos/database_table.IDs", sep = "\t", dec = ".",
            row.names = FALSE, col.names = FALSE, quote=FALSE)

#------------------------------------------------------------------------------------
