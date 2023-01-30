library("readr")
#cargamos archive
gtdbK_file <- read_tsv('Datos/gtdbtk.bac120.summary.tsv')
#aplicar para una lista de ids
#lista:
userg_list<-gtdbK_file$user_genome
user_id <- function(table,id,fv){ 
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
  #[1] "Sulfitobacter sp001634775 5mSIPHEX2 25"
  sp_rast<-paste(sp,gsub("_"," ",bin_id))
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
  feature_id<-paste("666666.",value_id,sep = "")
  #Hacer dataframe vacio y llenarlo.
  #Hacer 3 dataframes
  #df1<-Alphaproteobacteria
  df1 <- data.frame(matrix(ncol = 4, nrow = 0))
  colnames(df1) <-c("id_numero",	"feature_id","user_genome","gtdbk")
  #df2<-Bacteroidia
  df2 <- data.frame(matrix(ncol = 4, nrow = 0))
  colnames(df2) <-c("id_numero",	"feature_id","user_genome","gtdbk")
  #df3<-Gammaproteobacteria
  df3 <- data.frame(matrix(ncol = 4, nrow = 0))
  colnames(df3) <-c("id_numero",	"feature_id","user_genome","gtdbk")
  c<-lapply(w,`[[`, 5)
  d<-c
  if(c==fv){
    df1[1,] <-c( value_id, feature_id, bin_id,	sp_rast)
  }# else if(c=="f__Alteromonadaceae") {
    #df2[1,] <-c( value_id, feature_id, bin_id,	sp_rast)
  #}else if(c=="f__Flavobacteriaceae") {
  #  df3[1,] <-c( value_id, feature_id, bin_id,	sp_rast)
  #}else if(c=="f__Hyphomonadaceae") {
  #  df3[1,] <-c( value_id, feature_id, bin_id,	sp_rast)
  #}else if(c=="f__Oleiphilaceae") {
  #  df3[1,] <-c( value_id, feature_id, bin_id,	sp_rast)
  #}
  
  family_id<-select(df1, "id_numero", "feature_id", "gtdbk")
  #Bacteroidia<-select(df2, "id_numero", "feature_id", "gtdbk")
  #Gammaproteobacteria<-select(df3, "id_numero", "feature_id", "gtdbk")
  
return(family_id)
}

library(dplyr)
library(plyr)
library(tidyverse)
taxon_ids<-ldply(.data =userg_list,
      .fun= function(x) user_id(gtdbK_file,x,"f__Saccharospirillaceae"))



write.table(taxon_ids , file =  "Datos/ids_by_family/Saccharospirillaceae_bins.IDs", sep = "\t", dec = ".",
            row.names = FALSE, col.names = FALSE, quote=FALSE)
