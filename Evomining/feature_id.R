#crear columna feature id

# los feature_id van a ir 100000 20000000....420000
library("readr")
#cargamos archive
gtdbK_file <- read_tsv('Datos/gtdbtk.bac120.summary.tsv')
colnames(gtdbK_file)
gtdbK_file$classification
gtdbK_file$user_genome
gtdbK_file$fastani_taxonomy
gtdbK_file$closest_placement_taxonomy
gtdbK_file$pplacer_taxonomy


genome_example<-"700mSIPHEX1-concot_0"
x<-gtdbK_file[gtdbK_file$user_genome == genome_example,]
x$classification
x
id_completo<-gsub("-", "_",x$user_genome)
id_split <-strsplit(id_completo, "_")
id_split
bin_id<-paste(id_split[[1]][1],"_",id_split[[1]][3],sep = "")
bin_id
w<-strsplit(x$classification, ";")
w
specie<-lapply(w,`[[`, 7)
genera<-lapply(w,`[[`, 6)
f<-specie
if(specie=="s__") {
  f<-lapply(w,`[[`, 6)
  if(f=="g__"){
    f<-lapply(w,`[[`, 5)
  }
}

class(f)
s<-strsplit(as.character(f), "__")
sp<-s[[1]][2]
g_sp<- gsub(" ", "_", sp)
g_sp



user_id <- function(dataframe,id){ 
  #busco la fila del id en gtdbk_file en la columna user_genome
  #Ejemplo: A tibble: 1 × 21
  #user_genome    class…¹ fasta…² fasta…³ fasta…⁴ fasta…⁵ fasta…⁶ close…⁷ close…⁸ close…⁹ close…˟ pplac…˟ class…˟ note  other…˟ aa_pe…˟
  #<chr>          <chr>   <chr>   <chr>   <chr>   <chr>   <chr>   <chr>   <chr>   <chr>   <chr>   <chr>   <chr>   <chr> <chr>     <dbl>
  #700mSIPHEX1-c… d__Bac… GCF_00… 95.0    d__Bac… 96.36   0.87    GCF_00… d__Bac… 96.36   0.87    d__Bac… ANI/Pl… topo… GCF_90…    54.6
  x<-dataframe[dataframe$user_genome == genome_example,]
  #Encontrar en la fila la asignacion de gtdbk en clasification y separar por ";". Ejemplo:
  #[[1]]
  #[1] "d__Bacteria"                  "p__Proteobacteria"            "c__Alphaproteobacteria"       "o__Rhodobacterales"          
  #[5] "f__Rhodobacteraceae"          "g__Sulfitobacter"             "s__Sulfitobacter sp001634775"
  w<-strsplit(x$classification, ";")
  #En un if anidado, encontrar la especie correspondiente si está asignada, si no asignar genero, familia. Ejemplo:
  #[[1]]
  #[1] "s__Sulfitobacter sp001634775"
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
  #
  id_split <-strsplit(id_completo, "_")
  bin_id<-paste(id_split[[1]][1],"_",id_split[[1]][3],sep = "")
  #grep_index <-grep(id,file)
  gtdbtk <-file[file$user_genome == grep_line, "classification"]
  #grep_gtdbtk <-file[grep(id, file$classification), ]
  separador<- ";"
  splitname <- strsplit(as.character(gtdbtk), split = separador)
  genera <- splitname[[1]][7]
  genera_s<-gsub(" ", "_", genera)
  #ifelse( < 0, "-", "+")
  #s<- as.character(ifelse(splitname == 0, "-", "+"))
  #encontrar el index de identificador2
  #Hacer un dataframe vacio
  df <- data.frame(matrix(ncol = 4, nrow = 0))
  colnames(df) <-c("id_numero",	"feature_id","user_genome","gtdbk")
  #rellenar las filas de el df
  df[1,] <-c(100000, 666666.100000,	ID,	genera_s)
  return (df)
}

user_id(gtdbK_file,genome_example)
#-----------------------------------------------------------------------------------
#aplicar par una lista de ids
#lista:
userg_list<-gtdbK_file$user_genome
library(plyr)
taxon_assig<- ldply(.data =userg_list,
              .fun= function(x) user_id(gtdbK_file,x))

taxon_assig

#----------------------------------------------------------------------------------

grep_line <- grep(value = TRUE,genome_example,gtdbK_file$user_genome)
grep_line



class<-strsplit(gtdbK_file$classification, ";")
class[[1]][1]
lapply(class,`[[`, 6)

class1<-as.character(class)
df1 <- data.frame(matrix(ncol = 7, nrow = 0))
df[1,] <- c(class1)



