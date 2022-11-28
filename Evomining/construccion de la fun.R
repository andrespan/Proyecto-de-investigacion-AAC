library("readr")
#cargamos archive
gtdbK_file <- read_tsv('Datos/gtdbtk.bac120.summary.tsv')
genome_example<-"700mSIPHEX2-concot_9"

colnames(gtdbK_file)
gtdbK_file$classification
gtdbK_file$user_genome
gtdbK_file$fastani_taxonomy
gtdbK_file$closest_placement_taxonomy
gtdbK_file$pplacer_taxonomy
gtdbK_file
genome_example<-"700mSIPHEX2-concot_9"
x<-gtdbK_file[gtdbK_file$user_genome == genome_example,]
x
xvalue<-which(gtdbK_file == genome_example, arr.ind = TRUE)
xvalue
x_row<-xvalue[1]
x_row
value_id<-as.numeric(x_row)*10000
value_id
format(value_id, scientific = FALSE)
feature_id<-paste("666666.",value_id,sep = "")
feature_id
class(feature_id)
as.numeric(feature_id)
id_completo<-gsub("-", "_",x$user_genome)
id_split <-strsplit(id_completo, "_")
id_split
bin_id<-paste(id_split[[1]][1],"_",id_split[[1]][3],sep = "")
bin_id

w<-strsplit(x$classification, ";")
w
specie<-lapply(w,`[[`, 7)
specie
#-----------------------------------------------------------
c<-lapply(w,`[[`, 3)
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
###############################################
c<-lapply(w,`[[`, 3)
f<-c
if(c=="c__Alphaproteobacteria") {
  df1[1,] <-c( value_id, feature_id, bin_id,	sp_rast)
  if(f=="c__Bacteroidia"){
    df2[1,] <-c( value_id, feature_id, bin_id,	sp_rast)
    if(f=="c__Gammaproteobacteria"){
      df3[1,] <-c( value_id, feature_id, bin_id,	sp_rast)}
      
  }
}

df1
df2
df3
##############################################################
genera<-lapply(w,`[[`, 6)
genera
fam<-lapply(w,`[[`, 5)
fam
f<-specie
if(specie=="s__") {
  f<-lapply(w,`[[`, 6)
  if(f=="g__"){
    f<-lapply(w,`[[`, 5)
  }
}

class(f)
f
s<-strsplit(as.character(f), "__")
sp<-s[[1]][2]
g_sp<- gsub(" ", "_", sp)
g_sp
sp_rast<-paste(g_sp,"_",bin_id,sep = "")
sp_rast

###------------------------------------------------------------------------------

#obtener familias

famis<-function(file){
  x<-file$classification
  w<-strsplit(x, ";")
  fam<-lapply(w,`[[`, 5)
  specie<-lapply(w,`[[`, 7)
  e<-lapply(w,`[[`, 3)
  ug<-file$user_genome
  ret<-paste(e,fam, specie,ug)
  return(sort(ret))
}
famis(gtdbK_file)
