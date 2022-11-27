colnames(gtdbK_file)
gtdbK_file$classification
gtdbK_file$user_genome
gtdbK_file$fastani_taxonomy
gtdbK_file$closest_placement_taxonomy
gtdbK_file$pplacer_taxonomy
gtdbK_file
genome_example<-"700mSIPHEX2-concot_9"
x<-gtdbK_file[gtdbK_file$user_genome == genome_example,]

xvalue<-which(gtdbK_file == genome_example, arr.ind = TRUE)
x_row<-xvalue[1]
value_id<-as.numeric(x_row)*10000
format(value_id, scientific = FALSE)value_id
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
rownames(w)
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
f
s<-strsplit(as.character(f), "__")
sp<-s[[1]][2]
g_sp<- gsub(" ", "_", sp)
g_sp
sp_rast<-paste(g_sp,"_",bin_id,sep = "")
sp_rast