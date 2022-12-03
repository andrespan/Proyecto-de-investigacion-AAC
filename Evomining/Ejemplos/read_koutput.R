#Funcion read_koutput incorporada a convertir a rast y a lapply con funcion
#con la funcion read_ko buscar en la tabla la columna 3 (el numero de KO)
library(devtools)
#install_github("mirnavazquez/RbiMs")
library("rbims")
#dataframe proviene del output de la funcion read_ko 
k0<-read_ko("Datos/5mSIPHEX1_0_short.txt")
k0
#class(k0)

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
ldply(.data =Lista_IDs,
      .fun= function(x) ID_to_metabolic(x,k0))

