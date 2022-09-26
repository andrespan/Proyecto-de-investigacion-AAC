#con la funcion read_ko buscar en la tabla la columna 3 (el numero de KO)
#library(devtools)
#install_github("mirnavazquez/RbiMs")
 #library(rbims)
library("rbims")
#dataframe
k0<-read_ko("Datos/5mSIPHEX1_0_short.txt")
k0
class(k0)
#df1 <- k0[k0$KO == 1, ]
#df1
#anotacion <- function(file){
  #identificamos todas las lineas que contienen ">" 
  #grep_index <-grep("*",file)
  #le pedimos esas lineas al archivo
  #ID_lines <- file[grep_index]
  #recortamos por el  " #"
  #Id <-strsplit(ID_lines, )
  #de cada seleccion elegir la linea [1]
  #ID <-sapply(Id, `[`, 3, simplify = FALSE)
  #el valor numerico de cada linea 
  #grep_index <-grep(">",file)
  #return (Id)
#}

#anotacion(k0)

#class(str)
#Seleccionar la columna tres 
#column1 <- k0[,1]
#column1
#ID<-column1[1]
#ID
#class(ID) 
#variable id de prueba
id_prueba <- "5mSIPHEX1_0_scaffold_1104_c1_2"
id_prueba

#prueba grep
#grep("5mSIPHEX1_0_scaffold_1104_c1_2",k0$Scaffold_name)
#k0$Scaffold_name
#subset(k0, grepl("5mSIPHEX1_0_scaffold_1104_c1_2", k0$Scaffold_name))
#colnames(k0)
#My.Data <- k0
#grep("5mSIPHEX1_0_scaffold_1104_c1_2", My.Data$Scaffold_name)
grep_id <-k0[grep("5mSIPHEX1_0_scaffold_1104_c1_2", k0$Scaffold_name), ]
grep_id

#subset(My.Data, startsWith(as.character(Scaffold_name), "5mSIPHEX1_0_scaffold_1104_c1_2"))
#library(dplyr)
#library(stringr)
#My.Data %>% filter(str_detect(Scaffold_name, '5mSIPHEX1_0_scaffold_1104_c1_2'))

#funcion atomo 
# doy un ID y regresar una function metabolica
#5mSIPHEX1_0-scaffold_1104_c1_2 K02056
ID_to_metabolic <-function(id,df){
  #grep_id <- grep(value = TRUE,id,df)
  grep_id <-df[grep(id, df$Scaffold_name), ]
  #grep_id
  #column1 <-strsplit(ID_lines,\)[1]
  metabolic<-grep_id[3]
  #grep_id <- grep(value = TRUE,2,df)
  #column3 <- df[,3]
  #grep_index <-grep(value = TRUE,id,df)
  #ID_ko <-grep_index[1]
  return(metabolic)
}

ID_to_metabolic(id_prueba,k0)
#aplicamos funcion para todos los ids
library(plyr)
ldply(.data =column1,
      .fun= function(x) ID_to_metabolic(x))




k0

class(funcion)

#aplicarla a todos los Id
#con la funcion dply 
#pegarla al dataframe


class(column3)