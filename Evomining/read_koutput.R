#con la funcion read_ko buscar en la tabla la columna 3 (el numero de KO)
#library(devtools)
#install_github("mirnavazquez/RbiMs")
 #library(rbims)
library("rbims")

k0<-read_ko("Datos/5mSIPHEX1_0_short.txt")
k0
getclass(k0)
#df1 <- k0[k0$KO == 1, ]
#df1

anotacion <- function(file){
  #identificamos todas las lineas que contienen ">" 
  grep_index <-grep("*",file)
  #le pedimos esas lineas al archivo
  ID_lines <- file[grep_index]
  #recortamos por el  " #"
  Id <-strsplit(ID_lines, )
  #de cada seleccion elegir la linea [1]
  #ID <-sapply(Id, `[`, 3, simplify = FALSE)
  #el valor numerico de cada linea 
  #grep_index <-grep(">",file)
  return (Id)
}
#tr<-
anotacion(k0)

class(str)
#Seleccionar la columna tres 
column1 <- k0[,1]
column1
ID<-column1[1]
ID
class(ID) 
#funcion atomo 
atom <-function(df){
  column1 <- df[1,]
  #column1 <-strsplit(ID_lines,\)[1]
  grep_id <- grep(value = TRUE,id,column1)
  funcion<-column1[3]
  #grep_id <- grep(value = TRUE,2,df)
  #column3 <- df[,3]
  #grep_index <-grep(value = TRUE,id,df)
  #ID_ko <-grep_index[1]
  return(funcion)
}

atom(k0)
#aplicamos funcion para todos los ids
library(plyr)
ddply(.data =column1,
      .fun= function(x) atom(x))




k0

class(funcion)
# doy un ID y regresar una function metabolica 
#aplicarla a todos los Id
#con la funcion dply 
#pegarla al dataframe


class(column3)