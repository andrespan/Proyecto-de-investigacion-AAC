#con la funcion read_ko buscar en la tabla la columna 3 (el numero de KO)
#library(devtools)
#install_github("mirnavazquez/RbiMs")
 #library(rbims)
library("rbims")

k0<-read_ko("Datos/")
k0
getclass(k0)
#df1 <- k0[k0$KO == 1, ]
#df1

#Seleccionar la columna tres 
column3 <- k0[,3]
column3
ID<-"5mSIPHEX1_0_scaffold_1104_c1_2"
  
#funcion atomo 
atom <-function(df,id){
  grep_index <-grep(value = TRUE,id,df)
  #ID_ko <-grep_index[1][4]
  return(grep_index)
}

funcion #atom(k0)
class(funcion)
# doy un ID y regresar una function metabolica 
#aplicarla a todos los Id
#con la funcion dply 
#pegarla al dataframe


class(column3)