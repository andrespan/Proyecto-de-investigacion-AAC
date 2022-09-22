#con la funcion read_ko buscar en la tabla la columna 3 (el numero de KO)
library(devtools)
install_github("mirnavazquez/RbiMs")
library(rbims)
library("rbims")

k0<-read_ko("Datos/")
k0
getclass(k0)
#df1 <- k0[k0$KO == 1, ]
#df1

#Seleccionar la columna tres 
column3 <- k0[,3]
column3
#funcion atomo 
atom <-function(df){
  grep_index <-grep("*",df)
}

# doy un ID y regresar una function metabolica 
#aplicarla a todos los Id
#con la funcion dply 
#pegarla al dataframe


class(column3)