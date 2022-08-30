##buscar con grep un identificador 
identificador1 <- ">*-scaffold_*_*_*"
identificador1
#identificador1 <-">*"
#identificador2 <- ">5mSIPHEX1_0-scaffold_1104_c1_2"
path <- readLines('Datos/5mSIPHEX1_0_short.faa')
#path <- read.delim('Datos'/'5mSIPHEX1_0_short.faa',header=TRUE,sep = " ")
#path <- read.table(file('Datos'/'5mSIPHEX1_0_short.faa'),row.names=0,sep='\t')
#name_numeric  <- grep(identificador1,path) 
#as.numeric(as.factor(identificador1))
#grep(identificador2,path)
#grep(">5mSIPHEX1_0-scaffold_1104_c1_1", path)
#grep(">5mSIPHEX1_0-scaffold_1104_c1_2", path)
#imprime las lineas desde el primero hasta el segundo ID
#path[grep(identificador1,path):grep(identificador1,path)]
#imprime hasta la segunda linea del archivo
name <-path[grep(identificador1,path)]
name
ID <- substring (name, 1, 32)
coordenadas <- substring (name, 32, 56)  
ID
coordenadas
#name_numeric  <- as.numeric(as.factor(name))
#sequence <- for (name in path) {nchar(identificador1)+1}
sequence

path[grep(identificador1,path)]



#name 
#path
identificador1
#setwd() y getwd()