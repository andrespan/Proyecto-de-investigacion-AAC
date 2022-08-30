##buscar con grep un identificador 
identificador1 <- ">5mSIPHEX1_0-scaffold_1104_c1_1"
identificador2 <- ">5mSIPHEX1_0-scaffold_1104_c1_2"
path <- readLines('Datos/5mSIPHEX1_0_short.faa')
#path <- read.delim('Datos'/'5mSIPHEX1_0_short.faa',header=TRUE,sep = " ")
#path <- read.table(file('Datos'/'5mSIPHEX1_0_short.faa'),row.names=0,sep='\t')
grep(identificador1,path) 
grep(identificador2,path)
#grep(">5mSIPHEX1_0-scaffold_1104_c1_1", path)
#grep(">5mSIPHEX1_0-scaffold_1104_c1_2", path)
#imprime las lineas desde el primero hasta el segundo ID
path[grep(identificador1,path):grep(identificador2,path)]
#imprime hasta la segunda linea del archivo
path[grep(identificador1,path):2]

path[grep(identificador2,path):4]


#name 
#path
identificador1
setwd()
getwd()