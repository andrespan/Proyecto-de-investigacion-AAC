#Esta funcion toma una la lista de ids rast-alike y copia todos aquellos archivos
#que coinciden con la lista a un directorio de destino
#ruta al directorio de entrada
inputdir  <- "Archivos_convertidos_prueba"  
#ruta al directorio de salida
targetdir <- "Hyphomonadaceae_bins"  
#aqui se carga el archivo de ids rast-alike
df <- readLines("Datos/ids_by_family/Hyphomonadaceae_bins.IDs")
#df
#dividimos las columnas del archivo por tabulador
v1<-strsplit(df, '\t')
#buscamos el primer elemento de la separacion para cada linea
listofnames<-lapply(v1,`[[`, 1)
#listofnames

# 1. listamos todos los archivos en el directorio
# podemos listar archivos por un patron especifico añadiendo un patron en el argumento = ".faa" por ejemplo.
filestocopy <- gsub(".txt","",gsub(".faa","",list.files(inputdir,full.names = TRUE))) #full.names = TRUE)

# 2. Conserva solo los arrchivos que coinciden con el patron que pusiste 
filestocopy <- unique(grep(paste(listofnames,collapse="|"), filestocopy, value=TRUE))
#tomamos las listas .faa y .txt y las unimos
faa<-paste(filestocopy,".faa",sep = "")
txt<-paste(filestocopy,".txt",sep = "")
filestocopy<-c(faa,txt)
#list

filestocopy
#paste(listofnames,collapse="|")
# 3. copia los archivos desados
sapply(filestocopy, function(x) file.copy(from=x, to=targetdir, copy.mode = TRUE))
