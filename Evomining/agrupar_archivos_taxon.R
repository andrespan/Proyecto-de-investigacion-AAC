inputdir  <- "Archivos_convertidos_prueba"  
targetdir <- "Alphaproteobacteria1"  
#id_example<-"429000"
df <- readLines("Datos/Alphaproteobacteria_rastid2.IDs")
df
#grep_line <- grep(value = TRUE, id_example,df)
v1<-strsplit(df, '\t')
listofnames<-lapply(v1,`[[`, 1)
listofnames
#specie<-lapply(v1,`[[`, 1)
#specie
#c("c2a1de2e-6451-4c95-8ce6-263f2b7e6eff", "67fa3e41-c7c6-44c5-9e67-6bcb2715aacc", "e21e2af3-dc71-41fa-94ba-7a7929f47d90", "5fd0f07b-8bb9-4378-bd26-28a26057e8fe", "4036c6fc-c82e-47bb-a384-6f299dc30b52", "bab43415-d413-40be-a4c0-2c40a52afe6a")

# 1. List all the files in the folder
#    Note that you can also chose to list files with a specific pattern by adding an argument pattern = ".csv" for example.
filestocopy <- gsub(".txt","",gsub(".faa","",list.files(inputdir,full.names = TRUE))) #full.names = TRUE)

# 2. Keep only the files that match the pattern you filtered 
filestocopy <- unique(grep(paste(listofnames,collapse="|"), filestocopy, value=TRUE))

faa<-paste(filestocopy,".faa",sep = "")
txt<-paste(filestocopy,".txt",sep = "")
filestocopy<-c(faa,txt)
#list

filestocopy
#paste(listofnames,collapse="|")
# 3. Copy the files
sapply(filestocopy, function(x) file.copy(from=x, to=targetdir, copy.mode = TRUE))
