#mezclar los archivo rast.IDs
library(dplyr)
#data_all <- list.files(path = "/Datos",  # Identify all CSV files
#                      pattern = "*.IDs", full.names = TRUE) %>% 
#  lapply(read_csv) %>%                              # Store all files in list
#  bind_rows                                       # Combine data sets into one data set 
#data_all    
one<-readLines("Datos/Corason_nospace_rast.IDs")#colClasses = "character")
two<-readLines("Datos/Alphaproteobacteria_rastid2.IDs")#,colClasses = "character")

one
class(two)
dput(two$V2)
dput(one$V2)


v<-c(one,two)
v


writeLines(v, "Datos/Alpha_merged0.IDs")
#write.table(v , file =  "Datos/Alpha_merged0.IDs", sep = "\t", dec = ".",
#            row.names = FALSE, col.names = FALSE, quote=FALSE)
