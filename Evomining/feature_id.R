#crear columna feature id

# los feature_id van a ir 100000 20000000....420000
library("readr")
gtdbK_file <- read_table('Datos/gtdbtk.bac120.summary.tsv')
gtdbK_file$classification
gtdbK_file$user_genome

