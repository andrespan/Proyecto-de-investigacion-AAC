x <- c("G448", "G459", "G479", "G406")  
y <- c(1:4)
My.Data <- data.frame (x,y)
grep("^G45", My.Data$x)
My.Data[grep("^G45", My.Data$x), ]
library(dplyr)
library(stringr)
My.Data %>% filter(str_detect(x, '^G45'))
