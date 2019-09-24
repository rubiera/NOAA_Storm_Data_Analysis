library(data.table)
library(tidyverse)


StormData <- read.csv("./data/repdata_data_StormData.csv.bz2")
head(StormData)
write.csv(StormData, file="./data/StormData.csv")
StormDataDT <- fread("./data/StormData.csv")
StormDataTB <- as_tibble(StormData) 

class(StormData)
class(StormDataTB)
class(StormDataDT)

colnames(StormDataDT)

head(StormData)
head(StormDataTB)
head(StormDataDT)

