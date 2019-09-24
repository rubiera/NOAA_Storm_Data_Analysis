library(data.table)
library(tidyverse)
library(rsconnect)

StormData <- read.csv("./data/StormData.csv")
StormDataDT <- fread("./data/StormData.csv")
StormDataTB <- as_tibble(StormData) 

class(StormData)
class(StormDataTB)
class(StormDataDT)

colnames(StormDataDT)

head(StormData)
head(StormDataTB)
head(StormDataDT)

#straw for rpubs
rpubsUpload("NOAA Storm Data Analysis",
            "./_site/index.html","https://rpubs.com/mulchlax")
#update for image
rpubsUpload("NOAA Storm Data Analysis",
      "./_site/index_files/figure-html/pressure-1.png",
      "http://rpubs.com/mulchlax/531829")


