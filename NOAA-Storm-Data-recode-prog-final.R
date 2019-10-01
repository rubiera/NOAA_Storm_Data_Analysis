library(data.table)
library(R.utils)
library(tidyverse)
library(ggplot2)

StormDataDT <- fread("./data/repdata_data_StormData.csv.bz2")
head(StormDataDT)

#########recode begins here
#need columns for outlier analysis
StormDataDT_recode <- StormDataDT[,
    c("REFNUM","BGN_DATE","EVTYPE","FATALITIES","INJURIES", 
      "PROPDMG","PROPDMGEXP","CROPDMG","CROPDMGEXP")]
head(StormDataDT_recode)
tail(StormDataDT_recode)
str(StormDataDT_recode)

#whole list
BGN_DATE_year <- as.character(StormDataDT_recode$BGN_DATE)

for (i in 1:length(BGN_DATE_year)){
  print(i)
  print(unlist(substring(strsplit(BGN_DATE_year[[i]], "/")[[1]][[3]],1,4)))
  BGN_DATE_year[i] <- 
    unlist(substring(strsplit(BGN_DATE_year[[i]], "/")[[1]][[3]],1,4))
}

head(BGN_DATE_year)
tail(BGN_DATE_year)
length(BGN_DATE_year)

StormDataDT_recode <- cbind(StormDataDT_recode,BGN_DATE_year)
head(StormDataDT_recode)

###year recode complete
class(StormDataDT_recode)
#make sure we get back to data.table after cbind
StormDataDT_recode <- setDT(StormDataDT_recode)

################PROPDMGEXP recode

table(StormDataDT_recode[,PROPDMGEXP])
#     -      ?      +      0      1      2      3      4       
#465934      1      8      5    216     25     13      4      
#5      6      7      8      B      h      H      K
#4     28      4      5      1     40      1      6 424665 
#m      M 
#7  11330

recode_PROPDMGEXP <- StormDataDT_recode[,PROPDMGEXP]
class(recode_PROPDMGEXP)
table(recode_PROPDMGEXP)

for (i in seq_along(recode_PROPDMGEXP)){

  print(i)
  #before
  print("before")
  print(recode_PROPDMGEXP[[i]])

  if(recode_PROPDMGEXP[[i]] == "1"){
    recode_PROPDMGEXP[[i]] <- 10
  }
    
  if(recode_PROPDMGEXP[[i]] == "" | 
     recode_PROPDMGEXP[[i]] == "0" |
     recode_PROPDMGEXP[[i]] == "-" |
     recode_PROPDMGEXP[[i]] == "?" |
     recode_PROPDMGEXP[[i]] == "+" |
     recode_PROPDMGEXP[[i]] == "h" |
     recode_PROPDMGEXP[[i]] == "H"){
    recode_PROPDMGEXP[[i]] <- 1
  }

  if(recode_PROPDMGEXP[[i]] == "2"){
    recode_PROPDMGEXP[[i]] <- 100
  }
  if(recode_PROPDMGEXP[[i]] == "k" | 
     recode_PROPDMGEXP[[i]] == "K" |
     recode_PROPDMGEXP[[i]] == "3"){
    recode_PROPDMGEXP[[i]] <- 1000
  }
  if(recode_PROPDMGEXP[[i]] == "4"){
    recode_PROPDMGEXP[[i]] <- 10000
  }
  if(recode_PROPDMGEXP[[i]] == "5"){
    recode_PROPDMGEXP[[i]] <- 100000
  }
  if(recode_PROPDMGEXP[[i]] == "m" | 
     recode_PROPDMGEXP[[i]] == "M" |
     recode_PROPDMGEXP[[i]] == "6"){
    recode_PROPDMGEXP[[i]] <- 1000000
  }
  if(recode_PROPDMGEXP[[i]] == "7"){
    recode_PROPDMGEXP[[i]] <- 10000000
  }
  if(recode_PROPDMGEXP[[i]] == "8"){
    recode_PROPDMGEXP[[i]] <- 100000000
  }
  if(recode_PROPDMGEXP[[i]] == "B"){
    recode_PROPDMGEXP[[i]] <- 1000000000
  }
  
  #after
  print("after")
  print(recode_PROPDMGEXP[[i]])
  
}

table(recode_PROPDMGEXP)
#1     10    100   1000  10000  1e+05  1e+06  1e+07  1e+08  1e+09 
#466171     25     13 424669      4     28  11341      5      1     40  


################CROPDMGEXP recode

table(StormDataDT_recode[,CROPDMGEXP])
#            ?      0      2      B      k      K      m      M 
#618413      7     19      1      9     21 281832      1   1994 

recode_CROPDMGEXP <- StormDataDT_recode[,CROPDMGEXP]
class(recode_CROPDMGEXP)
table(recode_CROPDMGEXP)

for (i in seq_along(recode_CROPDMGEXP)){
  
  print(i)
  #before
  print("before")
  print(recode_CROPDMGEXP[[i]])
  
  
  if(recode_CROPDMGEXP[[i]] == "" | 
     recode_CROPDMGEXP[[i]] == "0" |
     recode_CROPDMGEXP[[i]] == "?" ){
    recode_CROPDMGEXP[[i]] <- 1
  }
  
  if(recode_CROPDMGEXP[[i]] == "2"){
    recode_CROPDMGEXP[[i]] <- 100
  }
  if(recode_CROPDMGEXP[[i]] == "k" | 
     recode_CROPDMGEXP[[i]] == "K" ){
    recode_CROPDMGEXP[[i]] <- 1000
  }

  if(recode_CROPDMGEXP[[i]] == "m" | 
     recode_CROPDMGEXP[[i]] == "M" ){
    recode_CROPDMGEXP[[i]] <- 1000000
  }

  if(recode_CROPDMGEXP[[i]] == "B"){
    recode_CROPDMGEXP[[i]] <- 1000000000
  }
  
  #after
  print("after")
  print(recode_CROPDMGEXP[[i]])
  
}

table(recode_CROPDMGEXP)
#1    100   1000  1e+06  1e+09 
#618439      1 281853   1995      9  

###PROPDMGEXP and CROPDMGEXP recode complete
StormDataDT_recode <- cbind(StormDataDT_recode,recode_PROPDMGEXP,recode_CROPDMGEXP)

#before big column trim
#need columns for outlier analysis here
head(StormDataDT_recode)

class(StormDataDT_recode)
#make sure we get back to data.table after cbind
#not needed this time since we do have a data.table


sum(StormDataDT_recode$PROPDMG)
sum(StormDataDT_recode$CROPDMG)

StormDataDT_recode_totals <- StormDataDT_recode[,
                             PROPDMG.Total := PROPDMG*as.numeric(recode_PROPDMGEXP)]
head(StormDataDT_recode_totals)
StormDataDT_recode_totals <- StormDataDT_recode[,
                             CROPDMG.Total := CROPDMG*as.numeric(recode_CROPDMGEXP)]
head(StormDataDT_recode_totals)
StormDataDT_recode_totals <- StormDataDT_recode[,
                             Fatalities.and.Injuries := FATALITIES + INJURIES]
head(StormDataDT_recode_totals)
StormDataDT_recode_totals <- StormDataDT_recode[,
                             Property.and.Crop.Damage := PROPDMG.Total + CROPDMG.Total]
head(StormDataDT_recode_totals)

##############################################
#perform outlier analysis here

sum(StormDataDT_recode_totals$FATALITIES)
sum(StormDataDT_recode_totals$INJURIES)
sum(StormDataDT_recode_totals$Fatalities.and.Injuries)
sum(StormDataDT_recode_totals$PROPDMG.Total)
sum(StormDataDT_recode_totals$CROPDMG.Total)
sum(StormDataDT_recode_totals$Property.and.Crop.Damage)

StormDataDT_recode_large <- StormDataDT_recode_totals[Property.and.Crop.Damage >= 1000000000]
StormDataDT_recode_large$REFNUM
#[1] 187564 194932 194933 194939 198375 207124 211887 243394 298057 347811 366653 398999 444407 485535
#[15] 488004 525145 529299 529307 529311 529384 529446 564962 569065 569288 577615 577616 577623 581533
#[29] 581535 581537 598472 605943 639314 739514 739515 808257 834634 859151 860355 862563 867679 867996

StormDataDT[REFNUM == 187564,REMARKS]
StormDataDT_recode_large[REFNUM == 187564,
                      c("PROPDMG","PROPDMGEXP","CROPDMG","CROPDMGEXP",
                        "recode_PROPDMGEXP", "recode_CROPDMGEXP", 
                        "PROPDMG.Total","CROPDMG.Total","Property.and.Crop.Damage")]

Disposition:
  --75 million and not $5 billion
--Property.and.Crop.Damage 50000000
--PROPDMG 75 PROPDMGEXP M recode_PROPDMGEXP 1000000 PROPDMG.Total 75000000
Most of the damage estimates were at least $50 million.  
Some estimates ranged between $80 and $100 million

StormDataDT[REFNUM == 194932,REMARKS]
StormDataDT_recode_large[REFNUM == 194932,
                       c("PROPDMG","PROPDMGEXP","CROPDMG","CROPDMGEXP",
                         "recode_PROPDMGEXP", "recode_CROPDMGEXP", 
                         "PROPDMG.Total","CROPDMG.Total","Property.and.Crop.Damage")]

#"Preliminary  damage estimates were $2.1 billion dollars."

#ACCURATE

StormDataDT[REFNUM == 194933,REMARKS]
StormDataDT_recode_large[REFNUM == 194933,
                         c("PROPDMG","PROPDMGEXP","CROPDMG","CROPDMGEXP",
                           "recode_PROPDMGEXP", "recode_CROPDMGEXP", 
                           "PROPDMG.Total","CROPDMG.Total","Property.and.Crop.Damage")]

#ACCURATE

StormDataDT[REFNUM == 194939,REMARKS]
StormDataDT_recode_large[REFNUM == 194939,
                         c("PROPDMG","PROPDMGEXP","CROPDMG","CROPDMGEXP",
                           "recode_PROPDMGEXP", "recode_CROPDMGEXP", 
                           "PROPDMG.Total","CROPDMG.Total","Property.and.Crop.Damage")]

#Seems reasonable

StormDataDT[REFNUM == 198375,REMARKS]
StormDataDT_recode_large[REFNUM == 198375,
                         c("PROPDMG","PROPDMGEXP","CROPDMG","CROPDMGEXP",
                           "recode_PROPDMGEXP", "recode_CROPDMGEXP", 
                           "PROPDMG.Total","CROPDMG.Total","Property.and.Crop.Damage")]

#[1]   207124 211887 243394 298057 347811 366653 398999 444407 485535
#[15] 488004 525145 529299 529307 529311 529384 529446 564962 569065 569288 577615 577616 577623 581533
#[29] 581535 581537 598472 605943 639314 739514 739515 808257 834634 859151 860355 862563 867679 867996

#Prop damage at $5 billion and crop damage at $5 billion
#Seems resonable

StormDataDT[REFNUM == 207124,REMARKS]
StormDataDT_recode_large[REFNUM == 207124,
                         c("PROPDMG","PROPDMGEXP","CROPDMG","CROPDMGEXP",
                           "recode_PROPDMGEXP", "recode_CROPDMGEXP", 
                           "PROPDMG.Total","CROPDMG.Total","Property.and.Crop.Damage")]

# ACCURATE

StormDataDT[REFNUM == 211887,REMARKS]
StormDataDT_recode_large[REFNUM == 211887,
                         c("PROPDMG","PROPDMGEXP","CROPDMG","CROPDMGEXP",
                           "recode_PROPDMGEXP", "recode_CROPDMGEXP", 
                           "PROPDMG.Total","CROPDMG.Total","Property.and.Crop.Damage")]

"Twenty five percent of the states pecan crop will be lost for the next five to ten 
years at an estimated cost of $5.5 million per year"

#disposition
#CROPDMG 55 CROPDMGEXP M recode_CROPDMGEXP 1000000 CROPDMG.Total 55000000
#Property.and.Crop.Damage 55500000

StormDataDT[REFNUM == 243394,REMARKS]
StormDataDT_recode_large[REFNUM == 243394,
                         c("PROPDMG","PROPDMGEXP","CROPDMG","CROPDMGEXP",
                           "recode_PROPDMGEXP", "recode_CROPDMGEXP", 
                           "PROPDMG.Total","CROPDMG.Total","Property.and.Crop.Damage")]

# ACCURATE

StormDataDT[REFNUM == 298057,REMARKS]
StormDataDT_recode_large[REFNUM == 298057,
                         c("PROPDMG","PROPDMGEXP","CROPDMG","CROPDMGEXP",
                           "recode_PROPDMGEXP", "recode_CROPDMGEXP", 
                           "PROPDMG.Total","CROPDMG.Total","Property.and.Crop.Damage")]

#seems reasonable

StormDataDT[REFNUM == 347811,REMARKS]
StormDataDT_recode_large[REFNUM == 347811,
                         c("PROPDMG","PROPDMGEXP","CROPDMG","CROPDMGEXP",
                           "recode_PROPDMGEXP", "recode_CROPDMGEXP", 
                           "PROPDMG.Total","CROPDMG.Total","Property.and.Crop.Damage")]


#seems reasonable

StormDataDT[REFNUM == 366653,REMARKS]
StormDataDT_recode_large[REFNUM == 366653,
                         c("PROPDMG","PROPDMGEXP","CROPDMG","CROPDMGEXP",
                           "recode_PROPDMGEXP", "recode_CROPDMGEXP", 
                           "PROPDMG.Total","CROPDMG.Total","Property.and.Crop.Damage")]

# ACCURATE

StormDataDT[REFNUM == 398999,REMARKS]
StormDataDT_recode_large[REFNUM == 398999,
                         c("PROPDMG","PROPDMGEXP","CROPDMG","CROPDMGEXP",
                           "recode_PROPDMGEXP", "recode_CROPDMGEXP", 
                           "PROPDMG.Total","CROPDMG.Total","Property.and.Crop.Damage")]

#seems reasonable

StormDataDT[REFNUM == 444407,REMARKS]
StormDataDT_recode_large[REFNUM == 444407,
                         c("PROPDMG","PROPDMGEXP","CROPDMG","CROPDMGEXP",
                           "recode_PROPDMGEXP", "recode_CROPDMGEXP", 
                           "PROPDMG.Total","CROPDMG.Total","Property.and.Crop.Damage")]

#seems reasonable

StormDataDT[REFNUM == 485535,REMARKS]
StormDataDT_recode_large[REFNUM == 485535,
                         c("PROPDMG","PROPDMGEXP","CROPDMG","CROPDMGEXP",
                           "recode_PROPDMGEXP", "recode_CROPDMGEXP", 
                           "PROPDMG.Total","CROPDMG.Total","Property.and.Crop.Damage")]

#seems an over-estimate
#PROPDMG 100 PROPDMGEXP M recode_PROPDMGEXP 100000000 Property.and.Crop.Damage 100000000

StormDataDT[REFNUM == 488004,REMARKS]
StormDataDT_recode_large[REFNUM == 488004,
                         c("PROPDMG","PROPDMGEXP","CROPDMG","CROPDMGEXP",
                           "recode_PROPDMGEXP", "recode_CROPDMGEXP", 
                           "PROPDMG.Total","CROPDMG.Total","Property.and.Crop.Damage")]

#seems reasonable

StormDataDT[REFNUM == 525145,REMARKS]
StormDataDT_recode_large[REFNUM == 525145,
                         c("PROPDMG","PROPDMGEXP","CROPDMG","CROPDMGEXP",
                           "recode_PROPDMGEXP", "recode_CROPDMGEXP", 
                           "PROPDMG.Total","CROPDMG.Total","Property.and.Crop.Damage")]

#seems reasonable

StormDataDT[REFNUM == 529299,REMARKS]
StormDataDT_recode_large[REFNUM == 529299,
                         c("PROPDMG","PROPDMGEXP","CROPDMG","CROPDMGEXP",
                           "recode_PROPDMGEXP", "recode_CROPDMGEXP", 
                           "PROPDMG.Total","CROPDMG.Total","Property.and.Crop.Damage")]

#" $11.2 billion in property damage (estimated to be about twice that of the insured damage), 
#and $460 million in crop damage

#carries over multiple entries
#seems accurate when combined with other entries

StormDataDT[REFNUM == 529307,REMARKS]
StormDataDT_recode_large[REFNUM == 529307,
                         c("PROPDMG","PROPDMGEXP","CROPDMG","CROPDMGEXP",
                           "recode_PROPDMGEXP", "recode_CROPDMGEXP", 
                           "PROPDMG.Total","CROPDMG.Total","Property.and.Crop.Damage")]

#carries over multiple entries
#seems accurate when combined with other entries

StormDataDT[REFNUM == 529311,REMARKS]
StormDataDT_recode_large[REFNUM == 529311,
                         c("PROPDMG","PROPDMGEXP","CROPDMG","CROPDMGEXP",
                           "recode_PROPDMGEXP", "recode_CROPDMGEXP", 
                           "PROPDMG.Total","CROPDMG.Total","Property.and.Crop.Damage")]

#carries over multiple entries
#seems accurate when combined with other entries

StormDataDT[REFNUM == 529384,REMARKS]
StormDataDT_recode_large[REFNUM == 529384,
                         c("PROPDMG","PROPDMGEXP","CROPDMG","CROPDMGEXP",
                           "recode_PROPDMGEXP", "recode_CROPDMGEXP", 
                           "PROPDMG.Total","CROPDMG.Total","Property.and.Crop.Damage")]

#seems accurate

StormDataDT[REFNUM == 529446,REMARKS]
StormDataDT_recode_large[REFNUM == 529446,
                         c("PROPDMG","PROPDMGEXP","CROPDMG","CROPDMGEXP",
                           "recode_PROPDMGEXP", "recode_CROPDMGEXP", 
                           "PROPDMG.Total","CROPDMG.Total","Property.and.Crop.Damage")]

# ACCURATE

StormDataDT[REFNUM == 564962,REMARKS]
StormDataDT_recode_large[REFNUM == 564962,
                         c("PROPDMG","PROPDMGEXP","CROPDMG","CROPDMGEXP",
                           "recode_PROPDMGEXP", "recode_CROPDMGEXP", 
                           "PROPDMG.Total","CROPDMG.Total","Property.and.Crop.Damage")]

# KAtrina... yes.

StormDataDT[REFNUM == 569065,REMARKS]
StormDataDT_recode_large[REFNUM == 569065,
                         c("PROPDMG","PROPDMGEXP","CROPDMG","CROPDMGEXP",
                           "recode_PROPDMGEXP", "recode_CROPDMGEXP", 
                           "PROPDMG.Total","CROPDMG.Total","Property.and.Crop.Damage")]

# seems reasonable

StormDataDT[REFNUM == 569288,REMARKS]
StormDataDT_recode_large[REFNUM == 569288,
                         c("PROPDMG","PROPDMGEXP","CROPDMG","CROPDMGEXP",
                           "recode_PROPDMGEXP", "recode_CROPDMGEXP", 
                           "PROPDMG.Total","CROPDMG.Total","Property.and.Crop.Damage")]

#Total damage estimates from all the effects range from $9 to $12 billion
# ACCURATE

StormDataDT[REFNUM == 577615,REMARKS]
StormDataDT_recode_large[REFNUM == 577615,
                         c("PROPDMG","PROPDMGEXP","CROPDMG","CROPDMGEXP",
                           "recode_PROPDMGEXP", "recode_CROPDMGEXP", 
                           "PROPDMG.Total","CROPDMG.Total","Property.and.Crop.Damage")]

# Katrina

StormDataDT[REFNUM == 577616,REMARKS]
StormDataDT_recode_large[REFNUM == 577616,
                         c("PROPDMG","PROPDMGEXP","CROPDMG","CROPDMGEXP",
                           "recode_PROPDMGEXP", "recode_CROPDMGEXP", 
                           "PROPDMG.Total","CROPDMG.Total","Property.and.Crop.Damage")]

#seems reasonable

StormDataDT[REFNUM == 577623,REMARKS]
StormDataDT_recode_large[REFNUM == 577623,
                         c("PROPDMG","PROPDMGEXP","CROPDMG","CROPDMGEXP",
                           "recode_PROPDMGEXP", "recode_CROPDMGEXP", 
                           "PROPDMG.Total","CROPDMG.Total","Property.and.Crop.Damage")]

#seems reasonable

StormDataDT[REFNUM == 581533,REMARKS]
StormDataDT_recode_large[REFNUM == 581533,
                         c("PROPDMG","PROPDMGEXP","CROPDMG","CROPDMGEXP",
                           "recode_PROPDMGEXP", "recode_CROPDMGEXP", 
                           "PROPDMG.Total","CROPDMG.Total","Property.and.Crop.Damage")]

#more Katrina

StormDataDT[REFNUM == 581535,REMARKS]
StormDataDT_recode_large[REFNUM == 581535,
                         c("PROPDMG","PROPDMGEXP","CROPDMG","CROPDMGEXP",
                           "recode_PROPDMGEXP", "recode_CROPDMGEXP", 
                           "PROPDMG.Total","CROPDMG.Total","Property.and.Crop.Damage")]

#Thousands of homes and businesses were destroyed by the storm surge.
# seems reasonable

StormDataDT[REFNUM == 581537,REMARKS]
StormDataDT_recode_large[REFNUM == 581537,
                         c("PROPDMG","PROPDMGEXP","CROPDMG","CROPDMGEXP",
                           "recode_PROPDMGEXP", "recode_CROPDMGEXP", 
                           "PROPDMG.Total","CROPDMG.Total","Property.and.Crop.Damage")]

#katrina

StormDataDT[REFNUM == 598472,REMARKS]
StormDataDT_recode_large[REFNUM == 598472,
                         c("PROPDMG","PROPDMGEXP","CROPDMG","CROPDMGEXP",
                           "recode_PROPDMGEXP", "recode_CROPDMGEXP", 
                           "PROPDMG.Total","CROPDMG.Total","Property.and.Crop.Damage")]

# seems reasonable

StormDataDT[REFNUM == 605943,REMARKS]
StormDataDT_recode_large[REFNUM == 605943,
                         c("PROPDMG","PROPDMGEXP","CROPDMG","CROPDMGEXP",
                           "recode_PROPDMGEXP", "recode_CROPDMGEXP", 
                           "PROPDMG.Total","CROPDMG.Total","Property.and.Crop.Damage")]

#The City of Napa had 600 homes with moderate damage, 
#150 damaged businesses with costs of at least $70 million
#big outlier mistake
#PROPDMG OK PROPDMGEXP M and not B (BIG DIFFERENCE)
#recode_PROPDMGEXP 1000000
#PROPDMG.Total 115000000
#Property.and.Crop.Damage (115+32.5)= 147500000 

StormDataDT[REFNUM == 639314,REMARKS]
StormDataDT_recode_large[REFNUM == 639314,
                         c("PROPDMG","PROPDMGEXP","CROPDMG","CROPDMGEXP",
                           "recode_PROPDMGEXP", "recode_CROPDMGEXP", 
                           "PROPDMG.Total","CROPDMG.Total","Property.and.Crop.Damage")]

# seems reasonable

StormDataDT[REFNUM == 739514,REMARKS]
StormDataDT_recode_large[REFNUM == 739514,
                         c("PROPDMG","PROPDMGEXP","CROPDMG","CROPDMGEXP",
                           "recode_PROPDMGEXP", "recode_CROPDMGEXP", 
                           "PROPDMG.Total","CROPDMG.Total","Property.and.Crop.Damage")]

StormDataDT[REFNUM == 739515,REMARKS]
StormDataDT_recode_large[REFNUM == 739515,
                         c("PROPDMG","PROPDMGEXP","CROPDMG","CROPDMGEXP",
                           "recode_PROPDMGEXP", "recode_CROPDMGEXP", 
                           "PROPDMG.Total","CROPDMG.Total","Property.and.Crop.Damage")]

#"damage amounts are estimated to be near 14 billion dollars over the counties of 
#Harris, Chambers, Galveston, Liberty, Polk, Matagorda, Brazoria, Fort Bend, 
#San Jacinto, and Montgomery with an estimated 8 billion of that due to storm surge 
#in coastal Galveston, Harris and Chambers Counties"

StormDataDT[REFNUM == 808257,REMARKS]
StormDataDT_recode_large[REFNUM == 808257,
                         c("PROPDMG","PROPDMGEXP","CROPDMG","CROPDMGEXP",
                           "recode_PROPDMGEXP", "recode_CROPDMGEXP", 
                           "PROPDMG.Total","CROPDMG.Total","Property.and.Crop.Damage")]

#seems like an overestimate
#will leave as is

StormDataDT[REFNUM == 834634,REMARKS]
StormDataDT_recode_large[REFNUM == 834634,
                         c("PROPDMG","PROPDMGEXP","CROPDMG","CROPDMGEXP",
                           "recode_PROPDMGEXP", "recode_CROPDMGEXP", 
                           "PROPDMG.Total","CROPDMG.Total","Property.and.Crop.Damage")]

#The second big error in the data
#"The damages of 200 thousand dollars 
#covered both the roof damage as well as money to replace the ruined food"
#PROPDMG 200 PROPDMGEXP K recode_PROPDMGEXP 1000
# PROPDMG.Total 200000 Property.and.Crop.Damage 200000

StormDataDT[REFNUM == 859151,REMARKS]
StormDataDT_recode_large[REFNUM == 859151,
                         c("PROPDMG","PROPDMGEXP","CROPDMG","CROPDMGEXP",
                           "recode_PROPDMGEXP", "recode_CROPDMGEXP", 
                           "PROPDMG.Total","CROPDMG.Total","Property.and.Crop.Damage")]

#seems reasonable
#"In all, hundreds of homes received moderate to major 
#damage along the path with many of these being total losses"

StormDataDT[REFNUM == 860355,REMARKS]
StormDataDT_recode_large[REFNUM == 860355,
                         c("PROPDMG","PROPDMGEXP","CROPDMG","CROPDMGEXP",
                           "recode_PROPDMGEXP", "recode_CROPDMGEXP", 
                           "PROPDMG.Total","CROPDMG.Total","Property.and.Crop.Damage")]

#seems like an over estimate
#PROPDMG 150 PROPDMGEXP M recode_PROPDMGEXP 1000000 
#PROPDMG.Total 150000000 Property.and.Crop.Damage 150000000

StormDataDT[REFNUM == 862563,REMARKS]
StormDataDT_recode_large[REFNUM == 862563,
                         c("PROPDMG","PROPDMGEXP","CROPDMG","CROPDMGEXP",
                           "recode_PROPDMGEXP", "recode_CROPDMGEXP", 
                           "PROPDMG.Total","CROPDMG.Total","Property.and.Crop.Damage")]

# seems reasonable only if we make an economic assessment of the loss of life, which was very high
#The tornado killed 158 directly, three indirectly, and injured over 1150 people.

StormDataDT[REFNUM == 867679,REMARKS]
StormDataDT_recode_large[REFNUM == 867679,
                         c("PROPDMG","PROPDMGEXP","CROPDMG","CROPDMGEXP",
                           "recode_PROPDMGEXP", "recode_CROPDMGEXP", 
                           "PROPDMG.Total","CROPDMG.Total","Property.and.Crop.Damage")]

# seems like an over estimate
#The area known as Tunica Cut-Off was flooded as many as 357 homes sustained damage
#PROPDMG 100 PROPDMGEXP M recode_PROPDMGEXP 1000000 
#PROPDMG.Total 100000000    Property.and.Crop.Damage 100000000

StormDataDT[REFNUM == 867996,REMARKS]
StormDataDT_recode_large[REFNUM == 867996,
                         c("PROPDMG","PROPDMGEXP","CROPDMG","CROPDMGEXP",
                           "recode_PROPDMGEXP", "recode_CROPDMGEXP", 
                           "PROPDMG.Total","CROPDMG.Total","Property.and.Crop.Damage")]

# seems like an over estimate
#PROPDMG 200 PROPDMGEXP M recode_PROPDMGEXP 1000000 
#PROPDMG.Total 200000000 #Property.and.Crop.Damage 200000000

#####################life cost outlier analysis
StormDataDT_recode_life <- StormDataDT_recode_totals[FATALITIES >= 100]
StormDataDT_recode_life$REFNUM
#[1]  68670 148852 198690 862563

#tornadoes kill and injure lots of people!!

StormDataDT[REFNUM == 68670,REMARKS]
StormDataDT_recode_life[REFNUM == 68670,
                         c("EVTYPE","FATALITIES","INJURIES","Fatalities.and.Injuries", 
                           "PROPDMG.Total","CROPDMG.Total","Property.and.Crop.Damage")]

StormDataDT[REFNUM == 148852,REMARKS]
StormDataDT_recode_life[REFNUM == 148852,
                        c("EVTYPE","FATALITIES","INJURIES","Fatalities.and.Injuries", 
                          "PROPDMG.Total","CROPDMG.Total","Property.and.Crop.Damage")]

StormDataDT[REFNUM == 198690,REMARKS]
StormDataDT_recode_life[REFNUM == 198690,
                        c("EVTYPE","FATALITIES","INJURIES","Fatalities.and.Injuries", 
                          "PROPDMG.Total","CROPDMG.Total","Property.and.Crop.Damage")]

#"583 people died as a result of the heat in Chicago and surrounding areas"

StormDataDT[REFNUM == 862563,REMARKS]
StormDataDT_recode_life[REFNUM == 862563,
                        c("EVTYPE","FATALITIES","INJURIES","Fatalities.and.Injuries", 
                          "PROPDMG.Total","CROPDMG.Total","Property.and.Crop.Damage")]


#additional recode for character 

StormDataDT[REFNUM == 215144, EVTYPE]

#disposition
#change EVTYPE to "WINTER WEATHER"
#explain that this finding came later in the analysis and was re-inserted here for 
#all recodes to be done in one loop.


###################implement outlier analysis
###all records

for (i in 1:nrow(StormDataDT_recode_totals)) {
  
    if(StormDataDT_recode_totals$REFNUM[[i]] == 187564)
    {
      #disposition
      #CROPDMG 55 CROPDMGEXP M recode_CROPDMGEXP 1000000 CROPDMG.Total 55000000
      #Property.and.Crop.Damage 55500000
      print("REFNUM == 187564")
      print(i)
      StormDataDT_recode_totals$PROPDMG[[i]] <- 75
      StormDataDT_recode_totals$PROPDMGEXP[[i]] <- "M"
      StormDataDT_recode_totals$recode_PROPDMGEXP[[i]] <- 1000000
      StormDataDT_recode_totals$PROPDMG.Total[[i]] <-  75000000
      StormDataDT_recode_totals$Property.and.Crop.Damage[[i]] <- 75000000
      print(select(filter(StormDataDT_recode_totals,REFNUM == 187564),
                   PROPDMG.Total:Property.and.Crop.Damage))
    }
    
    if(StormDataDT_recode_totals$REFNUM[[i]] == 211887)
    {
      #disposition
      #CROPDMG 55 CROPDMGEXP M recode_CROPDMGEXP 1000000 CROPDMG.Total 55000000
      #Property.and.Crop.Damage 55500000
      print("REFNUM == 211887")
      print(i)
      StormDataDT_recode_totals$CROPDMG[[i]] <- 55
      StormDataDT_recode_totals$CROPDMGEXP[[i]] <- "M"
      StormDataDT_recode_totals$recode_CROPDMGEXP[[i]] <- 1000000
      StormDataDT_recode_totals$CROPDMG.Total[[i]] <-  55000000
      StormDataDT_recode_totals$Property.and.Crop.Damage[[i]] <- 55500000
      print(select(filter(StormDataDT_recode_totals,REFNUM == 211887),
                   PROPDMG.Total:Property.and.Crop.Damage))
    }
  
      ###special recode
      ###for \
      if(StormDataDT_recode_totals$REFNUM[[i]] == 215144)
      {
        #disposition
        #StormDataDT[REFNUM == ,EVTYPE] <- "WINTER WEATHER"
        #EVTYPE <- "WINTER WEATHER"
        #Property.and.Crop.Damage 55500000
        print("REFNUM == 215144")
        print("Special EV_TYPE recode")
        print(i)
        StormDataDT_recode_totals$EVTYPE[[i]] <- "WINTER WEATHER"
        print(select(filter(StormDataDT_recode_totals,REFNUM == 215144),
                     PROPDMG.Total:Property.and.Crop.Damage))
      }
  
    if(StormDataDT_recode_totals$REFNUM[[i]] == 485535)
    {
      #disposition
      #seems an over-estimate
      #PROPDMG 100 PROPDMGEXP M 
      #recode_PROPDMGEXP 1000000 Property.and.Crop.Damage 100000000
      print("REFNUM == 485535")
      print(i)
      StormDataDT_recode_totals$PROPDMG[[i]] <- 100
      StormDataDT_recode_totals$PROPDMGEXP[[i]] <- "M"
      StormDataDT_recode_totals$recode_PROPDMGEXP[[i]] <- 1000000
      StormDataDT_recode_totals$PROPDMG.Total[[i]] <-  100000000
      StormDataDT_recode_totals$Property.and.Crop.Damage[[i]] <- 100000000
      print(select(filter(StormDataDT_recode_totals,REFNUM == 485535),
                   PROPDMG.Total:Property.and.Crop.Damage))
    }
    if(StormDataDT_recode_totals$REFNUM[[i]] == 605943)
    {
      #disposition
      #big outlier mistake
      #PROPDMG OK PROPDMGEXP M and not B (BIG DIFFERENCE)
      #recode_PROPDMGEXP 1000000
      #PROPDMG.Total 115000000
      #Property.and.Crop.Damage (115+32.5)= 147500000 
      print("REFNUM == 605943")
      print(i)
      StormDataDT_recode_totals$PROPDMGEXP[[i]] <- "M"
      StormDataDT_recode_totals$recode_PROPDMGEXP[[i]] <- 1000000
      StormDataDT_recode_totals$PROPDMG.Total[[i]] <-  115000000
      StormDataDT_recode_totals$Property.and.Crop.Damage[[i]] <- 147500000
      print(select(filter(StormDataDT_recode_totals,REFNUM == 605943),
                   PROPDMG.Total:Property.and.Crop.Damage))
    }
    if(StormDataDT_recode_totals$REFNUM[[i]] == 834634)
    {
      #disposition
      #PROPDMG 200 PROPDMGEXP K recode_PROPDMGEXP 1000
      # PROPDMG.Total 200000 Property.and.Crop.Damage 200000
      print("REFNUM == 834634")
      print(i)
      StormDataDT_recode_totals$PROPDMG[[i]] <- 200
      StormDataDT_recode_totals$PROPDMGEXP[[i]] <- "K"
      StormDataDT_recode_totals$recode_PROPDMGEXP[[i]] <- 1000
      StormDataDT_recode_totals$PROPDMG.Total[[i]] <-  200000
      StormDataDT_recode_totals$Property.and.Crop.Damage[[i]] <- 200000
      print(select(filter(StormDataDT_recode_totals,REFNUM == 834634),
                   PROPDMG.Total:Property.and.Crop.Damage))
    }
    if(StormDataDT_recode_totals$REFNUM[[i]] == 860355)
    {
      #disposition
      #PROPDMG 150 PROPDMGEXP M recode_PROPDMGEXP 1000000 
      #PROPDMG.Total 150000000 Property.and.Crop.Damage 150000000
      print("REFNUM == 860355")
      print(i)
      StormDataDT_recode_totals$PROPDMG[[i]] <- 150
      StormDataDT_recode_totals$PROPDMGEXP[[i]] <- "M"
      StormDataDT_recode_totals$recode_PROPDMGEXP[[i]] <- 1000000
      StormDataDT_recode_totals$PROPDMG.Total[[i]] <-  150000000
      StormDataDT_recode_totals$Property.and.Crop.Damage[[i]] <- 150000000
      print(select(filter(StormDataDT_recode_totals,REFNUM == 860355),
                   PROPDMG.Total:Property.and.Crop.Damage))
    }
    if(StormDataDT_recode_totals$REFNUM[[i]] == 867679)
    {
      #disposition
      #PROPDMG 100 PROPDMGEXP M recode_PROPDMGEXP 1000000 
      #PROPDMG.Total 100000000    Property.and.Crop.Damage 100000000
      print("REFNUM == 867679")
      print(i)
      StormDataDT_recode_totals$PROPDMG[[i]] <- 100
      StormDataDT_recode_totals$PROPDMGEXP[[i]] <- "M"
      StormDataDT_recode_totals$recode_PROPDMGEXP[[i]] <- 1000000
      StormDataDT_recode_totals$PROPDMG.Total[[i]] <-  100000000
      StormDataDT_recode_totals$Property.and.Crop.Damage[[i]] <- 100000000
      print(select(filter(StormDataDT_recode_totals,REFNUM == 867679),
                   PROPDMG.Total:Property.and.Crop.Damage))
    }
    if(StormDataDT_recode_totals$REFNUM[[i]] == 867996)
    {
      #disposition
      #PROPDMG 200 PROPDMGEXP M recode_PROPDMGEXP 1000000 
      #PROPDMG.Total 200000000 #Property.and.Crop.Damage 200000000
      print("REFNUM == 867996")
      print(i)
      StormDataDT_recode_totals$PROPDMG[[i]] <- 200
      StormDataDT_recode_totals$PROPDMGEXP[[i]] <- "M"
      StormDataDT_recode_totals$recode_PROPDMGEXP[[i]] <- 1000000
      StormDataDT_recode_totals$PROPDMG.Total[[i]] <-  200000000
      StormDataDT_recode_totals$Property.and.Crop.Damage[[i]] <- 200000000
      print(select(filter(StormDataDT_recode_totals,REFNUM == 867996),
                   PROPDMG.Total:Property.and.Crop.Damage))
    }
  
  
}


#[1] "REFNUM == 211887"
#[1] 211900
#PROPDMG.Total CROPDMG.Total Fatalities.and.Injuries Property.and.Crop.Damage
#1         5e+05       5.5e+07                       0                 55500000
#[1] "REFNUM == 485535"
#[1] 485577
#PROPDMG.Total CROPDMG.Total Fatalities.and.Injuries Property.and.Crop.Damage
#1         1e+08             0                       0                    1e+08
#[1] "REFNUM == 605943"
#[1] 605953
#PROPDMG.Total CROPDMG.Total Fatalities.and.Injuries Property.and.Crop.Damage
#1      1.15e+08      32500000                       0                147500000
#[1] "REFNUM == 834634"
#[1] 834674
#PROPDMG.Total CROPDMG.Total Fatalities.and.Injuries Property.and.Crop.Damage
#1         2e+05             0                       1                    2e+05
#[1] "REFNUM == 860355"
#[1] 860386
#PROPDMG.Total CROPDMG.Total Fatalities.and.Injuries Property.and.Crop.Damage
#1       1.5e+08             0                     844                  1.5e+08
#[1] "REFNUM == 867679"
#[1] 867749
#PROPDMG.Total CROPDMG.Total Fatalities.and.Injuries Property.and.Crop.Damage
#1         1e+08             0                       0                    1e+08
#[1] "REFNUM == 867996"
#[1] 868046
#PROPDMG.Total CROPDMG.Total Fatalities.and.Injuries Property.and.Crop.Damage
#1         2e+08             0                       0                    2e+08#

filter(StormDataDT_recode_totals,REFNUM == 215144)
#recode complete: WINTER WEATHER

####sums sall before outlier recode
#> sum(StormDataDT_recode_totals$FATALITIES)
#[1] 15145
#> sum(StormDataDT_recode_totals$INJURIES)
#1] 140528
#> sum(StormDataDT_recode_totals$Fatalities.and.Injuries)
#[1] 155673
#> sum(StormDataDT_recode_totals$PROPDMG.Total)
#[1] 428224866095
#> sum(StormDataDT_recode_totals$CROPDMG.Total)
#[1] 49104192181
#> sum(StormDataDT_recode_totals$Property.and.Crop.Damage)
#[1] 477 329 058 276 that's $477 billion

####sums all after outlier recode

#> sum(StormDataDT_recode_totals$FATALITIES)
#[1] 15145
#> sum(StormDataDT_recode_totals$INJURIES)
#[1] 140528
#> sum(StormDataDT_recode_totals$Fatalities.and.Injuries)
#[1] 155673
#> sum(StormDataDT_recode_totals$PROPDMG.Total)
#[1] 306590066095
#> sum(StormDataDT_recode_totals$CROPDMG.Total)
#[1] 44159192181
#> sum(StormDataDT_recode_totals$Property.and.Crop.Damage)
#[1] 350749258276

sum(StormDataDT_recode_totals$FATALITIES)
sum(StormDataDT_recode_totals$INJURIES)
sum(StormDataDT_recode_totals$Fatalities.and.Injuries)
sum(StormDataDT_recode_totals$PROPDMG.Total)
sum(StormDataDT_recode_totals$CROPDMG.Total)
sum(StormDataDT_recode_totals$Property.and.Crop.Damage)


#######################################################################

head(StormDataDT_recode_totals)
nrow(StormDataDT_recode_totals)
#[1] 902297
StormDataDT_recode <- StormDataDT_recode_totals[,
                          c("BGN_DATE_year","EVTYPE","FATALITIES","INJURIES",
                            "PROPDMG.Total","CROPDMG.Total",
                            "Fatalities.and.Injuries","Property.and.Crop.Damage")]
class(StormDataDT_recode)
head(StormDataDT_recode)

#check for zeroes
nrow(StormDataDT_recode[FATALITIES == 0  & INJURIES == 0])
#[1] 880368
nrow(StormDataDT_recode[Property.and.Crop.Damage == 0])
#[1] 657266
nrow(StormDataDT_recode[(FATALITIES == 0  & INJURIES == 0) | 
                          Property.and.Crop.Damage == 0])
#[1] 889970
nrow(StormDataDT_recode[!((FATALITIES == 0  & INJURIES == 0) | 
                          Property.and.Crop.Damage == 0)])
#[1] 12327

StormDataDT_recode_nonzero <- StormDataDT_recode[!(FATALITIES == 0  & INJURIES == 0 & 
                                                     Property.and.Crop.Damage == 0)]
nrow(StormDataDT_recode_nonzero)
#[1] 254633

sum(StormDataDT_recode_totals$FATALITIES)
sum(StormDataDT_recode_totals$INJURIES)
sum(StormDataDT_recode_totals$Fatalities.and.Injuries)
sum(StormDataDT_recode_totals$PROPDMG.Total)
sum(StormDataDT_recode_totals$CROPDMG.Total)
sum(StormDataDT_recode_totals$Property.and.Crop.Damage)

#logic is correct
sum(StormDataDT_recode_nonzero$FATALITIES)
sum(StormDataDT_recode_nonzero$INJURIES)
sum(StormDataDT_recode_nonzero$Fatalities.and.Injuries)
sum(StormDataDT_recode_nonzero$PROPDMG.Total)
sum(StormDataDT_recode_nonzero$CROPDMG.Total)
sum(StormDataDT_recode_nonzero$Property.and.Crop.Damage)

####recode EVTYPE for StormDataDT_recode_nonzero
write.csv(StormDataDT_recode_nonzero, "./data-test/StormDataDT_recode_nonzero.csv")
head(StormDataDT_recode_nonzero)
nrow(StormDataDT_recode_nonzero)
#[1] 254633

#do not need to be recoded
#originally as: (but other fields will be recoded into these)
#EVTYPE	COUNT
#WILDFIRE	857
#WINTER WEATHER	407
#TSUNAMI	14

#1950 to 2011
tail(StormDataDT_recode_nonzero)

#https://www.usinflationcalculator.com/

Inflation_multiplier <- tribble(
  ~year, ~Inflation.Adjustment,
  #----/----------------------
  1950	,	9.33	,
  1951	,	8.65	,
  1952	,	8.49	,
  1953	,	8.42	,
  1954	,	8.36	,
  1955	,	8.39	,
  1956	,	8.27	,
  1957	,	8	,
  1958	,	7.78	,
  1959	,	7.73	,
  1960	,	7.6	,
  1961	,	7.52	,
  1962	,	7.45	,
  1963	,	7.35	,
  1964	,	7.26	,
  1965	,	7.14	,
  1966	,	6.94	,
  1967	,	6.73	,
  1968	,	6.46	,
  1969	,	6.13	,
  1970	,	5.8	,
  1971	,	5.55	,
  1972	,	5.38	,
  1973	,	5.07	,
  1974	,	4.56	,
  1975	,	4.18	,
  1976	,	3.95	,
  1977	,	3.71	,
  1978	,	3.45	,
  1979	,	3.1	,
  1980	,	2.73	,
  1981	,	2.47	,
  1982	,	2.33	,
  1983	,	2.26	,
  1984	,	2.16	,
  1985	,	2.09	,
  1986	,	2.05	,
  1987	,	1.98	,
  1988	,	1.9	,
  1989	,	1.81	,
  1990	,	1.72	,
  1991	,	1.65	,
  1992	,	1.6	,
  1993	,	1.56	,
  1994	,	1.52	,
  1995	,	1.48	,
  1996	,	1.43	,
  1997	,	1.4	,
  1998	,	1.38	,
  1999	,	1.35	,
  2000	,	1.31	,
  2001	,	1.27	,
  2002	,	1.25	,
  2003	,	1.22	,
  2004	,	1.19	,
  2005	,	1.15	,
  2006	,	1.12	,
  2007	,	1.08	,
  2008	,	1.04	,
  2009	,	1.05	,
  2010	,	1.03	,
  2011	,	1	
)  

head(Inflation_multiplier)
tail(Inflation_multiplier)
StormDataDT_recode_nonzero <- as_tibble(StormDataDT_recode_nonzero)

#all data

StormDataDT_recode_nonzero <- 
  mutate(StormDataDT_recode_nonzero, year = as.numeric(BGN_DATE_year))

StormDataDT_recode_infla <- StormDataDT_recode_nonzero %>% 
  inner_join(Inflation_multiplier, by = "year")

names(StormDataDT_recode_infla)
head(StormDataDT_recode_infla$Inflation.Adjustment,100)
head(StormDataDT_recode_infla)
tail(StormDataDT_recode_infla)

StormDataDT_recode_infla <- select(StormDataDT_recode_infla, -(BGN_DATE_year))
StormDataDT_recode_infla_adjusted <- transmute(StormDataDT_recode_infla, year,
          EVTYPE, FATALITIES, INJURIES, Fatalities.and.Injuries,
          PROPDMG.Total.Infla = PROPDMG.Total * Inflation.Adjustment,
          CROPDMG.Total.Infla = CROPDMG.Total * Inflation.Adjustment, 
          Property.and.Crop.Damage.Infla = Property.and.Crop.Damage * Inflation.Adjustment)

head(StormDataDT_recode_infla_adjusted)
tail(StormDataDT_recode_infla_adjusted)
nrow(StormDataDT_recode_infla_adjusted)
#[1] 254633

#######################################################
#file for EVTYPE recode
class(StormDataDT_recode_infla_adjusted)
colnames(StormDataDT_recode_infla_adjusted)

sum(StormDataDT_recode_infla_adjusted$FATALITIES)
sum(StormDataDT_recode_infla_adjusted$INJURIES)
sum(StormDataDT_recode_infla_adjusted$Fatalities.and.Injuries)
sum(StormDataDT_recode_infla_adjusted$FATALITIES)/sum(StormDataDT_recode_infla_adjusted$Fatalities.and.Injuries)
sum(StormDataDT_recode_infla_adjusted$INJURIES)/sum(StormDataDT_recode_infla_adjusted$Fatalities.and.Injuries)



StormDataDT_recode_summed <- 
  StormDataDT_recode_infla_adjusted %>% group_by(year, EVTYPE) %>% 
  summarize(Property.and.Crop.Damage.Sum = sum(Property.and.Crop.Damage.Infla),
            Fatalities.and.Injuries.Sum = sum(Fatalities.and.Injuries))

head(StormDataDT_recode_summed)
nrow(StormDataDT_recode_summed)

write.csv(StormDataDT_recode_summed, "./data-test/StormDataDT_recode_summed.csv")

#######################################################
#EVTYPE recode

#No recode needed
#EVTYPE	
#TSUNAMI	
#WILDFIRE	
#INTER WEATHER	

StormDataDT_recode_summed_test$EVTYPE[[1]] 
StormDataDT_recode_summed_test$EVTYPE[[1000]] 

StormDataDT_recode_summed_test <- StormDataDT_recode_summed
class(StormDataDT_recode_summed_test)

for (i in 1:nrow(StormDataDT_recode_summed_test)) {
  
  if (StormDataDT_recode_summed_test$EVTYPE[[i]] =="DROUGHT" | 
      StormDataDT_recode_summed_test$EVTYPE[[i]] =="DROUGHT/EXCESSIVE HEAT" | 
      StormDataDT_recode_summed_test$EVTYPE[[i]] =="EXCESSIVE HEAT" | 
      StormDataDT_recode_summed_test$EVTYPE[[i]] =="HEAT" | 
      StormDataDT_recode_summed_test$EVTYPE[[i]] =="HEAT WAVE" | 
      StormDataDT_recode_summed_test$EVTYPE[[i]] =="HEAT WAVE DROUGHT" | 
      StormDataDT_recode_summed_test$EVTYPE[[i]] =="HEAT WAVES" | 
      StormDataDT_recode_summed_test$EVTYPE[[i]] =="RECORD HEAT" | 
      StormDataDT_recode_summed_test$EVTYPE[[i]] =="RECORD/EXCESSIVE HEAT" | 
      StormDataDT_recode_summed_test$EVTYPE[[i]] =="UNSEASONABLY WARM" | 
      StormDataDT_recode_summed_test$EVTYPE[[i]] =="UNSEASONABLY WARM AND DRY" | 
      StormDataDT_recode_summed_test$EVTYPE[[i]] =="WARM WEATHER" |
      #straggler
      StormDataDT_recode_summed_test$EVTYPE[[i]] =="Heat Wave" ) 
  
  {StormDataDT_recode_summed_test$EVTYPE[[i]] <- "DROUGHT, EXCESSIVE HEAT"}
  
  if(StormDataDT_recode_summed_test$EVTYPE[[i]] =="   HIGH SURF ADVISORY" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] ==" FLASH FLOOD" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] =="BREAKUP FLOODING" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] =="COASTAL  FLOODING/EROSION" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] =="COASTAL EROSION" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] =="COASTAL FLOOD" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] =="COASTAL FLOODING" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] =="COASTAL FLOODING/EROSION" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] =="COASTAL STORM" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] =="COASTAL SURGE" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] =="COASTALSTORM" |
     StormDataDT_recode_summed_test$EVTYPE[[i]] =="Coastal Flood" |
     StormDataDT_recode_summed_test$EVTYPE[[i]] =="Coastal Flooding" |
     StormDataDT_recode_summed_test$EVTYPE[[i]] =="Coastal Storm" |
     StormDataDT_recode_summed_test$EVTYPE[[i]] =="Erosion/Cstl Flood" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] =="EXCESSIVE RAINFALL" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] =="EXCESSIVE SNOW" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] =="EXTREME HEAT" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] =="FLASH FLOOD" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] =="FLASH FLOOD - HEAVY RAIN" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] =="FLASH FLOOD LANDSLIDES" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] =="FLASH FLOOD WINDS" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] =="FLASH FLOOD/" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] =="FLASH FLOOD/ STREET" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] =="FLASH FLOOD/FLOOD" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] =="FLASH FLOOD/LANDSLIDE" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] =="FLASH FLOODING" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] =="FLASH FLOODING/FLOOD" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] =="FLASH FLOODS" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] =="FLOOD" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] =="FLOOD & HEAVY RAIN" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] =="FLOOD FLASH" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] =="FLOOD/FLASH" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] =="FLOOD/FLASH FLOOD" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] =="FLOOD/FLASH/FLOOD" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] =="FLOOD/FLASHFLOOD" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] =="FLOOD/RAIN/WINDS" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] =="FLOOD/RIVER FLOOD" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] =="FLOODING" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] =="FLOODING/HEAVY RAIN" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] =="FLOODS" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] =="HAZARDOUS SURF" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] =="HEAVY RAIN" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] =="HEAVY RAIN AND FLOOD" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] =="Heavy Rain/High Surf" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] =="HEAVY RAIN/LIGHTNING" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] =="HEAVY RAIN/SEVERE WEATHER" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] =="HEAVY RAIN/SMALL STREAM URBAN" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] =="HEAVY RAIN/SNOW" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] =="HEAVY RAINS" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] =="HEAVY RAINS/FLOODING" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] =="HEAVY SEAS" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] =="HEAVY SHOWER" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] =="HEAVY SURF" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] =="Heavy surf and wind" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] =="HEAVY SURF COASTAL FLOODING" |
     StormDataDT_recode_summed_test$EVTYPE[[i]] =="HEAVY SURF/HIGH SURF" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] =="HEAVY SWELLS" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] =="HIGH  WINDS" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] =="HIGH SEAS" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] =="HIGH SURF" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] =="HIGH SWELLS" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] =="HIGH TIDES" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] =="HIGH WATER" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] =="HIGH WAVES" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] =="HVY RAIN" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] =="LAKE FLOOD" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] =="LAKE-EFFECT SNOW" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] =="LAKESHORE FLOOD" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] =="LANDSLIDE" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] =="LANDSLIDES" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] =="Landslump" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] =="LANDSPOUT" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] =="MAJOR FLOOD" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] =="MARINE HIGH WIND" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] =="MARINE STRONG WIND" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] =="MINOR FLOODING" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] =="MIXED PRECIP" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] =="Mixed Precipitation" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] =="MUD SLIDE" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] =="MUD SLIDES" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] =="MUD SLIDES URBAN FLOODING" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] =="MUDSLIDE" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] =="MUDSLIDES" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] =="RAIN" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] =="RAIN/SNOW" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] =="RAIN/WIND" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] =="RAINSTORM" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] =="RAPIDLY RISING WATER" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] =="RECORD RAINFALL" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] =="RIP CURRENTS/HEAVY SURF" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] =="RIVER AND STREAM FLOOD" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] =="RIVER FLOOD" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] =="RIVER FLOODING" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] =="ROCK SLIDE" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] =="ROGUE WAVE" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] =="ROUGH SEAS" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] =="ROUGH SURF" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] =="RURAL FLOOD" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] =="SMALL STREAM FLOOD" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] =="STORM FORCE WINDS" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] =="STORM SURGE" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] =="STORM SURGE/TIDE" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] =="TIDAL FLOODING" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] =="TORNADO F0" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] =="TORNADO F1" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] =="TORNADO F2" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] =="TORNADO F3" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] =="TORNADOES" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] =="Torrential Rainfall" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] =="UNSEASONAL RAIN" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] =="URBAN AND SMALL" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] =="URBAN AND SMALL STREAM FLOODIN" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] =="URBAN FLOOD" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] =="URBAN FLOODING" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] =="URBAN FLOODS" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] =="URBAN SMALL" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] =="URBAN/SMALL STREAM" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] =="URBAN/SMALL STREAM FLOOD" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] =="URBAN/SML STREAM FLD" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] =="WIND AND WAVE" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] =="WIND STORM" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] =="WIND/HAIL" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] =="WINDS" )
  
  {StormDataDT_recode_summed_test$EVTYPE[[i]] <- "HEAVY RAIN, FLOODING, MUDSLIDES, LANDSLIDES"}
  
  if(StormDataDT_recode_summed_test$EVTYPE[[i]] == "HURRICANE" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "Hurricane Edouard" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "HURRICANE EMILY" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "HURRICANE ERIN" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "HURRICANE FELIX" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "HURRICANE GORDON" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "HURRICANE OPAL" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "HURRICANE OPAL/HIGH WINDS" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "HURRICANE/TYPHOON" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "HURRICANE-GENERATED SWELLS" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "TROPICAL DEPRESSION" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "TROPICAL STORM" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "TROPICAL STORM ALBERTO" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "TROPICAL STORM DEAN" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "TROPICAL STORM GORDON" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "TROPICAL STORM JERRY" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "TYPHOON" ) 
  
  {StormDataDT_recode_summed_test$EVTYPE[[i]] <- "HURRICANE SEASON"}

  if(StormDataDT_recode_summed_test$EVTYPE[[i]] == "?" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "APACHE COUNTY" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "ASTRONOMICAL HIGH TIDE" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "ASTRONOMICAL LOW TIDE" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "Beach Erosion" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "COOL AND WET" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "DAM BREAK" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "DENSE FOG" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "DENSE SMOKE" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "DOWNBURST" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "DROWNING" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "FOG" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "FOG AND COLD TEMPERATURES" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "HEAVY MIX" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "HEAVY PRECIPITATION" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "HIGH" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "HYPERTHERMIA/EXPOSURE" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "HYPOTHERMIA" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "Hypothermia/Exposure" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "Marine Accident" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "MARINE MISHAP" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "Other" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "RIP CURRENT" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "RIP CURRENTS" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "VOLCANIC ASH" |
     #stragglers
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "HYPOTHERMIA/EXPOSURE" |
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "MIXED PRECIPITATION" |
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "TSUNAMI") 

  {StormDataDT_recode_summed_test$EVTYPE[[i]] <- "OTHER"}
  
    if(StormDataDT_recode_summed_test$EVTYPE[[i]] == " TSTM WIND" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == " TSTM WIND (G45)" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "FLASH FLOODING/THUNDERSTORM WI" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "LIGHTING" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "LIGHTNING" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "LIGHTNING  WAUSEON" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "LIGHTNING AND HEAVY RAIN" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "LIGHTNING AND THUNDERSTORM WIN" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "LIGHTNING FIRE" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "LIGHTNING INJURY" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "LIGHTNING THUNDERSTORM WINDS" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "LIGHTNING." | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "LIGHTNING/HEAVY RAIN" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "LIGNTNING" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "MARINE THUNDERSTORM WIND" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "MARINE TSTM WIND" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "SEVERE THUNDERSTORM" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "SEVERE THUNDERSTORM WINDS" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "SEVERE THUNDERSTORMS" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "THUDERSTORM WINDS" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "THUNDEERSTORM WINDS" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "THUNDERESTORM WINDS" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "THUNDERSNOW" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "THUNDERSTORM" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "THUNDERSTORM  WINDS" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "THUNDERSTORM DAMAGE TO" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "THUNDERSTORM HAIL" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "THUNDERSTORM WIND" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "THUNDERSTORM WIND (G40)" |
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "THUNDERSTORM WIND 60 MPH" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "THUNDERSTORM WIND 65 MPH" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "THUNDERSTORM WIND 65MPH" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "THUNDERSTORM WIND 98 MPH" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "THUNDERSTORM WIND G50" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "THUNDERSTORM WIND G52" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "THUNDERSTORM WIND G55" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "THUNDERSTORM WIND G60" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "THUNDERSTORM WIND TREES" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "THUNDERSTORM WIND." | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "THUNDERSTORM WIND/ TREE" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "THUNDERSTORM WIND/ TREES" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "THUNDERSTORM WIND/AWNING" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "THUNDERSTORM WIND/HAIL" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "THUNDERSTORM WIND/LIGHTNING" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "THUNDERSTORM WINDS" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "THUNDERSTORM WINDS 13" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "THUNDERSTORM WINDS 63 MPH" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "THUNDERSTORM WINDS AND" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "THUNDERSTORM WINDS G60" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "THUNDERSTORM WINDS HAIL" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "THUNDERSTORM WINDS LIGHTNING" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "THUNDERSTORM WINDS." | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "THUNDERSTORM WINDS/ FLOOD" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "THUNDERSTORM WINDS/FLOODING" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "THUNDERSTORM WINDS/FUNNEL CLOU" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "THUNDERSTORM WINDS/HAIL" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "THUNDERSTORM WINDS53" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "THUNDERSTORM WINDSHAIL" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "THUNDERSTORM WINDSS" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "THUNDERSTORM WINS" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "THUNDERSTORMS" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "THUNDERSTORMS WIND" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "THUNDERSTORMS WINDS" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "THUNDERSTORMW" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "THUNDERSTORMWINDS" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "THUNDERSTROM WIND" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "THUNDERTORM WINDS" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "THUNERSTORM WINDS" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "TSTM WIND" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "TSTM WIND  (G45)" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "TSTM WIND (41)" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "TSTM WIND (G35)" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "TSTM WIND (G40)" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "TSTM WIND (G45)" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "TSTM WIND 40" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "TSTM WIND 45" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "TSTM WIND 55" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "TSTM WIND 65)" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "TSTM WIND AND LIGHTNING" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "TSTM WIND DAMAGE" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "TSTM WIND G45" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "TSTM WIND G58" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "TSTM WIND/HAIL" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "TSTM WINDS" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "TSTMW" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "TUNDERSTORM WIND" |
       #straggler
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "Tstm Wind")
  
    {StormDataDT_recode_summed_test$EVTYPE[[i]] <- "THUNDERSTORM"}
  
    if(StormDataDT_recode_summed_test$EVTYPE[[i]] == "DRY MICROBURST" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "DRY MIRCOBURST WINDS" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "DUST DEVIL" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "DUST DEVIL WATERSPOUT" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "DUST STORM" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "DUST STORM/HIGH WINDS" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "FUNNEL CLOUD" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "GRADIENT WIND" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "GUSTNADO" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "GUSTY WIND" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "GUSTY WIND/HAIL" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "GUSTY WIND/HVY RAIN" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "Gusty wind/rain" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "GUSTY WINDS" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "HAIL" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "HAIL 0.75" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "HAIL 075" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "HAIL 100" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "HAIL 125" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "HAIL 150" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "HAIL 175" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "HAIL 200" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "HAIL 275" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "HAIL 450" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "HAIL 75" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "HAIL DAMAGE" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "HAIL/WIND" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "HAIL/WINDS" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "HAILSTORM" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "HIGH WIND" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "HIGH WIND (G40)" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "HIGH WIND 48" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "HIGH WIND AND SEAS" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "HIGH WIND DAMAGE" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "HIGH WIND/BLIZZARD" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "HIGH WIND/HEAVY SNOW" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "HIGH WIND/SEAS" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "HIGH WINDS" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "HIGH WINDS HEAVY RAINS" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "HIGH WINDS/" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "HIGH WINDS/COASTAL FLOOD" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "HIGH WINDS/COLD" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "HIGH WINDS/HEAVY RAIN" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "HIGH WINDS/SNOW" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "MARINE HAIL" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "MICROBURST" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "MICROBURST WINDS" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "NON TSTM WIND" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "NON-SEVERE WIND DAMAGE" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "NON-TSTM WIND" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "SEICHE" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "SEVERE TURBULENCE" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "SMALL HAIL" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "STRONG WIND" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "STRONG WINDS" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "TORNADO" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "TORNADOES, TSTM WIND, HAIL" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "TORNDAO" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "WATERSPOUT" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "WATERSPOUT-" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "WATERSPOUT TORNADO" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "WATERSPOUT/ TORNADO" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "WATERSPOUT/TORNADO" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "WATERSPOUT-TORNADO" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "WET MICROBURST" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "Whirlwind" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "WIND" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "WIND DAMAGE" |
       #stragglers 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "Dust Devil" |
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "gradient wind" |
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "Gusty winds" |
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "Microburst" |
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "Strong Wind" |
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "Strong Winds" |
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "WHIRLWIND" |
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "Wind" |
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "Wind Damage" |
       #stargglers
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "Heavy Surf" |
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "High Surf" |
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "Mudslide" |
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "Mudslides" |
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "River Flooding" |
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "Tidal Flooding" |
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "Gradient wind" |
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "Gusty Winds" )
    
    {StormDataDT_recode_summed_test$EVTYPE[[i]] <- "TORNADO, HAIL, HIGH WIND"}
  
    if(StormDataDT_recode_summed_test$EVTYPE[[i]] == "BRUSH FIRE" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "FOREST FIRES" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "GRASS FIRES" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "WILD FIRES" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "WILD/FOREST FIRE" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "WILD/FOREST FIRES" | 
       StormDataDT_recode_summed_test$EVTYPE[[i]] == "WILDFIRES" )

    {StormDataDT_recode_summed_test$EVTYPE[[i]] <- "WILDFIRE"} 
  
  if(StormDataDT_recode_summed_test$EVTYPE[[i]] == "AGRICULTURAL FREEZE" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "AVALANCE" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "AVALANCHE" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "BLACK ICE" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "BLIZZARD" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "BLIZZARD/WINTER STORM" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "BLOWING DUST" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "BLOWING SNOW" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "blowing snow" |
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "COLD" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "COLD AIR TORNADO" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "COLD AND SNOW" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "COLD AND WET CONDITIONS" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "Cold Temperature" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "COLD WAVE" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "COLD WEATHER" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "COLD/WIND CHILL" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "COLD/WINDS" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "DAMAGING FREEZE" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "Early Frost" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "EXCESSIVE WETNESS" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "Extended Cold" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "EXTREME COLD" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "EXTREME COLD/WIND CHILL" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "EXTREME WIND CHILL" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "EXTREME WINDCHILL" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "FALLING SNOW/ICE" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "FLASH FLOOD FROM ICE JAMS" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "FREEZE" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "FREEZING DRIZZLE" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "FREEZING FOG" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "FREEZING RAIN" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "FREEZING RAIN/SLEET" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "FREEZING RAIN/SNOW" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "Freezing Spray" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "FROST" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "Frost/Freeze" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "FROST/FREEZE" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "GLAZE" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "GLAZE ICE" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "GLAZE/ICE STORM" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "GROUND BLIZZARD" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "HARD FREEZE" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "HEAVY LAKE SNOW" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "HEAVY SNOW" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "HEAVY SNOW AND HIGH WINDS" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "HEAVY SNOW AND STRONG WINDS" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "Heavy snow shower" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "HEAVY SNOW SQUALLS" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "HEAVY SNOW/BLIZZARD" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "HEAVY SNOW/BLIZZARD/AVALANCHE" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "HEAVY SNOW/FREEZING RAIN" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "HEAVY SNOW/HIGH WINDS & FLOOD" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "HEAVY SNOW/ICE" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "HEAVY SNOW/SQUALLS" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "HEAVY SNOW/WIND" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "HEAVY SNOW/WINTER STORM" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "HEAVY SNOWPACK" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "HEAVY SNOW-SQUALLS" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "ICE" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "ICE AND SNOW" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "ICE FLOES" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "ICE JAM" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "Ice jam flood (minor" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "ICE JAM FLOODING" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "ICE ON ROAD" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "ICE ROADS" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "ICE STORM" |
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "ICE STORM/FLASH FLOOD" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "ICE/STRONG WINDS" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "ICY ROADS" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "Lake Effect Snow" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "LATE SEASON SNOW" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "LIGHT FREEZING RAIN" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "Light snow" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "Light Snowfall" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "LOW TEMPERATURE" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "RECORD COLD" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "RECORD SNOW" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "SLEET" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "SLEET/ICE STORM" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "SNOW" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "SNOW ACCUMULATION" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "SNOW AND HEAVY SNOW" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "SNOW AND ICE" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "SNOW AND ICE STORM" |
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "SNOW FREEZING RAIN" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "SNOW SQUALL" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "SNOW SQUALLS" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "SNOW/ BITTER COLD" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "SNOW/ ICE" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "SNOW/BLOWING SNOW" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "SNOW/COLD" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "SNOW/FREEZING RAIN" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "SNOW/HEAVY SNOW" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "SNOW/HIGH WINDS" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "SNOW/ICE" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "SNOW/ICE STORM" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "SNOW/SLEET" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "SNOW/SLEET/FREEZING RAIN" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "SNOWMELT FLOODING" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "Unseasonable Cold" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "UNSEASONABLY COLD" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "WINTER STORM" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "WINTER STORM HIGH WINDS" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "WINTER STORMS" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "WINTER WEATHER MIX" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "WINTER WEATHER/MIX" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "Wintry Mix" |
     #stragglers
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "Cold" |
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "Damaging Freeze" |
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "Extreme Cold" |
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "Freeze" |
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "Freezing Drizzle" |
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "Freezing Rain" |
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "Glaze" |
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "LAKE EFFECT SNOW" |
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "Light Snow" |
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "Snow" |
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "Snow Squalls" | 
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "WINTRY MIX" |
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "Freezing drizzle" |
     StormDataDT_recode_summed_test$EVTYPE[[i]] == "LIGHT SNOW" )

   {StormDataDT_recode_summed_test$EVTYPE[[i]] <- "WINTER WEATHER"}   

}  
 

 
#tests

filter(StormDataDT_recode_summed,EVTYPE == "DROUGHT")
filter(StormDataDT_recode_summed,EVTYPE == "DROUGHT, EXCESSIVE HEAT")
filter(StormDataDT_recode_summed_test,EVTYPE == "DROUGHT")
filter(StormDataDT_recode_summed_test,EVTYPE == "DROUGHT, EXCESSIVE HEAT")

filter(StormDataDT_recode_summed,EVTYPE == "URBAN/SMALL STREAM")
filter(StormDataDT_recode_summed,EVTYPE == "HEAVY RAIN, FLOODING, MUDSLIDES, LANDSLIDES")
filter(StormDataDT_recode_summed_test,EVTYPE == "URBAN/SMALL STREAM")
filter(StormDataDT_recode_summed_test,EVTYPE == "HEAVY RAIN, FLOODING, MUDSLIDES, LANDSLIDES")

filter(StormDataDT_recode_summed,EVTYPE == "TROPICAL STORM JERRY")
filter(StormDataDT_recode_summed,EVTYPE == "HURRICANE SEASON")
filter(StormDataDT_recode_summed_test,EVTYPE == "TROPICAL STORM JERRY")
filter(StormDataDT_recode_summed_test,EVTYPE == "HURRICANE SEASON")

filter(StormDataDT_recode_summed,EVTYPE == "VOLCANIC ASH" ) 
filter(StormDataDT_recode_summed,EVTYPE == "OTHER")
filter(StormDataDT_recode_summed_test,EVTYPE == "VOLCANIC ASH" ) 
filter(StormDataDT_recode_summed_test,EVTYPE == "OTHER")

filter(StormDataDT_recode_summed,EVTYPE == "TUNDERSTORM WIND" ) 
filter(StormDataDT_recode_summed,EVTYPE == "THUNDERSTORM")
filter(StormDataDT_recode_summed_test,EVTYPE == "TUNDERSTORM WIND" ) 
filter(StormDataDT_recode_summed_test,EVTYPE == "THUNDERSTORM")

filter(StormDataDT_recode_summed,EVTYPE == "WIND DAMAGE" ) 
filter(StormDataDT_recode_summed,EVTYPE == "TORNADO, HAIL, HIGH WIND")
filter(StormDataDT_recode_summed_test,EVTYPE == "WIND DAMAGE" ) 
filter(StormDataDT_recode_summed_test,EVTYPE == "TORNADO, HAIL, HIGH WIND")

filter(StormDataDT_recode_summed,EVTYPE == "WILDFIRES" ) 
filter(StormDataDT_recode_summed,EVTYPE == "WILDFIRE")
filter(StormDataDT_recode_summed_test,EVTYPE == "WILDFIRES" ) 
filter(StormDataDT_recode_summed_test,EVTYPE == "WILDFIRE")

filter(StormDataDT_recode_summed,EVTYPE == "Wintry Mix" ) 
filter(StormDataDT_recode_summed,EVTYPE == "WINTER WEATHER")
filter(StormDataDT_recode_summed_test,EVTYPE == "Wintry Mix" ) 
filter(StormDataDT_recode_summed_test,EVTYPE == "WINTER WEATHER")

#########################################
#a tibble
class(StormDataDT_recode_summed_test)
#sum for EV_TYPE

StormDataDT_recode_summed_final <- 
  StormDataDT_recode_summed_test %>% group_by(year, EVTYPE) %>%
  summarize(Property.and.Crop.Damage.Final = (sum(Property.and.Crop.Damage.Sum)/1000000),
          Fatalities.and.Injuries.Final = sum(Fatalities.and.Injuries.Sum))

write.csv(StormDataDT_recode_summed_final, "./data-test/StormDataDT_recode_summed_final.csv")

######plots

ggplot(data = StormDataDT_recode_summed_final) +
  geom_col(mapping = aes(x = year, y = Property.and.Crop.Damage.Final)) + 
  facet_wrap(~ EVTYPE, nrow = 2) +
  ggtitle("Total Property and Crop Damage Caused by Natural Disasters (Source: NOAA)", 
          subtitle = "1950-2011 For TORNADO..., 1993-2011 for other Categories, 
          \n United States and Territories, In $US Millions, Inflation Adjusted (2011 Dollars)") +
  ylab("Total Property and Crop Damage")

ggplot(data = StormDataDT_recode_summed_final) +
  geom_col(mapping = aes(x = year, y = Fatalities.and.Injuries.Final)) + 
  facet_wrap(~ EVTYPE, nrow = 2) +
  ggtitle("Total Fatalities and Injuries Caused by Natural Disasters (Source: NOAA)", 
          subtitle = "1950-2011 For TORNADO..., 1983-2011 for THUNDERSTORMS, 1993-2011 for other Categories, 
          \n United States and Territories") +
  ylab("Total Fatalities and Injuries")


sum(StormDataDT_recode_summed_final$FATALITIES)
sum(StormDataDT_recode_summed_final$INJURIES)
sum(StormDataDT_recode_summed_final$Fatalities.and.Injuries)
sum(StormDataDT_recode_summed_final$PROPDMG.Total)
sum(StormDataDT_recode_summed_final$CROPDMG.Total)
sum(StormDataDT_recode_summed_final$Property.and.Crop.Damage)

#For tables:
  
property_crop_damage_years <- StormDataDT_recode_summed_final %>% group_by(EVTYPE) %>% 
  summarize(Property.and.Crop.Damage.Years = sum(Property.and.Crop.Damage.Final)) %>% 
  arrange(desc(Property.and.Crop.Damage.Years)) 
  

fatalities_injuries_years <- StormDataDT_recode_summed_final %>% group_by(EVTYPE) %>% 
  summarize(Fatalities.and.Injuries.Years = sum(Fatalities.and.Injuries.Final)) %>%
  arrange(desc(Fatalities.and.Injuries.Years))

StormDataDT_recode_since1993_final <- filter(StormDataDT_recode_summed_final, year >= 1993) 

property_crop_damage_years_since1993 <- StormDataDT_recode_since1993_final %>% group_by(EVTYPE) %>% 
  summarize(Property.and.Crop.Damage.Years = sum(Property.and.Crop.Damage.Final)) %>% 
  arrange(desc(Property.and.Crop.Damage.Years)) 


fatalities_injuries_years_since1993 <- StormDataDT_recode_since1993_final %>% group_by(EVTYPE) %>% 
  summarize(Fatalities.and.Injuries.Years = sum(Fatalities.and.Injuries.Final)) %>%
  arrange(desc(Fatalities.and.Injuries.Years))

ggplot(data = StormDataDT_recode_since1993_final) +
  geom_col(mapping = aes(x = year, y = Property.and.Crop.Damage.Final)) + 
  facet_wrap(~ EVTYPE, nrow = 2) +
  ggtitle("Total Property and Crop Damage Caused by Natural Disasters (Source: NOAA)", 
          subtitle = "1993-2011, United States and Territories, In $US Millions, Inflation Adjusted (2011 Dollars)") +
  ylab("Total Property and Crop Damage")

