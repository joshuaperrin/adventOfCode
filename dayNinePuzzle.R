library(tidyverse)
library(data.table)

rm(list = ls())

`%ni%` <- Negate(`%in%`)

#Part one

map = readLines("dayNineInput.txt") %>%
  strsplit(split = "") %>%
  unlist() %>%
  as.integer()

addMatrix <- matrix(map, nrow = 100, ncol = 100)



mapMatrix <- matrix(nrow = 102, ncol = 102)

mapMatrix[2:101, 2:101] <- addMatrix     

totalRisk <- 0

for(i in 2:101){
  for(j in 2:101){
    currentValue <- mapMatrix[i, j]
    if((mapMatrix[i - 1, j] > currentValue | is.na(mapMatrix[i - 1, j]) == TRUE)
       & (mapMatrix[i + 1, j] > currentValue | is.na(mapMatrix[i + 1, j]) == TRUE)
       & (mapMatrix[i, j - 1] > currentValue | is.na(mapMatrix[i, j - 1]) == TRUE)
       & (mapMatrix[i, j + 1] > currentValue | is.na(mapMatrix[i, j + 1]) == TRUE)){
      totalRisk = totalRisk + currentValue + 1
    }
  }
}
          

answer = totalRisk        #This answer seems very low for a 100*100 matrix but it's right.

#Part two

#Needs a search algorithm. I have never done this before apparently.


