library(data.table)
library(tidyverse)

rm(list = ls())

syntax <- readLines("dayTenInput.txt")

substr(syntax[1], 1, 1)

triangleBracket <- 0
squareBracket <- 0
curvedBracket <- 0
curlyBrack <- 0


for(i in 1:length(syntax)){
  for(j in 1:nchar(syntax[i]){
    if(substr(syntax[i], j, j) == "<")
      triangleBracket = triangleBracket + 1
  } else 
    if(substr(syntax[i], j, j) == ">"{
      triangleBracket = triangleBracket + 1
    } else
      if(substr(syntax[i], j, j) == "[")
        squareBracket = squareBracket + 1
} else 
  if(substr(syntax[i], j, j) == "]"{
    squareBracket = squareBracket + 1
  } else
      
    
  