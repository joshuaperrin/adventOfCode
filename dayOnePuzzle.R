library(tidyverse)

#Part one.

dayOneInput <- "dayOneInputPuzzleOne.csv"

depthReport <- read.csv(dayOneInput, header = FALSE)

increaseCount <- 0

for(i in 1:(nrow(depthReport)-1)){
  if(depthReport[i+1,1] > depthReport[i,1]){
    increaseCount <- increaseCount + 1
  } else{
    
  }
}



#increaseCount gives us the answer, which is correct. 
#Part two.

slidingWindowVector <- c()

for(j in 1:(nrow(depthReport)-2)){
  slidingWindowVector[j] = sum(depthReport[j, 1], depthReport[j+1, 1], depthReport[j+2, 1]) #Not clear why I can't do sum(depthReport[j:j2, 1]) here? It doesn't work properly. 
}

windowCount <- 0

for(k in 1:(length(slidingWindowVector)-1)){
  if(slidingWindowVector[k+1] > slidingWindowVector[k]){
    windowCount <- windowCount + 1
  } else{
    
  }
}

#windowCount gives us the answer, which is correct.

