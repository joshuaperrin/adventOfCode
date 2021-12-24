library(tidyverse)

dayThree <- read.csv("dayThreeInput.csv", header = FALSE, colClasses = "character") %>%
  rename(Binary = V1)



binarySplit = strsplit(dayThree$Binary, split = "")

binaryTable = data.frame(t(sapply(binarySplit, c))) %>%
  tibble() %>%
  mutate(across(.fns = as.integer))

gammaVector <- c()
epsilonVector <- c()

for(i in 1:12){
  print(sum(binaryTable[i])) #Checking that none have equal numbers of digits.
}
  
for(i in 1:12){
  if(sum(binaryTable[i]) > 500){
    gammaVector[i] <- "1"
    epsilonVector[i] <- "0"}
  else{
    gammaVector[i] <- "0"
    epsilonVector[i] <- "1"
  }
  
}

gammaResult = paste0(gammaVector, collapse = "") %>%
  strtoi(base = 2)
  
epsilonResult = paste0(epsilonVector, collapse = "") %>%
  strtoi(base = 2)

answer = gammaResult * epsilonResult

#Part 2.1 - Oxygen

binaryTableWork <- binaryTable

oxygenVector <- c()

vars <- names(binaryTableWork)


  for(i in 1:12){
  if(sum(binaryTableWork[i]) >= nrow(binaryTableWork)/2){
      oxygenVector[i] <- "1"
}
  else{
    oxygenVector[i] <- "0"
  }
  binaryTableWork <- binaryTableWork %>% filter(
    .data[[vars[[i]]]] == oxygenVector[i])
  }

oxygenResult = paste0(binaryTableWork, collapse = "") %>% #This works fine surprisingly
  strtoi(base = 2)

###

#Part 2.2 - Scrubber

binaryTableWork <- binaryTable

scrubberVector <- c()

vars <- names(binaryTableWork)

#Just reusing code here...


for(i in 1:12){
  if(nrow(binaryTableWork) > 1){
  if(sum(binaryTableWork[i]) >= nrow(binaryTableWork)/2){ #Had to consider this inequality carefully.
    scrubberVector[i] <- "0"}
  else{
    scrubberVector[i] <- "1"
  }
  binaryTableWork <- binaryTableWork %>% filter(
    .data[[vars[[i]]]] == scrubberVector[i])
  }
}



scrubberResult = paste0(binaryTableWork, collapse = "") %>% #This works fine surprisingly
  strtoi(base = 2)


answer = oxygenResult * scrubberResult

