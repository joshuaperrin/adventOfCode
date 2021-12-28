library(tidyverse)
library(data.table)

#Part 1

signals = fread("dayEightInput.txt", header = FALSE) %>%
  rename(unique = V1, output = V2)

#2 4 3 7

output = signals %>%
  select(output)

outputRaw = strsplit(pull(output), split = " ") %>%
  unlist()

lengthVector = c()

for(i in 1:length(outputRaw)){
  lengthVector = append(lengthVector, nchar(outputRaw[i]))
}

answer = length(lengthVector[lengthVector %in% c(2, 4, 3, 7)])

#Part 2

#Length 2 - C and F

#Length 3 - C, F, and A

#Length 4 - C, B, D, F

#Length 7 - All

unique = signals %>%
  select(unique)

uniqueRaw <- strsplit(pull(unique), split = " ")


# 
# order(uniqueRaw[[k]][nchar(uniqueRaw[[k]]) == 3])
# 
# str_remove(, uniqueRaw[[k]][nchar(uniqueRaw[[k]]) == 2])



sortString = function(x){
  strsplit(x, split = "") %>%
  unlist() %>%
  sort() %>%
  paste(collapse = "")
}

rawCut <- function(i, n){
  unlist(strsplit(uniqueRaw[[i]][nchar(uniqueRaw[[i]]) == n], split = ""))
}



for(k in 1:200){
  
  characterA = rawCut(k,3)[rawCut(k, 3) %ni% rawCut(k, 2)]
  
  
  characterBD = rawCut(k,4)[rawCut(k, 4) %ni% rawCut(k, 2)]
  
  characterEG = rawCut(k, 7)[rawCut(k, 7) %ni% append(rawCut(k, 4), characterA)]
  
  characterABDEG <- c(characterA, characterBD, characterEG)
  
  rawCutTwo <- function(i, n, position){
    unlist(strsplit(uniqueRaw[[i]][nchar(uniqueRaw[[i]]) == n], split = ""))[position]
  }
  
  
  
  for(i in c(1, 7, 13)){
    sixVector = rawCutTwo(k, 6, i:(i+5))
    
    if(length(sixVector[sixVector %ni% characterABDEG]) == 1){
      characterF = sixVector[sixVector %ni% characterABDEG]
    }
  }
  
  characterABDEFG <- c(characterA, characterBD, characterEG, characterF)
  
  characterC = letters[1:7][letters[1:7] %ni% characterABDEFG]
  
  characterABDF <- c(characterA, characterBD, characterF)
  
  
  for(i in c(1, 6, 11)){
    fiveVector = rawCutTwo(k, 5, i:(i+4))
    
    if(length(fiveVector[fiveVector %ni% characterABDF]) == 1){
      characterG = fiveVector[fiveVector %ni% characterABDF]
    }
  }
  
  characterE = characterEG[characterEG != characterG]
  
  characterACEG <- c(characterA, characterC, characterE, characterG)
  
  for(i in c(1, 6, 11)){
    fiveVector = rawCutTwo(k, 5, i:(i+4))
    
    if(length(fiveVector[fiveVector %ni% characterACEG]) == 1){
      characterD = fiveVector[fiveVector %ni% characterACEG]
    }
  }
  
  characterB = characterBD[characterBD != characterD]
  
  bindLetters <- tibble(a = characterA, b = characterB, c = characterC,
                        d = characterD, e = characterE, f = characterF,
                        g = characterG)
  
  letterSort = bind_rows(letterSort, bindLetters)
  
  
}

unFix <- function(index, lettersCount){
sort(unname(unlist(letterSort[index,lettersCount])))
}

zeroValue = unFix(1, c(1:3, 4:7))
oneValue = unFix(1, c(3, 6))
twoValue = unFix(1, c(1, 3, 4, 5, 7))
threeValue = unFix(1, c(1, 3, 4, 6, 7))


  