
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