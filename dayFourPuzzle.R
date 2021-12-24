library(data.table)
library(tidyverse)

bingoSheets = readLines("dayFourInputSheets.txt")

bingoSheets = bingoSheets[!grepl("^$", bingoSheets)]

bingoSheets = strsplit(bingoSheets, split = " ") %>%
  unlist()

bingoSheets = as.numeric(bingoSheets[!bingoSheets == ""])

bingoArray <- array(bingoSheets, dim = c(5, 5, 100))




bingoLines = tibble(BoardNumber = 1:100, Line = 1:100)

blankLines <- c()


for(i in 1:100){
  for(j in 1:5){
    blankLines[j + 10*(i-1)] <- list(bingoArray[1:5, j, i])

  }
  for(j in 1:5){
    blankLines[j + 5 + 10*(i-1)] <- list(bingoArray[j, 1:5, i])

  }
}




bingoCalls = fread("dayFourInputCalls.txt")

for(i in 1:100){
  for(j in 1:1000){
    if(sum(bingoCalls[,1:i] %in% blankLines[[j]]) == 5){
      print(j)
      print(i)
      print(blankLines[[j]])
      
    }
  }
}

ceiling(38.4)

uncalledNumbers <- sum(bingoArray[1:5, 1:5, 39][!bingoArray[1:5, 1:5, 39] %in% bingoCalls[,1:24]])

justCalled <- pull(bingoCalls[,24])

#Below loop prints out the sheet in true/false format

for(i in 1:5){

print(bingoArray[i, 1:5, 39] %in% bingoCalls[,1:24] )
  
}

answer = justCalled * uncalledNumbers

#Part two

sheetCounter <- c()

#We append each board's ID to a vector and find what's the last one.

for(i in 1:100){
  for(j in 1:1000){
    if(sum(bingoCalls[,1:i] %in% blankLines[[j]]) == 5){
      # print(j)
      # print(i)
      # print(blankLines[[j]])
      sheetCounter <- append(sheetCounter, ceiling(j/10)) %>%
        unique()
    }
  }
}

#It's 87. When does 87 win?

for(i in 1:100){
  for(j in 861:870) #It would have been better to have a board ID...this caused problems.
    if(sum(bingoCalls[,1:i] %in% blankLines[[j]]) == 5){
      print(i)
      print(blankLines[[j]])
      stop()

    }
  }

#After 89 calls.

for(i in 1:5){
  
  print(bingoArray[i, 1:5, 87] %in% bingoCalls[,1:89] )
  
}



uncalledNumbers <- sum(bingoArray[1:5, 1:5, 87][!bingoArray[1:5, 1:5, 87] %in% bingoCalls[,1:89]])

justCalled <- pull(bingoCalls[,89])

answer = justCalled * uncalledNumbers



#Not wholly happy with this one...I would've rather stayed in the array. One to look for discussion around.