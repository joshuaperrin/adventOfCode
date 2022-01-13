library(data.table)
library(tidyverse)

rm(list = ls())

`%ni%` <- Negate(`%in%`)

syntax <- readLines("dayTenInput.txt")

substr(syntax[1], 1, 1)

stack = c(1, 2, 3)

tail(stack, 1)

wrongVec = c()
wrongLines = c()

#syntax = append(syntax, "<<<>>><]")


for(i in 1:94){
  stack = c()
  for(j in 1:nchar(syntax[i])){
    cChar <- substr(syntax[i], j, j)
    if(cChar %in% c("<", "[", "{", "(")){
      stack = append(stack, cChar)
    }
    else {
      verification = paste0(tail(stack, 1), cChar)
      if(verification %in% c("<>", "{}", "()", "[]")){
        stack = stack[-length(stack)]
      } else {
        wrongVec = append(wrongVec, paste0(cChar))
        wrongLines = append(wrongLines, i)
        break
      }
    }
  }
}
    

# ): 3 points.
# ]: 57 points.
# }: 1197 points.
# >: 25137 points.


table(wrongVec)

answer = 3 * 7 + 57 * 12 + 15 * 1197 + 13 * 25137    

#Part Two

paste0(1:3, collapse = "")

incompleteLines = (1:94)[1:94 %ni% wrongLines]

syntaxIncomplete = syntax[incompleteLines]

uncompleted = c()

for(i in 1:47){
  stack = c()
  for(j in 1:nchar(syntaxIncomplete[i])){
    cChar <- substr(syntaxIncomplete[i], j, j)
    if(cChar %in% c("<", "[", "{", "(")){
      stack = append(stack, cChar)
    }
    else {
      verification = paste0(tail(stack, 1), cChar)
      if(verification %in% c("<>", "{}", "()", "[]")){
        stack = stack[-length(stack)]
      }
    }
    if(j == nchar(syntaxIncomplete[i])){
      uncompleted = append(uncompleted, paste0(stack, collapse = ""))

      }
    }
  }

correctionList = c()

for(i in 1:47){
  correctStack = ""
  for(j in nchar(uncompleted[i]):1){
    correctChar = case_when(
      substr(uncompleted[i], j, j) == "<" ~ ">",
      substr(uncompleted[i], j, j) == "[" ~ "]",
      substr(uncompleted[i], j, j) == "{" ~ "}",
      substr(uncompleted[i], j, j) == "(" ~ ")",
      TRUE ~ "FLAGGED!"
    )
    correctStack = paste0(correctStack, correctChar)
  }
  correctionList = append(correctionList, correctStack)
}

scoreList = c()



for(i in 1:47){
  count = 0
  for(j in 1:nchar(correctionList[i])){
    count = count * 5
    addition = case_when(
      substr(correctionList[i], j, j) == ")" ~ 1,
      substr(correctionList[i], j, j) == "]" ~ 2,
      substr(correctionList[i], j, j) == "}" ~ 3,
      substr(correctionList[i], j, j) == ">" ~ 4
    )
    if(substr(correctionList[i], j, j) %ni% c(")", "]", "}", ">")){
      stop("What?")
    }
    count = count + addition
  }
  scoreList = append(scoreList, count)
}

answer = sort(scoreList)[24]
    
  