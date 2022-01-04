library(data.table)
library(tidyverse)

rm(list = ls())

syntax <- readLines("dayTenInput.txt")

delimiterStack = c()

errorList = c("1")



lastCheck <- function(o, c){
  if(substr(syntax[i], j, j) == c){
    if(tail(delimiterStack, 1) == o){
      delimiterStack = delimiterStack[-length(delimiterStack)]
    } else {
      warning("help")
      errorList = append(errorList, paste0(i))
    }
      
    }
  }


for(i in 1:length(syntax)){
  delimiterStack = c()
  for(j in 1:nchar(syntax[i])){
    if(substr(syntax[i], j, j) %in% c("<", "(", "{", "[")){
      delimiterStack = append(delimiterStack, substr(syntax[i], j, j))
    }
    lastCheck("[", "]")
    lastCheck("(", ")")
    lastCheck("{", "}")
    lastCheck("<", ">")
  }
}

# for(i in 1:length(syntax)){
#   triangleBracket <- 0
#   squareBracket <- 0
#   curvedBracket <- 0
#   curlyBracket <- 0
#   for(j in 1:nchar(syntax[i])){
#     if(substr(syntax[i], j, j) == "<"){
#       triangleBracket = triangleBracket + 1
#   } else 
#     if(substr(syntax[i], j, j) == ">"){
#       triangleBracket = triangleBracket - 1
#     } else
#       if(substr(syntax[i], j, j) == "["){
#         squareBracket = squareBracket + 1
# } else 
#   if(substr(syntax[i], j, j) == "]"){
#     squareBracket = squareBracket - 1
#   } else
#     if(substr(syntax[i], j, j) == "("){
#       curvedBracket = curvedBracket + 1
#     } else 
#       if(substr(syntax[i], j, j) == ")"){
#         curvedBracket = curvedBracket - 1
#       } else
#         if(substr(syntax[i], j, j) == "{"){
#           curlyBracket = curlyBracket + 1
#         } else 
#           if(substr(syntax[i], j, j) == "}"){
#             curlyBracket = curlyBracket - 1
#           }
#     if(curvedBracket == -1){
#       break(print("Curly"))
#     }
#   }
# }
            
# breaker = syntax[1] %>%
#   str_extract_all("[\\[\\]]") %>%
#   unlist()
# 
# counter = 0
# 
# for(j in 1:length(breaker)){
#   counter = if_else(breaker[j] == "[", counter + 1, counter - 1)
#   if(counter < 0){
#     stop("Counter below zero.")
#   }
# }
# 
# errorList = c("1")
# 
# regex <- function(open, close, syntaxCount){
#   breaker = syntax[syntaxCount] %>%
#     str_extract_all(paste0("[\\", open, "\\", close, "]")) %>%
#     unlist()
#   
#   counter = 0
#   
#   for(j in 1:length(breaker)){
#     counter = if_else(breaker[j] == open, counter + 1, counter - 1)
#     if(counter < 0){
#       warning(paste0("Counter below zero for ", open, close, " in line ", syntaxCount))
#     }
#   }
#   
# }
# 
# for(i in 1:94){
# regex("(", ")", i)
# regex("[", "]", i)
# regex("<", ">", i)
# regex("{", "}", i)
# }
# 



