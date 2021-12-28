rm(list = ls())

#Part one

chp <- scan("daySevenInput.txt", sep = ",")

differenceCalc <- function(x, i){
  abs(x - i)
}

positionVector <- c()

for(i in 1:2000){
  positionVector <- append(positionVector, sum(differenceCalc(chp, i)))
}

answer = min(positionVector)


#Part two

# 
# 
# differenceCalcOdd <- function(x, i){
#     abs(x - i) + (((abs(x - i) - 1)/2) * abs(x - i))
#   } 
# 
# differenceCalcEven <- function(x, i){
#   (((abs(x - i))/2) * abs(x - i))
# }
# 
# # differenceCalcTwo <- function(x, i){
# #   if(abs(x - i) %% 2 == 1){
# #     differenceCalcOdd(x, i)
# #   } else {
# #     differenceCalcEven(x, i)
# #   }
# # }
# 
# chpEven <- chp[chp %% 2 == 0]
# 
# chpOdd <- chp[chp %% 2 == 1]
# 
# positionVector <- c()
# 
# for(i in 1:2000){
#   if(i %% 2 == 0){
#   positionVector <- append(positionVector, 
#                            sum(differenceCalcOdd(chpOdd, i)) + sum(differenceCalcEven(chpEven, i)))
#   } else {
#     positionVector <- append(positionVector, 
#                              sum(differenceCalcEven(chpOdd, i)) + sum(differenceCalcOdd(chpEven, i)))
#   }
#     
# }

positionVector <- c()


differenceCalcTwo <- function(x, i){
  if(abs(x - i) != 0){
 sum(1:abs(x - i))
  } else {
    0
  }
}
 


for(i in 1:2000){
  differenceCount = 0
  for(j in 1:1000){
    differenceCount = differenceCount + differenceCalcTwo(chp[j], i)

  }
  positionVector <- append(positionVector, differenceCount)
}


answer = min(positionVector)

