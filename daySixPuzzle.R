library(tidyverse)
library(data.table)

# Part 1

lfv = scan("daySixInput.txt", what = numeric(), sep = ",")

for(i in 1:80){
  lfv = lfv - 1
  newFish = length(which(lfv == -1))
  lfv[which(lfv == -1)] <- 6
  lfv = append(lfv, rep(8, newFish))
}

answer = length(lfv)

# Part 2

lfv = scan("daySixInput.txt", what = numeric(), sep = ",")

for(i in 1:10){
  lfv = lfv - 1
  newFish = length(which(lfv == -1))
  lfv[which(lfv == -1)] <- 6
  lfv = append(lfv, rep(8, newFish))
}

e = as.numeric(table(lfv))



for(i in 1:246){
f = c()
f[1] <- e[2]
f[2] <- e[3]
f[3] <- e[4]
f[4] <- e[5]
f[5] <- e[6]
f[6] <- e[7]
f[7] <- e[8] + e[1]
f[8] <- e[9]
f[9] <- e[1]
e = f
}

359344


answer = sum(e)

paste0(answer)
