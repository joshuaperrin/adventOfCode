library(tidyverse)

#Part one.

dayTwo <- read.csv("dayTwoInput.csv", header = FALSE) %>%
  rename(Movements = V1) %>%
  separate(Movements, c("Movements", "Magnitude")) %>%
  mutate(Magnitude = as.integer(Magnitude))

unique(dayTwo$Movement) #Checking there's no backward movement.


horizontalPosition <- 0
verticalDepth <- 0


for(i in 1:nrow(dayTwo)){
  if(dayTwo$Movements[i] == "forward"){
    horizontalPosition <- horizontalPosition + dayTwo$Magnitude[i]
  } else if(dayTwo$Movements[i] == "down"){
    verticalDepth <- verticalDepth + dayTwo$Magnitude[i]
  } else if(dayTwo$Movements[i] == "up"){
    verticalDepth <- verticalDepth - dayTwo$Magnitude[i]
  } else{
    print("Error.")
  }
  
}

answer = horizontalPosition * verticalDepth

#Part two.

horizontalPosition <- 0
verticalDepth <- 0
currentAim <- 0

for(i in 1:nrow(dayTwo)){
  if(dayTwo$Movements[i] == "forward"){
    horizontalPosition <- horizontalPosition + dayTwo$Magnitude[i]
    verticalDepth <- verticalDepth + dayTwo$Magnitude[i]*currentAim
  } else if(dayTwo$Movements[i] == "down"){
    currentAim <- currentAim + dayTwo$Magnitude[i]
  } else if(dayTwo$Movements[i] == "up"){
    currentAim <- currentAim - dayTwo$Magnitude[i]
  } else{
    print("Error.")
  }
  
}

answer = horizontalPosition * verticalDepth

