library(tidyverse)
rm(list = ls())

submarineValues =  read_tsv("dayFiveInput.txt", col_names = FALSE) %>%
separate(col = X1, into = c("StartX", "StartY", "EndX", "EndY")) %>%
  mutate(across(.fns = as.integer))


submarineValues_A = submarineValues %>%
  filter(StartX == EndX | StartY == EndY)

submarineValuesX = submarineValues_A %>%
  filter(StartX != EndX) %>%
  select(-EndY) %>%
  rename(Y = StartY) 

submarineValuesY = submarineValues_A %>%
  filter(StartY != EndY)%>%
  select(-EndX) %>%
  rename(X = StartX)


matrixSubmarine <- matrix(0, nrow = 1000, ncol = 1000)


for(i in 1:nrow(submarineValuesX)){

matrixSubmarine[pull(submarineValuesX, StartX)[i]:pull(submarineValuesX, EndX)[i], pull(submarineValuesX, Y)[i]] <- 
  matrixSubmarine[pull(submarineValuesX, StartX)[i]:pull(submarineValuesX, EndX)[i], pull(submarineValuesX, Y)[i]] + 1

}

for(i in 1:nrow(submarineValuesY)){
  
  matrixSubmarine[pull(submarineValuesY, X)[i], pull(submarineValuesY, StartY)[i]:pull(submarineValuesY, EndY)[i]] <- 
    matrixSubmarine[pull(submarineValuesY, X)[i], pull(submarineValuesY, StartY)[i]:pull(submarineValuesY, EndY)[i]] + 1
  
}

length(which(matrixSubmarine > 1))


# submarineValuesFinal <- tibble(X = numeric(), Y = )
# for(i in 1:nrow(submarineValuesX)){
#   
# }


# for(i in 1:nrow(submarineValuesX)){
#   for(j in 1:nrow(submarineValuesY)){
#     if(submarineValuesY$X[j] %in% range(submarineValuesX$StartX[i]:submarineValuesX$EndX[i])){
#       submarineX = append(submarineX, submarineValuesY$X[j])   
#     }
#   }
# }

submarineValues_B = submarineValues %>%
  filter(StartX != EndX & StartY != EndY) %>%
  mutate(MovementX = StartX - EndX, MovementY = StartY - EndY) %>%
  mutate(Sign = if_else(MovementX == MovementY, "Positive", "Negative"))
