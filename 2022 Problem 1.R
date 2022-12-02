##Problem 1

## Part 1

install.packages("clipr")
library("clipr")

setwd("C:/Users/whitn/OneDrive/Documents/Code/AdventOfCode2022")
#data2 <- read.table("Problem_One.csv", header=FALSE,fileEncoding="UTF-8-BOM")


data3 <- read_clip()
data4 <- as.integer(data3)

i <- 1
totals <- c()
sum <- 0

while (i<= length(data4)){
  if (is.na(data4[i])){
    totals <- append(totals, sum)
    sum <-0
    i <- i+1
  } else {
    sum <- sum + data4[i]
    i <-i+1
  }
}

max(totals)

#Part 2

top3 <- sort(totals, decreasing = TRUE)
top3sum <- sum(top3[1:3])
