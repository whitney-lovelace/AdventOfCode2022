##Problem 4

## Part 1

install.packages("clipr")
install.packages("car")
install.packages("vecsets")
install.packages("stringr")
install.packages("tidyr")
library("clipr")
library("car")
library("vecsets")
library("stringr")
library("tidyr")

setwd("C:/Users/whitn/OneDrive/Documents/Code/AdventOfCode2022")
#data <- read.table("Problem_One.csv", header=FALSE,fileEncoding="UTF-8-BOM")

data <- read_clip_tbl(header = FALSE, sep = " ")
data2 <- separate(data, 1,c("a","b"),sep= ',')

data3 <- separate(data2,1,c("a.1","a.2"),sep='-')
data4 <- separate(data3,3,c("b.1","b.2"),sep='-')

createNum <- matrix(as.numeric(unlist(data4)),ncol=4)

createNum$match <- (createNum[,1] >= createNum[,3] & createNum[,2] <= createNum[,4]) | (createNum[,3] >= createNum[,1] & createNum[,4] <= createNum[,2])

sum(createNum$match)

# matches <- c()
# i <-1
# while (i <= nrow(createNum))
# {
#   x <- c(createNum[i,1]:createNum[i,2])
#   y <- c(createNum[i,3]:createNum[i,4])
#   if (length(y) >= length(x))
#   {
#     if (any(apply(embed(y,length(y)-length(x)+1),2,identical,x)))
#     {
#     matches[i] <- 1
#     }else {matches[i]<-0}
#   }  
#   else if (length(x) > length(y))
#   {
#     if (any(apply(embed(x,length(x)-length(y)+1),2,identical,y)))
#   {
#     matches[i] <- 1
#   } else {matches[i]<-0}}
#   else {matches[i] <- 0
#   }
#   i <- i+1
# 
# }
# sum(matches)

## Problem 2
data <- read_clip_tbl(header = FALSE, sep = " ")
data2 <- separate(data, 1,c("a","b"),sep= ',')

data3 <- separate(data2,1,c("a.1","a.2"),sep='-')
data4 <- separate(data3,3,c("b.1","b.2"),sep='-')

createNum <- matrix(as.numeric(unlist(data4)),ncol=4)

createNum$match <- (createNum[,2] <= createNum[,4] & createNum[,2] >= createNum[,3]) | (createNum[,4] <= createNum[,2] & createNum[,4] >= createNum[,1])
sum(createNum$match)
