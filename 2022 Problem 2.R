##Problem 2

## Part 1

install.packages("clipr")
install.packages("car")
library("clipr")
library("car")


setwd("C:/Users/whitn/OneDrive/Documents/Code/AdventOfCode2022")
#data2 <- read.table("Problem_One.csv", header=FALSE,fileEncoding="UTF-8-BOM")


data <- read_clip_tbl(header = FALSE, sep = " ")

them <- as.numeric(factor(data[,1]))
me <- as.numeric(factor(data[,2]))

results <- me - them

scores <- recode(results, "c(0)=3; c(-1, 2)=0; else=6") + me
sum(scores)

## Part 2
data <- read_clip_tbl(header = FALSE, sep = " ")
data$V1 <- as.numeric(factor(data[,1]))
data$V2 <- as.numeric(factor(data[,2]))

data[data$V1==1 & data$V2 == 1,3] <- 3
data[data$V1==1 & data$V2 == 2,3] <- 1
data[data$V1==1 & data$V2 == 3,3] <- 2
data[data$V1==2 & data$V2 == 1,3] <- 1
data[data$V1==2 & data$V2 == 2,3] <- 2
data[data$V1==2 & data$V2 == 3,3] <- 3
data[data$V1==3 & data$V2 == 1,3] <- 2
data[data$V1==3 & data$V2 == 2,3] <- 3
data[data$V1==3 & data$V2 == 3,3] <- 1

names(data)[1] <- "them"
names(data)[2] <- "outcome"
names(data)[3] <- "me"

results <- data$me - data$them

scores <- recode(results, "c(0)=3; c(-1, 2)=0; else=6") + data$me
sum(scores)
