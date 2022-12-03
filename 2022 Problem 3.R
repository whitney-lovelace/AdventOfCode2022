##Problem 2

## Part 1

install.packages("clipr")
install.packages("car")
install.packages("vecsets")
install.packages("stringr")
library("clipr")
library("car")
library("vecsets")
library("stringr")

setwd("C:/Users/whitn/OneDrive/Documents/Code/AdventOfCode2022")
#data <- read.table("Problem_One.csv", header=FALSE,fileEncoding="UTF-8-BOM")

data2 <-data
data <- read_clip_tbl(header = FALSE, sep = " ")
test <- data


lengthStrings <- apply(data,1,nchar)/2
test$length1 <- lengthStrings
test$length2 <- lengthStrings*2


firstHalf <- substr(test$V1,1,test$length1)
#test2 <- str_split_fixed(firstHalf,"",max(nchar(firstHalf)))

secondHalf <- substr(test$V1,test$length1+1,test$length2)

commonLetters <- c()
for (x in 1:length(firstHalf))
{
  commonLetters[x] <- vintersect(strsplit(firstHalf, "")[[x]], strsplit(secondHalf, "")[[x]], multiple = FALSE)
}

key <- matrix(c(letters,LETTERS,c(1:52)),nrow=52)

sum(match(commonLetters,key))

## Part 2
data <- read_clip_tbl(header = FALSE, sep = " ")

groups <- matrix(unlist(test),ncol=3, byrow = TRUE)
first <- groups[,1]
second <- groups[,2]
third <- groups[,3]

commonLetters <- c()
for (x in 1:length(first))
{
  commonLetters[x] <- vintersect(vintersect(strsplit(first, "")[[x]], strsplit(second, "")[[x]], multiple = FALSE),strsplit(third, "")[[x]], multiple = FALSE)
}

key <- matrix(c(letters,LETTERS,c(1:52)),nrow=52)

sum(match(commonLetters,key))