##Problem 1

## Part 1

setwd("C:/Users/whitn/OneDrive/Documents/Code/AdventOfCode2022")
data <- read.csv("Problem_One.csv", header=TRUE,fileEncoding="UTF-8-BOM")

x <- data$Problem

dif <- 2020 - x
difInx <- dif %in% x

values <- difInx*x

values2multiply <- values[values>0]

answer <- prod(values2)
