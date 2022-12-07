## Problem 5

# Part 1

#rm(list = ls())

setwd("C:/Users/whitn/OneDrive/Documents/Code/AdventOfCode2022")
data <- read.table("Problem 5.csv", header=FALSE,fileEncoding="UTF-8-BOM",sep=",",colClasses = 'character')

# dataSep <- separate(data, 1,c("a","b","c"),sep= ' ')
# dataSep <- dataSep[1:nrow(dataSep)-1,]
# 
# a <- dataSep$a
# b <- dataSep$b[dataSep$b != ""]
# c <- dataSep$c[dataSep$c != ""]

data <- data[-1,-1]
#data<- data[c(1:3),c(1:3)]

a <- data$V2[data$V2 != "   "]
b <- data$V3[data$V3 != "   "]
c <- data$V4[data$V4 != "   "]
d <- data$V5[data$V5 != "   "]
e <- data$V6[data$V6 != "   "]
f <- data$V7[data$V7 != "   "]
g <- data$V8[data$V8 != "   "]
h <- data$V9[data$V9 != "   "]
i <- data$V10[data$V10 != "   "]

#boxes<- list(a,b,c)
boxes<- list(a,b,c,d,e,f,g,h,i)

instructions <- read_clip_tbl(header = FALSE, sep = " ")
inst <- cbind(instructions$V2,instructions$V4, instructions$V6)


i<-1

while (i <= nrow(inst))
{
 j <- 1
   while (j <= inst[i,1])
  {
    to <- unlist(boxes[inst[i,3]])
    from <- unlist(boxes[inst[i,2]])
    new <- append(to,from[1],0)
    boxes[inst[i,3]] <- list(new)
    from <- from[-1]
    boxes[inst[i,2]] <- list(from)
    
  }
  i <- i+1
}

boxes

# part 2
i<-1

while (i <= nrow(inst))
{
    to <- unlist(boxes[inst[i,3]])
    from <- unlist(boxes[inst[i,2]])
    new <- append(to,from[c(1:inst[i,1])],0)
    boxes[inst[i,3]] <- list(new)
    from <- from[-c(1:inst[i,1])]
    boxes[inst[i,2]] <- list(from)
    
  
  i <- i+1
}

boxes
BPCZJLFJW