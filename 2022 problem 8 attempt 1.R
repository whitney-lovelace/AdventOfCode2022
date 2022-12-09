## Day 8

#Part 1
#rm(list = ls())
#data <- read_clip_tbl(header = FALSE, sep = " ")
data <- read.table("Problem 8.csv", header=FALSE,fileEncoding="UTF-8-BOM",sep=",",colClasses = 'character')

data2 <- str_split_fixed(data$V1,'',5)
forest <- as.data.frame(data2)

visible <- as.data.frame(matrix(data = c(0), ncol = ncol(forest),nrow = nrow(forest)))

visible[,1] <- 1
visible[1,] <- 1
visible[,ncol(visible)] <- 1
visible[nrow(visible),] <- 1
   
c <-2
while (c<ncol(visible))
{
  r <-2
  while(r <nrow (visible)) {
    if(forest[r,c] > unique(max(forest[1:r-1,c])) | forest[r,c] > unique(max(forest[(r+1):ncol(forest),c])) | forest[r,c] > unique(max(as.numeric(forest[r,1:c-1]))) | forest[r,c] > unique(max(as.numeric(forest[r,(c+1):nrow(forest)])))) {
      visible[r,c] <-1
      r <- r+1
    } 
    else {
      visible[r,c] <- 0
      r <- r+1
    }
  }
  c <- c+1
}
sum(visible)

#Part 2
#data <- read_clip_tbl(header = FALSE, sep = " ")
data <- read.table("Problem 8.csv", header=FALSE,fileEncoding="UTF-8-BOM",sep=",",colClasses = 'character')

data2 <- str_split_fixed(data$V1,'',99)
forest <- as.data.frame(data2)

visible <- as.data.frame(matrix(data = c(0), ncol = ncol(forest),nrow = nrow(forest)))

op <- function(v, x){ # v=vector, x=value
  w <- which(v==x) # 1)
  s <- seq(w[1],length.out=length(w)) # 2)
  return(w[which(w!=s)[1]]) # 3)
}

c <-2
while (c<ncol(visible))
{
  r <-2
  while(r <nrow (visible)) {
    leftVec <- forest[r,1:c-1]
    leftResGreat <- sapply(leftVec, function(x) forest[r,c]>= x)
    leftResEqual <- sapply(leftVec, function(x) forest[r,c]== x)
    leftResGreat <- rev(leftResGreat)
    leftResEqual <- rev(leftResEqual)
    stopIndex <- length(leftResEqual)
    if(sum(leftResEqual) > 1){
     # stopIndex <- min(op(leftResEqual,TRUE))-1
      stopIndex <- which(leftResEqual == TRUE)[2]-1
    }
    if (sum(leftResEqual) == 1){
      stopIndex <-which(leftResEqual==TRUE)
    }
    if (length(leftResGreat) - sum(leftResGreat)>0){
      stopIndex <- min(stopIndex, min(which(leftResGreat==FALSE)))
    }
    if (stopIndex >0){
      leftVal <- length(leftResGreat[1:stopIndex])
    } else {leftVal <- 0}
    
    rightVec <- forest[r,(c+1):nrow(forest)]
    rightResGreat <- sapply(rightVec, function(x) forest[r,c]>= x)
    rightResEqual <- sapply(rightVec, function(x) forest[r,c]== x)
    stopIndex <- length(rightResEqual)
    if(sum(rightResEqual) > 1){
      #stopIndex <- min(op(rightResEqual,TRUE))-1
      stopIndex <- which(rightResEqual == TRUE)[2]-1
    }
    if (sum(rightResEqual) == 1){
      stopIndex <- which(rightResEqual==TRUE)
    }
    if (length(rightResGreat) - sum(rightResGreat)>0){
      stopIndex <- min(stopIndex, min(which(rightResGreat==FALSE)))
    }
    if (stopIndex >0){
      rightVal <- length(rightResGreat[1:stopIndex])
    } else {rightVal <- 0}
    
    upVec <- forest[1:r-1,c]
    upResGreat <- sapply(upVec, function(x) forest[r,c]>= x)
    upResEqual <- sapply(upVec, function(x) forest[r,c]== x)
    upResGreat <- rev(upResGreat)
    upResEqual <- rev(upResEqual)
    stopIndex <- length(upResEqual)
    if(sum(upResEqual) > 1){
      #stopIndex <- min(op(upResEqual,TRUE))-1
      stopIndex <- which(upResEqual == TRUE)[2]-1
    }
    if (sum(upResEqual) == 1){
      stopIndex <-which(upResEqual==TRUE)
    }
    if (length(upResGreat) - sum(upResGreat)>0){
      stopIndex <- min(stopIndex, min(which(upResGreat==FALSE)))
    }
    if (stopIndex >0){
      upVal <- length(upResGreat[1:stopIndex])
    } else {upVal <- 0}
    
    downVec <- forest[(r+1):ncol(forest),c]
    downResGreat <- sapply(downVec, function(x) forest[r,c]>= x)
    downResEqual <- sapply(downVec, function(x) forest[r,c]== x)
    stopIndex <- length(downResEqual)
    if(sum(downResEqual) > 1){
      #stopIndex <- min(op(downResEqual,TRUE))-1
      stopIndex <- which(downResEqual == TRUE)[2]-1
    }
    if (sum(downResEqual) == 1){
      stopIndex <-which(downResEqual==TRUE)
    }
     if (length(downResGreat) - sum(downResGreat)>0){
      stopIndex <- min(stopIndex, min(which(downResGreat==FALSE)))
     }
    if (stopIndex >0){
      downVal <- length(downResGreat[1:stopIndex])
    } else {downVal <- 0}
    
    visible[r,c] <- leftVal * rightVal * upVal * downVal 
    r<- r+1
  }
  c <- c+1
  }


max(visible)
visible

#write.csv(visible, "test.csv", row.names=FALSE)
#write.csv(forest, "forest.csv", row.names=FALSE)


