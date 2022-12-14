## Day 12
#21,146
# Part 1
#install.packages("clipr")
#library("clipr")
#install.packages("stringr")
#library("stringr")
#data <- read.table("Day12Sample.csv", header=FALSE,fileEncoding="UTF-8-BOM",sep=",",colClasses = 'character')
#data <- data[-1,-1]


values <- as.data.frame(cbind(letters,c(1:26)))
colnames(values) <- c("letters","nums")

paths <- data

i <-1
while(i<= nrow(paths)){
  j<-1
  while(j<=ncol(paths)){
    if (data[i,j] == "S"){
      paths[i,j] <- 0
      j <- j+1
    }
    if (data[i,j] == "E"){
      paths[i,j] <- 27
      j <- j+1
    }
    else {
    paths[i,j] <- values$nums[which(values$letters == data[i,j])]
    j <- j+1
    }
  }
  i <- i+1
}

distances <- matrix(NA, ncol = ncol(data), nrow = nrow(data))
visited <- matrix(NA, ncol = ncol(data), nrow = nrow(data))

i <- 1
while(i <= nrow(distances)){
  j<- 1
  while(j<=ncol(distances)){
    if (paths[j+1,i])
  }
}
