## Day 12
#21,146
# Part 1
#install.packages("clipr")
#library("clipr")
#install.packages("stringr")
#library("stringr")
#data <- read.table("Day12Input.csv", header=FALSE,fileEncoding="UTF-8-BOM",sep=",",colClasses = 'character')
#data <- data[-1,-1]


values <- as.data.frame(cbind(letters,c(1:26)))
colnames(values) <- c("letters","nums")

paths <- data

i <-1
while(i<= nrow(paths)){
  j<-1
  while(j<=ncol(paths)){
    if (data[i,j] == "S"){
      paths[i,j] <- 1
      j <- j+1
    }
    if (data[i,j] == "E"){
      paths[i,j] <- 26
      j <- j+1
    }
    else {
    paths[i,j] <- values$nums[which(values$letters == data[i,j])]
    j <- j+1
    }
  }
  i <- i+1
}

df <- data.frame(matrix(ncol=5, nrow = 0))
colnames(df) <- c("x","y","path","distance","visited")

i <- 1
while(i <= nrow(paths)){
  j<- 1
  while (j<= ncol(paths)){
   df[nrow(df)+1,]<-  c(i,j,paths[i,j],10000,"n")
   j <- j+1
  }
  i <- i+1
}

df$distance[df$path == 1] <- 0
head(df)
df$path <- as.numeric(df$path)
df$distance <- as.numeric(df$distance)
df$x <- as.numeric(df$x)
df$y <- as.numeric(df$y)

while (df$distance[df$x == 21 & df$y == 146] == 10000){
  mins <- df[df$visited == "n",]
  mins <- mins[mins$distance == min(mins$distance),]
  df$visited[df$x == mins$x[1] & df$y == mins$y[1]] <- "y"
  
    #up
  if(mins$x[1] > 1){
  up <- df[df$x == mins$x[1]-1 & df$y == mins$y[1],]
      if (up$path <= mins$path[1]+1 & (up$distance > mins$distance[1])){
        df$distance[df$x == up$x & df$y == up$y] <- mins$distance[1]+1
      }
  }
    #down
  if(mins$x[1] < nrow(data)){
  down <- df[df$x == mins$x[1]+1 & df$y == mins$y[1],]
  if (down$path <= mins$path[1]+1 & (down$distance > mins$distance[1])){
    df$distance[df$x == down$x & df$y == down$y] <- mins$distance[1]+1
  }
  }
    #left
  if(mins$y[1] > 1){
  left <- df[df$x == mins$x[1] & df$y == mins$y[1]-1,]
  if (left$path <= mins$path[1]+1 & (left$distance > mins$distance[1])){
    df$distance[df$x == left$x & df$y == left$y] <- mins$distance[1]+1
  }
  }
    #right
  if(mins$y[1] < ncol(data)){
  right <- df[df$x == mins$x[1] & df$y == mins$y[1]+1,]
  if ( right$path <= mins$path[1]+1 & (right$distance > mins$distance[1])){
    df$distance[df$x == right$x & df$y == right$y] <- mins$distance[1]+1
  }
  }
}# end outer while
  
#df$distance[df$x == 21 & df$y == 146]
  
  