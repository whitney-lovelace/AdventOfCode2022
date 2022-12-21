#day 15

install.packages("clipr")
library("clipr")

data <- read_clip_tbl(header = FALSE, sep = " ")

data2 <- data
data2 <- data[,c(3,4,9,10)]

res = sapply(data2, function (x) as.numeric(gsub(".*?([0-9\\-]+).*","\\1",x)))

min(res[,c(1,3)]) # -2
min(res[,c(2,4)]) #0
max(res[,c(1,3)]) # 25
max(res[,c(2,4)]) #22

manhattan_dist <- function(a, b){
  dist <- abs(a-b)
  dist <- sum(dist)
  return(dist)
}

df <- data.frame(matrix(0,ncol = 28, nrow = 23))

i<-1
while (i <= nrow(res)){
  dist <- abs(res[i,1] - res[i,3]) + abs(res[i,2] - res[i,4])
  j<-0
  while(j<=dist){
    colIndex1 <- res[i,1]+3
    colIndex2 <- max(colIndex1 - j,1)
    colIndex3 <- min(colIndex1 + j,ncol(df))
    rowIndex1 <- res[i,2]+1
    rowIndex2 <- max(rowIndex1 - dist +j,1)
    rowIndex3 <- min(rowIndex1 + dist -j,nrow(df))
    df[c(rowIndex2:rowIndex3),c(colIndex2:colIndex3)] <-1
    j <- j+1
  }
  i<- i+1
}
k<- 1
while (i <= nrow(res)){
  df[(res[i,2]+1),(res[i,1]+3)] <- "S"
  df[(res[i,4]+1),(res[i,3]+3)] <- "B"
  k <- k+1
}

sum(df[11,]==1)
