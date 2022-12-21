#day 15

install.packages("clipr")
library("clipr")

data <- read_clip_tbl(header = FALSE, sep = " ")

data2 <- data
data2 <- data[,c(3,4,9,10)]

res = sapply(data2, function (x) as.numeric(gsub(".*?([0-9\\-]+).*","\\1",x)))

min(res[,c(1,3)]) # -631655
min(res[,c(2,4)]) #-772833
max(res[,c(1,3)]) # 3994575
max(res[,c(2,4)]) # 3989039

df <- data.frame(matrix(0,ncol = 4761873, nrow = 4626231))

i<-1
while (i <= nrow(res)){
  dist <- abs(res[i,1] - res[i,3]) + abs(res[i,2] - res[i,4])
  j<-0
  while(j<=dist){
    colIndex1 <- res[i,1]+772833
    colIndex2 <- max(colIndex1 - j,1)
    colIndex3 <- min(colIndex1 + j,ncol(df))
    rowIndex1 <- res[i,2]+631656
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

sum(df[2631656,]==1)
