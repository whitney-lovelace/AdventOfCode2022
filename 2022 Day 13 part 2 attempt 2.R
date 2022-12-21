#install.packages("clipr")
#library("clipr")
#install.packages("jsonlite")
#library("jsonlite")
#data <- read_clip_tbl(header = FALSE, sep = " ")
#data2 <- read_clip_tbl(header = FALSE, sep = " ")
#data <- read.table("Day10.csv", header=FALSE,fileEncoding="UTF-8-BOM",sep=",",colClasses = 'character')

  

##### Part 2
data3 <- rbind(data, data2)

df <- data.frame(matrix(ncol=1, nrow = 0))
colnames(df) <- c("name")
x <- list()

i <- 1
#data3 <- data3[order(data3$V1),]

while (i <= nrow(data3)){
  a <-  fromJSON(data3[i,],simplifyVector = FALSE)
  aa <- a
  j <-1
  while (j == 1){
  if (setequal(aa,x)){
    df[nrow(df)+1,] <- 0
    i <- i+1
    j <- 2
  }
  else if (typeof(aa[[1]]) == "list"){
    aa <- aa[[1]]
  }
  else {
    df[nrow(df)+1,] <- aa[[1]]
    i <- i+1
    j <-2
  }
  }
}



sorted_li <- df[order(df$name),]
which(sorted_li == "6") * which(sorted_li == "2")
