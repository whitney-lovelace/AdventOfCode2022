#install.packages("stringr")
#library("stringr")
#install.packages("dplyr")
#library("dplyr")

#setwd("C:/Users/whitn/OneDrive/Documents/Code/AdventOfCode2022")
#data <- read_clip_tbl(header = FALSE)
#data <- read.table("Day14Sample.csv", header=FALSE,sep=",")

#get max row
# findRow <- data.frame(matrix(ncol = 2))
# colnames(findRow) <- c("index","count")
# i <- 1
# while(i<= nrow(df2)){
#   current <- df2[i,]
#   findRow[nrow(findRow)+1,] <- c(i,length(current[current == "#"]))
#   i <- i+1
# }
# findRow <- findRow[findRow$count != 0,]
# max(findRow$index)
#171, so solid line at 173

df <- data.frame(matrix(".",ncol=600, nrow = 300))

#make rocks
i<-1
while (i <= nrow(data)){
  temp <- data[i,]
  temp <- str_split_fixed(temp," -> ",(Inf))
  j <- 2
  start <- str_split_fixed(temp[1],",",(Inf))
  start <- as.numeric(start)
  while (j <= length(temp)){
    end <- str_split_fixed(temp[j],",",(Inf))
    end <- as.numeric(end)
    if (start[1] == end[1])
    {
      df[c((start[2]+1):(end[2]+1)),start[1]-300] <- "#"
    }
    else {
      df[(start[2]+1),c((start[1]-300):(end[1]-300))] <- "#"
    }
    j <- j+1
    start <- end
  }# end j loop
  i <- i+1
}# end i loop

df[174,] <- "#"

#sand
df2 <- df

col <- 500-300
row <- 1
while(df2[1,500-300] != 0){
  if (df2[row+1,col] == "."){
    row <- row + 1
  }
  else if (df2[row+1,col-1] == "."){
    col <- col-1
  }
  else if (df2[row+1,col+1] == "."){
    col <- col +1
  }
  else {
    df2[row,col] <- 0
    row <- 1
    col <- 500-300
    print(length(df2[df2 == 0]) )
  }
}

totals <- length(df2[df2 == 0]) 
