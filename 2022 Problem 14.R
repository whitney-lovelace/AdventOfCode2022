#install.packages("stringr")
#library("stringr")
#install.packages("dplyr")
#library("dplyr")

#setwd("C:/Users/whitn/OneDrive/Documents/Code/AdventOfCode2022")
#data <- read_clip_tbl(header = FALSE)
#data <- read.table("Day14Sample.csv", header=FALSE,sep=",")

df <- data.frame(matrix(".",ncol=150, nrow = 300))

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
      df[c(start[2]:end[2]),start[1]-450] <- "#"
    }
    else {
      df[start[2],c((start[1]-450):(end[1]-450))] <- "#"
    }
    j <- j+1
    start <- end
  }# end j loop
  i <- i+1
}# end i loop


#sand
df2 <- df

col <- 500-450
row <- 0
while(row < 300){
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
    row <- 0
    col <- 500-450
    #print(df2)
  }
}

totals <- length(df2[df2 == 0]) 
