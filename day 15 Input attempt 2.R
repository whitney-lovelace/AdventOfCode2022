#day 15

install.packages("clipr")
library("clipr")

data <- read_clip_tbl(header = FALSE, sep = " ")

data2 <- data
data2 <- data[,c(3,4,9,10)]

res = sapply(data2, function (x) as.numeric(gsub(".*?([0-9\\-]+).*","\\1",x)))

minIndex <- min(res[,c(1,3)]) # -631655
maxIndex <- max(res[,c(1,3)]) # 3989039
# 
# df <- data.frame(matrix(0,ncol = 4761873, nrow = 4626231))

rowVal <- 2000000

df <- data.frame(matrix(ncol=5,nrow=0))
colnames(df)<- c("sensorR","sensorC","dist", "start","end")

i<-1
while (i <= nrow(res)){
  df[nrow(df)+1,] <- c(res[i,2],res[i,1],abs(res[i,1] - res[i,3]) + abs(res[i,2] - res[i,4]),"no","no")
  i<- i+1
}

j<-1
while(j<= nrow(df)){
  temp <- as.numeric(df[j,1])
  dist <- as.numeric(df[j,3])
  if(temp >= rowVal & (temp-dist) <= rowVal){
    df[j,4] <- max((as.numeric(df[j,2]) - abs(dist - (temp-rowVal+1))), minIndex)
    df[j,5] <- min((as.numeric(df[j,2]) + abs(dist - (temp-rowVal+1))),maxIndex)
  }
   else if (temp < rowVal & (dist - (rowVal-temp+1)) >= 0){
     df[j,4] <- max(as.numeric(df[j,2]) - (dist - (rowVal-temp)+1),minIndex)
     df[j,5] <- min(as.numeric(df[j,2]) + (dist - (rowVal-temp)+1),maxIndex)
   }
  else{
    df[j,4] <- "no"
    df[j,5] <- "no"
  }
  j <- j+1
}

poss <- df[df$start != "no",]

rowEval <-data.frame(cbind(c(minIndex:maxIndex),"no"))
colnames(rowEval) <- c("num","mark")
k <- 1
while(k<=nrow(poss)){
  l <- as.numeric(poss$start[k])
  while(l <= as.numeric(poss$end[k])){
    rowEval$mark[as.numeric(rowEval$num) == l] <- "yes"
    l <- l+1
    l
  }
  k<-k+1
}

beacons <- unique(res[,c(3,4)])

sum(rowEval$mark == "yes") - sum(beacons[,2]==10)
