#day 15

#install.packages("clipr")
#library("clipr")

data <- read_clip_tbl(header = FALSE, sep = " ")

data2 <- data
data2 <- data[,c(3,4,9,10)]

res = sapply(data2, function (x) as.numeric(gsub(".*?([0-9\\-]+).*","\\1",x)))

max <- 4000000

rowVal <- 0
while(rowVal <= max){
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
  if(temp >= rowVal & (temp-rowVal) <= dist){
    df[j,4] <- as.numeric(df[j,2]) - abs(dist - (temp-rowVal))
    df[j,5] <- as.numeric(df[j,2]) + abs(dist - (temp-rowVal))
  }
  else if (temp < rowVal & (dist - (rowVal-temp)) >= 0){
    df[j,4] <- as.numeric(df[j,2]) - (dist - (rowVal-temp))
    df[j,5] <- as.numeric(df[j,2]) + (dist - (rowVal-temp))
  }
  else{
    df[j,4] <- "no"
    df[j,5] <- "no"
  }
  j <- j+1
}

poss <- df[df$start != "no",]

test <- c()
i <- 1
while(i <= nrow(poss)){
  test <- append(test,c(poss$start[i]:poss$end[i]))
  i <- i+1
}

test <- unique(test)
test <- test[test %in% c(0:max)]

if (length(test) != max+1){
  print(rowVal)
  break
}
else{rowVal <- rowVal+1}
}
b<-test
a <- c(0:max)
setdiff(a,b)
