## Day 10
install.packages("clipr")
library("clipr")

# Part 1
#data <- read_clip_tbl(header = FALSE, sep = " ")
data <- read.table("Day10.csv", header=FALSE,fileEncoding="UTF-8-BOM",sep=",",colClasses = 'character')

commands <- separate(data,col = 1,sep =" ", into = c("a","b"))

addLinesNum <- nrow(commands[commands$a == "addx",])
noopLinesNum <-  nrow(commands[commands$a == "noop",])

cycle <- vector("numeric",addLinesNum*2+noopLinesNum)
value <- vector("numeric",addLinesNum*2+noopLinesNum)
df <- data.frame(cycle,value)

i<- 1
cycleNum <- 1
value <- 1
while(i <= nrow(commands)){
  if (commands$a[i] == "addx"){
    df$cycle[cycleNum] <- cycleNum
    df$value[cycleNum] <- value
    cycleNum <- cycleNum +1
    df$cycle[cycleNum] <- cycleNum
    df$value[cycleNum] <- value
    cycleNum <- cycleNum +1
    value <- value + as.numeric(commands$b[i])
  }
  else {
    df$cycle[cycleNum] <- cycleNum
    df$value[cycleNum] <- value
    cycleNum <- cycleNum +1
  }
  i<-i+1
}

#20th, 60th, 100th, 140th, 180th, and 220th 

df$signalStrength <- df$cycle * df$value

answer <- sum(df$signalStrength[20],df$signalStrength[60],df$signalStrength[100],df$signalStrength[140],df$signalStrength[180],df$signalStrength[220])

#Part 2
p1 <- df
p1$symbol <- "."
i <- 1
rows <- c(1:40)
p1$rows <- rows
while (i <= nrow(p1)){
  if (p1$rows[i] %in% c(p1$value[i],p1$value[i]+1,p1$value[i]+2)){
    p1$symbol[i] <- "#"
  }
  i<-i+1
}

test<- matrix(p1$symbol,nrow = 6, byrow = TRUE)
View(test)
