## Day 7

#Part 1
install.packages("data.tree")
library("data.tree")

#Replaced '$ ' with '' in Excel
#Replaced '/' with 'start' in Excel
data <- read.table("Problem7Test.csv", header=FALSE,fileEncoding="UTF-8-BOM",sep=",",colClasses = 'character')

test <- separate(data,col = 1,sep =" ", into = c("a","b"))
test <- test[test$a !="ls",]

i <- 1
parent <- c()
child <- c()
directories <- c()

while (i <= nrow(test)){
  if (test$a[i] == "cd" & test$b[i] != ".."){
    directories <- append(directories,test$b[i])
  }
 else if (test$a[i] == "cd" & test$b[i] == ".."){
    directories <- directories[-length(directories)]
  } 
  else if (test$a[i] == "dir") {
    parent <- append(parent,paste(directories, collapse = ' '))
    child <- append(child,paste(c(directories,test$b[i]), collapse = ' '))
  }
  else {
    parent <- append(parent,paste(directories, collapse = ' '))
    child <- append(child,test$a[i])
  }
  i <- i+1
}

treeData <- data.frame(parent,child)
treeData

nums <- treeData[!(treeData$child %in% unique(treeData$parent)),]
chain <- treeData[treeData$child %in% unique(treeData$parent),]

test3 <- aggregate(as.numeric(nums$child),list(nums$parent), FUN = sum)
colnames(test3) <- c("dirs","sums")

dirs <- unique(treeData$parent[!(treeData$parent %in% test3$dirs)])
sums <- 0
empties <- data.frame(dirs, sums)



answers <- rbind(test3,empties)

i <- 1
while (i<=nrow(chain)){
 
  answers$sums[answers$dirs == chain$parent[i]] <- answers$sums[answers$dirs == chain$parent[i]] + answers$sums[answers$dirs == chain$child[i]]
  j <- 1
  while (j<i) {
    if (chain$child[j]==chain$parent[i]){
      answers$sums[answers$dirs == chain$parent[j]] <- answers$sums[answers$dirs == chain$parent[j]] + answers$sums[answers$dirs == chain$child[i]]
      j <- j+1
    }
    else (j <- j+1)
  }
  i <- i+1
  }
sum(answers$sums[answers$sums <= 100000])

answers[answers$dirs == 'start',]


#Part 2
k <- 1
while (k <- nrow(answers)){
  temp <- filter(answers, str_detect(answers$dirs, paste(c("^",answers$dirs[k]," *"),collapse = '')))
answers$new[k] <- sum(temp$sums)
  k <- k +1
}


sum(b$sums)
