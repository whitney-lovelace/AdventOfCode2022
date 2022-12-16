#install.packages("clipr")
#library("clipr")
#install.packages("jsonlite")
#library("jsonlite")
#data <- read_clip_tbl(header = FALSE, sep = " ")
#data <- read.table("Day10.csv", header=FALSE,fileEncoding="UTF-8-BOM",sep=",",colClasses = 'character')

df <- data.frame(matrix(ncol=2, nrow = 0))
colnames(df) <- c("pair","match")

i <- 1
pair <- 1
while(i<=nrow(data)){
  a <- fromJSON(data[i,],simplifyVector = FALSE)
  b <- fromJSON(data[i+1,],simplifyVector = FALSE)
  aa <- a
  bb <- b
  j <- 1
  while(j <= min(length(aa),length(bb))){
    if(typeof(aa[[j]]) == "list" & typeof(bb[[j]]) == "list"){
      if(setequal(aa[[j]],bb[[j]]) & length(aa[[j]]) == length(bb[[j]])){
        j <- j+1
      }
      else {
        aa <- aa[[j]]
        bb <- bb[[j]]
        j <- 1
      }
    }# end check if both lists
    else if (typeof(aa[[j]]) == "integer" & typeof(bb[[j]]) == "integer"){
      if (aa[[j]] < bb[[j]]){
        df[nrow(df)+1,] <-  c(pair,1)
        j <- length(aa) +1
        #i <- i+2
        #pair <- pair +1
      }
      else if(aa[[j]] > bb[[j]]){
        df[nrow(df)+1,] <-  c(pair,0)
        j <- length(aa) +1
        #i <- i+2
        #pair <- pair +1
      }
      else {
        j <- j+1
      }
    } #end integer check
    else if (typeof(aa[[j]]) != typeof(bb[[j]])){
      if (typeof(aa[[j]])=="integer"){
        aa[[j]] <- list(aa[[j]])
      }
      else {
        bb[[j]] <- list(bb[[j]])
      }
    }# check mismatch
  }# end j loop
  if(nrow(df[df$pair==pair,]) == 0){
    print(paste("i=",i," pair=",pair))
    if (typeof(aa) == "list" & typeof(bb) == "list"){
       if (length(aa) < length(bb)){
        df[nrow(df)+1,] <-  c(pair,1)
        #i <- i+2
        #pair <- pair +1
      }
      else {
        df[nrow(df)+1,] <-  c(pair,0)
        # i <- i+2
        #pair <- pair +1
      }
    } # end lists
    else if (length(aa) < length(bb)){
    df[nrow(df)+1,] <-  c(pair,1)
    #i <- i+2
    #pair <- pair +1
    }
    else {
      df[nrow(df)+1,] <-  c(pair,0)
     # i <- i+2
      #pair <- pair +1
    }
  }
  i <- i+2
  pair <- pair +1
} #end i loop
    
sum(df$pair[df$match == 1])    
   
   