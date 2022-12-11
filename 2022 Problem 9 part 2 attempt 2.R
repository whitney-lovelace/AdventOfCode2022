#Part 2
#data <- read_clip_tbl(header = FALSE, sep = " ")

tail <- as.data.frame(matrix(data = c(0), 1000,1000))

tr <- 500
tc <- 500
tail[tr,tc] <- 1

tr <- c(replicate(10,500))
tc <- c(replicate(10,500))

i<-1
while (i <= nrow(data)){
  j <- 1
  while (j <= data$V2[i]){
  if(data$V1[i] == "L"){
    tc[1] <- tc[1]-1
  }
  if(data$V1[i] == "R"){
    tc[1] <- tc[1]+1
  }
  if(data$V1[i] == "U"){
    tr[1] <- tr[1]-1
  }
  if(data$V1[i] == "D"){
    tr[1] <- tr[1]+1
  }
    tailNum <- 1
    while (tailNum < 10){
      if (tr[tailNum+1] %in% c(tr[tailNum]-1,tr[tailNum],tr[tailNum]+1) & tc[tailNum+1] %in% c(tc[tailNum]-1,tc[tailNum],tc[tailNum]+1))
      {
    
      } #end of check if in same spot
      else if (tr[tailNum+1] == tr[tailNum]) #if on same row
      {
        if (tc[tailNum+1] > tc[tailNum]){ #if buddy is to the left move left
          tc[tailNum+1] <- tc[tailNum+1] - 1
        }
        else { # move right
          tc[tailNum+1] <- tc[tailNum+1] + 1
        }
      } # end check if on same row
      else if (tr[tailNum+1] > tr[tailNum]){ #check if above you
        if (tc[tailNum+1] == tc[tailNum]){ #if in same column move up
          tr[tailNum+1] <- tr[tailNum+1]-1
        }
        else if (tc[tailNum+1] > tc[tailNum]){ # move diagonal up left
          tr[tailNum+1] <- tr[tailNum+1]-1
          tc[tailNum+1] <- tc[tailNum+1]-1
        }
        else { # move diagonal up right
          tr[tailNum+1] <- tr[tailNum+1]-1
          tc[tailNum+1] <- tc[tailNum+1]+1
        }
      }# end check if above you
      else { #check if below you
        if (tc[tailNum+1] == tc[tailNum]){ #if in same column move down
          tr[tailNum+1] <- tr[tailNum+1]+1
        }
        else if (tc[tailNum+1] > tc[tailNum]){ # move diagonal down left
          tr[tailNum+1] <- tr[tailNum+1]+1
          tc[tailNum+1] <- tc[tailNum+1]-1
        }
        else { # move diagonal down right
          tr[tailNum+1] <- tr[tailNum+1]+1
          tc[tailNum+1] <- tc[tailNum+1]+1
        }
      }# end check if below you
      tailNum <- tailNum +1
    } #end tailNum loop
    tail[tr[10],tc[10]] <- 1
    j<- j+1
  } #end of j loop
  i <- i+1
} #end of i loop
  
sum(tail)
  
 