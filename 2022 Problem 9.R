## Day 9

# Part 1
data <- read_clip_tbl(header = FALSE, sep = " ")


tail <- as.data.frame(matrix(data = c(0), 1000,1000))

hr <- 500
hc <- 500

tr <- 500
tc <- 500
tail[tr,tc] <- 1



i<-1
while (i <= nrow(data)){
  #Left
  if(data$V1[i] == "L"){
    j<-1
    while (j <= data$V2[i]){
      hc <- hc-1
      if (tr %in% c(hr-1,hr,hr+1) & tc %in% c(hc-1,hc,hc+1))
      {
        tail[tr,tc] <- 1
      } #end of check if in same spot
      else if (tr == hr) # left
      {
        tc <- tc-1
        tail[tr,tc] <- 1
      }
      else if (tr > hr) # diagonal up left
      {
        tr <- tr - 1
        tc <- tc - 1
        tail[tr,tc] <- 1
      }
      else #diagonal down left
      {
        tr <- tr + 1
        tc <- tc - 1
        tail[tr,tc] <- 1
      }
     # print(j)
      j <- j+1
     # print(tail)
      #print(i)
      
    } # end left while loop
    #i<- i+1
  } # end Left
  #Right
  if(data$V1[i] == "R"){
    j<-1
    while (j <=  data$V2[i]){
      hc <- hc+1
      if (tr %in% c(hr-1,hr,hr+1) & tc %in% c(hc-1,hc,hc+1))
      {
        tail[tr,tc] <- 1
      } #end of check if in same spot
      else if (tr == hr) # right
      {
        tc <- tc +1
        tail[tr,tc] <- 1
      }
      else if (tr > hr) # diagonal up right
      {
        tr <- tr - 1
        tc <- tc + 1
        tail[tr,tc] <- 1
      }
      else #diagonal down right
      {
        tr <- tr + 1
        tc <- tc + 1
        tail[tr,tc] <- 1
      }
    #  print(j)
      j<-j+1
     # print(tail)
     # print(i)
      
    } # end right while loop
    #i<- i+1
  } # end right
  #Up
  if(data$V1[i] == "U"){
    j<-1
    while (j <=  data$V2[i]){
      hr <- hr-1
      if (tr %in% c(hr-1,hr,hr+1) & tc %in% c(hc-1,hc,hc+1))
      {
        tail[tr,tc] <- 1
      } #end of check if in same spot
      else if (tc == hc) # up
      {
        tr <- tr -1
        tail[tr,tc] <- 1
      }
      else if (tc < hc) # diagonal up right
      {
        tr <- tr - 1
        tc <- tc + 1
        tail[tr,tc] <- 1
      }
      else if (tc > hc) # diagonal up left
      {
        tr <- tr - 1
        tc <- tc - 1
        tail[tr,tc] <- 1
      }
     # print(j)
       j<-j+1
      # print(tail)
      #print(i)
      
    } # end up while loop
   # i<- i+1
  } # end up
  
  #Down
   if(data$V1[i] == "D"){
    j<-1
    while (j <=  data$V2[i]){
      hr <- hr+1
      if (tr %in% c(hr-1,hr,hr+1) & tc %in% c(hc-1,hc,hc+1))
      {
        tail[tr,tc] <- 1
      } #end of check if in same spot
      else if (tc == hc) # down
      {
        tr <- tr +1
        tail[tr,tc] <- 1
      }
      else if  (tc < hc) #diagonal down right
      {
        tr <- tr + 1
        tc <- tc + 1
        tail[tr,tc] <- 1
      }
      else #diagonal down left
      {
        tr <- tr + 1
        tc <- tc - 1
        tail[tr,tc] <- 1
      }
      j<-j+1
     # print(j)
     # print(tail)
     # print(i)
      
    } # end down while loop
   # i<- i+1
  } # end down
  i <- i+1
  
}

sum(tail)
#tail


#Part 2
data <- read_clip_tbl(header = FALSE, sep = " ")


tail <- as.data.frame(matrix(data = c(0), 50,50))

hr <- 25
hc <- 25

tr <- 25
tc <- 25
tail[tr,tc] <- 1

tr <- c(replicate(10,25))
tc <- c(replicate(10,25))

i<-1
while (i <= nrow(data)){
    
  #Left
  if(data$V1[i] == "L"){
    j<-1
    while (j <= data$V2[i]){
      tailNum <-1
      tc[1] <- tc[1]-1
      while (tailNum < 10){
      if (tr[tailNum+1] %in% c(tr[tailNum]-1,tr[tailNum],tr[tailNum]+1) & tc[tailNum+1] %in% c(tc[tailNum]-1,tc[tailNum],tc[tailNum]+1))
      {
        #tail[tr,tc] <- 1
      } #end of check if in same spot
      else if (tr[tailNum+1] == tr[tailNum+1]) # left
      {
        tc[tailNum+1] <- tc[tailNum+1]-1
        #tail[tr,tc] <- 1
      }
      else if (tr[tailNum] > tr[tailNum+1]) # diagonal up left
      {
        tr[tailNum+1] <- tr[tailNum+1] - 1
        tc[tailNum+1] <- tc[tailNum+1] - 1
        #tail[tr,tc] <- 1
      }
      else #diagonal down left
      {
        tr[tailNum+1] <- tr[tailNum+1] + 1
        tc[tailNum+1] <- tc[tailNum+1] - 1
        #tail[tr,tc] <- 1
      }
      tailNum <- tailNum +1
      } #end Tail Num Loop
      # print(j)
      j <- j+1
      tail[tr[10],tc[10]] <-1
      # print(tail)
      #print(i)
      
    } # end right while loop
    #i<- i+1
  } # end right
  
  #Up New
  if(data$V1[i] == "U"){
    j<-1
    while (j <= data$V2[i]){
      tailNum <-1
      tr[1] <- tr[1]-1
      while (tailNum < 10){
        if (tr[tailNum+1] %in% c(tr[tailNum]-1,tr[tailNum],tr[tailNum]+1) & tc[tailNum+1] %in% c(tc[tailNum]-1,tc[tailNum],tc[tailNum]+1))
        {
          #tail[tr,tc] <- 1
        } #end of check if in same spot
        else if (tr[tailNum+1] == tr[tailNum+1]) # up
        {
          tc[tailNum+1] <- tc[tailNum+1]-1
          #tail[tr,tc] <- 1
        }
        else if (tc[tailNum] > tc[tailNum+1]) # diagonal up right
        {
          tr[tailNum+1] <- tr[tailNum+1] - 1
          tc[tailNum+1] <- tc[tailNum+1] + 1
          #tail[tr,tc] <- 1
        }
        else #diagonal up left
        {
          tr[tailNum+1] <- tr[tailNum+1] - 1
          tc[tailNum+1] <- tc[tailNum+1] - 1
          #tail[tr,tc] <- 1
        }
        tailNum <- tailNum +1
      } #end Tail Num Loop
      # print(j)
      j <- j+1
      tail[tr[10],tc[10]] <-1
      # print(tail)
      #print(i)
      
    } # end up while loop
    #i<- i+1
  } # end up
  #Down New
  if(data$V1[i] == "D"){
    j<-1
    while (j <= data$V2[i]){
      tailNum <-1
      tr[1] <- tr[1]+1
      while (tailNum < 10){
        if (tr[tailNum+1] %in% c(tr[tailNum]-1,tr[tailNum],tr[tailNum]+1) & tc[tailNum+1] %in% c(tc[tailNum]-1,tc[tailNum],tc[tailNum]+1))
        {
          #tail[tr,tc] <- 1
        } #end of check if in same spot
        else if (tr[tailNum+1] == tr[tailNum+1]) # up
        {
          tc[tailNum+1] <- tc[tailNum+1]-1
          #tail[tr,tc] <- 1
        }
        else if (tc[tailNum] > tc[tailNum+1]) # diagonal down right
        {
          tr[tailNum+1] <- tr[tailNum+1] + 1
          tc[tailNum+1] <- tc[tailNum+1] + 1
          #tail[tr,tc] <- 1
        }
        else #diagonal down left
        {
          tr[tailNum+1] <- tr[tailNum+1] + 1
          tc[tailNum+1] <- tc[tailNum+1] - 1
          #tail[tr,tc] <- 1
        }
        tailNum <- tailNum +1
      } #end Tail Num Loop
      # print(j)
      j <- j+1
      tail[tr[10],tc[10]] <-1
      # print(tail)
      #print(i)
      
    } # end down while loop
    #i<- i+1
  } # end down
  i <- i+1
 
  }
  

sum(tail)
#tail