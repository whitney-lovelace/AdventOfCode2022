## Day 10
#install.packages("pracma")
#library("pracma")
# part 1
divisors <- c(11, 5, 7,  2, 17,13, 3, 19)
M0 <- c(54,82,90,88,86,54)
M0m <- matrix(nrow = length(M0), ncol = 8)
i <- 1
while(i <= length(M0)){
  M0m[i,] <- M0[i] %% divisors
  i <- i +1
}
M0m <- as.data.frame(M0m)
M1 <- c(91,65)
M1m <- matrix(nrow = length(M1), ncol = 8)
i <- 1
while(i <= length(M1)){
  M1m[i,] <- M1[i] %% divisors
  i <- i +1
}
M1m <- as.data.frame(M1m)
M2 <- c(62,54,57,92,83,63,63)
M2m <- matrix(nrow = length(M2), ncol = 8)
i <- 1
while(i <= length(M2)){
  M2m[i,] <- M2[i] %% divisors
  i <- i +1
}
M2m <- as.data.frame(M2m)
M3 <- c(67,72,68)
M3m <- matrix(nrow = length(M3), ncol = 8)
i <- 1
while(i <= length(M3)){
  M3m[i,] <- M3[i] %% divisors
  i <- i +1
}
M3m <- as.data.frame(M3m)
M4 <- c(68,89,90,86,84,57,72,84)
M4m <- matrix(nrow = length(M4), ncol = 8)
i <- 1
while(i <= length(M4)){
  M4m[i,] <- M4[i] %% divisors
  i <- i +1
}
M4m <- as.data.frame(M4m)
M5 <- c(79,83,64,58)
M5m <- matrix(nrow = length(M5), ncol = 8)
i <- 1
while(i <= length(M5)){
  M5m[i,] <- M5[i] %% divisors
  i <- i +1
}
M5m <- as.data.frame(M5m)
M6 <- c(96,72,89,70,88)
M6m <- matrix(nrow = length(M6), ncol = 8)
i <- 1
while(i <= length(M6)){
  M6m[i,] <- M6[i] %% divisors
  i <- i +1
}
M6m <- as.data.frame(M6m)
M7 <- c(79)
M7m <- matrix(nrow = length(M7), ncol = 8)
i <- 1
while(i <= length(M7)){
  M7m[i,] <- M7[i] %% divisors
  i <- i +1
}
M7m <- as.data.frame(M7m)


df <- matrix(ncol = 8, nrow = 20)

i <- 1 
while (i <= 20){
  M0i <- 1
  df[i,1] <- length(M0m[,1])
  
  while (M0i <= length(M0m[,1])){
    M0m[M0i,] <- M0m[M0i,] * 7
    M0m[M0i,]<- M0m[M0i,]%% divisors
    #M0[M0i,] <- floor(M0[M0i,]/3)
    if (M0m[M0i,1] == 0){
      M2m <- rbind(M2m,M0m[M0i,])
      M0m <- M0m[-M0i,]
    }
    else {
      M6m <- rbind(M6m,M0m[M0i,])
      M0m <- M0m[-M0i,]
    }
  } # end M0i while loop
  M1i <- 1
  df[i,2] <- nrow(M1m)
  while (M1i <= nrow(M1m)){
    M1m[M1i,] <- M1m[M1i,] *13
    M1m[M1i,]<- M1m[M1i,]%% divisors
    
  #  M1[M1i,] <- floor(M1[M1i,]/3)
    
    if (M1m[M1i,2] == 0){
      M7m <- rbind(M7m,M1m[M1i,])
      
    }
    else {
      M4m <- rbind(M4m,M1m[M1i,])
    }
    M1m <- M1m[-M1i,]
  } # end M1i while loop
  M2i <- 1
  df[i,3] <- nrow(M2m)
  while (M2i <= nrow(M2m)){
    M2m[M2i,] <- M2m[M2i,] +1
    M2m[M2i,]<- M2m[M2i,]%% divisors
    
   # M2[M2i,] <- floor(M2[M2i,]/3)
   
    if (M2m[M2i,3] == 0){
      M1m <- rbind(M1m,M2m[M2i,])
      
    }
    else {
      M7m <- rbind(M7m,M2m[M2i,])
    }
    M2m <- M2m[-M2i,]
  } # end M2i while loop
  M3i <- 1
  df[i,4] <- nrow(M3m)
  while (M3i <= nrow(M3m)){
    M3m[M3i,] <- M3m[M3i,] * M3m[M3i,]
    M3m[M3i,]<- M3m[M3i,]%% divisors
    
 #  M3[M3i,] <- floor(M3[M3i,]/3)
    
    if (M3m[M3i,4] == 0){
      M0m <- rbind(M0m,M3m[M3i,])
      
    }
    else {
      M6m <- rbind(M6m,M3m[M3i,])
    }
    M3m <- M3m[-M3i,]
  } # end M3i while loop
  M4i <- 1
  df[i,5] <- nrow(M4m)
  while (M4i <= nrow(M4m)){
    M4m[M4i,] <- M4m[M4i,] +7
    M4m[M4i,]<- M4m[M4i,]%% divisors
    
  # M4[M4i,] <- floor(M4[M4i,]/3)
   
    if (M4m[M4i,5] == 0){
      M3m <- rbind(M3m,M4m[M4i,])
      
    }
    else {
      M5m <- rbind(M5m,M4m[M4i,])
    }
    M4m <- M4m[-M4i,]
  } # end M4i while loop
  M5i <- 1
  df[i,6] <- nrow(M5m)
  while (M5i <= nrow(M5m)){
    M5m[M5i,] <- M5m[M5i,] +6
    M5m[M5i,]<- M5m[M5i,]%% divisors
    
   # M5[M5i] <- floor(M5[M5i]/3)
    
    if (M5m[M5i,6] == 0){
      M3m <- rbind(M3m,M5m[M5i,])
      
    }
    else {
      M0m <- rbind(M0m,M5m[M5i,])
    }
    M5m <- M5m[-M5i,]
  } # end M5i while loop
  M6i <- 1
  df[i,7] <- nrow(M6m)
  while (M6i <= nrow(M6m)){
    M6m[M6i,] <- M6m[M6i,] +4
    M6m[M6i,]<- M6m[M6i,]%% divisors
    
  #  M6[M6i] <- floor(M6[M6i]/3)
    
    if (M6m[M6i,7] == 0){
      M1m <- rbind(M1m,M6m[M6i,])
      
    }
    else {
      M2m <- rbind(M2m,M6m[M6i,])
    }
    M6m <- M6m[-M6i,]
  } # end M6i while loop
  M7i <- 1
  df[i,8] <- nrow(M7m)
  while (M7i <= nrow(M7m)){
    M7m[M7i,] <- M7m[M7i,] +8
    M7m[M7i,]<- M7m[M7i,]%% divisors
    
   # M7[M7i] <- floor(M7[M7i]/3)
  
    if (M7m[M7i,8] == 0){
      M4m <- rbind(M4m,M7m[M7i,])
      
    }
    else {
      M5m <- rbind(M5m,M7m[M7i,])
    }
    M7m <- M7m[-M7i,]
  } # end M7i while loop
  i<- i+1
}#end round loop

M0Total <- sum(df[,1])
M1Total <- sum(df[,2])
M2Total <- sum(df[,3])
M3Total <- sum(df[,4])
M4Total <- sum(df[,5])
M5Total <- sum(df[,6])
M6Total <- sum(df[,7])
M7Total <- sum(df[,8])

M0Total
M1Total
M2Total
M3Total
M4Total 
M5Total 
M6Total 
M7Total 





#write.csv(df,"IncorrectVersion.csv")
