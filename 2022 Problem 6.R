## Day 6

# Part 1
install.packages("data.table")
library("data.table")
install.packages("dplyr")
library("dplyr")
install.packages("stringr")
library("stringr")

data <- read_clip_tbl(header = FALSE, sep = " ")

splitLetters <- str_split_fixed(toString(data),"",(Inf))

df <- t(as.data.frame(splitLetters))

shift1 <- shift(df,1L)
shift2 <- shift(df,2L)
shift3 <- shift(df,3L)
shift4 <- shift(df,4L)
shift5 <- shift(df,5L)
shift6 <- shift(df,6L)
shift7 <- shift(df,7L)
shift8 <- shift(df,8L)
shift9 <- shift(df,9L)
shift10 <- shift(df,10L)
shift11 <- shift(df,11L)
shift12<- shift(df,12L)
shift13 <- shift(df,13L)


combined <- data.frame(df,shift1,shift2,shift3,shift4,shift5,shift6,shift7,shift8,shift9,shift10,shift11,shift12,shift13)
combined <- combined[14:nrow(combined),]


i <- 1
check <- c()
while (i <= nrow(combined)) {
  check[i] <- length(unique(c(combined$df[i],combined$shift1[i],combined$shift2[i],combined$shift3[i],combined$shift4[i],combined$shift5[i],combined$shift6[i],combined$shift7[i],combined$shift8[i],combined$shift9[i],combined$shift10[i],combined$shift11[i],combined$shift12[i],combined$shift13[i])))
  i <- i+1
}
check
nums <- c(1:4082)

answer <- data.frame(check,nums)
answer$answer <- answer$nums + 13
answer <- answer[answer$check == 14,]
head(answer)
