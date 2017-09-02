library(tidyr)
library(varhandle)
library(dplyr)

setwd("C:/Users/Administrator/Documents/Movie_R_2/170820_EDM_PCA_Pearson")
plus <- read.csv("wide_plus.csv", header = TRUE)
View(plus)

nrow(plus)

plus_no_cd <-subset(plus, select = -c(movieCd))
plus_no_cd

#attach하게 되면 data.frame 지정 없이 열에 접근 가능 ex)movieCd
#attach(minus_no_cd)
names(plus_no_cd)

sum <- colSums(plus_no_cd)
sum
plus_ <-rbind(plus_no_cd, sum)
plus_

#1행, 마지막 열 추출
nrow_ <- nrow(plus_)
nrow_

plus_ <- rbind(names(plus_), plus_[nrow_, ])
#View(plus_)
#write.csv(plus_, "kjk.csv")


#행/열 전환
plus_t <- t(plus_)
plus_t
class(plus_t)

plus_t <- data.frame(plus_t)
plus_t
nrow(plus_t)

colnames(plus_t)[1] <- "Dday"
colnames(plus_t)[2] <- "Sum"

str(plus_t)

plus_t_u <- unfactor(plus_t)

str(plus_t_u)
plus_t_u$Dday <- gsub("X", "", plus_t_u$Dday)
str(plus_t_u)
plus_t_u$Dday <- as.numeric(plus_t_u$Dday)
str(plus_t_u)
plus_t_u
#class(minus_t)
#class(minus_t_s_tt) <-"numeric"


#1행 데이터 (atomic vector)로 요인분석 하는 것 포기..
#e_Value <- eigen(cor(minus_t_u))
#e_Value


#plotting
plot(plus_t_u)
#barplot(minus_t_u)


#0 아닌 숫자는 1로 바꾸기
plus_1 <- subset(plus, select = -c(movieCd))
plus_1[plus_1!= 0] <- 1
#minus_1 <- cbind(minus$movieCd, minus_1)
#colnames(minus_1)[1] <- "MovieCd"

#minus_1 <- data.frame(minus_1)
#minus_1
#View(minus_1)
#class(minus_1)
#attach(minus_1)
str(plus_1)

#minus_1_ncd <-subset(minus_1, select = -c(MovieCd))
#minus_1_ncd

#attach(minus_1_ncd)

sum_1 <- colSums(plus_1)#주의:rawSum이 아님!
sum_1

plus_1_s <- rbind(plus_1, sum_1)
#minus_1_s <- subset(minus_1, select = -c(MovieCd))
#minus_1_s <- cbind(minus$movieCd, minus_1_s)
plus_1_s

#1행, 마지막 열 추출
nrow_1 <- nrow(plus_1_s)
nrow_1
plus_1_s[nrow_1,]

plus_1_ss <- rbind(names(plus_1_s), plus_1_s[nrow_1, ])
plus_1_ss

str(plus_1_ss)
plus_1_ss <- data.frame(plus_1_ss)

plus_1_sst <- t(plus_1_ss)
plus_1_sst
str(plus_1_sst)

#minus_1_sst <- asunfactor(minus_1_sst)
#minus_1_sst

#str(minus_1_sst)

colnames(plus_1_sst)[1] <- "Dday"
colnames(plus_1_sst)[2] <- "Sum"
plus_1_sst <- data.frame(plus_1_sst)
plus_1_sstu<- unfactor(plus_1_sst)
str(plus_1_sstu)


#minus_1_sst$Sum <- as.numeric(minus_1_sst$Sum) #이유 알 수 없지만 오류 생김
#transform(minus_1_sst, Sum = as.numeric(Sum)) #마찬가지

plus_1_sstu$Dday <- gsub("X", "", plus_1_sstu$Dday)
plus_1_sstu$Dday <- as.numeric(plus_1_sstu$Dday)
plus_1_sstu


str(plus_1_sstu)

plot(plus_1_sstu)

#table(minus_1_sst$Sum)
#minus_1_sst
#plot(minus_1_sst)


s_1 <- plus_1_sstu[plus_1_sstu$Sum < 2, ]
s_1

#.578,507,278,82,317, 147
#class(minus_1)
#as.data.frame(minus_1)
#View(minus_1)
#View(minus_1_sst)
#small <- subset(minus_1, select = c(MovieCd, X.578, X.507, X.278 ))
#View(small)

#smalls <- small[small$X.578 > 0 | small$X.507 > 0 | small$X.278 > 0,]
#smalls

#t <- subset(minus_1, select = c(MovieCd, X.1))
#t <- t[t$X.1 > 0,]
#t
#minus_1$X.1

#colnames(smalls)[1] <- "movieCd"

#find index of label
grep("X265", colnames(plus_1))
ncol <- ncol(plus_1)
ncol
smalls_265 <- plus[,265:ncol]
smalls_265 <- cbind(plus$movieCd, smalls_265)
#smalls_500_n <- smalls_500[smalls_500$X.1854 >0,]
#smalls_500_n
#smalls_2 <- smalls_500[smalls_500$X.1854>0 |smalls_500$X.1845>0| smalls_500$X.1768>0 | smalls_500$X.1491 >0
#                        | smalls_500$X.1482>0 | smalls_500$X.1376>0 | smalls_500$X.1364>0|smalls_500$X.1283>0
#                        | smalls_500$X.1237 >0| smalls_500$X.1222>0 | smalls_500$X.1217>0 |smalls_500$X.1050>0
#                        |smalls_500$X.1044>0|smalls_500$X.649>0 , ]

#smalls_2
grep("X.1044", colnames(minus_1))
smalls_1000 <- minus[ ,1:13]
smalls_1000

n_s <- ncol(smalls_265)
smalls_a <- smalls_265[apply(smalls_265[ ,2:n_s], 1, sum) >0, ]

smalls_a

class(smalls_a)
smalls_a <- data.frame(smalls_a)
class(smalls_a)
View(smalls_a)
colnames(smalls_a)[1] <- "movieCd"

n_a <- nrow(smalls_a)
n_a
#View(smalls_a)

#import movieName with Code
NameCode = read.csv("C:/Users/Administrator/Documents/Movie_R_2/movieKeys.csv", header = TRUE)
NameCode
#str(NameCode)

smalls_n <- merge(NameCode, smalls_a, by="movieCd")
smalls_n

write.csv(smalls_n, "plus265.csv", row.names=FALSE)

smalls_a_1000 <- smalls_1000[apply(smalls_1000[ ,2:13], 1, sum) >0, ]
smalls_a_1000 <- data.frame(smalls_a_1000)
colnames(smalls_a_1000)[1] <- "movieCd"

smalls_1000_n <- merge(NameCode, smalls_a_1000, by="movieCd")
smalls_1000_n
write.csv(smalls_1000_n, "minus1000.csv", row.names=FALSE)
