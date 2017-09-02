library(tidyr)
#library(psych)#For PCA
library(varhandle)
#library(nFactors)#For EigenValue
library(dplyr)
#a <- c(1,2,3)
#a
#class(a)
#a<-t(a)
#a
#class(a)
#a <- data.frame(a)
#a
#class(a)
#colnames (a) <- c("one", "two", "three")
#a
#str(a)#Compactly Display the structure of an object

setwd("C:/Users/Administrator/Documents/Movie_R_2/170820_EDM_PCA_Pearson")
minus <- read.csv("wide_minus.csv", header = TRUE)
View(minus)

nrow(minus)
minus

#movieCd <- subset(minus, select = c(movieCd))
#movieCd

minus_no_cd <-subset(minus, select = -c(movieCd))
minus_no_cd

#attach하게 되면 data.frame 지정 없이 열에 접근 가능 ex)movieCd
#attach(minus_no_cd)
names(minus_no_cd)

sum <- rowSums(minus_no_cd)
sum
minus_ <-rbind(minus_no_cd, sum)
minus_

#1행, 마지막 열 추출
nrow_ <- nrow(minus_)
nrow_

minus_ <- rbind(names(minus_), minus_[nrow_, ])
minus_

#행/열 전환
minus_t <- t(minus_)
minus_t
class(minus_t)

minus_t <- data.frame(minus_t)
minus_t
nrow(minus_t)

colnames(minus_t)[1] <- "Dday"
colnames(minus_t)[2] <- "Sum"

str(minus_t)

minus_t_u <- unfactor(minus_t)

str(minus_t_u)
minus_t_u$Dday <- gsub("X.", "", minus_t_u$Dday)
str(minus_t_u)
minus_t_u$Dday <- as.numeric(minus_t_u$Dday)
str(minus_t_u)
minus_t_u
#class(minus_t)
#class(minus_t_s_tt) <-"numeric"


#1행 데이터 (atomic vector)로 요인분석 하는 것 포기..
#e_Value <- eigen(cor(minus_t_u))
#e_Value


#plotting
plot(minus_t_u)
#barplot(minus_t_u)


#0 아닌 숫자는 1로 바꾸기
minus_1 <- subset(minus, select = -c(movieCd))
minus_1[minus_1!= 0] <- 1
#minus_1 <- cbind(minus$movieCd, minus_1)
#colnames(minus_1)[1] <- "MovieCd"

#minus_1 <- data.frame(minus_1)
#minus_1
#View(minus_1)
#class(minus_1)
#attach(minus_1)
str(minus_1)

#minus_1_ncd <-subset(minus_1, select = -c(MovieCd))
#minus_1_ncd

#attach(minus_1_ncd)

sum_1 <- colSums(minus_1)#주의:rawSum이 아님!
sum_1

minus_1_s <- rbind(minus_1, sum_1)
#minus_1_s <- subset(minus_1, select = -c(MovieCd))
#minus_1_s <- cbind(minus$movieCd, minus_1_s)
minus_1_s

#1행, 마지막 열 추출
nrow_1 <- nrow(minus_1_s)
nrow_1
minus_1_s[nrow_1,]

minus_1_ss <- rbind(names(minus_1_s), minus_1_s[nrow_1, ])
minus_1_ss

str(minus_1_ss)
minus_1_ss <- data.frame(minus_1_ss)

minus_1_sst <- t(minus_1_ss)
minus_1_sst
str(minus_1_sst)

#minus_1_sst <- asunfactor(minus_1_sst)
#minus_1_sst

#str(minus_1_sst)

colnames(minus_1_sst)[1] <- "Dday"
colnames(minus_1_sst)[2] <- "Sum"
minus_1_sst <- data.frame(minus_1_sst)
minus_1_sstu<- unfactor(minus_1_sst)
str(minus_1_sstu)


#minus_1_sst$Sum <- as.numeric(minus_1_sst$Sum) #이유 알 수 없지만 오류 생김
#transform(minus_1_sst, Sum = as.numeric(Sum)) #마찬가지

minus_1_sstu$Dday <- gsub("X.", "", minus_1_sstu$Dday)
minus_1_sstu$Dday <- as.numeric(minus_1_sstu$Dday)
minus_1_sstu

minus_t_u <- unfactor(minus_t)
str(minus_1_sstu)

plot(minus_1_sstu)

#table(minus_1_sst$Sum)
#minus_1_sst
#plot(minus_1_sst)


#s_1 <- minus_1_sst[minus_1_sst$Sum < 2, ]
#s_1

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
grep("X.500", colnames(minus_1))
minus_1

smalls_500 <- minus[,1:51]
smalls_500
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

smalls_a <- smalls_500[apply(smalls_500[ ,2:51], 1, sum) >0, ]

smalls_a

class(smalls_a)
smalls_a <- data.frame(smalls_a)
class(smalls_a)
#View(smalls_a)
colnames(smalls_a)[1] <- "movieCd"
#View(smalls_a)

#import movieName with Code
NameCode = read.csv("C:/Users/Administrator/Documents/Movie_R_2/movieKeys.csv", header = TRUE)
NameCode
#str(NameCode)

smalls_n <- merge(NameCode, smalls_a, by="movieCd")
smalls_n

write.csv(smalls_n, "minus500.csv", row.names=FALSE)

smalls_a_1000 <- smalls_1000[apply(smalls_1000[ ,2:13], 1, sum) >0, ]
smalls_a_1000 <- data.frame(smalls_a_1000)
colnames(smalls_a_1000)[1] <- "movieCd"

smalls_1000_n <- merge(NameCode, smalls_a_1000, by="movieCd")
smalls_1000_n
write.csv(smalls_1000_n, "minus1000.csv", row.names=FALSE)
