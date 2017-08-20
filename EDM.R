x <- read.csv("c:/workspace_git/Movie_R/201707.txt", header = T, fileEncoding = "UTF-8-BOM", sep = "\t", fill = TRUE, quote="" )
names(x)
x[1:4,]

library(dplyr)
library(plyr)
library(ggplot2)
library(reshape2)

summary(x)

#Extract the following columns
#date, openDt, movieCd, movieNm, audiCnt, scrCnt (상영 스크린 수), showCnt (상영 횟수)
x_2 <- select(x , date, openDt, movieCd, movieNm, audiCnt, scrnCnt, showCnt)
summary(x_2)

#Change data type from string to date
as.Date(x_2$date, format ="%Y%m%d")

#Why is there 2 n/as in Date_f?
#sort(x_2$date) 

#change strange values
x_2$date <- gsub("\\?", "", x_2$date)
sort(x_2$date)
summary(x_2)

x_2 <-mutate(x_2, Date_f = as.Date(x_2$date, format = "%Y%m%d"))
summary(x_2)

#delete Date_f which contains null values
#x_2 <- subset(x_2, select = -c(Date_f))
#x_2 <-subset(x_2, select = -c(openDt_f))
#summary(x_2)

#데이터 오류 발견, (변환되지 않는 openDt 확인, 정리해줌)
#엑셀에서 openDt만 정렬, 1964-00-00이 문제
# strange <- subset(x_2, openDt == '1964-00-00')
# strange
x_2$openDt <-gsub("1964-00-00", "1964-01-01", x_2$openDt)
# 
# strange <- subset(x_2, openDt == '1964-00-00')
# strange

x_2 <-mutate(x_2, openDt_f = as.Date(x_2$openDt, format ="%Y-%m-%d"))
summary(x_2)

#delete original columns
x_2 <-subset(x_2, select = -c(date, openDt))

#make Dday column (개봉한 지 얼마나 지났는 지)
x_2 <- mutate(x_2, D_day = x_2$Date_f - x_2$openDt_f)
summary(x_2)

#change difftime class to numeric
x_2$D_day <- as.numeric(x_2$D_day)
summary(x_2)

#개봉값이 음수인 것 (개봉 전)
minus <- subset(x_2, D_day < 0)
minus$D_day <- minus$D_day * (-1)
minus$D_day

hist(minus$D_day)

minus_1000 <-subset(x_2, D_day < -1000)
minus_1000

minus_500 <-subset(minus, D_day > 500)
minus_500

#개봉일이 관측치보다 1년 이상 뒤인 것은 무엇일까요....?
#일단 개봉일 이전/이후를 각각 100일,200일로 삼기로 함.
x_audiCnt <- subset(x_2, select = c(movieCd, D_day, audiCnt))
x_audiCnt <- subset(x_audiCnt, D_day > -100 & D_day <200)
summary(x_audiCnt)

wide_x_a <- dcast(x_audiCnt, movieCd ~ D_day, sum)
wide_x_a

write.csv(wide_x_a, file ="wide_x_a_100_200.csv", row.names = FALSE)

x_scr <- subset(x_2, select = c(movieCd, D_day, scrnCnt))
x_scr <- subset(x_scr, D_day > -100 & D_day <200)
summary(x_scr)

wide_x_s <-dcast(x_scr, movieCd ~ D_day, sum)
wide_x_s

write.csv(wide_x_s, file = "wide_x_s_100_200.csv", row.names = FALSE)

x_show <- subset(x_2, select = c(movieCd, D_day, showCnt))
x_show <- subset(x_show, D_day > -100 & D_day < 200)

wide_x_sh <-dcast(x_show, movieCd ~ D_day, sum)
wide_x_sh

write.csv(wide_x_sh, file = "wide_x_sh_100_200.csv", row.names = FALSE)

#make key vector(movie code, name) to add name column
x_key <- subset(x_2, select = c(movieNm, movieCd))
x_key <- unique(x_key) #중복값 제거

#set working directory
setwd("C:\Users\Administrator\Documents\Movie_R")
write.csv(x_key, file = "movieKeys.csv", row.names = FALSE)

z<- join(x_key, wide_x_a, by="movieCd")
summary(z)
wide_x_a <-z

z_s <- join(x_key, wide_x_s, by = "movieCd")
wide_x_s <- z_s

z_sh <- join(x_key, wide_x_sh, by = "movieCd")
wide_x_sh <- z_sh

#데이터 전체를 wide로 만들기

x_all_au <- subset(x_2, select = c(movieCd, D_day, audiCnt))
wide_x_all_au <- dcast(x_all_au, movieCd ~ D_day, sum)

#add all values in a row except the first column (MovieCode)
rowSums(wide_x_all_au[,-1])
wide_x_all_au <- cbind(wide_x_all_au, rowSums(wide_x_all_au[,-1]))
colnames(wide_x_all_au)[ncol(wide_x_all_au)] <- "Sum_au"
sum_all <-subset(wide_x_all_au, select = c(movieCd, Sum_au))

x_all_s <- subset(x_2, select = c(movieCd, D_day, scrnCnt))
wide_x_all_s <- dcast(x_all_s, movieCd ~ D_day, sum)

rowSums(wide_x_all_s[,-1])
sum_all <- cbind(sum_all, rowSums(wide_x_all_s[,-1]))
colnames(sum_all)[ncol(sum_all)] <- "Sum_s"

x_all_sh <-subset(x_2, select = c(movieCd, D_day, showCnt))
wide_x_all_sh <- dcast(x_all_sh, movieCd ~ D_day, sum)

sum_all <- cbind(sum_all, rowSums(wide_x_all_sh[,-1]))
colnames(sum_all)[ncol(sum_all)] <- "Sum_sh"

sum_all
sum_all_add <- join(x_key, sum_all, by ="movieCd")
sum_all_add
write.csv(sum_all_add, file= "sum_all.csv", row.names = FALSE)

#빈도 수 확인
head(melt(x_2))
ggplot(data = melt(x_2), mapping = aes(x = value)) + geom_histogram(bins = 30) + facet_wrap(~variable, scales = 'free_x')

#상영횟수와 관객 수의 관련성
attach(x)
plot(audiCnt~showCnt)

