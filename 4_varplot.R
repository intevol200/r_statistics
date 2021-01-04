# 가로결합 full-join
library(readxl)
library(dplyr)

m_history <- read_excel("data/Sample4_y17_history.xlsx")
f_history <- read_excel("data/Sample5_y16_history.xlsx")
bind_col_full <- full_join(m_history, f_history, by="ID")
View(bind_col_full)

# 실습문제1
test7 <- data.frame(ID=c(1,2,3,5),
                    거주지=c("서울","부산","광주","제주"))
test8 <- data.frame(ID=c(1,2,4,5),
                    성별=c("남","여","남","남"))
test_col_full <- full_join(test7, test8, by="ID")
test_col_full

# 실습문제2
test11 <- data.frame(ID=c(1,2,3,5,7),
                    거주지=c(NA,"부산","광주","제주","제주"))
test12 <- data.frame(ID=c(1,2,4,6,7),
                    성별=c("남","여","남","남","여"))
test_join <- inner_join(test11 %>% filter(!is.na(거주지)), test12, by="ID")
test_join


# 기초 통계 분석함수
x1 <- c(1,2,3,4,5,6,100)
x2 <- c(1,2,3,4,5,100)

mean(x1)
median(x1)
median(x2)
max(x1)
min(x1)
range(x1)

quantile(x2)
quantile(x2, 0.25)
quantile(x2, 0.5)
quantile(x2, 0.75)
quantile(x2, 0.8)

mean(x2)
var(x2)

x3 <- c(5, 5.5, 4, 6, 5.7, 6)
mean(x3)
var(x3)

sd(x2)
sd(x3)

install.packages("psych")
library(psych)
kurtosi(x1)
skew(x1)


# 빈도 분석
load("data/exdata1.rda")
exdata1

install.packages("descr")
library(descr)

freq_test <- freq(exdata1$AREA, plot=F)
freq_test

freq_test2 <- freq(exdata1$AREA)
freq_test2

# 줄기 잎 그림
exdata1$AGE
stem(exdata1$AGE)

# 히스토그램
hist(exdata1$AGE)

# 실습문제
hist(exdata1$AGE, main = paste("AGE분포"), xlim = range(0, 60), ylim = range(0, 5))


# 막대그래프 varplot
table(exdata1$SEX)
dist_sex <- table(exdata1$SEX)
dist_sex
barplot(dist_sex)
barplot(dist_sex, ylim = c(0,8))
barplot(dist_sex, ylim = c(0,8), 
        main = "BARPLOT", 
        xlab = "SEX", 
        ylab = "FREQUENCY", 
        names = c("Female", "Male"))
barplot(dist_sex, 
        ylim = c(0,8), 
        main = "BARPLOT", 
        xlab = "SEX", 
        ylab = "FREQUENCY", 
        names = c("Female", "Male"), 
        col = c("pink", "navy"))


boxplot(exdata1$Y17_CNT, exdata1$Y16_CNT)
boxplot(exdata1$Y17_CNT, exdata1$Y16_CNT, 
        ylim = c(0,60), 
        main = "boxplot", 
        names = c("17년건수", "16년건수"))
boxplot(exdata1$Y17_CNT, exdata1$Y16_CNT, 
        ylim = c(0,60), 
        main = "boxplot", 
        names = c("17년건수", "16년건수"),
        col = c("green","yellow"))

y1 <- c(1,2,3,4,5,6,7,8,9,10,20,25)
boxplot(y1)
