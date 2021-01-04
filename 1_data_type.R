ex_data <- read.table("data/data_ex.txt", header = TRUE)
View(ex_data)
ex_data1 <- read.table("data/data_ex.txt", header = TRUE, sep = ",")
View(ex_data1)

headname <- c("번호", "성별", "나이", "지역")
data1 <- read.table("data/data_ex2.txt", sep = ",", col.names = headname, skip = 1, nrows = 5)
View(data1)


# RDA 파일 열기
id <- c(1,2,3,4,5)
sex <- c("F","M","F","M","F")
data_ex <- data.frame(id = id, sex = sex)
save(data_ex, file = "data/data_ex.rda")
load("data/data_ex.rda")
View(data_ex)


# 파일 저장하기
write.csv(data_ex, file = "data/data1.csv")
write.table(data_ex, file = "data/data1.txt")


# 산술연산자
1+2
5-3
3*7
20/4
-1
-1+3
20/7
20%/%7
20%%7
2^3
2**3


# 논리연산자
5>3
5>=4
5>=5
5<6
5==4
5!=4
x <- 1:3
x <- 3
y <- 1
(x > 0) & (y <= 1)
(x > 0) | (y > 1)


# 데이터 파악 함수
library(readxl)
exdata1 <- read_excel("data/Sample1.xlsx")
exdata1
View(exdata1)
str(exdata1)
dim(exdata1)
ls(exdata1)

df <- data.frame(sex=c("M","F",NA,"M","F"), score=c(5,4,3,4,NA))
df
class(df$sex)
class(df$score)
summary(df)
is.na(df)
table(df$sex)
table(is.na(df))


# 변수명 변경하기
install.packages("dplyr")
library(dplyr)
library(readxl)
exdata1 <- read_excel("data/Sample1.xlsx")
exdata1 <- rename(exdata1, Y17_AMT = AMT17, Y16_AMT = AMT16)
View(exdata1)
