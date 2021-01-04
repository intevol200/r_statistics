x <- c(1:6)
matrix(x, nrow = 2, ncol = 3)
matrix(x, nrow = 3, ncol = 2)
matrix(x, nrow = 2, ncol = 3, byrow = T)

x1 <- c(1:20)
matrix(x1, nrow = 4, ncol = 5, byrow = T)

x2 <- seq(1,99,by=2)
matrix(x2, nrow = 10, ncol = 5, byrow = T)

y = c(1,2,3,4,5,6)
array(y, dim = c(2,2,3))

y1 = c(1:40)
array(y1, dim = c(4,5,2))

list1 <-list(c(1,2,3), "hello")
list1
str(list1)

# 데이터프레임 만들기
id <- c(1,2,3,4,5,6,7,8,9,10)
sex <- c("F","M","F","M","M","F","F","F","M","F")
age <- c(50,40,28,50,27,23,56,47,20,38)
area <- c("서울","경기","제주","서울","서울","서울","경기","서울","인천","경기")
data.frame_ex <- data.frame(id, sex, age, area)
data.frame_ex
str(data.frame_ex)

ID <- c("1","2","3","4","5")
MID_EXAM <- c(10,25,100,75,30)
CLASS <- c("1반","2반","3반","4반","5반")
example_test <- data.frame(ID, MID_EXAM, CLASS)
example_test
str(example_test)

name <- c("홍길동","김이순","이순신","정민서")
eng <- c(80,90,85,90)
mat <- c(100,80,90,70)
class_test <- data.frame(name, eng, mat)
class_test


# 여러줄 데이터프레임
sales <- data.frame(fruit = c("사과","딸기","수박"), 
                    price = c(1800,1500,3000),
                    volume = c(24,38,13))
sales


# 엑셀파일 불러오기
install.packages("readxl")
library(readxl)
exel_data_ex <- read_excel("data/data_ex.xls")
View(exel_data_ex)

ex_data <- read.table("data/data_ex.txt")
ex_data

ex_data1 <- read.table("data/data_ex.txt", header = TRUE)
ex_data1

# 2행부터 불러오기
ex_data2 <- read.table("data/data_ex.txt", header = TRUE, skip = 2)
ex_data2

# 7행까지만 불러오기
ex_data3 <- read.table("data/data_ex.txt", header = TRUE, nrows = 7)
ex_data3

# ","로 탭 구분하기
exl_data <- read.table("data/data_ex1.txt", header = TRUE, sep = ",")
exl_data
