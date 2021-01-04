# melt 함수
library(reshape2)

df <- data.frame(이름=c("민철", "지수", '지영'),
                   국어=c(100,70,50), 영어=c(80,70,100), 컴퓨터=c(85,100,80))
df <- melt(df, id.vars = "이름", measure.vars=c("국어", "컴퓨터"))
df

## 실습문제
head(airquality)

melt_test2 <- melt(airquality, id=c("Month", "Wind"), measure.vars=c("Ozone"))
head(melt_test2)


# cast 함수
exam2 <- dcast(df, 이름 ~ variable)
exam2

## dcast 실습
names(airquality) <- tolower(names(airquality))
aq_melt <- melt(airquality, id=c("month","day"), na.rm = TRUE)
head(aq_melt)

aq_dcast <- dcast(aq_melt, month + day ~ variable)
head(aq_dcast)

## acast 실습
aa <- acast(aq_melt, day ~ month ~ variable)
aa

acast(aq_melt, month ~ variable, mean)
acast(aq_melt, month ~ variable, sum)


# treemap
install.packages("treemap")
library(treemap)
library(readxl)

sales_df <- read_excel("data/product_sale.xlsx")
str(sales_df)

treemap(sales_df, vSize = "saleAmt", index = "region")
treemap(sales_df, vSize = "saleAmt", index = c("product", "region"))

x <- c("Seoul", "New York", "London", "1234")
nchar(x)

# substr 함수
string_1 <- "Hello World"
substr(string_1, 7, 9)


# transform 함수
test1 <- data.frame(이름=c("서연", "서윤", "민준", "하윤", "현우"),
                      반=c("1반", "1반", "2반", "2반", "2반"),
                      score = c(55,95,70,85,50))

test1

test2 <- transform(test1, scoreplus = score + 10)
test2

## 실습문제
time_stamp <- c("201507251040", "201507251041", "201507251042", "201507251043", "201507251044")
gas_temp <- c(145.0, 145.1, 145.5, 150.1, 150.6)
ts_gas_temp <- data.frame(time_stamp, gas_temp)
ts_gas_temp

ts_gas_temp <- transform(ts_gas_temp, year=substr(time_stamp, 1, 4))
ts_gas_temp <- transform(ts_gas_temp, mmdd=substr(time_stamp, 5, 8))
ts_gas_temp <- transform(ts_gas_temp, hhmm=substr(time_stamp, 9, 12))
ts_gas_temp

# paste 함수
paste("I", "LOVE", "NEW YORK", sep = "")
paste("I", "LOVE", "NEW YORK", sep = " ")
paste("I", "LOVE", "NEW YORK", sep = "_")

# split 함수
name <- c("Chulu, Kim", "Younghei, Lee", "Dongho, Choi")
name_split <- strsplit(name, split = ",")
name_split

name_split[[1]][2]

## 실습문제
last_name <- c(name_split[[1]][2], name_split[[2]][2], name_split[[3]][2])
last_name

first_name <- c(name_split[[1]][1], name_split[[2]][1], name_split[[3]][1])
first_name

name_d.f <- data.frame(last_name, first_name, name)
name_d.f