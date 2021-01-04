library(MASS)
library(dplyr)
library(ggplot2)
View(Cars93)

car_type_train <- Cars93 %>% group_by(Type, DriveTrain) %>% summarise(n=n())
car_type_train
ggplot(car_type_train, aes(x=Type, y=n, fill=DriveTrain)) + geom_col()

list_order <- car_type_train %>% filter(DriveTrain=="Front") %>% arrange(n)
list_order
order <- list_order$Type
order

car_type_train$DriveTrain <- factor(car_type_train$DriveTrain, levels = c("Front", "Rear", "4WD"))
ggplot(car_type_train, aes(x=Type, y=n, fill=DriveTrain)) + geom_col() +
  coord_flip() + scale_x_discrete(limits=order)


# subset 함수
## 조건이 하나일 때
head(diamonds)
subset(diamonds, cut=="Premium")

## 조건이 두 개 이상일 때
subset(diamonds, cut==c("Premium", "Fair"))
subset(diamonds, cut %in% c("Premium", "Fair", "Good"))

# 실습문제
Cars93_sample <- subset(Cars93, Type %in% c('Compact','Van'), 
                        select = c(Type, Origin, MPG.city, MPG.highway))
View(Cars93_sample)
dim(Cars93_sample)

library(reshape2)
Cars93_sample_melt <- melt(Cars93_sample, id.vars = c("Type", "Origin"),
                           measure.vars = c("MPG.city", "MPG.highway"))
View(Cars93_sample_melt)

# melt 함수(표시형식을 가로세로 변경)
df <- data.frame(이름=c("민철", "지수", '지영'),
                   국어=c(100,70,50), 영어=c(80,70,100), 컴퓨터=c(85,100,80))

exam1 <- melt(df)
exam1

exam1 <- melt(df, id.vars = "이름", measure.vars=c("국어"))
exam1

exam1 <- melt(df, id.vars = "이름", measure.vars=c("국어", "컴퓨터"))
exam1


# 지하철역 이용객수 전처리
library(readxl)
library(dplyr)
library(descr)
library(ggplot2)

subway <- read.csv("data/subway_data/CARD_SUBWAY_MONTH_201912.csv")
head(subway)
str(subway)

sw <- subset(subway, 노선명 %in% c("1호선", "2호선", "3호선", 
                                "4호선", "5호선", "6호선", 
                                "7호선", "8호선", "9호선"),
             select=c(노선명, 역명, 승차총승객수))
View(sw)

## 승차가 많은 노선 시각화
sw_no_tot <- sw %>% group_by(노선명) %>% summarise(tot=sum(승차총승객수))
sw_no_tot

ggplot(sw_no_tot, aes(x=노선명, y=tot, fill=노선명)) + geom_col()
ggplot(sw_no_tot, aes(x=reorder(노선명, -tot), y=tot, fill=노선명)) +
  geom_col() + scale_y_continuous(labels = scales::comma)

## 승하차 총합 파생변수
subway$tot = subway$승차총승객수 + subway$하차총승객수
head(subway)

## 상위 5개역 파생변수
st5 <- subway %>% group_by(역명) %>% summarise(n=sum(tot)) %>% 
  arrange(desc(n)) %>% head(5)
st5
st5_name <- st5$역명
st5_name
View(st5_name)

day_st5 <- subset(subway, 역명 %in% st5_name, select=c(사용일자, 역명, tot))
View(day_st5)

d_day_St5 <- day_st5 %>% group_by(역명, 사용일자) %>% summarise(n=sum(tot))
d_day_St5
d_day_St5$역명 <- factor(d_day_St5$역명, levels = st5_name)
ggplot(d_day_St5, aes(x=사용일자, y=n, fill=역명)) + geom_col() +
  scale_y_continuous(labels = scales::comma)
