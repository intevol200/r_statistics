library(MASS)
library(ggplot2)
library(descr)
library(dplyr)
library(reshape2)
library(treemap)

View(Cars93)
ls(Cars93)

# car_Type_train 데이터셋을 이용해 그래프 작성
car_Type_train <- Cars93 %>% group_by(Type, DriveTrain) %>% summarise(n=n())
car_Type_train$DriveTrain <- factor(car_Type_train$DriveTrain, levels=c('Front','Rear', '4WD'))

ggplot(car_Type_train, aes(x=Type, y=n, fill=DriveTrain)) + geom_col()


# 전륜구동 자동차 개수
list_order <- car_Type_train %>% filter(DriveTrain=="Front") %>% arrange(n)
list_order


# 자동차 타입의 순서
order <- list_order$Type
order


# Cars93의 Type 살펴보기
table(Cars93$Type)


# 타입과 구동방식에 따른 자동차 개수
car_Type_train <- Cars93 %>% group_by(Type, DriveTrain) %>% summarise(n=n())
car_Type_train


# DriveTrain 순서 바꾸기
car_Type_train$DriveTrain <- factor(car_Type_train$DriveTrain, levels=c("Front","Rear","4WD"))
levels(car_Type_train$DriveTrain)


# car_Type_train 그래프 만들기(x축 순서 바꾸기 포함)
car_Type_train
ggplot(car_Type_train, aes(x=Type, y=n, fill=DriveTrain)) + geom_col() + 
  scale_x_discrete(limits=c("Van", "Large", "Sporty", "Compact", "Midsize", "Small"))


# 자동차 타입별 개수
car_type <- Cars93 %>% group_by(Type) %>% summarise(n=n())
car_type


# Compact 타입 자동차의 제조국가 출력
Cars93_com <- subset(Cars93, Type %in% c("Compact"), select=c(Type, Origin))
Cars93_com


# 변수명 출력(원본 순서대로)
names(Cars93)