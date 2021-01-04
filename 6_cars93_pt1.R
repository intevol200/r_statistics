library(MASS)
library(ggplot2)
library(descr)
library(dplyr)

Cars93$AirBags
View(Cars93)

# Cars93 데이터 셋의 AirBags 변수의 데이터빈도를 출력하시오
table(Cars93$AirBags)

# Cars93 데이터셋을 이용하여 자동차 타입 데이터셋 생성
car_type <- Cars93 %>% group_by(Type) %>% summarise(n = n())
car_type

# car_type 데이터셋을 이용해 막대그래프 작성
ggplot(car_type, aes(x=Type, y=n)) + geom_col()
