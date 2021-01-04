library(MASS)
library(ggplot2)
library(descr)
library(dplyr)

# Cars93 데이터셋을 이용해 그래프 작성
## 자동차 타입
cal <- Cars93 %>% group_by(Type) %>% summarise(num = n())
cal

ggplot(cal, aes(x=Type, y=num)) + geom_col()

## 에어백
Cars93$AirBags

air_type <- Cars93 %>% group_by(Type, AirBags) %>% summarise(n=n())
ggplot(air_type, aes(x=Type, y=n, fill=AirBags)) + geom_col()

## 결측치 제거
air_n <- Cars93 %>% filter(AirBags!="None") %>% group_by(Type, AirBags) %>% summarise(n=n())
ggplot(air_n, aes(x=Type, y=n, fill=AirBags)) + geom_col(position = "dodge")


# 종교 유무에 따른 이혼율 분석
load("data/welfare.rda")

## 종교 변수 전처리
table(welfare$religion)

welfare$religion <- ifelse(welfare$religion == 1, "yes", "no")
table(welfare$religion)

qplot(welfare$religion)

## 결혼여부 변수 전처리
table(welfare$marriage)
welfare$group_marriage <- ifelse(welfare$marriage == 1, "marriage",
                                 ifelse(welfare$marriage == 3, "divorce", NA))
table(welfare$group_marriage)
table(is.na(welfare$group_marriage))
qplot(welfare$group_marriage)

#### count 함수 활용
religion_marriage <- welfare %>% 
  filter(!is.na(group_marriage)) %>%
  count(religion, group_marriage) %>% 
  group_by(religion)

#### 일반적인 방법
religion_marriage <- welfare %>% 
  filter(!is.na(group_marriage)) %>% 
  group_by(religion, group_marriage) %>% 
  summarise(n=n())

religion_marriage

## 데이터 결합
divorce <- religion_marriage %>% filter(group_marriage == "divorce")
divorce

pct_no <- religion_marriage %>% filter(religion == "no") %>% summarise(n=sum(n))
pct_no

pct_yes <- religion_marriage %>% filter(religion == "yes") %>% summarise(n=sum(n))
pct_yes

divorce_tot_n <- data.frame(religion = c(pct_no$religion, pct_yes$religion), tot = c(pct_no$n, pct_yes$n))
divorce_tot_n

divorce <- left_join(divorce, divorce_tot_n, by="religion")
divorce

## 이혼율 구하기
divorce$pct <- (divorce$n / divorce$tot) * 100
divorce

ggplot(divorce, aes(x=religion, y=pct)) + geom_col()
