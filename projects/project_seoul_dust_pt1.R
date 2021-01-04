library(readxl)
library(dplyr)

dustdata <- read_excel("data/seoul_dust/dustdata.xlsx")
str(dustdata)

dustdata_analysis <- dustdata %>% filter(area %in% c("성북구", "중구"))
View(dustdata_analysis)

# count 함수를 이용한 결측치 확인
count(dustdata_analysis, yyyymmdd) %>% arrange(desc(n))
count(dustdata_analysis, area) %>% arrange(desc(n))

# describe 함수
install.packages("psych")
library(psych)

head(mtcars)
describe(mtcars$mpg)

# 구별로 분리
dustdata_analysis_area_sb <- subset(dustdata_analysis, area == '성북구')
dustdata_analysis_area_jg <- subset(dustdata_analysis, area == '중구')
describe(dustdata_analysis_area_sb$finedust)
describe(dustdata_analysis_area_jg$finedust)

# boxplot으로 만들기
boxplot(dustdata_analysis_area_sb$finedust,
        dustdata_analysis_area_jg$finedust,
        main = "finedust_compare", xlab = "AREA",
        names = c("성북구", "중구"),
        ylab = "FINEDUST_PM", col = c("blue", "green"))


# line 그래프
library(ggplot2)
View(Orange)

Orange %>% filter(Tree==1) %>% ggplot(aes(age, circumference)) + geom_line()
Orange %>% filter(Tree==1) %>% ggplot(aes(age, circumference)) + geom_line(color = "red")

# color 속성 - 변수 구분
ggplot(Orange, aes(age, circumference)) + geom_line()

ggplot(Orange, aes(age, circumference, color = Tree)) + geom_line()
ggplot(Orange, aes(age, circumference)) + geom_line(aes(color = Tree))

# linetype(선 유형)
ggplot(Orange, aes(age, circumference, color = Tree)) + geom_line(linetype = 6) +
  theme(panel.background = element_blank())

## 데이터 별로 선 구분
### 흑백
ggplot(Orange, aes(age, circumference)) + geom_line(aes(linetype = Tree)) +
  theme(panel.background = element_blank())

### 컬러
ggplot(Orange, aes(age, circumference, color = Tree)) + geom_line(aes(linetype = Tree)) +
  theme(panel.background = element_blank())

## 그룹별 구분
ggplot(Orange, aes(age, circumference, color = Tree)) + geom_line()
ggplot(Orange, aes(age, circumference, color = Tree, group = Tree)) + geom_line()


# 실습문제
## 서울시 최근 10년간 월별 미세먼지 평균농도 그래프 만들기
library(reshape2)

dust10 <- read_excel("data/seoul_dust/seoul_y10_finedust.xlsx")
View(dust10)

v_dust <- melt(dust10, id.vars = "year")
v_dust <- rename(v_dust, mon = variable, f_dust = value)
View(v_dust)

v_dust$year <- factor(v_dust$year, levels = c(2010:2019))

ggplot(v_dust, aes(mon, f_dust, color = year, group = year)) + geom_line()
