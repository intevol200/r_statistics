# 산점도
group <- c(1,1,1,2,2,2,2,2,1,1,2,1)
age <- c(12,15,29,22,40,33,31,38,12,30,25,19)
weight <- c(30,45,58,50,61,65,50,51,28,62,50,40)

dat <- data.frame(group, age, weight)
plot(x=age, y=weight, col=c("blue", "red")[group])

# cars93을 이용한 산점도
library(ggplot2)
library(MASS)

names(Cars93)

ggplot(data=Cars93, aes(x=EngineSize, y=MPG.highway)) + geom_point()

# 점 스타일과 색상, 크기 바꾸기
ggplot(data=Cars93, aes(x=EngineSize, y=MPG.highway)) + 
  geom_point(shape=25, size=1, color="blue")

ggplot(data=Cars93, aes(x=Weight, y=MPG.highway, color=Type)) + 
  geom_point(shape=19, size=3) + 
  ggtitle("Scatter Plot by Type, using different Colors")


# facet_grid 함수 (그룹별 산포도)
## 가로 분할
ggplot(data=Cars93, aes(x=Weight, y=MPG.highway)) + 
  geom_point(shape=21, color="black") + 
  facet_grid(Type ~ .)

## 세로 분할
ggplot(data=Cars93, aes(x=Weight, y=MPG.highway)) + 
  geom_point(shape=21, color="black") + 
  facet_grid(. ~ Type)

## 가로 + 세로 분할
ggplot(data=Cars93, aes(x=Weight, y=MPG.highway)) + 
  geom_point(shape=21, color="black") + 
  facet_grid(Origin ~ Type)


# 원 그래프
library(dplyr)

cnt <- Cars93 %>% group_by(Type, Origin) %>% summarise(n=n())

## 막대그래프
ggplot(cnt, aes(x="", y=n, fill=Type)) + 
  facet_grid(facets = .~Origin) +
  geom_bar(stat = "identity", width = 10)

## 원형 좌표계를 가진 차트
ggplot(cnt, aes(x="", y=n, fill=Type)) + 
  facet_grid(facets = .~Origin) +
  geom_bar(stat = "identity", width = 10) + coord_polar()

## 파이 차트
ggplot(cnt, aes(x="", y=n, fill=Type)) + 
  facet_grid(facets = .~Origin) +
  geom_bar(stat = "identity", width = 10) + coord_polar(theta = "y")


# 실습 프로젝트 - 레스토랑 매출 분석
library(readxl)

customer_r <- read_excel("data/r_practice/customer_r.xlsx")
reservation_r <- read_excel("data/r_practice/reservation_r.xlsx")
order_info_r <- read_excel("data/r_practice/order_info_r.xlsx")
item_r <- read_excel("data/r_practice/item_r.xlsx")

# 변수명 소문자 변환
colnames(customer_r) <- tolower(colnames(customer_r))
colnames(reservation_r) <- tolower(colnames(reservation_r))
colnames(order_info_r) <- tolower(colnames(order_info_r))
colnames(item_r) <- tolower(colnames(item_r))

# 데이터셋 결합하기(reservation_r + order_info_r)
df_cfm_order <- inner_join(reservation_r, order_info_r, by="reserv_no") %>% 
  select(customer_id, reserv_no, visitor_cnt, cancel, item_id, sales) %>% 
  arrange(customer_id)

# 고객별 방문횟수, 총 매출액 요약
df_sct_graph <- df_cfm_order %>% group_by(customer_id) %>% summarise(vst_cnt=sum(visitor_cnt), cust_amt=sum(sales/1000))

# 1차 산점도 그래프
ggplot(df_sct_graph, aes(x=vst_cnt, y=cust_amt)) + 
  geom_point() + 
  xlim(c(0, 75)) + 
  ylim(c(0, 900))

# 고객 성별 자료 추가하기
df_sct_graph2 <- inner_join(customer_r, df_sct_graph, "customer_id") %>% 
  arrange(customer_id) %>% 
  select(vst_cnt, cust_amt, sex_code)

# 성별 자료 결측치 제거
df_sct_graph2 <- df_sct_graph2 %>%
  filter(!is.na(df_sct_graph2$sex_code))

# 2차 산점도 그래프
ggplot(df_sct_graph2, aes(x=vst_cnt, y=cust_amt, color=sex_code)) +
  geom_point() + 
  xlim(c(0, 55)) + 
  ylim(c(0, 600))

