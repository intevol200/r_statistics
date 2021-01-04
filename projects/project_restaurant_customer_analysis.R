library(readxl)
library(dplyr)
library(ggplot2)

# 프로젝트 실습 1
## RFM 분석 : 레스토랑의 고객현황은 어떨까?

customer_r <- read_excel("data/r_practice/customer_r.xlsx")
reservation_r <- read_excel("data/r_practice/reservation_r.xlsx")
order_info_r <- read_excel("data/r_practice/order_info_r.xlsx")
item_r <- read_excel("data/r_practice/item_r.xlsx")

colnames(customer_r) <- tolower(colnames(customer_r))
colnames(reservation_r) <- tolower(colnames(reservation_r))
colnames(order_info_r) <- tolower(colnames(order_info_r))
colnames(item_r) <- tolower(colnames(item_r))

# 데이터셋 결합하기
df_f_join_1 <- inner_join(reservation_r, order_info_r, by="reserv_no")

# 전체 고객별로 방문회수(F)와 매출(M) 정리
df_rfm_data <- df_f_join_1 %>% group_by(customer_id) %>% 
  summarise(visit_sum=n_distinct(reserv_no), sales_sum=sum(sales)/1000) %>% 
  arrange(customer_id)

df_rfm_data

# 요약 통계값 확인
summary(df_rfm_data)

# 상자그림 그리기
ggplot(df_rfm_data, aes(x="", y=visit_sum)) +
  geom_boxplot(width = 0.8, outlier.size = 2, outlier.color = "red") +
  labs(title = "방문 횟수 상자그림", x="빈도", y="방문횟수")

ggplot(df_rfm_data, aes(x="", y=sales_sum)) +
  geom_boxplot(width = 0.8, outlier.size = 2, outlier.color = "red") +
  labs(title = "매출 상자그림", x="매출", y="금액")

# 방문 횟수 60%, 90%에 해당하는 분위수 찾기
quantile(df_rfm_data$visit_sum, probs = c(0.6, 0.9))

# 매출 60%, 90%에 해당하는 분위수 찾기
quantile(df_rfm_data$sales_sum, probs = c(0.6, 0.9))

# 우수 고객이 차지하는 비율 계산
total_sum_data <- df_rfm_data %>% 
  summarise(t_visit_sum=sum(visit_sum), t_sales_sum=sum(sales_sum))
total_sum_data

loyalty_sum_data <- df_rfm_data %>% 
  summarise(l_visit_sum = sum(ifelse(visit_sum > 2, visit_sum, 0)),
            l_sales_sum = sum(ifelse(sales_sum > 135, sales_sum, 0)))

loyalty_sum_data / total_sum_data*100


# 상관분석
str(airquality)
airquality_1 <- airquality[, c(1:4)]
airquality_2 <- na.omit(airquality_1)

str(airquality_2)
cor(airquality_2) ## 상관관계 생성
plot(airquality_2)

cor(airquality_2$Ozone, airquality_2$Temp)
plot(airquality_2$Ozone, airquality_2$Temp)


# 프로젝트 실습 2
## 상관분석 : 스테이크와 와인의 주문은 관계가 있을까?
df_f_join_1 <- inner_join(reservation_r, order_info_r, by="reserv_no")
df_f_join_2 <- inner_join(df_f_join_1, item_r, by = "item_id")

# 스테이크, 와인 아이템 정보 확인
item_r
target_item <- c("M0005", "M0009")

# 스테이크와 와인을 주문한 데이터셋 생성
df_stime_order <- df_f_join_2 %>% 
  filter(item_id %in% target_item) %>% 
  group_by(reserv_no) %>% 
  mutate(order_cnt = n()) %>% 
  select(branch, reserv_no, order_cnt)

View(df_stime_order)

# 스테이크와 와인을 동시에 주문한 예약번호 찾기
df_stime_order <- df_stime_order %>% 
  distinct(branch, reserv_no, order_cnt) %>% 
  filter(order_cnt == 2) %>% 
  arrange(branch)

df_stime_order

# 동시주문 건의 각 아이템별 매출금액 계산
stime_order_rsv_no <- df_stime_order$reserv_no

df_stime_sales <- df_f_join_2 %>% 
  filter((reserv_no %in% stime_order_rsv_no) &
           (item_id %in% target_item)) %>% 
  group_by(reserv_no, product_name) %>% 
  summarise(sales_amt = sum(sales)/1000) %>% 
  arrange(product_name, reserv_no)

View(df_stime_sales)

# 스테이크, 와인별로 매출금액 데이터셋 생성
steak <- df_stime_sales %>% filter(product_name == "STEAK")
wine <- df_stime_sales %>% filter(product_name == "WINE")
steak
wine

# 그래프 및 상관계수
plot(steak$sales_amt, wine$sales_amt)
cor(steak$sales_amt, wine$sales_amt)
