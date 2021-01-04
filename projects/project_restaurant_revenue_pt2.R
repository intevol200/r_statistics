# (복습) 실습 프로젝트 - 레스토랑 매출 분석
library(readxl)
library(dplyr)
library(ggplot2)

customer_r <- read_excel("data/r_practice/customer_r.xlsx")
reservation_r <- read_excel("data/r_practice/reservation_r.xlsx")
order_info_r <- read_excel("data/r_practice/order_info_r.xlsx")
item_r <- read_excel("data/r_practice/item_r.xlsx")

# 조작 편의성을 위해 열이름을 소문자로 변환
colnames(customer_r) <- tolower(colnames(customer_r))
colnames(reservation_r) <- tolower(colnames(reservation_r))
colnames(order_info_r) <- tolower(colnames(order_info_r))
colnames(item_r) <- tolower(colnames(item_r))

head(customer_r)
head(reservation_r)
head(order_info_r)
head(item_r)

# 데이터셋 결합하기(reservation_r + order_info_r)
df_cfm_order <- inner_join(reservation_r, order_info_r, by="reserv_no") %>% 
  select(customer_id, reserv_no, visitor_cnt, cancel, item_id, sales) %>% 
  arrange(customer_id, reserv_no, item_id)

head(df_cfm_order)

# 고객별 방문횟수, 총 매출액 요약
df_sct_graph <- df_cfm_order %>% 
  group_by(customer_id) %>% 
  summarise(vst_cnt = sum(visitor_cnt), cust_amt = sum(sales/1000))

head(df_sct_graph)

# 1차 산점도 그래프
ggplot(df_sct_graph, aes(x=vst_cnt, y=cust_amt)) + 
  geom_point() + 
  xlim(c(0, 70)) + 
  ylim(c(0, 900))

# 고객 성별 자료 추가하기
df_sct_graph2 <- inner_join(customer_r, df_sct_graph, by = "customer_id") %>% 
  select(vst_cnt, cust_amt, sex_code)

head(df_sct_graph2)

table(is.na(df_sct_graph2$vst_cnt))
table(is.na(df_sct_graph2$cust_amt))
table(is.na(df_sct_graph2$sex_code))

# 성별 자료 결측치 제거
df_sct_graph2 <- df_sct_graph2 %>%
  filter(!is.na(df_sct_graph2$sex_code))

# 2차 산점도 그래프
ggplot(df_sct_graph2, aes(x=vst_cnt, y=cust_amt, color=sex_code)) +
  geom_point() + 
  xlim(c(0, 50)) + 
  ylim(c(0, 600))


#########################################
# 지점별 예약건수와 매출은 어떻게 될까? #
#########################################

# 지점별 예약건수 빈도표 만들기
## 순수 예약건수
table(reservation_r$branch)
temp <- table(reservation_r$branch) %>% data.frame()
arrange(temp, desc(Freq))

## 예약이 취소되지 않은 건수
no_cancel_data <- reservation_r %>% filter(cancel == "N")
temp <- table(no_cancel_data$branch) %>% data.frame()
arrange(temp, desc(Freq))

## 테이블 조인
df_f_join_1 <- inner_join(reservation_r, order_info_r, by = "reserv_no")
df_f_join_2 <- inner_join(df_f_join_1, item_r, by = "item_id")

head(df_f_join_2)

## 주요 지점의 메뉴별 매출액 데이터셋 생성
df_branch_sales <- subset(df_f_join_2, branch %in% c("강남", "마포", "강서")) %>% 
  group_by(branch, product_name) %>% 
  summarise(sales_amt = sum(sales)/1000)

View(df_branch_sales)

## 누적 막대 그래프
ggplot(df_branch_sales, aes(x="", y=sales_amt, fill=product_name)) +
  geom_bar(stat = "identity") +
  facet_grid(facets = . ~ branch)

## 파이 그래프
ggplot(df_branch_sales, aes(x="", y=sales_amt, fill=product_name)) +
  geom_bar(stat = "identity") + 
  facet_grid(facets = . ~ branch) +
  coord_polar(theta = "y")


# 교차빈도분석
table(mtcars$gear)
table(mtcars$gear, mtcars$cyl)


## 각 메뉴 아이템의 주문비율을 누적막대그래프로 작성
df_branch_items <- subset(df_f_join_2, branch %in% c("강남", "마포", "강서")) %>% 
  group_by(branch, product_name)

View(df_branch_items)

df_branch_items_table <- table(df_branch_items$branch, df_branch_items$product_name) %>% data.frame()
df_branch_items2 <- df_branch_items_table %>% group_by(Var1) %>% mutate(n = Freq/sum(Freq)*100)

ggplot(df_branch_items2, aes(x=Var1, y=n, fill=Var2)) +
  geom_bar(stat = "identity") + 
  labs(title="지역별상품주문비율", x="지점", y="메뉴아이템판매비율", fill="메뉴아이템")
