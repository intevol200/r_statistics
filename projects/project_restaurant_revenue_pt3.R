library(readxl)
library(dplyr)
library(ggplot2)

customer_r <- read_excel("data/r_practice/customer_r.xlsx")
reservation_r <- read_excel("data/r_practice/reservation_r.xlsx")
order_info_r <- read_excel("data/r_practice/order_info_r.xlsx")
item_r <- read_excel("data/r_practice/item_r.xlsx")

colnames(customer_r) <- tolower(colnames(customer_r))
colnames(reservation_r) <- tolower(colnames(reservation_r))
colnames(order_info_r) <- tolower(colnames(order_info_r))
colnames(item_r) <- tolower(colnames(item_r))

df_f_join1 <- inner_join(reservation_r, order_info_r, by = "reserv_no")
df_f_join2 <- inner_join(df_f_join1, item_r, by = "item_id")
head(df_f_join2)

df_branch_items <- df_f_join2 %>% filter(branch == "강남" | branch == "마포" | branch == "강서")
table(df_branch_items$branch)

table(df_branch_items$branch, df_branch_items$product_name)

df_branch_items_table <- as.data.frame(table(df_branch_items$branch, df_branch_items$product_name))
df_branch_items_table

df_branch_items_percent <- df_branch_items_table %>% 
  group_by(Var1) %>% mutate(percent_items = Freq/sum(Freq) * 100)
df_branch_items_percent

ggplot(df_branch_items_percent, aes(x = Var1, y = percent_items, fill = Var2)) + 
  geom_bar(stat = "identity") +
  labs(title = "지점별 주문 건수 그래프", x = "지점", y = "메뉴 아이템 판매비율", fill = "메뉴 아이템")

# 결측값 확인 및 처리
x <- c(1,2,3,4,NA,6,7,8,9,NA)

is.na(x)
table(is.na(x))

sum(is.na(x))
sum(x)
sum(x, na.rm = TRUE)

# 실습문제
# • (a) 총 관측치의 개수
# • (b) 제조사(Manufacturer)의 개수(유일한 값)
# • (c) 첫번째 관측치의 제조사 이름
# • (d) 마지막 관측치의 제조사 이름
# • (e) 5번째 관측치의 제조사 이름은?

library(MASS)

Cars93_1 <- Cars93[c(1:10), c("Manufacturer", "Model", "Type")]
Cars93_1

Cars93_1 %>% summarise(n=n(),
          Mfc = n_distinct(Cars93_1$Manufacturer),
          First = first(Cars93_1$Manufacturer),
          Last = last(Cars93_1$Manufacturer),
          NTH = nth(Cars93_1$Manufacturer, 5))

# Cars93 데이터 프레임에서 '차종(Type)' 별로 구분해서
# • (a) 전체 관측치 개수
# • (b) (중복 없이 센) 제조사 개수
# • (c) 가격(Price)의 평균과
# • (d) 가격의 표준편차를 구하시오.
# • (단, 결측값은 포함하지 않고 계산함)

Cars93 %>% group_by(Type) %>%
  summarise(n=n(), manufacturer = n_distinct(Manufacturer),
            priceAvg = mean(Price, na.rm=TRUE), priceSd = sd(Price, na.rm=TRUE))