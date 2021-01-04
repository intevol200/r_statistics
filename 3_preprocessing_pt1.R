library(dplyr)
library(readxl)

# 실습문제
df <- data.frame(sex=c("M","F",NA,"M","F"),
                 score=c(5,4,3,4,NA))
df_filter <- df %>% filter(!is.na(score))
sum(df_filter$score)
mean(df_filter$score)


# 데이터 정렬하기
load("data/exdata1.rda")
exdata1
exdata1 %>% arrange(AGE)
exdata1 %>% arrange(desc(Y17_AMT))
exdata1 %>% arrange(AREA, desc(Y17_AMT))

# 실습문제
test <- data.frame(name=c("kim1","lee1","park1","kim2","park2","lee2","kim3"),
                   kor=c(54,92,83,NA,74,55,52),
                   eng=c(61,82,NA,64,84,60,58))
test_f <- test %>% filter(!is.na(kor) & (!is.na(eng)))
test_f

test_f$avg <- (test_f$kor + test_f$eng)/2
test_f$result <- ifelse(test_f$avg >= 70, "합격", "불합격")
test_f %>% arrange(desc(avg))


# 데이터 요약하기
exdata1 %>% summarise(TOT_Y17_AMT = sum(Y17_AMT))
exdata1 %>% d


# 데이터 결합하기
m_history <- read_excel("data/Sample2_m_history.xlsx")
f_history <- read_excel("data/Sample3_f_history.xlsx")
View(m_history)
View(f_history)

exdata_bindjoin <- bind_rows(m_history, f_history)
View(exdata_bindjoin)

# 실습문제
test1 <- data.frame(ID=c(1,2),
                    거주지=c("서울", "부산"))
test2 <- data.frame(ID=c(3,4),
                    성별=c("남","여"))
test_bindjoin <- bind_rows(test1, test2)
test_bindjoin


# 가로결합 - left_join
jeju_y17 <- read_excel("data/Sample4_y17_history.xlsx")
jeju_y16 <- read_excel("data/Sample5_y16_history.xlsx")
View(jeju_y17)
View(jeju_y16)
bind_col <- left_join(jeju_y17, jeju_y16, by="ID")
View(bind_col)

# 실습문제
test3 <- data.frame(ID=c(1,2),
                    거주지=c("서울", "부산"))
test4 <- data.frame(ID=c(1,2),
                    성별=c("남","여"))
test_bind_col <- left_join(test3, test4, by="ID")
test_bind_col


# 가로결합 - inner_join
bind_col_inner <- inner_join(jeju_y17, jeju_y16, by="ID")
View(bind_col_inner)

# 실습문제
test5 <- data.frame(ID=c(1,2,3),
                    거주지=c("서울", "부산","제주"))
test6 <- data.frame(ID=c(1,2,4),
                    성별=c("남", "여","남"))
test_col_inner <- inner_join(test5, test6, by="ID")
View(test_col_inner)
