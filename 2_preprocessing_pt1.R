# 복습
library(dplyr)
library(readxl)

exdata1 <- read_excel("data/Sample1.xlsx")
exdata1 <- rename(exdata1, Y17_AMT = AMT17, Y16_AMT = AMT16)
View(data1)


# 파생변수
ls(exdata1)
exdata1$AMT <- exdata1$Y17_AMT + exdata1$Y16_AMT
exdata1$CNT <- exdata1$Y17_CNT + exdata1$Y16_CNT
exdata1$AVG_AMT <- exdata1$AMT / exdata1$CNT
View(exdata1)

# 조건문
x <- 85
result <- ifelse(x >= 80, "합격", "불합격")
result
result <- ifelse(x >= 90, "A",
                 ifelse(x >= 80, "B", "F"))
result

# 실습문제
exdata1$AGE50_YN <- ifelse(exdata1$AGE >= 50, "Y", "N")
exdata1$AGE_GR10 <- ifelse(exdata1$AGE >= 50, "A1.50++",
                           ifelse(exdata1$AGE >= 40, "A2.4049",
                                  ifelse(exdata1$AGE >= 30, "A3.3039",
                                         ifelse(exdata1$AGE >= 20,"A5.0019"))))
View(exdata1)


# 데이터 전처리
select(exdata1, ID)
exdata1 %>% select(ID)
exdata1 %>% select(ID, AREA, Y17_CNT)
exdata1 %>% select(-AREA)
exdata1 %>% select(-AREA, -Y17_CNT)
exdata1 %>% filter(AREA %in% c("서울", "경기"))
seoul_gg <- exdata1 %>% filter(AREA %in% c("서울", "경기"))
seoul_gg

# 실습문제
exdata1 %>% filter(AREA == "서울" & Y17_CNT >= 10 & Y17_CNT <= 30)
exdata1 %>% filter(AREA != "서울" & SEX == "M")
save(exdata1, file="data/exdata1.rda")


# subset 함수
a1 <- c(1:9)
a2 <- c(11:19)
df <- data.frame(a1,a2)
df

filter(df, a1 > 5)
subset(df, a1 > 5)
subset(df, a1 > 5, select=c(a1,a2))


# 결측치 제거하기
df <- data.frame(sex=c("M","F",NA,"M","F"),
                 score=c(5,4,3,4,NA))
df
sum(df$score)
table(is.na(df))
df_nimiss <- df %>% filter(!is.na(score))
df_nimiss
mean(df_nimiss$score)
sum(df_nimiss$score)
