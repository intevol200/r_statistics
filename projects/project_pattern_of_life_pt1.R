# 실습문제 exam-1
library(readxl)
library(dplyr)

mid_exam <- read_excel("data/mid_exam.xlsx")
mid_exam <- rename(mid_exam, MATH_MID = MATH, ENG_MID = ENG)
View(mid_exam)

# 실습문제 exam-2
final_exam <- read_excel("data/final_exam.xlsx")
final_exam <- rename(final_exam, MATH_FINAL = MATH, ENG_FINAL = ENG)
View(final_exam)

# 실습문제 exam-3
total_exam <- inner_join(mid_exam, final_exam, by="ID")
total_exam$MATH_AVG <- (total_exam$MATH_MID + total_exam$MATH_FINAL)/2
total_exam$ENG_AVG <- (total_exam$ENG_MID + total_exam$ENG_FINAL)/2
total_exam$TOTAL_AVG <- (total_exam$MATH_MID + total_exam$MATH_FINAL +
                         total_exam$ENG_MID + total_exam$ENG_FINAL)/4
View(total_exam)

# 실습문제 exam-4
mean(total_exam$MATH_AVG)
mean(total_exam$ENG_AVG)
total_exam %>% filter(MATH_MID >= 80 & ENG_MID >= 90)
boxplot(total_exam$MATH_AVG, total_exam$ENG_AVG, 
        names = c("수학평균", "영어평균"), col = c("green", "yellow"))



# 실습 프로젝트 : 한국인의 삶 분석
install.packages("foreign")
library(foreign)
raw_welfare <- read.spss(file = "data/Koweps_hpc10_2018_beta1.sav",
                         to.data.frame = T)
welfare <- raw_welfare

head(welfare)
tail(welfare)
View(welfare)
dim(welfare)
str(welfare)
summary(welfare)

welfare <- rename(welfare, sex = h10_g3, birth = h10_g4,
                  marriage = h10_g10, religion = h10_g11,
                  income = p1002_8aq1, code_job = h10_eco9,
                  code_region = h10_reg7)
View(welfare)

# 성별
class(welfare$sex)
table(welfare$sex)

welfare$sex <- ifelse(welfare$sex == 9, NA, welfare$sex)
welfare <- welfare %>% filter(!is.na(sex))
table(is.na(welfare$sex))

welfare$sex <- ifelse(welfare$sex == 1, "male", "female")
table(welfare$sex)
welfare_sex <- table(welfare$sex)
barplot(welfare_sex)


# 월급
class(welfare$income)
summary(welfare$income)

welfare$income <- ifelse(welfare$income %in% 
                           c(0,9999), NA, welfare$income)
table(is.na(welfare$income))

income_tr <- welfare %>% filter(!is.na(income))
table(is.na(income_tr$income))


# 성별 월급 평균
sex_income <- income_tr %>% group_by(sex) %>% summarise(mean_income = mean(income))
sex_income

barplot(sex_income$mean_income, xlab = "SEX", ylab = "MEAN_INCOME")


# 나이 월급 관계
summary(welfare$birth)

welfare$birth <- ifelse(welfare$birth == 9999, NA, welfare$birth)
table(is.na(welfare$birth))

birth_tr <- welfare %>% filter(!is.na(birth))
table(is.na(birth_tr$birth))
summary(birth_tr$birth)

barplot(table(birth_tr$birth))


## age 변수 만들기
welfare$age <- (2018 - welfare$birth) + 1
summary(welfare$age)

barplot(table(welfare$age))


## 나이별 월급 평균표
age_income <- welfare %>% filter(!is.na(income)) %>% group_by(age) %>% summarise(mean_income = mean(income))
plot(age_income$age, age_income$mean_income, xlab = "AGE", ylab = "MEAN_INCOME")
