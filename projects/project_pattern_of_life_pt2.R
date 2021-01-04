# (복습) 실습 프로젝트 : 한국인의 삶 분석
library(readxl)
library(dplyr)
library(foreign)
raw_welfare <- read.spss(file = "data/Koweps_hpc10_2018_beta1.sav",
                         to.data.frame = T)
welfare <- raw_welfare

welfare <- rename(welfare, sex = h10_g3, birth = h10_g4,
                  marriage = h10_g10, religion = h10_g11,
                  income = p1002_8aq1, code_job = h10_eco9,
                  code_region = h10_reg7)

## 성별에 따른 월급차이
welfare$sex <- ifelse(welfare$sex == 1, "male", "female")
income_tr <- welfare %>% filter(!is.na(income))
sex_income <- income_tr %>% group_by(sex) %>% summarise(mean_income = mean(income))

barplot(sex_income$mean_income, xlab = "SEX", ylab = "MEAN_INCOME")


## 나이와 월급 관계
summary(welfare$birth)

welfare_birth <- table(welfare$birth)
barplot(welfare_birth)

table(is.na(welfare$birth))
welfare$birth <- ifelse(welfare$birth == 9999, NA, welfare$birth)

welfare <- welfare %>% filter(!is.na(birth))
table(is.na(welfare$birth))
barplot(table(birth_tr$birth))

welfare$age <- (2018 - welfare$birth) + 1
summary(welfare$age)
barplot(table(welfare$age))

age_income <- welfare %>% filter(!is.na(income)) %>% group_by(age) %>% summarise(mean_income = mean(income))
plot(age_income$age, age_income$mean_income, xlab = "AGE", ylab = "MEAN_INCOME")

save(welfare, file = "data/welfare.rda")


# ggplot2 패키지
install.packages("ggplot2")
library(ggplot2)

ggplot(data = datasample, aes(x=class, y=math))

str(airquality)
ggplot(data = airquality, aes(x=Day, y=Temp)) + geom_point()


# 연령대에 따른 월급차이 분석
library(descr)
table(welfare$age)

welfare$ageg <- ifelse(welfare$age < 30, "young", 
                       ifelse(welfare$age <= 59, "middle", "old"))
welfare_age <- table(welfare$ageg)

barplot(welfare_age)
freq(welfare$ageg)
qplot(welfare$ageg)
ggplot(welfare, aes(ageg)) + geom_bar()


# 연령대 및 성별 월급 차이 분석
welfare_age <- table(welfare$ageg)
barplot(welfare_age)
freq(welfare$ageg)
qplot(welfare$ageg)
ggplot(welfare, aes(ageg)) + geom_bar()

ageg_income <- welfare %>% 
                filter(!is.na(income)) %>% 
                group_by(ageg) %>% 
                summarise(mean_income=mean(income))

ggplot(ageg_income, aes(x=ageg, y=mean_income)) + 
       geom_col() + scale_x_discrete(limits = c("young", "middle", "old"))

sex_income <- welfare %>% filter(!is.na(income)) %>% 
              group_by(ageg, sex) %>% summarise(mean_income=mean(income))
sex_income

ggplot(sex_income, aes(x=ageg, y=mean_income, fill=sex)) + geom_col()
##################################################
ggplot(sex_income, aes(x=ageg, y=mean_income, fill=sex)) + geom_col() +
  scale_x_discrete(limits=c("young", "middle", "old"))
##################################################
ggplot(sex_income, aes(x=ageg, y=mean_income, fill=sex)) + geom_col(position = "dodge") +
  scale_x_discrete(limits=c("young", "middle", "old"))


# 실습문제
sex_age <- welfare %>% filter(!is.na(income)) %>% 
           group_by(age, sex) %>% summarise(mean_income=mean(income))
head(sex_age)

ggplot(sex_age, aes(x=age, y=mean_income, col = sex)) + geom_line()

save(welfare, file = "data/welfare.rda")
