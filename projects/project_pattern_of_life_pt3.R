# 복습
library(dplyr)
library(ggplot2)
library(descr)
load("data/welfare.rda")
table(welfare$ageg)

sex_age <- welfare %>% 
           filter(!is.na(income)) %>% 
           group_by(age, sex) %>% 
           summarise(mean_income=mean(income))
head(sex_age)

ggplot(sex_age, aes(x=age, y=mean_income, col=sex)) + geom_line()

# 직업별 월급 차이 분석
## 직업분류코드 파일 불러서 구조 확인
class(welfare$code_job)
table(welfare$code_job)
library(readxl)

list_job <- read_excel("data/Koweps_Codebook.xlsx", col_names = T, sheet = 2)
head(list_job)
dim(list_job)

## job 변수를 welfare와 결합
welfare <- left_join(welfare, list_job, by="code_job")
head(welfare)
table(welfare$job)

## welfare에 결합된 내용 필터링
welfare %>% filter(!is.na(code_job)) %>% select(code_job, job) %>% head(10)

## 직업별 월급 평균표 만들기
job_income <- welfare %>% 
              filter(!is.na(job) & !is.na(income)) %>% 
              group_by(job) %>% 
              summarise(mean_income=mean(income))
job_income

## 월급을 내림차순 정렬 후 상위 10개 추출
top10 <- job_income %>% arrange(desc(mean_income)) %>% head(10)
top10

ggplot(top10, aes(x=job, y=mean_income)) + geom_col() + coord_flip()

### 문제: 하위 10위에 해당하는 직업에 대한 시각화 해보기
bottom10 <- job_income %>% arrange(mean_income) %>% head(10)
bottom10

ggplot(bottom10, aes(x=job, y=mean_income)) + geom_col() + coord_flip() + ylim(0,850)



# 성별 직업 빈도
## 각 성별로 직업별 빈도 상위 10개 추출
job_male <- welfare %>% 
            filter(!is.na(job) & sex=="male") %>% 
            group_by(job) %>% 
            summarise(n=n()) %>% 
            arrange(desc(n)) %>% 
            head(10)

job_female <- welfare %>% 
              filter(!is.na(job) & sex=="female") %>% 
              group_by(job) %>% 
              summarise(n=n()) %>% 
              arrange(desc(n)) %>% 
              head(10)

job_male
job_female

ggplot(job_male, aes(x=job, y=n)) + geom_col() + coord_flip()
ggplot(job_female, aes(x=job, y=n)) + geom_col() + coord_flip()


## reorder 함수 (막대그래프의 길이에 따라 정렬)
## reorder(정렬하고 싶은 변수, 정렬 기준)
ggplot(job_male, aes(x=reorder(job, n), y=n)) + geom_col() + coord_flip()
ggplot(job_female, aes(x=reorder(job, -n), y=n)) + geom_col() + coord_flip()
