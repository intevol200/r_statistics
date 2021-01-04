library(readxl)
library(dplyr)
library(ggplot2)
library(reshape2)

chicken_s <- read_excel("data/서울치킨인허가.xlsx")
names(chicken_s)

# 결측치 확인
table(is.na(chicken_s))
table(is.na(chicken_s$인허가일자))
table(is.na(chicken_s$영업상태명))
table(is.na(chicken_s$소재지전체주소))

# 결측치 제거
chicken_s <- chicken_s %>% filter(!is.na(소재지전체주소))
table(is.na(chicken_s$소재지전체주소))

# 영업중인 업체 추출 + 새로운 데이터셋 생성(chicken_s_ft)
chicken_s_ft <- subset(chicken_s, 영업상태명="영업/정상", select=c(인허가일자,소재지전체주소))
table(is.na(chicken_s_ft))

# 주소에서 OO구 추출
chicken_s_ft$addr <- substr(chicken_s_ft$소재지전체주소, 7, 10)
table(chicken_s_ft$addr)

chicken_s_ft$addr <- gsub("중구 \\w", "중구", chicken_s_ft$addr)
chicken_s_ft$addr <- gsub(" ","", chicken_s_ft$addr)
table(chicken_s_ft$addr)

# 인허가일자에서 연도 추출
table(chicken_s_ft$인허가일자)
chicken_s_ft$year <- substr(chicken_s_ft$인허가일자, 1, 4)
table(chicken_s_ft$year)
chicken_s_ft$year

# 상위 4개 구 추출
chicken_s_5 <- chicken_s_ft %>% group_by(addr) %>% summarise(n=n()) %>% arrange(desc(n)) %>% head(5)
chicken_s_5

# 2000년 이후 정보만 필터링
chicken_s_ft <- subset(chicken_s_ft, year >= '2000', select=c(addr,year))
chicken_s_ft

# 구, 연도별 그룹화하고 개수 계산
chicken_1 <- chicken_s_ft %>% subset(addr=='마포구') %>% group_by(year) %>% summarise(마포구=n())
chicken_2 <- chicken_s_ft %>% subset(addr=='구로구') %>% group_by(year) %>% summarise(구로구=n())
chicken_3 <- chicken_s_ft %>% subset(addr=='양천구') %>% group_by(year) %>% summarise(양천구=n())
chicken_4 <- chicken_s_ft %>% subset(addr=='중랑구') %>% group_by(year) %>% summarise(중랑구=n())
chicken_5 <- chicken_s_ft %>% subset(addr=='동대문구') %>% group_by(year) %>% summarise(동대문구=n())

# 연도별 조인
full_col <- full_join(chicken_1,chicken_2, by = "year")
full_col <- full_join(full_col,chicken_3, by = "year")
full_col <- full_join(full_col,chicken_4, by = "year")
full_col <- full_join(full_col,chicken_5, by = "year")
full_col

# 값이 없으면 0으로 처리
full_col[is.na(full_col)] = 0
View(full_col)

# melt 함수로 행단위로 데이터 변경
exam1 <- melt(full_col, id = "year", measure.vars=c("마포구","구로구","양천구","중랑구","동대문구"))
View(exam1)

# melt 함수에 따른 열이름 변경
exam1 <- rename(exam1, 구이름 = variable)
exam1 <- rename(exam1, 허가수 = value)
levels(exam1$구이름)

# 구, 연도별 정렬
exam1_arrange <- exam1 %>% arrange(구이름, year)
View(exam1_arrange)

# 그래프 작성
ggplot(exam1_arrange, aes(year,허가수,group=구이름))+geom_line(aes(color=구이름))
