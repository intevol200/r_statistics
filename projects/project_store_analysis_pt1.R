library(readxl)
library(treemap)
library(dplyr)
library(ggplot2)

ck <- read_excel("data/치킨집_가공.xlsx")
head(ck)
table(is.na(ck$소재지전체주소))

# 동이름 추출하기
addr <- substr(ck$소재지전체주소, 11, 16)
head(addr)

# 동이름에서 숫자제거
addr_num <- gsub("[0-9]", "", addr)
head(addr_num)

# 주소에서 공백제거
addr_trim <- gsub(" ", "", addr_num)
addr_trim

# 동별 변수 카운트
addr_count <- addr_trim %>% table() %>% data.frame()
addr_count
treemap(addr_count, index = ".", vSize = "Freq",
        title = "서대문구 동별 치킨집 분포")
arrange(addr_count, desc(Freq)) %>% head()

## 실습문제
## 연도별/동별 인허가 현황 시각화
n_ck <- read_excel("data/치킨집_가공2.xlsx")
head(n_ck)
table(is.na(ck$소재지전체주소))

n_addr <- substr(n_ck$소재지전체주소, 11, 16)
n_addr_num <- gsub("[0-9]", "", n_addr)
n_ck$소재지전체주소 <- gsub(" ", "", n_addr_num)
n_ck$인허가일자 <- substr(n_ck$인허가일자, 1, 4)

count_ck <- n_ck %>% group_by(소재지전체주소, 인허가일자) %>% summarise(n=n())
View(count_ck)

ggplot(count_ck, aes(x=소재지전체주소, y=n, fill=인허가일자)) + geom_col() + coord_flip()
