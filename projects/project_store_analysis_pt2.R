library(readxl)
library(treemap)
library(dplyr)
library(ggplot2)

# 복습
ck <- read_excel("data/치킨집_가공2.xlsx")
head(ck)
table(is.na(ck$소재지전체주소))
table(is.na(ck$인허가일자))

ck$소재지전체주소 <- substr(ck$소재지전체주소, 11, 16)
head(ck)

ck$소재지전체주소 <- gsub("[0-9]", "", ck$소재지전체주소)
ck$소재지전체주소 <- gsub(" ", "", ck$소재지전체주소)
ck$인허가일자 <- substr(ck$인허가일자, 1, 4)

seodaemoon_year_count <- ck %>% group_by(소재지전체주소, 인허가일자) %>% summarise(n=n())
View(seodaemoon_year_count)

ggplot(seodaemoon_year_count, aes(x=reorder(소재지전체주소, n), y=n, fill=인허가일자)) + geom_col() + coord_flip()

# 실습문제
dangu <- read_excel("data/서울_당구장업.xlsx")
names(dangu)

table(is.na(dangu))
table(is.na(dangu$인허가일자))
table(is.na(dangu$영업상태명))
table(is.na(dangu$소재지전체주소))

dangu <- dangu %>% filter(!is.na(소재지전체주소))
table(is.na(dangu$소재지전체주소))

dangu_ft <- subset(dangu, 영업상태="영업/정상", 
                   select=c(인허가일자,소재지전체주소))
table(is.na(dangu_ft))

dangu_ft$addr <- substr(dangu_ft$소재지전체주소, 7, 10)
table(dangu_ft$addr)
dangu_ft$addr <- gsub("중구 \\w", "중구", dangu_ft$addr)
dangu_ft$addr <- gsub(" ","", dangu_ft$addr)
table(dangu_ft$addr)

table(dangu_ft$인허가일자)
dangu_ft$year <- substr(dangu_ft$인허가일자, 1, 4)
table(dangu_ft$year)
dangu_ft <- subset(dangu_ft, year >= '2010')
table(dangu_ft$year)

ggplot(dangu_ft, aes(x=addr, fill=year)) + geom_bar() +
  ggtitle("서울시 최근 10년간 구별 당구장 인가현황")

dangu_ft$year <- factor(dangu_ft$year, levels=c(2020,2019,2018,2017,2016,2015,2014,2013,2012,2011,2010))

ggplot(dangu_ft, aes(x=addr, fill=year)) + geom_bar() +
  ggtitle("서울시 최근 10년간 구별 당구장 인가현황")

dangu_ft_as <- dangu_ft %>% group_by(addr,year) %>% summarise(n=n())
dangu_ft_as
ggplot(dangu_ft_as, aes(x=reorder(addr,-n, FUN=sum),   
                        y=n,fill=year)) + geom_col() +
  ggtitle("서울시 최근 10년간 구별 당구장 인가현황")
