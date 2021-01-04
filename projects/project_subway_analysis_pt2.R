library(readxl)
library(dplyr)
library(descr)
library(ggplot2)
library(reshape2)

subway01 <-read.csv("c:/Rstudy/data/subway_data/CARD_SUBWAY_MONTH_201901.csv")
subway02 <-read.csv("c:/Rstudy/data/subway_data/CARD_SUBWAY_MONTH_201902.csv")
subway03 <-read.csv("c:/Rstudy/data/subway_data/CARD_SUBWAY_MONTH_201903.csv")
subway04 <-read.csv("c:/Rstudy/data/subway_data/CARD_SUBWAY_MONTH_201904.csv")
subway05 <-read.csv("c:/Rstudy/data/subway_data/CARD_SUBWAY_MONTH_201905.csv")
subway06 <-read.csv("c:/Rstudy/data/subway_data/CARD_SUBWAY_MONTH_201906.csv")
subway07 <-read.csv("c:/Rstudy/data/subway_data/CARD_SUBWAY_MONTH_201907.csv")
subway08 <-read.csv("c:/Rstudy/data/subway_data/CARD_SUBWAY_MONTH_201908.csv")
subway09 <-read.csv("c:/Rstudy/data/subway_data/CARD_SUBWAY_MONTH_201909.csv")
subway10 <-read.csv("c:/Rstudy/data/subway_data/CARD_SUBWAY_MONTH_201910.csv")
subway11 <-read.csv("c:/Rstudy/data/subway_data/CARD_SUBWAY_MONTH_201911.csv")
subway12 <-read.csv("c:/Rstudy/data/subway_data/CARD_SUBWAY_MONTH_201912.csv")

subway <-bind_rows(subway01, subway02, subway03, subway04,
                   subway05, subway06, subway07, subway08,
                   subway09, subway10, subway11, subway12)
View(subway)

# 데이터추출하기(1호선~9호선)
sw1_9 <-subset(subway, 노선명%in% c("1호선","2호선","3호선",
                                 "4호선","5호선","6호선",
                                 "7호선","8호선","9호선"))
View(sw1_9)

sw1_9$tot = sw1_9$승차총승객수 + sw1_9$하차총승객수
head(sw1_9) 

st5 <-sw1_9 %>% group_by(역명) %>% summarise(n=sum(tot)) %>% arrange(desc(n)) %>% head(5)
st5
st5_name <-st5$역명
st5_name

y_st5 <-subset(sw1_9, 역명%in% st5_name, select=c(사용일자, 역명,tot))
View(y_st5)

m01 <-y_st5 %>% filter(사용일자>= 20190101 & 사용일자<= 20190131) %>%  group_by(역명) %>% summarise(n = sum(tot)) 
m02 <-y_st5 %>% filter(사용일자>= 20190201 & 사용일자<= 20190231) %>%  group_by(역명) %>% summarise(n = sum(tot)) 
m03 <-y_st5 %>% filter(사용일자>= 20190301 & 사용일자<= 20190331) %>%  group_by(역명) %>% summarise(n = sum(tot))
m04 <-y_st5 %>% filter(사용일자>= 20190401 & 사용일자<= 20190431) %>%  group_by(역명) %>% summarise(n = sum(tot))
m05 <-y_st5 %>% filter(사용일자>= 20190501 & 사용일자<= 20190531) %>% group_by(역명) %>% summarise(n = sum(tot))
m06 <-y_st5 %>% filter(사용일자>= 20190601 & 사용일자<= 20190631) %>%  group_by(역명) %>% summarise(n = sum(tot))
m07 <-y_st5 %>% filter(사용일자>= 20190701 & 사용일자<= 20190731) %>%  group_by(역명) %>% summarise(n = sum(tot))
m08 <-y_st5 %>% filter(사용일자>= 20190801 & 사용일자<= 20190831) %>%  group_by(역명) %>% summarise(n = sum(tot))
m09 <-y_st5 %>% filter(사용일자>= 20190901 & 사용일자<= 20190931) %>%  group_by(역명) %>% summarise(n = sum(tot))
m10 <-y_st5 %>% filter(사용일자>= 20191001 & 사용일자<= 20191031) %>%  group_by(역명) %>% summarise(n = sum(tot))
m11 <-y_st5 %>% filter(사용일자>= 20191101 & 사용일자<= 20191131) %>%  group_by(역명) %>% summarise(n = sum(tot))
m12 <-y_st5 %>% filter(사용일자>= 20191201 & 사용일자<= 20191231) %>%  group_by(역명) %>% summarise(n = sum(tot))

y_st<-mutate(m01, m02$n, m03$n, m04$n, m05$n, m06$n, m07$n, m08$n, m09$n, m10$n, m11$n, m12$n)
View(y_st)

y_st<-rename(y_st,Jan=n, Feb=`m02$n`, Mar=`m03$n`, 
             Apr=`m04$n`,May=`m05$n`, Jun=`m06$n`, 
             Jul=`m07$n`,Aug=`m08$n`,Sep=`m09$n`, 
             Oct=`m10$n`,Nov=`m11$n`, Dec=`m12$n`)

y_st<-melt(y_st, id.var="역명")
View(y_st)

y_st<-rename(y_st, month=variable, tot=value)
y_st$역명<-factor(y_st$역명, levels = st5_name)

ggplot(y_st, aes(x=month, y=tot, fill=역명))+ geom_col() +
  scale_y_continuous(labels = scales::comma)
