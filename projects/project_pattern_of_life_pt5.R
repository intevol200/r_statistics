library(dplyr)
library(ggplot2)
library(descr)

load("data/welfare.rda")
welfare$religion <- ifelse(welfare$religion == 1, "yes", "no")
welfare$group_marriage <- ifelse(welfare$marriage == 1, "marriage",
                                 ifelse(welfare$marriage == 3, "divorce", NA))

religion_marriage <- welfare %>% 
  filter(!is.na(group_marriage)) %>% 
  group_by(religion, group_marriage) %>% 
  summarise(n=n()) %>% 
  mutate(tot_group = sum(n)) %>% 
  mutate(pct = round(n/tot_group*100, 1))

religion_marriage

divorce <- religion_marriage %>% filter(group_marriage == "divorce")
divorce

ggplot(divorce, aes(x=religion, y=pct)) + geom_col()


## 실습문제(count 함수와 mutate를 사용해 간소화)
religion_marriage <- welfare %>% 
  filter(!is.na(group_marriage)) %>% 
  count(religion, group_marriage) %>% 
  group_by(religion) %>% 
  mutate(pct = round(n/sum(n)*100, 1))

religion_marriage

# 나이대에 따른 이혼율 분석
table(welfare$ageg)
table(welfare$group_marriage)

ageg_marriage <- welfare %>% 
  filter(!is.na(group_marriage)) %>% 
  count(ageg, group_marriage) %>% 
  group_by(ageg) %>% 
  mutate(pct = round(n/sum(n)*100, 1))

ageg_marriage


ageg_divorce <- ageg_marriage %>%
  filter(ageg != "young" & group_marriage == "divorce")
ageg_divorce

ggplot(ageg_divorce, aes(x=ageg, y=pct)) + geom_col()

# 나이대와 종교에 따른 이혼율 분석
ageg_religion_marriage <- welfare %>% 
  filter(!is.na(group_marriage) & ageg != "young") %>% 
  count(ageg, religion, group_marriage) %>% 
  group_by(ageg, religion) %>% 
  mutate(pct = round(n/sum(n)*100,1))
ageg_religion_marriage

df_divorce <- ageg_religion_marriage %>% 
  filter(group_marriage == "divorce")
df_divorce

ggplot(df_divorce, aes(x=ageg, y=pct, fill=religion)) + geom_col(position = "dodge")

# 노년층이 많은 지역 분석
table(welfare$code_region)
list_region <- data.frame(code_region = c(1:7),
                          region = c("서울", "수도권(인천/경기)", "부산/경남/울산", 
                                     "대구/경북", "대전/충남", "강원/충북", "광주/전남/전북/제주도"))
list_region

welfare <- left_join(welfare, list_region, id="code_region")
welfare %>% select(code_region, region) %>% head(20)

region_ageg <- welfare %>% count(region, ageg) %>% 
  group_by(region) %>% mutate(pct = round(n/sum(n)*100,2))
region_ageg

ggplot(region_ageg, aes(x=region, y=pct, fill=ageg)) + geom_col() + coord_flip()


##
list_order_old <- region_ageg %>% filter(ageg == "old") %>% arrange(pct)
list_order_old

order <- list_order_old$region
order

ggplot(region_ageg, aes(x=region, y=pct, fill=ageg)) + 
  geom_col() + coord_flip() + scale_x_discrete(limits=order)


levels(region_ageg$ageg)
region_ageg$ageg <- factor(region_ageg$ageg, levels = c("old", "middle", "young"))
levels(region_ageg$ageg)

ggplot(data=region_ageg, aes(x=region, y=pct, fill=ageg)) + geom_col() + coord_flip() + scale_x_discrete(limits=order)

