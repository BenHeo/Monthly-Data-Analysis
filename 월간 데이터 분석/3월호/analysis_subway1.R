library(tidyverse)
library(lubridate)
library(data.table)

setwd('C:/Users/asd/Desktop/데이터 사이언스/월간 데이터 분석/3월호/data')
# read files
df <- fread('cleansedDf.csv', header = TRUE)
head(df)

# 목표1: 주말과 주중에 시간대별 분석을 진행한다. 또한 전체적으로 어느 역이 가장 유동인구가 많은지 등을 분석해 보고 강남 등 흔히 생각할 수 있는 곳인지 아닌 곳이 있는지를 본다.
#        출퇴근 시간대에는 대학가, 회사가 쪽에 유동인구가 많을 것은 당연하지만 그들이 어디서부터 몇 분 거리부터 많아지는지 물어볼만하다.

# 목표2: 24시 이후 늦은 시간에 타는 사람들이 많은 곳은 어디이며, 5시에 타는 사람이 많은 곳은 어디일까?

# 주중 전체 역 중 5시에 (타는 사람-내리는 사람)이 많은 top40
weekdayat5 <- df %>% filter(weekend == 0) %>% select(stationK, five) %>% 
  group_by(stationK) %>% summarise(avg = mean(five)) %>%
  arrange(desc(avg)) %>% mutate(rank = dense_rank(desc(avg))) %>% top_n(40,avg)

wd5 <- ggplot(weekdayat5, aes(x=reorder(stationK,-avg), y=avg, fill=avg)) + 
  geom_bar(stat="identity", position="dodge")+ scale_fill_gradient(low = 'skyblue', high = 'green') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  geom_hline(yintercept = mean(weekdayat5$avg), lty = 2) + 
  geom_hline(yintercept = quantile(weekdayat5$avg, 0.75), lty = 2, color = 'red')+
  geom_hline(yintercept = quantile(weekdayat5$avg, 0.25), lty = 2, color = 'blue')+
  geom_hline(yintercept = quantile(weekdayat5$avg, 0.9), lty = 1, color = 'deeppink')+
  labs(x='Station', y='average # of (board - exit)')

# 주말 전체 역 중 5시에 (타는 사람-내리는 사람)이 많은 top40
weekendat5 <- df %>% filter(weekend == 1) %>% select(stationK, five) %>% 
  group_by(stationK) %>% summarise(avg = mean(five)) %>%
  arrange(desc(avg)) %>% mutate(rank = dense_rank(desc(avg))) %>% top_n(40,avg)

we5 <- ggplot(weekendat5, aes(x=reorder(stationK,-avg), y=avg, fill=avg)) + 
  geom_bar(stat="identity", position="dodge")+ scale_fill_gradient(low = 'skyblue', high = 'green') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  geom_hline(yintercept = mean(weekendat5$avg), lty = 2) + 
  geom_hline(yintercept = quantile(weekendat5$avg, 0.75), lty = 2, color = 'red')+
  geom_hline(yintercept = quantile(weekendat5$avg, 0.25), lty = 2, color = 'blue')+
  geom_hline(yintercept = quantile(weekendat5$avg, 0.9), lty = 1, color = 'deeppink')+
  labs(x='Station', y='average # of (board - exit)')

# 주중 전체 역 중 6시에 (타는 사람-내리는 사람)이 많은 top40
weekdayat6 <- df %>% filter(weekend == 0) %>% select(stationK, six) %>% 
  group_by(stationK) %>% summarise(avg=mean(six)) %>%
  arrange(desc(avg)) %>% mutate(rank = dense_rank(desc(avg))) %>% top_n(40,avg)

wd6 <- ggplot(weekdayat6, aes(x=reorder(stationK,-avg), y=avg, fill=avg)) + 
  geom_bar(stat="identity", position="dodge")+ scale_fill_gradient(low = 'skyblue', high = 'green') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  geom_hline(yintercept = mean(weekdayat6$avg), lty = 2) + 
  geom_hline(yintercept = quantile(weekdayat6$avg, 0.75), lty = 2, color = 'red')+
  geom_hline(yintercept = quantile(weekdayat6$avg, 0.25), lty = 2, color = 'blue')+
  geom_hline(yintercept = quantile(weekdayat6$avg, 0.9), lty = 1, color = 'deeppink')+
  labs(x='Station', y='average # of (board - exit)')

# 주말 전체 역 중 6시에 (타는 사람-내리는 사람)이 많은 top40
weekendat6 <- df %>% filter(weekend == 1) %>% select(stationK, six) %>% 
  group_by(stationK) %>% summarise(avg=mean(six)) %>%
  arrange(desc(avg)) %>% mutate(rank = dense_rank(desc(avg))) %>% top_n(40,avg)

we6 <- ggplot(weekendat6, aes(x=reorder(stationK,-avg), y=avg, fill=avg)) + 
  geom_bar(stat="identity", position="dodge")+ scale_fill_gradient(low = 'skyblue', high = 'green') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  geom_hline(yintercept = mean(weekendat6$avg), lty = 2) + 
  geom_hline(yintercept = quantile(weekendat6$avg, 0.75), lty = 2, color = 'red')+
  geom_hline(yintercept = quantile(weekendat6$avg, 0.25), lty = 2, color = 'blue')+
  geom_hline(yintercept = quantile(weekendat6$avg, 0.9), lty = 1, color = 'deeppink')+
  labs(x='Station', y='average # of (board - exit)')


# 주중 전체 역 중 7시에 (타는 사람-내리는 사람)이 많은 top40
weekdayat7 <- df %>% filter(weekend == 0) %>% select(stationK, seven) %>% 
  group_by(stationK) %>% summarise(avg = mean(seven)) %>%
  arrange(desc(avg)) %>% mutate(rank = dense_rank(desc(avg))) %>% top_n(40,avg)

wd7 <- ggplot(weekdayat7, aes(x=reorder(stationK,-avg), y=avg, fill=avg)) + 
  geom_bar(stat="identity", position="dodge")+ scale_fill_gradient(low = 'skyblue', high = 'green') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  geom_hline(yintercept = mean(weekdayat7$avg), lty = 2) + 
  geom_hline(yintercept = quantile(weekdayat7$avg, 0.75), lty = 2, color = 'red')+
  geom_hline(yintercept = quantile(weekdayat7$avg, 0.25), lty = 2, color = 'blue')+
  geom_hline(yintercept = quantile(weekdayat7$avg, 0.9), lty = 1, color = 'deeppink')+
  labs(x='Station', y='average # of (board - exit)')


# 주말 전체 역 중 7시에 (타는 사람-내리는 사람)이 많은 top40
weekendat7 <- df %>% filter(weekend == 0) %>% select(stationK, seven) %>% 
  group_by(stationK) %>% summarise(avg = mean(seven)) %>%
  arrange(desc(avg)) %>% mutate(rank = dense_rank(desc(avg))) %>% top_n(40,avg)

we7 <- ggplot(weekendat7, aes(x=reorder(stationK,-avg), y=avg, fill=avg)) + 
  geom_bar(stat="identity", position="dodge")+ scale_fill_gradient(low = 'skyblue', high = 'green') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  geom_hline(yintercept = mean(weekendat7$avg), lty = 2) + 
  geom_hline(yintercept = quantile(weekendat7$avg, 0.75), lty = 2, color = 'red')+
  geom_hline(yintercept = quantile(weekendat7$avg, 0.25), lty = 2, color = 'blue')+
  geom_hline(yintercept = quantile(weekendat7$avg, 0.9), lty = 1, color = 'deeppink')+
  labs(x='Station', y='average # of (board - exit)')

# 주중 전체 역 중 18시에 (타는 사람-내리는 사람)이 많은 top40
weekdayat18 <- df %>% filter(weekend == 0) %>% select(stationK, eighteen) %>% 
  group_by(stationK) %>% summarise(avg = mean(eighteen)) %>%
  arrange(desc(avg)) %>% mutate(rank = dense_rank(desc(avg))) %>% top_n(40,avg)

wd18 <- ggplot(weekdayat18, aes(x=reorder(stationK,-avg), y=avg, fill=avg)) + 
  geom_bar(stat="identity", position="dodge")+ scale_fill_gradient(low = 'skyblue', high = 'green') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  geom_hline(yintercept = mean(weekdayat18$avg), lty = 2) + 
  geom_hline(yintercept = quantile(weekdayat18$avg, 0.75), lty = 2, color = 'red')+
  geom_hline(yintercept = quantile(weekdayat18$avg, 0.25), lty = 2, color = 'blue')+
  geom_hline(yintercept = quantile(weekdayat18$avg, 0.9), lty = 1, color = 'deeppink')+
  labs(x='Station', y='average # of (board - exit)')

# 주말 전체 역 중 18시에 (타는 사람-내리는 사람)이 많은 top40
weekendat18 <- df %>% filter(weekend == 1) %>% select(stationK, eighteen) %>% 
  group_by(stationK) %>% summarise(avg = mean(eighteen)) %>%
  arrange(desc(avg)) %>% mutate(rank = dense_rank(desc(avg))) %>% top_n(40,avg)

we18 <- ggplot(weekendat18, aes(x=reorder(stationK,-avg), y=avg, fill=avg)) + 
  geom_bar(stat="identity", position="dodge")+ scale_fill_gradient(low = 'skyblue', high = 'green') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  geom_hline(yintercept = mean(weekendat18$avg), lty = 2) + 
  geom_hline(yintercept = quantile(weekendat18$avg, 0.75), lty = 2, color = 'red')+
  geom_hline(yintercept = quantile(weekendat18$avg, 0.25), lty = 2, color = 'blue')+
  geom_hline(yintercept = quantile(weekendat18$avg, 0.9), lty = 1, color = 'deeppink')+
  labs(x='Station', y='average # of (board - exit)')

# 24시 이후 전체 역 중 (타는 사람-내리는 사람)이 많은 top40
afternn <- df %>% filter(weekend == 0) %>% select(stationK, after24) %>% 
  group_by(stationK) %>% summarise(avg = mean(after24)) %>%
  arrange(desc(avg)) %>% mutate(rank = dense_rank(desc(avg))) %>% top_n(40,avg)

wd24 <- ggplot(afternn, aes(x=reorder(stationK,-avg), y=avg, fill=avg)) + 
  geom_bar(stat="identity", position="dodge")+ scale_fill_gradient(low = 'skyblue', high = 'green') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  geom_hline(yintercept = mean(afternn$avg), lty = 2) + 
  geom_hline(yintercept = quantile(afternn$avg, 0.75), lty = 2, color = 'red')+
  geom_hline(yintercept = quantile(afternn$avg, 0.25), lty = 2, color = 'blue')+
  geom_hline(yintercept = quantile(afternn$avg, 0.9), lty = 1, color = 'deeppink')+
  labs(x='Station', y='average # of (board - exit)')

# 주말 24시 이후는 별로 필요 없다 오히려 23시가 궁금
# 주중 전체 역 중 23시에 (타는 사람-내리는 사람)이 많은 top40
weekdayat23 <- df %>% filter(weekend == 0) %>% select(stationK, twentythree) %>% 
  group_by(stationK) %>% summarise(avg = mean(twentythree)) %>%
  arrange(desc(avg)) %>% mutate(rank = dense_rank(desc(avg))) %>% top_n(40,avg)

wd23 <- ggplot(weekdayat23, aes(x=reorder(stationK,-avg), y=avg, fill=avg)) + 
  geom_bar(stat="identity", position="dodge")+ scale_fill_gradient(low = 'skyblue', high = 'green') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  geom_hline(yintercept = mean(weekdayat23$avg), lty = 2) + 
  geom_hline(yintercept = quantile(weekdayat23$avg, 0.75), lty = 2, color = 'red')+
  geom_hline(yintercept = quantile(weekdayat23$avg, 0.25), lty = 2, color = 'blue')+
  geom_hline(yintercept = quantile(weekdayat23$avg, 0.9), lty = 1, color = 'deeppink')+
  labs(x='Station', y='average # of (board - exit)')

# 주말 전체 역 중 23시에 (타는 사람-내리는 사람)이 많은 top40
weekendat23 <- df %>% filter(weekend == 1) %>% select(stationK, twentythree) %>% 
  group_by(stationK) %>% summarise(avg = mean(twentythree)) %>%
  arrange(desc(avg)) %>% mutate(rank = dense_rank(desc(avg))) %>% top_n(40,avg)

we23 <- ggplot(weekendat23, aes(x=reorder(stationK,-avg), y=avg, fill=avg)) + 
  geom_bar(stat="identity", position="dodge")+ scale_fill_gradient(low = 'skyblue', high = 'green') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  geom_hline(yintercept = mean(weekendat23$avg), lty = 2) + 
  geom_hline(yintercept = quantile(weekendat23$avg, 0.75), lty = 2, color = 'red')+
  geom_hline(yintercept = quantile(weekendat23$avg, 0.25), lty = 2, color = 'blue')+
  geom_hline(yintercept = quantile(weekendat23$avg, 0.9), lty = 1, color = 'deeppink')+
  labs(x='Station', y='average # of (board - exit)')

# 주중과 주말 + 아침 6-7시 타임에 (타는 사람-내리는 사람)수 랭크를 비교하기
a <- weekdayat6[,c(1,3)]; b <- weekendat6[,c(1,3)]; c <- weekdayat7[,c(1,3)]; d <- weekendat7[,c(1,3)]
forties <- data.frame(a,b,c,d); forties
mergedforties <- merge(x =a, y = b, by = "stationK", all = TRUE)
mergedforties <- merge(mergedforties, y = c, by = "stationK", all = TRUE)
mergedforties <- merge(mergedforties, y = d, by = "stationK", all = TRUE)
colnames(mergedforties) <- c('station', 'day6', 'end6', 'day7', 'end7')
mergedforties[,2:5] <- 41-mergedforties[,2:5]
nazero <- mergedforties
nazero[is.na(nazero)] <- 0; nazero
timeforties <- gather(nazero, timing, value, -station)
library(directlabels)
rank40s <- ggplot(timeforties, aes(x=timing, y=value, group = station, color=station)) + geom_line() + geom_point()+
  geom_dl(aes(label = station), method = list(dl.trans(x = x + .2), "last.points",cex = 1,hjust = 1)) +
  geom_dl(aes(label = station), method = list(dl.trans(x = x - .2), "first.points",cex = 1, hjust = 1)) +
  scale_colour_discrete(guide="none")+
  labs(y='rank')
# 하단의 문법은 사용하려면 새로운 패키지를 받아야 하며 tidyverse의 많은 패키지를 잡아먹어서 한 번 쓰고 새로 시작해야함
#library(plotly)
# g<-ggplot(timeforties, aes(x=timing, y=value, group = station, color=station)) + geom_line() + geom_point()+
#   geom_dl(aes(label = station), method = list(dl.trans(x = x + .2), "last.points",cex = 0.8,hjust = 1)) +
#   geom_dl(aes(label = station), method = list(dl.trans(x = x - .2), "first.points",cex = 0.8, hjust = 1)) +
#   scale_colour_discrete(guide="none")
# ggplotly(g, tooltip=c("station"))

ggsave('wd5.png', wd5); ggsave('we5.png', we5); ggsave('wd6.png', wd6); ggsave('we6.png', we6)
ggsave('wd7.png', wd7); ggsave('we7.png', we7); ggsave('wd18.png', wd18); ggsave('we18.png', we18)
ggsave('wd23.png', wd23); ggsave('we23.png', we23); ggsave('wd24.png', wd24)

