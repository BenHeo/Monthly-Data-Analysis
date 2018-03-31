library(tidyverse)
library(lubridate)
library(data.table)

setwd('C:/Users/asd/Desktop/데이터 사이언스/월간 데이터 분석/3월호/data')
# read files
df <- fread('cleansedDf.csv', header = TRUE)
head(df)

# 주중 전체 역 중 6시에 (타는 사람-내리는 사람)이 많은 top40
weekdayat6 <- df %>% filter(weekend == 0) %>% select(stationK, six) %>% 
  group_by(stationK) %>% summarise(avg=mean(six)) %>%
  arrange(desc(avg)) %>% mutate(rank = dense_rank(desc(avg))) %>% top_n(40,avg)

ggplot(weekdayat6, aes(x=reorder(stationK,-avg), y=avg, fill=avg)) + 
  geom_bar(stat="identity", position="dodge")+ scale_fill_gradient(low = 'skyblue', high = 'green') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  geom_hline(yintercept = mean(weekdayat6$avg), lty = 2) + 
  geom_hline(yintercept = quantile(weekdayat6$avg, 0.75), lty = 2, color = 'red')+
  geom_hline(yintercept = quantile(weekdayat6$avg, 0.25), lty = 2, color = 'blue')+
  geom_hline(yintercept = quantile(weekdayat6$avg, 0.9), lty = 1, color = 'deeppink')

# 주말 전체 역 중 6시에 (타는 사람-내리는 사람)이 많은 top40
weekendat6 <- df %>% filter(weekend == 1) %>% select(stationK, six) %>% 
  group_by(stationK) %>% summarise(avg=mean(six)) %>%
  arrange(desc(avg)) %>% mutate(rank = dense_rank(desc(avg))) %>% top_n(40,avg)

ggplot(weekendat6, aes(x=reorder(stationK,-avg), y=avg, fill=avg)) + 
  geom_bar(stat="identity", position="dodge")+ scale_fill_gradient(low = 'skyblue', high = 'green') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  geom_hline(yintercept = mean(weekendat6$avg), lty = 2) + 
  geom_hline(yintercept = quantile(weekendat6$avg, 0.75), lty = 2, color = 'red')+
  geom_hline(yintercept = quantile(weekendat6$avg, 0.25), lty = 2, color = 'blue')+
  geom_hline(yintercept = quantile(weekendat6$avg, 0.9), lty = 1, color = 'deeppink')


# 주중 전체 역 중 7시에 (타는 사람-내리는 사람)이 많은 top40
weekdayat7 <- df %>% filter(weekend == 0) %>% select(stationK, seven) %>% 
  group_by(stationK) %>% summarise(avg = mean(seven)) %>%
  arrange(desc(avg)) %>% mutate(rank = dense_rank(desc(avg))) %>% top_n(40,avg)

ggplot(weekdayat7, aes(x=reorder(stationK,-avg), y=avg, fill=avg)) + 
  geom_bar(stat="identity", position="dodge")+ scale_fill_gradient(low = 'skyblue', high = 'green') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  geom_hline(yintercept = mean(weekdayat7$avg), lty = 2) + 
  geom_hline(yintercept = quantile(weekdayat7$avg, 0.75), lty = 2, color = 'red')+
  geom_hline(yintercept = quantile(weekdayat7$avg, 0.25), lty = 2, color = 'blue')+
  geom_hline(yintercept = quantile(weekdayat7$avg, 0.9), lty = 1, color = 'deeppink')


# 주말 전체 역 중 7시에 (타는 사람-내리는 사람)이 많은 top40
weekendat7 <- df %>% filter(weekend == 0) %>% select(stationK, seven) %>% 
  group_by(stationK) %>% summarise(avg = mean(seven)) %>%
  arrange(desc(avg)) %>% mutate(rank = dense_rank(desc(avg))) %>% top_n(40,avg)

ggplot(weekendat7, aes(x=reorder(stationK,-avg), y=avg, fill=avg)) + 
  geom_bar(stat="identity", position="dodge")+ scale_fill_gradient(low = 'skyblue', high = 'green') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  geom_hline(yintercept = mean(weekendat7$avg), lty = 2) + 
  geom_hline(yintercept = quantile(weekendat7$avg, 0.75), lty = 2, color = 'red')+
  geom_hline(yintercept = quantile(weekendat7$avg, 0.25), lty = 2, color = 'blue')+
  geom_hline(yintercept = quantile(weekendat7$avg, 0.9), lty = 1, color = 'deeppink')

a <- weekdayat6[,c(1,3)]
b <- weekendat6[,c(1,3)]
c <- weekdayat7[,c(1,3)]
d <- weekendat7[,c(1,3)]
forties <- data.frame(a,b,c,d); forties
mergedforties <- merge(x =a, y = b, by = "stationK", all = TRUE)
mergedforties <- merge(mergedforties, y = c, by = "stationK", all = TRUE)
mergedforties <- merge(mergedforties, y = d, by = "stationK", all = TRUE)
colnames(mergedforties) <- c('station', 'day6', 'end6', 'day7', 'end7')
mergedforties[,2:5] <- 41-mergedforties[,2:5]
mergedforties
nazero <- mergedforties
nazero[is.na(nazero)] <- 0; nazero
timeforties <- gather(nazero, timing, value, -station)
#library(directlabels)
ggplot(timeforties, aes(x=timing, y=value, group = station, color=station)) + geom_line() + geom_point()+
  geom_dl(aes(label = station), method = list(dl.trans(x = x + .2), "last.points",cex = 0.8,hjust = 1)) +
  geom_dl(aes(label = station), method = list(dl.trans(x = x - .2), "first.points",cex = 0.8, hjust = 1)) +
  scale_colour_discrete(guide="none")
#library(plotly)
g<-ggplot(timeforties, aes(x=timing, y=value, group = station, color=station)) + geom_line() + geom_point()+
  geom_dl(aes(label = station), method = list(dl.trans(x = x + .2), "last.points",cex = 0.8,hjust = 1)) +
  geom_dl(aes(label = station), method = list(dl.trans(x = x - .2), "first.points",cex = 0.8, hjust = 1)) +
  scale_colour_discrete(guide="none")
ggplotly(g, tooltip=c("station"))

# 목표1: 5호선 타고 어딘가를 갈 때 -주로 학교나 핫플- 언제 타는 것이 덜 막힐까
# 예상 과정: 주로 5호선 특히 우장산역 오기 전 타는 사람들의 수, 
#           동대문역사문화공원 오기 전 타는 사람들의 수 등을 고려할 것 같다. 이 때 지하철 칸마다 유니폼하게 탄다고 가정한다.

# 목표2: 주말과 주중에 시간대별 분석을 진행한다. 또한 전체적으로 어느 역이 가장 유동인구가 많은지 등을 분석해 보고 강남 등 흔히 생각할 수 있는 곳인지 아닌 곳이 있는지를 본다.
#        출퇴근 시간대에는 대학가, 회사가 쪽에 유동인구가 많을 것은 당연하지만 그들이 어디서부터 몇 분 거리부터 많아지는지 물어볼만하다.

# 목표3: 24시 이후 늦은 시간에 타는 사람들이 많은 곳은 어디이며, 5시에 타는 사람이 많은 곳은 어디일까?

# 목표4: 월별 총 승객수에 차이가 있는지

# 517 ~ 537  우장산 ~ 동대문역사문화공원station number
# 422 ~ 420  동대문역사문화공원 ~ 혜화station number
# 한 칸당 usable 좌석수 40, 총 9칸 => 360칸, 한 시간당 지하철 수 대5시대는 첫차라 3대, 6시대 11개, 7시대 19대
