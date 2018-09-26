library(tidyverse)
library(lubridate)
library(data.table)

setwd('C:/Users/asd/Desktop/데이터 사이언스/Monthly Data Analysis/월간 데이터 분석/March2018/data')
# read files
df <- fread('cleansedDf.csv', header = TRUE)
head(df)

# 목표1: 5호선 타고 어딘가를 갈 때 -주로 학교나 핫플- 언제 타는 것이 덜 막힐까
# 예상 과정: 주로 5호선 특히 우장산역 오기 전 타는 사람들의 수, 
#           동대문역사문화공원 오기 전 타는 사람들의 수 등을 고려할 것 같다. 이 때 지하철 칸마다 유니폼하게 탄다고 가정한다.

# 511 ~ 516 방화 ~ 발산
# 517 ~ 537  우장산 ~ 동대문역사문화공원station number
# 422 ~ 420  동대문역사문화공원 ~ 혜화station number
# 한 칸당 usable 좌석수 40, 총 9칸 => 360칸, 한 시간당 지하철 수 대5시대는 첫차라 3대, 6시대 11개, 7시대 19대

# for my house 
# When the subway get into ujangsan station, how many people would be there? So need to use cumsum
target <- 511:516
target_stations <- df %>% filter(weekend == 0) %>% select(stationN, stationK, five:after24) %>% 
  group_by(stationN, stationK) %>% summarise_all(funs(mean)) %>% filter(stationN %in% target)
target_station2 <- target_stations %>% select(stationN, stationK, seven)
at_five_with_cum <- bind_cols(target_station2, cum_seven = cumsum(target_station2$seven))
usn<-ggplot(at_five_with_cum, aes(x=stationN, y=cum_seven)) + geom_line(size=1, color='#996CAC') +
  geom_area(position='identity', fill='#996CAC') + geom_point(size = 4, shape=10)
#ggsave('usn.png', usn) # colors are the same as color of each lane

target <- 511:517
target_stations <- df %>% filter(weekend == 0) %>% select(stationN, stationK, five:after24) %>% 
  group_by(stationN, stationK) %>% summarise_all(funs(mean)) %>% filter(stationN %in% target)
target_station2 <- target_stations %>% select(stationN, stationK, seven)
at_five_with_cum <- bind_cols(target_station2, cum_seven = cumsum(target_station2$seven))
usn2<-ggplot(at_five_with_cum, aes(x=stationN, y=cum_seven)) + geom_line(size=1, color='#996CAC') +
  geom_area(position='identity', fill='#996CAC') + geom_point(size = 4, shape=10)
#ggsave('usn2.png', usn2)

# for lane 1 users
target <- 101:177
target_stations <- df %>% filter(weekend == 0) %>% select(stationN, stationK, five:after24) %>% 
  group_by(stationN, stationK) %>% summarise_all(funs(mean)) %>% filter(stationN %in% target)
target_station2 <- target_stations %>% select(stationN, stationK, seven)
lane1 <- ggplot(target_station2, aes(x=stationN, y=seven)) + geom_line(size=1, color='#0052A4') +
  geom_area(position='identity', fill='#0052A4') + geom_point(size = 1, shape=10)
#ggsave('lane1.png', lane1)

# for lane 2 users
target <- 200:271
target_stations <- df %>% filter(weekend == 0) %>% select(stationN, stationK, five:after24) %>% 
  group_by(stationN, stationK) %>% summarise_all(funs(mean)) %>% filter(stationN %in% target)
target_station2 <- target_stations %>% select(stationN, stationK, seven)
lane2 <- ggplot(target_station2, aes(x=stationN, y=seven)) + geom_line(size=1, color='#009D3E') +
  geom_area(position='identity', fill='#009D3E') + geom_point(size = 1, shape=10)
#ggsave('lane2.png', lane2)

# for lane 3 users
target <- 300:370
target_stations <- df %>% filter(weekend == 0) %>% select(stationN, stationK, five:after24) %>% 
  group_by(stationN, stationK) %>% summarise_all(funs(mean)) %>% filter(stationN %in% target)
target_station2 <- target_stations %>% select(stationN, stationK, seven)
lane3 <- ggplot(target_station2, aes(x=stationN, y=seven)) + geom_line(size=1, color='#EF7C1C') +
  geom_area(position='identity', fill='#EF7C1C') + geom_point(size = 1, shape=10)
#ggsave('lane3.png', lane3)

# for lane 4 users
target <- 400:470
target_stations <- df %>% filter(weekend == 0) %>% select(stationN, stationK, five:after24) %>% 
  group_by(stationN, stationK) %>% summarise_all(funs(mean)) %>% filter(stationN %in% target)
target_station2 <- target_stations %>% select(stationN, stationK, seven)
lane4 <- ggplot(target_station2, aes(x=stationN, y=seven)) + geom_line(size=1, color='#00A5DE') +
  geom_area(position='identity', fill='#00A5DE') + geom_point(size = 1, shape=10)
#ggsave('lane4.png', lane4)

# for lane 5 users
target <- 511:561
target_stations <- df %>% filter(weekend == 0) %>% select(stationN, stationK, five:after24) %>% 
  group_by(stationN, stationK) %>% summarise_all(funs(mean)) %>% filter(stationN %in% target)
target_station2 <- target_stations %>% select(stationN, stationK, seven)
lane5 <- ggplot(target_station2, aes(x=stationN, y=seven)) + geom_line(size=1, color='#996CAC') +
  geom_area(position='identity', fill='#996CAC') + geom_point(size = 1, shape=10)
#ggsave('lane5.png', lane5)

# for lane 6 users
target <- 600:671
target_stations <- df %>% filter(weekend == 0) %>% select(stationN, stationK, five:after24) %>% 
  group_by(stationN, stationK) %>% summarise_all(funs(mean)) %>% filter(stationN %in% target)
target_station2 <- target_stations %>% select(stationN, stationK, seven)
lane6 <- ggplot(target_station2, aes(x=stationN, y=seven)) + geom_line(size=1, color='#CD7C2F') +
  geom_area(position='identity', fill='#CD7C2F') + geom_point(size = 1, shape=10)
#ggsave('lane6.png', lane6)

# for lane 7 users
target <- 701:771
target_stations <- df %>% filter(weekend == 0) %>% select(stationN, stationK, five:after24) %>% 
  group_by(stationN, stationK) %>% summarise_all(funs(mean)) %>% filter(stationN %in% target)
target_station2 <- target_stations %>% select(stationN, stationK, seven)
lane7 <- ggplot(target_station2, aes(x=stationN, y=seven)) + geom_line(size=1, color='#747F00') +
  geom_area(position='identity', fill='#747F00') + geom_point(size = 1, shape=10)
#ggsave('lane7.png', lane7)

# for lane 8 users
target <- 801:888
target_stations <- df %>% filter(weekend == 0) %>% select(stationN, stationK, five:after24) %>% 
  group_by(stationN, stationK) %>% summarise_all(funs(mean)) %>% filter(stationN %in% target)
target_station2 <- target_stations %>% select(stationN, stationK, seven)
lane8 <- ggplot(target_station2, aes(x=stationN, y=seven)) + geom_line(size=1, color='#EA545D') +
  geom_area(position='identity', fill='#EA545D') + geom_point(size = 1, shape=10)
#ggsave('lane8.png', lane8)
