library(tidyverse)
library(lubridate)
library(data.table)

setwd('C:/Users/asd/Desktop/데이터 사이언스/월간 데이터 분석/3월호/data')
# read files
df <- fread('2017년_1_9월_일별_역별_시간대별_승하차인원_1_8호선_.csv')
dfcolname <- c('date','month', 'stationN', 'stationK','i/o', 
               'five', 'six', 'seven', 'eight', 'nine', 'ten', 'eleven', 'twelve', 'thirteen', 'forteen',
               'fifteen', 'sixteen', 'seventeen', 'eighteen', 'ninteen', 'twenty', 'twentyone', 
               'twentytwo', 'twentythree', 'after24')  #change colnames and time range is 4-5 ~ 23-24
colnames(df) <- dfcolname
df <- df %>% mutate(stationN = ifelse(stationN >= 1000, (stationN%%1000), (stationN%/%1))) #%>% select(stationN)
df <- df %>% mutate(lane = (stationN%/%100))
df <- df[,c(1,26,3:25)]

# extract station number part
# it was like this: 서울역(150) and I want to change it like this: 서울역, because station number is already in stationN col
tempdf <- df
tempdf <- tempdf %>% separate(stationK, c('stationK', 'stnumbs'))
head(tempdf)
df <- tempdf[-5]

# this data is only from 2017, so doesn't need year part, and dividing month and day column is good for analysis
df$date <- ymd(df$date)
df <- df %>% mutate('weekday' = lubridate::wday(df$date, label = TRUE, abbr = FALSE, week_start = 1)) # insert weekday
df$date <- format(df$date, format = '%m-%d')
df <- df %>% separate(date,c('month','day'))
summary(df) # knew that number of people were written in char and some of them have ',', e.g. 1,329
timedf <- df[,7:26]
timezone <- c(1:20)

timedf[,timezone] <- gsub(',', '',as.matrix(timedf[,timezone]))
timedf[,timezone] <- gsub(',', '',as.matrix(timedf[,timezone]))
timedf[,timezone] <- gsub('\n','',as.matrix(timedf[,timezone]))
timedf[,timezone] <- sapply(timedf[,timezone], as.numeric)
summary(timedf) # confirm it changed as wanted
df[,7:26] <- timedf[,timezone]

for (i in 1:3){ # month, day, lane, station number are better if it is factor type
  df[,i] <- as.factor(df[,i])
}

df <- df %>% mutate(weekend = ifelse(weekday == '토요일'|weekday == '일요일', 1, 0))
df <- df[,c(1:5,27,28,6:26)]
#head(df)
#head(df[,6:26])

odd_row <- seq(1,150154,2)
even_row <- seq(2, 150154, 2)

ndf <- df[1:(nrow(df)/2), -8]
if (all.equal(df$stationN[odd_row], df$stationN[even_row])){
  ndf[,1:7] <- df[odd_row, 1:7]
  ndf[, 8:27] <- df[odd_row, 9:28] - df[even_row, 9:28]
} else stop("Crucial Error!")

df <- ndf
df$weekend <- as.factor(df$weekend)

write.csv(df, file = 'cleansedDf.csv', row.names = FALSE, fileEncoding = "EUC-KR")
#fwrite(df, file = 'cleansedDf2.csv', append = TRUE) encoding problem

