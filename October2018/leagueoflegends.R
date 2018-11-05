library(tidyverse)
library(data.table)
library(stringr)
library(arules)
library(arulesViz)

champions <- fread("leagueoflegends/matchinfo.csv")
champ_col <- colnames(champions)[endsWith(colnames(champions), "Champ")]
champ <- champions %>%
  select(League, Year, Season, champ_col)


blue = champ[,1:8]
colnames(blue)[4:8] <- str_sub(colnames(blue)[4:8], 5)
red = champ[,c(1:3, 9:13)]
colnames(red)[4:8] <- str_sub(colnames(red)[4:8], 4)
tot_champ <- rbind(blue, red) %>% arrange(Year, Season, League)

# games for each year
tot_champ %>%
  group_by(Year) %>%
  summarise(N = n())
#    Year     N
# 1  2014   156
# 2  2015  2992
# 3  2016  4988
# 4  2017  6622
# 5  2018   482

#         15240
############################### How many times each champ picked ###################

top_lane <- tot_champ  %>%
  filter(Year != 2014) %>% # 2014는 너무 옛날이다
  group_by(TopChamp) %>%
  summarise(N = n()) %>%
  filter(N > 30) # 30번보다 많이 나온 챔피언만 보자
tot_champ %>%
  filter(Year != 2014, TopChamp %in% top_lane$TopChamp) %>%
  group_by(TopChamp, Year) %>%
  summarise(cnt = n()) %>%
  left_join(top_lane) %>%
  ggplot(aes(x=reorder(TopChamp, N), y=cnt, fill = factor(Year))) + 
  geom_bar(stat = "identity") + xlab("탑 챔피언") +  ylab("픽 수") + coord_flip() # 가로로 눕히는 것이 더 보기 좋다

jungle_lane <- tot_champ  %>%
  filter(Year != 2014) %>% # 2014는 너무 옛날이다
  group_by(JungleChamp) %>%
  summarise(N = n()) %>%
  filter(N > 30) # 30번보다 많이 나온 챔피언만 보자
tot_champ %>%
  filter(Year != 2014, JungleChamp %in% jungle_lane$JungleChamp) %>%
  group_by(JungleChamp, Year) %>%
  summarise(cnt = n()) %>%
  left_join(jungle_lane) %>%
  ggplot(aes(x=reorder(JungleChamp, N), y=cnt, fill = factor(Year))) + 
  geom_bar(stat = "identity") + xlab("정글 챔피언") +  ylab("픽 수") + coord_flip() # 가로로 눕히는 것이 더 보기 좋다

mid_lane <- tot_champ  %>%
  filter(Year != 2014) %>% # 2014는 너무 옛날이다
  group_by(MiddleChamp) %>%
  summarise(N = n()) %>%
  filter(N > 30) # 30번보다 많이 나온 챔피언만 보자
tot_champ %>%
  filter(Year != 2014, MiddleChamp %in% mid_lane$MiddleChamp) %>%
  group_by(MiddleChamp, Year) %>%
  summarise(cnt = n()) %>%
  left_join(mid_lane) %>%
  ggplot(aes(x=reorder(MiddleChamp, N), y=cnt, fill = factor(Year))) + 
  geom_bar(stat = "identity") + xlab("미드 챔피언") +  ylab("픽 수") + coord_flip() # 가로로 눕히는 것이 더 보기 좋다

ad_lane <- tot_champ  %>%
  filter(Year != 2014) %>% # 2014는 너무 옛날이다
  group_by(ADCChamp) %>%
  summarise(N = n()) %>%
  filter(N > 30) # 30번보다 많이 나온 챔피언만 보자
tot_champ %>%
  filter(Year != 2014, ADCChamp %in% ad_lane$ADCChamp) %>%
  group_by(ADCChamp, Year) %>%
  summarise(cnt = n()) %>%
  left_join(ad_lane) %>%
  ggplot(aes(x=reorder(ADCChamp, N), y=cnt, fill = factor(Year))) + 
  geom_bar(stat = "identity") + xlab("원딜 챔피언") +  ylab("픽 수") + coord_flip() # 가로로 눕히는 것이 더 보기 좋다

spt_lane <- tot_champ  %>%
  filter(Year != 2014) %>% # 2014는 너무 옛날이다
  group_by(SupportChamp) %>%
  summarise(N = n()) %>%
  filter(N > 30) # 30번보다 많이 나온 챔피언만 보자
tot_champ %>%
  filter(Year != 2014, SupportChamp %in% spt_lane$SupportChamp) %>%
  group_by(SupportChamp, Year) %>%
  summarise(cnt = n()) %>%
  left_join(spt_lane) %>%
  ggplot(aes(x=reorder(SupportChamp, N), y=cnt, fill = factor(Year))) + 
  geom_bar(stat = "identity") + xlab("서포터 챔피언") +  ylab("픽 수") + coord_flip() # 가로로 눕히는 것이 더 보기 좋다


################# Top 5 For each year ########################

# top_5 
tot_champ  %>%
  group_by(Year, TopChamp) %>%
  summarise(N = n()) %>%
  top_n(5) %>% # top 5
  ggplot(aes(TopChamp, N, fill = factor(Year))) + geom_bar(stat = "identity", position = "dodge") + coord_flip() +
  xlab("연도별 픽 된 탑 챔피언 탑 5") + ylab("픽 횟수") 

# jungle_5 
tot_champ  %>%
  group_by(Year, JungleChamp) %>%
  summarise(N = n()) %>%
  top_n(5) %>% # top 5
  ggplot(aes(JungleChamp, N, fill = factor(Year))) + geom_bar(stat = "identity", position = "dodge") +
  xlab("연도별 픽 된 정글 챔피언 탑 5") + ylab("픽 횟수")

# mid_5
tot_champ  %>%
  group_by(Year, MiddleChamp) %>%
  summarise(N = n()) %>%
  top_n(5) %>% # top 5
  ggplot(aes(MiddleChamp, N, fill = factor(Year))) + geom_bar(stat = "identity", position = "dodge") +
  xlab("연도별 픽 된 미드 챔피언 탑 5") + ylab("픽 횟수")

# ad_5 
tot_champ  %>%
  group_by(Year, ADCChamp) %>%
  summarise(N = n()) %>%
  top_n(5) %>% # top 5
  ggplot(aes(ADCChamp, N, fill = factor(Year))) + geom_bar(stat = "identity", position = "dodge") +
  xlab("연도별 픽 된 원딜 챔피언 탑 5") + ylab("픽 횟수")

# spt_5
tot_champ  %>%
  group_by(Year, SupportChamp) %>%
  summarise(N = n()) %>%
  top_n(5) %>% # top 5
  ggplot(aes(SupportChamp, N, fill = factor(Year))) + geom_bar(stat = "identity", position = "dodge") +
  xlab("연도별 픽 된 서포터 챔피언 탑 5") + ylab("픽 횟수")


##################### bans ###########################

bans <- fread("leagueoflegends/bans.csv")
bans <- bans[,3:7]
bans <- bind_cols(Year = tot_champ$Year, bans)
bans <- data.frame(bans)
bans[bans==""] <- NA
total_ban <- bans %>%
  gather(ban_i, banned_champ, ban_1:ban_5, na.rm = TRUE) %>%
  filter(Year != 2014) %>%
  group_by(Year, banned_champ) %>%
  summarise(N = n()) %>%
  top_n(10, N) %>%
  group_by(banned_champ) %>%
  summarise(cnt = sum(N))
bans %>%
  gather(ban_i, banned_champ, ban_1:ban_5, na.rm = TRUE) %>%
  filter(Year != 2014) %>%
  group_by(Year, banned_champ) %>%
  summarise(N = n()) %>%
  top_n(10, N) %>%
  left_join(total_ban) %>%
  ggplot(aes(reorder(banned_champ, cnt), N, fill = factor(Year))) + geom_bar(stat = "identity", position = "dodge") + coord_flip() +
  xlab("연도별 밴 된 챔피언 탑 10") + ylab("밴 횟수")

##################### association rules ###################

played_champ <- tot_champ %>%
  filter(Year != 2014) %>%
  select(TopChamp:SupportChamp) %>% 
  mutate(game = row_number())
pld_champ <- as.data.frame(sapply(played_champ, factor))
champ_transaction <- as(pld_champ, "transactions")
itemFrequencyPlot(champ_transaction, topN = 20) # top20 most frequentry picked champions
# ecl <- eclat(champ_transaction, parameter=list(supp=30/15240,minlen=2 ,maxlen=5)); ecl
# inspect(sort(ecl)[1:10]) # 엘리스-브라움, 렉사이-브라움 같은 이상한 조합이 상위권인 이유는 애초에 많이 픽 되어서인 것 같다
# 애쉬-자이라, 자야-라칸, 코르키(미드)-브라움, 트런들-엘리스 조합은 생각해볼만한 발견인 듯
summary(ecl)
champ_rule <- apriori(champ_transaction, parameter = list(supp = 30/15240, conf = 0.2, minlen=2, maxlen=5))
plot(sort(champ_rule))
plot(sort(champ_rule)[1:30], method="graph", engine = "interactive", shading=NA)
plot(sort(champ_rule, by = "confidence")[1:40], method="graph", engine = "interactive", measure = "confidence", shading="lift")
options(digits=2)
inspect(sort(champ_rule)[1:10])
inspect(sort(champ_rule, by = "confidence")[1:40])
inspect(sort(champ_rule, by = "lift")[1:40])


# missfortune.sub <- subset(champ_rule, lhs %pin% c("ADCChamp=MissFortune"))
# inspect(missfortune.sub)
# missfortune.sub2 <- subset(champ_rule, rhs %pin% c("ADCChamp=MissFortune"))
# inspect(missfortune.sub2)
jhin.sub <- subset(champ_rule, lhs %pin% c("ADCChamp=Jhin"))
inspect(jhin.sub)
jhin.sub2 <- subset(champ_rule, rhs %pin% c("ADCChamp=Jhin"))
inspect(jhin.sub2)
lucian.sub <- subset(champ_rule, lhs %pin% c("ADCChamp=Lucian"))
inspect(lucian.sub)
lucian.sub2 <- subset(champ_rule, rhs %pin% c("ADCChamp=Lucian"))
inspect(lucian.sub2)

####################### 한 챔피언끼리만 보자 #########################

played_champ1 <- tot_champ %>%
  filter(Year != 2014) %>%
  select(TopChamp:SupportChamp) %>% 
  mutate(game = row_number())
pld_champ1 <- as.data.frame(sapply(played_champ1, factor))
champ_transaction1 <- as(pld_champ1, "transactions")
itemFrequencyPlot(champ_transaction1, topN = 20) # top20 most frequentry picked champions
ecl1 <- eclat(champ_transaction1, parameter=list(supp=30/15240,minlen=2 ,maxlen=2)); ecl
inspect(sort(ecl1)[1:10]) # 엘리스-브라움, 렉사이-브라움 같은 이상한 조합이 상위권인 이유는 애초에 많이 픽 되어서인 것 같다
# 애쉬-자이라, 자야-라칸, 코르키(미드)-브라움, 트런들-엘리스 조합은 생각해볼만한 발견인 듯
summary(ecl1)
champ_rule1 <- apriori(champ_transaction1, parameter = list(supp = 30/15240, conf = 0.2, minlen=2, maxlen=2))
plot(champ_rule1)
plot(sort(champ_rule1, by = "confidence")[1:40], method="graph", engine = "interactive", measure = "confidence", shading="lift")
inspect(sort(champ_rule1[1:40]))
inspect(sort(champ_rule1, by = "confidence")[1:40])
inspect(sort(champ_rule1, by = "lift")[1:40])


########################### 주챔프를 넣어보자 #############################

missfortune1 <- apriori(champ_transaction1, parameter = list(supp = 10/15240, conf = 0.1, minlen=2, maxlen=2),
                       appearance = list(default = "rhs", lhs = "ADCChamp=MissFortune"))
inspect(sort(missfortune1))

missfortune11 <- apriori(champ_transaction1, parameter = list(supp = 10/15240, conf = 0.1, minlen=2, maxlen=2),
                        appearance = list(default = "lhs", rhs = "ADCChamp=MissFortune"))
inspect(sort(missfortune11)) # nothing

jhin1 <- apriori(champ_transaction1, parameter = list(supp = 10/15240, conf = 0.1, minlen=2, maxlen=2),
                appearance = list(default = "rhs", lhs = "ADCChamp=Jhin"))
inspect(sort(jhin1))

jhin11 <- apriori(champ_transaction1, parameter = list(supp = 10/15240, conf = 0.1, minlen=2, maxlen=2),
                 appearance = list(default = "lhs", rhs = "ADCChamp=Jhin"))
inspect(sort(jhin11, by = "lift")[1:20])

lucian1 <- apriori(champ_transaction1, parameter = list(supp = 10/15240, conf = 0.1, minlen=2, maxlen=2),
                  appearance = list(default = "rhs", lhs = "ADCChamp=Lucian"))
inspect(sort(lucian1))

lucian11 <- apriori(champ_transaction1, parameter = list(supp = 10/15240, conf = 0.1, minlen=2, maxlen=2),
                   appearance = list(default = "lhs", rhs = "ADCChamp=Lucian"))
inspect(sort(lucian11, by = "lift")[1:20])
