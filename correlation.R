library(tidyverse)


# reverse coding
Time$game <- 8-Time$game
Time$sports <- 8-Time$sports
Time$music<- 8-Time$music
Time$traffic <- 8-Time$traffic
Time$dish <- 8-Time$dish
Time$vacuum <- 8-Time$vacuum
# 0 = never, 8 = daily

Time[which(Time$game == 1), c("game")] <- NA
Time[which(Time$sports == 1), c("sports")] <- NA
Time[which(Time$music == 1), c("music")] <- NA
Time[which(Time$traffic == 1), c("traffic")] <- NA
Time[which(Time$dish == 1), c("dish")] <- NA
Time[which(Time$vacuum == 1), c("vacuum")] <- NA
# never = NA


Time[which(Time$gamehr == 8), c("gamehr")] <- NA
Time[which(Time$sportshr == 8), c("sportshr")] <- NA
Time[which(Time$musichr == 8), c("musichr")] <- NA
Time[which(Time$traffichr == 8), c("traffichr")] <- NA
Time[which(Time$dishhr == 8), c("dishhr")] <- NA
Time[which(Time$vacuumhr == 8), c("vacuumhr")] <- NA
# 1 = less than 15 mins, 7 = more than 90 mins, NA = mot applicable

# wtp
lm(Time$gp ~ Time$game + Time$gamehr) %>% summary()
lm(Time$sp ~ Time$sports + Time$sportshr) %>% summary()
lm(Time$mp ~ Time$music + Time$musichr) %>% summary()
lm(Time$tn ~ Time$traffic + Time$traffichr) %>% summary()
lm(Time$vn ~ Time$vacuum + Time$vacuumhr) %>% summary()
lm(Time$dn ~ Time$dish + Time$dishhr) %>% summary()

# evaluation
lm(Time$g ~ Time$game + Time$gamehr) %>% summary()
lm(Time$s ~ Time$sports + Time$sportshr) %>% summary()
lm(Time$m ~ Time$music + Time$musichr) %>% summary()
lm(Time$t ~ Time$traffic + Time$traffichr) %>% summary()
lm(Time$v ~ Time$vacuum + Time$vacuumhr) %>% summary()
lm(Time$d ~ Time$dish + Time$dishhr) %>% summary()
