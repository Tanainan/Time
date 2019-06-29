library(tidyverse)
library(sjstats)
library(Hmisc)
library(ggpubr)

#game wtp and eva
ggp <- data.frame(t = Time$gn)
ggo <- data.frame(t = Time$gp)

ggo$t[is.na(ggo$t)] <- ggp$t[is.na(ggo$t)]


# sports wtp and eva
ssp <- data.frame(t = Time$sn)
sso <- data.frame(t = Time$sp)
ssm <- data.frame(t = Time$sneun)
ssn <- data.frame(t = Time$sneup)

sso$t[is.na(sso$t)] <- ssp$t[is.na(sso$t)]
sso$t[is.na(sso$t)] <- ssm$t[is.na(sso$t)]
sso$t[is.na(sso$t)] <- ssn$t[is.na(sso$t)]


#music wtp and eva
mmp <- data.frame(t = Time$mn)
mmo <- data.frame(t = Time$mp)

mmo$t[is.na(mmo$t)] <- mmp$t[is.na(mmo$t)]

#traffic wtp and eva
ttp <- data.frame(t = Time$tn)
tto <- data.frame(t = Time$tp)
ttm <- data.frame(t = Time$tneun)
ttn <- data.frame(t = Time$tneup)

tto$t[is.na(tto$t)] <- ttp$t[is.na(tto$t)]
tto$t[is.na(tto$t)] <- ttm$t[is.na(tto$t)]
tto$t[is.na(tto$t)] <- ttn$t[is.na(tto$t)]

#dishes wtp and eva
ddp <- data.frame(t = Time$dn)
ddo <- data.frame(t = Time$dp)
ddm <- data.frame(t = Time$dneun)
ddn <- data.frame(t = Time$dneup)

ddo$t[is.na(ddo$t)] <- ddp$t[is.na(ddo$t)]
ddo$t[is.na(ddo$t)] <- ddm$t[is.na(ddo$t)]
ddo$t[is.na(ddo$t)] <- ddn$t[is.na(ddo$t)]


# vacuum wtp and eva
vvp <- data.frame(t = Time$vn)
vvo <- data.frame(t = Time$vp)
vvm <- data.frame(t = Time$vneun)
vvn <- data.frame(t = Time$vneup)

vvo$t[is.na(vvo$t)] <- vvp$t[is.na(vvo$t)]
vvo$t[is.na(vvo$t)] <- vvm$t[is.na(vvo$t)]
vvo$t[is.na(vvo$t)] <- vvn$t[is.na(vvo$t)]



#### reverse coding

# daily = 7
# 4-6 times a week = 6
# 2-3 times a week = 5
# once a week = 4
# 2-3 times a month = 3
# once a month = 2
# less than once a month = 1
# never = 0

Time$game <- 8-Time$game
Time$sports <- 8-Time$sports
Time$music<- 8-Time$music
Time$traffic <- 8-Time$traffic
Time$dish <- 8-Time$dish
Time$vacuum <- 8-Time$vacuum


# less than 15 minutes = 1
# between 15-30 minutes = 2
# between 30-45 minutes = 3
# between 45-60 minutes = 4
# between 60-75 minutes = 5
# between 75-90 minutes = 6
# more than 90 minutes = 7
# not applicable = NA

# Time[which(Time$gamehr == 8), c("gamehr")] <- NA
# Time[which(Time$sportshr == 8), c("sportshr")] <- NA
# Time[which(Time$musichr == 8), c("musichr")] <- NA
# Time[which(Time$traffichr == 8), c("traffichr")] <- NA
# Time[which(Time$dishhr == 8), c("dishhr")] <- NA
# Time[which(Time$vacuumhr == 8), c("vacuumhr")] <- NA


# create new data set for frequency
freqg <- (Time[,c("g","game")])
freqs <- (Time[,c("s","sports")])
freqm <- (Time[,c("m","music")])
freqd <- (Time[,c("d","dish")])
freqt <- (Time[,c("t","traffic")])
freqv <- (Time[,c("v","vacuum")])

# create new data set for duration
freqghr1 <- (Time[,c("g","gamehr", "game")])
freqshr1 <- (Time[,c("s","sportshr", "sports")])
freqmhr1 <- (Time[,c("m","musichr", "music")])
freqdhr1 <- (Time[,c("d","dishhr", "dish")])
freqthr1 <- (Time[,c("t","traffichr", "traffic")])
freqvhr1 <- (Time[,c("v","vacuumhr", "vacuum")])


# add wtp column
freqg$gw <- ggo$t
freqs$sw <- sso$t
freqm$mw <- mmo$t 
freqd$dw <- ddo$t
freqt$tw <- tto$t
freqv$vw <- vvo$t
freqghr1$gw <- ggo$t
freqshr1$sw <- sso$t
freqmhr1$mw <- mmo$t 
freqdhr1$dw <- ddo$t
freqthr1$tw <- tto$t
freqvhr1$vw <- vvo$t


freqg1 <- data.frame(freqg)
freqs1 <- data.frame(freqs)
freqm1 <- data.frame(freqm)
freqd1 <- data.frame(freqd)
freqt1 <- data.frame(freqt)
freqv1 <- data.frame(freqv)

freqghr <- data.frame(freqghr1)
freqshr <- data.frame(freqshr1)
freqmhr <- data.frame(freqmhr1)
freqdhr <- data.frame(freqdhr1)
freqthr <- data.frame(freqthr1)
freqvhr <- data.frame(freqvhr1)

freqg1$game <- freqg1$game + 1
freqs1$sports <- freqs1$sports + 1
freqm1$music <- freqm1$music + 1
freqd1$dish <- freqd1$dish + 1
freqt1$traffic <- freqt1$traffic + 1
freqv1$vacuum <- freqv1$vacuum + 1


freqghr[which(freqghr$gamehr == 8), c("gamehr")] <- NA
freqshr[which(freqshr$sportshr == 8), c("sportshr")] <- NA
freqmhr[which(freqmhr$musichr == 8), c("musichr")] <- NA
freqdhr[which(freqdhr$dishhr == 8), c("dishhr")] <- NA
freqthr[which(freqthr$traffichr == 8), c("traffichr")] <- NA
freqvhr[which(freqvhr$vacuumhr == 8), c("vacuumhr")] <- NA

# omit NA for duration
freqghr <- na.omit(freqghr)
freqshr <- na.omit(freqshr)
freqmhr <- na.omit(freqmhr)
freqdhr <- na.omit(freqdhr)
freqthr <- na.omit(freqthr)
freqvhr <- na.omit(freqvhr)


# create dummy variables
# game ref: 0 = never
# gamehr ref: 1 = less than 15 mins
freqg$game1 <- NA
freqg$game2 <- NA
freqg$game3 <- NA
freqg$game4 <- NA
freqg$game5 <- NA
freqg$game6 <- NA
freqg$game7 <- NA
for (i in 1:nrow(freqg)){
  if (freqg$game[i] == 1) {freqg$game1[i] <- 1} else {freqg$game1[i] <- 0}
  if (freqg$game[i] == 2) {freqg$game2[i] <- 1} else {freqg$game2[i] <- 0}
  if (freqg$game[i] == 3) {freqg$game3[i] <- 1} else {freqg$game3[i] <- 0}
  if (freqg$game[i] == 4) {freqg$game4[i] <- 1} else {freqg$game4[i] <- 0}
  if (freqg$game[i] == 5) {freqg$game5[i] <- 1} else {freqg$game5[i] <- 0}
  if (freqg$game[i] == 6) {freqg$game6[i] <- 1} else {freqg$game6[i] <- 0}
  if (freqg$game[i] == 7) {freqg$game7[i] <- 1} else {freqg$game7[i] <- 0}
}
freqghr$gamehr1 <- NA
freqghr$gamehr2 <- NA
freqghr$gamehr3 <- NA
freqghr$gamehr4 <- NA
freqghr$gamehr5 <- NA
freqghr$gamehr6 <- NA
for (i in 1:nrow(freqghr)){
  if (freqghr$gamehr[i] == 1) {freqghr$gamehr1[i] <- 1} else {freqghr$gamehr1[i] <- 0}
  if (freqghr$gamehr[i] == 2) {freqghr$gamehr2[i] <- 1} else {freqghr$gamehr2[i] <- 0}
  if (freqghr$gamehr[i] == 3) {freqghr$gamehr3[i] <- 1} else {freqghr$gamehr3[i] <- 0}
  if (freqghr$gamehr[i] == 4) {freqghr$gamehr4[i] <- 1} else {freqghr$gamehr4[i] <- 0}
  if (freqghr$gamehr[i] == 5) {freqghr$gamehr5[i] <- 1} else {freqghr$gamehr5[i] <- 0}
  if (freqghr$gamehr[i] == 6) {freqghr$gamehr6[i] <- 1} else {freqghr$gamehr6[i] <- 0}
}


# sports
freqs$sports1 <- NA
freqs$sports2 <- NA
freqs$sports3 <- NA
freqs$sports4 <- NA
freqs$sports5 <- NA
freqs$sports6 <- NA
freqs$sports7 <- NA
for (i in 1:nrow(freqs)){
  if (freqs$sports[i] == 1) {freqs$sports1[i] <- 1} else {freqs$sports1[i] <- 0}
  if (freqs$sports[i] == 2) {freqs$sports2[i] <- 1} else {freqs$sports2[i] <- 0}
  if (freqs$sports[i] == 3) {freqs$sports3[i] <- 1} else {freqs$sports3[i] <- 0}
  if (freqs$sports[i] == 4) {freqs$sports4[i] <- 1} else {freqs$sports4[i] <- 0}
  if (freqs$sports[i] == 5) {freqs$sports5[i] <- 1} else {freqs$sports5[i] <- 0}
  if (freqs$sports[i] == 6) {freqs$sports6[i] <- 1} else {freqs$sports6[i] <- 0}
  if (freqs$sports[i] == 7) {freqs$sports7[i] <- 1} else {freqs$sports7[i] <- 0}
}
freqshr$sportshr1 <- NA
freqshr$sportshr2 <- NA
freqshr$sportshr3 <- NA
freqshr$sportshr4 <- NA
freqshr$sportshr5 <- NA
freqshr$sportshr6 <- NA
for (i in 1:nrow(freqshr)){
  if (freqshr$sportshr[i] == 1) {freqshr$sportshr1[i] <- 1} else {freqshr$sportshr1[i] <- 0}
  if (freqshr$sportshr[i] == 2) {freqshr$sportshr2[i] <- 1} else {freqshr$sportshr2[i] <- 0}
  if (freqshr$sportshr[i] == 3) {freqshr$sportshr3[i] <- 1} else {freqshr$sportshr3[i] <- 0}
  if (freqshr$sportshr[i] == 4) {freqshr$sportshr4[i] <- 1} else {freqshr$sportshr4[i] <- 0}
  if (freqshr$sportshr[i] == 5) {freqshr$sportshr5[i] <- 1} else {freqshr$sportshr5[i] <- 0}
  if (freqshr$sportshr[i] == 6) {freqshr$sportshr6[i] <- 1} else {freqshr$sportshr6[i] <- 0}
}

# music
freqm$music1 <- NA
freqm$music2 <- NA
freqm$music3 <- NA
freqm$music4 <- NA
freqm$music5 <- NA
freqm$music6 <- NA
freqm$music7 <- NA
for (i in 1:nrow(freqm)){
  if (freqm$music[i] == 1) {freqm$music1[i] <- 1} else {freqm$music1[i] <- 0}
  if (freqm$music[i] == 2) {freqm$music2[i] <- 1} else {freqm$music2[i] <- 0}
  if (freqm$music[i] == 3) {freqm$music3[i] <- 1} else {freqm$music3[i] <- 0}
  if (freqm$music[i] == 4) {freqm$music4[i] <- 1} else {freqm$music4[i] <- 0}
  if (freqm$music[i] == 5) {freqm$music5[i] <- 1} else {freqm$music5[i] <- 0}
  if (freqm$music[i] == 6) {freqm$music6[i] <- 1} else {freqm$music6[i] <- 0}
  if (freqm$music[i] == 7) {freqm$music7[i] <- 1} else {freqm$music7[i] <- 0}
}
freqmhr$musichr1 <- NA
freqmhr$musichr2 <- NA
freqmhr$musichr3 <- NA
freqmhr$musichr4 <- NA
freqmhr$musichr5 <- NA
freqmhr$musichr6 <- NA
for (i in 1:nrow(freqmhr)){
  if (freqmhr$musichr[i] == 1) {freqmhr$musichr1[i] <- 1} else {freqmhr$musichr1[i] <- 0}
  if (freqmhr$musichr[i] == 2) {freqmhr$musichr2[i] <- 1} else {freqmhr$musichr2[i] <- 0}
  if (freqmhr$musichr[i] == 3) {freqmhr$musichr3[i] <- 1} else {freqmhr$musichr3[i] <- 0}
  if (freqmhr$musichr[i] == 4) {freqmhr$musichr4[i] <- 1} else {freqmhr$musichr4[i] <- 0}
  if (freqmhr$musichr[i] == 5) {freqmhr$musichr5[i] <- 1} else {freqmhr$musichr5[i] <- 0}
  if (freqmhr$musichr[i] == 6) {freqmhr$musichr6[i] <- 1} else {freqmhr$musichr6[i] <- 0}
}

# dish
freqd$dish1 <- NA
freqd$dish2 <- NA
freqd$dish3 <- NA
freqd$dish4 <- NA
freqd$dish5 <- NA
freqd$dish6 <- NA
freqd$dish7 <- NA
for (i in 1:nrow(freqd)){
  if (freqd$dish[i] == 1) {freqd$dish1[i] <- 1} else {freqd$dish1[i] <- 0}
  if (freqd$dish[i] == 2) {freqd$dish2[i] <- 1} else {freqd$dish2[i] <- 0}
  if (freqd$dish[i] == 3) {freqd$dish3[i] <- 1} else {freqd$dish3[i] <- 0}
  if (freqd$dish[i] == 4) {freqd$dish4[i] <- 1} else {freqd$dish4[i] <- 0}
  if (freqd$dish[i] == 5) {freqd$dish5[i] <- 1} else {freqd$dish5[i] <- 0}
  if (freqd$dish[i] == 6) {freqd$dish6[i] <- 1} else {freqd$dish6[i] <- 0}
  if (freqd$dish[i] == 7) {freqd$dish7[i] <- 1} else {freqd$dish7[i] <- 0}
}
freqdhr$dishhr1 <- NA
freqdhr$dishhr2 <- NA
freqdhr$dishhr3 <- NA
freqdhr$dishhr4 <- NA
freqdhr$dishhr5 <- NA
freqdhr$dishhr6 <- NA
for (i in 1:nrow(freqdhr)){
  if (freqdhr$dishhr[i] == 1) {freqdhr$dishhr1[i] <- 1} else {freqdhr$dishhr1[i] <- 0}
  if (freqdhr$dishhr[i] == 2) {freqdhr$dishhr2[i] <- 1} else {freqdhr$dishhr2[i] <- 0}
  if (freqdhr$dishhr[i] == 3) {freqdhr$dishhr3[i] <- 1} else {freqdhr$dishhr3[i] <- 0}
  if (freqdhr$dishhr[i] == 4) {freqdhr$dishhr4[i] <- 1} else {freqdhr$dishhr4[i] <- 0}
  if (freqdhr$dishhr[i] == 5) {freqdhr$dishhr5[i] <- 1} else {freqdhr$dishhr5[i] <- 0}
  if (freqdhr$dishhr[i] == 6) {freqdhr$dishhr6[i] <- 1} else {freqdhr$dishhr6[i] <- 0}
}


# traffic ref
freqt$traffic1 <- NA
freqt$traffic2 <- NA
freqt$traffic3 <- NA
freqt$traffic4 <- NA
freqt$traffic5 <- NA
freqt$traffic6 <- NA
freqt$traffic7 <- NA
for (i in 1:nrow(freqt)){
  if (freqt$traffic[i] == 1) {freqt$traffic1[i] <- 1} else {freqt$traffic1[i] <- 0}
  if (freqt$traffic[i] == 2) {freqt$traffic2[i] <- 1} else {freqt$traffic2[i] <- 0}
  if (freqt$traffic[i] == 3) {freqt$traffic3[i] <- 1} else {freqt$traffic3[i] <- 0}
  if (freqt$traffic[i] == 4) {freqt$traffic4[i] <- 1} else {freqt$traffic4[i] <- 0}
  if (freqt$traffic[i] == 5) {freqt$traffic5[i] <- 1} else {freqt$traffic5[i] <- 0}
  if (freqt$traffic[i] == 6) {freqt$traffic6[i] <- 1} else {freqt$traffic6[i] <- 0}
  if (freqt$traffic[i] == 7) {freqt$traffic7[i] <- 1} else {freqt$traffic7[i] <- 0}
}
freqthr$traffichr1 <- NA
freqthr$traffichr2 <- NA
freqthr$traffichr3 <- NA
freqthr$traffichr4 <- NA
freqthr$traffichr5 <- NA
freqthr$traffichr6 <- NA
for (i in 1:nrow(freqthr)){
  if (freqthr$traffichr[i] == 1) {freqthr$traffichr1[i] <- 1} else {freqthr$traffichr1[i] <- 0}
  if (freqthr$traffichr[i] == 2) {freqthr$traffichr2[i] <- 1} else {freqthr$traffichr2[i] <- 0}
  if (freqthr$traffichr[i] == 3) {freqthr$traffichr3[i] <- 1} else {freqthr$traffichr3[i] <- 0}
  if (freqthr$traffichr[i] == 4) {freqthr$traffichr4[i] <- 1} else {freqthr$traffichr4[i] <- 0}
  if (freqthr$traffichr[i] == 5) {freqthr$traffichr5[i] <- 1} else {freqthr$traffichr5[i] <- 0}
  if (freqthr$traffichr[i] == 6) {freqthr$traffichr6[i] <- 1} else {freqthr$traffichr6[i] <- 0}
}

# vacuum ref
freqv$vacuum1 <- NA
freqv$vacuum2 <- NA
freqv$vacuum3 <- NA
freqv$vacuum4 <- NA
freqv$vacuum5 <- NA
freqv$vacuum6 <- NA
freqv$vacuum7 <- NA
for (i in 1:nrow(freqv)){
  if (freqv$vacuum[i] == 1) {freqv$vacuum1[i] <- 1} else {freqv$vacuum1[i] <- 0}
  if (freqv$vacuum[i] == 2) {freqv$vacuum2[i] <- 1} else {freqv$vacuum2[i] <- 0}
  if (freqv$vacuum[i] == 3) {freqv$vacuum3[i] <- 1} else {freqv$vacuum3[i] <- 0}
  if (freqv$vacuum[i] == 4) {freqv$vacuum4[i] <- 1} else {freqv$vacuum4[i] <- 0}
  if (freqv$vacuum[i] == 5) {freqv$vacuum5[i] <- 1} else {freqv$vacuum5[i] <- 0}
  if (freqv$vacuum[i] == 6) {freqv$vacuum6[i] <- 1} else {freqv$vacuum6[i] <- 0}
  if (freqv$vacuum[i] == 7) {freqv$vacuum7[i] <- 1} else {freqv$vacuum7[i] <- 0}
}
freqvhr$vacuumhr1 <- NA
freqvhr$vacuumhr2 <- NA
freqvhr$vacuumhr3 <- NA
freqvhr$vacuumhr4 <- NA
freqvhr$vacuumhr5 <- NA
freqvhr$vacuumhr6 <- NA
for (i in 1:nrow(freqvhr)){
  if (freqvhr$vacuumhr[i] == 1) {freqvhr$vacuumhr1[i] <- 1} else {freqvhr$vacuumhr1[i] <- 0}
  if (freqvhr$vacuumhr[i] == 2) {freqvhr$vacuumhr2[i] <- 1} else {freqvhr$vacuumhr2[i] <- 0}
  if (freqvhr$vacuumhr[i] == 3) {freqvhr$vacuumhr3[i] <- 1} else {freqvhr$vacuumhr3[i] <- 0}
  if (freqvhr$vacuumhr[i] == 4) {freqvhr$vacuumhr4[i] <- 1} else {freqvhr$vacuumhr4[i] <- 0}
  if (freqvhr$vacuumhr[i] == 5) {freqvhr$vacuumhr5[i] <- 1} else {freqvhr$vacuumhr5[i] <- 0}
  if (freqvhr$vacuumhr[i] == 6) {freqvhr$vacuumhr6[i] <- 1} else {freqvhr$vacuumhr6[i] <- 0}
}

# game
ggame <- aov(freqg$g ~ freqg$game) %>% anova_stats()
sqrt(ggame[1,8])
summary(lm(freqg$g ~ freqg$game))
summary(lm(freqg$g ~ freqg$game1 + freqg$game2 + freqg$game3 + freqg$game4 + freqg$game5 + freqg$game6 + freqg$game7))

gg <- ggplot(freqg1, aes(x = game, y = g, group = game)) + 
  geom_boxplot() +
  theme_bw() +
  labs(title = "Playing Games") +
  scale_y_continuous(limits = c(-102, 102)) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank())

gwgame <- aov(freqg$gw ~ freqg$game) %>% anova_stats()
sqrt(gwgame[1,8])
summary(lm(freqg$gw ~ freqg$game))
summary(lm(freqg$gw ~ freqg$game1 + freqg$game2 + freqg$game3 + freqg$game4 + freqg$game5 + freqg$game6 + freqg$game7))

gwg <- ggplot(freqg1, aes(x = game, y = gw, group = game)) + 
  geom_boxplot() +
  theme_bw() +
  labs(title = "Playing Games") +
  scale_y_continuous(limits = c(-2, 102)) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank())

ggamehr <- aov(freqghr$g ~ freqghr$gamehr) %>% anova_stats()
sqrt(ggamehr[1,8])
summary(lm(freqghr$g ~ freqghr$gamehr))
summary(lm(freqghr$g ~ freqghr$gamehr1 + freqghr$gamehr2 + freqghr$gamehr3 + freqghr$gamehr4 + freqghr$gamehr5 + freqgw$gamehr6))

gghr <- ggplot(freqghr1, aes(x = gamehr, y = g, group = gamehr)) + 
  geom_boxplot() +
  theme_bw() +
  labs(title = "Playing Games") +
  scale_y_continuous(limits = c(-102, 102)) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank())

gwgamehr <- aov(freqghr$gw ~ freqghr$gamehr) %>% anova_stats()
sqrt(gwgamehr[1,8])
summary(lm(freqghr$gw ~ freqghr$gamehr))
summary(lm(freqghr$gw ~ freqghr$gamehr1 + freqghr$gamehr2 + freqghr$gamehr3 + freqghr$gamehr4 + freqghr$gamehr5 + freqghr$gamehr6))

gwghr <- ggplot(freqghr1, aes(x = gamehr, y = gw, group = gamehr)) + 
  geom_boxplot() +
  theme_bw() +
  labs(title = "Playing Games") +
  scale_y_continuous(limits = c(-2, 102)) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank())

# sports
ssports <- aov(freqs$s ~ freqs$sports) %>% anova_stats()
sqrt(ssports[1,8])
summary(lm(freqs$s ~ freqs$sports))
summary(lm(freqs$s ~ freqs$sports1 + freqs$sports2 + freqs$sports3 + freqs$sports4 + freqs$sports5 + freqs$sports6 + freqs$sports7))

ss <- ggplot(freqs1, aes(x = sports, y = s, group = sports)) + 
  geom_boxplot() +
  theme_bw() +
  labs(title = "Doing Sports") +
  scale_y_continuous(limits = c(-102, 102)) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank())

swsports <- aov(freqs$sw ~ freqs$sports) %>% anova_stats()
sqrt(swsports[1,8])
summary(lm(freqs$sw ~ freqs$sports))
summary(lm(freqs$sw ~ freqs$sports1 + freqs$sports2 + freqs$sports3 + freqs$sports4 + freqs$sports5 + freqs$sports6 + freqs$sports7))

sws <- ggplot(freqs1, aes(x = sports, y = sw, group = sports)) + 
  geom_boxplot() +
  theme_bw() +
  labs(title = "Doing Sports") +
  scale_y_continuous(limits = c(-2, 102)) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank())

ssportshr <- aov(freqshr$s ~ freqshr$sportshr) %>% anova_stats()
sqrt(ssportshr[1,8])
summary(lm(freqshr$s ~ freqshr$sportshr))
summary(lm(freqshr$s ~ freqshr$sportshr1 + freqshr$sportshr2 + freqshr$sportshr3 + freqshr$sportshr4 + freqshr$sportshr5 + freqshr$sportshr6))

sshr <- ggplot(freqshr1, aes(x = sportshr, y = s, group = sportshr)) + 
  geom_boxplot() +
  theme_bw() +
  labs(title = "Doing Sports") +
  scale_y_continuous(limits = c(-102, 102)) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank())


swsportshr <- aov(freqshr$sw ~ freqshr$sportshr) %>% anova_stats()
sqrt(swsportshr[1,8])
summary(lm(freqshr$sw ~ freqshr$sportshr))
summary(lm(freqshr$sw ~ freqshr$sportshr1 + freqshr$sportshr2 + freqshr$sportshr3 + freqshr$sportshr4 + freqshr$sportshr5 + freqshr$sportshr6))

swshr <- ggplot(freqshr1, aes(x = sportshr, y = sw, group = sportshr)) + 
  geom_boxplot() +
  theme_bw() +
  labs(title = "Doing Sports") +
  scale_y_continuous(limits = c(-2, 102)) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank())

# music
mmusic <- aov(freqm$m ~ freqm$music) %>% anova_stats()
sqrt(mmusic[1,8])
summary(lm(freqm$m ~ freqm$music))
summary(lm(freqm$m ~ freqm$music1 + freqm$music2 + freqm$music3 + freqm$music4 + freqm$music5 + freqm$music6 + freqm$music7))

mm <- ggplot(freqm1, aes(x = factor(music), y = m)) + 
  geom_boxplot() +
  theme_bw() +
  labs(title = "Listening to Music") +
  scale_y_continuous(limits = c(-102, 102)) +
  scale_x_discrete(drop = FALSE, limits = c(1:8), labels = c("Never", "Less than once a month", "Once a month", "2-3 times a month", "Once a week", "2-3 times a week", "4-6 times a week", "Daily")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank())



mwmusic <- aov(freqm$mw ~ freqm$music) %>% anova_stats()
sqrt(mwmusic[1,8])
summary(lm(freqm$mw ~ freqm$music))
summary(lm(freqm$mw ~ freqm$music1 + freqm$music2 + freqm$music3 + freqm$music4 + freqm$music5 + freqm$music6 + freqm$music7))

mwm <- ggplot(freqm1, aes(x = factor(music), y = mw)) + 
  geom_boxplot() +
  theme_bw() +
  labs(title = "Listening to Music") +
  scale_y_continuous(limits = c(-2, 102)) +
  scale_x_discrete(drop = FALSE, limits = c(1:8), labels = c("Never", "Less than once a month", "Once a month", "2-3 times a month", "Once a week", "2-3 times a week", "4-6 times a week", "Daily")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank())


mmusichr <- aov(freqmhr$m ~ freqmhr$musichr) %>% anova_stats()
sqrt(mmusichr[1,8])
summary(lm(freqmhr$m ~ freqmhr$musichr))
summary(lm(freqmhr$m ~ freqmhr$musichr1 + freqmhr$musichr2 + freqmhr$musichr3 + freqmhr$musichr4 + freqmhr$musichr5 + freqmhr$musichr6))

mmhr <- ggplot(freqmhr1, aes(x = musichr, y = m, group = musichr)) + 
  geom_boxplot() +
  theme_bw() +
  labs(title = "Listening to Music") +
  scale_x_discrete(limits = (1:8), label = c("< 15 mins", "15 - 30 mins", "30 - 45 mins", "45 - 60 mins", "60 - 75 mins", "75 - 90 mins", "> 90 mins", "NA")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_y_continuous(limits = c(-102, 102)) +
  theme(axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank())

mwmusichr <- aov(freqmhr$mw ~ freqmhr$musichr) %>% anova_stats()
sqrt(mwmusichr[1,8])
summary(lm(freqmhr$mw ~ freqmhr$musichr))
summary(lm(freqmhr$mw ~ freqmhr$musichr1 + freqmhr$musichr2 + freqmhr$musichr3 + freqmhr$musichr4 + freqmhr$musichr5 + freqmhr$musichr6)) # positive

mwmhr <- ggplot(freqmhr1, aes(x = musichr, y = mw, group = musichr)) + 
  geom_boxplot() +
  theme_bw() +
  labs(title = "Listening to Music") +
  scale_x_discrete(limits = (1:8), label = c("< 15 mins", "15 - 30 mins", "30 - 45 mins", "45 - 60 mins", "60 - 75 mins", "75 - 90 mins", "> 90 mins", "NA")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_y_continuous(limits = c(-2, 102)) +
  theme(axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank())

# dish
ddish <- aov(freqd$d ~ freqd$dish) %>% anova_stats()
sqrt(ddish[1,8])
summary(lm(freqd$d ~ freqd$dish))
summary(lm(freqd$d ~ freqd$dish1 + freqd$dish2 + freqd$dish3 + freqd$dish4 + freqd$dish5 + freqd$dish6 + freqd$dish7)) # positive

dd <- ggplot(freqd1, aes(x = dish, y = d, group = dish)) + 
  geom_boxplot() +
  theme_bw() +
  labs(title = "Washing Dishes") +
  scale_y_continuous(limits = c(-102, 102)) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank())

dwdish <- aov(freqd$dw ~ freqd$dish) %>% anova_stats()
sqrt(dwdish[1,8])
summary(lm(freqd$dw ~ freqd$dish))
summary(lm(freqd$dw ~ freqd$dish1 + freqd$dish2 + freqd$dish3 + freqd$dish4 + freqd$dish5 + freqd$dish6 + freqd$dish7)) # negative

dwd <- ggplot(freqd1, aes(x = dish, y = dw, group = dish)) + 
  geom_boxplot() +
  theme_bw() +
  labs(title = "Washing Dishes") +
  scale_y_continuous(limits = c(-2, 102)) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank())

ddishhr <- aov(freqdhr$d ~ freqdhr$dishhr) %>% anova_stats()
sqrt(ddishhr[1,8])
summary(lm(freqdhr$d ~ freqdhr$dishhr))
summary(lm(freqdhr$d ~ freqdhr$dishhr1 + freqdhr$dishhr2 + freqdhr$dishhr3 + freqdhr$dishhr4 + freqdhr$dishhr5 + freqdhr$dishhr6)) # positive

ddhr <- ggplot(freqdhr1, aes(x = dishhr, y = d, group = dishhr)) + 
  geom_boxplot() +
  theme_bw() +
  labs(title = "Washing Dishes") +
  scale_y_continuous(limits = c(-102, 102)) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank())

dwdishhr <- aov(freqdhr$dw ~ freqdhr$dishhr) %>% anova_stats()
sqrt(dwdishhr[1,8])
summary(lm(freqdhr$dw ~ freqdhr$dishhr))
summary(lm(freqdhr$dw ~ freqdhr$dishhr1 + freqdhr$dishhr2 + freqdhr$dishhr3 + freqdhr$dishhr4 + freqdhr$dishhr5 + freqdhr$dishhr6)) # positive

dwdhr <- ggplot(freqdhr1, aes(x = dishhr, y = dw, group = dishhr)) + 
  geom_boxplot() +
  theme_bw() +
  labs(title = "Washing Dishes") +
  scale_y_continuous(limits = c(-2, 102)) +
  theme(axis.title.x=element_blank(),
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank(),
      axis.title.y=element_blank())

# traffic
ttraffic <- aov(freqt$t ~ freqt$traffic) %>% anova_stats()
sqrt(ttraffic[1,8])
summary(lm(freqt$t ~ freqt$traffic))
summary(lm(freqt$t ~ freqt$traffic1 + freqt$traffic2 + freqt$traffic3 + freqt$traffic4 + freqt$traffic5 + freqt$traffic6 + freqt$traffic7)) # negative

tt <- ggplot(freqt1, aes(x = traffic, y = t, group = traffic)) + 
  geom_boxplot() +
  theme_bw() +
  labs(title = "Getting Stuck in a Traffic Jam") +
  scale_y_continuous(limits = c(-102, 102)) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank())

twtraffic <- aov(freqt$tw ~ freqt$traffic) %>% anova_stats()
sqrt(twtraffic[1,8])
summary(lm(freqt$tw ~ freqt$traffic))
summary(lm(freqt$tw ~ freqt$traffic1 + freqt$traffic2 + freqt$traffic3 + freqt$traffic4 + freqt$traffic5 + freqt$traffic6 + freqt$traffic7)) # positive

twt <- ggplot(freqt1, aes(x = traffic, y = tw, group = traffic)) + 
  geom_boxplot() +
  theme_bw() +
  labs(title = "Getting Stuck in a Traffic Jam") +
  scale_y_continuous(limits = c(-2, 102)) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank())

ttraffichr <- aov(freqthr$t ~ freqthr$traffichr) %>% anova_stats()
sqrt(ttraffichr[1,8])
summary(lm(freqthr$t ~ freqthr$traffichr))
summary(lm(freqthr$t ~ freqthr$traffichr1 + freqthr$traffichr2 + freqthr$traffichr3 + freqthr$traffichr4 + freqthr$traffichr5 + freqthr$traffichr6)) # positive

tthr <- ggplot(freqthr1, aes(x = traffichr, y = t, group = traffichr)) + 
  geom_boxplot() +
  theme_bw() +
  labs(title = "Getting Stuck in a Traffic Jam") +
  scale_y_continuous(limits = c(-102, 102)) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank())

twtraffichr <- aov(freqthr$tw ~ freqthr$traffichr) %>% anova_stats()
sqrt(twtraffichr[1,8])
summary(lm(freqthr$tw ~ freqthr$traffichr))
summary(lm(freqthr$tw ~ freqthr$traffichr1 + freqthr$traffichr2 + freqthr$traffichr3 + freqthr$traffichr4 + freqthr$traffichr5 + freqthr$traffichr6)) # positive

twthr <- ggplot(freqthr1, aes(x = traffichr, y = tw, group = traffichr)) + 
  geom_boxplot() +
  theme_bw() +
  labs(title = "Getting Stuck in a Traffic Jam") +
  scale_y_continuous(limits = c(-2, 102)) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank())

# vacuum
vvacuum <- aov(freqv$v ~ freqv$vacuum) %>% anova_stats()
sqrt(vvacuum[1,8])
summary(lm(freqv$v ~ freqv$vacuum))
summary(lm(freqv$v ~ freqv$vacuum1 + freqv$vacuum2 + freqv$vacuum3 + freqv$vacuum4 + freqv$vacuum5 + freqv$vacuum6 + freqv$vacuum7)) # positive

vv <- ggplot(freqv1, aes(x = vacuum, y = v, group = vacuum)) + 
  geom_boxplot() +
  theme_bw() +
  labs(title = "Vacuuming") +
  scale_y_continuous(limits = c(-102, 102)) +
  scale_x_discrete(limits = c(1:8), label = c("Never", "Less than once a month", "Once a month", "2-3 times a month", "Once a week", "2-3 times a week", "4-6 times a week", "Daily")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank())

vwvacuum <- aov(freqv$vw ~ freqv$vacuum) %>% anova_stats()
sqrt(vwvacuum[1,8])
summary(lm(freqv$vw ~ freqv$vacuum))
summary(lm(freqv$vw ~ freqv$vacuum1 + freqv$vacuum2 + freqv$vacuum3 + freqv$vacuum4 + freqv$vacuum5 + freqv$vacuum6 + freqv$vacuum7)) # negative

vwv <- ggplot(freqv1, aes(x = vacuum, y = vw, group = vacuum)) + 
  geom_boxplot() +
  theme_bw() +
  labs(title = "Vacuuming") +
  scale_y_continuous(limits = c(-2, 102)) +
  scale_x_discrete(limits = c(1:8), label = c("Never", "Less than once a month", "Once a month", "2-3 times a month", "Once a week", "2-3 times a week", "4-6 times a week", "Daily")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank())

vvacuumhr <- aov(freqvhr$v ~ freqvhr$vacuumhr) %>% anova_stats()
sqrt(vvacuumhr[1,8])
summary(lm(freqvhr$v ~ freqvhr$vacuumhr))
summary(lm(freqvhr$v ~ freqvhr$vacuumhr1 + freqvhr$vacuumhr2 + freqvhr$vacuumhr3 + freqvhr$vacuumhr4 + freqvhr$vacuumhr5 + freqvhr$vacuumhr6)) # positive

vvhr <- ggplot(freqvhr1, aes(x = factor(vacuumhr), y = v)) + 
  geom_boxplot() +
  theme_bw() +
  labs(title = "Vacuuming") +
  scale_x_discrete(drop = F, limits = (1:8), label = c("< 15 mins", "15 - 30 mins", "30 - 45 mins", "45 - 60 mins", "60 - 75 mins", "75 - 90 mins", "> 90 mins", "NA")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_y_continuous(limits = c(-102, 102)) +
  theme(axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank())

vwvacuumhr <- aov(freqvhr$vw ~ freqvhr$vacuumhr) %>% anova_stats()
sqrt(vwvacuumhr[1,8])
summary(lm(freqvhr$vw ~ freqvhr$vacuumhr))
summary(lm(freqvhr$vw ~ freqvhr$vacuumhr1 + freqvhr$vacuumhr2 + freqvhr$vacuumhr3 + freqvhr$vacuumhr4 + freqvhr$vacuumhr5 + freqvhr$vacuumhr6)) # positive

vwvhr <- ggplot(freqvhr1, aes(x = vacuumhr, y = vw, group = vacuumhr)) + 
  geom_boxplot() +
  theme_bw() +
  labs(title = "Vacuuming") +
  scale_x_discrete(limits = (1:8), label = c("< 15 mins", "15 - 30 mins", "30 - 45 mins", "45 - 60 mins", "60 - 75 mins", "75 - 90 mins", "> 90 mins", "NA")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_y_continuous(limits = c(-2, 102)) +
  theme(axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank())


# combine plots
evafreq <- ggarrange(gg,dd,ss,tt,mm,vv, nrow = 3, ncol = 2, heights = c(2, 2,3.75))
annotate_figure(evafreq, left = text_grob("Evaluation Score", rot = 90),
                bottom = text_grob("Frequency"))

wtpfreq <- ggarrange(gwg,dwd,sws,twt,mwm,vwv, nrow = 3, ncol = 2, heights = c(2, 2,3.75))
annotate_figure(wtpfreq, left = text_grob("Willingness-to-Pay", rot = 90),
                bottom = text_grob("Frequency"))


evahr <- ggarrange(gghr,ddhr,sshr,tthr,mmhr,vvhr, nrow = 3, ncol = 2, heights = c(2, 2,2.75))
annotate_figure(evahr, left = text_grob("Evaluation Score", rot = 90),
                bottom = text_grob("Duration"))

wtphr <- ggarrange(gwghr,dwdhr,swshr,twthr,mwmhr,vwvhr, nrow = 3, ncol = 2, heights = c(2, 2,2.75))
annotate_figure(wtphr, left = text_grob("Willingness-to-Pay", rot = 90),
             bottom = text_grob("Duration"))


