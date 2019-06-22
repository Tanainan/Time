library(tidyverse)
library(sjstats)
library(Hmisc)

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



# reverse coding

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
Time[which(Time$gamehr == 8), c("gamehr")] <- NA
Time[which(Time$sportshr == 8), c("sportshr")] <- NA
Time[which(Time$musichr == 8), c("musichr")] <- NA
Time[which(Time$traffichr == 8), c("traffichr")] <- NA
Time[which(Time$dishhr == 8), c("dishhr")] <- NA
Time[which(Time$vacuumhr == 8), c("vacuumhr")] <- NA


# create new data set for evaluation score = dv
freqg <- (Time[,c("g","game", "gamehr")])
freqs <- (Time[,c("s","sports","sportshr")])
freqm <- (Time[,c("m","music","musichr")])
freqd <- (Time[,c("d","dish","dishhr")])
freqt <- (Time[,c("t","traffic","traffichr")])
freqv <- (Time[,c("v","vacuum","vacuumhr")])

# create new data set for wtp = dv
freqgw <- (Time[,c("game", "gamehr")])
freqsw <- (Time[,c("sports","sportshr")])
freqmw <- (Time[,c("music","musichr")])
freqdw <- (Time[,c("dish","dishhr")])
freqtw <- (Time[,c("traffic","traffichr")])
freqvw <- (Time[,c("vacuum","vacuumhr")])


# add wtp column
freqgw$gw <- ggo$t
freqsw$sw <- sso$t
freqmw$mw <- mmo$t 
freqdw$dw <- ddo$t
freqtw$tw <- tto$t
freqvw$vw <- vvo$t

# omit NA for wtp
freqgw <- na.omit(freqgw)
freqsw <- na.omit(freqsw)
freqmw <- na.omit(freqmw)
freqdw <- na.omit(freqdw)
freqtw <- na.omit(freqtw)
freqvw <- na.omit(freqvw)

# create dummy variables
# game ref: 0 = never
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

# gamehr ref: 0 = never
freqgw$gamehr1 <- NA
freqgw$gamehr2 <- NA
freqgw$gamehr3 <- NA
freqgw$gamehr4 <- NA
freqgw$gamehr5 <- NA
freqgw$gamehr6 <- NA
for (i in 1:nrow(freqgw)){
  if (freqgw$gamehr[i] == 2) {freqgw$gamehr1[i] <- 1} else {freqgw$gamehr1[i] <- 0}
  if (freqgw$gamehr[i] == 3) {freqgw$gamehr2[i] <- 1} else {freqgw$gamehr2[i] <- 0}
  if (freqgw$gamehr[i] == 4) {freqgw$gamehr3[i] <- 1} else {freqgw$gamehr3[i] <- 0}
  if (freqgw$gamehr[i] == 5) {freqgw$gamehr4[i] <- 1} else {freqgw$gamehr4[i] <- 0}
  if (freqgw$gamehr[i] == 6) {freqgw$gamehr5[i] <- 1} else {freqgw$gamehr5[i] <- 0}
  if (freqgw$gamehr[i] == 7) {freqgw$gamehr6[i] <- 1} else {freqgw$gamehr6[i] <- 0}
}

# sports ref: 0 = never
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

# sportshr ref: 0 = never
freqsw$sportshr1 <- NA
freqsw$sportshr2 <- NA
freqsw$sportshr3 <- NA
freqsw$sportshr4 <- NA
freqsw$sportshr5 <- NA
freqsw$sportshr6 <- NA
for (i in 1:nrow(freqsw)){
  if (freqsw$sportshr[i] == 2) {freqsw$sportshr1[i] <- 1} else {freqsw$sportshr1[i] <- 0}
  if (freqsw$sportshr[i] == 3) {freqsw$sportshr2[i] <- 1} else {freqsw$sportshr2[i] <- 0}
  if (freqsw$sportshr[i] == 4) {freqsw$sportshr3[i] <- 1} else {freqsw$sportshr3[i] <- 0}
  if (freqsw$sportshr[i] == 5) {freqsw$sportshr4[i] <- 1} else {freqsw$sportshr4[i] <- 0}
  if (freqsw$sportshr[i] == 6) {freqsw$sportshr5[i] <- 1} else {freqsw$sportshr5[i] <- 0}
  if (freqsw$sportshr[i] == 7) {freqsw$sportshr6[i] <- 1} else {freqsw$sportshr6[i] <- 0}
}

# music ref: 0 = never
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

# musichr ref: 0 = never
freqmw$musichr1 <- NA
freqmw$musichr2 <- NA
freqmw$musichr3 <- NA
freqmw$musichr4 <- NA
freqmw$musichr5 <- NA
freqmw$musichr6 <- NA
for (i in 1:nrow(freqmw)){
  if (freqmw$musichr[i] == 2) {freqmw$musichr1[i] <- 1} else {freqmw$musichr1[i] <- 0}
  if (freqmw$musichr[i] == 3) {freqmw$musichr2[i] <- 1} else {freqmw$musichr2[i] <- 0}
  if (freqmw$musichr[i] == 4) {freqmw$musichr3[i] <- 1} else {freqmw$musichr3[i] <- 0}
  if (freqmw$musichr[i] == 5) {freqmw$musichr4[i] <- 1} else {freqmw$musichr4[i] <- 0}
  if (freqmw$musichr[i] == 6) {freqmw$musichr5[i] <- 1} else {freqmw$musichr5[i] <- 0}
  if (freqmw$musichr[i] == 7) {freqmw$musichr6[i] <- 1} else {freqmw$musichr6[i] <- 0}
}

# dish ref: 0 = never
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
# dishhr ref: 0 = never
freqdw$dishhr1 <- NA
freqdw$dishhr2 <- NA
freqdw$dishhr3 <- NA
freqdw$dishhr4 <- NA
freqdw$dishhr5 <- NA
freqdw$dishhr6 <- NA
for (i in 1:nrow(freqdw)){
  if (freqdw$dishhr[i] == 2) {freqdw$dishhr1[i] <- 1} else {freqdw$dishhr1[i] <- 0}
  if (freqdw$dishhr[i] == 3) {freqdw$dishhr2[i] <- 1} else {freqdw$dishhr2[i] <- 0}
  if (freqdw$dishhr[i] == 4) {freqdw$dishhr3[i] <- 1} else {freqdw$dishhr3[i] <- 0}
  if (freqdw$dishhr[i] == 5) {freqdw$dishhr4[i] <- 1} else {freqdw$dishhr4[i] <- 0}
  if (freqdw$dishhr[i] == 6) {freqdw$dishhr5[i] <- 1} else {freqdw$dishhr5[i] <- 0}
  if (freqdw$dishhr[i] == 7) {freqdw$dishhr6[i] <- 1} else {freqdw$dishhr6[i] <- 0}
}

# traffic ref: 0 = never
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

# traffichr ref: 0 = never
freqtw$traffichr1 <- NA
freqtw$traffichr2 <- NA
freqtw$traffichr3 <- NA
freqtw$traffichr4 <- NA
freqtw$traffichr5 <- NA
freqtw$traffichr6 <- NA
for (i in 1:nrow(freqtw)){
  if (freqtw$traffichr[i] == 2) {freqtw$traffichr1[i] <- 1} else {freqtw$traffichr1[i] <- 0}
  if (freqtw$traffichr[i] == 3) {freqtw$traffichr2[i] <- 1} else {freqtw$traffichr2[i] <- 0}
  if (freqtw$traffichr[i] == 4) {freqtw$traffichr3[i] <- 1} else {freqtw$traffichr3[i] <- 0}
  if (freqtw$traffichr[i] == 5) {freqtw$traffichr4[i] <- 1} else {freqtw$traffichr4[i] <- 0}
  if (freqtw$traffichr[i] == 6) {freqtw$traffichr5[i] <- 1} else {freqtw$traffichr5[i] <- 0}
  if (freqtw$traffichr[i] == 7) {freqtw$traffichr6[i] <- 1} else {freqtw$traffichr6[i] <- 0}
}


# vacuum ref: 0 = never
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

# vacuumhr ref: 0 = never
freqvw$vacuumhr1 <- NA
freqvw$vacuumhr2 <- NA
freqvw$vacuumhr3 <- NA
freqvw$vacuumhr4 <- NA
freqvw$vacuumhr5 <- NA
freqvw$vacuumhr6 <- NA
for (i in 1:nrow(freqvw)){
  if (freqvw$vacuumhr[i] == 2) {freqvw$vacuumhr1[i] <- 1} else {freqvw$vacuumhr1[i] <- 0}
  if (freqvw$vacuumhr[i] == 3) {freqvw$vacuumhr2[i] <- 1} else {freqvw$vacuumhr2[i] <- 0}
  if (freqvw$vacuumhr[i] == 4) {freqvw$vacuumhr3[i] <- 1} else {freqvw$vacuumhr3[i] <- 0}
  if (freqvw$vacuumhr[i] == 5) {freqvw$vacuumhr4[i] <- 1} else {freqvw$vacuumhr4[i] <- 0}
  if (freqvw$vacuumhr[i] == 6) {freqvw$vacuumhr5[i] <- 1} else {freqvw$vacuumhr5[i] <- 0}
  if (freqvw$vacuumhr[i] == 7) {freqvw$vacuumhr6[i] <- 1} else {freqvw$vacuumhr6[i] <- 0}
}


# game
ggame <- aov(freqg$g ~ freqg$game) %>% anova_stats()
sqrt(ggame[1,8])
summary(lm(freqg$g ~ freqg$game1 + freqg$game2 + freqg$game3 + freqg$game4 + freqg$game5 + freqg$game6 + freqg$game7))

gwgame <- aov(freqgw$gw ~ freqgw$game) %>% anova_stats()
sqrt(gwgame[1,8])
summary(lm(freqgw$gw ~ freqgw$game1 + freqgw$game2 + freqgw$game3 + freqgw$game4 + freqgw$game5 + freqgw$game6 + freqg$game7))

ggamehr <- aov(freqg$g ~ freqg$gamehr) %>% anova_stats()
sqrt(ggamehr[1,8])
summary(lm(freqg$g ~ freqg$gamehr1 + freqg$gamehr2 + freqg$gamehr3 + freqg$gamehr4 + freqg$gamehr5 + freqgw$gamehr6))

gwgamehr <- aov(freqgw$gw ~ freqgw$gamehr) %>% anova_stats()
sqrt(gwgamehr[1,8])
summary(lm(freqgw$gw ~ freqgw$gamehr1 + freqgw$gamehr2 + freqgw$gamehr3 + freqgw$gamehr4 + freqgw$gamehr5 + freqgw$gamehr6))

# sports
ssports <- aov(freqs$s ~ freqs$sports) %>% anova_stats()
sqrt(ssports[1,8])
summary(lm(freqs$s ~ freqs$sports1 + freqs$sports2 + freqs$sports3 + freqs$sports4 + freqs$sports5 + freqs$sports6 + freqs$sports7))

swsports <- aov(freqsw$sw ~ freqsw$sports) %>% anova_stats()
sqrt(swsports[1,8])
summary(lm(freqsw$sw ~ freqsw$sports1 + freqsw$sports2 + freqsw$sports3 + freqsw$sports4 + freqsw$sports5 + freqsw$sports6 + freqsw$sports7))

ssportshr <- aov(freqs$s ~ freqs$sportshr) %>% anova_stats()
sqrt(ssportshr[1,8])
summary(lm(freqs$s ~ freqs$sportshr1 + freqs$sportshr2 + freqs$sportshr3 + freqs$sportshr4 + freqs$sportshr5 + freqs$sportshr6))
swsportshr <- aov(freqsw$sw ~ freqsw$sportshr) %>% anova_stats()
sqrt(swsportshr[1,8])
summary(lm(freqsw$sw ~ freqsw$sportshr1 + freqsw$sportshr2 + freqsw$sportshr3 + freqsw$sportshr4 + freqsw$sportshr5 + freqsw$sportshr6))


# music
mmusic <- aov(freqm$m ~ freqm$music) %>% anova_stats()
sqrt(mmusic[1,8])
summary(lm(freqm$m ~ freqm$music1 + freqm$music2 + freqm$music3 + freqm$music4 + freqm$music5 + freqm$music6 + freqm$music7))

mwmusic <- aov(freqmw$mw ~ freqmw$music) %>% anova_stats()
sqrt(mwmusic[1,8])
summary(lm(freqmw$mw ~ freqmw$music1 + freqmw$music2 + freqmw$music3 + freqmw$music4 + freqmw$music5 + freqmw$music6 + freqmw$music7))

mmusichr <- aov(freqm$m ~ freqm$musichr) %>% anova_stats()
sqrt(mmusichr[1,8])
summary(lm(freqm$m ~ freqm$musichr1 + freqm$musichr2 + freqm$musichr3 + freqm$musichr4 + freqm$musichr5 + freqm$musichr6))

mwmusichr <- aov(freqmw$mw ~ freqmw$musichr) %>% anova_stats()
sqrt(mwmusichr[1,8])
summary(lm(freqmw$mw ~ freqmw$musichr1 + freqmw$musichr2 + freqmw$musichr3 + freqmw$musichr4 + freqmw$musichr5 + freqmw$musichr6)) # positive



ddish <- aov(freqd$d ~ freqd$dish) %>% anova_stats()
sqrt(ddish[1,8])
summary(lm(freqd$d ~ freqd$dish1 + freqd$dish2 + freqd$dish3 + freqd$dish4 + freqd$dish5 + freqd$dish6 + freqd$dish7)) # positive

dwdish <- aov(frefreqd$dish$dw ~ frefreqd$dish$dish) %>% anova_stats()
sqrt(dwdish[1,8])
summary(lm(freqdw$dw ~ freqdw$dish1 + freqdw$dish2 + freqdw$dish3 + freqdw$dish4 + freqdw$dish5 + freqdw$dish6 + freqdw$dish7)) # negative

ddishhr <- aov(freqd$d ~ freqd$dishhr) %>% anova_stats()
sqrt(ddishhr[1,8])
summary(lm(freqd$d ~ freqd$dishhr1 + freqd$dishhr2 + freqd$dishhr3 + freqd$dishhr4 + freqd$dishhr5 + freqd$dishhr6)) # positive

dwdishhr <- aov(freqdw$dw ~ freqdw$dishhr) %>% anova_stats()
sqrt(dwdishhr[1,8])
summary(lm(freqdw$dw ~ freqdw$dishhr1 + freqdw$dishhr2 + freqdw$dishhr3 + freqdw$dishhr4 + freqdw$dishhr5 + freqdw$dishhr6)) # positive



ttraffic <- aov(freqt$t ~ freqt$traffic) %>% anova_stats()
sqrt(ttraffic[1,8])
summary(lm(freqt$t ~ freqt$traffic1 + freqt$traffic2 + freqt$traffic3 + freqt$traffic4 + freqt$traffic5 + freqt$traffic6 + freqt$traffic7)) # negative

twtraffic <- aov(freqtw$tw ~ freqtw$traffic) %>% anova_stats()
sqrt(twtraffic[1,8])
summary(lm(freqtw$tw ~ freqtw$traffic1 + freqtw$traffic2 + freqtw$traffic3 + freqtw$traffic4 + freqtw$traffic5 + freqtw$traffic6 + freqtw$traffic7)) # positive

ttraffichr <- aov(freqt$t ~ freqt$traffichr) %>% anova_stats()
sqrt(ttraffichr[1,8])
summary(lm(freqt$t ~ freqt$traffichr1 + freqt$traffichr2 + freqt$traffichr3 + freqt$traffichr4 + freqt$traffichr5 + freqt$traffichr6)) # positive

twtraffichr <- aov(freqtw$tw ~ freqtw$traffichr) %>% anova_stats()
sqrt(twtraffichr[1,8])
summary(lm(freqtw$tw ~ freqtw$traffichr1 + freqtw$traffichr2 + freqtw$traffichr3 + freqtw$traffichr4 + freqtw$traffichr5 + freqtw$traffichr6)) # positive



vvacuum <- aov(freqv$v ~ freqv$vacuum) %>% anova_stats()
sqrt(vvacuum[1,8])
summary(lm(freqv$v ~ freqv$vacuum1 + freqv$vacuum2 + freqv$vacuum3 + freqv$vacuum4 + freqv$vacuum5 + freqv$vacuum6 + freqv$vacuum7)) # positive

vwvacuum <- aov(freqvw$vw ~ freqvw$vacuum) %>% anova_stats()
sqrt(vwvacuum[1,8])
summary(lm(freqvw$vw ~ freqvw$vacuum1 + freqvw$vacuum2 + freqvw$vacuum3 + freqvw$vacuum4 + freqvw$vacuum5 + freqvw$vacuum6 + freqvw$vacuum7)) # negative

vvacuumhr <- aov(freqv$v ~ freqv$vacuumhr) %>% anova_stats()
sqrt(vvacuumhr[1,8])
summary(lm(freqv$v ~ freqv$vacuumhr1 + freqv$vacuumhr2 + freqv$vacuumhr3 + freqv$vacuumhr4 + freqv$vacuumhr5 + freqv$vacuumhr6)) # positive

vwvacuumhr <- aov(freqvw$vw ~ freqvw$vacuumhr) %>% anova_stats()
sqrt(vwvacuumhr[1,8])
summary(lm(freqvw$vw ~ freqvw$vacuumhr1 + freqvw$vacuumhr2 + freqvw$vacuumhr3 + freqvw$vacuumhr4 + freqvw$vacuumhr5 + freqvw$vacuumhr6)) # positive




