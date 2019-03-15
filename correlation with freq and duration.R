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

freq <- (Time[,c("g","game", "gamehr",
                 "s","sports","sportshr",
                 "m","music","musichr",
                 "d","dish","dishhr",
                 "t","traffic","traffichr",
                 "v","vacuum","vacuumhr")])

# eva
corstars(as.matrix(freq[,c("g","game", "gamehr")]))
corstars(as.matrix(freq[,c("s","sports","sportshr")]))
corstars(as.matrix(freq[,c("m","music","musichr")]))
corstars(as.matrix(freq[,c("d","dish","dishhr")]))
corstars(as.matrix(freq[,c("t","traffic","traffichr")]))
corstars(as.matrix(freq[,c("v","vacuum","vacuumhr")]))

# wtp
freq$gw <- ggo$t
freq$sw <- sso$t
freq$mw <- mmo$t 
freq$dw <- ddo$t
freq$tw <- tto$t
freq$vw <- vvo$t

corstars(as.matrix(freq[,c("gw","game", "gamehr")]))
corstars(as.matrix(freq[,c("sw","sports","sportshr")]))
corstars(as.matrix(freq[,c("mw","music","musichr")]))
corstars(as.matrix(freq[,c("dw","dish","dishhr")]))
corstars(as.matrix(freq[,c("tw","traffic","traffichr")]))
corstars(as.matrix(freq[,c("vw","vacuum","vacuumhr")]))


res <- rcorr(freq$sportshr, freq$sw, type = "pearson")
res

