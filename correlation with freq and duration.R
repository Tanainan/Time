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

# corstars <-function(x, method=c("pearson", "spearman"), removeTriangle=c("upper", "lower"),
#                     result=c("none", "html", "latex")){
#   #Compute correlation matrix
#   require(Hmisc)
#   x <- as.matrix(x)
#   correlation_matrix<-rcorr(x, type=method[1])
#   R <- correlation_matrix$r # Matrix of correlation coeficients
#   p <- correlation_matrix$P # Matrix of p-value 
#   
#   ## Define notions for significance levels; spacing is important.
#   mystars <- ifelse(p < .0001, "****", ifelse(p < .001, "*** ", ifelse(p < .01, "**  ", ifelse(p < .05, "*   ", "    "))))
#   
#   ## trunctuate the correlation matrix to two decimal
#   R <- format(round(cbind(rep(-1.11, ncol(x)), R), 2))[,-1]
#   
#   ## build a new matrix that includes the correlations with their apropriate stars
#   Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x))
#   diag(Rnew) <- paste(diag(R), " ", sep="")
#   rownames(Rnew) <- colnames(x)
#   colnames(Rnew) <- paste(colnames(x), "", sep="")
#   
#   ## remove upper triangle of correlation matrix
#   if(removeTriangle[1]=="upper"){
#     Rnew <- as.matrix(Rnew)
#     Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
#     Rnew <- as.data.frame(Rnew)
#   }
#   
#   ## remove lower triangle of correlation matrix
#   else if(removeTriangle[1]=="lower"){
#     Rnew <- as.matrix(Rnew)
#     Rnew[lower.tri(Rnew, diag = TRUE)] <- ""
#     Rnew <- as.data.frame(Rnew)
#   }
#   
#   ## remove last column and return the correlation matrix
#   Rnew <- cbind(Rnew[1:length(Rnew)-1])
#   if (result[1]=="none") return(Rnew)
#   else{
#     if(result[1]=="html") print(xtable(Rnew), type="html")
#     else print(xtable(Rnew), type="latex") 
#   }
# } 

# reverse coding
Time$game <- 8-Time$game
Time$sports <- 8-Time$sports
Time$music<- 8-Time$music
Time$traffic <- 8-Time$traffic
Time$dish <- 8-Time$dish
Time$vacuum <- 8-Time$vacuum
# 0 = never, 8 = daily

# Time[which(Time$game == 1), c("game")] <- NA
# Time[which(Time$sports == 1), c("sports")] <- NA
# Time[which(Time$music == 1), c("music")] <- NA
# Time[which(Time$traffic == 1), c("traffic")] <- NA
# Time[which(Time$dish == 1), c("dish")] <- NA
# Time[which(Time$vacuum == 1), c("vacuum")] <- NA
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
# corstars(as.matrix(freq[,c("g","game", "gamehr")]))
# corstars(as.matrix(freq[,c("s","sports","sportshr")]))
# corstars(as.matrix(freq[,c("m","music","musichr")]))
# corstars(as.matrix(freq[,c("d","dish","dishhr")]))
# corstars(as.matrix(freq[,c("t","traffic","traffichr")]))
# corstars(as.matrix(freq[,c("v","vacuum","vacuumhr")]))

# wtp
freq$gw <- ggo$t
freq$sw <- sso$t
freq$mw <- mmo$t 
freq$dw <- ddo$t
freq$tw <- tto$t
freq$vw <- vvo$t

# corstars(as.matrix(freq[,c("gw","game", "gamehr")]))
# corstars(as.matrix(freq[,c("sw","sports","sportshr")]))
# corstars(as.matrix(freq[,c("mw","music","musichr")]))
# corstars(as.matrix(freq[,c("dw","dish","dishhr")]))
# corstars(as.matrix(freq[,c("tw","traffic","traffichr")]))
# corstars(as.matrix(freq[,c("vw","vacuum","vacuumhr")]))


rcorr(as.matrix(freq[, 1:3]), type = "spearman")
rcorr(as.matrix(freq[, 4:6]), type = "spearman")
rcorr(as.matrix(freq[, 7:9]), type = "spearman")
rcorr(as.matrix(freq[, 10:12]), type = "spearman")
rcorr(as.matrix(freq[, 14:15]), type = "spearman")
rcorr(as.matrix(freq[, 17:18]), type = "spearman")
rcorr(as.matrix(freq$gw), as.matrix(freq$game))

aov(freq$g ~ freq$game) %>% eta_sq()
aov(freq$gw ~ freq$game) %>% eta_sq()
aov(freq$g ~ freq$gamehr) %>% eta_sq()
aov(freq$gw ~ freq$gamehr) %>% eta_sq()

aov(freq$g ~ freq$game) %>% eta_sq()
aov(freq$g ~ freq$game) %>% eta_sq()
