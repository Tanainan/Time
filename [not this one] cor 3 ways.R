#game wtp and eva
ggp <- data.frame(t = Time$gn)
ggo <- data.frame(t = Time$gp)

ggo$t[is.na(ggo$t)] <- ggp$t[is.na(ggo$t)]

cor.test(Time$g, ggo$t)

# sports wtp and eva
ssp <- data.frame(t = Time$sn)
sso <- data.frame(t = Time$sp)
ssm <- data.frame(t = Time$sneun)
ssn <- data.frame(t = Time$sneup)

sso$t[is.na(sso$t)] <- ssp$t[is.na(sso$t)]
sso$t[is.na(sso$t)] <- ssm$t[is.na(sso$t)]
sso$t[is.na(sso$t)] <- ssn$t[is.na(sso$t)]

cor.test(Time$s, sso$t)

#music wtp and eva
mmp <- data.frame(t = Time$mn)
mmo <- data.frame(t = Time$mp)

mmo$t[is.na(mmo$t)] <- mmp$t[is.na(mmo$t)]

cor.test(Time$m, mmo$t)

#traffic wtp and eva
ttp <- data.frame(t = Time$tn)
tto <- data.frame(t = Time$tp)
ttm <- data.frame(t = Time$tneun)
ttn <- data.frame(t = Time$tneup)

tto$t[is.na(tto$t)] <- ttp$t[is.na(tto$t)]
tto$t[is.na(tto$t)] <- ttm$t[is.na(tto$t)]
tto$t[is.na(tto$t)] <- ttn$t[is.na(tto$t)]

cor.test(Time$t, tto$t)

#dishes wtp and eva
ddp <- data.frame(t = Time$dn)
ddo <- data.frame(t = Time$dp)
ddm <- data.frame(t = Time$dneun)
ddn <- data.frame(t = Time$dneup)

ddo$t[is.na(ddo$t)] <- ddp$t[is.na(ddo$t)]
ddo$t[is.na(ddo$t)] <- ddm$t[is.na(ddo$t)]
ddo$t[is.na(ddo$t)] <- ddn$t[is.na(ddo$t)]

cor.test(Time$d, ddo$t)

# vacuum wtp and eva
vvp <- data.frame(t = Time$vn)
vvo <- data.frame(t = Time$vp)
vvm <- data.frame(t = Time$vneun)
vvn <- data.frame(t = Time$vneup)

vvo$t[is.na(vvo$t)] <- vvp$t[is.na(vvo$t)]
vvo$t[is.na(vvo$t)] <- vvm$t[is.na(vvo$t)]
vvo$t[is.na(vvo$t)] <- vvn$t[is.na(vvo$t)]

cor.test(Time$v, vvo$t)


#g wtp and 45 mins
cor.test(ggo$t, as.numeric(Time$g45))

#s wtp and 45 mins
cor.test(sso$t, as.numeric(Time$s45))

#m wtp and 45 mins
cor.test(mmo$t, as.numeric(Time$m45))

#t wtp and 45 mins
cor.test(tto$t, as.numeric(Time$t45))

#d wtp and 45 mins
cor.test(ddo$t, as.numeric(Time$d45))

#v wtp and 45 mins
cor.test(vvo$t, as.numeric(Time$v45))



#g choice and eva
cor.test(ggo$t, Time$g)
#s choice and eva
cor.test(sso$t, Time$s)
#m choice and eva
cor.test(mmo$t, Time$m)
#t choice and eva
cor.test(tto$t, Time$t)
#d choice and eva
cor.test(ddo$t, Time$d)
#v choice and eva
cor.test(vvo$t, Time$v)
