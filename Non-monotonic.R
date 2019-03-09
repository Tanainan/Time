setwd("~/Desktop/time 2")
library(readr)
Time <- read_csv("Time.csv", skip = 1)
Time <- Time[-c(1),]
Time <- data.frame(sapply(Time, function(x) as.numeric(as.character(x))))
library(memisc)
library(ggplot2)




######Risk preferences
#count #positive and #negative
#create new columns
Time$gg <- NA
Time$ss <- NA
Time$mm <- NA
Time$tt <- NA
Time$dd <- NA
Time$vv <- NA
for(i in 1:nrow(Time)){
  if(Time$g[i] > 0){Time$gg[i] <- 1}
  else {Time$gg[i] <- 0}
}
print(Time$gg)

for(i in 1:nrow(Time)){
  if(Time$s[i] > 0){Time$ss[i] <- 1}
  else {Time$ss[i] <- 0}
}
print(Time$ss)

for(i in 1:nrow(Time)){
  if(Time$m[i] > 0){Time$mm[i] <- 1}
  else {Time$mm[i] <- 0}
}
print(Time$mm)

for(i in 1:nrow(Time)){
  if(Time$t[i] < 0){Time$tt[i] <- 1}
  else {Time$tt[i] <- 0}
}
print(Time$tt)

for(i in 1:nrow(Time)){
  if(Time$v[i] < 0){Time$vv[i] <- 1}
  else {Time$vv[i] <- 0}
}
print(Time$vv)

for(i in 1:nrow(Time)){
  if(Time$d[i] < 0){Time$dd[i] <- 1}
  else {Time$dd[i] <- 0}
}
print(Time$dd)

Time$posi <- rowSums(Time[,c("gg","mm","ss")])
print(Time$posi)
Time$neg <- rowSums(Time[,c("vv","tt","dd")])
print(Time$neg)

#Look at responses (1 = sure and 2 = gamble) change to (1 = sure and 0 = gamble) -- including negative activities because some people might rate positive activity as negative
Time$g45[Time$g45 == 2] <- "0"
Time$m45[Time$m45 == 2] <- "0"
Time$s45[Time$s45 == 2] <- "0"
Time$v45[Time$v45 == 2] <- "0"
Time$t45[Time$t45 == 2] <- "0"
Time$d45[Time$d45 == 2] <- "0"
print(Time$g45)

#create result of countings
Time$gr <- NA
Time$sr <- NA
Time$mr <- NA
Time$tr <- NA
Time$dr <- NA
Time$vr <- NA

#if evaluate positive activity as positive and answer sure option == 1
for(i in 1:nrow(Time)){
  if(Time$gg[i] == 1 & Time$g45[i] == 1){Time$gr[i] <- 1}
  else {Time$gr[i] <- 0}
}

for(i in 1:nrow(Time)){
  if(Time$mm[i] == 1 & Time$m45[i] == 1){Time$mr[i] <- 1}
  else {Time$mr[i] <- 0}
}
print(Time$mr)

for(i in 1:nrow(Time)){
  if(Time$ss[i] == 1 & Time$s45[i] == 1){Time$sr[i] <- 1}
  else {Time$sr[i] <- 0}
}
print(Time$sr)

#if evaluate negative activities as negative and answer gamble option == 1
for(i in 1:nrow(Time)){
  if(Time$vv[i] == 1 & Time$v45[i] == 0){Time$vr[i] <- 1}
  else {Time$vr[i] <- 0}
}
print(Time$vr)

for(i in 1:nrow(Time)){
  if(Time$dd[i] == 1 & Time$d45[i] == 0){Time$dr[i] <- 1}
  else {Time$dr[i] <- 0}
}
print(Time$dr)

for(i in 1:nrow(Time)){
  if(Time$tt[i] == 1 & Time$t45[i] == 0){Time$tr[i] <- 1}
  else {Time$tr[i] <- 0}
}
print(Time$tr)

#sum up the # of responses for each individual. If chose gamble for all 3 rated negative activities == 3. If only rated 2 negative activities as negative, and chose gamble only one of the two == 1
Time$posiresponse <- rowSums(Time[,c("gr","mr","sr")])
print(Time$posiresponse)
Time$negresponse <- rowSums(Time[,c("vr","tr","dr")])
print(Time$negresponse)


#calculate percentages of gain, loss, total
#positive -- predicting risk-aversion -- choosing sure option
Time$pst <- 100*Time$posiresponse/Time$posi; Time$pst
Time$ngt <- 100*Time$negresponse/Time$neg; Time$ngt
Time$total <- 100*(Time$posiresponse + Time$negresponse)/(Time$posi + Time$neg); Time$total      

u01 <- ggplot(Time, aes(pst)) + 
  geom_bar(fill = "light pink", width = 5) +
  theme_bw() + 
  ggtitle("Time Gain and Risk-Aversion") + 
  xlab("Participants' Percentages of Choosing a Certain Option for Time Gain") +
  ylab("Count") +
  geom_text(stat = "count", aes(label = ..count.., y = ..count..), vjust = -1) + 
  scale_y_continuous(limits = c(0,165)); u01

u02 <- ggplot(Time, aes(ngt)) + 
  geom_bar(fill = "light blue", width = 5) +
  theme_bw() + 
  ggtitle("Time Loss and Risk-Seeking") + 
  xlab("Participants' Percentages of Choosing a Gamble Option for Time Loss") +
  ylab("Count") +
  geom_text(stat = "count", aes(label = ..count.., y = ..count..), vjust = -1) + 
  scale_y_continuous(limits = c(0,165)); u02

u03 <- ggplot(Time, aes(total)) + 
  geom_bar(color = "black", width = 2, fill = "white") +
  theme_bw() + 
  ggtitle("Non-Monotonic (N = 234)") + 
  xlab("Percentages of Correct Predictions of the Reflection Effect") +
  ylab("Count") +
  geom_text(stat = "count", aes(label = ..count.., y = ..count..), vjust = -1) + 
  scale_y_continuous(limits = c(0,80)) +
  scale_x_continuous(limits = c(-5, 105)); u03

grid.arrange(u01, u02)
grid.arrange(u03, u3)

mean(Time$pst)
sd(Time$pst)
mean(Time$ngt) # #48 has 0 for both numerator and denominator
which(Time$ngt == "NaN")
Time[48,c("neg", "negresponse")]
mean(Time$ngt, na.rm = T)
sd(Time$ngt, na.rm = T)
mean(Time$total)
t.test(Time$pst, y = NULL, mu = 50)
t.test(Time$ngt, y = NULL, mu = 50) # not significant
t.test(Time$total, y = NULL, mu = 50)

table(Time$pst)
table(Time$ngt)
table(Time$total)

length(which((Time$posi + Time$neg) > 0))


##########Risk behavior patterns###############

par(mfrow = c(3,1)) # then rerun each barplot individually


#consistency for g
Time$gg1 <- NA
Time$gg2 <- NA
Time$gg3 <- NA
Time$gg4 <- NA
Time$gg5 <- NA
Time$gg6 <- NA
Time$gg7 <- NA
Time$gg8 <- NA
for(i in 1:nrow(Time)){
  if(Time$g[i] > 0) {if(Time$g10[i] == 1){Time$gg1[i] <- 1} else {Time$gg1[i] <- 2}}
  else{Time$gg1[i] <- 0}}
for(i in 1:nrow(Time)){
  if(Time$g[i] > 0) {if(Time$g20[i] == 1){Time$gg2[i] <- 1} else {Time$gg2[i] <- 2}}
  else{Time$gg2[i] <- 0}}
for(i in 1:nrow(Time)){
  if(Time$g[i] > 0) {if(Time$g30[i] == 1){Time$gg3[i] <- 1} else {Time$gg3[i] <- 2}}
  else{Time$gg3[i] <- 0}}
for(i in 1:nrow(Time)){
  if(Time$g[i] > 0) {if(Time$g40[i] == 1){Time$gg4[i] <- 1} else {Time$gg4[i] <- 2}}
  else{Time$gg4[i] <- 0}}
for(i in 1:nrow(Time)){
  if(Time$g[i] > 0) {if(Time$g50[i] == 1){Time$gg5[i] <- 1} else {Time$gg5[i] <- 2}}
  else{Time$gg5[i] <- 0}}
for(i in 1:nrow(Time)){
  if(Time$g[i] > 0) {if(Time$g60[i] == 1){Time$gg6[i] <- 1} else {Time$gg6[i] <- 2}}
  else{Time$gg6[i] <- 0}}
for(i in 1:nrow(Time)){
  if(Time$g[i] > 0) {if(Time$g70[i] == 1){Time$gg7[i] <- 1} else {Time$gg7[i] <- 2}}
  else{Time$gg7[i] <- 0}}
for(i in 1:nrow(Time)){
  if(Time$g[i] > 0) {if(Time$g80[i] == 1){Time$gg8[i] <- 1} else {Time$gg8[i] <- 2}}
  else{Time$gg8[i] <- 0}}
Time$ga <- (Time$gg8*.0001 + Time$gg7*.001 + Time$gg6*.01 + Time$gg5*.1 + Time$gg4*1 + Time$gg3*10 + Time$gg2*100 + Time$gg1*1000)
options(digits=8)
y1 <- barplot(table(Time$ga), main = "Risk Behavior Patterns for Playing Games", col = "orange", ylab = "Counts", las = 2, ylim = c(0,80),
        names = c("NA", "1111.1111", "1111.1112", "1111.1122", "1111.1221", "1111.1222", "1111.2222", "1112.2222", "1122.2222", "2111.1111", "2111.1122", "2111.1222", "2211.1111", "2211.1121", "2211.1122", "2221.2111", "2221.2222", "2221.1111", "2222.1111", "2222.2111", "2222.2211", "2222.2222"))
text(y1, table(Time$ga), pos = 3, cex = 1, labels=as.character(table(Time$ga)))



# consistency for s
Time$ss1 <- NA
Time$ss2 <- NA
Time$ss3 <- NA
Time$ss4 <- NA
Time$ss5 <- NA
Time$ss6 <- NA
Time$ss7 <- NA
Time$ss8 <- NA
for(i in 1:nrow(Time)){
  if(Time$s[i] > 0) {if(Time$s10[i] == 1){Time$ss1[i] <- 1} else {Time$ss1[i] <- 2}}
  else{Time$ss1[i] <- 0}}
for(i in 1:nrow(Time)){
  if(Time$s[i] > 0) {if(Time$s20[i] == 1){Time$ss2[i] <- 1} else {Time$ss2[i] <- 2}}
  else{Time$ss2[i] <- 0}}
for(i in 1:nrow(Time)){
  if(Time$s[i] > 0) {if(Time$s30[i] == 1){Time$ss3[i] <- 1} else {Time$ss3[i] <- 2}}
  else{Time$ss3[i] <- 0}}
for(i in 1:nrow(Time)){
  if(Time$s[i] > 0) {if(Time$s40[i] == 1){Time$ss4[i] <- 1} else {Time$ss4[i] <- 2}}
  else{Time$ss4[i] <- 0}}
for(i in 1:nrow(Time)){
  if(Time$s[i] > 0) {if(Time$s50[i] == 1){Time$ss5[i] <- 1} else {Time$ss5[i] <- 2}}
  else{Time$ss5[i] <- 0}}
for(i in 1:nrow(Time)){
  if(Time$s[i] > 0) {if(Time$s60[i] == 1){Time$ss6[i] <- 1} else {Time$ss6[i] <- 2}}
  else{Time$ss6[i] <- 0}}
for(i in 1:nrow(Time)){
  if(Time$s[i] > 0) {if(Time$s70[i] == 1){Time$ss7[i] <- 1} else {Time$ss7[i] <- 2}}
  else{Time$ss7[i] <- 0}}
for(i in 1:nrow(Time)){
  if(Time$s[i] > 0) {if(Time$s80[i] == 1){Time$ss8[i] <- 1} else {Time$ss8[i] <- 2}}
  else{Time$ss8[i] <- 0}}
Time$sa <- (Time$ss8*.0001 + Time$ss7*.001 + Time$ss6*.01 + Time$ss5*.1 + Time$ss4*1 + Time$ss3*10 + Time$ss2*100 + Time$ss1*1000)
options(digits=8)
y2 <- barplot(table(Time$sa), main = "Risk Behavior Patterns for Playing Sports", col = "orange", ylim = c(0,80), ylab = "Counts", las = 2,
        names = c("NA", "1111.1111", "1111.1112", "1111.1122", "1111.1222", "1111.2222", "1112.2222", "1122.2222", "2111.1111", "2111.1112", "2111.1122", "2211.1111", "2211.1122", "2221.1111", "2221.1122", "2222.1111", "2222.2111", "2222.2211", "2222.2222"))
text(y2, table(Time$sa), pos = 3, cex = 1, labels=as.character(table(Time$sa)))

# consistency for m
Time$mm1 <- NA
Time$mm2 <- NA
Time$mm3 <- NA
Time$mm4 <- NA
Time$mm5 <- NA
Time$mm6 <- NA
Time$mm7 <- NA
Time$mm8 <- NA
for(i in 1:nrow(Time)){
  if(Time$m[i] > 0) {if(Time$m10[i] == 1){Time$mm1[i] <- 1} else {Time$mm1[i] <- 2}}
  else{Time$mm1[i] <- 0}}
for(i in 1:nrow(Time)){
  if(Time$m[i] > 0) {if(Time$m20[i] == 1){Time$mm2[i] <- 1} else {Time$mm2[i] <- 2}}
  else{Time$mm2[i] <- 0}}
for(i in 1:nrow(Time)){
  if(Time$m[i] > 0) {if(Time$m30[i] == 1){Time$mm3[i] <- 1} else {Time$mm3[i] <- 2}}
  else{Time$mm3[i] <- 0}}
for(i in 1:nrow(Time)){
  if(Time$m[i] > 0) {if(Time$m40[i] == 1){Time$mm4[i] <- 1} else {Time$mm4[i] <- 2}}
  else{Time$mm4[i] <- 0}}
for(i in 1:nrow(Time)){
  if(Time$m[i] > 0) {if(Time$m50[i] == 1){Time$mm5[i] <- 1} else {Time$mm5[i] <- 2}}
  else{Time$mm5[i] <- 0}}
for(i in 1:nrow(Time)){
  if(Time$m[i] > 0) {if(Time$m60[i] == 1){Time$mm6[i] <- 1} else {Time$mm6[i] <- 2}}
  else{Time$mm6[i] <- 0}}
for(i in 1:nrow(Time)){
  if(Time$m[i] > 0) {if(Time$m70[i] == 1){Time$mm7[i] <- 1} else {Time$mm7[i] <- 2}}
  else{Time$mm7[i] <- 0}}
for(i in 1:nrow(Time)){
  if(Time$m[i] > 0) {if(Time$m80[i] == 1){Time$mm8[i] <- 1} else {Time$mm8[i] <- 2}}
  else{Time$mm8[i] <- 0}}
Time$ma <- (Time$mm8*.0001 + Time$mm7*.001 + Time$mm6*.01 + Time$mm5*.1 + Time$mm4*1 + Time$mm3*10 + Time$mm2*100 + Time$mm1*1000)
options(digits=8)
y3 <- barplot(table(Time$ma), main = "Risk Behavior Patterns for Listening to Music", ylim = c(0,80), ylab = "Counts", las = 2, col = "orange",
        names = c("1111.1111", "1111.1112", "1111.1122", "1111.1222", "1111.2221", "1111.2222", "1112.2222", "1122.2111", "1211.1111", "1221.1111", "2111.1111", "2111.1122", "2211.1111", "2211.1222", "2211.2222", "2221.1111", "2221.1112", "2222.1111", "2222.2111", "2222.2211", "2222.2222"))
text(y3, table(Time$ma), pos = 3, cex = 1, labels=as.character(table(Time$ma)))


par(mfrow = c(3,1)) # then rerun each barplot individually

# consistency for t
Time$tt1 <- NA
Time$tt2 <- NA
Time$tt3 <- NA
Time$tt4 <- NA
Time$tt5 <- NA
Time$tt6 <- NA
Time$tt7 <- NA
Time$tt8 <- NA

for(i in 1:nrow(Time)){
  if(Time$t[i] < 0) {if(Time$t10[i] == 1){Time$tt1[i] <- -1} else {Time$tt1[i] <- -2}}
  else{Time$tt1[i] <- 0}}
for(i in 1:nrow(Time)){
  if(Time$t[i] < 0) {if(Time$t20[i] == 1){Time$tt2[i] <- -1} else {Time$tt2[i] <- -2}}
  else{Time$tt2[i] <- 0}}
for(i in 1:nrow(Time)){
  if(Time$t[i] < 0) {if(Time$t30[i] == 1){Time$tt3[i] <- -1} else {Time$tt3[i] <- -2}}
  else{Time$tt3[i] <- 0}}
for(i in 1:nrow(Time)){
  if(Time$t[i] < 0) {if(Time$t40[i] == 1){Time$tt4[i] <- -1} else {Time$tt4[i] <- -2}}
  else{Time$tt4[i] <- 0}}
for(i in 1:nrow(Time)){
  if(Time$t[i] < 0) {if(Time$t50[i] == 1){Time$tt5[i] <- -1} else {Time$tt5[i] <- -2}}
  else{Time$tt5[i] <- 0}}
for(i in 1:nrow(Time)){
  if(Time$t[i] < 0) {if(Time$t60[i] == 1){Time$tt6[i] <- -1} else {Time$tt6[i] <- -2}}
  else{Time$tt6[i] <- 0}}
for(i in 1:nrow(Time)){
  if(Time$t[i] < 0) {if(Time$t70[i] == 1){Time$tt7[i] <- -1} else {Time$tt7[i] <- -2}}
  else{Time$tt7[i] <- 0}}
for(i in 1:nrow(Time)){
  if(Time$t[i] < 0) {if(Time$t80[i] == 1){Time$tt8[i] <- -1} else {Time$tt8[i] <- -2}}
  else{Time$tt8[i] <- 0}}
print(Time$tt5)
options(digits=8)
Time$ta <- (Time$tt8*.0001 + Time$tt7*.001 + Time$tt6*.01 + Time$tt5*.1 + Time$tt4*1 + Time$tt3*10 + Time$tt2*100 + Time$tt1*1000); Time$ta
y4 <- barplot(table(Time$ta), ylim = c(0,80), col = "light blue", main = "Risk Behavior Patterns for Getting Stuck in a Traffic Jam", ylab = "Counts", las = 2, 
        names = c("2222.2222", "2222.1111", "2111.2222", "1222.2222", "1221.1122", "1122.2222", "1112.2222", "1112.2221", "1111.2222", "1111.2122", "1111.1222", "1111.1122", "1111.1112", "1111.1111", "NA"))
text(y4, table(Time$ta), pos = 3, cex = 1, labels=as.character(table(Time$ta)))

# consistency for v
Time$vv1 <- NA
Time$vv2 <- NA
Time$vv3 <- NA
Time$vv4 <- NA
Time$vv5 <- NA
Time$vv6 <- NA
Time$vv7 <- NA
Time$vv8 <- NA

for(i in 1:nrow(Time)){
  if(Time$v[i] < 0) {if(Time$v10[i] == 1){Time$vv1[i] <- -1} else {Time$vv1[i] <- -2}}
  else{Time$vv1[i] <- 0}}
for(i in 1:nrow(Time)){
  if(Time$v[i] < 0) {if(Time$v20[i] == 1){Time$vv2[i] <- -1} else {Time$vv2[i] <- -2}}
  else{Time$vv2[i] <- 0}}
for(i in 1:nrow(Time)){
  if(Time$v[i] < 0) {if(Time$v30[i] == 1){Time$vv3[i] <- -1} else {Time$vv3[i] <- -2}}
  else{Time$vv3[i] <- 0}}
for(i in 1:nrow(Time)){
  if(Time$v[i] < 0) {if(Time$v40[i] == 1){Time$vv4[i] <- -1} else {Time$vv4[i] <- -2}}
  else{Time$vv4[i] <- 0}}
for(i in 1:nrow(Time)){
  if(Time$v[i] < 0) {if(Time$v50[i] == 1){Time$vv5[i] <- -1} else {Time$vv5[i] <- -2}}
  else{Time$vv5[i] <- 0}}
for(i in 1:nrow(Time)){
  if(Time$v[i] < 0) {if(Time$v60[i] == 1){Time$vv6[i] <- -1} else {Time$vv6[i] <- -2}}
  else{Time$vv6[i] <- 0}}
for(i in 1:nrow(Time)){
  if(Time$v[i] < 0) {if(Time$v70[i] == 1){Time$vv7[i] <- -1} else {Time$vv7[i] <- -2}}
  else{Time$vv7[i] <- 0}}
for(i in 1:nrow(Time)){
  if(Time$v[i] < 0) {if(Time$v80[i] == 1){Time$vv8[i] <- -1} else {Time$vv8[i] <- -2}}
  else{Time$vv8[i] <- 0}}
options(digits=8)
Time$va <- (Time$vv8*.0001 + Time$vv7*.001 + Time$vv6*.01 + Time$vv5*.1 + Time$vv4*1 + Time$vv3*10 + Time$vv2*100 + Time$vv1*1000); Time$va
y5 <- barplot(table(Time$va), ylim = c(0,70), col = "light blue", main = "Risk Behavior Patterns for Vacuuming the Theater", ylab = "Counts", las = 2, 
        names = c("2222.2222", "2221.1111", "2122.2222", "1222.2222", "1221.1222", "1122.2222", "1112.2222", "1112.2122", "1111.2222", "1111.1222", "1111.1122", "1111.1112", "1111.1111", "NA"))
text(y5, table(Time$va), pos = 3, cex = 1, labels=as.character(table(Time$va)))

# consistency for d
Time$dd1 <- NA
Time$dd2 <- NA
Time$dd3 <- NA
Time$dd4 <- NA
Time$dd5 <- NA
Time$dd6 <- NA
Time$dd7 <- NA
Time$dd8 <- NA

for(i in 1:nrow(Time)){
  if(Time$d[i] < 0) {if(Time$d10[i] == 1){Time$dd1[i] <- -1} else {Time$dd1[i] <- -2}}
  else{Time$dd1[i] <- 0}}
for(i in 1:nrow(Time)){
  if(Time$d[i] < 0) {if(Time$d20[i] == 1){Time$dd2[i] <- -1} else {Time$dd2[i] <- -2}}
  else{Time$dd2[i] <- 0}}
for(i in 1:nrow(Time)){
  if(Time$d[i] < 0) {if(Time$d30[i] == 1){Time$dd3[i] <- -1} else {Time$dd3[i] <- -2}}
  else{Time$dd3[i] <- 0}}
for(i in 1:nrow(Time)){
  if(Time$d[i] < 0) {if(Time$d40[i] == 1){Time$dd4[i] <- -1} else {Time$dd4[i] <- -2}}
  else{Time$dd4[i] <- 0}}
for(i in 1:nrow(Time)){
  if(Time$d[i] < 0) {if(Time$d50[i] == 1){Time$dd5[i] <- -1} else {Time$dd5[i] <- -2}}
  else{Time$dd5[i] <- 0}}
for(i in 1:nrow(Time)){
  if(Time$d[i] < 0) {if(Time$d60[i] == 1){Time$dd6[i] <- -1} else {Time$dd6[i] <- -2}}
  else{Time$dd6[i] <- 0}}
for(i in 1:nrow(Time)){
  if(Time$d[i] < 0) {if(Time$d70[i] == 1){Time$dd7[i] <- -1} else {Time$dd7[i] <- -2}}
  else{Time$dd7[i] <- 0}}
for(i in 1:nrow(Time)){
  if(Time$d[i] < 0) {if(Time$d80[i] == 1){Time$dd8[i] <- -1} else {Time$dd8[i] <- -2}}
  else{Time$dd8[i] <- 0}}
options(digits=8)
Time$da <- (Time$dd8*.0001 + Time$dd7*.001 + Time$dd6*.01 + Time$dd5*.1 + Time$dd4*1 + Time$dd3*10 + Time$dd2*100 + Time$dd1*1000); Time$da
y6 <- barplot(table(Time$da), col = "light blue", main = "Risk Behavior Patterns for Washing Dishes", ylim = c(0,80), las = 2, 
        names = c("2222.2222", "2221.1122", "2122.2222", "2111.2222", "1222.2222", "1122.2222", "1112.2222", "1112.1222", "1111.2222", "1111.1222", "1111.1122", "1111.1112", "1111.1111", "NA"), ylab = "Counts")
text(y6, table(Time$da), pos = 3, cex = 1, labels=as.character(table(Time$da)))


############### Mixed gamble ###########
# 1 = neither actitiyies and 2 = gamble
Time$l1 <- NA
Time$l2 <- NA
Time$l3 <- NA
#choosing a sure option = loss averse
for(i in 1:nrow(Time)){
  if(Time$gd0[i] == 1 & Time$g[i] > 0 & Time$d[i] < 0){Time$l1[i] <- 1} else
  {Time$l1[i] <- 0}
}
print(Time$l1)

for(i in 1:nrow(Time)){
  if(Time$st0[i] == 1 & Time$s[i] > 0 & Time$t[i] < 0){Time$l2[i] <- 1} else
  {Time$l2[i] <- 0}
}
print(Time$l2)

for(i in 1:nrow(Time)){
  if(Time$mv0[i] == 1 & Time$m[i] > 0 & Time$v[i] < 0){Time$l3[i] <- 1} else
  {Time$l3[i] <- 0}
}
print(Time$l3)

Time$loss <- Time$l1 + Time$l2 + Time$l3; Time$loss
Time$lp <- 100*Time$loss/3; Time$lp

hist(Time$lp, main = "Loss Aversion & Time", xlab = "Participants' Percentages of Loss Aversion")
t.test(Time$lp, y = NULL, mu = 50)

#Consistency Mixed Gamble

par(mfrow = c(3,1)) # then rerun each barplot individually


#gd
Time$gdn4 <- NA
Time$gdn3 <- NA
Time$gdn2 <- NA
Time$gdn1 <- NA
Time$gdp1 <- NA
Time$gdp2 <- NA
Time$gdp3 <- NA
Time$gdp4 <- NA
for(i in 1:nrow(Time)){
  if(Time$g[i] > 0 & Time$d[i] < 0) {if(Time$gd.40[i] == 1){Time$gdn4[i] <- -1} else {Time$gdn4[i] <- -2}}
  else{Time$gdn4[i] <- 0}}
for(i in 1:nrow(Time)){
  if(Time$g[i] > 0 & Time$d[i] < 0) {if(Time$gd.30[i] == 1){Time$gdn3[i] <- -1} else {Time$gdn3[i] <- -2}}
  else{Time$gdn3[i] <- 0}}
for(i in 1:nrow(Time)){
  if(Time$g[i] > 0 & Time$d[i] < 0) {if(Time$gd.20[i] == 1){Time$gdn2[i] <- -1} else {Time$gdn2[i] <- -2}}
  else{Time$gdn2[i] <- 0}}
for(i in 1:nrow(Time)){
  if(Time$g[i] > 0 & Time$d[i] < 0) {if(Time$gd.10[i] == 1){Time$gdn1[i] <- -1} else {Time$gdn1[i] <- -2}}
  else{Time$gdn1[i] <- 0}}
for(i in 1:nrow(Time)){
  if(Time$g[i] > 0 & Time$d[i] < 0) {if(Time$gd10[i] == 1){Time$gdp1[i] <- -1} else {Time$gdp1[i] <- -2}}
  else{Time$gdp1[i] <- 0}}
for(i in 1:nrow(Time)){
  if(Time$g[i] > 0 & Time$d[i] < 0) {if(Time$gd20[i] == 1){Time$gdp2[i] <- -1} else {Time$gdp2[i] <- -2}}
  else{Time$gdp2[i] <- 0}}
for(i in 1:nrow(Time)){
  if(Time$g[i] > 0 & Time$d[i] < 0) {if(Time$gd30[i] == 1){Time$gdp3[i] <- -1} else {Time$gdp3[i] <- -2}}
  else{Time$gdp3[i] <- 0}}
for(i in 1:nrow(Time)){
  if(Time$g[i] > 0 & Time$d[i] < 0) {if(Time$gd40[i] == 1){Time$gdp4[i] <- -1} else {Time$gdp4[i] <- -2}}
  else{Time$gdp4[i] <- 0}}
options(digits=8)
Time$gda <- (Time$gdp4*.0001 + Time$gdp3*.001 + Time$gdp2*.01 + Time$gdp1*.1 + Time$gdn1*1 + Time$gdn2*10 + Time$gdn3*100 + Time$gdn4*1000); Time$gda
y7 <- barplot(table(Time$gda), ylim = c(0,100), main = "Risk Behavior Patterns for Playing Games and Washing Dishes", col = "white", las = 2, 
        names = c("2222.2222", "2222.2221", "2222.2211", "2222.2111", "2222.1111", "2221.2211", "2221.2111", "2221.1122", "2221.1112", "2221.1111", "2211.2221", "2211.2211", "2211.2111", "2211.1112", "2211.1111", "2111.2111", "2111.1111", "1111.1111", "NA"), ylab = "Counts")
text(y7, table(Time$gda), pos = 3, cex = 1, labels=as.character(table(Time$gda)))



#mv
Time$mvn4 <- NA
Time$mvn3 <- NA
Time$mvn2 <- NA
Time$mvn1 <- NA
Time$mvp1 <- NA
Time$mvp2 <- NA
Time$mvp3 <- NA
Time$mvp4 <- NA
for(i in 1:nrow(Time)){
  if(Time$m[i] > 0 & Time$v[i] < 0) {if(Time$mv.40[i] == 1){Time$mvn4[i] <- -1} else {Time$mvn4[i] <- -2}}
  else{Time$mvn4[i] <- 0}}
for(i in 1:nrow(Time)){
  if(Time$m[i] > 0 & Time$v[i] < 0) {if(Time$mv.30[i] == 1){Time$mvn3[i] <- -1} else {Time$mvn3[i] <- -2}}
  else{Time$mvn3[i] <- 0}}
for(i in 1:nrow(Time)){
  if(Time$m[i] > 0 & Time$v[i] < 0) {if(Time$mv.20[i] == 1){Time$mvn2[i] <- -1} else {Time$mvn2[i] <- -2}}
  else{Time$mvn2[i] <- 0}}
for(i in 1:nrow(Time)){
  if(Time$m[i] > 0 & Time$v[i] < 0) {if(Time$mv.10[i] == 1){Time$mvn1[i] <- -1} else {Time$mvn1[i] <- -2}}
  else{Time$mvn1[i] <- 0}}
for(i in 1:nrow(Time)){
  if(Time$m[i] > 0 & Time$v[i] < 0) {if(Time$mv10[i] == 1){Time$mvp1[i] <- -1} else {Time$mvp1[i] <- -2}}
  else{Time$mvp1[i] <- 0}}
for(i in 1:nrow(Time)){
  if(Time$m[i] > 0 & Time$v[i] < 0) {if(Time$mv20[i] == 1){Time$mvp2[i] <- -1} else {Time$mvp2[i] <- -2}}
  else{Time$mvp2[i] <- 0}}
for(i in 1:nrow(Time)){
  if(Time$m[i] > 0 & Time$v[i] < 0) {if(Time$mv30[i] == 1){Time$mvp3[i] <- -1} else {Time$mvp3[i] <- -2}}
  else{Time$mvp3[i] <- 0}}
for(i in 1:nrow(Time)){
  if(Time$m[i] > 0 & Time$v[i] < 0) {if(Time$mv40[i] == 1){Time$mvp4[i] <- -1} else {Time$mvp4[i] <- -2}}
  else{Time$mvp4[i] <- 0}}
options(digits=8)
Time$mva <- (Time$mvp4*.0001 + Time$mvp3*.001 + Time$mvp2*.01 + Time$mvp1*.1 + Time$mvn1*1 + Time$mvn2*10 + Time$mvn3*100 + Time$mvn4*1000); Time$mva
y8 <- barplot(table(Time$mva), ylim = c(0,100), main = "Risk Behavior Patterns for Listening to Music and Vacuuming the Theater", col = "white", las = 2, 
        names = c("2222.2222", "2222.2111", "2222.1111", "2221.2211", "2221.2111", "2221.1111", "2211.2211", "2211.2111", "2211.1112", "2211.1111", "2111.2111", "2111.1111", "1111.1111", "NA"), ylab = "Counts")
text(y8, table(Time$mva), pos = 3, cex = 1, labels=as.character(table(Time$mva)))

#st
Time$stn4 <- NA
Time$stn3 <- NA
Time$stn2 <- NA
Time$stn1 <- NA
Time$stp1 <- NA
Time$stp2 <- NA
Time$stp3 <- NA
Time$stp4 <- NA
for(i in 1:nrow(Time)){
  if(Time$s[i] > 0 & Time$t[i] < 0) {if(Time$st.40[i] == 1){Time$stn4[i] <- -1} else {Time$stn4[i] <- -2}}
  else{Time$stn4[i] <- 0}}
for(i in 1:nrow(Time)){
  if(Time$s[i] > 0 & Time$t[i] < 0) {if(Time$st.30[i] == 1){Time$stn3[i] <- -1} else {Time$stn3[i] <- -2}}
  else{Time$stn3[i] <- 0}}
for(i in 1:nrow(Time)){
  if(Time$s[i] > 0 & Time$t[i] < 0) {if(Time$st.20[i] == 1){Time$stn2[i] <- -1} else {Time$stn2[i] <- -2}}
  else{Time$stn2[i] <- 0}}
for(i in 1:nrow(Time)){
  if(Time$s[i] > 0 & Time$t[i] < 0) {if(Time$st.10[i] == 1){Time$stn1[i] <- -1} else {Time$stn1[i] <- -2}}
  else{Time$stn1[i] <- 0}}
for(i in 1:nrow(Time)){
  if(Time$s[i] > 0 & Time$t[i] < 0) {if(Time$st10[i] == 1){Time$stp1[i] <- -1} else {Time$stp1[i] <- -2}}
  else{Time$stp1[i] <- 0}}
for(i in 1:nrow(Time)){
  if(Time$s[i] > 0 & Time$t[i] < 0) {if(Time$st20[i] == 1){Time$stp2[i] <- -1} else {Time$stp2[i] <- -2}}
  else{Time$stp2[i] <- 0}}
for(i in 1:nrow(Time)){
  if(Time$s[i] > 0 & Time$t[i] < 0) {if(Time$st30[i] == 1){Time$stp3[i] <- -1} else {Time$stp3[i] <- -2}}
  else{Time$stp3[i] <- 0}}
for(i in 1:nrow(Time)){
  if(Time$s[i] > 0 & Time$t[i] < 0) {if(Time$st40[i] == 1){Time$stp4[i] <- -1} else {Time$stp4[i] <- -2}}
  else{Time$stp4[i] <- 0}}
options(digits=8)
Time$sta <- (Time$stp4*.0001 + Time$stp3*.001 + Time$stp2*.01 + Time$stp1*.1 + Time$stn1*1 + Time$stn2*10 + Time$stn3*100 + Time$stn4*1000); Time$sta
y9 <- barplot(table(Time$sta), ylim = c(0,100), main = "Risk Behavior Patterns for Playing Sports and Getting Stuck in a Traffic", col = "white", las = 2, 
        names = c("2222.2211", "2222.2111", "2222.1111",  "2221.2211", "2221.2111", "2221.1111", "2211.2211", "2211.2111", "2211.1111", "2111.2111",
                  "2111.1112", "2111.1111", "1111.1111", "NA"), ylab = "Counts")
text(y9, table(Time$sta), pos = 3, cex = 1, labels=as.character(table(Time$sta)))






###### New graph (curve)
###### mixed gamble
dg <- Time[Time$g > 0 & Time$d < 0,]
ts <- Time[Time$s > 0 & Time$t < 0,]
vm <- Time[Time$m > 0 & Time$v < 0,]

###### Find proportions
#dg
dg$dgn4 <- NA
dg$dgn3 <- NA
dg$dgn2 <- NA
dg$dgn1 <- NA
dg$dgz <- NA
dg$dgp1 <- NA
dg$dgp2 <- NA
dg$dgp3 <- NA
dg$dgp4 <- NA
for(i in 1:nrow(dg)){
  if(dg$gd.40[i] == 1) {dg$dgn4[i] <- 1} else {dg$dgn4[i] <- 0}}
print(dg$dgn4)
for(i in 1:nrow(dg)){
  if(dg$gd.30[i] == 1) {dg$dgn3[i] <- 1} else {dg$dgn3[i] <- 0}}
print(dg$dgn3)
for(i in 1:nrow(dg)){
  if(dg$gd.20[i] == 1) {dg$dgn2[i] <- 1} else {dg$dgn2[i] <- 0}}
print(dg$dgn2)
for(i in 1:nrow(dg)){
  if(dg$gd.10[i] == 1) {dg$dgn1[i] <- 1} else {dg$dgn1[i] <- 0}}
print(dg$dgn1)
for(i in 1:nrow(dg)){
  if(dg$gd0[i] == 1) {dg$dgz[i] <- 1} else {dg$dgz[i] <- 0}}
print(dg$dgz) ###### not consistent
for(i in 1:nrow(dg)){
  if(dg$gd10[i] == 1) {dg$dgp1[i] <- 1} else {dg$dgp1[i] <- 0}}
print(dg$dgp1)
for(i in 1:nrow(dg)){
  if(dg$gd20[i] == 1) {dg$dgp2[i] <- 1} else {dg$dgp2[i] <- 0}}
print(dg$dgp2)
for(i in 1:nrow(dg)){
  if(dg$gd30[i] == 1) {dg$dgp3[i] <- 1} else {dg$dgp3[i] <- 0}}
print(dg$dgp3)
for(i in 1:nrow(dg)){
  if(dg$gd40[i] == 1) {dg$dgp4[i] <- 1} else {dg$dgp4[i] <- 0}}
print(dg$dgp4)

#calculate proportion
#gd
gd40n <- nrow(dg[dg$dgn4 == 1,])/nrow(dg); gd40n
gd30n <- nrow(dg[dg$dgn3 == 1,])/nrow(dg); gd30n
gd20n <- nrow(dg[dg$dgn2 == 1,])/nrow(dg); gd20n
gd10n <- nrow(dg[dg$dgn1 == 1,])/nrow(dg); gd10n
gdz <- nrow(dg[dg$dgz == 1,])/nrow(dg); gdz
gd10p <- nrow(dg[dg$dgp1 == 1,])/nrow(dg); gd10p
gd20p <- nrow(dg[dg$dgp2 == 1,])/nrow(dg); gd20p
gd30p <- nrow(dg[dg$dgp3 == 1,])/nrow(dg); gd30p
gd40p <- nrow(dg[dg$dgp4 == 1,])/nrow(dg); gd40p

pgd <- data.frame(Time = c("-40","-30","-20","-10","0","10","20","30","40"), Proportion = c(gd40n, gd30n, gd20n, gd10n, gdz, gd10p, gd20p, gd30p, gd40p)) 
ggplot(pgd, aes(Time, Proportion, group = 1)) + 
  geom_point() + 
  scale_x_discrete(limits=c("-40","-30","-20","-10","0","10","20","30","40")) +
  geom_line() + 
  ggtitle("Proportions of Dishes and Games") +
  theme(plot.title = element_text(hjust=0.5))


#ts
ts$tsn4 <- NA
ts$tsn3 <- NA
ts$tsn2 <- NA
ts$tsn1 <- NA
ts$tsz <- NA
ts$tsp1 <- NA
ts$tsp2 <- NA
ts$tsp3 <- NA
ts$tsp4 <- NA
for(i in 1:nrow(ts)){
  if(ts$st.40[i] == 1) {ts$tsn4[i] <- 1} else {ts$tsn4[i] <- 0}}
print(ts$tsn4)
for(i in 1:nrow(ts)){
  if(dg$st.30[i] == 1) {ts$tsn3[i] <- 1} else {ts$tsn3[i] <- 0}}
print(ts$tsn3)
for(i in 1:nrow(ts)){
  if(ts$st.20[i] == 1) {ts$tsn2[i] <- 1} else {ts$tsn2[i] <- 0}}
print(ts$tsn2)
for(i in 1:nrow(ts)){
  if(ts$st.10[i] == 1) {ts$tsn1[i] <- 1} else {ts$tsn1[i] <- 0}}
print(ts$tsn1)
for(i in 1:nrow(ts)){
  if(ts$st0[i] == 1) {ts$tsz[i] <- 1} else {ts$tsz[i] <- 0}}
print(ts$tsz) ###### not consistent
for(i in 1:nrow(ts)){
  if(ts$st10[i] == 1) {ts$tsp1[i] <- 1} else {ts$tsp1[i] <- 0}}
print(ts$tsp1)
for(i in 1:nrow(ts)){
  if(ts$st20[i] == 1) {ts$tsp2[i] <- 1} else {ts$tsp2[i] <- 0}}
print(ts$tsp2)
for(i in 1:nrow(ts)){
  if(ts$st30[i] == 1) {ts$tsp3[i] <- 1} else {ts$tsp3[i] <- 0}}
print(ts$tsp3)
for(i in 1:nrow(ts)){
  if(ts$st40[i] == 1) {ts$tsp4[i] <- 1} else {ts$tsp4[i] <- 0}}
print(ts$tsp4)

#calculate proportion
#n40
ts40n <- nrow(ts[ts$tsn4 == 1,])/nrow(ts); ts40n
ts30n <- nrow(ts[ts$tsn3 == 1,])/nrow(ts); ts30n
ts20n <- nrow(ts[ts$tsn2 == 1,])/nrow(ts); ts20n
ts10n <- nrow(ts[ts$tsn1 == 1,])/nrow(ts); ts10n
tsz <- nrow(ts[ts$tsz == 1,])/nrow(ts); tsz
ts10p <- nrow(ts[ts$tsp1 == 1,])/nrow(ts); ts10p
ts20p <- nrow(ts[ts$tsp2 == 1,])/nrow(ts); ts20p
ts30p <- nrow(ts[ts$tsp3 == 1,])/nrow(ts); ts30p
ts40p <- nrow(ts[ts$tsp4 == 1,])/nrow(ts); ts40p

pts <- data.frame(Time = c("-40","-30","-20","-10","0","10","20","30","40"), Proportion = c(ts40n, ts30n, ts20n, ts10n, tsz, ts10p, ts20p, ts30p, ts40p)) 
ggplot(pts, aes(Time, Proportion, group = 1)) + 
  geom_point() + 
  scale_x_discrete(limits=c("-40","-30","-20","-10","0","10","20","30","40")) +
  geom_line() +
  ggtitle("Proportions of Traffic and Sports") +
  theme(plot.title = element_text(hjust=0.5))

#vm
vm$vmn4 <- NA
vm$vmn3 <- NA
vm$vmn2 <- NA
vm$vmn1 <- NA
vm$vmz <- NA
vm$vmp1 <- NA
vm$vmp2 <- NA
vm$vmp3 <- NA
vm$vmp4 <- NA
for(i in 1:nrow(vm)){
  if(vm$mv.40[i] == 1) {vm$vmn4[i] <- 1} else {vm$vmn4[i] <- 0}}
print(vm$vmn4)
for(i in 1:nrow(vm)){
  if(vm$mv.30[i] == 1) {vm$vmn3[i] <- 1} else {vm$vmn3[i] <- 0}}
print(vm$vmn3)
for(i in 1:nrow(vm)){
  if(vm$mv.20[i] == 1) {vm$vmn2[i] <- 1} else {vm$vmn2[i] <- 0}}
print(vm$vmn2)
for(i in 1:nrow(vm)){
  if(vm$mv.10[i] == 1) {vm$vmn1[i] <- 1} else {vm$vmn1[i] <- 0}}
print(vm$vmn1)
for(i in 1:nrow(vm)){
  if(vm$mv0[i] == 1) {vm$vmz[i] <- 1} else {vm$vmz[i] <- 0}}
print(vm$vmz) ###### not consistent
for(i in 1:nrow(vm)){
  if(vm$mv10[i] == 1) {vm$vmp1[i] <- 1} else {vm$vmp1[i] <- 0}}
print(vm$vmp1)
for(i in 1:nrow(vm)){
  if(vm$mv20[i] == 1) {vm$vmp2[i] <- 1} else {vm$vmp2[i] <- 0}}
print(vm$vmp2)
for(i in 1:nrow(vm)){
  if(vm$mv30[i] == 1) {vm$vmp3[i] <- 1} else {vm$vmp3[i] <- 0}}
print(vm$vmp3)
for(i in 1:nrow(vm)){
  if(vm$mv40[i] == 1) {vm$vmp4[i] <- 1} else {vm$vmp4[i] <- 0}}
print(vm$vmp4)

#calculate proportion
vm40n <- nrow(vm[vm$vmn4 == 1,])/nrow(vm); vm40n
vm30n <- nrow(vm[vm$vmn3 == 1,])/nrow(vm); vm30n
vm20n <- nrow(vm[vm$vmn2 == 1,])/nrow(vm); vm20n
vm10n <- nrow(vm[vm$vmn1 == 1,])/nrow(vm); vm10n
vmz <- nrow(vm[vm$vmz == 1,])/nrow(vm); vmz
vm10p <- nrow(vm[vm$vmp1 == 1,])/nrow(vm); vm10p
vm20p <- nrow(vm[vm$vmp2 == 1,])/nrow(vm); vm20p
vm30p <- nrow(vm[vm$vmp3 == 1,])/nrow(vm); vm30p
vm40p <- nrow(vm[vm$vmp4 == 1,])/nrow(vm); vm40p

pvm <- data.frame(Time = c("-40","-30","-20","-10","0","10","20","30","40"), Proportion = c(vm40n, vm30n, vm20n, vm10n, vmz, vm10p, vm20p, vm30p, vm40p)) 
ggplot(pvm, aes(Time, Proportion, group = 1)) + 
  geom_point() + 
  scale_x_discrete(limits=c("-40","-30","-20","-10","0","10","20","30","40")) +
  geom_line() +
  ggtitle("Proportions of Vacuum and Music") +
  theme(plot.title = element_text(hjust=0.5))


#####graph for mixed activities combined
mixed <- rbind(pts, pvm, pgd)
mixed$type <- factor(c(rep(c("Traffic Jam & Sports"), times = 9),rep(c("Vacuum & Music"), times = 9),rep(c("Dishes & Games"), times = 9))) 

ggplot(data = mixed, aes(x = Time, y = Proportion, group = type)) + 
  geom_point(aes(col = type)) + 
  scale_x_discrete(limits=c("-40","-30","-20","-10","0","10","20","30","40")) +
  geom_line(aes(col = type)) +
  scale_color_brewer(palette="Paired") + 
  theme_bw() +
  labs(color = "Activity Pair", title = "Proportions People Who Chose Certain Options", subtitle = "for Disiked (Negative) and Liked (Positive) Activities")


# descriptive of risk beh for mixed gamble
gdz <- nrow(dg[dg$dgz == 1,])/nrow(dg); gdz
tsz <- nrow(ts[ts$tsz == 1,])/nrow(ts); tsz
vmz <- nrow(vm[vm$vmz == 1,])/nrow(vm); vmz

# overall
# certain option for overall
nrow(Time[which(Time$gd0 == 1),])/234
nrow(Time[which(Time$st0 == 1),])/234
nrow(Time[which(Time$mv0 == 1),])/234
t.test(as.numeric(Time$gd0), mu = 0.5)
t.test(as.numeric(Time$st0), mu = 0.5)
t.test(as.numeric(Time$mv0), mu = 0.5)



# congruent responses
nrow(Time[which(Time$g > 0 & Time$d < 0),])
t.test(as.numeric(Time[which(Time$g > 0 & Time$d < 0),c("gd0")]), mu = 0.5)
nrow(Time[which(Time$s > 0 & Time$t < 0),])
t.test(as.numeric(Time[which(Time$s > 0 & Time$t < 0),c("st0")]), mu = 0.5)
nrow(Time[which(Time$m > 0 & Time$v < 0),])
t.test(as.numeric(Time[which(Time$m > 0 & Time$v < 0),c("mv0")]), mu = 0.5)

# congruent certain option
nrow(Time[which(Time$g > 0 & Time$d < 0 & Time$gd0 == 1),])/218
nrow(Time[which(Time$s > 0 & Time$t < 0 & Time$st0 == 1),])/213
nrow(Time[which(Time$m > 0 & Time$v < 0 & Time$mv0 == 1),])/208

# monotonic
nrow(Time[which(Time$g > 0 & Time$d < 0 & Time$gd0 == 1 & Time$g01 == 2 & Time$d01 == 1),])/168
nrow(Time[which(Time$s > 0 & Time$t < 0 & Time$st0 == 1 & Time$s01 == 2 & Time$t01 == 1),])/167
nrow(Time[which(Time$m > 0 & Time$v < 0 & Time$mv0 == 1 & Time$m01 == 2 & Time$v01 == 1),])/201
t.test(as.numeric(Time[which(Time$g > 0 & Time$d < 0 & Time$g01 == 2 & Time$d01 == 1),c("gd0")]), mu = 0.5)
t.test(as.numeric(Time[which(Time$s > 0 & Time$t < 0 & Time$s01 == 2 & Time$t01 == 1),c("st0")]), mu = 0.5)
t.test(as.numeric(Time[which(Time$m > 0 & Time$v < 0 & Time$m01 == 2 & Time$v01 == 1),c("mv0")]), mu = 0.5)


#####Gain only
g. <- Time[Time$g > 0,]
s. <- Time[Time$s > 0,]
m. <- Time[Time$m > 0,]

#g.
g.$g1 <- NA
g.$g2 <- NA
g.$g3 <- NA
g.$g4 <- NA
g.$g5 <- NA
g.$g6 <- NA
g.$g7 <- NA
g.$g8 <- NA

for(i in 1:nrow(g.)){
  if(g.$g10[i] == 1) {g.$g1[i] <- 1} else {g.$g1[i] <- 0}}
print(g.$g1)
for(i in 1:nrow(g.)){
  if(g.$g20[i] == 1) {g.$g2[i] <- 1} else {g.$g2[i] <- 0}}
print(g.$g2)
for(i in 1:nrow(g.)){
  if(g.$g30[i] == 1) {g.$g3[i] <- 1} else {g.$g3[i] <- 0}}
print(g.$g3)
for(i in 1:nrow(g.)){
  if(g.$g40[i] == 1) {g.$g4[i] <- 1} else {g.$g4[i] <- 0}}
print(g.$g4)
for(i in 1:nrow(g.)){
  if(g.$g50[i] == 1) {g.$g5[i] <- 1} else {g.$g5[i] <- 0}}
print(g.$g5)
for(i in 1:nrow(g.)){
  if(g.$g60[i] == 1) {g.$g6[i] <- 1} else {g.$g6[i] <- 0}}
print(g.$g6)
for(i in 1:nrow(g.)){
  if(g.$g70[i] == 1) {g.$g7[i] <- 1} else {g.$g7[i] <- 0}}
print(g.$g7)
for(i in 1:nrow(g.)){
  if(g.$g80[i] == 1) {g.$g8[i] <- 1} else {g.$g8[i] <- 0}}
print(g.$g8)

#calculate proportion
g11 <- nrow(g.[g.$g1 == 1,])/nrow(g.); g11
g22 <- nrow(g.[g.$g2 == 1,])/nrow(g.); g22
g33 <- nrow(g.[g.$g3 == 1,])/nrow(g.); g33
g44 <- nrow(g.[g.$g4 == 1,])/nrow(g.); g44
g55 <- nrow(g.[g.$g5 == 1,])/nrow(g.); g55
g66 <- nrow(g.[g.$g6 == 1,])/nrow(g.); g66
g77 <- nrow(g.[g.$g7 == 1,])/nrow(g.); g77
g88 <- nrow(g.[g.$g8 == 1,])/nrow(g.); g88

g.0 <- data.frame(Time = c("10","20","30","40","50","60","70","80"), Proportion = c(g11,g22,g33,g44,g55,g66,g77,g88)) 
ggplot(g.0, aes(Time, Proportion, group = 1)) + 
  geom_point() + 
  scale_x_discrete(limits=c("10","20","30","40","50","60","70","80")) +
  geom_line() +
  ggtitle("Proportions People That Chose Certain Options for Games") +
  theme(plot.title = element_text(hjust=0.5))

#s.
s.$s1 <- NA
s.$s2 <- NA
s.$s3 <- NA
s.$s4 <- NA
s.$s5 <- NA
s.$s6 <- NA
s.$s7 <- NA
s.$s8 <- NA

for(i in 1:nrow(s.)){
  if(s.$s10[i] == 1) {s.$s1[i] <- 1} else {s.$s1[i] <- 0}}
print(s.$s1)
for(i in 1:nrow(s.)){
  if(s.$s20[i] == 1) {s.$s2[i] <- 1} else {s.$s2[i] <- 0}}
print(s.$s2)
for(i in 1:nrow(s.)){
  if(s.$s30[i] == 1) {s.$s3[i] <- 1} else {s.$s3[i] <- 0}}
print(s.$s3)
for(i in 1:nrow(s.)){
  if(s.$s40[i] == 1) {s.$s4[i] <- 1} else {s.$s4[i] <- 0}}
print(s.$s4)
for(i in 1:nrow(s.)){
  if(s.$s50[i] == 1) {s.$s5[i] <- 1} else {s.$s5[i] <- 0}}
print(s.$s5)
for(i in 1:nrow(s.)){
  if(s.$s60[i] == 1) {s.$s6[i] <- 1} else {s.$s6[i] <- 0}}
print(s.$s6)
for(i in 1:nrow(s.)){
  if(s.$s70[i] == 1) {s.$s7[i] <- 1} else {s.$s7[i] <- 0}}
print(s.$s7)
for(i in 1:nrow(s.)){
  if(s.$s80[i] == 1) {s.$s8[i] <- 1} else {s.$s8[i] <- 0}}
print(s.$s8)


#calculate proportion
s11 <- nrow(s.[s.$s1 == 1,])/nrow(s.); s11
s22 <- nrow(s.[s.$s2 == 1,])/nrow(s.); s22
s33 <- nrow(s.[s.$s3 == 1,])/nrow(s.); s33
s44 <- nrow(s.[s.$s4 == 1,])/nrow(s.); s44
s55 <- nrow(s.[s.$s5 == 1,])/nrow(s.); s55
s66 <- nrow(s.[s.$s6 == 1,])/nrow(s.); s66
s77 <- nrow(s.[s.$s7 == 1,])/nrow(s.); s77
s88 <- nrow(s.[s.$s8 == 1,])/nrow(s.); s88



s.0 <- data.frame(Time = c("10","20","30","40","50","60","70","80"), Proportion = c(s11,s22,s33,s44,s55,s66,s77,s88)) 
ggplot(s.0, aes(Time, Proportion, group = 1)) + 
  geom_point() + 
  scale_x_discrete(limits=c("10","20","30","40","50","60","70","80")) +
  geom_line() +
  ggtitle("Proportions People That Chose Certain Options for Sports") +
  theme(plot.title = element_text(hjust=0.5))

#m.
m.$m1 <- NA
m.$m2 <- NA
m.$m3 <- NA
m.$m4 <- NA
m.$m5 <- NA
m.$m6 <- NA
m.$m7 <- NA
m.$m8 <- NA

for(i in 1:nrow(m.)){
  if(m.$m10[i] == 1) {m.$m1[i] <- 1} else {m.$m1[i] <- 0}}
print(m.$m1)
for(i in 1:nrow(m.)){
  if(m.$m20[i] == 1) {m.$m2[i] <- 1} else {m.$m2[i] <- 0}}
print(m.$m2)
for(i in 1:nrow(m.)){
  if(m.$m30[i] == 1) {m.$m3[i] <- 1} else {m.$m3[i] <- 0}}
print(m.$m3)
for(i in 1:nrow(m.)){
  if(m.$m40[i] == 1) {m.$m4[i] <- 1} else {m.$m4[i] <- 0}}
print(m.$m4)
for(i in 1:nrow(m.)){
  if(m.$m50[i] == 1) {m.$m5[i] <- 1} else {m.$m5[i] <- 0}}
print(m.$m5)
for(i in 1:nrow(m.)){
  if(m.$m60[i] == 1) {m.$m6[i] <- 1} else {m.$m6[i] <- 0}}
print(m.$m6)
for(i in 1:nrow(m.)){
  if(m.$m70[i] == 1) {m.$m7[i] <- 1} else {m.$m7[i] <- 0}}
print(m.$m7)
for(i in 1:nrow(m.)){
  if(m.$m80[i] == 1) {m.$m8[i] <- 1} else {m.$m8[i] <- 0}}
print(m.$m8)

#calculate proportion
m11 <- nrow(m.[m.$m1 == 1,])/nrow(m.); m11
m22 <- nrow(m.[m.$m2 == 1,])/nrow(m.); m22
m33 <- nrow(m.[m.$m3 == 1,])/nrow(m.); m33
m44 <- nrow(m.[m.$m4 == 1,])/nrow(m.); m44
m55 <- nrow(m.[m.$m5 == 1,])/nrow(m.); m55
m66 <- nrow(m.[m.$m6 == 1,])/nrow(m.); m66
m77 <- nrow(m.[m.$m7 == 1,])/nrow(m.); m77
m88 <- nrow(m.[m.$m8 == 1,])/nrow(m.); m88

m.0 <- data.frame(Time = c("10","20","30","40","50","60","70","80"), Proportion = c(m11,m22,m33,m44,m55,m66,m77,m88)) 
ggplot(m.0, aes(Time, Proportion, group = 1)) + 
  geom_point() + 
  scale_x_discrete(limits=c("10","20","30","40","50","60","70","80")) +
  geom_line() +
  ggtitle("Proportions People That Chose Certain Options for Music") +
  theme(plot.title = element_text(hjust=0.5))

#####graph for positive activities combined#######################
positive <- rbind(g.0, m.0, s.0)
positive$type <- factor(c(rep(c("Games"), times = 8),rep(c("Music"), times = 8),rep(c("Sports"), times = 8))) 


oo1 <- ggplot(data = positive, aes(x = Time, y = Proportion, group = type)) + 
  geom_point(aes(col = type)) + 
  scale_x_discrete(limits=c("10","20","30","40","50","60","70","80")) +
  geom_line(aes(col = type)) +
  theme(plot.title = element_text(hjust=0.5)) + 
  theme_bw() +
  scale_y_continuous(c(0,1)) +
  labs(color = "Activity", title = "Proportions People Who Chose Certain Options for Liked (Positive) Activities")






#####Loss only
t. <- Time[Time$t < 0,]
v. <- Time[Time$v < 0,]
d. <- Time[Time$d < 0,]

#t.
t.$t1 <- NA
t.$t2 <- NA
t.$t3 <- NA
t.$t4 <- NA
t.$t5 <- NA
t.$t6 <- NA
t.$t7 <- NA
t.$t8 <- NA

for(i in 1:nrow(t.)){
  if(t.$t10[i] == 1) {t.$t1[i] <- 1} else {t.$t1[i] <- 0}}
print(t.$t1)
for(i in 1:nrow(t.)){
  if(t.$t20[i] == 1) {t.$t2[i] <- 1} else {t.$t2[i] <- 0}}
print(t.$t2)
for(i in 1:nrow(t.)){
  if(t.$t30[i] == 1) {t.$t3[i] <- 1} else {t.$t3[i] <- 0}}
print(t.$t3)
for(i in 1:nrow(t.)){
  if(t.$t40[i] == 1) {t.$t4[i] <- 1} else {t.$t4[i] <- 0}}
print(t.$t4)
for(i in 1:nrow(t.)){
  if(t.$t50[i] == 1) {t.$t5[i] <- 1} else {t.$t5[i] <- 0}}
print(t.$t5)
for(i in 1:nrow(t.)){
  if(t.$t60[i] == 1) {t.$t6[i] <- 1} else {t.$t6[i] <- 0}}
print(t.$t6)
for(i in 1:nrow(t.)){
  if(t.$t70[i] == 1) {t.$t7[i] <- 1} else {t.$t7[i] <- 0}}
print(t.$t7)
for(i in 1:nrow(t.)){
  if(t.$t80[i] == 1) {t.$t8[i] <- 1} else {t.$t8[i] <- 0}}
print(t.$t8)


#calculate proportion
t11 <- nrow(t.[t.$t1 == 1,])/nrow(t.); t11
t22 <- nrow(t.[t.$t2 == 1,])/nrow(t.); t22
t33 <- nrow(t.[t.$t3 == 1,])/nrow(t.); t33
t44 <- nrow(t.[t.$t4 == 1,])/nrow(t.); t44
t55 <- nrow(t.[t.$t5 == 1,])/nrow(t.); t55
t66 <- nrow(t.[t.$t6 == 1,])/nrow(t.); t66
t77 <- nrow(t.[t.$t7 == 1,])/nrow(t.); t77
t88 <- nrow(t.[t.$t8 == 1,])/nrow(t.); t88

t.0 <- data.frame(Time = c("10","20","30","40","50","60","70","80"), Proportion = c(t11,t22,t33,t44,t55,t66,t77,t88)) 
ggplot(t.0, aes(Time, Proportion, group = 1)) + 
  geom_point() + 
  scale_x_discrete(limits=c("10","20","30","40","50","60","70","80")) +
  geom_line() +
  ggtitle("Proportions People That Chose Certain Options for Traffic Jam") +
  theme(plot.title = element_text(hjust=0.5))


#v.
v.$v1 <- NA
v.$v2 <- NA
v.$v3 <- NA
v.$v4 <- NA
v.$v5 <- NA
v.$v6 <- NA
v.$v7 <- NA
v.$v8 <- NA

for(i in 1:nrow(v.)){
  if(v.$v10[i] == 1) {v.$v1[i] <- 1} else {v.$v1[i] <- 0}}
print(v.$v1)
for(i in 1:nrow(v.)){
  if(v.$v20[i] == 1) {v.$v2[i] <- 1} else {v.$v2[i] <- 0}}
print(v.$v2)
for(i in 1:nrow(v.)){
  if(v.$v30[i] == 1) {v.$v3[i] <- 1} else {v.$v3[i] <- 0}}
print(v.$v3)
for(i in 1:nrow(v.)){
  if(v.$v40[i] == 1) {v.$v4[i] <- 1} else {v.$v4[i] <- 0}}
print(v.$v4)
for(i in 1:nrow(v.)){
  if(v.$v50[i] == 1) {v.$v5[i] <- 1} else {v.$v5[i] <- 0}}
print(v.$v5)
for(i in 1:nrow(v.)){
  if(v.$v60[i] == 1) {v.$v6[i] <- 1} else {v.$v6[i] <- 0}}
print(v.$v6)
for(i in 1:nrow(v.)){
  if(v.$v70[i] == 1) {v.$v7[i] <- 1} else {v.$v7[i] <- 0}}
print(v.$v7)
for(i in 1:nrow(v.)){
  if(v.$v80[i] == 1) {v.$v8[i] <- 1} else {v.$v8[i] <- 0}}
print(v.$v8)

#calculate proportion
v11 <- nrow(v.[v.$v1 == 1,])/nrow(v.); v11
v22 <- nrow(v.[v.$v2 == 1,])/nrow(v.); v22
v33 <- nrow(v.[v.$v3 == 1,])/nrow(v.); v33
v44 <- nrow(v.[v.$v4 == 1,])/nrow(v.); v44
v55 <- nrow(v.[v.$v5 == 1,])/nrow(v.); v55
v66 <- nrow(v.[v.$v6 == 1,])/nrow(v.); v66
v77 <- nrow(v.[v.$v7 == 1,])/nrow(v.); v77
v88 <- nrow(v.[v.$v8 == 1,])/nrow(v.); v88

v.0 <- data.frame(Time = c("10","20","30","40","50","60","70","80"), Proportion = c(v11,v22,v33,v44,v55,v66,v77,v88)) 
ggplot(v.0, aes(Time, Proportion, group = 1)) + 
  geom_point() + 
  scale_x_discrete(limits=c("10","20","30","40","50","60","70","80")) +
  geom_line() +
  ggtitle("Proportions People That Chose Certain Options for Vacuum") +
  theme(plot.title = element_text(hjust=0.5))

#d.
d.$d1 <- NA
d.$d2 <- NA
d.$d3 <- NA
d.$d4 <- NA
d.$d5 <- NA
d.$d6 <- NA
d.$d7 <- NA
d.$d8 <- NA

for(i in 1:nrow(d.)){
  if(d.$d10[i] == 1) {d.$d1[i] <- 1} else {d.$d1[i] <- 0}}
print(d.$d1)
for(i in 1:nrow(d.)){
  if(d.$d20[i] == 1) {d.$d2[i] <- 1} else {d.$d2[i] <- 0}}
print(d.$d2)
for(i in 1:nrow(d.)){
  if(d.$d30[i] == 1) {d.$d3[i] <- 1} else {d.$d3[i] <- 0}}
print(d.$d3)
for(i in 1:nrow(d.)){
  if(d.$d40[i] == 1) {d.$d4[i] <- 1} else {d.$d4[i] <- 0}}
print(d.$d4)
for(i in 1:nrow(d.)){
  if(d.$d50[i] == 1) {d.$d5[i] <- 1} else {d.$d5[i] <- 0}}
print(d.$d5)
for(i in 1:nrow(d.)){
  if(d.$d60[i] == 1) {d.$d6[i] <- 1} else {d.$d6[i] <- 0}}
print(d.$d6)
for(i in 1:nrow(d.)){
  if(d.$d70[i] == 1) {d.$d7[i] <- 1} else {d.$d7[i] <- 0}}
print(d.$d7)
for(i in 1:nrow(d.)){
  if(d.$d80[i] == 1) {d.$d8[i] <- 1} else {d.$d8[i] <- 0}}
print(d.$d8)


#calculate proportion
d11 <- nrow(d.[d.$d1 == 1,])/nrow(d.); d11
d22 <- nrow(d.[d.$d2 == 1,])/nrow(d.); d22
d33 <- nrow(d.[d.$d3 == 1,])/nrow(d.); d33
d44 <- nrow(d.[d.$d4 == 1,])/nrow(d.); d44
d55 <- nrow(d.[d.$d5 == 1,])/nrow(d.); d55
d66 <- nrow(d.[d.$d6 == 1,])/nrow(d.); d66
d77 <- nrow(d.[d.$d7 == 1,])/nrow(d.); d77
d88 <- nrow(d.[d.$d8 == 1,])/nrow(d.); d88

d.0 <- data.frame(Time = c("10","20","30","40","50","60","70","80"), Proportion = c(d11,d22,d33,d44,d55,d66,d77,d88)) 
ggplot(d.0, aes(Time, Proportion, group = 1)) + 
  geom_point() + 
  scale_x_discrete(limits=c("10","20","30","40","50","60","70","80")) +
  geom_line() +
  ggtitle("Proportions People That Chose Certain Options for Dishes") +
  theme(plot.title = element_text(hjust=0.5))



#####graph for negative activity combined
negative <- rbind(t.0, v.0, d.0)
negative$type <- factor(c(rep(c("Traffic Jam"), times = 8),rep(c("Vacuum"), times = 8),rep(c("Dishes"), times = 8))) 

oo2 <- ggplot(data = negative, aes(x = Time, y = Proportion, group = type)) + 
  geom_point(aes(col = type)) + 
  scale_x_discrete(limits=c("10","20","30","40","50","60","70","80")) +
  geom_line(aes(col = type)) +
  theme(plot.title = element_text(hjust=0.5)) +
  scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9")) +
  scale_y_continuous(limits = c(0,1)) +
  theme_bw() +
  labs(color = "Activity", title = "Proportions People Who Chose Certain Options for Disiked (Negative) Activities")

library(patchwork)
(oo1/oo2)
