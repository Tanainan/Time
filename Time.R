setwd("~/Desktop/time 2")
library(readr)
Time <- read_csv("Time.csv", skip = 1)
Time <- Time[-c(1),]
Time <- data.frame(sapply(Time, function(x) as.numeric(as.character(x))))
library(memisc)
library(ggplot2)
summary(Time$age) # age
percent(Time$gender == 1) #male
percent(Time$gender == 2) #female

#Risk preferences
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
hist(Time$pst, main = "Time Gain and Risk-Aversion", xlab = "Participants' Percentages of Choosing a Certain Option for Time Gain")
hist(Time$ngt, main = "Time Loss and Risk-Seeking", xlab = "Participants' Percentages of Choosing a Gamble Option for Time Loss")
hist(Time$total, main = "Time and Reflection Effect", xlab = "Percentages of Correct Predictions of the Reflection Effect")
mean(Time$pst)
mean(Time$ngt) # #48 has 0 for both numerator and denominator
which(Time$ngt == "NaN")
Time[48,c("neg", "negresponse")]
mean(Time$total)
t.test(Time$pst, y = NULL, mu = 50)
t.test(Time$ngt, y = NULL, mu = 50) # not significant
t.test(Time$total, y = NULL, mu = 50)


# calculate the average of answers for each activity
# possible values are 1, 1.125, 1.25, 1.375, 1.5, 1.625, 1.75, 1.875, 2 << 1 means people chose 1 for all questions, 1.125 means people chose 1 for 7 questions and 2 for 1 question

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
barplot(table(Time$ga), main = "Risk Behavior Patterns for Playing Games", ylab = "Counts", las = 2, 
        names = c("NA", "1111.1111", "1111.1112", "1111.1122", "1111.1221", "1111.1222", "1111.2222", "1112.2222", "1122.2222", "2111.1111", "2111.1122", "2111.1222", "2211.1111", "2211.1121", "2211.1122", "2221.2111", "2221.2222", "2221.1111", "2222.1111", "2222.2111", "2222.2211", "2222.2222"))
legend("topright", legend = c("NA = Rated This Activity Negatively", "1 = Certain Option", "2 = Gamble Option", "Digits from Left to Right Indicate Multiplies of 10 Minutes, Starting from 10 to 80 "))

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
barplot(table(Time$sa), main = "Risk Behavior Patterns for Playing Sports", ylab = "Counts", las = 2,
        names = c("NA", "1111.1111", "1111.1112", "1111.1122", "1111.1222", "1111.2222", "1112.2222", "1122.2222", "2111.1111", "2111.1112", "2111.1122", "2211.1111", "2211.1122", "2221.1111", "2221.1122", "2222.1111", "2222.2111", "2222.2211", "2222.2222"))
legend("topright", legend = c("NA = Rated This Activity Negatively", "1 = Certain Option", "2 = Gamble Option", "Digits from Left to Right Indicate Multiplies of 10 Minutes, Starting from 10 to 80 "))

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
barplot(table(Time$ma), main = "Risk Behavior Patterns for Listening to Music", ylab = "Counts", las = 2, 
        names = c("1111.1111", "1111.1112", "1111.1122", "1111.1222", "1111.2221", "1111.2222", "1112.2222", "1122.2111", "1211.1111", "1221.1111", "2111.1111", "2111.1122", "2211.1111", "2211.1222", "2211.2222", "2221.1111", "2221.1112", "2222.1111", "2222.2111", "2222.2211", "2222.2222"))
legend("topright", legend = c("1 = Certain Option", "2 = Gamble Option", "Digits from Left to Right Indicate Multiplies of 10 Minutes, Starting from 10 to 80 "))

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
barplot(table(Time$ta), main = "Risk Behavior Patterns for Getting Stuck in a Traffic Jam", ylab = "Counts", las = 2, 
        names = c("2222.2222", "2222.1111", "2111.2222", "1222.2222", "1221.1122", "1122.2222", "1112.2222", "1112.2221", "1111.2222", "1111.2122", "1111.1222", "1111.1122", "1111.1112", "1111.1111", "NA"))
legend("topright", legend = c("NA = Rated This Activity Positively", "1 = Certain Option", "2 = Gamble Option", "Digits from Left to Right Indicate Multiplies of 10 Minutes, Starting from 10 to 80 "))

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
barplot(table(Time$va), main = "Risk Behavior Patterns for Vacuuming the Theater", ylab = "Counts", las = 2, 
        names = c("2222.2222", "2221.1111", "2122.2222", "1222.2222", "1221.1222", "1122.2222", "1112.2222", "1112.2122", "1111.2222", "1111.1222", "1111.1122", "1111.1112", "1111.1111", "NA"))
legend("topright", legend = c("NA = Rated This Activity Positively", "1 = Certain Option", "2 = Gamble Option", "Digits from Left to Right Indicate Multiplies of 10 Minutes, Starting from 10 to 80 "))

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
barplot(table(Time$da), main = "Risk Behavior Patterns for Washing Dishes", col = "brown", las = 2, 
        names = c("2222.2222", "2221.1122", "2122.2222", "2111.2222", "1222.2222", "1122.2222", "1112.2222", "1112.1222", "1111.2222", "1111.1222", "1111.1122", "1111.1112", "1111.1111", "NA"), ylab = "Counts")
legend("topright", legend = c("NA = Rated This Activity Positively", "1 = Certain Option", "2 = Gamble Option", "Digits from Left to Right Indicate Multiplies of 10 Minutes, Starting from 10 to 80 "))


#Loss aversion
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
barplot(table(Time$gda), main = "Risk Behavior Patterns for Playing Games and Washing Dishes", col = "brown", las = 2, 
        names = c("2222.2222", "2222.2221", "2222.2211", "2222.2111", "2222.1111", "2221.2211", "2221.2111", "2221.1122", "2221.1112", "2221.1111", "2211.2221", "2211.2211", "2211.2111", "2211.1112", "2211.1111", "2111.2111", "2111.1111", "1111.1111", "NA"), ylab = "Counts")
legend("topright", legend = c("NA = Rated Playing Games as Negative or Washing Dishes as Positive or Both", "1 = Certain Option", "2 = Gamble Option", "Digits from Left to Right Indicate Multiplies of 10 Minutes, Starting from 10 to 80 "))

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
barplot(table(Time$mva), main = "Risk Behavior Patterns for Listening to Music and Vacuuming the Theater", col = "brown", las = 2, 
        names = c("2222.2222", "2222.2111", "2222.1111", "2221.2211", "2221.2111", "2221.1111", "2211.2211", "2211.2111", "2211.1112", "2211.1111", "2111.2111", "2111.1111", "1111.1111", "NA"), ylab = "Counts")
legend("topright", legend = c("NA = Rated Listening to Music as Negative or Vacuuming a Theater as Positive or Both", "1 = Certain Option", "2 = Gamble Option", "Digits from Left to Right Indicate Multiplies of 10 Minutes, Starting from 10 to 80 "))

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
barplot(table(Time$sta), main = "Risk Behavior Patterns for Playing Sports and Getting Stuck in a Traffic", col = "brown", las = 2, 
        names = c("2222.2211", "2222.2111", "2222.1111", "2221.2221", "2221.2211", "2221.2111", "2221.1112", "2221.1111", "2211.2222", "2211.2221", "2211.2211", "2211.2111", "2211.1122", "2211.1111", "2111.2111", "2111.1112", "2111.1111", "1111.2222", "1111.1222", "1111.1112", "1111.1111", "NA"), ylab = "Counts")
legend("topright", legend = c("NA = Rated Playing Sports as Negative or Getting Stuck in a Traffic as Positive or Both", "1 = Certain Option", "2 = Gamble Option", "Digits from Left to Right Indicate Multiplies of 10 Minutes, Starting from 10 to 80 "))

# 0 vs 10
percent(Time$g01)
percent(Time$m01)
percent(Time$s01)
percent(Time$t01)
percent(Time$v01)
percent(Time$d01)

# 80 vs 90
percent(Time$g89)
percent(Time$s89)
percent(Time$m89)
percent(Time$t89)
percent(Time$v89)
percent(Time$d89)

#monotonicity*************************************************************************************
Time$gg0 <- NA
Time$ss0 <- NA
Time$mm0 <- NA
Time$tt0 <- NA
Time$dd0 <- NA
Time$vv0 <- NA
for(i in 1:nrow(Time)){
  if(Time$g[i] > 0 & Time$g01[i] == 2 & Time$g89[i] == 2){Time$gg0[i] <- 1}
  else {Time$gg0[i] <- 0}
}
print(Time$gg0)

for(i in 1:nrow(Time)){
  if(Time$s[i] > 0 & Time$s01[i] == 2 & Time$s89[i] == 2){Time$ss0[i] <- 1}
  else {Time$ss0[i] <- 0}
}
print(Time$ss0)

for(i in 1:nrow(Time)){
  if(Time$m[i] > 0 & Time$m01[i] == 2 & Time$m89[i] == 2){Time$mm0[i] <- 1}
  else {Time$mm0[i] <- 0}
}
print(Time$mm0)

for(i in 1:nrow(Time)){
  if(Time$t[i] < 0 & Time$t01[i] == 1 & Time$t89[i] == 1){Time$tt0[i] <- 1}
  else {Time$tt0[i] <- 0}
}
print(Time$tt0)

for(i in 1:nrow(Time)){
  if(Time$v[i] < 0 & Time$v01[i] == 1 & Time$v89[i] == 1){Time$vv0[i] <- 1}
  else {Time$vv0[i] <- 0}
}
print(Time$vv0)

for(i in 1:nrow(Time)){
  if(Time$d[i] < 0 & Time$d01[i] == 1 & Time$d89[i] == 1){Time$dd0[i] <- 1}
  else {Time$dd0[i] <- 0}
}
print(Time$dd0)

Time$posi0 <- rowSums(Time[,c("gg0","mm0","ss0")])
print(Time$posi0)
Time$neg0 <- rowSums(Time[,c("vv0","tt0","dd0")])
print(Time$neg0)

#Look at responses (1 = sure and 2 = gamble) change to (1 = sure and 0 = gamble) -- including negative activities because some people might rate positive activity as negative
Time$g45[Time$g45 == 2] <- "0"
Time$m45[Time$m45 == 2] <- "0"
Time$s45[Time$s45 == 2] <- "0"
Time$v45[Time$v45 == 2] <- "0"
Time$t45[Time$t45 == 2] <- "0"
Time$d45[Time$d45 == 2] <- "0"
print(Time$g45)

#create result of countings
Time$gr0 <- NA
Time$sr0 <- NA
Time$mr0 <- NA
Time$tr0 <- NA
Time$dr0 <- NA
Time$vr0 <- NA

#if evaluate positive activity as positive and answer sure option == 1
for(i in 1:nrow(Time)){
  if(Time$gg0[i] == 1 & Time$g45[i] == 1){Time$gr0[i] <- 1}
  else {Time$gr0[i] <- 0}
}

for(i in 1:nrow(Time)){
  if(Time$mm0[i] == 1 & Time$m45[i] == 1){Time$mr0[i] <- 1}
  else {Time$mr0[i] <- 0}
}
print(Time$mr0)

for(i in 1:nrow(Time)){
  if(Time$ss0[i] == 1 & Time$s45[i] == 1){Time$sr0[i] <- 1}
  else {Time$sr0[i] <- 0}
}
print(Time$sr0)

#if evaluate negative activities as negative and answer gamble option == 1
for(i in 1:nrow(Time)){
  if(Time$vv0[i] == 1 & Time$v45[i] == 0){Time$vr0[i] <- 1}
  else {Time$vr0[i] <- 0}
}
print(Time$vr0)

for(i in 1:nrow(Time)){
  if(Time$dd0[i] == 1 & Time$d45[i] == 0){Time$dr0[i] <- 1}
  else {Time$dr0[i] <- 0}
}
print(Time$dr0)

for(i in 1:nrow(Time)){
  if(Time$tt0[i] == 1 & Time$t45[i] == 0){Time$tr0[i] <- 1}
  else {Time$tr0[i] <- 0}
}
print(Time$tr0)

#sum up the # of responses for each individual. If chose gamble for all 3 rated negative activities == 3. If only rated 2 negative activities as negative, and chose gamble only one of the two == 1
Time$posiresponse0 <- rowSums(Time[,c("gr0","mr0","sr0")])
print(Time$posiresponse0)
Time$negresponse0 <- rowSums(Time[,c("vr0","tr0","dr0")])
print(Time$negresponse0)


#calculate percentages of gain, loss, total
#positive -- predicting risk-aversion -- choosing sure option
Time$pst0 <- 100*Time$posiresponse0/Time$posi0; Time$pst0
Time$ngt0 <- 100*Time$negresponse0/Time$neg0; Time$ngt0
Time$total0 <- 100*(Time$posiresponse0 + Time$negresponse0)/(Time$posi0 + Time$neg0); Time$total0     

hist(Time$pst0, ylim = c(0,200), labels = TRUE, main = "Time Gain and Risk-Aversion", xlab = "Participants' Percentages of Choosing a Certain Option for Time Gain")
hist(Time$ngt0, ylim = c(0,80), labels = TRUE, main = "Time Loss and Risk-Seeking", xlab = "Participants' Percentages of Choosing a Gamble Option for Time Loss")
hist(Time$total0, ylim = c(0,70), labels = TRUE, main = "Time and Reflection Effect", xlab = "Percentages of Correct Predictions of the Reflection Effect")

t.test(Time$pst0, y = NULL, mu = 50)
t.test(Time$ngt0, y = NULL, mu = 50) # not significant
t.test(Time$total0, y = NULL, mu = 50)


# calculate the average of answers for each activity
# possible values are 1, 1.125, 1.25, 1.375, 1.5, 1.625, 1.75, 1.875, 2 << 1 means people chose 1 for all questions, 1.125 means people chose 1 for 7 questions and 2 for 1 question

#consistency for g
Time$gg01 <- NA
Time$gg02 <- NA
Time$gg03 <- NA
Time$gg04 <- NA
Time$gg05 <- NA
Time$gg06 <- NA
Time$gg07 <- NA
Time$gg08 <- NA
for(i in 1:nrow(Time)){
  if(Time$g[i] > 0 & Time$g01[i] == 2 & Time$g89[i] == 2) {if(Time$g10[i] == 1){Time$gg01[i] <- 1} else {Time$gg01[i] <- 2}}
  else{Time$gg01[i] <- 0}}
for(i in 1:nrow(Time)){
  if(Time$g[i] > 0 & Time$g01[i] == 2 & Time$g89[i] == 2) {if(Time$g20[i] == 1){Time$gg02[i] <- 1} else {Time$gg02[i] <- 2}}
  else{Time$gg02[i] <- 0}}
for(i in 1:nrow(Time)){
  if(Time$g[i] > 0 & Time$g01[i] == 2 & Time$g89[i] == 2) {if(Time$g30[i] == 1){Time$gg03[i] <- 1} else {Time$gg03[i] <- 2}}
  else{Time$gg03[i] <- 0}}
for(i in 1:nrow(Time)){
  if(Time$g[i] > 0 & Time$g01[i] == 2 & Time$g89[i] == 2) {if(Time$g40[i] == 1){Time$gg04[i] <- 1} else {Time$gg04[i] <- 2}}
  else{Time$gg04[i] <- 0}}
for(i in 1:nrow(Time)){
  if(Time$g[i] > 0 & Time$g01[i] == 2 & Time$g89[i] == 2) {if(Time$g50[i] == 1){Time$gg05[i] <- 1} else {Time$gg05[i] <- 2}}
  else{Time$gg05[i] <- 0}}
for(i in 1:nrow(Time)){
  if(Time$g[i] > 0 & Time$g01[i] == 2 & Time$g89[i] == 2) {if(Time$g60[i] == 1){Time$gg06[i] <- 1} else {Time$gg06[i] <- 2}}
  else{Time$gg06[i] <- 0}}
for(i in 1:nrow(Time)){
  if(Time$g[i] > 0 & Time$g01[i] == 2 & Time$g89[i] == 2) {if(Time$g70[i] == 1){Time$gg07[i] <- 1} else {Time$gg07[i] <- 2}}
  else{Time$gg07[i] <- 0}}
for(i in 1:nrow(Time)){
  if(Time$g[i] > 0 & Time$g01[i] == 2 & Time$g89[i] == 2) {if(Time$g80[i] == 1){Time$gg08[i] <- 1} else {Time$gg08[i] <- 2}}
  else{Time$gg08[i] <- 0}}
Time$ga00 <- (Time$gg08*.0001 + Time$gg07*.001 + Time$gg06*.01 + Time$gg05*.1 + Time$gg04*1 + Time$gg03*10 + Time$gg02*100 + Time$gg01*1000)
options(digits=8)
i <- barplot(table(Time$ga00), ylim = c(0,120), main = "Risk Behavior Patterns for Playing Games", ylab = "Counts", las = 2, 
        names = c("NA", "1111.1111", "1111.1221", "1111.1222", "2111.1111", "2111.1122", "2211.1111", "2211.2111", "2221.1111", "2222.1111", "2222.2111", "2222.2222"))
legend("topright", legend = c("NA = Rated This Activity Negatively", "1 = Certain Option", "2 = Gamble Option", "Digits from Left to Right Indicate Multiplies of 10 Minutes, Starting from 10 to 80 "))
text(i, table(Time$ga00), pos = 3, cex = 1, labels=as.character(table(Time$ga00)))


# consistency for s
Time$ss01 <- NA
Time$ss02 <- NA
Time$ss03 <- NA
Time$ss04 <- NA
Time$ss05 <- NA
Time$ss06 <- NA
Time$ss07 <- NA
Time$ss08 <- NA
for(i in 1:nrow(Time)){
  if(Time$s[i] > 0 & Time$s01[i] == 2 & Time$s89[i] == 2) {if(Time$s10[i] == 1){Time$ss01[i] <- 1} else {Time$ss01[i] <- 2}}
  else{Time$ss01[i] <- 0}}
for(i in 1:nrow(Time)){
  if(Time$s[i] > 0 & Time$s01[i] == 2 & Time$s89[i] == 2) {if(Time$s20[i] == 1){Time$ss02[i] <- 1} else {Time$ss02[i] <- 2}}
  else{Time$ss02[i] <- 0}}
for(i in 1:nrow(Time)){
  if(Time$s[i] > 0 & Time$s01[i] == 2 & Time$s89[i] == 2) {if(Time$s30[i] == 1){Time$ss03[i] <- 1} else {Time$ss03[i] <- 2}}
  else{Time$ss03[i] <- 0}}
for(i in 1:nrow(Time)){
  if(Time$s[i] > 0 & Time$s01[i] == 2 & Time$s89[i] == 2) {if(Time$s40[i] == 1){Time$ss04[i] <- 1} else {Time$ss04[i] <- 2}}
  else{Time$ss04[i] <- 0}}
for(i in 1:nrow(Time)){
  if(Time$s[i] > 0 & Time$s01[i] == 2 & Time$s89[i] == 2) {if(Time$s50[i] == 1){Time$ss05[i] <- 1} else {Time$ss05[i] <- 2}}
  else{Time$ss05[i] <- 0}}
for(i in 1:nrow(Time)){
  if(Time$s[i] > 0 & Time$s01[i] == 2 & Time$s89[i] == 2) {if(Time$s60[i] == 1){Time$ss06[i] <- 1} else {Time$ss06[i] <- 2}}
  else{Time$ss06[i] <- 0}}
for(i in 1:nrow(Time)){
  if(Time$s[i] > 0 & Time$s01[i] == 2 & Time$s89[i] == 2) {if(Time$s70[i] == 1){Time$ss07[i] <- 1} else {Time$ss07[i] <- 2}}
  else{Time$ss07[i] <- 0}}
for(i in 1:nrow(Time)){
  if(Time$s[i] > 0 & Time$s01[i] == 2 & Time$s89[i] == 2) {if(Time$s80[i] == 1){Time$ss08[i] <- 1} else {Time$ss08[i] <- 2}}
  else{Time$ss08[i] <- 0}}
Time$sa00 <- (Time$ss08*.0001 + Time$ss07*.001 + Time$ss06*.01 + Time$ss05*.1 + Time$ss04*1 + Time$ss03*10 + Time$ss02*100 + Time$ss01*1000)
options(digits=8)
h <- barplot(table(Time$sa00), ylim = c(0,120), main = "Risk Behavior Patterns for Playing Sports", ylab = "Counts", las = 2,
        names = c("NA", "1111.1111", "1111.1112", "1111.1122", "1111.2222", "2111.1111", "2111.1122", "2211.1111", "2211.1122", "2221.1111", "2222.1111", "2222.2111", "2222.2211", "2222.2222"))
legend("topright", legend = c("NA = Rated This Activity Negatively", "1 = Certain Option", "2 = Gamble Option", "Digits from Left to Right Indicate Multiplies of 10 Minutes, Starting from 10 to 80 "))
text(h, table(Time$sa00), pos = 3, cex = 1, labels=as.character(table(Time$sa00)))



# consistency for m
Time$mm01 <- NA
Time$mm02 <- NA
Time$mm03 <- NA
Time$mm04 <- NA
Time$mm05 <- NA
Time$mm06 <- NA
Time$mm07 <- NA
Time$mm08 <- NA
for(i in 1:nrow(Time)){
  if(Time$m[i] > 0 & Time$m01[i] == 2 & Time$m89[i] == 2) {if(Time$m10[i] == 1){Time$mm01[i] <- 1} else {Time$mm01[i] <- 2}}
  else{Time$mm01[i] <- 0}}
for(i in 1:nrow(Time)){
  if(Time$m[i] > 0 & Time$m01[i] == 2 & Time$m89[i] == 2) {if(Time$m20[i] == 1){Time$mm02[i] <- 1} else {Time$mm02[i] <- 2}}
  else{Time$mm02[i] <- 0}}
for(i in 1:nrow(Time)){
  if(Time$m[i] > 0 & Time$m01[i] == 2 & Time$m89[i] == 2) {if(Time$m30[i] == 1){Time$mm03[i] <- 1} else {Time$mm03[i] <- 2}}
  else{Time$mm03[i] <- 0}}
for(i in 1:nrow(Time)){
  if(Time$m[i] > 0 & Time$m01[i] == 2 & Time$m89[i] == 2) {if(Time$m40[i] == 1){Time$mm04[i] <- 1} else {Time$mm04[i] <- 2}}
  else{Time$mm04[i] <- 0}}
for(i in 1:nrow(Time)){
  if(Time$m[i] > 0 & Time$m01[i] == 2 & Time$m89[i] == 2) {if(Time$m50[i] == 1){Time$mm05[i] <- 1} else {Time$mm05[i] <- 2}}
  else{Time$mm05[i] <- 0}}
for(i in 1:nrow(Time)){
  if(Time$m[i] > 0 & Time$m01[i] == 2 & Time$m89[i] == 2) {if(Time$m60[i] == 1){Time$mm06[i] <- 1} else {Time$mm06[i] <- 2}}
  else{Time$mm06[i] <- 0}}
for(i in 1:nrow(Time)){
  if(Time$m[i] > 0 & Time$m01[i] == 2 & Time$m89[i] == 2) {if(Time$m70[i] == 1){Time$mm07[i] <- 1} else {Time$mm07[i] <- 2}}
  else{Time$mm07[i] <- 0}}
for(i in 1:nrow(Time)){
  if(Time$m[i] > 0 & Time$m01[i] == 2 & Time$m89[i] == 2) {if(Time$m80[i] == 1){Time$mm08[i] <- 1} else {Time$mm08[i] <- 2}}
  else{Time$mm08[i] <- 0}}
Time$ma00 <- (Time$mm08*.0001 + Time$mm07*.001 + Time$mm06*.01 + Time$mm05*.1 + Time$mm04*1 + Time$mm03*10 + Time$mm02*100 + Time$mm01*1000)
options(digits=8)
g <- barplot(table(Time$ma00), ylim = c(0,70), main = "Risk Behavior Patterns for Listening to Music", ylab = "Counts", las = 2, 
        names = c("NA", "1111.1111", "1111.1112", "1111.1122", "1111.1222", "1111.2221", "1122.2111", "1211.1111", "1221.1111", "2111.1111", "2211.1111", "2221.1111", "2222.1111", "2222.2111", "2222.2211", "2222.2222"))
legend("topright", legend = c("1 = Certain Option", "2 = Gamble Option", "Digits from Left to Right Indicate Multiplies of 10 Minutes, Starting from 10 to 80 "))
text(g, table(Time$ma00), pos = 3, cex = 1, labels=as.character(table(Time$ma00)))



# consistency for t
Time$tt01 <- NA
Time$tt02 <- NA
Time$tt03 <- NA
Time$tt04 <- NA
Time$tt05 <- NA
Time$tt06 <- NA
Time$tt07 <- NA
Time$tt08 <- NA

for(i in 1:nrow(Time)){
  if(Time$t[i] < 0 & Time$t01[i] == 1 & Time$t89[i] == 1) {if(Time$t10[i] == 1){Time$tt01[i] <- -1} else {Time$tt01[i] <- -2}}
  else{Time$tt01[i] <- 0}}
for(i in 1:nrow(Time)){
  if(Time$t[i] < 0 & Time$t01[i] == 1 & Time$t89[i] == 1) {if(Time$t20[i] == 1){Time$tt02[i] <- -1} else {Time$tt02[i] <- -2}}
  else{Time$tt02[i] <- 0}}
for(i in 1:nrow(Time)){
  if(Time$t[i] < 0 & Time$t01[i] == 1 & Time$t89[i] == 1) {if(Time$t30[i] == 1){Time$tt03[i] <- -1} else {Time$tt03[i] <- -2}}
  else{Time$tt03[i] <- 0}}
for(i in 1:nrow(Time)){
  if(Time$t[i] < 0 & Time$t01[i] == 1 & Time$t89[i] == 1) {if(Time$t40[i] == 1){Time$tt04[i] <- -1} else {Time$tt04[i] <- -2}}
  else{Time$tt04[i] <- 0}}
for(i in 1:nrow(Time)){
  if(Time$t[i] < 0 & Time$t01[i] == 1 & Time$t89[i] == 1) {if(Time$t50[i] == 1){Time$tt05[i] <- -1} else {Time$tt05[i] <- -2}}
  else{Time$tt05[i] <- 0}}
for(i in 1:nrow(Time)){
  if(Time$t[i] < 0 & Time$t01[i] == 1 & Time$t89[i] == 1) {if(Time$t60[i] == 1){Time$tt06[i] <- -1} else {Time$tt06[i] <- -2}}
  else{Time$tt06[i] <- 0}}
for(i in 1:nrow(Time)){
  if(Time$t[i] < 0 & Time$t01[i] == 1 & Time$t89[i] == 1) {if(Time$t70[i] == 1){Time$tt07[i] <- -1} else {Time$tt07[i] <- -2}}
  else{Time$tt07[i] <- 0}}
for(i in 1:nrow(Time)){
  if(Time$t[i] < 0 & Time$t01[i] == 1 & Time$t89[i] == 1) {if(Time$t80[i] == 1){Time$tt08[i] <- -1} else {Time$tt08[i] <- -2}}
  else{Time$tt08[i] <- 0}}
print(Time$tt05)
options(digits=8)
Time$ta00 <- (Time$tt08*.0001 + Time$tt07*.001 + Time$tt06*.01 + Time$tt05*.1 + Time$tt04*1 + Time$tt03*10 + Time$tt02*100 + Time$tt01*1000); Time$ta00
f <- barplot(table(Time$ta00), ylim = c(0,80), col = "light blue", main = "Risk Behavior Patterns for Getting Stuck in a Traffic Jam", ylab = "Counts", las = 2, 
        names = c("2222.2222", "2222.1111", "2111.2222", "1222.2222", "1221.1122", "1122.2222", "1112.2222", "1112.2221", "1111.2222", "1111.2122", "1111.1222", "1111.1122", "1111.1112", "1111.1111", "NA"))
legend("topright", legend = c("NA = Rated This Activity Positively", "1 = Certain Option", "2 = Gamble Option", "Digits from Left to Right Indicate Multiplies of 10 Minutes, Starting from 10 to 80 "))
text(f, table(Time$ta00), pos = 3, cex = 1, labels=as.character(table(Time$ta00)))


# consistency for v
Time$vv01 <- NA
Time$vv02 <- NA
Time$vv03 <- NA
Time$vv04 <- NA
Time$vv05 <- NA
Time$vv06 <- NA
Time$vv07 <- NA
Time$vv08 <- NA

for(i in 1:nrow(Time)){
  if(Time$v[i] < 0 & Time$v01[i] == 1 & Time$v89[i] == 1) {if(Time$v10[i] == 1){Time$vv01[i] <- -1} else {Time$vv01[i] <- -2}}
  else{Time$vv01[i] <- 0}}
for(i in 1:nrow(Time)){
  if(Time$v[i] < 0 & Time$v01[i] == 1 & Time$v89[i] == 1) {if(Time$v20[i] == 1){Time$vv02[i] <- -1} else {Time$vv02[i] <- -2}}
  else{Time$vv02[i] <- 0}}
for(i in 1:nrow(Time)){
  if(Time$v[i] < 0 & Time$v01[i] == 1 & Time$v89[i] == 1) {if(Time$v30[i] == 1){Time$vv03[i] <- -1} else {Time$vv03[i] <- -2}}
  else{Time$vv03[i] <- 0}}
for(i in 1:nrow(Time)){
  if(Time$v[i] < 0 & Time$v01[i] == 1 & Time$v89[i] == 1) {if(Time$v40[i] == 1){Time$vv04[i] <- -1} else {Time$vv04[i] <- -2}}
  else{Time$vv04[i] <- 0}}
for(i in 1:nrow(Time)){
  if(Time$v[i] < 0 & Time$v01[i] == 1 & Time$v89[i] == 1) {if(Time$v50[i] == 1){Time$vv05[i] <- -1} else {Time$vv05[i] <- -2}}
  else{Time$vv05[i] <- 0}}
for(i in 1:nrow(Time)){
  if(Time$v[i] < 0 & Time$v01[i] == 1 & Time$v89[i] == 1) {if(Time$v60[i] == 1){Time$vv06[i] <- -1} else {Time$vv06[i] <- -2}}
  else{Time$vv06[i] <- 0}}
for(i in 1:nrow(Time)){
  if(Time$v[i] < 0 & Time$v01[i] == 1 & Time$v89[i] == 1) {if(Time$v70[i] == 1){Time$vv07[i] <- -1} else {Time$vv07[i] <- -2}}
  else{Time$vv07[i] <- 0}}
for(i in 1:nrow(Time)){
  if(Time$v[i] < 0 & Time$v01[i] == 1 & Time$v89[i] == 1) {if(Time$v80[i] == 1){Time$vv08[i] <- -1} else {Time$vv08[i] <- -2}}
  else{Time$vv08[i] <- 0}}
options(digits=8)
Time$va00 <- (Time$vv08*.0001 + Time$vv07*.001 + Time$vv06*.01 + Time$vv05*.1 + Time$vv04*1 + Time$vv03*10 + Time$vv02*100 + Time$vv01*1000); Time$va00
e <- barplot(table(Time$va00), ylim = c(0, 70), col = "light blue", main = "Risk Behavior Patterns for Vacuuming the Theater", ylab = "Counts", las = 2, 
        names = c("2221.1111", "2122.2222", "1222.2222", "1221.1222", "1122.2222", "1112.2222", "1112.2122", "1111.2222", "1111.1222", "1111.1122", "1111.1112", "1111.1111", "NA"))
legend("topright", legend = c("NA = Rated This Activity Positively", "1 = Certain Option", "2 = Gamble Option", "Digits from Left to Right Indicate Multiplies of 10 Minutes, Starting from 10 to 80 "))
text(e, table(Time$va00), pos = 3, cex = 1, labels=as.character(table(Time$va00)))


# consistency for d
Time$dd01 <- NA
Time$dd02 <- NA
Time$dd03 <- NA
Time$dd04 <- NA
Time$dd05 <- NA
Time$dd06 <- NA
Time$dd07 <- NA
Time$dd08 <- NA

for(i in 1:nrow(Time)){
  if(Time$d[i] < 0 & Time$d01[i] == 1 & Time$d89[i] == 1) {if(Time$d10[i] == 1){Time$dd01[i] <- -1} else {Time$dd01[i] <- -2}}
  else{Time$dd01[i] <- 0}}
for(i in 1:nrow(Time)){
  if(Time$d[i] < 0 & Time$d01[i] == 1 & Time$d89[i] == 1) {if(Time$d20[i] == 1){Time$dd02[i] <- -1} else {Time$dd02[i] <- -2}}
  else{Time$dd02[i] <- 0}}
for(i in 1:nrow(Time)){
  if(Time$d[i] < 0 & Time$d01[i] == 1 & Time$d89[i] == 1) {if(Time$d30[i] == 1){Time$dd03[i] <- -1} else {Time$dd03[i] <- -2}}
  else{Time$dd03[i] <- 0}}
for(i in 1:nrow(Time)){
  if(Time$d[i] < 0 & Time$d01[i] == 1 & Time$d89[i] == 1) {if(Time$d40[i] == 1){Time$dd04[i] <- -1} else {Time$dd04[i] <- -2}}
  else{Time$dd04[i] <- 0}}
for(i in 1:nrow(Time)){
  if(Time$d[i] < 0 & Time$d01[i] == 1 & Time$d89[i] == 1) {if(Time$d50[i] == 1){Time$dd05[i] <- -1} else {Time$dd05[i] <- -2}}
  else{Time$dd05[i] <- 0}}
for(i in 1:nrow(Time)){
  if(Time$d[i] < 0 & Time$d01[i] == 1 & Time$d89[i] == 1) {if(Time$d60[i] == 1){Time$dd06[i] <- -1} else {Time$dd06[i] <- -2}}
  else{Time$dd06[i] <- 0}}
for(i in 1:nrow(Time)){
  if(Time$d[i] < 0 & Time$d01[i] == 1 & Time$d89[i] == 1) {if(Time$d70[i] == 1){Time$dd07[i] <- -1} else {Time$dd07[i] <- -2}}
  else{Time$dd07[i] <- 0}}
for(i in 1:nrow(Time)){
  if(Time$d[i] < 0 & Time$d01[i] == 1 & Time$d89[i] == 1) {if(Time$d80[i] == 1){Time$dd08[i] <- -1} else {Time$dd08[i] <- -2}}
  else{Time$dd08[i] <- 0}}
options(digits=8)
Time$da00 <- (Time$dd08*.0001 + Time$dd07*.001 + Time$dd06*.01 + Time$dd05*.1 + Time$dd04*1 + Time$dd03*10 + Time$dd02*100 + Time$dd01*1000); Time$da00
d <- barplot(table(Time$da00), ylim = c(0,80), col = "light blue", main = "Risk Behavior Patterns for Washing Dishes", las = 2, 
        names = c("2222.2222", "2221.1122", "2122.2222", "2111.2222", "1222.2222", "1122.2222", "1112.2222", "1112.1222", "1111.2222", "1111.1222", "1111.1122", "1111.1112", "1111.1111", "NA"), ylab = "Counts")
legend("topright", legend = c("NA = Rated This Activity Positively", "1 = Certain Option", "2 = Gamble Option", "Digits from Left to Right Indicate Multiplies of 10 Minutes, Starting from 10 to 80 "))
text(d, table(Time$da00), pos = 3, cex = 1, labels=as.character(table(Time$da00)))


#Loss aversion
# 1 = neither actitiyies and 2 = gamble
Time$l10 <- NA
Time$l20 <- NA
Time$l30 <- NA
#choosing a sure option = loss averse
for(i in 1:nrow(Time)){
  if(Time$gd0[i] == 1 & Time$g[i] > 0 & Time$d[i] < 0 & Time$g01[i] == 2 & Time$g89[i] == 2 & Time$d01[i] == 1 & Time$d89[i] == 1){Time$l10[i] <- 1} else
  {Time$l10[i] <- 0}
}
print(Time$l10)

for(i in 1:nrow(Time)){
  if(Time$st0[i] == 1 & Time$s[i] > 0 & Time$t[i] < 0 & Time$s01[i] == 2 & Time$s89[i] == 2 & Time$t01[i] == 1 & Time$t89[i] == 1){Time$l20[i] <- 1} else
  {Time$l20[i] <- 0}
}
print(Time$l20)

for(i in 1:nrow(Time)){
  if(Time$mv0[i] == 1 & Time$m[i] > 0 & Time$v[i] < 0 & Time$m01[i] == 2 & Time$m89[i] == 2 & Time$v01[i] == 1 & Time$v89[i] == 1){Time$l30[i] <- 1} else
  {Time$l30[i] <- 0}
}
print(Time$l30)

Time$loss0 <- Time$l10 + Time$l20 + Time$l30; Time$loss0
Time$lp0 <- 100*Time$loss0/3; Time$lp0

hist(Time$lp0, main = "Loss Aversion & Time", xlab = "Participants' Percentages of Loss Aversion")
t.test(Time$lp0, y = NULL, mu = 50)

#Loss aversion Excluding 80 vs 90 mins
# 1 = neither actitiyies and 2 = gamble
Time$l100 <- NA
Time$l200 <- NA
Time$l300 <- NA
#choosing a sure option = loss averse
for(i in 1:nrow(Time)){
  if(Time$gd0[i] == 1 & Time$g[i] > 0 & Time$d[i] < 0 & Time$g01[i] == 2 & Time$d01[i] == 1){Time$l100[i] <- 1} else
  {Time$l100[i] <- 0}
}
print(Time$l100)

for(i in 1:nrow(Time)){
  if(Time$st0[i] == 1 & Time$s[i] > 0 & Time$t[i] < 0 & Time$s01[i] == 2 & Time$t01[i] == 1){Time$l200[i] <- 1} else
  {Time$l200[i] <- 0}
}
print(Time$l200)

for(i in 1:nrow(Time)){
  if(Time$mv0[i] == 1 & Time$m[i] > 0 & Time$v[i] < 0 & Time$m01[i] == 2 & Time$v01[i] == 1){Time$l300[i] <- 1} else
  {Time$l300[i] <- 0}
}
print(Time$l300)

Time$loss00 <- Time$l100 + Time$l200 + Time$l300; Time$loss00
Time$lp00 <- 100*Time$loss00/3; Time$lp00

hist(Time$lp00, main = "Loss Aversion & Time", xlab = "Participants' Percentages of Loss Aversion")
t.test(Time$lp00, y = NULL, mu = 50) # not significant!

#Consistency Mixed Gamble Excluding 80 vs 90 mins
#gd
Time$gdn4. <- NA
Time$gdn3. <- NA
Time$gdn2. <- NA
Time$gdn1. <- NA
Time$gdp1. <- NA
Time$gdp2. <- NA
Time$gdp3. <- NA
Time$gdp4. <- NA
for(i in 1:nrow(Time)){
  if(Time$g[i] > 0 & Time$d[i] < 0 & Time$g01[i] == 2 & Time$d01[i] == 1) {if(Time$gd.40[i] == 1){Time$gdn4.[i] <- -1} else {Time$gdn4.[i] <- -2}}
  else{Time$gdn4.[i] <- 0}}
for(i in 1:nrow(Time)){
  if(Time$g[i] > 0 & Time$d[i] < 0 & Time$g01[i] == 2 & Time$d01[i] == 1) {if(Time$gd.30[i] == 1){Time$gdn3.[i] <- -1} else {Time$gdn3.[i] <- -2}}
  else{Time$gdn3.[i] <- 0}}
for(i in 1:nrow(Time)){
  if(Time$g[i] > 0 & Time$d[i] < 0 & Time$g01[i] == 2 & Time$d01[i] == 1) {if(Time$gd.20[i] == 1){Time$gdn2.[i] <- -1} else {Time$gdn2.[i] <- -2}}
  else{Time$gdn2.[i] <- 0}}
for(i in 1:nrow(Time)){
  if(Time$g[i] > 0 & Time$d[i] < 0 & Time$g01[i] == 2 & Time$d01[i] == 1) {if(Time$gd.10[i] == 1){Time$gdn1.[i] <- -1} else {Time$gdn1.[i] <- -2}}
  else{Time$gdn1.[i] <- 0}}
for(i in 1:nrow(Time)){
  if(Time$g[i] > 0 & Time$d[i] < 0 & Time$g01[i] == 2 & Time$d01[i] == 1) {if(Time$gd10[i] == 1){Time$gdp1.[i] <- -1} else {Time$gdp1.[i] <- -2}}
  else{Time$gdp1.[i] <- 0}}
for(i in 1:nrow(Time)){
  if(Time$g[i] > 0 & Time$d[i] < 0 & Time$g01[i] == 2 & Time$d01[i] == 1) {if(Time$gd20[i] == 1){Time$gdp2.[i] <- -1} else {Time$gdp2.[i] <- -2}}
  else{Time$gdp2.[i] <- 0}}
for(i in 1:nrow(Time)){
  if(Time$g[i] > 0 & Time$d[i] < 0 & Time$g01[i] == 2 & Time$d01[i] == 1) {if(Time$gd30[i] == 1){Time$gdp3.[i] <- -1} else {Time$gdp3.[i] <- -2}}
  else{Time$gdp3.[i] <- 0}}
for(i in 1:nrow(Time)){
  if(Time$g[i] > 0 & Time$d[i] < 0 & Time$g01[i] == 2 & Time$d01[i] == 1) {if(Time$gd40[i] == 1){Time$gdp4.[i] <- -1} else {Time$gdp4.[i] <- -2}}
  else{Time$gdp4.[i] <- 0}}
options(digits=8)
Time$gda. <- (Time$gdp4.*.0001 + Time$gdp3.*.001 + Time$gdp2.*.01 + Time$gdp1.*.1 + Time$gdn1.*1 + Time$gdn2.*10 + Time$gdn3.*100 + Time$gdn4.*1000); Time$gda.
c <- barplot(table(Time$gda.), ylim = c(0,70), density = 60, main = "Risk Behavior Patterns for Playing Games and Washing Dishes", col = "dark green", las = 2, 
        names = c("2222.2222", "2222.2211", "2222.2111", "2222.1111", "2221.2211", "2221.2111", "2221.1112", "2221.1111", "2211.2221", 
                  "2211.2211", "2211.2111", "2211.1112", "2211.1111", "2111.2111", "2111.1111", "1111.1111", "NA"), ylab = "Counts")
legend("topright", legend = c("NA = Rated Playing Games as Negative or Washing Dishes as Positive or Both", "1 = Certain Option", "2 = Gamble Option", "Digits from Left to Right Indicate Multiplies of 10 Minutes, Starting from 10 to 80 "))
text(c, table(Time$gda.), pos = 3, cex = 1, labels=as.character(table(Time$gda.)))

#mv
Time$mvn4. <- NA
Time$mvn3. <- NA
Time$mvn2. <- NA
Time$mvn1. <- NA
Time$mvp1. <- NA
Time$mvp2. <- NA
Time$mvp3. <- NA
Time$mvp4. <- NA
for(i in 1:nrow(Time)){
  if(Time$m[i] > 0 & Time$v[i] < 0 & Time$m01[i] == 2 & Time$v01[i] == 1) {if(Time$mv.40[i] == 1){Time$mvn4.[i] <- -1} else {Time$mvn4.[i] <- -2}}
  else{Time$mvn4.[i] <- 0}}
for(i in 1:nrow(Time)){
  if(Time$m[i] > 0 & Time$v[i] < 0 & Time$m01[i] == 2 & Time$v01[i] == 1) {if(Time$mv.30[i] == 1){Time$mvn3.[i] <- -1} else {Time$mvn3.[i] <- -2}}
  else{Time$mvn3.[i] <- 0}}
for(i in 1:nrow(Time)){
  if(Time$m[i] > 0 & Time$v[i] < 0 & Time$m01[i] == 2 & Time$v01[i] == 1) {if(Time$mv.20[i] == 1){Time$mvn2.[i] <- -1} else {Time$mvn2.[i] <- -2}}
  else{Time$mvn2.[i] <- 0}}
for(i in 1:nrow(Time)){
  if(Time$m[i] > 0 & Time$v[i] < 0 & Time$m01[i] == 2 & Time$v01[i] == 1) {if(Time$mv.10[i] == 1){Time$mvn1.[i] <- -1} else {Time$mvn1.[i] <- -2}}
  else{Time$mvn1.[i] <- 0}}
for(i in 1:nrow(Time)){
  if(Time$m[i] > 0 & Time$v[i] < 0 & Time$m01[i] == 2 & Time$v01[i] == 1) {if(Time$mv10[i] == 1){Time$mvp1.[i] <- -1} else {Time$mvp1.[i] <- -2}}
  else{Time$mvp1.[i] <- 0}}
for(i in 1:nrow(Time)){
  if(Time$m[i] > 0 & Time$v[i] < 0 & Time$m01[i] == 2 & Time$v01[i] == 1) {if(Time$mv20[i] == 1){Time$mvp2.[i] <- -1} else {Time$mvp2.[i] <- -2}}
  else{Time$mvp2.[i] <- 0}}
for(i in 1:nrow(Time)){
  if(Time$m[i] > 0 & Time$v[i] < 0 & Time$m01[i] == 2 & Time$v01[i] == 1) {if(Time$mv30[i] == 1){Time$mvp3.[i] <- -1} else {Time$mvp3.[i] <- -2}}
  else{Time$mvp3.[i] <- 0}}
for(i in 1:nrow(Time)){
  if(Time$m[i] > 0 & Time$v[i] < 0 & Time$m01[i] == 2 & Time$v01[i] == 1) {if(Time$mv40[i] == 1){Time$mvp4.[i] <- -1} else {Time$mvp4.[i] <- -2}}
  else{Time$mvp4.[i] <- 0}}
options(digits=8)
Time$mva. <- (Time$mvp4.*.0001 + Time$mvp3.*.001 + Time$mvp2.*.01 + Time$mvp1.*.1 + Time$mvn1.*1 + Time$mvn2.*10 + Time$mvn3.*100 + Time$mvn4.*1000); Time$mva.
b <- barplot(table(Time$mva.), ylim = c(0,80), density = 40, main = "Risk Behavior Patterns for Listening to Music and Vacuuming the Theater", col = "red", las = 2, 
        names = c("2222.2222", "2222.2111", "2222.1111", "2221.2211", "2221.2111", "2221.1111", "2211.2211", "2211.2111", "2211.1112",
                  "2211.1111", "2111.2111", "2111.1111", "1111.1111", "NA"), ylab = "Counts")
legend("topright", legend = c("NA = Rated Listening to Music as Negative or Vacuuming a Theater as Positive or Both", "1 = Certain Option", "2 = Gamble Option", "Digits from Left to Right Indicate Multiplies of 10 Minutes, Starting from 10 to 80 "))
text(b, table(Time$mva.), pos = 3, cex = 1, labels=as.character(table(Time$mva.)))

#st
Time$stn4. <- NA
Time$stn3. <- NA
Time$stn2. <- NA
Time$stn1. <- NA
Time$stp1. <- NA
Time$stp2. <- NA
Time$stp3. <- NA
Time$stp4. <- NA
for(i in 1:nrow(Time)){
  if(Time$s[i] > 0 & Time$t[i] < 0 & Time$s01[i] == 2 & Time$t01[i] == 1) {if(Time$st.40[i] == 1){Time$stn4.[i] <- -1} else {Time$stn4.[i] <- -2}}
  else{Time$stn4.[i] <- 0}}
for(i in 1:nrow(Time)){
  if(Time$s[i] > 0 & Time$t[i] < 0 & Time$s01[i] == 2 & Time$t01[i] == 1) {if(Time$st.30[i] == 1){Time$stn3.[i] <- -1} else {Time$stn3.[i] <- -2}}
  else{Time$stn3.[i] <- 0}}
for(i in 1:nrow(Time)){
  if(Time$s[i] > 0 & Time$t[i] < 0 & Time$s01[i] == 2 & Time$t01[i] == 1) {if(Time$st.20[i] == 1){Time$stn2.[i] <- -1} else {Time$stn2.[i] <- -2}}
  else{Time$stn2.[i] <- 0}}
for(i in 1:nrow(Time)){
  if(Time$s[i] > 0 & Time$t[i] < 0 & Time$s01[i] == 2 & Time$t01[i] == 1) {if(Time$st.10[i] == 1){Time$stn1.[i] <- -1} else {Time$stn1.[i] <- -2}}
  else{Time$stn1.[i] <- 0}}
for(i in 1:nrow(Time)){
  if(Time$s[i] > 0 & Time$t[i] < 0 & Time$s01[i] == 2 & Time$t01[i] == 1) {if(Time$st10[i] == 1){Time$stp1.[i] <- -1} else {Time$stp1.[i] <- -2}}
  else{Time$stp1.[i] <- 0}}
for(i in 1:nrow(Time)){
  if(Time$s[i] > 0 & Time$t[i] < 0 & Time$s01[i] == 2 & Time$t01[i] == 1) {if(Time$st20[i] == 1){Time$stp2.[i] <- -1} else {Time$stp2.[i] <- -2}}
  else{Time$stp2.[i] <- 0}}
for(i in 1:nrow(Time)){
  if(Time$s[i] > 0 & Time$t[i] < 0 & Time$s01[i] == 2 & Time$t01[i] == 1) {if(Time$st30[i] == 1){Time$stp3.[i] <- -1} else {Time$stp3[i] <- -2}}
  else{Time$stp3.[i] <- 0}}
for(i in 1:nrow(Time)){
  if(Time$s[i] > 0 & Time$t[i] < 0 & Time$s01[i] == 2 & Time$t01[i] == 1) {if(Time$st40[i] == 1){Time$stp4.[i] <- -1} else {Time$stp4.[i] <- -2}}
  else{Time$stp4.[i] <- 0}}
options(digits=8)
Time$sta. <- (Time$stp4.*.0001 + Time$stp3.*.001 + Time$stp2.*.01 + Time$stp1.*.1 + Time$stn1.*1 + Time$stn2.*10 + Time$stn3.*100 + Time$stn4.*1000); Time$sta.
a <- barplot(table(Time$sta.), ylim = c(0,80), density = 30, main = "Risk Behavior Patterns for Playing Sports and Getting Stuck in a Traffic", col = "blue", las = 2, 
        names = c("2222.2211", "2222.2111", "2222.1111", "2221.2211", "2221.2111", "2221.1111", "2211.2211", "2211.2111", "2211.1111",
                  "2111.2111", "2111.1112", "2111.1111", "1111.1111", "NA"), ylab = "Counts")
legend("topright", legend = c("NA = Rated Playing Sports as Negative or Getting Stuck in a Traffic as Positive or Both", "1 = Certain Option", "2 = Gamble Option", "Digits from Left to Right Indicate Multiplies of 10 Minutes, Starting from 10 to 80 "))
text(a, table(Time$sta.), pos = 3, cex = 1, labels=as.character(table(Time$sta.)))

###### New graph (curve)
dg <- Time[Time$g > 0 & Time$d < 0 & Time$g01 == 2 & Time$d01 == 1,]
ts <- Time[Time$s > 0 & Time$t < 0 & Time$s01 == 2 & Time$t01 == 1,]
vm <- Time[Time$m > 0 & Time$v < 0 & Time$m01 == 2 & Time$v01 == 1,]

######Find proportions
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
#n40
gd40n <- nrow(dg[dg$dgn4 == 1,])/168; gd40n
gd30n <- nrow(dg[dg$dgn3 == 1,])/168; gd30n
gd20n <- nrow(dg[dg$dgn2 == 1,])/168; gd20n
gd10n <- nrow(dg[dg$dgn1 == 1,])/168; gd10n
gdz <- nrow(dg[dg$dgz == 1,])/168; gdz
gd10p <- nrow(dg[dg$dgp1 == 1,])/168; gd10p
gd20p <- nrow(dg[dg$dgp2 == 1,])/168; gd20p
gd30p <- nrow(dg[dg$dgp3 == 1,])/168; gd30p
gd40p <- nrow(dg[dg$dgp4 == 1,])/168; gd40p

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
ts40n <- nrow(ts[ts$tsn4 == 1,])/167; ts40n
ts30n <- nrow(ts[ts$tsn3 == 1,])/167; ts30n
ts20n <- nrow(ts[ts$tsn2 == 1,])/167; ts20n
ts10n <- nrow(ts[ts$tsn1 == 1,])/167; ts10n
tsz <- nrow(ts[ts$tsz == 1,])/167; tsz
ts10p <- nrow(ts[ts$tsp1 == 1,])/167; ts10p
ts20p <- nrow(ts[ts$tsp2 == 1,])/167; ts20p
ts30p <- nrow(ts[ts$tsp3 == 1,])/167; ts30p
ts40p <- nrow(ts[ts$tsp4 == 1,])/167; ts40p

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
#n40
vm40n <- nrow(vm[vm$vmn4 == 1,])/201; vm40n
vm30n <- nrow(vm[vm$vmn3 == 1,])/201; vm30n
vm20n <- nrow(vm[vm$vmn2 == 1,])/201; vm20n
vm10n <- nrow(vm[vm$vmn1 == 1,])/201; vm10n
vmz <- nrow(vm[vm$vmz == 1,])/201; vmz
vm10p <- nrow(vm[vm$vmp1 == 1,])/201; vm10p
vm20p <- nrow(vm[vm$vmp2 == 1,])/201; vm20p
vm30p <- nrow(vm[vm$vmp3 == 1,])/201; vm30p
vm40p <- nrow(vm[vm$vmp4 == 1,])/201; vm40p

pvm <- data.frame(Time = c("-40","-30","-20","-10","0","10","20","30","40"), Proportion = c(vm40n, vm30n, vm20n, vm10n, vmz, vm10p, vm20p, vm30p, vm40p)) 
ggplot(pvm, aes(Time, Proportion, group = 1)) + 
  geom_point() + 
  scale_x_discrete(limits=c("-40","-30","-20","-10","0","10","20","30","40")) +
  geom_line() +
  ggtitle("Proportions of Vacuum and Music") +
  theme(plot.title = element_text(hjust=0.5))

