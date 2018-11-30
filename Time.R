setwd("~/Desktop/time 2")
library(readr)
Time <- read_csv("Time.csv", skip = 1)
Time <- Time[-c(1),]
Time <- data.frame(sapply(Time, function(x) as.numeric(as.character(x))))
library(memisc)
library(ggplot2)
library(gridExtra)
library(grid)
library(lattice)
library(stats)
summary(Time$age) # age
percent(Time$gender == 1) #male
percent(Time$gender == 2) #female
sd(Time$age) # SD


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

# monotonicity*************************************************************************************
# gain or loss (risk behaviors)
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

hist(Time$pst0, ylim = c(0,200), labels = TRUE, main = "Time Gain and Risk-Aversion", xlab = "Participants' Percentages of Choosing a Certain Option for Time Gain", ylab = "Counts")
hist(Time$ngt0, ylim = c(0,80), labels = TRUE, main = "Time Loss and Risk-Seeking", xlab = "Participants' Percentages of Choosing a Gamble Option for Time Loss", ylab = "Counts")
hist(Time$total0, ylim = c(0,70), labels = TRUE, main = "Time and Reflection Effect", xlab = "Percentages of Correct Predictions of the Reflection Effect", ylab = "Counts")

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


#####Gain only
g. <- Time[Time$g > 0 & Time$g01 == 2 & Time$g89 == 2,]
s. <- Time[Time$s > 0 & Time$s01 == 2 & Time$s89 == 2,]
m. <- Time[Time$m > 0 & Time$m01 == 2 & Time$m89 == 2,]

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


#####Loss only
t. <- Time[Time$t < 0 & Time$t01 == 1 & Time$t89 == 1,]
v. <- Time[Time$v < 0 & Time$v01 == 1 & Time$v89 == 1,]
d. <- Time[Time$d < 0 & Time$d01 == 1 & Time$d89 == 1,]

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


#####graph for positive activities combined#######################
positive <- rbind(g.0, m.0, s.0)
positive$type <- factor(c(rep(c("Games"), times = 8),rep(c("Music"), times = 8),rep(c("Sports"), times = 8))) 


ggplot(data = positive, aes(x = Time, y = Proportion, group = type)) + 
  geom_point(aes(col = type)) + 
  scale_x_discrete(limits=c("10","20","30","40","50","60","70","80")) +
  geom_line(aes(col = type)) +
  ggtitle("Proportions People Who Chose Certain Options for Liked Activities") +
  theme(plot.title = element_text(hjust=0.5))

#####graph for negative activity combined
negative <- rbind(t.0, v.0, d.0)
negative$type <- factor(c(rep(c("Traffic Jam"), times = 8),rep(c("Vacuum"), times = 8),rep(c("Dishes"), times = 8))) 

ggplot(data = negative, aes(x = Time, y = Proportion, group = type)) + 
  geom_point(aes(col = type)) + 
  scale_x_discrete(limits=c("10","20","30","40","50","60","70","80")) +
  geom_line(aes(col = type)) +
  ggtitle("Proportions People Who Chose Certain Options for Disiked Activities") +
  theme(plot.title = element_text(hjust=0.5)) +
  scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"))


#####graph for mixed activities combined
mixed <- rbind(pts, pvm, pgd)
mixed$type <- factor(c(rep(c("Traffic Jam & Sports"), times = 9),rep(c("Vacuum & Music"), times = 9),rep(c("Dishes & Games"), times = 9))) 

ggplot(data = mixed, aes(x = Time, y = Proportion, group = type)) + 
  geom_point(aes(col = type)) + 
  scale_x_discrete(limits=c("-40","-30","-20","-10","0","10","20","30","40")) +
  geom_line(aes(col = type)) +
  ggtitle("Proportions People Who Chose Certain Options for Disiked and Liked Activities") +
  theme(plot.title = element_text(hjust=0.5)) +
  scale_color_brewer(palette="Paired")


#calculate total amount of participants after excluding the monotonicity
#g
length(which(Time$g > 0 & Time$g01 == 2 & Time$g89 == 2)) #128
#s
length(which(Time$s > 0 & Time$s01 == 2 & Time$s89 == 2)) #125
#m
length(which(Time$m > 0 & Time$m01 == 2 & Time$m89 == 2)) #169
#t
length(which(Time$t < 0 & Time$t01 == 1 & Time$t89 == 1)) #215
#d
length(which(Time$d < 0 & Time$d01 == 1 & Time$d89 == 1)) #218
#v
length(which(Time$v < 0 & Time$v01 == 1 & Time$v89 == 1)) #205

#Mixed 
#gd
length(which(Time$g > 0 & Time$g01 == 2 & Time$d01 == 1 & Time$d < 0)) #168
#st
length(which(Time$s > 0 & Time$s01 == 2 & Time$t01 == 1 & Time$t < 0)) #167
#mv
length(which(Time$m > 0 & Time$m01 == 2 & Time$v01 == 1 & Time$v < 0)) #201

#only 80 vs 90 but not 0 vs 10
#g
length(which(Time$g > 0 & Time$g89 == 2)) #156
#s
length(which(Time$s > 0 & Time$s89 == 2)) #157
#m
length(which(Time$m > 0 & Time$m89 == 2)) #171
#t
length(which(Time$t < 0 & Time$t89 == 1)) #218
#d
length(which(Time$d < 0 & Time$d89 == 1)) #223
#v
length(which(Time$v < 0 & Time$v89 == 1)) #207

########################### Descriptive 
#Evaluation for all
c1 <- data.frame(Evaluation = Time$g)
c2 <- data.frame(Evaluation = Time$s)
c3 <- data.frame(Evaluation = Time$m)
# cc <- rbind(c1, c2 ,c3)
# cc$cond <- factor(c(rep(c("Games"), times = 234), rep(c("Sports"), times = 234), rep(c("Music"), times = 234)))
# 
# ggplot(cc, aes(x = Evaluation, fill = cond)) +
#   geom_histogram(alpha=.7, binwidth=1) +
#   theme_bw()

# All g
mean(Time$g)
sd(Time$g)

ggplot(c1, aes(x = Evaluation)) +
  geom_histogram(binwidth=.8, alpha=.5, position="identity", fill="#3300FF") +
  theme_bw() +
  ggtitle("Evaluations for Playing Games for 45 Minutes")

# All s
mean(Time$s)
sd(Time$s)

ggplot(c2, aes(x = Evaluation)) +
  geom_histogram(binwidth=.8, alpha=.5, position="identity", fill="#0066FF") +
  theme_bw() +
  ggtitle("Evaluations for Playing Sports for 45 Minutes")


# All m
mean(Time$m)
sd(Time$m)

ggplot(c3, aes(x = Evaluation)) +
  geom_histogram(binwidth=.8, alpha=.5, position="identity", fill="#3333FF") +
  theme_bw() +
  ggtitle("Evaluations for Listening to Music for 45 Minutes")



c4 <- data.frame(Evaluation = Time$t)
c5 <- data.frame(Evaluation = Time$v)
c6 <- data.frame(Evaluation = Time$d)
# ca <- rbind(c4, c5 ,c6)
# ca$cond <- factor(c(rep(c("Traffic Jam"), times = 234), rep(c("Vacuum"), times = 234), rep(c("Dishes"), times = 234)))
# 
# ggplot(ca, aes(x = Evaluation, fill = cond)) +
#   geom_density(alpha=.1) +
#   theme_bw()

# All t
mean(Time$t)
sd(Time$t)

ggplot(c4, aes(x = Evaluation)) +
  geom_histogram(binwidth=.8, alpha=.5, position="identity", fill="#669933") +
  theme_bw() +
  ggtitle("Evaluations for Getting Stuck in a Traffic Jam for 45 Minutes")


# All v
mean(Time$v)
sd(Time$v)

ggplot(c5, aes(x = Evaluation)) +
  geom_histogram(binwidth=.8, alpha=.5, position="identity", fill="#996666") +
  theme_bw() +
  ggtitle("Evaluations for Vacuuming a Movie Theater for 45 Minutes")

# All d
mean(Time$d)
sd(Time$d)

ggplot(c6, aes(x = Evaluation)) +
  geom_histogram(binwidth=.8, alpha=.5, position="identity", fill="#00cccc") +
  theme_bw() +
  ggtitle("Evaluations for Washing Dishes for 45 Minutes")


################################ only positive or negative

z1 <- data.frame(Evaluation = Time[which(Time$g > 0), c("g")])
z2 <- data.frame(Evaluation = Time[which(Time$s > 0), c("s")])
z3 <- data.frame(Evaluation = Time[which(Time$m > 0), c("m")])
# zz <- rbind(z1, z2 ,z3)
# zz$cond <- factor(c(rep(c("Games"), times = 229), rep(c("Sports"), times = 228), rep(c("Music"), times = 234)))
# 
# ggplot(zz, aes(x = Evaluation, fill = cond)) +
#   geom_histogram(alpha=.7, binwidth=1) +
#   theme_bw() + 
#   ggtitle("Evaluations for Games, Sports, and Music (Positively Only)")


#g
mean(Time$g > 0)
sd(Time$g > 0)

a4 <- ggplot(z1, aes(x = Evaluation)) +
  geom_histogram(binwidth=.8, alpha=.5, position="identity", fill="#00cccc") +
  theme_bw() +
  ggtitle("Playing Games") + 
  scale_y_continuous(limits=c(0,125)) +
  scale_x_continuous(limits=c(0,103)) +
  xlab("") +
  theme(plot.title = element_text(size=12)); a4


#s
mean(Time$s > 0)
sd(Time$s > 0)

a5 <- ggplot(z2, aes(x = Evaluation)) +
  geom_histogram(binwidth=.8, alpha=.5, position="identity", fill="#00cccc") +
  theme_bw() +
  ggtitle("Playing Sports") + 
  scale_y_continuous(limits=c(0,125)) +
  scale_x_continuous(limits=c(0,103)) +
  xlab("") +
  theme(plot.title = element_text(size=12)); a5


#m
mean(Time$m > 0)
sd(Time$m > 0)

a6 <- ggplot(z3, aes(x = Evaluation)) +
  geom_histogram(binwidth=.8, alpha=.5, position="identity", fill="#00cccc") +
  theme_bw() +
  ggtitle("Listening to Music") + 
  scale_y_continuous(limits=c(0,125)) +
  scale_x_continuous(limits=c(0,103)) +
  xlab("") +
  theme(plot.title = element_text(size=12)); a6


z4 <- data.frame(Evaluation = Time[which(Time$t < 0), c("t")])
z5 <- data.frame(Evaluation = Time[which(Time$v < 0), c("v")])
z6 <- data.frame(Evaluation = Time[which(Time$d < 0), c("d")])
# zzz <- rbind(z4, z5 ,z6)
# zzz$cond <- factor(c(rep(c("Traffic Jam"), times = 218), rep(c("Vacuum"), times = 208), rep(c("Dishes"), times = 223)))
# 
# ggplot(zzz, aes(x = Evaluation, fill = cond)) +
#   geom_histogram(alpha=.7, binwidth=1) +
#   theme_bw() + 
#   ggtitle("Evaluations for Traffic Jam, Vacuum, and Dishes (Negatively Only)")


#t
mean(Time$t < 0)
sd(Time$t < 0)

a1 <- ggplot(z4, aes(x = Evaluation)) +
  geom_histogram(binwidth=.8, alpha=.5, position="identity", fill="#996666") +
  theme_bw() +
  ggtitle("Getting Stuck in a Traffic Jam") + 
  scale_y_continuous(limits=c(0,125)) +
  scale_x_continuous(limits=c(-103,0)) +
  xlab("") +
  theme(plot.title = element_text(size=12)); a1


#d
mean(Time$d < 0)
sd(Time$d < 0)

a2 <- ggplot(z5, aes(x = Evaluation)) +
  geom_histogram(binwidth=.8, alpha=.5, position="identity", fill="#996666") +
  theme_bw() +
  ggtitle("Washing Dishes") + 
  scale_y_continuous(limits=c(0,125)) +
  scale_x_continuous(limits=c(-103,0)) +
  xlab("") +
  theme(plot.title = element_text(size=12)); a2


#v
mean(Time$v < 0)
sd(Time$v < 0)

a3 <- ggplot(z6, aes(x = Evaluation)) +
  geom_histogram(binwidth=.8, alpha=.5, position="identity", fill="#996666") +
  theme_bw() +
  ggtitle("Vacuuming a Movie Theater") + 
  scale_y_continuous(limits=c(0,125)) +
  scale_x_continuous(limits=c(-103,0)) +
  xlab("") +
  theme(plot.title = element_text(size=12)); a3

aaa <- paste("Evaluations", "(-100 = Absolutely Dislike and 100 = Absolutely Like)", sep="\n")
grid.arrange(arrangeGrob(a2, top = "Negatively Rated Activities"), arrangeGrob(a4, top = "Positively Rated Activities"), a1, a5, a3, a6, 
             top = textGrob("Evaluations of Doing Positive Activity and Negative Activities for 45 Minutes", gp=gpar(fontsize=15)),
             bottom = aaa)


######################################### WTdo for positive
# g
mean(Time$gp, na.rm = T)
sd(Time$gp, na.rm = T)

b1 <- data.frame(WTP_to_do = Time$gp, na.rm = T)

e1 <- ggplot(b1, aes(x = WTP_to_do)) +
  geom_histogram(binwidth=.8, alpha=.5, position="identity", fill="#00cccc") +
  theme_bw() +
  xlab("") +
  ggtitle("Play Games") + 
  scale_y_continuous(limits=c(0,50)) +
  scale_x_continuous(limits=c(0,101)) +
  theme(plot.title = element_text(size=10)); e1

# s
mean(Time$sp, na.rm = T)
sd(Time$sp, na.rm = T)

b2 <- data.frame(WTP_to_do = Time$sp, na.rm = T)

e2 <- ggplot(b2, aes(x = WTP_to_do)) +
  geom_histogram(binwidth=.8, alpha=.5, position="identity", fill="#00cccc") +
  theme_bw() +
  xlab("") +
  ggtitle("Play Sports") + 
  scale_y_continuous(limits=c(0,50)) +
  scale_x_continuous(limits=c(0,101)) +
  theme(plot.title = element_text(size=10)); e2

# m
mean(Time$mp, na.rm = T)
sd(Time$mp, na.rm = T)

b3 <- data.frame(WTP_to_do = Time$mp, na.rm = T)

e3 <- ggplot(b3, aes(x = WTP_to_do)) +
  geom_histogram(binwidth=.8, alpha=.5, position="identity", fill="#00cccc") +
  theme_bw() +
  xlab("") +
  ggtitle("Listen to Music") + 
  scale_y_continuous(limits=c(0,50)) +
  scale_x_continuous(limits=c(0,101)) +
  theme(plot.title = element_text(size=10)); e3


#WTavoid for negative
# t
mean(Time$tn, na.rm = T)
sd(Time$tn, na.rm = T)

b4 <- data.frame(WTP_to_avoid = Time$tn, na.rm = T)

e4 <- ggplot(b4, aes(x = WTP_to_avoid)) +
  geom_histogram(binwidth=.8, alpha=.5, position="identity", fill="#996666") +
  scale_y_continuous(limits=c(0,50)) +
  scale_x_continuous(limits=c(0,101)) +
  theme_bw() +
  xlab("") +
  ggtitle("Getting Stuck in a Traffic Jam") + 
  theme(plot.title = element_text(size=10)); e4


# v
mean(Time$vn, na.rm = T)
sd(Time$vn, na.rm = T)

b5 <- data.frame(WTP_to_avoid = Time$vn, na.rm = T)

e5 <- ggplot(b5, aes(x = WTP_to_avoid)) +
  geom_histogram(binwidth=.8, alpha=.5, position="identity", fill="#996666") +
  scale_y_continuous(limits=c(0,50)) +
  scale_x_continuous(limits=c(0,101)) +
  theme_bw() +
  xlab("") +
  ggtitle("Vacuuming a Movie Theater") + 
  theme(plot.title = element_text(size=10)); e5


# d
mean(Time$dn, na.rm = T)
sd(Time$dn, na.rm = T)

b6 <- data.frame(WTP_to_avoid = Time$dn, na.rm = T)

e6 <- ggplot(b6, aes(x = WTP_to_avoid)) +
  geom_histogram(binwidth=.8, alpha=.5, position="identity", fill="#996666") +
  scale_y_continuous(limits=c(0,50)) +
  scale_x_continuous(limits=c(0,101)) +
  theme_bw() +
  ggtitle("Washing Dishes") + 
  xlab("") +
  theme(plot.title = element_text(size=10)); e6

grid.arrange(arrangeGrob(e6, top = "Negatively Rated Activities"), arrangeGrob(e1, top = "Positively Rated Activities"), e4, e2, arrangeGrob(e5, bottom = "WTP to Avoid"), arrangeGrob(e3, bottom = "WTP to Do"), top = textGrob("WTP to Do a Positive Activity or to Avoid Doing a Negative Activity for 45 Minutes", gp=gpar(fontsize=15)))

####################### WTP positive - negative 
gd9$gd0.0 <- NA
mv9$mv0.0 <- NA
st9$st0.0 <- NA

####################### WTP percentage of chooing gamble when WTP positive - negative > 0
for (i in 1:nrow(gd9)) {
  if (gd9$gdw[i] > 0 & gd9$gd0[i] == 2) {gd9$gd0.0[i] <- 1} else {gd9$gd0.0[i] <- 0}}
for (i in 1:nrow(mv9)) {
  if (mv9$mvw[i] > 0 & mv9$mv0[i] == 2) {mv9$mv0.0[i] <- 1} else {mv9$mv0.0[i] <- 0}}
for (i in 1:nrow(st9)) {
  if (st9$stw[i] > 0 & st9$st0[i] == 2) {st9$st0.0[i] <- 1} else {st9$st0.0[i] <- 0}}


# average?
min(gd9$gdw)
max(gd9$gdw)
min(mv9$mvw)
max(mv9$mvw)
min(st9$stw)
max(st9$stw)

percent(gd9$gd0.0 == 1)
percent(mv9$mv0.0 == 1)
percent(st9$st0.0 == 1)

o1 <- ggplot(gd9, aes(x = gd9$gdw)) + 
  geom_histogram(binwidth = 1) + 
  scale_y_continuous(limits=c(0,45)) +
  scale_x_continuous(limits=c(-75,75)) +
  theme_bw() +
  ggtitle("Games & Dishes") + 
  theme(plot.title = element_text(size=10)) + 
  labs(x = "Difference of WTPs (Games - Dishes)")

o2 <- ggplot(mv9, aes(x = mv9$mvw)) + 
  geom_histogram(binwidth = 1) + 
  scale_y_continuous(limits=c(0,45)) +
  scale_x_continuous(limits=c(-75,75)) +
  theme_bw() +
  ggtitle("Music & Vacuum") + 
  theme(plot.title = element_text(size=10)) + 
  labs(x = "Difference of WTPs (Music - Vacuum)")

o3 <- ggplot(st9, aes(x = st9$stw)) + 
  geom_histogram(binwidth = 1) + 
  scale_y_continuous(limits=c(0,45)) +
  scale_x_continuous(limits=c(-75,75)) +
  theme_bw() +
  ggtitle("Sports & Traffic Jam") + 
  theme(plot.title = element_text(size=10)) + 
  labs(x = "Difference of WTPs (Sports - Traffic Jam)")

grid.arrange(o1, o2, o3, top = textGrob("WTP to Do a Positive Activity - WTP to Avoid Doing a Negative Activity"))


############## Bubbles (with time, choice, and WTP differences)

f <- data.frame(gd = Time$g - Time$d, # evaluations
                mv = Time$m - Time$v,
                st = Time$s - Time$t,
                gdw = Time$gp - Time$dn, #wtp to do - wtp to avoid
                mvw = Time$mp - Time$vn,
                stw = Time$sp - Time$tn,
                gd0 = Time$gd0, # mixed gamble for 0
                mv0 = Time$mv0,
                st0 = Time$st0,
                Time$gd.10, Time$gd.20, Time$gd.30, Time$gd.40, Time$gd40, Time$gd30, Time$gd20, Time$gd10,
                Time$mv.10, Time$mv.20, Time$mv.30, Time$mv.40, Time$mv40, Time$mv30, Time$mv20, Time$mv10,
                Time$st.10, Time$st.20, Time$st.30, Time$st.40, Time$st40, Time$st30, Time$st20, Time$st10,
                Time$g, Time$d, Time$m, Time$v, Time$s, Time$t)



gd9 <- data.frame(gdw = f[which(complete.cases(f$gdw) == T), c("gdw")], gd0 = f[which(complete.cases(f$gdw) == T), c("gd0")],
                  gd.40 = f[which(complete.cases(f$gdw) == T), c("Time.gd.40")],
                  gd.30 = f[which(complete.cases(f$gdw) == T), c("Time.gd.30")],
                  gd.20 = f[which(complete.cases(f$gdw) == T), c("Time.gd.20")],
                  gd.10 = f[which(complete.cases(f$gdw) == T), c("Time.gd.10")],
                  gd10 = f[which(complete.cases(f$gdw) == T), c("Time.gd10")],
                  gd20 = f[which(complete.cases(f$gdw) == T), c("Time.gd20")],
                  gd30 = f[which(complete.cases(f$gdw) == T), c("Time.gd30")],
                  gd40 = f[which(complete.cases(f$gdw) == T), c("Time.gd40")],
                  g = f[which(complete.cases(f$gdw) == T), c("Time.g")],
                  d = f[which(complete.cases(f$gdw) == T), c("Time.d")])
 

gd10 <-  data.frame(Choice = gd9$gd10, Evaluation = gd9$gdw)
gd20 <-  data.frame(Choice = gd9$gd20, Evaluation = gd9$gdw)
gd30 <-  data.frame(Choice = gd9$gd30, Evaluation = gd9$gdw)
gd40 <-  data.frame(Choice = gd9$gd40, Evaluation = gd9$gdw)
gd.0 <- data.frame(Choice = gd9$gd0, Evaluation = gd9$gdw)
gd.10 <-  data.frame(Choice = gd9$gd.10, Evaluation = gd9$gdw)
gd.20 <-  data.frame(Choice = gd9$gd.20, Evaluation = gd9$gdw)
gd.30 <-  data.frame(Choice = gd9$gd.30, Evaluation = gd9$gdw)
gd.40 <-  data.frame(Choice = gd9$gd.40, Evaluation = gd9$gdw)

gd99<- rbind(gd.40, gd.30, gd.20, gd.10, gd.0, gd10, gd20, gd30, gd40)
gd99$Time <- factor(rep(c("-40", "-30", "-20", "-10", "0", "10", "20", "30", "40"), each = 218))

gd9$Eva_Proportion <- round(ave(seq(nrow(gd9)), gd9[,1], FUN=length), digits = 2) # get proportion of WTP

gd99$Eva_Proportion <- factor(rep(c(gd9$Eva_Proportion), times = 9))

#create choice proportion for each time
pgd$P <- round(1-pgd$Proportion, digits = 2) #use the propotion calculated earlier (make it gamble option)

gd99$Choice_Proportion <- NA
for (i in 1:nrow(gd99)) {
  if (gd99$Time[i] == -40) {gd99$Choice_Proportion[i] <- pgd$P[1]}
  if (gd99$Time[i] == -30) {gd99$Choice_Proportion[i] <- pgd$P[2]}
  if (gd99$Time[i] == -20) {gd99$Choice_Proportion[i] <- pgd$P[3]}
  if (gd99$Time[i] == -10) {gd99$Choice_Proportion[i] <- pgd$P[4]}
  if (gd99$Time[i] == 0) {gd99$Choice_Proportion[i] <- pgd$P[5]}
  if (gd99$Time[i] == 10) {gd99$Choice_Proportion[i] <- pgd$P[6]}
  if (gd99$Time[i] == 20) {gd99$Choice_Proportion[i] <- pgd$P[7]}
  if (gd99$Time[i] == 30) {gd99$Choice_Proportion[i] <- pgd$P[8]}
  if (gd99$Time[i] == 40) {gd99$Choice_Proportion[i] <- pgd$P[9]}
}


gd99 <- data.frame(sapply(gd99, function(x) as.numeric(as.character(x))))
  
gd99$Weight_Choice = gd99$Eva_Proportion * gd99$Choice_Proportion #weighted the choice and the evaluation proportion
gd99 <- data.frame(sapply(gd99, function(x) as.numeric(as.character(x))))


#bubble for gd
ggplot(gd99, aes(x = as.character(Time), y = Evaluation, size = Weight_Choice)) +
  geom_point(alpha = 0.08, color = "#0066cc") +
  scale_x_discrete(limits=c("-40","-30","-20","-10","0","10","20","30","40")) + 
  theme_bw() +
  coord_cartesian(xlim = NULL) +
  labs(x = "Time", y = "Difference of WTP (Games - Dishes)", size = "Gamble Proportion x Difference of WTP") 


##### for mv
mv9 <- data.frame(mvw = f[which(complete.cases(f$mvw) == T), c("mvw")], mv0 = f[which(complete.cases(f$mvw) == T), c("mv0")],
                  mv.40 = f[which(complete.cases(f$mvw) == T), c("Time.mv.40")],
                  mv.30 = f[which(complete.cases(f$mvw) == T), c("Time.mv.30")],
                  mv.20 = f[which(complete.cases(f$mvw) == T), c("Time.mv.20")],
                  mv.10 = f[which(complete.cases(f$mvw) == T), c("Time.mv.10")],
                  mv10 = f[which(complete.cases(f$mvw) == T), c("Time.mv10")],
                  mv20 = f[which(complete.cases(f$mvw) == T), c("Time.mv20")],
                  mv30 = f[which(complete.cases(f$mvw) == T), c("Time.mv30")],
                  mv40 = f[which(complete.cases(f$mvw) == T), c("Time.mv40")], 
                  m = f[which(complete.cases(f$mvw) == T), c("Time.m")],
                  v = f[which(complete.cases(f$mvw) == T), c("Time.v")])


mv10 <-  data.frame(Choice = mv9$mv10, Evaluation = mv9$mvw)
mv20 <-  data.frame(Choice = mv9$mv20, Evaluation = mv9$mvw)
mv30 <-  data.frame(Choice = mv9$mv30, Evaluation = mv9$mvw)
mv40 <-  data.frame(Choice = mv9$mv40, Evaluation = mv9$mvw)
mv.0 <- data.frame(Choice = mv9$mv0, Evaluation = mv9$mvw)
mv.10 <-  data.frame(Choice = mv9$mv.10, Evaluation = mv9$mvw)
mv.20 <-  data.frame(Choice = mv9$mv.20, Evaluation = mv9$mvw)
mv.30 <-  data.frame(Choice = mv9$mv.30, Evaluation = mv9$mvw)
mv.40 <-  data.frame(Choice = mv9$mv.40, Evaluation = mv9$mvw)

mv99<- rbind(mv.40, mv.30, mv.20, mv.10, mv.0, mv10, mv20, mv30, mv40)
mv99$Time <- factor(rep(c("-40", "-30", "-20", "-10", "0", "10", "20", "30", "40"), each = 208))

mv9$Eva_Proportion <- round(ave(seq(nrow(mv9)), mv9[,1], FUN=length), digits = 2) # get proportion of WTP

mv99$Eva_Proportion <- factor(rep(c(mv9$Eva_Proportion), times = 9))

#create choice proportion for each time
pvm$P <- round(1-pvm$Proportion, digits = 2) #use the propotion calculated earlier (make it gamble option)

mv99$Choice_Proportion <- NA
for (i in 1:nrow(mv99)) {
  if (mv99$Time[i] == -40) {mv99$Choice_Proportion[i] <- pvm$P[1]}
  if (mv99$Time[i] == -30) {mv99$Choice_Proportion[i] <- pvm$P[2]}
  if (mv99$Time[i] == -20) {mv99$Choice_Proportion[i] <- pvm$P[3]}
  if (mv99$Time[i] == -10) {mv99$Choice_Proportion[i] <- pvm$P[4]}
  if (mv99$Time[i] == 0) {mv99$Choice_Proportion[i] <- pvm$P[5]}
  if (mv99$Time[i] == 10) {mv99$Choice_Proportion[i] <- pvm$P[6]}
  if (mv99$Time[i] == 20) {mv99$Choice_Proportion[i] <- pvm$P[7]}
  if (mv99$Time[i] == 30) {mv99$Choice_Proportion[i] <- pvm$P[8]}
  if (mv99$Time[i] == 40) {mv99$Choice_Proportion[i] <- pvm$P[9]}
}


mv99 <- data.frame(sapply(mv99, function(x) as.numeric(as.character(x))))

mv99$Weight_Choice = mv99$Eva_Proportion * mv99$Choice_Proportion #weighted the choice and the evaluation proportion
mv99 <- data.frame(sapply(mv99, function(x) as.numeric(as.character(x))))


#bubble for mv
ggplot(mv99, aes(x = as.character(Time), y = Evaluation, size = Weight_Choice)) +
  geom_point(alpha = 0.08, color = "#ff6666") +
  scale_x_discrete(limits=c("-40","-30","-20","-10","0","10","20","30","40")) + 
  theme_bw() +
  coord_cartesian(xlim = NULL) +
  labs(x = "Time", y = "Difference of WTP (Music - Vacuum)", size = "Gamble Proportion x Difference of WTP") 



###### for st

st9 <- data.frame(stw = f[which(complete.cases(f$stw) == T), c("stw")], st0 = f[which(complete.cases(f$stw) == T), c("st0")],
                  st.40 = f[which(complete.cases(f$stw) == T), c("Time.st.40")],
                  st.30 = f[which(complete.cases(f$stw) == T), c("Time.st.30")],
                  st.20 = f[which(complete.cases(f$stw) == T), c("Time.st.20")],
                  st.10 = f[which(complete.cases(f$stw) == T), c("Time.st.10")],
                  st10 = f[which(complete.cases(f$stw) == T), c("Time.st10")],
                  st20 = f[which(complete.cases(f$stw) == T), c("Time.st20")],
                  st30 = f[which(complete.cases(f$stw) == T), c("Time.st30")],
                  st40 = f[which(complete.cases(f$stw) == T), c("Time.st40")], 
                  s = f[which(complete.cases(f$stw) == T), c("Time.s")],
                  t = f[which(complete.cases(f$stw) == T), c("Time.t")])

st10 <-  data.frame(Choice = st9$st10, Evaluation = st9$stw)
st20 <-  data.frame(Choice = st9$st20, Evaluation = st9$stw)
st30 <-  data.frame(Choice = st9$st30, Evaluation = st9$stw)
st40 <-  data.frame(Choice = st9$st40, Evaluation = st9$stw)
st.0 <- data.frame(Choice = st9$st0, Evaluation = st9$stw)
st.10 <-  data.frame(Choice = st9$st.10, Evaluation = st9$stw)
st.20 <-  data.frame(Choice = st9$st.20, Evaluation = st9$stw)
st.30 <-  data.frame(Choice = st9$st.30, Evaluation = st9$stw)
st.40 <-  data.frame(Choice = st9$st.40, Evaluation = st9$stw)

st99<- rbind(st.40, st.30, st.20, st.10, st.0, st10, st20, st30, st40)
st99$Time <- factor(rep(c("-40", "-30", "-20", "-10", "0", "10", "20", "30", "40"), each = 213))

st9$Eva_Proportion <- round(ave(seq(nrow(st9)), st9[,1], FUN=length), digits = 2) # get proportion of WTP

st99$Eva_Proportion <- factor(rep(c(st9$Eva_Proportion), times = 9))

#create choice proportion for each time
pts$P <- round(1-pts$Proportion, digits = 2) #use the propotion calculated earlier (make it gamble option)

st99$Choice_Proportion <- NA
for (i in 1:nrow(st99)) {
  if (st99$Time[i] == -40) {st99$Choice_Proportion[i] <- pts$P[1]}
  if (st99$Time[i] == -30) {st99$Choice_Proportion[i] <- pts$P[2]}
  if (st99$Time[i] == -20) {st99$Choice_Proportion[i] <- pts$P[3]}
  if (st99$Time[i] == -10) {st99$Choice_Proportion[i] <- pts$P[4]}
  if (st99$Time[i] == 0) {st99$Choice_Proportion[i] <- pts$P[5]}
  if (st99$Time[i] == 10) {st99$Choice_Proportion[i] <- pts$P[6]}
  if (st99$Time[i] == 20) {st99$Choice_Proportion[i] <- pts$P[7]}
  if (st99$Time[i] == 30) {st99$Choice_Proportion[i] <- pts$P[8]}
  if (st99$Time[i] == 40) {st99$Choice_Proportion[i] <- pts$P[9]}
}


st99 <- data.frame(sapply(st99, function(x) as.numeric(as.character(x))))

st99$Weight_Choice = st99$Eva_Proportion * st99$Choice_Proportion #weighted the choice and the evaluation proportion
st99 <- data.frame(sapply(st99, function(x) as.numeric(as.character(x))))


#bubble for st
ggplot(st99, aes(x = as.character(Time), y = Evaluation, size = Weight_Choice)) +
  geom_point(alpha = 0.08, color = "#339966") +
  scale_x_discrete(limits=c("-40","-30","-20","-10","0","10","20","30","40")) + 
  theme_bw() +
  coord_cartesian(xlim = NULL) +
  labs(x = "Time", y = "Difference of WTP (Sports - Traffic Jam)", size = "Gamble Proportion x Difference of WTP") 










# gd <- data.frame(Time$gp - Time$dn)
# mv <- data.frame(Time$mp - Time$vn)
# st <-  data.frame(Time$sp - Time$tn)

# Time$gdw = Time$gp - Time$dn #wtp to do - wtp to avoid
# Time$mvw = Time$mp - Time$vn
# Time$stw = Time$sp - Time$tn



