
Time$tr10 <- NA
Time$dr10 <- NA
Time$vr10 <- NA

Time$tr20 <- NA
Time$dr20 <- NA
Time$vr20 <- NA

Time$tr30 <- NA
Time$dr30 <- NA
Time$vr30 <- NA

Time$tr40 <- NA
Time$dr40 <- NA
Time$vr40 <- NA

Time$tr50 <- NA
Time$dr50 <- NA
Time$vr50 <- NA

Time$tr60 <- NA
Time$dr60 <- NA
Time$vr60 <- NA

Time$tr70 <- NA
Time$dr70 <- NA
Time$vr70 <- NA

Time$tr80 <- NA
Time$dr80 <- NA
Time$vr80 <- NA


# 10
for(i in 1:nrow(Time)){
  if(Time$vv0[i] == 1 & Time$v10[i] == 2){Time$vr10[i] <- 1}
  else {Time$vr10[i] <- 0}
}
print(Time$vr10)

for(i in 1:nrow(Time)){
  if(Time$dd0[i] == 1 & Time$d10[i] == 2){Time$dr10[i] <- 1}
  else {Time$dr10[i] <- 0}
}
print(Time$dr10)

for(i in 1:nrow(Time)){
  if(Time$tt0[i] == 1 & Time$t10[i] == 2){Time$tr10[i] <- 1}
  else {Time$tr10[i] <- 0}
}
print(Time$tr10)

# 20
for(i in 1:nrow(Time)){
  if(Time$vv0[i] == 1 & Time$v20[i] == 2){Time$vr20[i] <- 1}
  else {Time$vr20[i] <- 0}
}
print(Time$vr20)

for(i in 1:nrow(Time)){
  if(Time$dd0[i] == 1 & Time$d20[i] == 2){Time$dr20[i] <- 1}
  else {Time$dr20[i] <- 0}
}
print(Time$dr20)

for(i in 1:nrow(Time)){
  if(Time$tt0[i] == 1 & Time$t20[i] == 2){Time$tr20[i] <- 1}
  else {Time$tr20[i] <- 0}
}
print(Time$tr20)

# 30
for(i in 1:nrow(Time)){
  if(Time$vv0[i] == 1 & Time$v30[i] == 2){Time$vr30[i] <- 1}
  else {Time$vr30[i] <- 0}
}
print(Time$vr30)

for(i in 1:nrow(Time)){
  if(Time$dd0[i] == 1 & Time$d30[i] == 2){Time$dr30[i] <- 1}
  else {Time$dr30[i] <- 0}
}
print(Time$dr30)

for(i in 1:nrow(Time)){
  if(Time$tt0[i] == 1 & Time$t30[i] == 2){Time$tr30[i] <- 1}
  else {Time$tr30[i] <- 0}
}
print(Time$tr30)

# 40
for(i in 1:nrow(Time)){
  if(Time$vv0[i] == 1 & Time$v40[i] == 2){Time$vr40[i] <- 1}
  else {Time$vr40[i] <- 0}
}
print(Time$vr40)

for(i in 1:nrow(Time)){
  if(Time$dd0[i] == 1 & Time$d40[i] == 2){Time$dr40[i] <- 1}
  else {Time$dr40[i] <- 0}
}
print(Time$dr40)

for(i in 1:nrow(Time)){
  if(Time$tt0[i] == 1 & Time$t40[i] == 2){Time$tr40[i] <- 1}
  else {Time$tr40[i] <- 0}
}
print(Time$tr40)

# 50
for(i in 1:nrow(Time)){
  if(Time$vv0[i] == 1 & Time$v50[i] == 2){Time$vr50[i] <- 1}
  else {Time$vr50[i] <- 0}
}
print(Time$vr50)

for(i in 1:nrow(Time)){
  if(Time$dd0[i] == 1 & Time$d50[i] == 2){Time$dr50[i] <- 1}
  else {Time$dr50[i] <- 0}
}
print(Time$dr50)

for(i in 1:nrow(Time)){
  if(Time$tt0[i] == 1 & Time$t50[i] == 2){Time$tr50[i] <- 1}
  else {Time$tr50[i] <- 0}
}
print(Time$tr50)

# 60
for(i in 1:nrow(Time)){
  if(Time$vv0[i] == 1 & Time$v60[i] == 2){Time$vr60[i] <- 1}
  else {Time$vr60[i] <- 0}
}
print(Time$vr60)

for(i in 1:nrow(Time)){
  if(Time$dd0[i] == 1 & Time$d60[i] == 2){Time$dr60[i] <- 1}
  else {Time$dr60[i] <- 0}
}
print(Time$dr60)

for(i in 1:nrow(Time)){
  if(Time$tt0[i] == 1 & Time$t60[i] == 2){Time$tr60[i] <- 1}
  else {Time$tr60[i] <- 0}
}
print(Time$tr60)

# 70
for(i in 1:nrow(Time)){
  if(Time$vv0[i] == 1 & Time$v70[i] == 2){Time$vr70[i] <- 1}
  else {Time$vr70[i] <- 0}
}
print(Time$vr70)

for(i in 1:nrow(Time)){
  if(Time$dd0[i] == 1 & Time$d70[i] == 2){Time$dr70[i] <- 1}
  else {Time$dr70[i] <- 0}
}
print(Time$dr70)

for(i in 1:nrow(Time)){
  if(Time$tt0[i] == 1 & Time$t70[i] == 2){Time$tr70[i] <- 1}
  else {Time$tr70[i] <- 0}
}
print(Time$tr70)

# 80
for(i in 1:nrow(Time)){
  if(Time$vv0[i] == 1 & Time$v80[i] == 2){Time$vr80[i] <- 1}
  else {Time$vr80[i] <- 0}
}
print(Time$vr80)

for(i in 1:nrow(Time)){
  if(Time$dd0[i] == 1 & Time$d80[i] == 2){Time$dr80[i] <- 1}
  else {Time$dr80[i] <- 0}
}
print(Time$dr80)

for(i in 1:nrow(Time)){
  if(Time$tt0[i] == 1 & Time$t80[i] == 2){Time$tr80[i] <- 1}
  else {Time$tr80[i] <- 0}
}
print(Time$tr80)




Time$negresponse10 <- rowSums(Time[,c("vr10","tr10","dr10")])
print(Time$negresponse10)
Time$negresponse20 <- rowSums(Time[,c("vr20","tr20","dr20")])
print(Time$negresponse20)
Time$negresponse30 <- rowSums(Time[,c("vr30","tr30","dr30")])
print(Time$negresponse30)
Time$negresponse40 <- rowSums(Time[,c("vr40","tr40","dr40")])
print(Time$negresponse40)
Time$negresponse50 <- rowSums(Time[,c("vr50","tr50","dr50")])
print(Time$negresponse50)
Time$negresponse60 <- rowSums(Time[,c("vr60","tr60","dr60")])
print(Time$negresponse60)
Time$negresponse70 <- rowSums(Time[,c("vr70","tr70","dr70")])
print(Time$negresponse70)
Time$negresponse80 <- rowSums(Time[,c("vr80","tr80","dr80")])
print(Time$negresponse80)


ngt10 <- data.frame(nt = (100*Time$negresponse10/Time$neg0)); ngt10
ngt20 <- data.frame(nt = (100*Time$negresponse20/Time$neg0)); ngt20
ngt30 <- data.frame(nt = (100*Time$negresponse30/Time$neg0)); ngt30
ngt40 <- data.frame(nt = (100*Time$negresponse40/Time$neg0)); ngt40
ngt50 <- data.frame(nt = (100*Time$negresponse50/Time$neg0)); ngt50
ngt60 <- data.frame(nt = (100*Time$negresponse60/Time$neg0)); ngt60
ngt70 <- data.frame(nt = (100*Time$negresponse70/Time$neg0)); ngt70
ngt80 <- data.frame(nt = (100*Time$negresponse80/Time$neg0)); ngt80


ntdf <- rbind(ngt10, ngt20, ngt30, ngt40, ngt50, ngt60, ngt70, ngt80)
ntdf$time <- (seq(10, 80, 234))
length(which(Time$neg0 > 0))
table(ngt10)


mean(Time$ngt0, na.rm = T)
sd(Time$ngt0, na.rm = T)

u2 <- ggplot(Time, aes(ngt0)) + 
  geom_bar(fill = "light blue", width = 5) +
  theme_bw() + 
  ggtitle("Time Loss and Risk-Seeking") + 
  xlab("Participants' Percentages of Choosing a Gamble Option for Time Loss") +
  ylab("Count") +
  geom_text(stat = "count", aes(label = ..count.., y = ..count..), vjust = -1) + 
  scale_y_continuous(limits = c(0,200)); u2

t.test(Time$ngt0, y = NULL, mu = 50) 