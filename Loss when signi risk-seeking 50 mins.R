






for(i in 1:nrow(Time)){
  if(Time$vv0[i] == 1 & Time$v50[i] == 2){Time$vr0[i] <- 1}
  else {Time$vr0[i] <- 0}
}
print(Time$vr0)

for(i in 1:nrow(Time)){
  if(Time$dd0[i] == 1 & Time$d50[i] == 2){Time$dr0[i] <- 1}
  else {Time$dr0[i] <- 0}
}
print(Time$dr0)

for(i in 1:nrow(Time)){
  if(Time$tt0[i] == 1 & Time$t50[i] == 2){Time$tr0[i] <- 1}
  else {Time$tr0[i] <- 0}
}
print(Time$tr0)


Time$negresponse0 <- rowSums(Time[,c("vr0","tr0","dr0")])
print(Time$negresponse0)


Time$ngt0 <- 100*Time$negresponse0/Time$neg0; Time$ngt0
length(which(Time$neg0 > 0))
table(Time$ngt0)
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