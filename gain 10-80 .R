Time$gr10 <- NA
Time$sr10 <- NA
Time$mr10 <- NA

Time$gr20 <- NA
Time$sr20 <- NA
Time$mr20 <- NA

Time$gr30 <- NA
Time$sr30 <- NA
Time$mr30 <- NA

Time$gr40 <- NA
Time$sr40 <- NA
Time$mr40 <- NA

Time$gr50 <- NA
Time$sr50 <- NA
Time$mr50 <- NA

Time$gr60 <- NA
Time$sr60 <- NA
Time$mr60 <- NA

Time$gr70 <- NA
Time$sr70 <- NA
Time$mr70 <- NA

Time$gr80 <- NA
Time$sr80 <- NA
Time$mr80 <- NA

# 10
for(i in 1:nrow(Time)){
  if(Time$gg0[i] == 1 & Time$g10[i] == 1){Time$gr10[i] <- 1}
  else {Time$gr10[i] <- 0}
}
print(Time$gr10)

for(i in 1:nrow(Time)){
  if(Time$mm0[i] == 1 & Time$m10[i] == 1){Time$mr10[i] <- 1}
  else {Time$mr10[i] <- 0}
}
print(Time$mr10)

for(i in 1:nrow(Time)){
  if(Time$ss0[i] == 1 & Time$s10[i] == 1){Time$sr10[i] <- 1}
  else {Time$sr10[i] <- 0}
}
print(Time$sr10)

# 20
for(i in 1:nrow(Time)){
  if(Time$gg0[i] == 1 & Time$g20[i] == 1){Time$gr20[i] <- 1}
  else {Time$gr20[i] <- 0}
}
print(Time$gr20)

for(i in 1:nrow(Time)){
  if(Time$mm0[i] == 1 & Time$m20[i] == 1){Time$mr20[i] <- 1}
  else {Time$mr20[i] <- 0}
}
print(Time$mr20)

for(i in 1:nrow(Time)){
  if(Time$ss0[i] == 1 & Time$s20[i] == 1){Time$sr20[i] <- 1}
  else {Time$sr20[i] <- 0}
}
print(Time$sr20)

# 30
for(i in 1:nrow(Time)){
  if(Time$gg0[i] == 1 & Time$g30[i] == 1){Time$gr30[i] <- 1}
  else {Time$gr30[i] <- 0}
}
print(Time$gr30)

for(i in 1:nrow(Time)){
  if(Time$mm0[i] == 1 & Time$m30[i] == 1){Time$mr30[i] <- 1}
  else {Time$mr30[i] <- 0}
}
print(Time$mr30)

for(i in 1:nrow(Time)){
  if(Time$ss0[i] == 1 & Time$s30[i] == 1){Time$sr30[i] <- 1}
  else {Time$sr30[i] <- 0}
}
print(Time$sr30)

# 40
for(i in 1:nrow(Time)){
  if(Time$gg0[i] == 1 & Time$g40[i] == 1){Time$gr40[i] <- 1}
  else {Time$gr40[i] <- 0}
}
print(Time$gr40)

for(i in 1:nrow(Time)){
  if(Time$mm0[i] == 1 & Time$m40[i] == 1){Time$mr40[i] <- 1}
  else {Time$mr40[i] <- 0}
}
print(Time$mr40)

for(i in 1:nrow(Time)){
  if(Time$ss0[i] == 1 & Time$s40[i] == 1){Time$sr40[i] <- 1}
  else {Time$sr40[i] <- 0}
}
print(Time$sr40)

# 50
for(i in 1:nrow(Time)){
  if(Time$gg0[i] == 1 & Time$g50[i] == 1){Time$gr50[i] <- 1}
  else {Time$gr50[i] <- 0}
}
print(Time$gr50)

for(i in 1:nrow(Time)){
  if(Time$mm0[i] == 1 & Time$m50[i] == 1){Time$mr50[i] <- 1}
  else {Time$mr50[i] <- 0}
}
print(Time$mr50)

for(i in 1:nrow(Time)){
  if(Time$ss0[i] == 1 & Time$s50[i] == 1){Time$sr50[i] <- 1}
  else {Time$sr50[i] <- 0}
}
print(Time$sr50)

# 60
for(i in 1:nrow(Time)){
  if(Time$gg0[i] == 1 & Time$g60[i] == 1){Time$gr60[i] <- 1}
  else {Time$gr60[i] <- 0}
}
print(Time$gr60)

for(i in 1:nrow(Time)){
  if(Time$mm0[i] == 1 & Time$m60[i] == 1){Time$mr60[i] <- 1}
  else {Time$mr60[i] <- 0}
}
print(Time$mr60)

for(i in 1:nrow(Time)){
  if(Time$ss0[i] == 1 & Time$s60[i] == 1){Time$sr60[i] <- 1}
  else {Time$sr60[i] <- 0}
}
print(Time$sr60)

# 70
for(i in 1:nrow(Time)){
  if(Time$gg0[i] == 1 & Time$g70[i] == 1){Time$gr70[i] <- 1}
  else {Time$gr70[i] <- 0}
}
print(Time$gr70)

for(i in 1:nrow(Time)){
  if(Time$mm0[i] == 1 & Time$m70[i] == 1){Time$mr70[i] <- 1}
  else {Time$mr70[i] <- 0}
}
print(Time$mr70)

for(i in 1:nrow(Time)){
  if(Time$ss0[i] == 1 & Time$s70[i] == 1){Time$sr70[i] <- 1}
  else {Time$sr70[i] <- 0}
}
print(Time$sr70)

# 80
for(i in 1:nrow(Time)){
  if(Time$gg0[i] == 1 & Time$g80[i] == 1){Time$gr80[i] <- 1}
  else {Time$gr80[i] <- 0}
}
print(Time$gr80)

for(i in 1:nrow(Time)){
  if(Time$mm0[i] == 1 & Time$m80[i] == 1){Time$mr80[i] <- 1}
  else {Time$mr80[i] <- 0}
}
print(Time$mr80)

for(i in 1:nrow(Time)){
  if(Time$ss0[i] == 1 & Time$s80[i] == 1){Time$sr80[i] <- 1}
  else {Time$sr80[i] <- 0}
}
print(Time$sr80)




Time$posresponse10 <- rowSums(Time[,c("gr10","sr10","mr10")])
print(Time$posresponse10)
Time$posresponse20 <- rowSums(Time[,c("gr20","sr20","mr20")])
print(Time$posresponse20)
Time$posresponse30 <- rowSums(Time[,c("gr30","sr30","mr30")])
print(Time$posresponse30)
Time$posresponse40 <- rowSums(Time[,c("gr40","sr40","mr40")])
print(Time$posresponse40)
Time$posresponse50 <- rowSums(Time[,c("gr50","sr50","mr50")])
print(Time$posresponse50)
Time$posresponse60 <- rowSums(Time[,c("gr60","sr60","mr60")])
print(Time$posresponse60)
Time$posresponse70 <- rowSums(Time[,c("gr70","sr70","mr70")])
print(Time$posresponse70)
Time$posresponse80 <- rowSums(Time[,c("gr80","sr80","mr80")])
print(Time$posresponse80)


psss10 <- data.frame(pt = (100*Time$posresponse10/Time$posi0)); psss10
psss20 <- data.frame(pt = (100*Time$posresponse20/Time$posi0)); psss20
psss30 <- data.frame(pt = (100*Time$posresponse30/Time$posi0)); psss30
psss40 <- data.frame(pt = (100*Time$posresponse40/Time$posi0)); psss40
psss50 <- data.frame(pt = (100*Time$posresponse50/Time$posi0)); psss50
psss60 <- data.frame(pt = (100*Time$posresponse60/Time$posi0)); psss60
psss70 <- data.frame(pt = (100*Time$posresponse70/Time$posi0)); psss70
psss80 <- data.frame(pt = (100*Time$posresponse80/Time$posi0)); psss80

psss10$freq <- NA
psss20$freq <- NA
psss30$freq <- NA
psss40$freq <- NA
psss50$freq <- NA
psss60$freq <- NA
psss70$freq <- NA
psss80$freq <- NA


ptdf <- rbind(psss10, psss20, psss30, psss40, psss50, psss60, psss70, psss80)
ptdf$time <- rep(c(10,20,30,40,50,60,70,80), each = 234)
round(ptdf$pt, digits = 2)



# table(psss10)
# table(psss20)
# table(psss30)
# table(psss40)
# table(psss50)
# table(psss60)
# table(psss70)
# table(psss80)


# mean(Time$psss0, na.rm = T)
# sd(Time$psss0, na.rm = T)

# ggplot(ptdf, aes(y = as.character(time), x = pt)) +
#   geom_density_ridges(scale = 2, alpha = 0.8) + 
#   #scale_fill_brewer(palesse = 5) +
#   theme_ridges() +
#   #scale_x_discrete(limits = c("0","33.33","50","66.67","100")) +
#   #scale_y_discrete(limits=c("-40","-30","-20","-10","0","10","20","30","40")) +
#   labs(y = "Time", x = "Proportion", title = "Risk-seeking for Disliked Activities")

# ggplot(ptdf, aes(x = pt, y = as.character(time))) +
#   geom_poipt(alpha = 0.08, color = "#339966") +
#   #scale_x_discrete(limits=c("-40","-30","-20","-10","0","10","20","30","40")) +
#   theme_bw() +
#   coord_cartesian(xlim = NULL) +
#   #scale_x_discrete(limits = c("0.00","33.33","50.00","66.67","100.00")) +
#   labs(x = "Proportion", y = "Time")

#I coupt the occurence of each couple of values. Eg : number of time a=1 and b=1, number of time a=1 and b=2 etc...
AA=xyTable(ptdf$pt,ptdf$time)

par(mfrow = c(1,1))

#Now I can plot this ! I represept the dots as big as the couple occurs often
coeff_bigger=0.08
plot(AA$x , AA$y , cex=AA$number*coeff_bigger  , pch=16 , col=rgb(1,0,1,0.5),
     main= "Percentage of Risk-Aversion for Doing Liked Activities" , ylab="Minutes", xlab = "Percentage")
abline(h = 45, lty = 5)
#text (AA$x , AA$y , AA$number )