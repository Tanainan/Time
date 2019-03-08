Mix <- data.frame(gd = rep(NA, 234), mv = rep(NA, 234), st = rep(NA, 234), 
                  gd1 = rep(NA, 234), mv1 = rep(NA, 234), st1 = rep(NA, 234),
                  gd. = rep(NA, 234), mv. = rep(NA, 234), st. = rep(NA, 234), 
                  gd1. = rep(NA, 234), mv1. = rep(NA, 234), st1. = rep(NA, 234))


# Those who fulfill the criteria (monotonic 0 vs 10)
for (i in 1:234) {
  if(Time$g[i] > 0 & Time$d[i] < 0 & Time$g01[i] == 2 & Time$d01[i] == 1) {Mix$gd1[i] <- 1} else {Mix$gd1[i] <- 0}}
for (i in 1:234) {
  if(Time$m[i] > 0 & Time$v[i] < 0 & Time$m01[i] == 2 & Time$v01[i] == 1) {Mix$mv1[i] <- 1} else {Mix$mv1[i] <- 0}}
for (i in 1:234) {
  if(Time$s[i] > 0 & Time$t[i] < 0 & Time$s01[i] == 2 & Time$t01[i] == 1) {Mix$st1[i] <- 1} else {Mix$st1[i] <- 0}}



# Those who answer neither activity
for (i in 1:234) {
  if(Time$g[i] > 0 & Time$d[i] < 0 & Time$g01[i] == 2 & Time$d01[i] == 1) {if(Time$gd0[i] == 1){Mix$gd[i] <- 1} else {Mix$gd[i] <- 0}}
  else{Mix$gd[i] <- 0}}
for (i in 1:234) {
  if(Time$m[i] > 0 & Time$v[i] < 0 & Time$m01[i] == 2 & Time$v01[i] == 1) {if(Time$mv0[i] == 1){Mix$mv[i] <- 1} else {Mix$mv[i] <- 0}}
  else{Mix$mv[i] <- 0}}
for (i in 1:234) {
  if(Time$s[i] > 0 & Time$t[i] < 0 & Time$s01[i] == 2 & Time$t01[i] == 1) {if(Time$st0[i] == 1){Mix$st[i] <- 1} else {Mix$st[i] <- 0}}
  else{Mix$st[i] <- 0}}


Mix$ans <- rowSums(Mix[,c("gd","mv","st")])
Mix$all <- rowSums(Mix[,c("gd1","mv1","st1")])

Mix$per <- 100*Mix$ans/Mix$all; Mix$per # monotonic
table(Mix$per)
mean(Mix$per, na.rm = T)
sd(Mix$per, na.rm = T)


t.test(Mix$per, mu = 50, y = NULL)
length(which(Mix$all > 0))


#### non-monotonic #### all
for (i in 1:234) {
  if(Time$g[i] > 0 & Time$d[i] < 0) {Mix$gd1.[i] <- 1} else {Mix$gd1.[i] <- 0}}
for (i in 1:234) {
  if(Time$m[i] > 0 & Time$v[i] < 0) {Mix$mv1.[i] <- 1} else {Mix$mv1.[i] <- 0}}
for (i in 1:234) {
  if(Time$s[i] > 0 & Time$t[i] < 0) {Mix$st1.[i] <- 1} else {Mix$st1.[i] <- 0}}



# Those who answer neither activity # ans
for (i in 1:234) {
  if(Time$g[i] > 0 & Time$d[i] < 0) {if(Time$gd0[i] == 1){Mix$gd.[i] <- 1} else {Mix$gd.[i] <- 0}}
  else{Mix$gd.[i] <- 0}}
for (i in 1:234) {
  if(Time$m[i] > 0 & Time$v[i] < 0) {if(Time$mv0[i] == 1){Mix$mv.[i] <- 1} else {Mix$mv.[i] <- 0}}
  else{Mix$mv.[i] <- 0}}
for (i in 1:234) {
  if(Time$s[i] > 0 & Time$t[i] < 0) {if(Time$st0[i] == 1){Mix$st.[i] <- 1} else {Mix$st.[i] <- 0}}
  else{Mix$st.[i] <- 0}}


Mix$ans. <- rowSums(Mix[,c("gd.","mv.","st.")])
Mix$all. <- rowSums(Mix[,c("gd1.","mv1.","st1.")])

Mix$per. <- 100*Mix$ans./Mix$all.; Mix$per. # non-monotonic
table(Mix$per.)
mean(Mix$per., na.rm = T)
sd(Mix$per., na.rm = T)


t.test(Mix$per., mu = 50, y = NULL)
length(which(Mix$all. > 0))



