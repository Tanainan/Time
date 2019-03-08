par(mfrow = c(1,1))

# see at time = 10 and time = 80 risk behaviors
#consistency for g
gg011 <- NA
gg0111 <- NA
gg081 <- NA
gg0811 <- NA
for(i in 1:nrow(Time)){
  if(Time$g[i] > 0 & Time$g01[i] == 1) {if(Time$g10[i] == 1){gg011[i] <- 1} else {gg011[i] <- 2}}
  else{gg011[i] <- 0}}
for(i in 1:nrow(Time)){
  if(Time$g[i] > 0 & Time$g01[i] == 2) {if(Time$g10[i] == 1){gg0111[i] <- 1} else {gg0111[i] <- 2}}
  else{gg0111[i] <- 0}}
for(i in 1:nrow(Time)){
  if(Time$g[i] > 0 & Time$g89[i] == 1) {if(Time$g80[i] == 1){gg081[i] <- 1} else {gg081[i] <- 2}}
  else{gg081[i] <- 0}}
for(i in 1:nrow(Time)){
  if(Time$g[i] > 0 & Time$g89[i] == 2) {if(Time$g80[i] == 1){gg0811[i] <- 1} else {gg0811[i] <- 2}}
  else{gg0811[i] <- 0}}

gg011 <- as.data.frame(gg011)
gg0111 <- as.data.frame(gg0111)
gg081 <- as.data.frame(gg081)
gg0811 <- as.data.frame(gg0811)

colnames(gg011) <- c("t")
colnames(gg0111) <- c("t")
colnames(gg081) <- c("t")
colnames(gg0811) <- c("t")


egg <- rbind(gg011, gg0111)

egg$w <- factor(c(rep(0, times = 234), rep(1, 234)))

# dodge <- position_dodge(width = 0.9)
ggplot(egg, aes(x = as.numeric(w), fill = factor(t, labels = c("NA", "Certain", "Gamble")))) + 
  geom_bar(position = position_dodge()) + 
  labs(x = "Preference", y = "Count", fill = "Choice") + 
  theme_bw() + 
  scale_x_discrete(limits = c("0 < 10", "0 > 10")) +
  geom_text(vjust=-1, stat='count', aes(label=..count..), position = position_dodge((width = 0.9))) 


egg89 <- rbind(gg081, gg0811)

egg89$w <- factor(c(rep(0, times = 234), rep(1, 234)))

ggplot(egg89, aes(x = as.numeric(w), fill = factor(t, labels = c("NA", "Certain", "Gamble")))) + 
  geom_bar(position = position_dodge()) + 
  labs(x = "Preference", y = "Count", fill = "Choice") + 
  theme_bw() + 
  scale_x_discrete(limits = c("80 < 90", "80 > 90")) +
  geom_text(vjust=-1, stat='count', aes(label=..count..), position = position_dodge((width = 0.9))) 




# consistency for s
ss011 <- NA
ss0111 <- NA
ss081 <- NA
ss0811 <- NA
for(i in 1:nrow(Time)){
  if(Time$s[i] > 0 & Time$s01[i] == 1) {if(Time$s10[i] == 1){ss011[i] <- 1} else {ss011[i] <- 2}}
  else{ss011[i] <- 0}}
for(i in 1:nrow(Time)){
  if(Time$s[i] > 0 & Time$s01[i] == 2) {if(Time$s10[i] == 1){ss0111[i] <- 1} else {ss0111[i] <- 2}}
  else{ss0111[i] <- 0}}

for(i in 1:nrow(Time)){
  if(Time$s[i] > 0 & Time$s89[i] == 1) {if(Time$s80[i] == 1){ss081[i] <- 1} else {ss081[i] <- 2}}
  else{ss081[i] <- 0}}
for(i in 1:nrow(Time)){
  if(Time$s[i] > 0 & Time$s89[i] == 2) {if(Time$s80[i] == 1){ss0811[i] <- 1} else {ss0811[i] <- 2}}
  else{ss0811[i] <- 0}}

ss011 <- as.data.frame(ss011)
ss0111 <- as.data.frame(ss0111)
ss081 <- as.data.frame(ss081)
ss0811 <- as.data.frame(ss0811)

colnames(ss011) <- c("t")
colnames(ss0111) <- c("t")
colnames(ss081) <- c("t")
colnames(ss0811) <- c("t")


es <- rbind(ss011, ss0111)

es$w <- factor(c(rep(0, times = 234), rep(1, 234)))

# dodge <- position_dodge(width = 0.9)
ggplot(es, aes(x = as.numeric(w), fill = factor(t, labels = c("NA", "Certain", "Gamble")))) + 
  geom_bar(position = position_dodge()) + 
  labs(x = "Preference", y = "Count", fill = "Choice") + 
  theme_bw() + 
  scale_x_discrete(limits = c("0 > 10", "0 < 10")) +
  geom_text(vjust=-1, stat='count', aes(label=..count..), position = position_dodge((width = 0.9))) 

es89 <- rbind(ss081, ss0811)

es89$w <- factor(c(rep(0, times = 234), rep(1, 234)))

ggplot(es89, aes(x = as.numeric(w), fill = factor(t, labels = c("NA", "Certain", "Gamble")))) + 
  geom_bar(position = position_dodge()) + 
  labs(x = "Preference", y = "Count", fill = "Choice") + 
  theme_bw() + 
  scale_x_discrete(limits = c("80 > 90", "80 < 90")) +
  geom_text(vjust=-1, stat='count', aes(label=..count..), position = position_dodge((width = 0.9))) 


# consistency for m
mm011 <- NA
mm0111 <- NA
mm081 <- NA
mm0811 <- NA
for(i in 1:nrow(Time)){
  if(Time$m[i] > 0 & Time$m01[i] == 1) {if(Time$m10[i] == 1){mm011[i] <- 1} else {mm011[i] <- 2}}
  else{mm011[i] <- 0}}
for(i in 1:nrow(Time)){
  if(Time$m[i] > 0 & Time$m01[i] == 2) {if(Time$m10[i] == 1){mm0111[i] <- 1} else {mm0111[i] <- 2}}
  else{mm0111[i] <- 0}}

for(i in 1:nrow(Time)){
  if(Time$m[i] > 0 & Time$m89[i] == 1) {if(Time$m80[i] == 1){mm081[i] <- 1} else {mm081[i] <- 2}}
  else{mm081[i] <- 0}}
for(i in 1:nrow(Time)){
  if(Time$m[i] > 0 & Time$m89[i] == 2) {if(Time$m80[i] == 1){mm0811[i] <- 1} else {mm0811[i] <- 2}}
  else{mm0811[i] <- 0}}


mm011 <- as.data.frame(mm011)
mm0111 <- as.data.frame(mm0111)
mm081 <- as.data.frame(mm081)
mm0811 <- as.data.frame(mm0811)

colnames(mm011) <- c("t")
colnames(mm0111) <- c("t")
colnames(mm081) <- c("t")
colnames(mm0811) <- c("t")


emm <- rbind(mm011, mm0111)

emm$w <- factor(c(rep(0, times = 234), rep(1, 234)))

# dodge <- position_dodge(width = 0.9)
ggplot(emm, aes(x = as.numeric(w), fill = factor(t, labels = c("NA", "Certain", "Gamble")))) + 
  geom_bar(position = position_dodge()) + 
  labs(x = "Preference", y = "Count", fill = "Choice") + 
  theme_bw() + 
  scale_x_discrete(limits = c("0 > 10", "0 < 10")) +
  geom_text(vjust=-1, stat='count', aes(label=..count..), position = position_dodge((width = 0.9))) 

emm89 <- rbind(mm081, mm0811)

emm89$w <- factor(c(rep(0, times = 234), rep(1, 234)))

ggplot(emm89, aes(x = as.numeric(w), fill = factor(t, labels = c("NA", "Certain", "Gamble")))) + 
  geom_bar(position = position_dodge()) + 
  labs(x = "Preference", y = "Count", fill = "Choice") + 
  theme_bw() + 
  scale_x_discrete(limits = c("80 > 90", "80 < 90")) +
  geom_text(vjust=-1, stat='count', aes(label=..count..), position = position_dodge((width = 0.9))) 

# consistency for t
tt011 <- NA
tt0111 <- NA
tt081 <- NA
tt0811 <- NA
for(i in 1:nrow(Time)){
  if(Time$t[i] < 0 & Time$t01[i] == 1) {if(Time$t10[i] == 1){tt011[i] <- 1} else {tt011[i] <- 2}}
  else{tt011[i] <- 0}}
for(i in 1:nrow(Time)){
  if(Time$t[i] < 0 & Time$t01[i] == 2) {if(Time$t10[i] == 1){tt0111[i] <- 1} else {tt0111[i] <- 2}}
  else{tt0111[i] <- 0}}

for(i in 1:nrow(Time)){
  if(Time$t[i] < 0 & Time$t89[i] == 1) {if(Time$t80[i] == 1){tt081[i] <- 1} else {tt081[i] <- 2}}
  else{tt081[i] <- 0}}
for(i in 1:nrow(Time)){
  if(Time$t[i] < 0 & Time$t89[i] == 2) {if(Time$t80[i] == 1){tt0811[i] <- 1} else {tt0811[i] <- 2}}
  else{tt0811[i] <- 0}}


tt011 <- as.data.frame(tt011)
tt0111 <- as.data.frame(tt0111)
tt081 <- as.data.frame(tt081)
tt0811 <- as.data.frame(tt0811)

colnames(tt011) <- c("t")
colnames(tt0111) <- c("t")
colnames(tt081) <- c("t")
colnames(tt0811) <- c("t")


ett <- rbind(tt011, tt0111)

ett$w <- factor(c(rep(0, times = 234), rep(1, 234)))

# dodge <- position_dodge(width = 0.9)
ggplot(ett, aes(x = as.numeric(w), fill = factor(t, labels = c("NA", "Certain", "Gamble")))) + 
  geom_bar(position = position_dodge()) + 
  labs(x = "Preference", y = "Count", fill = "Choice") + 
  theme_bw() + 
  scale_x_discrete(limits = c("0 > 10", "0 < 10")) +
  geom_text(vjust=-1, stat='count', aes(label=..count..), position = position_dodge((width = 0.9))) 

ett89 <- rbind(tt081, tt0811)

ett89$w <- factor(c(rep(0, times = 234), rep(1, 234)))

ggplot(ett89, aes(x = as.numeric(w), fill = factor(t, labels = c("NA", "Certain", "Gamble")))) + 
  geom_bar(position = position_dodge()) + 
  labs(x = "Preference", y = "Count", fill = "Choice") + 
  theme_bw() + 
  scale_x_discrete(limits = c("80 > 90", "80 < 90")) +
  geom_text(vjust=-1, stat='count', aes(label=..count..), position = position_dodge((width = 0.9))) 



# consistency for v
vv011 <- NA
vv0111 <- NA
vv081 <- NA
vv0811 <- NA
for(i in 1:nrow(Time)){
  if(Time$v[i] < 0 & Time$v01[i] == 1) {if(Time$v10[i] == 1){vv011[i] <- 1} else {vv011[i] <- 2}}
  else{vv011[i] <- 0}}
for(i in 1:nrow(Time)){
  if(Time$v[i] < 0 & Time$v01[i] == 2) {if(Time$v10[i] == 1){vv0111[i] <- 1} else {vv0111[i] <- 2}}
  else{vv0111[i] <- 0}}

for(i in 1:nrow(Time)){
  if(Time$v[i] < 0 & Time$v89[i] == 1) {if(Time$v80[i] == 1){vv081[i] <- 1} else {vv081[i] <- 2}}
  else{vv081[i] <- 0}}
for(i in 1:nrow(Time)){
  if(Time$v[i] < 0 & Time$v89[i] == 2) {if(Time$v80[i] == 1){vv0811[i] <- 1} else {vv0811[i] <- 2}}
  else{vv0811[i] <- 0}}


vv011 <- as.data.frame(vv011)
vv0111 <- as.data.frame(vv0111)
vv081 <- as.data.frame(vv081)
vv0811 <- as.data.frame(vv0811)

colnames(vv011) <- c("t")
colnames(vv0111) <- c("t")
colnames(vv081) <- c("t")
colnames(vv0811) <- c("t")


evv <- rbind(vv011, vv0111)

evv$w <- factor(c(rep(0, times = 234), rep(1, 234)))

# dodge <- position_dodge(width = 0.9)
ggplot(evv, aes(x = as.numeric(w), fill = factor(t, labels = c("NA", "Certain", "Gamble")))) + 
  geom_bar(position = position_dodge()) + 
  labs(x = "Preference", y = "Count", fill = "Choice") + 
  theme_bw() + 
  scale_x_discrete(limits = c("0 > 10", "0 < 10")) +
  geom_text(vjust=-1, stat='count', aes(label=..count..), position = position_dodge((width = 0.9))) 

evv89 <- rbind(vv081, vv0811)

evv89$w <- factor(c(rep(0, times = 234), rep(1, 234)))

ggplot(evv89, aes(x = as.numeric(w), fill = factor(t, labels = c("NA", "Certain", "Gamble")))) + 
  geom_bar(position = position_dodge()) + 
  labs(x = "Preference", y = "Count", fill = "Choice") + 
  theme_bw() + 
  scale_x_discrete(limits = c("80 > 90", "80 < 90")) +
  geom_text(vjust=-1, stat='count', aes(label=..count..), position = position_dodge((width = 0.9))) 


# consistency for d
dd011 <- NA
dd0111 <- NA
dd081 <- NA
dd0811 <- NA
for(i in 1:nrow(Time)){
  if(Time$d[i] < 0 & Time$d01[i] == 1) {if(Time$d10[i] == 1){dd011[i] <- 1} else {dd011[i] <- 2}}
  else{dd011[i] <- 0}}
for(i in 1:nrow(Time)){
  if(Time$d[i] < 0 & Time$d01[i] == 2) {if(Time$d10[i] == 1){dd0111[i] <- 1} else {dd0111[i] <- 2}}
  else{dd0111[i] <- 0}}

for(i in 1:nrow(Time)){
  if(Time$d[i] < 0 & Time$d89[i] == 1) {if(Time$d80[i] == 1){dd081[i] <- 1} else {dd081[i] <- 2}}
  else{dd081[i] <- 0}}
for(i in 1:nrow(Time)){
  if(Time$d[i] < 0 & Time$d89[i] == 2) {if(Time$d80[i] == 1){dd0811[i] <- 1} else {dd0811[i] <- 2}}
  else{dd0811[i] <- 0}}


dd011 <- as.data.frame(dd011)
dd0111 <- as.data.frame(dd0111)
dd081 <- as.data.frame(dd081)
dd0811 <- as.data.frame(dd0811)

colnames(dd011) <- c("t")
colnames(dd0111) <- c("t")
colnames(dd081) <- c("t")
colnames(dd0811) <- c("t")


edd <- rbind(dd011, dd0111)

edd$w <- factor(c(rep(0, times = 234), rep(1, 234)))

# dodge <- position_dodge(width = 0.9)
ggplot(edd, aes(x = as.numeric(w), fill = factor(t, labels = c("NA", "Certain", "Gamble")))) + 
  geom_bar(position = position_dodge()) + 
  labs(x = "Preference", y = "Count", fill = "Choice") + 
  theme_bw() + 
  scale_x_discrete(limits = c("0 > 10", "0 < 10")) +
  geom_text(vjust=-1, stat='count', aes(label=..count..), position = position_dodge((width = 0.9))) 

edd89 <- rbind(dd081, dd0811)

edd89$w <- factor(c(rep(0, times = 234), rep(1, 234)))

ggplot(edd89, aes(x = as.numeric(w), fill = factor(t, labels = c("NA", "Certain", "Gamble")))) + 
  geom_bar(position = position_dodge()) + 
  labs(x = "Preference", y = "Count", fill = "Choice") + 
  theme_bw() + 
  scale_x_discrete(limits = c("80 > 90", "80 < 90")) +
  geom_text(vjust=-1, stat='count', aes(label=..count..), position = position_dodge((width = 0.9))) 
