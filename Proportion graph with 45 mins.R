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
g.$g045 <- NA

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
for(i in 1:nrow(g.)){
  if(g.$g45[i] == 1) {g.$g045[i] <- 1} else {g.$g045[i] <- 0}}
print(g.$g045)

#calculate proportion
g11 <- nrow(g.[g.$g1 == 1,])/nrow(g.); g11
g22 <- nrow(g.[g.$g2 == 1,])/nrow(g.); g22
g33 <- nrow(g.[g.$g3 == 1,])/nrow(g.); g33
g44 <- nrow(g.[g.$g4 == 1,])/nrow(g.); g44
g55 <- nrow(g.[g.$g5 == 1,])/nrow(g.); g55
g66 <- nrow(g.[g.$g6 == 1,])/nrow(g.); g66
g77 <- nrow(g.[g.$g7 == 1,])/nrow(g.); g77
g88 <- nrow(g.[g.$g8 == 1,])/nrow(g.); g88
g455 <- nrow(g.[g.$g045 == 1,])/nrow(g.); g455

g.0 <- data.frame(Time = c("10","20","30","40","45","50","60","70","80"), Proportion = c(g11,g22,g33,g44,g455,g55,g66,g77,g88)) 
ggplot(g.0, aes(Time, Proportion, group = 1)) + 
  geom_point() + 
  scale_x_discrete(limits=c("10","20","30","40","45","50","60","70","80")) +
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
s.$s045 <- NA

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
for(i in 1:nrow(s.)){
  if(s.$s45[i] == 1) {s.$s045[i] <- 1} else {s.$s045[i] <- 0}}
print(s.$s045)


#calculate proportion
s11 <- nrow(s.[s.$s1 == 1,])/nrow(s.); s11
s22 <- nrow(s.[s.$s2 == 1,])/nrow(s.); s22
s33 <- nrow(s.[s.$s3 == 1,])/nrow(s.); s33
s44 <- nrow(s.[s.$s4 == 1,])/nrow(s.); s44
s55 <- nrow(s.[s.$s5 == 1,])/nrow(s.); s55
s66 <- nrow(s.[s.$s6 == 1,])/nrow(s.); s66
s77 <- nrow(s.[s.$s7 == 1,])/nrow(s.); s77
s88 <- nrow(s.[s.$s8 == 1,])/nrow(s.); s88
s455 <- nrow(s.[s.$s045 == 1,])/nrow(s.); s455



s.0 <- data.frame(Time = c("10","20","30","40","45","50","60","70","80"), Proportion = c(s11,s22,s33,s44,s455,s55,s66,s77,s88)) 
ggplot(s.0, aes(Time, Proportion, group = 1)) + 
  geom_point() + 
  scale_x_discrete(limits=c("10","20","30","40","45","50","60","70","80")) +
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
m.$m045 <- NA

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
for(i in 1:nrow(m.)){
  if(m.$m45[i] == 1) {m.$m045[i] <- 1} else {m.$m045[i] <- 0}}
print(m.$m045)


#calculate proportion
m11 <- nrow(m.[m.$m1 == 1,])/nrow(m.); m11
m22 <- nrow(m.[m.$m2 == 1,])/nrow(m.); m22
m33 <- nrow(m.[m.$m3 == 1,])/nrow(m.); m33
m44 <- nrow(m.[m.$m4 == 1,])/nrow(m.); m44
m55 <- nrow(m.[m.$m5 == 1,])/nrow(m.); m55
m66 <- nrow(m.[m.$m6 == 1,])/nrow(m.); m66
m77 <- nrow(m.[m.$m7 == 1,])/nrow(m.); m77
m88 <- nrow(m.[m.$m8 == 1,])/nrow(m.); m88
m455 <- nrow(m.[m.$m045 == 1,])/nrow(m.); m455


m.0 <- data.frame(Time = c("10","20","30","40","45","50","60","70","80"), Proportion = c(m11,m22,m33,m44,m455,m55,m66,m77,m88)) 
ggplot(m.0, aes(Time, Proportion, group = 1)) + 
  geom_point() + 
  scale_x_discrete(limits=c("10","20","30","40","45","50","60","70","80")) +
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
t.$t045 <- NA

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
for(i in 1:nrow(t.)){
  if(t.$t45[i] == 1) {t.$t045[i] <- 1} else {t.$t045[i] <- 0}}
print(t.$t045)


#calculate proportion
t11 <- nrow(t.[t.$t1 == 1,])/nrow(t.); t11
t22 <- nrow(t.[t.$t2 == 1,])/nrow(t.); t22
t33 <- nrow(t.[t.$t3 == 1,])/nrow(t.); t33
t44 <- nrow(t.[t.$t4 == 1,])/nrow(t.); t44
t55 <- nrow(t.[t.$t5 == 1,])/nrow(t.); t55
t66 <- nrow(t.[t.$t6 == 1,])/nrow(t.); t66
t77 <- nrow(t.[t.$t7 == 1,])/nrow(t.); t77
t88 <- nrow(t.[t.$t8 == 1,])/nrow(t.); t88
t455 <- nrow(t.[t.$t045 == 1,])/nrow(t.); t455


t.0 <- data.frame(Time = c("10","20","30","40","45","50","60","70","80"), Proportion = c(t11,t22,t33,t44,t455,t55,t66,t77,t88)) 
ggplot(t.0, aes(Time, Proportion, group = 1)) + 
  geom_point() + 
  scale_x_discrete(limits=c("10","20","30","40","45","50","60","70","80")) +
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
v.$v045 <- NA

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
for(i in 1:nrow(v.)){
  if(v.$v45[i] == 1) {v.$v045[i] <- 1} else {v.$v045[i] <- 0}}
print(v.$v045)

#calculate proportion
v11 <- nrow(v.[v.$v1 == 1,])/nrow(v.); v11
v22 <- nrow(v.[v.$v2 == 1,])/nrow(v.); v22
v33 <- nrow(v.[v.$v3 == 1,])/nrow(v.); v33
v44 <- nrow(v.[v.$v4 == 1,])/nrow(v.); v44
v55 <- nrow(v.[v.$v5 == 1,])/nrow(v.); v55
v66 <- nrow(v.[v.$v6 == 1,])/nrow(v.); v66
v77 <- nrow(v.[v.$v7 == 1,])/nrow(v.); v77
v88 <- nrow(v.[v.$v8 == 1,])/nrow(v.); v88
v455 <- nrow(v.[v.$v045 == 1,])/nrow(v.); v455

v.0 <- data.frame(Time = c("10","20","30","40","45","50","60","70","80"), Proportion = c(v11,v22,v33,v44,v455,v55,v66,v77,v88)) 
ggplot(v.0, aes(Time, Proportion, group = 1)) + 
  geom_point() + 
  scale_x_discrete(limits=c("10","20","30","40","45","50","60","70","80")) +
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
d.$d045 <- NA

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
for(i in 1:nrow(d.)){
  if(d.$d45[i] == 1) {d.$d045[i] <- 1} else {d.$d045[i] <- 0}}
print(d.$d045)


#calculate proportion
d11 <- nrow(d.[d.$d1 == 1,])/nrow(d.); d11
d22 <- nrow(d.[d.$d2 == 1,])/nrow(d.); d22
d33 <- nrow(d.[d.$d3 == 1,])/nrow(d.); d33
d44 <- nrow(d.[d.$d4 == 1,])/nrow(d.); d44
d55 <- nrow(d.[d.$d5 == 1,])/nrow(d.); d55
d66 <- nrow(d.[d.$d6 == 1,])/nrow(d.); d66
d77 <- nrow(d.[d.$d7 == 1,])/nrow(d.); d77
d88 <- nrow(d.[d.$d8 == 1,])/nrow(d.); d88
d455 <- nrow(d.[d.$d045 == 1,])/nrow(d.); d455

d.0 <- data.frame(Time = c("10","20","30","40","45","50","60","70","80"), Proportion = c(d11,d22,d33,d44,d455,d55,d66,d77,d88)) 
ggplot(d.0, aes(Time, Proportion, group = 1)) + 
  geom_point() + 
  scale_x_discrete(limits=c("10","20","30","40","45","50","60","70","80")) +
  geom_line() +
  ggtitle("Proportions People That Chose Certain Options for Dishes") +
  theme(plot.title = element_text(hjust=0.5))


#####graph for positive activities combined#######################
positive <- rbind(g.0, m.0, s.0)
positive$type <- factor(c(rep(c("Games"), times = 9),rep(c("Music"), times = 9),rep(c("Sports"), times = 9))) 


oo01 <- ggplot(data = positive, aes(x = Time, y = Proportion, group = type)) + 
  geom_point(aes(col = type)) + 
  scale_x_discrete(limits=c("10","20","30","40","45","50","60","70","80")) +
  geom_line(aes(col = type)) +
  theme(plot.title = element_text(hjust=0.5)) + 
  theme_bw() + 
  scale_y_continuous(limits = c(0,1)) +
  labs(color = "Activity", title = "Proportions People Who Chose Certain Options for Liked (Positive) Activities")


#####graph for negative activity combined
negative <- rbind(t.0, v.0, d.0)
negative$type <- factor(c(rep(c("Traffic Jam"), times = 9),rep(c("Vacuum"), times = 9),rep(c("Dishes"), times = 9))) 

oo02 <- ggplot(data = negative, aes(x = Time, y = Proportion, group = type)) + 
  geom_point(aes(col = type)) + 
  scale_x_discrete(limits=c("10","20","30","40","45","50","60","70","80")) +
  geom_line(aes(col = type)) +
  theme(plot.title = element_text(hjust=0.5)) +
  scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9")) + 
  theme_bw() +
  scale_y_continuous(limits = c(0,1)) +
  labs(color = "Activity", title = "Proportions People Who Chose Certain Options for Disiked (Negative) Activities")

library(patchwork)
(oo01/oo02)
