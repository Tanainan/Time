# consistency for evaluation and mixed gamble
# separate when WTP diff is positive vs negative
Time$mv0[Time$mv0 == 0] <- "2"
Time$st0[Time$st0 == 0] <- "2"
Time$gd0[Time$gd0 == 0] <- "2"

f <- data.frame(gd = Time$g + Time$d, # evaluations
                mv = Time$m + Time$v,
                st = Time$s + Time$t,
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
                  d = f[which(complete.cases(f$gdw) == T), c("Time.d")],
                  gd = f[which(complete.cases(f$gdw) == T), c("gd")])

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
                  v = f[which(complete.cases(f$mvw) == T), c("Time.v")],
                  mv = f[which(complete.cases(f$mvw) == T), c("mv")])

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
                  t = f[which(complete.cases(f$stw) == T), c("Time.t")],
                  st = f[which(complete.cases(f$stw) == T), c("st")])


gdwp <- gd9[(which(gd9$gdw > 0)),]
gdwn <- gd9[(which(gd9$gdw < 0)),]
gdwz <- gd9[(which(gd9$gdw == 0)),]

# compare between choices for ppl with WTP diff +, -, or zero
binom.test(nrow(gdwp[which(gdwp$gd0 == 1),]), nrow(gdwp), p = 0.5)
binom.test(nrow(gdwz[which(gdwz$gd0 == 1),]), nrow(gdwz), p = 0.5)
binom.test(nrow(gdwn[which(gdwn$gd0 == 1),]), nrow(gdwn), p = 0.5)
# t.test(as.numeric(gdwp$gd0))
# t.test(as.numeric(gdwz$gd0))
# t.test(as.numeric(gdwn$gd0))

# +

q1 <- ggplot(gdwp, aes(x = as.numeric(gd0))) + 
  geom_bar(width = 0.4, fill = "white", col = "dark red") + 
  scale_x_discrete(limits = c("Neither", "Gamble 50/50")) + 
  labs(x = "", y = "Count", title = "Dishes < Games") +
  theme_bw() + 
  scale_y_continuous(limits = c(0, 100)) +
  geom_text(stat = "count", aes(label = ..count.., y = ..count..), vjust = -1); q1

# -

q12 <- ggplot(gdwn, aes(x = as.numeric(gd0))) + 
  geom_bar(width = 0.4, fill = "white", col = "dark red") + 
  scale_x_discrete(limits = c("Neither", "Gamble 50/50")) + 
  labs(x = "", y = "Count", title = "Dishes > Games") +
  theme_bw() +
  scale_y_continuous(limits = c(0, 100)) +
  geom_text(stat = "count", aes(label = ..count.., y = ..count..), vjust = -1); q12


# = zero

q13 <- ggplot(gdwz, aes(x = as.numeric(gd0))) + 
  geom_bar(width = 0.4, fill = "white", col = "dark red") + 
  scale_x_discrete(limits = c("Neither", "Gamble 50/50")) + 
  labs(x = "", y = "Count", title = " Dishes = Games") +
  scale_y_continuous(limits = c(0, 100)) +
  theme_bw() +
  geom_text(stat = "count", aes(label = ..count.., y = ..count..), vjust = -1); q13


###### st consistency

stwp <- st9[(which(st9$stw > 0)),]
stwn <- st9[(which(st9$stw < 0)),]
stwz <- st9[(which(st9$stw == 0)),]

binom.test(nrow(stwp[which(stwp$st0 == 1),]), nrow(stwp), p = 0.5)
binom.test(nrow(stwz[which(stwz$st0 == 1),]), nrow(stwz), p = 0.5)
binom.test(nrow(stwn[which(stwn$st0 == 1),]), nrow(stwn), p = 0.5)

q2 <- ggplot(stwp, aes(x = as.numeric(st0))) + 
  geom_bar(width = 0.4, fill = "white", col = "dark blue") + 
  scale_x_discrete(limits = c("Neither", "Gamble 50/50")) + 
  labs(x = "", y = "Count", title = "Traffic Jam < Sports") +
  scale_y_continuous(limits = c(0, 100)) +
  theme_bw() +
  geom_text(stat = "count", aes(label = ..count.., y = ..count..), vjust = -1); q2


q22 <- ggplot(stwz, aes(x = as.numeric(st0))) + 
  geom_bar(width = 0.4, fill = "white", col = "dark blue") + 
  scale_x_discrete(limits = c("Neither", "Gamble 50/50")) + 
  labs(x = "", y = "Count", title = "Traffic Jam = Sports") +
  scale_y_continuous(limits = c(0, 100)) +
  theme_bw() +
  geom_text(stat = "count", aes(label = ..count.., y = ..count..), vjust = -1); q22


q23 <- ggplot(stwn, aes(x = as.numeric(st0))) + 
  geom_bar(width = 0.4, fill = "white", col = "dark blue") + 
  scale_x_discrete(limits = c("Neither", "Gamble 50/50")) + 
  labs(x = "", y = "Count", title = "Traffic Jam > Sports") +
  scale_y_continuous(limits = c(0, 100)) +
  theme_bw() +
  geom_text(stat = "count", aes(label = ..count.., y = ..count..), vjust = -1); q23


#### mv

mvwp <- mv9[(which(mv9$mvw > 0)),]
mvwn <- mv9[(which(mv9$mvw < 0)),]
mvwz <- mv9[(which(mv9$mvw == 0)),]

binom.test(nrow(mvwp[which(mvwp$mv0 == 1),]), nrow(mvwp), p = 0.5)
binom.test(nrow(mvwz[which(mvwz$mv0 == 1),]), nrow(mvwz), p = 0.5)
binom.test(nrow(mvwn[which(mvwn$mv0 == 1),]), nrow(mvwn), p = 0.5)

q3 <- ggplot(mvwp, aes(x = as.numeric(mv0))) + 
  geom_bar(width = 0.4, fill = "white", col = "dark green") + 
  scale_x_discrete(limits = c("Neither", "Gamble 50/50")) + 
  labs(x = "", y = "Count", title = "Vacuum < Music") +
  scale_y_continuous(limits = c(0, 100)) +
  theme_bw() +
  geom_text(stat = "count", aes(label = ..count.., y = ..count..), vjust = -1); q3


q31 <- ggplot(mvwz, aes(x = as.numeric(mv0))) + 
  geom_bar(width = 0.4, fill = "white", col = "dark green") + 
  scale_x_discrete(limits = c("Neither", "Gamble 50/50")) + 
  labs(x = "", y = "Count", title = "Vacuum = Music") +
  scale_y_continuous(limits = c(0, 100)) +
  theme_bw() +
  geom_text(stat = "count", aes(label = ..count.., y = ..count..), vjust = -1); q31


q32 <- ggplot(mvwn, aes(x = as.numeric(mv0))) + 
  geom_bar(width = 0.4, fill = "white", col = "dark green") + 
  scale_x_discrete(limits = c("Neither", "Gamble 50/50")) + 
  labs(x = "", y = "Count", title = "Vacuum > Music") +
  scale_y_continuous(limits = c(0, 100)) +
  theme_bw() +
  geom_text(stat = "count", aes(label = ..count.., y = ..count..), vjust = -1); q32


grid.arrange(q1,q13,q12,q2,q22,q23,q3,q31,q32, nrow = 3, top = textGrob("WTP Differences", gp=gpar(fontsize=15)))




#### Evaluation Difference
gd9$gde <- gd9$g + gd9$d
st9$ste <- st9$s + st9$t
mv9$mve <- mv9$m + mv9$v

##### gd

gdwp1 <- gd9[(which(gd9$gde > 0)),]
gdwn1 <- gd9[(which(gd9$gde < 0)),]
gdwz1 <- gd9[(which(gd9$gde == 0)),]

# compare between choices for ppl with WTP diff +, -, or zero
binom.test(nrow(gdwp1[which(gdwp1$gd0 == 1),]), nrow(gdwp1), p = 0.5)
binom.test(nrow(gdwz1[which(gdwz1$gd0 == 1),]), nrow(gdwz1), p = 0.5)
binom.test(nrow(gdwn1[which(gdwn1$gd0 == 1),]), nrow(gdwn1), p = 0.5)

# +

q10 <- ggplot(gdwp1, aes(x = as.numeric(gd0))) + 
  geom_bar(width = 0.4, fill = "white", col = "dark red") + 
  scale_x_discrete(limits = c("Neither", "Gamble 50/50")) + 
  labs(x = "", y = "Count", title = "Dishes < Games") +
  theme_bw() + 
  scale_y_continuous(limits = c(0, 100)) +
  geom_text(stat = "count", aes(label = ..count.., y = ..count..), vjust = -1); q10

# -

q120 <- ggplot(gdwn1, aes(x = as.numeric(gd0))) + 
  geom_bar(width = 0.4, fill = "white", col = "dark red") + 
  scale_x_discrete(limits = c("Neither", "Gamble 50/50")) + 
  labs(x = "", y = "Count", title = "Dishes > Games") +
  theme_bw() +
  scale_y_continuous(limits = c(0, 100)) +
  geom_text(stat = "count", aes(label = ..count.., y = ..count..), vjust = -1); q120


# = zero

q130 <- ggplot(gdwz1, aes(x = as.numeric(gd0))) + 
  geom_bar(width = 0.4, fill = "white", col = "dark red") + 
  scale_x_discrete(limits = c("Neither", "Gamble 50/50")) + 
  labs(x = "", y = "Count", title = " Dishes = Games") +
  scale_y_continuous(limits = c(0, 100)) +
  theme_bw() +
  geom_text(stat = "count", aes(label = ..count.., y = ..count..), vjust = -1); q130


###### st consistency

stwp1 <- st9[(which(st9$ste > 0)),]
stwn1 <- st9[(which(st9$ste < 0)),]
stwz1 <- st9[(which(st9$ste == 0)),]

binom.test(nrow(stwp1[which(stwp1$st0 == 1),]), nrow(stwp1), p = 0.5)
binom.test(nrow(stwz1[which(stwz1$st0 == 1),]), nrow(stwz1), p = 0.5)
binom.test(nrow(stwn1[which(stwn1$st0 == 1),]), nrow(stwn1), p = 0.5)

q20 <- ggplot(stwp1, aes(x = as.numeric(st0))) + 
  geom_bar(width = 0.4, fill = "white", col = "dark blue") + 
  scale_x_discrete(limits = c("Neither", "Gamble 50/50")) + 
  labs(x = "", y = "Count", title = "Traffic Jam < Sports") +
  scale_y_continuous(limits = c(0, 100)) +
  theme_bw() +
  geom_text(stat = "count", aes(label = ..count.., y = ..count..), vjust = -1); q20


q220 <- ggplot(stwz1, aes(x = as.numeric(st0))) + 
  geom_bar(width = 0.4, fill = "white", col = "dark blue") + 
  scale_x_discrete(limits = c("Neither", "Gamble 50/50")) + 
  labs(x = "", y = "Count", title = "Traffic Jam = Sports") +
  scale_y_continuous(limits = c(0, 100)) +
  theme_bw() +
  geom_text(stat = "count", aes(label = ..count.., y = ..count..), vjust = -1); q220


q230 <- ggplot(stwn1, aes(x = as.numeric(st0))) + 
  geom_bar(width = 0.4, fill = "white", col = "dark blue") + 
  scale_x_discrete(limits = c("Neither", "Gamble 50/50")) + 
  labs(x = "", y = "Count", title = "Traffic Jam > Sports") +
  scale_y_continuous(limits = c(0, 100)) +
  theme_bw() +
  geom_text(stat = "count", aes(label = ..count.., y = ..count..), vjust = -1); q230


#### mv

mvwp1 <- mv9[(which(mv9$mvw > 0)),]
mvwn1 <- mv9[(which(mv9$mvw < 0)),]
mvwz1 <- mv9[(which(mv9$mvw == 0)),]

binom.test(nrow(mvwp1[which(mvwp1$mv0 == 1),]), nrow(mvwp1), p = 0.5)
binom.test(nrow(mvwz1[which(mvwz1$mv0 == 1),]), nrow(mvwz1), p = 0.5)
binom.test(nrow(mvwn1[which(mvwn1$mv0 == 1),]), nrow(mvwn1), p = 0.5)

q30 <- ggplot(mvwp1, aes(x = as.numeric(mv0))) + 
  geom_bar(width = 0.4, fill = "white", col = "dark green") + 
  scale_x_discrete(limits = c("Neither", "Gamble 50/50")) + 
  labs(x = "", y = "Count", title = "Vacuum < Music") +
  scale_y_continuous(limits = c(0, 100)) +
  theme_bw() +
  geom_text(stat = "count", aes(label = ..count.., y = ..count..), vjust = -1); q30


q310 <- ggplot(mvwz1, aes(x = as.numeric(mv0))) + 
  geom_bar(width = 0.4, fill = "white", col = "dark green") + 
  scale_x_discrete(limits = c("Neither", "Gamble 50/50")) + 
  labs(x = "", y = "Count", title = "Vacuum = Music") +
  scale_y_continuous(limits = c(0, 100)) +
  theme_bw() +
  geom_text(stat = "count", aes(label = ..count.., y = ..count..), vjust = -1); q310


q320 <- ggplot(mvwn1, aes(x = as.numeric(mv0))) + 
  geom_bar(width = 0.4, fill = "white", col = "dark green") + 
  scale_x_discrete(limits = c("Neither", "Gamble 50/50")) + 
  labs(x = "", y = "Count", title = "Vacuum > Music") +
  scale_y_continuous(limits = c(0, 100)) +
  theme_bw() +
  geom_text(stat = "count", aes(label = ..count.., y = ..count..), vjust = -1); q320


grid.arrange(q10,q130,q120,q20,q220,q230,q30,q310,q320, nrow = 3, top = textGrob("Evaluation Differences", gp=gpar(fontsize=15)))


