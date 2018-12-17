# consistency for evaluation and mixed gamble
# separate when WTP diff is positive vs negative
gdwp <- gd9[(which(gd9$gdw > 0)),]
gdwn <- gd9[(which(gd9$gdw < 0)),]
gdwz <- gd9[(which(gd9$gdw == 0)),]

# compare between choices for ppl with WTP diff +, -, or zero
binom.test(nrow(gdwp[which(gdwp$gd0 == 1),]), nrow(gdwp), p = 0.5)
binom.test(nrow(gdwz[which(gdwz$gd0 == 1),]), nrow(gdwz), p = 0.5)
binom.test(nrow(gdwn[which(gdwn$gd0 == 1),]), nrow(gdwn), p = 0.5)

# +

q1 <- ggplot(gdwp, aes(x = gd0)) + 
  geom_bar(width = 0.4, fill = "white", col = "dark red") + 
  scale_x_discrete(limits = c("Neither", "Gamble 50/50")) + 
  labs(x = "", y = "Count", title = "Games > Dishes") +
  theme_bw() + 
  scale_y_continuous(limits = c(0, 100)) +
  geom_text(stat = "count", aes(label = ..count.., y = ..count..), vjust = -1); q1

# -

q12 <- ggplot(gdwn, aes(x = gd0)) + 
  geom_bar(width = 0.4, fill = "white", col = "dark red") + 
  scale_x_discrete(limits = c("Neither", "Gamble 50/50")) + 
  labs(x = "", y = "Count", title = "Games < Dishes") +
  theme_bw() +
  scale_y_continuous(limits = c(0, 100)) +
  geom_text(stat = "count", aes(label = ..count.., y = ..count..), vjust = -1); q12


# = zero

q13 <- ggplot(gdwz, aes(x = gd0)) + 
  geom_bar(width = 0.4, fill = "white", col = "dark red") + 
  scale_x_discrete(limits = c("Neither", "Gamble 50/50")) + 
  labs(x = "", y = "Count", title = " Games = Dishes") +
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

q2 <- ggplot(stwp, aes(x = st0)) + 
  geom_bar(width = 0.4, fill = "white", col = "dark blue") + 
  scale_x_discrete(limits = c("Neither", "Gamble 50/50")) + 
  labs(x = "", y = "Count", title = "Sports > Traffic Jam") +
  scale_y_continuous(limits = c(0, 100)) +
  theme_bw() +
  geom_text(stat = "count", aes(label = ..count.., y = ..count..), vjust = -1); q2


q22 <- ggplot(stwz, aes(x = st0)) + 
  geom_bar(width = 0.4, fill = "white", col = "dark blue") + 
  scale_x_discrete(limits = c("Neither", "Gamble 50/50")) + 
  labs(x = "", y = "Count", title = "Sports = Traffic Jam") +
  scale_y_continuous(limits = c(0, 100)) +
  theme_bw() +
  geom_text(stat = "count", aes(label = ..count.., y = ..count..), vjust = -1); q22


q23 <- ggplot(stwn, aes(x = st0)) + 
  geom_bar(width = 0.4, fill = "white", col = "dark blue") + 
  scale_x_discrete(limits = c("Neither", "Gamble 50/50")) + 
  labs(x = "", y = "Count", title = "Sports < Traffic Jam") +
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

q3 <- ggplot(mvwp, aes(x = mv0)) + 
  geom_bar(width = 0.4, fill = "white", col = "dark green") + 
  scale_x_discrete(limits = c("Neither", "Gamble 50/50")) + 
  labs(x = "", y = "Count", title = "Music > Vacuum") +
  scale_y_continuous(limits = c(0, 100)) +
  theme_bw() +
  geom_text(stat = "count", aes(label = ..count.., y = ..count..), vjust = -1); q3


q31 <- ggplot(mvwz, aes(x = mv0)) + 
  geom_bar(width = 0.4, fill = "white", col = "dark green") + 
  scale_x_discrete(limits = c("Neither", "Gamble 50/50")) + 
  labs(x = "", y = "Count", title = "Music = Vacuum") +
  scale_y_continuous(limits = c(0, 100)) +
  theme_bw() +
  geom_text(stat = "count", aes(label = ..count.., y = ..count..), vjust = -1); q31


q32 <- ggplot(mvwn, aes(x = mv0)) + 
  geom_bar(width = 0.4, fill = "white", col = "dark green") + 
  scale_x_discrete(limits = c("Neither", "Gamble 50/50")) + 
  labs(x = "", y = "Count", title = "Music < Vacuum") +
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

q10 <- ggplot(gdwp1, aes(x = gd0)) + 
  geom_bar(width = 0.4, fill = "white", col = "dark red") + 
  scale_x_discrete(limits = c("Neither", "Gamble 50/50")) + 
  labs(x = "", y = "Count", title = "Games > Dishes") +
  theme_bw() + 
  scale_y_continuous(limits = c(0, 100)) +
  geom_text(stat = "count", aes(label = ..count.., y = ..count..), vjust = -1); q10

# -

q120 <- ggplot(gdwn1, aes(x = gd0)) + 
  geom_bar(width = 0.4, fill = "white", col = "dark red") + 
  scale_x_discrete(limits = c("Neither", "Gamble 50/50")) + 
  labs(x = "", y = "Count", title = "Games < Dishes") +
  theme_bw() +
  scale_y_continuous(limits = c(0, 100)) +
  geom_text(stat = "count", aes(label = ..count.., y = ..count..), vjust = -1); q120


# = zero

q130 <- ggplot(gdwz1, aes(x = gd0)) + 
  geom_bar(width = 0.4, fill = "white", col = "dark red") + 
  scale_x_discrete(limits = c("Neither", "Gamble 50/50")) + 
  labs(x = "", y = "Count", title = " Games = Dishes") +
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

q20 <- ggplot(stwp1, aes(x = st0)) + 
  geom_bar(width = 0.4, fill = "white", col = "dark blue") + 
  scale_x_discrete(limits = c("Neither", "Gamble 50/50")) + 
  labs(x = "", y = "Count", title = "Sports > Traffic Jam") +
  scale_y_continuous(limits = c(0, 100)) +
  theme_bw() +
  geom_text(stat = "count", aes(label = ..count.., y = ..count..), vjust = -1); q200


q220 <- ggplot(stwz1, aes(x = st0)) + 
  geom_bar(width = 0.4, fill = "white", col = "dark blue") + 
  scale_x_discrete(limits = c("Neither", "Gamble 50/50")) + 
  labs(x = "", y = "Count", title = "Sports = Traffic Jam") +
  scale_y_continuous(limits = c(0, 100)) +
  theme_bw() +
  geom_text(stat = "count", aes(label = ..count.., y = ..count..), vjust = -1); q220


q230 <- ggplot(stwn1, aes(x = st0)) + 
  geom_bar(width = 0.4, fill = "white", col = "dark blue") + 
  scale_x_discrete(limits = c("Neither", "Gamble 50/50")) + 
  labs(x = "", y = "Count", title = "Sports < Traffic Jam") +
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

q30 <- ggplot(mvwp1, aes(x = mv0)) + 
  geom_bar(width = 0.4, fill = "white", col = "dark green") + 
  scale_x_discrete(limits = c("Neither", "Gamble 50/50")) + 
  labs(x = "", y = "Count", title = "Music > Vacuum") +
  scale_y_continuous(limits = c(0, 100)) +
  theme_bw() +
  geom_text(stat = "count", aes(label = ..count.., y = ..count..), vjust = -1); q30


q310 <- ggplot(mvwz1, aes(x = mv0)) + 
  geom_bar(width = 0.4, fill = "white", col = "dark green") + 
  scale_x_discrete(limits = c("Neither", "Gamble 50/50")) + 
  labs(x = "", y = "Count", title = "Music = Vacuum") +
  scale_y_continuous(limits = c(0, 100)) +
  theme_bw() +
  geom_text(stat = "count", aes(label = ..count.., y = ..count..), vjust = -1); q310


q320 <- ggplot(mvwn1, aes(x = mv0)) + 
  geom_bar(width = 0.4, fill = "white", col = "dark green") + 
  scale_x_discrete(limits = c("Neither", "Gamble 50/50")) + 
  labs(x = "", y = "Count", title = "Music < Vacuum") +
  scale_y_continuous(limits = c(0, 100)) +
  theme_bw() +
  geom_text(stat = "count", aes(label = ..count.., y = ..count..), vjust = -1); q320


grid.arrange(q10,q130,q120,q20,q220,q230,q30,q310,q320, nrow = 3, top = textGrob("Absolute Evaluation Differences", gp=gpar(fontsize=15)))


