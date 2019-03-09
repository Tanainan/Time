# consistency for evaluation and mixed gamble
# separate when WTP diff is positive vs negative
gdwp <- gd9[(which(gd9$gdw > 0)),]
gdwn <- gd9[(which(gd9$gdw < 0)),]
gdwz <- gd9[(which(gd9$gdw == 0)),]

# compare between choices for ppl with WTP diff +, -, or zero
binom.test(nrow(gdwp[which(gdwp$gd.10 == 1),]), nrow(gdwp), p = 0.5)
binom.test(nrow(gdwz[which(gdwz$gd.10 == 1),]), nrow(gdwz), p = 0.5)
binom.test(nrow(gdwn[which(gdwn$gd.10 == 1),]), nrow(gdwn), p = 0.5)

# +

q110 <- ggplot(gdwp, aes(x = gd.10)) + 
  geom_bar(width = 0.4, fill = "white", col = "dark red") + 
  scale_x_discrete(limits = c("Certain", "Gamble 50/50")) + 
  labs(x = "", y = "Count", title = "Games > Dishes") +
  theme_bw() + 
  scale_y_continuous(limits = c(0, 120)) +
  geom_text(stat = "count", aes(label = ..count.., y = ..count..), vjust = -1); q110

# -

q210 <- ggplot(gdwn, aes(x = gd.10)) + 
  geom_bar(width = 0.4, fill = "white", col = "dark red") + 
  scale_x_discrete(limits = c("Certain", "Gamble 50/50")) + 
  labs(x = "", y = "Count", title = "Games < Dishes") +
  theme_bw() +
  scale_y_continuous(limits = c(0, 120)) +
  geom_text(stat = "count", aes(label = ..count.., y = ..count..), vjust = -1); q210


# = zero

q310 <- ggplot(gdwz, aes(x = gd.10)) + 
  geom_bar(width = 0.4, fill = "white", col = "dark red") + 
  scale_x_discrete(limits = c("Certain", "Gamble 50/50")) + 
  labs(x = "", y = "Count", title = " Games = Dishes") +
  scale_y_continuous(limits = c(0, 120)) +
  theme_bw() +
  geom_text(stat = "count", aes(label = ..count.., y = ..count..), vjust = -1); q310


###### st consistency

stwp <- st9[(which(st9$stw > 0)),]
stwn <- st9[(which(st9$stw < 0)),]
stwz <- st9[(which(st9$stw == 0)),]

binom.test(nrow(stwp[which(stwp$st.10 == 1),]), nrow(stwp), p = 0.5)
binom.test(nrow(stwz[which(stwz$st.10 == 1),]), nrow(stwz), p = 0.5)
binom.test(nrow(stwn[which(stwn$st.10 == 1),]), nrow(stwn), p = 0.5)

q220 <- ggplot(stwp, aes(x = st.10)) + 
  geom_bar(width = 0.4, fill = "white", col = "dark blue") + 
  scale_x_discrete(limits = c("Certain", "Gamble 50/50")) + 
  labs(x = "", y = "Count", title = "Sports > Traffic Jam") +
  scale_y_continuous(limits = c(0, 120)) +
  theme_bw() +
  geom_text(stat = "count", aes(label = ..count.., y = ..count..), vjust = -1); q220


q2220 <- ggplot(stwz, aes(x = st.10)) + 
  geom_bar(width = 0.4, fill = "white", col = "dark blue") + 
  scale_x_discrete(limits = c("Certain", "Gamble 50/50")) + 
  labs(x = "", y = "Count", title = "Sports = Traffic Jam") +
  scale_y_continuous(limits = c(0, 120)) +
  theme_bw() +
  geom_text(stat = "count", aes(label = ..count.., y = ..count..), vjust = -1); q2220


q2320 <- ggplot(stwn, aes(x = st.10)) + 
  geom_bar(width = 0.4, fill = "white", col = "dark blue") + 
  scale_x_discrete(limits = c("Certain", "Gamble 50/50")) + 
  labs(x = "", y = "Count", title = "Sports < Traffic Jam") +
  scale_y_continuous(limits = c(0, 120)) +
  theme_bw() +
  geom_text(stat = "count", aes(label = ..count.., y = ..count..), vjust = -1); q2320


#### mv

mvwp <- mv9[(which(mv9$mvw > 0)),]
mvwn <- mv9[(which(mv9$mvw < 0)),]
mvwz <- mv9[(which(mv9$mvw == 0)),]

binom.test(nrow(mvwp[which(mvwp$mv.10 == 1),]), nrow(mvwp), p = 0.5)
binom.test(nrow(mvwz[which(mvwz$mv.10 == 1),]), nrow(mvwz), p = 0.5)
binom.test(nrow(mvwn[which(mvwn$mv.10 == 1),]), nrow(mvwn), p = 0.5)

q330 <- ggplot(mvwp, aes(x = mv.10)) + 
  geom_bar(width = 0.4, fill = "white", col = "dark green") + 
  scale_x_discrete(limits = c("Certain", "Gamble 50/50")) + 
  labs(x = "", y = "Count", title = "Music > Vacuum") +
  scale_y_continuous(limits = c(0, 120)) +
  theme_bw() +
  geom_text(stat = "count", aes(label = ..count.., y = ..count..), vjust = -1); q330


q3130 <- ggplot(mvwz, aes(x = mv.10)) + 
  geom_bar(width = 0.4, fill = "white", col = "dark green") + 
  scale_x_discrete(limits = c("Certain", "Gamble 50/50")) + 
  labs(x = "", y = "Count", title = "Music = Vacuum") +
  scale_y_continuous(limits = c(0, 120)) +
  theme_bw() +
  geom_text(stat = "count", aes(label = ..count.., y = ..count..), vjust = -1); q3130


q3230 <- ggplot(mvwn, aes(x = mv.10)) + 
  geom_bar(width = 0.4, fill = "white", col = "dark green") + 
  scale_x_discrete(limits = c("Certain", "Gamble 50/50")) + 
  labs(x = "", y = "Count", title = "Music < Vacuum") +
  scale_y_continuous(limits = c(0, 120)) +
  theme_bw() +
  geom_text(stat = "count", aes(label = ..count.., y = ..count..), vjust = -1); q3230


grid.arrange(q110,q310,q210,q220,q2220,q2320,q330,q3130,q3230, nrow = 3, top = textGrob("WTP Differences", gp=gpar(fontsize=15)))




#### Evaluation Difference
gd9$gde <- gd9$g + gd9$d
st9$ste <- st9$s + st9$t
mv9$mve <- mv9$m + mv9$v

##### gd

gdwp1 <- gd9[(which(gd9$gde > 0)),]
gdwn1 <- gd9[(which(gd9$gde < 0)),]
gdwz1 <- gd9[(which(gd9$gde == 0)),]

# compare between choices for ppl with WTP diff +, -, or zero
binom.test(nrow(gdwp1[which(gdwp1$gd.10 == 1),]), nrow(gdwp1), p = 0.5)
binom.test(nrow(gdwz1[which(gdwz1$gd.10 == 1),]), nrow(gdwz1), p = 0.5)
binom.test(nrow(gdwn1[which(gdwn1$gd.10 == 1),]), nrow(gdwn1), p = 0.5)

# +

q101 <- ggplot(gdwp1, aes(x = gd.10)) + 
  geom_bar(width = 0.4, fill = "white", col = "dark red") + 
  scale_x_discrete(limits = c("Certain", "Gamble 50/50")) + 
  labs(x = "", y = "Count", title = "Games > Dishes") +
  theme_bw() + 
  scale_y_continuous(limits = c(0, 120)) +
  geom_text(stat = "count", aes(label = ..count.., y = ..count..), vjust = -1); q101

# -

q1201 <- ggplot(gdwn1, aes(x = gd.10)) + 
  geom_bar(width = 0.4, fill = "white", col = "dark red") + 
  scale_x_discrete(limits = c("Certain", "Gamble 50/50")) + 
  labs(x = "", y = "Count", title = "Games < Dishes") +
  theme_bw() +
  scale_y_continuous(limits = c(0, 120)) +
  geom_text(stat = "count", aes(label = ..count.., y = ..count..), vjust = -1); q1201


# = zero
q1301 <- ggplot(gdwz1, aes(x = gd.10, y = length(which(gdwz1$gd.10 == 1)))) + 
  geom_bar(width = 0.4, fill = "white", col = "dark red", stat = "identity") + 
  scale_x_discrete(limits = c("Certain", "Gamble 50/50")) + 
  labs(x = "", y = "Count", title = " Games = Dishes") +
  scale_y_continuous(limits = c(0, 120)) +
  theme_bw() +
  geom_text(stat = "count", aes(label = ..count.., y = ..count..), vjust = -1); q1301


###### st consistency

stwp1 <- st9[(which(st9$ste > 0)),]
stwn1 <- st9[(which(st9$ste < 0)),]
stwz1 <- st9[(which(st9$ste == 0)),]

binom.test(nrow(stwp1[which(stwp1$st.10 == 1),]), nrow(stwp1), p = 0.5)
binom.test(nrow(stwz1[which(stwz1$st.10 == 1),]), nrow(stwz1), p = 0.5)
binom.test(nrow(stwn1[which(stwn1$st.10 == 1),]), nrow(stwn1), p = 0.5)

q201 <- ggplot(stwp1, aes(x = st.10)) + 
  geom_bar(width = 0.4, fill = "white", col = "dark blue") + 
  scale_x_discrete(limits = c("Certain", "Gamble 50/50")) + 
  labs(x = "", y = "Count", title = "Sports > Traffic Jam") +
  scale_y_continuous(limits = c(0, 120)) +
  theme_bw() +
  geom_text(stat = "count", aes(label = ..count.., y = ..count..), vjust = -1); q201


q2201 <- ggplot(stwz1, aes(x = st.10)) + 
  geom_bar(width = 0.4, fill = "white", col = "dark blue") + 
  scale_x_discrete(limits = c("Certain", "Gamble 50/50")) + 
  labs(x = "", y = "Count", title = "Sports = Traffic Jam") +
  scale_y_continuous(limits = c(0, 120)) +
  theme_bw() +
  geom_text(stat = "count", aes(label = ..count.., y = ..count..), vjust = -1); q2201


q2301 <- ggplot(stwn1, aes(x = st.10)) + 
  geom_bar(width = 0.4, fill = "white", col = "dark blue") + 
  scale_x_discrete(limits = c("Certain", "Gamble 50/50")) + 
  labs(x = "", y = "Count", title = "Sports < Traffic Jam") +
  scale_y_continuous(limits = c(0, 120)) +
  theme_bw() +
  geom_text(stat = "count", aes(label = ..count.., y = ..count..), vjust = -1); q2301


#### mv

mvwp1 <- mv9[(which(mv9$mvw > 0)),]
mvwn1 <- mv9[(which(mv9$mvw < 0)),]
mvwz1 <- mv9[(which(mv9$mvw == 0)),]

binom.test(nrow(mvwp1[which(mvwp1$mv.10 == 1),]), nrow(mvwp1), p = 0.5)
binom.test(nrow(mvwz1[which(mvwz1$mv.10 == 1),]), nrow(mvwz1), p = 0.5)
binom.test(nrow(mvwn1[which(mvwn1$mv.10 == 1),]), nrow(mvwn1), p = 0.5)

q301 <- ggplot(mvwp1, aes(x = mv.10)) + 
  geom_bar(width = 0.4, fill = "white", col = "dark green") + 
  scale_x_discrete(limits = c("Certain", "Gamble 50/50")) + 
  labs(x = "", y = "Count", title = "Music > Vacuum") +
  scale_y_continuous(limits = c(0, 120)) +
  theme_bw() +
  geom_text(stat = "count", aes(label = ..count.., y = ..count..), vjust = -1); q301


q3101 <- ggplot(mvwz1, aes(x = mv.10)) + 
  geom_bar(width = 0.4, fill = "white", col = "dark green") + 
  scale_x_discrete(limits = c("Certain", "Gamble 50/50")) + 
  labs(x = "", y = "Count", title = "Music = Vacuum") +
  scale_y_continuous(limits = c(0, 120)) +
  theme_bw() +
  geom_text(stat = "count", aes(label = ..count.., y = ..count..), vjust = -1); q3101


q3201 <- ggplot(mvwn1, aes(x = mv.10)) + 
  geom_bar(width = 0.4, fill = "white", col = "dark green") + 
  scale_x_discrete(limits = c("Certain", "Gamble 50/50")) + 
  labs(x = "", y = "Count", title = "Music < Vacuum") +
  scale_y_continuous(limits = c(0, 120)) +
  theme_bw() +
  geom_text(stat = "count", aes(label = ..count.., y = ..count..), vjust = -1); q3201


grid.arrange(q101,q1301,q1201,q201,q2201,q2301,q301,q3101,q3201, nrow = 3, top = textGrob("Absolute Evaluation Differences", gp=gpar(fontsize=15)))


