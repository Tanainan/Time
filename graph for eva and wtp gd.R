# consistency for evaluation and mixed gamble
# separate when eva diff is positive vs negative
gdwp <- gd9[(which(gd9$gd > 0)),]
gdwn <- gd9[(which(gd9$gd < 0)),]
gdwz <- gd9[(which(gd9$gd == 0)),]

# separate when wtp diff is positive vs negative
# gdwp <- gd9[(which(gd9$gdw > 0)),]
# gdwn <- gd9[(which(gd9$gdw < 0)),]
# gdwz <- gd9[(which(gd9$gdw == 0)),]

# when g > d
g11 <- nrow(gdwp[gdwp$gd.40 == 1,])/nrow(gdwp); g11
g22 <- nrow(gdwp[gdwp$gd.30 == 1,])/nrow(gdwp); g22
g33 <- nrow(gdwp[gdwp$gd.20 == 1,])/nrow(gdwp); g33
g44 <- nrow(gdwp[gdwp$gd.10 == 1,])/nrow(gdwp); g44
g55 <- nrow(gdwp[gdwp$gd0 == 1,])/nrow(gdwp); g55
g66 <- nrow(gdwp[gdwp$gd10 == 1,])/nrow(gdwp); g66
g77 <- nrow(gdwp[gdwp$gd20 == 1,])/nrow(gdwp); g77
g88 <- nrow(gdwp[gdwp$gd30 == 1,])/nrow(gdwp); g88
g99 <- nrow(gdwp[gdwp$gd40 == 1,])/nrow(gdwp); g99



g.10s <- data.frame(Time = c("-40", "-30", "-20", "-10", "0", "10", "20", "30", "40"), Proportion = c(g11,g22,g33,g44,g55,g66,g77,g88,g99)) 
g.10sp <- ggplot(g.10s, aes(Time, Proportion, group = 1)) + 
  geom_point() + 
  scale_x_discrete(limits=c("-40", "-30", "-20", "-10", "0", "10", "20", "30", "40")) +
  geom_line() +
  ggtitle("Proportions People That Chose Certain Options When Games > Dishes") +
  theme(plot.title = element_text(hjust=0.5))

# when g = d
g11z <- nrow(gdwz[gdwz$gd.40 == 1,])/nrow(gdwz); g11z
g22z <- nrow(gdwz[gdwz$gd.30 == 1,])/nrow(gdwz); g22z
g33z <- nrow(gdwz[gdwz$gd.20 == 1,])/nrow(gdwz); g33z
g44z <- nrow(gdwz[gdwz$gd.10 == 1,])/nrow(gdwz); g44z
g55z <- nrow(gdwz[gdwz$gd0 == 1,])/nrow(gdwz); g55z
g66z <- nrow(gdwz[gdwz$gd10 == 1,])/nrow(gdwz); g66z
g77z <- nrow(gdwz[gdwz$gd20 == 1,])/nrow(gdwz); g77z
g88z <- nrow(gdwz[gdwz$gd30 == 1,])/nrow(gdwz); g88z
g99z <- nrow(gdwz[gdwz$gd40 == 1,])/nrow(gdwz); g99z



g.10z <- data.frame(Time = c("-40", "-30", "-20", "-10", "0", "10", "20", "30", "40"), Proportion = c(g11z,g22z,g33z,g44z,g55z,g66z,g77z,g88z,g99z)) 
g.10sz <- ggplot(g.10z, aes(Time, Proportion, group = 1)) + 
  geom_point() + 
  scale_x_discrete(limits=c("-40", "-30", "-20", "-10", "0", "10", "20", "30", "40")) +
  geom_line() +
  ggtitle("Proportions People That Chose Certain Options When Games = Dishes") +
  theme(plot.title = element_text(hjust=0.5))

# when g < d
g11n <- nrow(gdwn[gdwn$gd.40 == 1,])/nrow(gdwn); g11n
g22n <- nrow(gdwn[gdwn$gd.30 == 1,])/nrow(gdwn); g22n
g33n <- nrow(gdwn[gdwn$gd.20 == 1,])/nrow(gdwn); g33n
g44n <- nrow(gdwn[gdwn$gd.10 == 1,])/nrow(gdwn); g44n
g55n <- nrow(gdwn[gdwn$gd0 == 1,])/nrow(gdwn); g55n
g66n <- nrow(gdwn[gdwn$gd10 == 1,])/nrow(gdwn); g66n
g77n <- nrow(gdwn[gdwn$gd20 == 1,])/nrow(gdwn); g77n
g88n <- nrow(gdwn[gdwn$gd30 == 1,])/nrow(gdwn); g88n
g99n <- nrow(gdwn[gdwn$gd40 == 1,])/nrow(gdwn); g99n



g.10n <- data.frame(Time = c("-40", "-30", "-20", "-10", "0", "10", "20", "30", "40"), Proportion = c(g11n,g22n,g33n,g44n,g55n,g66n,g77n,g88n,g99n)) 
g.10sn <- ggplot(g.10n, aes(Time, Proportion, group = 1)) + 
  geom_point() + 
  scale_x_discrete(limits=c("-40", "-30", "-20", "-10", "0", "10", "20", "30", "40")) +
  geom_line() +
  ggtitle("Proportions People That Chose Certain Options When Games < Dishes") +
  theme(plot.title = element_text(hjust=0.5))



#combined gd
posigd <- rbind(g.10s, g.10z, g.10n)
posigd$type <- factor(c(rep(c("More"), times = 9),rep(c("Equal"), times = 9),rep(c("Less"), times = 9))) 


oo01gd <- ggplot(data = posigd, aes(x = Time, y = Proportion, group = type)) + 
  geom_point() + 
  scale_x_discrete(limits=c("-40", "-30", "-20", "-10", "0", "10", "20", "30", "40")) +
  scale_linetype_discrete(limits = c("More","Equal","Less")) +
  geom_line(aes(linetype = type)) +
  theme(plot.title = element_text(hjust=0.5)) + 
  theme_bw() + 
  scale_y_continuous(limits = c(0,1)) +
  labs(linetype = "Activity", title = "Games and Dishes", y = "", x = ""); oo01gd



