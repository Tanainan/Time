# consistency for evaluation and mixed gamble
# separate when WTP diff is positive vs negative
mvwp <- mv9[(which(mv9$mv > 0)),]
mvwn <- mv9[(which(mv9$mv < 0)),]
mvwz <- mv9[(which(mv9$mv == 0)),]


# when m > v
m11 <- nrow(mvwp[mvwp$mv.40 == 1,])/nrow(mvwp); m11
m22 <- nrow(mvwp[mvwp$mv.30 == 1,])/nrow(mvwp); m22
m33 <- nrow(mvwp[mvwp$mv.20 == 1,])/nrow(mvwp); m33
m44 <- nrow(mvwp[mvwp$mv.10 == 1,])/nrow(mvwp); m44
m55 <- nrow(mvwp[mvwp$mv0 == 1,])/nrow(mvwp); m55
m66 <- nrow(mvwp[mvwp$mv10 == 1,])/nrow(mvwp); m66
m77 <- nrow(mvwp[mvwp$mv20 == 1,])/nrow(mvwp); m77
m88 <- nrow(mvwp[mvwp$mv30 == 1,])/nrow(mvwp); m88
m99 <- nrow(mvwp[mvwp$mv40 == 1,])/nrow(mvwp); m99



m.10s <- data.frame(Time = c("-40", "-30", "-20", "-10", "0", "10", "20", "30", "40"), Proportion = c(m11,m22,m33,m44,m55,m66,m77,m88,m99)) 
m.10sp <- ggplot(m.10s, aes(Time, Proportion, group = 1)) + 
  geom_point() + 
  scale_x_discrete(limits=c("-40", "-30", "-20", "-10", "0", "10", "20", "30", "40")) +
  geom_line() +
  ggtitle("Proportions People That Chose Certain Options When Music > Vacuum") +
  theme(plot.title = element_text(hjust=0.5))

# when m = v
m11z <- nrow(mvwz[mvwz$mv.40 == 1,])/nrow(mvwz); m11z
m22z <- nrow(mvwz[mvwz$mv.30 == 1,])/nrow(mvwz); m22z
m33z <- nrow(mvwz[mvwz$mv.20 == 1,])/nrow(mvwz); m33z
m44z <- nrow(mvwz[mvwz$mv.10 == 1,])/nrow(mvwz); m44z
m55z <- nrow(mvwz[mvwz$mv0 == 1,])/nrow(mvwz); m55z
m66z <- nrow(mvwz[mvwz$mv10 == 1,])/nrow(mvwz); m66z
m77z <- nrow(mvwz[mvwz$mv20 == 1,])/nrow(mvwz); m77z
m88z <- nrow(mvwz[mvwz$mv30 == 1,])/nrow(mvwz); m88z
m99z <- nrow(mvwz[mvwz$mv40 == 1,])/nrow(mvwz); m99z



m.10z <- data.frame(Time = c("-40", "-30", "-20", "-10", "0", "10", "20", "30", "40"), Proportion = c(m11z,m22z,m33z,m44z,m55z,m66z,m77z,m88z,m99z)) 
m.10sz <- ggplot(m.10z, aes(Time, Proportion, group = 1)) + 
  geom_point() + 
  scale_x_discrete(limits=c("-40", "-30", "-20", "-10", "0", "10", "20", "30", "40")) +
  geom_line() +
  ggtitle("Proportions People That Chose Certain Options When Music = Vacuum") +
  theme(plot.title = element_text(hjust=0.5))

# when m < v
m11n <- nrow(mvwn[mvwn$mv.40 == 1,])/nrow(mvwn); m11n
m22n <- nrow(mvwn[mvwn$mv.30 == 1,])/nrow(mvwn); m22n
m33n <- nrow(mvwn[mvwn$mv.20 == 1,])/nrow(mvwn); m33n
m44n <- nrow(mvwn[mvwn$mv.10 == 1,])/nrow(mvwn); m44n
m55n <- nrow(mvwn[mvwn$mv0 == 1,])/nrow(mvwn); m55n
m66n <- nrow(mvwn[mvwn$mv10 == 1,])/nrow(mvwn); m66n
m77n <- nrow(mvwn[mvwn$mv20 == 1,])/nrow(mvwn); m77n
m88n <- nrow(mvwn[mvwn$mv30 == 1,])/nrow(mvwn); m88n
m99n <- nrow(mvwn[mvwn$mv40 == 1,])/nrow(mvwn); m99n



m.10n <- data.frame(Time = c("-40", "-30", "-20", "-10", "0", "10", "20", "30", "40"), Proportion = c(m11n,m22n,m33n,m44n,m55n,m66n,m77n,m88n,m99n)) 
m.10sn <- ggplot(m.10n, aes(Time, Proportion, group = 1)) + 
  geom_point() + 
  scale_x_discrete(limits=c("-40", "-30", "-20", "-10", "0", "10", "20", "30", "40")) +
  geom_line() +
  ggtitle("Proportions People That Chose Certain Options When Music < Vacuum") +
  theme(plot.title = element_text(hjust=0.5))



#combined mv
posimv <- rbind(m.10s, m.10z, m.10n)
posimv$type <- factor(c(rep(c("more"), times = 9),rep(c("equal"), times = 9),rep(c("less"), times = 9))) 


oo01mv <- ggplot(data = posimv, aes(x = Time, y = Proportion, group = type)) + 
  geom_point() + 
  scale_x_discrete(limits=c("-40", "-30", "-20", "-10", "0", "10", "20", "30", "40")) +
  geom_line(aes(linetype = type)) +
  theme(plot.title = element_text(hjust=0.5)) + 
  theme_bw() + 
  scale_y_continuous(limits = c(0,1)) +
  labs(color = "Activity", title = "Proportions People Who Chose Certain Options Between Music and Vacuum"); oo01mv



