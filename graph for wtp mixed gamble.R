library(ggpubr)
# consistency for evaluation and mixed gamble
# separate when wtp diff is positive vs negative
gdwp <- gd9[(which(gd9$gdw > 0)),]
gdwn <- gd9[(which(gd9$gdw < 0)),]
gdwz <- gd9[(which(gd9$gdw == 0)),]

nrow(gdwp)
nrow(gdwz)
nrow(gdwn)

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
posigd$type <- factor(c(rep(c("Disliked < Liked"), times = 9),rep(c("Disliked = Liked"), times = 9),rep(c("Disliked > Liked"), times = 9))) 

oo01gd <- ggplot(data = posigd, aes(x = Time, y = Proportion, group = type)) + 
  geom_point() + 
  scale_x_discrete(limits=c("-40", "-30", "-20", "-10", "0", "10", "20", "30", "40")) +
  scale_linetype_discrete(limits = c("Disliked < Liked","Disliked = Liked","Disliked > Liked")) +
  geom_line(aes(linetype = type)) +
  theme(plot.title = element_text(hjust=0.5)) + 
  theme_bw() + 
  scale_y_continuous(limits = c(0,1)) +
  labs(linetype = "WTP Difference", title = "Dishes & Games", y = "", x = "")

# st *****************************************************************
# consistency for evaluation and mixed gamble
# separate when WTP diff is positive vs negative
stwp <- st9[(which(st9$stw > 0)),]
stwn <- st9[(which(st9$stw < 0)),]
stwz <- st9[(which(st9$stw == 0)),]

nrow(stwp)
nrow(stwz)
nrow(stwn)

# when s > t
s11 <- nrow(stwp[stwp$st.40 == 1,])/nrow(stwp); s11
s22 <- nrow(stwp[stwp$st.30 == 1,])/nrow(stwp); s22
s33 <- nrow(stwp[stwp$st.20 == 1,])/nrow(stwp); s33
s44 <- nrow(stwp[stwp$st.10 == 1,])/nrow(stwp); s44
s55 <- nrow(stwp[stwp$st0 == 1,])/nrow(stwp); s55
s66 <- nrow(stwp[stwp$st10 == 1,])/nrow(stwp); s66
s77 <- nrow(stwp[stwp$st20 == 1,])/nrow(stwp); s77
s88 <- nrow(stwp[stwp$st30 == 1,])/nrow(stwp); s88
s99 <- nrow(stwp[stwp$st40 == 1,])/nrow(stwp); s99



s.10s <- data.frame(Time = c("-40", "-30", "-20", "-10", "0", "10", "20", "30", "40"), Proportion = c(s11,s22,s33,s44,s55,s66,s77,s88,s99)) 
s.10sp <- ggplot(s.10s, aes(Time, Proportion, group = 1)) + 
  geom_point() + 
  scale_x_discrete(limits=c("-40", "-30", "-20", "-10", "0", "10", "20", "30", "40")) +
  geom_line() +
  ggtitle("Proportions People That Chose Certain Options When Sports > Traffic") +
  theme(plot.title = element_text(hjust=0.5))

# when s = t
s11z <- nrow(stwz[stwz$st.40 == 1,])/nrow(stwz); s11z
s22z <- nrow(stwz[stwz$st.30 == 1,])/nrow(stwz); s22z
s33z <- nrow(stwz[stwz$st.20 == 1,])/nrow(stwz); s33z
s44z <- nrow(stwz[stwz$st.10 == 1,])/nrow(stwz); s44z
s55z <- nrow(stwz[stwz$st0 == 1,])/nrow(stwz); s55z
s66z <- nrow(stwz[stwz$st10 == 1,])/nrow(stwz); s66z
s77z <- nrow(stwz[stwz$st20 == 1,])/nrow(stwz); s77z
s88z <- nrow(stwz[stwz$st30 == 1,])/nrow(stwz); s88z
s99z <- nrow(stwz[stwz$st40 == 1,])/nrow(stwz); s99z



s.10z <- data.frame(Time = c("-40", "-30", "-20", "-10", "0", "10", "20", "30", "40"), Proportion = c(s11z,s22z,s33z,s44z,s55z,s66z,s77z,s88z,s99z)) 
s.10sz <- ggplot(s.10z, aes(Time, Proportion, group = 1)) + 
  geom_point() + 
  scale_x_discrete(limits=c("-40", "-30", "-20", "-10", "0", "10", "20", "30", "40")) +
  geom_line() +
  ggtitle("Proportions People That Chose Certain Options When Sports = Traffic") +
  theme(plot.title = element_text(hjust=0.5))

# when s < t
s11n <- nrow(stwn[stwn$st.40 == 1,])/nrow(stwn); s11n
s22n <- nrow(stwn[stwn$st.30 == 1,])/nrow(stwn); s22n
s33n <- nrow(stwn[stwn$st.20 == 1,])/nrow(stwn); s33n
s44n <- nrow(stwn[stwn$st.10 == 1,])/nrow(stwn); s44n
s55n <- nrow(stwn[stwn$st0 == 1,])/nrow(stwn); s55n
s66n <- nrow(stwn[stwn$st10 == 1,])/nrow(stwn); s66n
s77n <- nrow(stwn[stwn$st20 == 1,])/nrow(stwn); s77n
s88n <- nrow(stwn[stwn$st30 == 1,])/nrow(stwn); s88n
s99n <- nrow(stwn[stwn$st40 == 1,])/nrow(stwn); s99n



s.10n <- data.frame(Time = c("-40", "-30", "-20", "-10", "0", "10", "20", "30", "40"), Proportion = c(s11n,s22n,s33n,s44n,s55n,s66n,s77n,s88n,s99n)) 
s.10sn <- ggplot(s.10n, aes(Time, Proportion, group = 1)) + 
  geom_point() + 
  scale_x_discrete(limits=c("-40", "-30", "-20", "-10", "0", "10", "20", "30", "40")) +
  geom_line() +
  ggtitle("Proportions People That Chose Certain Options When Sports < Traffic") +
  theme(plot.title = element_text(hjust=0.5))



#combined st
posist <- rbind(s.10s, s.10z, s.10n)
posist$type <- factor(c(rep(c("Disliked < Liked"), times = 9),rep(c("Disliked = Liked"), times = 9),rep(c("Disliked > Liked"), times = 9))) 

oo01st <- ggplot(data = posist, aes(x = Time, y = Proportion, group = type)) + 
  geom_point() + 
  scale_x_discrete(limits=c("-40", "-30", "-20", "-10", "0", "10", "20", "30", "40")) +
  scale_linetype_discrete(limits = c("Disliked < Liked","Disliked = Liked","Disliked > Liked")) +
  geom_line(aes(linetype = type)) +
  theme(plot.title = element_text(hjust=0.5)) + 
  theme_bw() + 
  scale_y_continuous(limits = c(0,1)) +
  labs(linetype = "WTP Difference", title = "Traffic Jam & Sports", y = "", x = "")





# mv *****************************************************************
# consistency for evaluation and mixed gamble
# separate when WTP diff is positive vs negative
mvwp <- mv9[(which(mv9$mvw > 0)),]
mvwn <- mv9[(which(mv9$mvw < 0)),]
mvwz <- mv9[(which(mv9$mvw == 0)),]

nrow(mvwp)
nrow(mvwz)
nrow(mvwn)

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
posimv$type <- factor(c(rep(c("Disliked < Liked"), times = 9),rep(c("Disliked = Liked"), times = 9),rep(c("Disliked > Liked"), times = 9))) 


oo01mv <- ggplot(data = posimv, aes(x = Time, y = Proportion, group = type)) + 
  geom_point() + 
  scale_x_discrete(limits=c("-40", "-30", "-20", "-10", "0", "10", "20", "30", "40")) +
  scale_linetype_discrete(limits = c("Disliked < Liked","Disliked = Liked","Disliked > Liked")) +
  geom_line(aes(linetype = type)) +
  theme(plot.title = element_text(hjust=0.5)) + 
  theme_bw() + 
  scale_y_continuous(limits = c(0,1)) +
  labs(linetype = "WTP Difference", title = "Vacuum & Music", y = "", x = "")




# WTP diff
figwtp <- ggarrange(oo01gd,oo01st,oo01mv, nrow = 3, common.legend = T, legend = "right")
annotate_figure(figwtp, top = text_grob("Risk-Aversion Proportions in Mixed Gamble \n Based on WTP Difference"),
                left = text_grob("Proportion", rot = 90),
                bottom = text_grob("Time", vjust = -1, hjust = 2))
