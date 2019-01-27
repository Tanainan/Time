# consistency for evaluation and mixed gamble
# separate when WTP diff is positive vs negative
stwp <- st9[(which(st9$ste > 0)),]
stwn <- st9[(which(st9$ste < 0)),]
stwz <- st9[(which(st9$ste == 0)),]


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
  theme(plot.title = element_text(hjust=0.5)); s.10sp 

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
  theme(plot.title = element_text(hjust=0.5)); s.10sz 

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
  theme(plot.title = element_text(hjust=0.5)); s.10sn 



#combined st
posist <- rbind(s.10s, s.10z, s.10n)
posist$type <- factor(c(rep(c("more"), times = 9),rep(c("equal"), times = 9),rep(c("less"), times = 9))) 


oo01st <- ggplot(data = posist, aes(x = Time, y = Proportion, group = type)) + 
  geom_point() + 
  scale_x_discrete(limits=c("-40", "-30", "-20", "-10", "0", "10", "20", "30", "40")) +
  geom_line(aes(linetype = type)) +
  theme(plot.title = element_text(hjust=0.5)) + 
  theme_bw() + 
  scale_y_continuous(limits = c(0,1)) +
  labs(color = "Activity", title = "Proportions People Who Chose Certain Options Between Sports and Traffic"); oo01st



