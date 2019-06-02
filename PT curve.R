par(mfrow = c(1,1))

x <- data.frame(x = seq(-5,5,length=200))
yp <- NA
for (i in 1:200) {ifelse (x$x[i] > 0, x$yp[i] <- 0.4*pexp(x$x[i]), x$yp[i] <- .9999*exp(x$x[i]) - 1)}
plot(x,yp,type="l", ylab = NA, yaxt="n", axes=F, bty="n", xlim = c(-6.1,6.1), ylim = c(-1, 1)) # xlim = c(-5.1,5.1)
#ticks<-c(-1,0,1)
t <- c(-5,-4,-3,-2,-1,1,2,3,4,5)
#axis(1, pos=0, at=t,labels=t,tck = -.02)
axis(1, pos=0, labels=F,tck = 0)
axis(2, pos=0, labels = F,tck = 0)
#axis(2, pos=0, at=ticks,labels=ticks, las = 2,tck = -.02)
mtext("Gain", side=4, line=-0.5, las = 2)
mtext("Loss", side=2, line=-0.5, las = 2)
mtext("Negative Utility", side=1, line=-0.1)
mtext("Positive Utility", side=3, line=-0.1)


par(mfrow = c(1,1))

x <- data.frame(x = seq(-50,50,length=500))
yp <- NA
for (i in 1:500) {
  if (x$x[i] > -20) {if (x$x[i] > 10) {x$yp[i] <- (2*pexp(x$x[i]/20)-0.79)} else {x$yp[i] <- 0}}
    if (x$x[i] < -20) {x$yp[i] <- (6*exp(x$x[i]/20)-2.19)}}

plot(x,yp,type="l", ylab = NA, yaxt="n", axes=F, bty="n", xlim = c(-50.1,50.1), ylim = c(-2, 2), lwd=2)
#ticks<-c(-1,0,1)
t <- c(-50,-40,-30,-20,-10,0,10,20,30,40,50)
axis(1, pos=0, at=t,labels=t,tck = -.02)
axis(2, pos=0, labels = F,tck = 0)
#axis(2, pos=0, at=ticks,labels=ticks, las = 2,tck = -.02)
mtext("Spending Time on a Liked Activity", side=4, line=-0.2, las = 3)
mtext("Spending Time on a Disliked Activity", side=2, line=-0.2, las = 3)
mtext("Negative Utility", side=1, line=-0.1)
mtext("Positive Utility", side=3, line=-0.1)




# perceive liked activity as loss
x <- data.frame(x = seq(-50,50,length=500))
yp <- NA
for (i in 1:500) {
  if (x$x[i] > -20) {if (x$x[i] > 0) {x$yp[i] <- (2*pexp(x$x[i]/20)-0.79)} else {x$yp[i] <- 0}}
  if (x$x[i] < -20) {x$yp[i] <- (6*exp(x$x[i]/20)-2.19)}}

plot(x,yp,type="l", ylab = NA, yaxt="n", axes=F, bty="n", xlim = c(-50.1,50.1), ylim = c(-2, 2), lwd=2)
#ticks<-c(-1,0,1)
t <- c(-50,-40,-30,-20,-10,0,10,20,30,40,50)
axis(1, pos=0, at=t,labels=t,tck = -.02)
axis(2, pos=0, labels = F,tck = 0)
#axis(2, pos=0, at=ticks,labels=ticks, las = 2,tck = -.02)
mtext("Spending Time on a Liked Activity", side=4, line=-0.2, las = 3)
mtext("Spending Time on a Disliked Activity", side=2, line=-0.2, las = 3)
mtext("Negative Utility", side=1, line=-0.1)
mtext("Positive Utility", side=3, line=-0.1)



#negative activity
dt <- data.frame(x = c(20),
                 y = c(45))

an <- ggplot(dt, aes(x,y)) + 
  theme_bw() + 
  scale_x_continuous(limits = c(0,80), breaks = c(0,10,20,30,40,50,60,70,80), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0,90), breaks = c(10,20,30,40,50,60,70,80), expand = c(0, 0)) +
  labs(x = "Time", y = "Utility", title = "Disliked Activity") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_abline(intercept = 45, slope = 0) +
  geom_rect(xmin = 0, xmax = 20, ymin = 45, ymax = 90, fill = "gray", alpha = 0.5) +
  geom_rect(xmin = 20, xmax = 80, ymin = 0, ymax = 45, fill = "gray", alpha = 0.5)

#positive activity
dt1 <- data.frame(x = c(10),
                 y = c(45))

ap <- ggplot(dt1, aes(x,y)) + 
  theme_bw() + 
  scale_x_continuous(limits = c(0,80), breaks = c(0,10,20,30,40,50,60,70,80), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0,90), breaks = c(10,20,30,40,50,60,70,80), expand = c(0, 0)) +
  labs(x = "Time", y = "Utility", title = "Liked Activity") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_abline(intercept = 45, slope = 0) +
  geom_rect(xmin = 0, xmax = 10, ymin = 0, ymax = 45, fill = "gray", alpha = 0.5) +
  geom_rect(xmin = 10, xmax = 80, ymin = 45, ymax = 90, fill = "gray", alpha = 0.5)

grid.arrange(an, ap)


#mixed gamble
dt2 <- data.frame(x = c(-20),
                  y = c(45))

ggplot(dt1, aes(x,y)) + 
  theme_bw() + 
  scale_x_continuous(limits = c(-80,80),
                     breaks = c(-80,-70,-60,-50,-40,-30,-20,-10,0,10,20,30,40,50,60,70,80),
                     expand = c(0, 0), position = "top") +
  scale_y_continuous(limits = c(-90,90),
                     breaks = c(-90,-80,-70,-60,-50,-40,-30,-20,-10,0,10,20,30,40,50,60,70,80,90), expand = c(0, 0)) +
  labs(title = "Mixed Gamble \n Disliked and Liked Activities", x = "Utility", y = "Time") +
  theme(plot.title = element_text(hjust = 0.5), panel.border = element_blank(), panel.grid.minor = element_blank()) +
  geom_vline(xintercept = 0, alpha = 0.5) +
  geom_hline(yintercept = 0, alpha = 0.5) +
  theme(axis.text = element_blank(), axis.ticks = element_blank()) +
  geom_rect(xmin = 0, xmax = 80, ymin = 0, ymax = 90, fill = "gray", alpha = 0.5) +
  geom_rect(xmin = -20, xmax = 0, ymin = 0, ymax = 90, fill = "gray", alpha = 0.5) +
  geom_rect(xmin = -80, xmax = -20, ymin = -90, ymax = 0, fill = "gray", alpha = 0.5) +
  geom_segment(aes(x = -60, xend = -60.1, y = 0, yend = -2)) + 
  geom_segment(aes(x = -40, xend = -40.1, y = 0, yend = -2)) + 
  geom_segment(aes(x = -20, xend = -20.1, y = 0, yend = -2)) + 
  geom_segment(aes(x = 20, xend = 20.1, y = 0, yend = -2)) + 
  geom_segment(aes(x = 40, xend = 40.1, y = 0, yend = -2)) + 
  geom_segment(aes(x = 60, xend = 60.1, y = 0, yend = -2)) +
  annotate("text", x = c(-60, -40, -20, 0, 20, 40, 60), y = -5, 
           label = c("-60", "-40", "-20", "0", "20", "40", "60")) +
  geom_segment(aes(x = 0, xend = -1, y = 80, yend = 80.1)) + 
  geom_segment(aes(x = 0, xend = -1, y = 60, yend = 60.1)) + 
  geom_segment(aes(x = 0, xend = -1, y = 40, yend = 40.1)) + 
  geom_segment(aes(x = 0, xend = -1, y = 20, yend = 20.1)) + 
  geom_segment(aes(x = 0, xend = -1, y = -20, yend = -20.1)) + 
  geom_segment(aes(x = 0, xend = -1, y = -40, yend = -40.1)) + 
  geom_segment(aes(x = 0, xend = -1, y = -60, yend = -60.1)) +
  geom_segment(aes(x = 0, xend = -1, y = -80, yend = -80.1)) +
  annotate("text", y = c(-80, -60, -40, -20, 20, 40, 60, 80), x = -5, 
           label = c("-80", "-60", "-40", "-20", "20", "40", "60", "80")) 
  
  
  
  
  
  
  
  
