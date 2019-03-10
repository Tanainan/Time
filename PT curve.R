par(mfrow = c(1,1))

x <- data.frame(x = seq(-5,5,length=200))
yp <- NA
for (i in 1:200) {ifelse (x$x[i] > 0, x$yp[i] <- 0.4*pexp(x$x[i]), x$yp[i] <- .9999*exp(x$x[i]) - 1)}
plot(x,yp,type="l", ylab = NA, yaxt="n", axes=F, bty="n", xlim = c(-5.1,5.1), ylim = c(-1, 1))
#ticks<-c(-1,0,1)
t <- c(-5,-4,-3,-2,-1,1,2,3,4,5)
axis(1, pos=0, at=t,labels=t,tck = -.02)
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
