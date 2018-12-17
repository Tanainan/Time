x <- data.frame(x = seq(-5,5,length=200))
yp <- NA
for (i in 1:200) {
  ifelse (x$x[i] > 0, x$yp[i] <- 0.6*pexp(x$x[i]), x$yp[i] <- .96*exp(x$x[i]) - 1)}
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


