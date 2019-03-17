library(lme4)
library(lmerTest)
library(MuMIn)
Time$id <- c(1:nrow(Time)) # adding subject id

# create a new data frame without monotonicity
cw1 <- data.frame(wtp = Time[(Time$g) > 0, c("gp")], id = Time[(Time$g) > 0, c("id")], eva = Time[(Time$g) > 0, c("g")])
cw2 <- data.frame(wtp = Time[(Time$m) > 0, c("mp")], id = Time[(Time$m) > 0, c("id")], eva = Time[(Time$m) > 0, c("m")])
cw3 <- data.frame(wtp = Time[(Time$s) > 0, c("sp")], id = Time[(Time$s) > 0, c("id")], eva = Time[(Time$s) > 0, c("s")])
cw4 <- data.frame(wtp = Time[(Time$t) < 0, c("tn")], id = Time[(Time$t) < 0, c("id")], eva = Time[(Time$t) < 0, c("t")])
cw5 <- data.frame(wtp = Time[(Time$v) < 0, c("vn")], id = Time[(Time$v) < 0, c("id")], eva = Time[(Time$v) < 0, c("v")])
cw6 <- data.frame(wtp = Time[(Time$d) < 0, c("dn")], id = Time[(Time$d) < 0, c("id")], eva = Time[(Time$d) < 0, c("d")])

neww <- rbind(cw1, cw2, cw3, cw4, cw5, cw6)
# dummy coding for activity 
# games is the ref group
neww$act1 <- as.integer(c(rep(0, nrow(cw1)), rep(1, nrow(cw2)),
                             rep(0, nrow(cw3)), rep(0, nrow(cw4)),
                             rep(0, nrow(cw5)), rep(0, nrow(cw6))))
neww$act2 <- as.integer(c(rep(0, nrow(cw1)), rep(0, nrow(cw2)),
                          rep(1, nrow(cw3)), rep(0, nrow(cw4)),
                          rep(0, nrow(cw5)), rep(0, nrow(cw6))))
neww$act3 <- as.integer(c(rep(0, nrow(cw1)), rep(0, nrow(cw2)),
                          rep(0, nrow(cw3)), rep(1, nrow(cw4)),
                          rep(0, nrow(cw5)), rep(0, nrow(cw6))))
neww$act4 <- as.integer(c(rep(0, nrow(cw1)), rep(0, nrow(cw2)),
                          rep(0, nrow(cw3)), rep(0, nrow(cw4)),
                          rep(1, nrow(cw5)), rep(0, nrow(cw6))))
neww$act5 <- as.integer(c(rep(0, nrow(cw1)), rep(0, nrow(cw2)),
                          rep(0, nrow(cw3)), rep(0, nrow(cw4)),
                          rep(0, nrow(cw5)), rep(1, nrow(cw6))))

# 0 = positive and 1 = negative
neww$domain <- as.integer(c(rep(0, nrow(cw1) + nrow(cw2) + nrow(cw3)),
                           rep(1, nrow(cw4) + nrow(cw5) + nrow(cw6))))

# null model # choose 1 = certain, choose 0 = gamble
nullw <- lmer(wtp ~ 1 + (1| id),
              data = neww)
summary(nullw)


# model with domain
domw <- lmer(wtp ~ 1 + domain + (1| id),
             data = neww)
summary(domw)

# model with activity
# actw <- lmer(wtp ~ 1 + act1 + act2 + act3 + act4 + act5 + (1| id),
#             data = neww)
# summary(actw)

# model with evaluation
# evaw <- lmer(wtp ~ 1 + eva + (1| id),
#              data = neww)
# summary(evaw)

# # model with evaluation and domain and activities
# evadw <- lmer(wtp ~ 1 + eva + domain + (1| id)
#              data = neww)
# summary(evadw)


anova(nullw, domw, evaw)
r.squaredGLMM(nullw)
r.squaredGLMM(domw)
0.00014671505/(1-0.00014671505)
r.squaredGLMM(evaw)
0.00043737731/(1-0.00043737731)

par(mfrow = c(1,1))
plot(neww$eva, neww$wtp, xlab = "Evaluation Score", ylab = " Willingness-to-Pay", main = "Relationship between Evaluation Score and WTP")
plot(neww$activity, neww$wtp, xlab = "Evaluation Score", ylab = " Willingness-to-Pay", main = "Relationship between Evaluation Score and WTP")
plot(neww$domain, neww$wtp, xlab = "Evaluation Score", ylab = " Willingness-to-Pay", main = "Relationship between Evaluation Score and WTP")


# eva as dv and domain as iv

# cc <- lmer(eva ~ 1 + domain + (1|id),
#            data = neww)
# summary(cc)
