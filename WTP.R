library(lme4)
library(lmerTest)
Time$id <- c(1:nrow(Time)) # adding subject id

# create a new data frame without monotonicity
cw1 <- data.frame(wtp = Time[(Time$g) > 0, c("gp")], id = Time[(Time$g) > 0, c("id")], eva = Time[(Time$g) > 0, c("g")])
cw2 <- data.frame(wtp = Time[(Time$m) > 0, c("mp")], id = Time[(Time$m) > 0, c("id")], eva = Time[(Time$m) > 0, c("m")])
cw3 <- data.frame(wtp = Time[(Time$s) > 0, c("sp")], id = Time[(Time$s) > 0, c("id")], eva = Time[(Time$s) > 0, c("s")])
cw4 <- data.frame(wtp = Time[(Time$t) < 0, c("tn")], id = Time[(Time$t) < 0, c("id")], eva = Time[(Time$t) < 0, c("t")])
cw5 <- data.frame(wtp = Time[(Time$v) < 0, c("vn")], id = Time[(Time$v) < 0, c("id")], eva = Time[(Time$v) < 0, c("v")])
cw6 <- data.frame(wtp = Time[(Time$d) < 0, c("dn")], id = Time[(Time$d) < 0, c("id")], eva = Time[(Time$d) < 0, c("d")])

neww <- rbind(cw1, cw2, cw3, cw4, cw5, cw6)
# effect coding for activity
neww$activity <- as.integer(c(rep(1, nrow(cw1)), rep(2, nrow(cw2)),
                             rep(3, nrow(cw3)), rep(4, nrow(cw4)),
                             rep(5, nrow(cw5)), rep(6, nrow(cw6))))

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
actw <- lmer(wtp ~ 1 + activity + (1| id),
            data = neww)
summary(actw)

# model with evaluation
evaw <- lmer(wtp ~ 1 + eva + (1| id),
             data = neww)
summary(evaw)

# model with evaluation and domain
evadw <- lmer(wtp ~ 1 + eva + domain + (1| id),
             data = neww)
summary(evadw)


anova(nullw, domw, actw, evaw)


par(mfrow = c(1,1))
plot(neww$eva, neww$wtp, xlab = "Evaluation Score", ylab = " Willingness-to-Pay", main = "Relationship between Evaluation Score and WTP")
plot(neww$activity, neww$wtp, xlab = "Evaluation Score", ylab = " Willingness-to-Pay", main = "Relationship between Evaluation Score and WTP")
plot(neww$domain, neww$wtp, xlab = "Evaluation Score", ylab = " Willingness-to-Pay", main = "Relationship between Evaluation Score and WTP")


# eva as dv and domain as iv

# cc <- lmer(eva ~ 1 + domain + (1|id),
#            data = neww)
# summary(cc)
