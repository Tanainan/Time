library(lme4)
library(afex)
Time$id <- c(1:nrow(Time)) # adding subject id

# create a new data frame without monotonicity
ch1 <- data.frame(choice = Time[(Time$g) > 0, c("g45")], id = Time[(Time$g) > 0, c("id")])
ch2 <- data.frame(choice = Time[(Time$m) > 0, c("m45")], id = Time[(Time$m) > 0, c("id")])
ch3 <- data.frame(choice = Time[(Time$s) > 0, c("s45")], id = Time[(Time$s) > 0, c("id")])
ch4 <- data.frame(choice = Time[(Time$t) < 0, c("t45")], id = Time[(Time$t) < 0, c("id")])
ch5 <- data.frame(choice = Time[(Time$v) < 0, c("v45")], id = Time[(Time$v) < 0, c("id")])
ch6 <- data.frame(choice = Time[(Time$d) < 0, c("d45")], id = Time[(Time$d) < 0, c("id")])

new <- rbind(ch1, ch2, ch3, ch4, ch5, ch6)
# effect coding for activity
new$activity <- as.integer(c(rep(1, nrow(ch1)), rep(2, nrow(ch2)),
                       rep(3, nrow(ch3)), rep(4, nrow(ch4)),
                       rep(5, nrow(ch5)), rep(6, nrow(ch6))))
new$domain <- as.integer(c(rep(0, nrow(ch1) + nrow(ch2) + nrow(ch3)),
                       rep(1, nrow(ch4) + nrow(ch5) + nrow(ch6))))

# # null model # choose 1 = certain, choose 0 = gamble
# null <- glmer(choice ~ 1 + (1| id),
#             data = new, family = binomial("logit"))
# summary(null)
# 
# 
# # model with domain
# dom <- glmer(choice ~ 1 + domain + (1| id),
#             data = new, family = binomial("logit"),
#             control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
# summary(dom)
# 
# # model with activity
# act <- glmer(choice ~ 1 + activity + (1| id),
#              data = new, family = binomial("logit"))
# summary(act)
# 
# both <- glmer(choice ~ 1 + activity + domain + (1| id),
#              data = new, family = binomial("logit"),
#              control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
# 
# summary(both)
# 
# anova(null, dom, act, both)



##### monotonic
# create a new data frame without monotonicity
ch10 <- data.frame(choice = Time[(Time$g) > 0 & Time$g01 == 2 & Time$g89 == 2, c("g45")],
                   id = Time[(Time$g) > 0 & Time$g01 == 2 & Time$g89 == 2, c("id")])
ch20 <- data.frame(choice = Time[(Time$m) > 0 & Time$m01 == 2 & Time$m89 == 2, c("m45")],
                   id = Time[(Time$m) > 0 & Time$m01 == 2 & Time$m89 == 2, c("id")])
ch30 <- data.frame(choice = Time[(Time$s) > 0 & Time$s01 == 2 & Time$s89 == 2, c("s45")], 
                   id = Time[(Time$s) > 0 & Time$s01 == 2 & Time$s89 == 2, c("id")])
ch40 <- data.frame(choice = Time[(Time$t) < 0 & Time$t01 == 1 & Time$t89 == 1, c("t45")], 
                   id = Time[(Time$t) < 0 & Time$t01 == 1 & Time$t89 == 1, c("id")])
ch50 <- data.frame(choice = Time[(Time$v) < 0 & Time$v01 == 1 & Time$v89 == 1, c("v45")],
                   id = Time[(Time$v) < 0 & Time$v01 == 1 & Time$v89 == 1, c("id")])
ch60 <- data.frame(choice = Time[(Time$d) < 0 & Time$d01 == 1 & Time$d89 == 1, c("d45")],
                   id = Time[(Time$d) < 0 & Time$d01 == 1 & Time$d89 == 1, c("id")])

new0 <- rbind(ch10, ch20, ch30, ch40, ch50, ch60)
# effect coding for activity
new0$activity <- as.integer(c(rep(1, nrow(ch10)), rep(2, nrow(ch20)),
                             rep(3, nrow(ch30)), rep(4, nrow(ch40)),
                             rep(5, nrow(ch50)), rep(6, nrow(ch60))))
new0$domain <- as.integer(c(rep(0, nrow(ch10) + nrow(ch20) + nrow(ch30)),
                           rep(1, nrow(ch40) + nrow(ch50) + nrow(ch60))))

# null model # choose 1 = certain, choose 0 = gamble
null0 <- glmer(choice ~ 1 + (1| id),
              data = new0, family = binomial("logit"))
summary(null0)

# model with domain
dom0 <- glmer(choice ~ 1 + domain + (1| id),
              data = new0, family = binomial("logit"),
              control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(dom0)

dom1 <- mixed(choice ~ 1 + domain + (1| id),
      data = new0, family = binomial(link="logit"), method="LRT")
summary(dom1) ##### same result as well

# model with activity
act0 <- glmer(choice ~ 1 + activity + (1| id),
             data = new0, family = binomial("logit"))
summary(act0)

both0 <- glmer(choice ~ 1 + activity + domain +(1| id),
              data = new0, family = binomial("logit"),
              control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(both0)



anova(null0, dom0, act0, both0)


rand <- glmer(choice ~ 1 + domain + activity + (1 + domain| id),
              data = new0, family = binomial("logit"),
              control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

summary(rand)


