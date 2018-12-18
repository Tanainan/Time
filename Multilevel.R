library(nlme)
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
new$activity <- factor(c(rep(1, nrow(ch1)), rep(2, nrow(ch2)),
                       rep(3, nrow(ch3)), rep(4, nrow(ch4)),
                       rep(5, nrow(ch5)), rep(6, nrow(ch6))))
new$domain <- factor(c(rep(0, nrow(ch1) + nrow(ch2) + nrow(ch3)),
                       rep(1, nrow(ch4) + nrow(ch5) + nrow(ch6))))

# null model # choose 1 = certain, choose 0 = gamble
summary(glm(choice ~ 1| id,
            data = new, family = "binomial"))

# model with activity
summary(glm(choice ~ 1 + activity| id,
            data = new, family = "binomial"))
# model with domain
summary(glm(choice ~ 1 + domain| id,
            data = new, family = "binomial"))
