if (Time$g > 0){if (Time$d > 0) {if (Time$g01 == 2) {if(Time$d01 == 1) {if (Time$gd0 == 1){if (Time$gd.10 == 1) {print("consistent")} else {print("inconsistent")}}}}} else {print("NA")}}

cons <- data.frame(con = rep(NA, 234))
for(i in 1:nrow(Time)){
  if(Time$g[i] > 0 & Time$d[i] < 0 & Time$g01[i] == 2 & Time$d01[i] == 1 & Time$gd0[i] == 1 & Time$gd.10[i] == 1) {cons$con[i] <- "consistent"}
  if(Time$g[i] > 0 & Time$d[i] < 0 & Time$g01[i] == 2 & Time$d01[i] == 1 & Time$gd0[i] == 1 & Time$gd.10[i] == 2) {cons$con[i] <- "inconsistent"}
}


length(cons[which(cons$con == "consistent"),])
length(cons[which(cons$con == "inconsistent"),])
