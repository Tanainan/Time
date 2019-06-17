library("Hmisc")
library(tidyverse)
library(xtable)
library(ltm)
library(sjstats)

#game wtp and eva
ggp <- data.frame(t = Time$gn)
ggo <- data.frame(t = Time$gp)

ggo$t[is.na(ggo$t)] <- ggp$t[is.na(ggo$t)]

# sports wtp and eva
ssp <- data.frame(t = Time$sn)
sso <- data.frame(t = Time$sp)
ssm <- data.frame(t = Time$sneun)
ssn <- data.frame(t = Time$sneup)

sso$t[is.na(sso$t)] <- ssp$t[is.na(sso$t)]
sso$t[is.na(sso$t)] <- ssm$t[is.na(sso$t)]
sso$t[is.na(sso$t)] <- ssn$t[is.na(sso$t)]


#music wtp and eva
mmp <- data.frame(t = Time$mn)
mmo <- data.frame(t = Time$mp)

mmo$t[is.na(mmo$t)] <- mmp$t[is.na(mmo$t)]


#traffic wtp and eva
ttp <- data.frame(t = Time$tn)
tto <- data.frame(t = Time$tp)
ttm <- data.frame(t = Time$tneun)
ttn <- data.frame(t = Time$tneup)

tto$t[is.na(tto$t)] <- ttp$t[is.na(tto$t)]
tto$t[is.na(tto$t)] <- ttm$t[is.na(tto$t)]
tto$t[is.na(tto$t)] <- ttn$t[is.na(tto$t)]

#dishes wtp and eva
ddp <- data.frame(t = Time$dn)
ddo <- data.frame(t = Time$dp)
ddm <- data.frame(t = Time$dneun)
ddn <- data.frame(t = Time$dneup)

ddo$t[is.na(ddo$t)] <- ddp$t[is.na(ddo$t)]
ddo$t[is.na(ddo$t)] <- ddm$t[is.na(ddo$t)]
ddo$t[is.na(ddo$t)] <- ddn$t[is.na(ddo$t)]

# vacuum wtp and eva
vvp <- data.frame(t = Time$vn)
vvo <- data.frame(t = Time$vp)
vvm <- data.frame(t = Time$vneun)
vvn <- data.frame(t = Time$vneup)

vvo$t[is.na(vvo$t)] <- vvp$t[is.na(vvo$t)]
vvo$t[is.na(vvo$t)] <- vvm$t[is.na(vvo$t)]
vvo$t[is.na(vvo$t)] <- vvn$t[is.na(vvo$t)]


# data <- Time[, c("g", "gp", "g45",
#                  "s", "sp", "s45",
#                  "m", "mp", "m45",
#                  "d", "dn", "d45",
#                  "t", "tn", "t45",
#                  "v", "vn", "v45",
#                  "gd0", "st0", "mv0")]

#when monotonic for risk-aversion
# data <- Time[, c("g", "gp", "gr0",
#                  "s", "sp", "sr0",
#                  "m", "mp", "mr0",
#                  "d", "dn", "dr0",
#                  "t", "tn", "tr0",
#                  "v", "vn", "vr0",
#                  "gd0", "st0", "mv0")]

#when wtp is for all
data <- Time[, c("g", "g45",
                 "s", "s45",
                 "m", "m45",
                 "d", "d45",
                 "t", "t45",
                 "v", "v45",
                 "gd0", "st0", "mv0")]

data$gw <- ggo$t
data$sw <- sso$t
data$mw <- mmo$t 
data$dw <- ddo$t
data$tw <- tto$t
data$vw <- vvo$t

data$g <- data$g %>% abs()
data$s <- data$s %>% abs()
data$m <- data$m %>% abs()
data$v <- data$v %>% abs()
data$t <- data$t %>% abs()
data$d <- data$d %>% abs()

# reorder
data <- data[c("g", "gw", "g45",
                 "s", "sw", "s45",
                 "m", "mw", "m45",
                 "d", "dw", "d45",
                 "t", "tw", "t45",
                 "v", "vw", "v45",
                 "gd0", "st0", "mv0")]


#biserial correlation
# ref group is 2 because risk aversion - risk seeking
biserial.cor(data$g, data$g45, use = "complete.obs", level = 2)
biserial.cor(data$gw, data$g45, use = "complete.obs", level = 2)
biserial.cor(data$s, data$s45, use = "complete.obs", level = 2)
biserial.cor(data$sw, data$s45, use = "complete.obs", level = 2)
biserial.cor(data$m, data$m45, use = "complete.obs", level = 2)
biserial.cor(data$mw, data$m45, use = "complete.obs", level = 2)
biserial.cor(data$t, data$t45, use = "complete.obs", level = 2)
biserial.cor(data$tw, data$t45, use = "complete.obs", level = 2)
biserial.cor(data$d, data$d45, use = "complete.obs", level = 2)
biserial.cor(data$dw, data$d45, use = "complete.obs", level = 2)
biserial.cor(data$v, data$v45, use = "complete.obs", level = 2)
biserial.cor(data$vw, data$v45, use = "complete.obs", level = 2)

# rcorr(as.matrix(data[,c(2,3)]), type = "pearson")
# res <- rcorr(as.matrix(data[,1:9]), type = "pearson")
# res
# 
# res2 <- rcorr(as.matrix(data[,10:18]), type = "pearson")
# res2
# 
# res3 <- rcorr(as.matrix(data[,19:21]), type = "pearson")
# res3

# risk aversion positive activities
gs45c <- aov(as.matrix(data$g45) ~ as.matrix(data$s45)) %>% anova_stats()
sqrt(gs45c[1,8])
gm45c <- aov(as.matrix(data$g45) ~ as.matrix(data$m45)) %>% anova_stats()
sqrt(gm45c[1,8])
sm45c <- aov(as.matrix(data$s45) ~ as.matrix(data$m45)) %>% anova_stats()
sqrt(sm45c[1,8])

# risk aversion negative activities
dt45c <- aov(as.matrix(data$d45) ~ as.matrix(data$t45)) %>% anova_stats()
sqrt(dt45c[1,8])
dv45c <- aov(as.matrix(data$d45) ~ as.matrix(data$v45)) %>% anova_stats()
sqrt(dv45c[1,8])
tv45c <- aov(as.matrix(data$t45) ~ as.matrix(data$v45)) %>% anova_stats()
sqrt(tv45c[1,8])


# risk aversion mixed gamble activities
gdst <- aov(as.matrix(data$gd0) ~ as.matrix(data$st0)) %>% anova_stats()
sqrt(gdst[1,8])
gdmv <- aov(as.matrix(data$gd0) ~ as.matrix(data$mv0)) %>% anova_stats()
sqrt(gdmv[1,8])
stmv <- aov(as.matrix(data$st0) ~ as.matrix(data$mv0)) %>% anova_stats()
sqrt(stmv[1,8])


# flattenCorrMatrix <- function(cormat, pmat) {
#   ut <- upper.tri(cormat)
#   data.frame(
#     row = rownames(cormat)[row(cormat)[ut]],
#     column = rownames(cormat)[col(cormat)[ut]],
#     cor  =(cormat)[ut],
#     p = pmat[ut]
#   )
# }
# flattenCorrMatrix(res$r, res$P)

# x is a matrix containing the data
# method : correlation method. "pearson"" or "spearman"" is supported
# removeTriangle : remove upper or lower triangle
# results :  if "html" or "latex"
# the results will be displayed in html or latex format
corstars <-function(x, method=c("pearson", "spearman"), removeTriangle=c("upper", "lower"),
                    result=c("none", "html", "latex")){
  #Compute correlation matrix
  require(Hmisc)
  x <- as.matrix(x)
  correlation_matrix<-rcorr(x, type=method[1])
  R <- correlation_matrix$r # Matrix of correlation coeficients
  p <- correlation_matrix$P # Matrix of p-value 
  
  ## Define notions for significance levels; spacing is important.
  mystars <- ifelse(p < .0001, "****", ifelse(p < .001, "*** ", ifelse(p < .01, "**  ", ifelse(p < .05, "*   ", "    "))))
  
  ## trunctuate the correlation matrix to two decimal
  R <- format(round(cbind(rep(-1.11, ncol(x)), R), 2))[,-1]
  
  ## build a new matrix that includes the correlations with their apropriate stars
  Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x))
  diag(Rnew) <- paste(diag(R), " ", sep="")
  rownames(Rnew) <- colnames(x)
  colnames(Rnew) <- paste(colnames(x), "", sep="")
  
  ## remove upper triangle of correlation matrix
  if(removeTriangle[1]=="upper"){
    Rnew <- as.matrix(Rnew)
    Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
    Rnew <- as.data.frame(Rnew)
  }
  
  ## remove lower triangle of correlation matrix
  else if(removeTriangle[1]=="lower"){
    Rnew <- as.matrix(Rnew)
    Rnew[lower.tri(Rnew, diag = TRUE)] <- ""
    Rnew <- as.data.frame(Rnew)
  }
  
  ## remove last column and return the correlation matrix
  Rnew <- cbind(Rnew[1:length(Rnew)-1])
  if (result[1]=="none") return(Rnew)
  else{
    if(result[1]=="html") print(xtable(Rnew), type="html")
    else print(xtable(Rnew), type="latex") 
  }
} 

corstars(as.matrix(data[,1:9]))
corstars(as.matrix(data[,10:18]))
corstars(as.matrix(data[,19:21])) # for mixed gamble


# res1 <- round(cor(as.numeric(data)),2)
# upper.tri(data, diag = FALSE)
# upper<-res1
# upper[upper.tri(res1)]<-""
# upper<-as.data.frame(upper)
# upper

