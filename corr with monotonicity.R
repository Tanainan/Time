library("Hmisc")
library(xtable)


dt <- data.frame(g = rep(NA, 234),
                 gw = rep(NA, 234),
                 g45 = rep(NA, 234),
                 s = rep(NA, 234),
                 sw = rep(NA, 234),
                 s45 = rep(NA, 234),
                 m = rep(NA, 234),
                 mw = rep(NA, 234),
                 m45 = rep(NA, 234),
                 d = rep(NA, 234),
                 dw = rep(NA, 234),
                 d45 = rep(NA, 234),
                 t = rep(NA, 234),
                 tw = rep(NA, 234),
                 t45 = rep(NA, 234),
                 v = rep(NA, 234),
                 vw = rep(NA, 234),
                 v45 = rep(NA, 234),
                 gd = rep(NA, 234),
                 st = rep(NA, 234),
                 mv = rep(NA, 234))

for (i in 1:nrow(Time)){
  if(Time$g[i] > 0 & Time$g01[i] == 2 & Time$g89[i] == 2) {dt$g[i] <- Time$g[i]}
  if(Time$g[i] > 0 & Time$g01[i] == 2 & Time$g89[i] == 2) {dt$gw[i] <- Time$gp[i]}
  if(Time$g[i] > 0 & Time$g01[i] == 2 & Time$g89[i] == 2) {dt$g45[i] <- Time$g45[i]}
  if(Time$s[i] > 0 & Time$s01[i] == 2 & Time$s89[i] == 2) {dt$s[i] <- Time$s[i]}
  if(Time$s[i] > 0 & Time$s01[i] == 2 & Time$s89[i] == 2) {dt$sw[i] <- Time$sp[i]}
  if(Time$s[i] > 0 & Time$s01[i] == 2 & Time$s89[i] == 2) {dt$s45[i] <- Time$s45[i]}
  if(Time$m[i] > 0 & Time$m01[i] == 2 & Time$m89[i] == 2) {dt$m[i] <- Time$m[i]}
  if(Time$m[i] > 0 & Time$m01[i] == 2 & Time$m89[i] == 2) {dt$mw[i] <- Time$mp[i]}
  if(Time$m[i] > 0 & Time$m01[i] == 2 & Time$m89[i] == 2) {dt$m45[i] <- Time$m45[i]}
  if(Time$d[i] < 0 & Time$d01[i] == 1 & Time$d89[i] == 1) {dt$d[i] <- Time$d[i]}
  if(Time$d[i] < 0 & Time$d01[i] == 1 & Time$d89[i] == 1) {dt$dw[i] <- Time$dn[i]}
  if(Time$d[i] < 0 & Time$d01[i] == 1 & Time$d89[i] == 1) {dt$d45[i] <- Time$d45[i]}
  if(Time$t[i] < 0 & Time$t01[i] == 1 & Time$t89[i] == 1) {dt$t[i] <- Time$t[i]}
  if(Time$t[i] < 0 & Time$t01[i] == 1 & Time$t89[i] == 1) {dt$tw[i] <- Time$tn[i]}
  if(Time$t[i] < 0 & Time$t01[i] == 1 & Time$t89[i] == 1) {dt$t45[i] <- Time$t45[i]}
  if(Time$v[i] < 0 & Time$v01[i] == 1 & Time$v89[i] == 1) {dt$v[i] <- Time$v[i]}
  if(Time$v[i] < 0 & Time$v01[i] == 1 & Time$v89[i] == 1) {dt$vw[i] <- Time$vn[i]}
  if(Time$v[i] < 0 & Time$v01[i] == 1 & Time$v89[i] == 1) {dt$v45[i] <- Time$v45[i]}
  
  if(Time$g[i] > 0 & Time$d[i] < 0 & Time$g01[i] == 2 & Time$d01[i] == 1) {dt$gd[i] <- Time$gd0[i]}
  if(Time$s[i] > 0 & Time$t[i] < 0 & Time$s01[i] == 2 & Time$t01[i] == 1) {dt$st[i] <- Time$st0[i]}
  if(Time$m[i] > 0 & Time$v[i] < 0 & Time$m01[i] == 2 & Time$v01[i] == 1) {dt$mv[i] <- Time$mv0[i]}
}

res <- rcorr(as.matrix(dt[,1:9]), type = "pearson")
res

res2 <- rcorr(as.matrix(dt[,10:18]), type = "pearson")
res2

res3 <- rcorr(as.matrix(dt[,19:21]), type = "pearson")
res3


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

corstars(as.matrix(dt[,1:9]))
corstars(as.matrix(dt[,10:18]))
