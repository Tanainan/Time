library("Hmisc")

library(xtable)

data <- Time[, c("g", "gp", "g45",
                 "s", "sp", "s45",
                 "m", "mp", "m45",
                 "d", "dn", "d45",
                 "t", "tn", "t45",
                 "v", "vn", "v45",
                 "gd0", "st0", "mv0")]

#when monotonic for risk-aversion
data <- Time[, c("g", "gp", "gr0",
                 "s", "sp", "sr0",
                 "m", "mp", "mr0",
                 "d", "dn", "dr0",
                 "t", "tn", "tr0",
                 "v", "vn", "vr0",
                 "gd0", "st0", "mv0")]

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

# reorder
data <- data[c("g", "gw", "g45",
                 "s", "sw", "s45",
                 "m", "mw", "m45",
                 "d", "dw", "d45",
                 "t", "tw", "t45",
                 "v", "vw", "v45",
                 "gd0", "st0", "mv0")]

res <- rcorr(as.matrix(data[,1:9]), type = "pearson")
res

res2 <- rcorr(as.matrix(data[,10:18]), type = "pearson")
res2

res3 <- rcorr(as.matrix(data[,19:21]), type = "pearson")
res3

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

