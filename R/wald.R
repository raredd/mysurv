wald <- function(x,kk=1:length(coef(x))) {
  w <- coef(x)[kk]
  v <- x$var[kk,kk]
  stat <- sum(w*solve(v,w))
  df <- length(w)
  c(stat=stat,df=df,p=1-pchisq(stat,df))
}

ratio <- function(x,j=NULL,cntr=NULL,coef=.95,inv=FALSE) {
  if (!is.null(cntr)) {
    if (is.null(j)) j <- 1:length(coef(x))
    w <- sum(coef(x)[j]*cntr)
    v <- sum(cntr*c(x$var[j,j] %*% as.matrix(cntr)))
  } else {
    w <- coef(x)[j]
    v <- x$var[j,j]
  }
  zc <- -qnorm((1-coef)/2)
  if (inv) w=-w
  c(exp(w+c(0,-1,1)*zc*sqrt(v)),p=1-pchisq(w*w/v,1))
}
