quantile.km <- function(x, q = 0.5, ...){
  n <- length(x)
  u <- rep(-1, n)
  for(i in 1:n) {
    sub <- round(x[[i]]$est,6) == q
    if (any(sub)) {
      t1 <- range(x[[i]]$time[sub])
      u[i] <- t1[1]+(1-q)*(t1[2]-t1[1])
    } else {
      sub <- x[[i]]$est <= q
      u[i] <- (x[[i]]$time[sub])[1]
    }
  }
  names(u) <- names(x)
  u
}
