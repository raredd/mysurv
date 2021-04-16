plotmres <- function(x,z,f=.3,...) {
  plot(x,residuals(z),...)
  lines(lowess(x,residuals(z),f=f,iter=0))
  invisible()
}
plotschres <- function(x,del,z,j,f=.3,...) {
  r <- as.matrix(residuals(z,type='schoenfeld'))
  x <- sort(x[del==1])
  plot(x,r[,j],...)
  lines(lowess(x,r[,j],f=f,iter=0))
  invisible()
}
plotcph <- function(z,j,x,ylim,sub=rep(TRUE,length(x)),...) {
  p4 <- predict(z,type='terms',se.fit=TRUE)
  plot(x[sub],as.matrix(p4[[1]])[,j],ylim=ylim,...)
  x <- x[sub]
  o <- order(x)
  lines(x[o],as.matrix(p4[[1]])[o,j]+1.96*as.matrix(p4[[2]])[o,j])
  lines(x[o],as.matrix(p4[[1]])[o,j]-1.96*as.matrix(p4[[2]])[o,j])
  invisible()
}
