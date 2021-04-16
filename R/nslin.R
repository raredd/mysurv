nslin <- function(z,nscol,x,df=length(nscol)) {# use natural spline to test 
# for linearity of covariate
# z=coxph fit, nscol=index of spline coefficients, x=covariate ns is computed
# from
  b <- coef(z)[nscol]
  v <- z$var[nscol,nscol]
  u <- lm(x~ns(x,df=df))
  L <- cbind(-coef(u)[-(1:2)],coef(u)[2]*diag(df-1))
  b <- L%*%b
  stat=sum(b * solve(L %*% v %*% t(L), b))
  c(stat,pv=1-pchisq(stat,df-1))
}
