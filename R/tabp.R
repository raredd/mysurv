tabp <- function(x,y,dec=1) {
  u <- table(y,x)
  u <- rbind(u,apply(u,2,sum))
  us <- apply(u,1,sum)
  up <- round(100*u/us,dec)
  uc <- paste(format(u),' (',format(up),')',sep='')
  dim(uc) <- dim(u)
  dimnames(uc) <- dimnames(u)
  u <- t(u)
  up <- t(up)
  uc <- t(uc)
  un <- table(is.na(x),y)
  if (nrow(un)==2) {
    un <- matrix(c(un[2,],sum(un[2,])),nrow=1)
    dimnames(un) <- list('Unk',NULL)
    u <- rbind(u,un)
    up <- rbind(up,un)
    uc <- rbind(uc,format(un))
  }
  list(u,up,uc)
}
