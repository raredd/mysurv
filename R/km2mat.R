km2mat <- function(kmobj) {
  time <- NULL
  for (i in 1:length(kmobj)) time <- c(time,kmobj[[i]]$time[-1])
  time <- sort(unique(c(0,time)))
  uu <- (timepoints(kmobj,time)$est)[,c(rep(1:(length(time)-1),rep(2,length(time)-1)),length(time))]
  time <- c(0,rep(time[-1],rep(2,length(time)-1)))
  for (i in 1:nrow(uu)) uu[i,time>max(kmobj[[i]]$time)] <- NA
  uu <- rbind(time,uu)
  dimnames(uu) <- NULL
  uu
}
#### problem--doesn't go to 0 if largest obs is a failure
#library(local,lib.loc='../..',first=T)
#u2 <- km2mat(uu)
#write(round(u2,5),'data',ncol=nrow(u2))


