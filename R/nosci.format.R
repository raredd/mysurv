nosci.format <- function(x,digits=2) {
  format(x,nsmall=2,scientific=FALSE,digits=digits)
}

#  if (round(x,digits)>= 1) {
#    format(x,digits=digits)
#  } else {
#    u1 <- log(x,base=10)
#    u2 <- trunc(u1)
#    u3 <- format(x*10^(-u2),digits=digits)
#    if (u3 != '1') {
#      u3 <- (strsplit(u3,'\\.'))[[1]][2]
#      u4 <- paste(rep(0,abs(u2)),sep='',collapse='')
#    } else {
#      u4 <- paste(rep(0,abs(u2)-1),sep='',collapse='')
#    }
#    if (nchar(u3)<digits) u3 <- paste(u3,paste(rep(0,digits-nchar(u3)),sep='',collapse=''),sep='')
#    paste('0.',u4,u3,sep='')
#  }
#}

rpv <- function(x) ifelse(x<.095,signif(x,1),round(x,2))
