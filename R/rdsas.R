rdsas <- function(data = "SOUTD", name = "SOUTN",sep="\n")
{
# name is the list name
# data is in SOUTD, NAMES in SOUTN
	a <- scan(name, list("", 0))
	typ <- a[[2]]
	ll <- length(typ)
	type <- vector("list", ll)
	for(i in 1:ll) {
		if(typ[i] == 1) {
			type[[i]] <- 0
		}
		else {
			type[[i]] <- " "
		}
	}
	d <- scan(data, type, sep = sep, multi.line = TRUE)
	names(d) <- a[[1]]
	data.frame(d)
}

