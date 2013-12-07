queuePenalty <- function (x, ...) {
	stopifnot(is.list(x))
	#	order by penaltiy
	p <- judgePenalty(x, ...)	
	for (i in seq_along(x)) {
		if (length(p[[ i ]]) > 1) {
			x[[i]] <- x[[ i ]][ p[[ i ]] + 1]
		}
	}
	p <- judgePenalty(x, ...)
	r <- sapply(p, ">", 0)
	r <- sapply(r, all)	
	for(i in which(r)) {
		x[[i]] <- c("", x[[i]])
	}
	return(x)
}