queuePenalty <- function (x, ...) {
	#x <- r
	stopifnot(is.list(x))
	#	order by penalty
	p <- judgePenalty(x, ...)	
	for (i in seq_along(x)) {
		if (length(p[[ i ]]) > 1) {
			x[[ i ]] <- x[[ i ]][ p[[ i ]] + ifelse(any(p[[ i ]] == 0), 1, 0) ]
		}
	}
	p <- judgePenalty(x, ...)
	r <- sapply(p, ">", 0)
	r <- sapply(r, all)	
	for (i in which(r)) {
		x[[i]] <- c("", x[[i]])
	}
	return(x)
}