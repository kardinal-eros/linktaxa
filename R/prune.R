pruneGenus <- function (x) {
	sapply(.split0(x), "[", 1)
}

pruneSpecies <- function (x) {
	r <- .split0(x)
	l <- sapply(r, length)
	i <- isAffinis(x) | isHybrid(x) # second element afer space is not epitheton
	r <- paste(sapply(r, "[[", 1), sapply(r, "[[", 2))
	r[ i ] <- x[ i ]
	return(r)
}