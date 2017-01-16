groupSensuLato <- function (x) {
	r <- data.frame(isResolved(x))
	r <- r[ unlist(r$isSensuLato), ]

	x1 <- stripSensuLato(rownames(r))
	x2 <- pruneSpecies(x)

	rr <- sapply(x1, function (x) x == x2, simplify = FALSE)
	for (i in seq_along(rr)) {
		rr[[ i ]] <- x[ rr[[ i ]] ]
	}

	names(rr) <- rownames(r)

	rr <- rr[ sapply(rr, length) > 1 ]
	
	return(rr)
}