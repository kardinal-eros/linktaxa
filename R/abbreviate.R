#	select common string positions
".first" <- function (x, nchar = 4) {
	lapply(lapply(x, "[", 1), "substring", 1, nchar)
}

".pair" <- function (x, nchar = 4) {
	lapply(lapply(x, "[", c(1,2)), "substring", 1, nchar)
}


".triple" <- function (x, nchar = 4) {
	lapply(lapply(x, "[", c(1,2,4)), "substring", 1, nchar)
}

abbreviateSensuLato <- function (x) {
	a <- lapply(x, "paste", collapse = " ")
	a <- strsplit(stripSensuLato(unlist(a)), " ", fixed = TRUE)
	a <- lapply(lapply(a, "substring", 1, 4), "paste", collapse = " ")
	a <- lapply(a, function (x) paste(x, "slat", collapse = " "))
	return(a)
}

abbreviateSensuStricto <- function (x) {
	a <- lapply(x, "paste", collapse = " ")	
	a <- strsplit(expandSensuStricto(a), " ", fixed = TRUE)
	a <- lapply(lapply(.triple(a), "substring", 1, 4), "paste", collapse = " ")
	return(a)
}

abbreviateSubspecies <- function (x) {
	.triple(x)	
}

abbreviateVariety <- function (x) {
	.triple(x)
}

abbreviateHybrid <- function (x) {
	stop("hybrid abbrevaitions not implemented yet")
}

abbreviateAffinis <- function (x) {
	stop("affinis abbrevaitions not implemented yet")
}

abbreviateAggregate <- function (x) {
	a <- lapply(.pair(x), function (x) paste(x, "aggr", collapse = " "))
	return(a)
}
	
abbreviateGenus <- function (x) {
	a <- lapply(.first(x), function (x) paste(x, "spec ies", collapse = " "))
	return(a)
}

abbreviateTaxa <- function (x) {
#	x <- c("Ramalina fraxinea s.str.", "Leontodon hispidus s.lat.")
	w <- isWhat(x)
	s <- .split0(x)
	r <- vector("character", length = length(x))
	for (i in 1:nrow(w)) {
		if (any(w[i,])) {
		ii <- w[i, ]
		#	select tool
		t <- names(ii)[which(ii)]
		t <- gsub("is", "abbreviate", t, fixed = TRUE)
		ri <- do.call(t, list(s[i]))
		} else {
			ri <- lapply(.pair(s[i]), ".paste0")
		}
		r[i] <- tolower(unlist(ri))
	}
	return(r)
}