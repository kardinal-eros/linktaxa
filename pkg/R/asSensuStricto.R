asSensuStricto <- function (x, y = "s.str.") {
	if (length(x) != 1) {
		stop("x must be of length one, use sapply(x, asSensuStricto) for longer vectors",
			call. = FALSE)
	}
	if (!isSensuStricto(x)) {
		#	warning: must compare x[2] aignst x[3]
		r <- paste(dropIntraspecific(x, y = c("ssp.", "supsp.")), "s.str.")
	}
	else {
		r <- x
	}
	return(r)
}

