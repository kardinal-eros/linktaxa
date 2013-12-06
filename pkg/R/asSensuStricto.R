#	Strip of subspecies and (ssp., subsp) and replace with s.str. (nominotypical taxon)

asSensuStricto <- function (x, y = "s.str.") {
	if (length(x) != 1) {
		stop("x must be of length one, use sapply(x, asSensuStricto) for longer vectors",
			call. = FALSE)
	}
	if (!isSensuStricto(x)) {	
		r <- paste(dropInfraspecific(x, y = c("ssp.", "supsp.")), "s.str.")
	}
	else {
		r <- x
	}
	return(r)
}

# example
#asSensuStricto(x = "Quercus pubescens ssp. pubescens")
#x <- c("Abietinella abietina var. abietina", "Luzula spicata ssp. conglomerata")
#sapply(x, asSensuStricto)
