addSensuStricto <- function (x) {
	if (!isSensuStricto(x)) {
		r <- paste(x, "s.str.")
	} else {
		r <- x
	}
	return(r)
}
