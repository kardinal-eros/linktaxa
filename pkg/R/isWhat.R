".isWhat" <- function (x, y) {
	if (!is.list(x) & length(x) > 1) {
		stopifnot(is.character(x))
		n <- x
		x <- as.list(x)
		names(x) <- n
	}
	if (is.list(x)) {
		if (is.null(names(x))) {
			message("x is not a named list")
			n <- unlist(x)
			x <- as.list(n)
			names(x) <- n			
		}
		r <- vector("list", length = length(x))
		names(r) <- names(x)
		for (i in seq_along(x)) {
			ii <- sapply(y, function (z) grep(z, names(x)[ i ], fixed = TRUE))	
			ii <- any(sapply(ii, length) > 0)
			r[[ i ]] <- ii
		}
		r <- unlist(r)		
	}
	else {
		r <- sapply(y, function (z) grep(z, x, fixed = TRUE))	
		r <- any(sapply(r, length) > 0)		
	}
	return(r)	
}

isSensuLato <- function (x) {
	#	x = "Luzula sylvatica s.lat."
	y <- c(
		"s.lat. ", "s.lat.", "s.lat",
		"s.l. ", "s.l.", "sl.", "sl"
	)
	if (missing(x)) return(y) else .isWhat(x, y)
}

isSensuStricto <- function (x) {
	#	x = "Luzula multiflora s.str."
	y <- c(
		"s.str. ", "s.str.", "s.str",
		"sstr ", "sstr"
	)
	if (missing(x)) return(y) else .isWhat(x, y)
}

isSubspecies <- function (x) {
	y <- c(
		"ssp.", "supsp."
	)
	if (missing(x)) return(y) else .isWhat(x, y)
}

isVariety <- function (x) {
	y <- c(
		"var. ", "var."
	)
	if (missing(x)) return(y) else .isWhat(x, y)
}

isAggregate <- function (x) {
	y <- c(
		"agg. ", "agg.",
		"aggr ", "aggr"
	)
	if (missing(x)) return(y) else .isWhat(x, y)
}

isHybrid <- function (x) {
	y <- c(
		"\u00D7", " x "
	)
	if (missing(x)) return(y) else .isWhat(x, y)
}

isAffinis <- function (x) {
	y <- c(
		"cf. ", "cf.",
		"aff. ", "aff.", "aff ", 
		"affin. ", "affin." 
	)
	if (missing(x)) return(y) else .isWhat(x, y)		
}

isWhat <- function (x) {
	f <- c(
		"isSensuLato", "isSensuStricto",
		"isSubspecies", "isVariety",
		"isHybrid",	"isAffinis", "isAggregate"
	)
	r <- sapply(f, function (f) do.call(f, list(x)))
	return(r)
}