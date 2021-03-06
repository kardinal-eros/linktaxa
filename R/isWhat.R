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
		" s.lat. ", " s.lat.",
		" s.lat", " s.lat ",
		" s.l. ", " s.l.", " sl.", " sl"
	)
	if (missing(x)) return(y) else .isWhat(x, y)
}

isSensuStricto <- function (x) {
	#	x = "Luzula multiflora s.str."
	y <- c(
		" s.str. ", " s.str.", " s.str ",
		" s.str", " sstr ", " sstr"
	)
	if (missing(x)) return(y) else .isWhat(x, y)
}

isSubspecies <- function (x) {
	y <- c(
		"ssp.", "subsp."
	)
	if (missing(x)) return(y) else .isWhat(x, y)
}

isSensuStrictoSubspecies <- function (x) {
	if (missing(x)) {
		""
	} else {
		i <- isSensuStricto(x)
		expandSensuStricto(x)
		r <- isSubspecies(x)
		l <- .split0(x)
		lr <- sapply(l[r], function (x) {
			x[2] == x[4] # not so rigid? stringdist(x[2], x[4])
		})
		r[which(r)] <- lr
		return(r)	
	}
}

isVariety <- function (x) {
	y <- c(
		" var. ", " var."
	)
	if (missing(x)) return(y) else .isWhat(x, y)
}

isSensuStrictoVariety <- function (x) {
	if (missing(x)) {
		""
	} else {
		r <- isVariety(x)
		l <- .split0(x)
		lr <- sapply(l[r], function (x) {
			x[2] == x[4] # not so rigid? stringdist(x[2], x[4])
		})
		r[which(r)] <- lr
		return(r)	
	}
}

isAggregate <- function (x) {
	y <- c(
		" agg. ", " agg.",
		" aggr ", " aggr"
	)
	if (missing(x)) return(y) else .isWhat(x, y)
}

isHybrid <- function (x) {
	y <- c(
		"\u00D7", " x "#  "-" dashes?
	)
	if (missing(x)) return(y) else .isWhat(x, y)
}

isAffinis <- function (x) {
	y <- c(
		" cf. ", " cf.",
		" aff. ", " aff.", " aff ", 
		" affin. ", " affin." 
	)
	if (missing(x)) return(y) else .isWhat(x, y)		
}

isGenus <- function (x) {
	y <- c(
		" sp. ", " sp.", " sp ",
		" spp. ", " spp.", " spp " 
	)
	if (missing(x)) return(y) else .isWhat(x, y)		
}

isWhat <- function (x) {
	f <- c(
		"isSensuLato", "isSensuStricto", "isSensuStrictoSubspecies",
		"isSubspecies", "isVariety", "isSensuStrictoVariety",
		"isHybrid",	"isAffinis", "isAggregate",
		"isGenus"
	)
	r <- sapply(f, function (f) do.call(f, list(x)))
	
	#	only one element shoud be TRUE
	#	isSensuStrictoSubspecies & isSubspecies
	if (class(r) == "list") {
	if (r[[3]] & r[[4]]) r[[4]] <- FALSE
	}
	if (class(r) == "matrix") {
		for (i in 1:nrow(r)) {
			if (r[[i, 3]] & r[[i, 4]]) r[[i, 4]] <- FALSE			
		}
		
	}
	#	isVariety & isSensuStrictoVariety
	if (class(r) == "list") {
	if (r[[5]] & r[[6]]) r[[5]] <- FALSE
	}
	if (class(r) == "matrix") {
		for (i in 1:nrow(r)) {
			if (r[[i, 5]] & r[[i, 6]]) r[[i, 5]] <- FALSE			
		}
		
	}

	r <- cbind(r, isResolved = rowSums(r == T) > 0)	
	return(r)
}

isResolved <- function (x) {
	r <- isWhat(x)
	r <- r[ unlist(r[ ,grep("isResolved", colnames(r)) ]), ]
	return(r)
}