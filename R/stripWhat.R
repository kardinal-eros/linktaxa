".stripWhat" <- function (x, y) {
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
			jj <- names(x)[i]
			for (j in y) {
				jj <- gsub(paste0(" ", j), " ", jj, fixed = TRUE)
			}			
			r[[ i ]] <- jj
		}
		r <- unlist(r)	
	}
	else {
		r <- x
		for (j in y) {
			r <- gsub(paste0(" ", j), "", r, fixed = TRUE)
		}
	}
	#	doubled spaces between words
	r <- gsub("\\s+", " ", r)
	#	trim ends
	r <- gsub("^\\s+|\\s+$", "", r)
	names(r) <- NULL	
	return(r)	
}

stripSensuLato <- function (x, y) {
	y <- c(
		"s.lat. ", "s.lat.", "s.lat",
		"s. lat. ", "s. lat.", "s. lat",
		"s.l. ", "s.l.",
		"s. l. ", "s. l.",
		"sl." #	"sl" better not
	)	
	if (missing(x)) return(y) else .stripWhat(x, y)
}

stripSensuStricto <- function (x, y) {
	y <- c(
		"s.str. ", "s.str.",
		"s.str ", "s.str",
		"sstr ", "sstr"
	)	
	if (missing(x)) return(y) else .stripWhat(x, y)
}

stripAffinis <- function (x, y) {
	y <- c(
		"cf. ", "cf.",
		"aff. ", "aff.", "aff ", 
		"affin. ", "affin." 
	)
	if (missing(x)) return(y) else .stripWhat(x, y)		
}

stripAll <- function (x) {
	y <- c(
		stripSensuLato(),
		stripSensuStricto(),
		stripAffinis()
	)	
	if (missing(x)) return(y) else .stripWhat(x, y)
}
		
