seekTaxon <- function (x, y, max.distance = 0.3, strip = TRUE, exact = TRUE) {
	#	x = taxa; y = reference
	stopifnot(is.numeric(max.distance))
	if (length(x) != 1) {
		stop("use seekTaxa for vectors", call. = FALSE)
	}
	if (missing(x)) {
		stop("x not provided", call. = FALSE)
	}
	else {
		if (is.factor(x)) {
			x <- as.character(x)
		}
		else {
			if (!is.character(x)) {
				stop("x must be a character", call. = FALSE)
			}
		}	
	}
	if (missing(y)) {
		stop("y not provided",
			call. = FALSE)
	}
	else {
		if (is.factor(x)) {
			x <- as.character(x)
		}
		else {
			if (!is.character(x)) {
				stop("x must be a character",
					call. = FALSE)
			}
		}
	}	
	#	x = "Abietinella abietina var. abietina"
	
	#	save for final validation
	x0 <- x
	
	#	clean up string
	if (strip) {
		x <- stripAll(x)
	}
	
	#	if taxon is Genus sp. it has to match precisely
	if (length(agrep(" sp.", x, max.distance = 0)) == 1) {
		r <- agrep(x, y, max.distance = 0)	
	}	else {
		#	first try
		t1 <- agrep(x, y, max.distance = 0.05, ignore.case = TRUE)
		if (length(t1) == 0) {
			#	second try
			#	drop intraspecific taxon (ssp.)
			xx <- dropIntraspecific(x)
			#	should return lower distance if ssp. is missing in y
			t2 <- agrep(xx, y, max.distance = 0.1, ignore.case = TRUE)
			if (length(t2) == 0) {
				xx <- asSensuStricto(x)
				t3 <- agrep(xx, y, max.distance = 0.1, ignore.case = TRUE)
				if (length(t3) == 0) {
					#	relax distance to return at least something similar	
					r <- agrep(x, y, max.distance = max.distance, ignore.case = TRUE)	
				}				else {
					r <- t3					
				}	
			}			else {
				r <- t2	
			}
		}		else {
			r <- t1
		}		
	}
	
	#	retrieve strings by index
	r <- sapply(r, function (x) y[x])
	
	#	order matches (stripped if clean = TRUE)
	if (length(r) > 1) {
		r <- r[order(stringdist(x, r))] # was: x0
	}
	
	#	exact matches should have string distance 0, first position
	#	drop all other matches
	if (exact) {
		if (x0 == r[1]) {
			r <- r[1]
		}		
	}
	#	if definetly no match, 
	if (length(r) == 0) {
		r <- ""
	}
	return(r)
}

seekTaxa <- function (x, y, max.distance = 0.3, strip = TRUE, exact = TRUE) {
	r <- sapply(x, function (x) seekTaxon(x, y, max.distance, strip, exact))
	return(r)
}
