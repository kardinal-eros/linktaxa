dropIntraspecific <- function (x, y) {
	#	x = "Asplenium × alternifolium nsubsp. alternifolium (= A. septentrionale ssp. septentrionale × A. trichomanes ssp. trichomanes)"
	if (missing(y)) {
		y <- c(
			"var.", "ssp.", "subsp.", "nsubsp."
		)
	}
	if (missing(x)) {
		print(y)
	}
	else {
		if (length(x) != 1) {
			stop("x must be of length one, use sapply(x, dropIntraspecific) for vectors",
				call. = FALSE)
		}
		x <- strsplit(x, " ", fixed = TRUE)[[1]]		
		i <- sapply(y, function (z) grep(z, x, fixed = TRUE))
		if (any(sapply(i, length) > 1)) {
			message("ambigous matches for ", y, " in ", paste(x, collapse = " "))
			ii <- FALSE
		}
		else {
			ii <- sapply(i, length) > 0		
		}
		
		if (any(ii)) {
			if (sum(ii) > 1) {
				message("both ssp. and var. in taxon name!")
				r <- x
			}
			else {
				r <- x[1:(unlist(i[ ii ]) - 1)]
			}
		}
		else {
			r <- x
		}
		r <- paste(r, collapse = " ")
		return(r)
	}
}