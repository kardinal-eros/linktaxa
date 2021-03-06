".castList" <- function (x, order = TRUE, ...) { # to penalty
	stopifnot(is.list(x))
		
	l <- sapply(x, length)
	r <- matrix("", nrow = length(x), ncol = max(l))
	for (i in 1:length(x)) {
		if (l[i] > 0) {
			r[ i, 1:l[i] ] <- x[[i]]
		}
	}
	r <- as.data.frame(r,
		stringsAsFactors = FALSE)
	names(r)[1] <- "matched.taxon"
	if (ncol(r) > 1) {
		names(r)[ 2:max(l) ] <- paste0("guess", 1:(max(l) - 1))
	}
	
	r <- data.frame(
		penalty = sapply(judgePenalty(x, ...), sum),
		taxon = names(x),
		r,
		stringsAsFactors = FALSE)
	if (order) {
		r <- r[order(r$penalty, decreasing = TRUE), ]
	}
	rownames(r) <- 1:nrow(r)
	return(r)
}

linktaxa <- function (x, y, order = TRUE, file, sep = ",", overwrite = FALSE, ...) {
	requireNamespace("pbapply")
	stopifnot(is.vector(x))
	stopifnot(is.vector(y))

	if (!inherits(x, "character"))
		x <- as.character(x)
	if (!inherits(y, "character"))
		y <- as.character(y)
		
	if (!missing(file)) {

	}		
	
	r <- pbapply::pbsapply(x, function (x) seekTaxon(x, y), simplify = FALSE)

	q <- queuePenalty(r, ...)
	df <- .castList(q, order = order)
	df[df == ""] <- NA

	#	write file
	if (!missing(file)) {
		if (file.exists(file) & !overwrite) {
			stop("output file exits:\n", file,
				"\nuse overwrite = TRUE", call. = FALSE)
		}
		else {		
			write.table(df, file, sep = sep, row.names = FALSE, quote = FALSE)
		}
	}
	return(df)	
}