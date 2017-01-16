".expandSensuStricto" <- function (x) {
	stopifnot(length(x) == 1)
	if (isSensuStricto(x)) {
		#	assume specific epitheton is the word before s.str.
		r <- .split0(x)[[1]]
		i <- str_trim(isSensuStricto(), "left") # remove leading space
		i <- unlist(sapply(i, function (x) which(x == r)))
		r2 <- paste("ssp.", r[ i - 1 ], collapse = " ")
		r1 <- paste(r[ -i ], collapse = " ")
		r <- paste(r1, r2)
	} else {
		r <- x
	}
	return(r)	
}

expandSensuStricto <- function (x) {
	#	x = "Luzula multiflora s.str."
	if (is.list(x)) {
		ii <- which(isSensuStricto(x))
		rl <- strsplit(names(x), " ", fixed = TRUE)
		r <- names(x)
		
		for (i in ii) {
			ri <- rl[[ i ]]
			j <- which(sapply(ri, isSensuStricto))
			r1 <- paste(ri[ -j ], collapse = " ")
			#	assume specific epitheton is the word before s.str.
			r2 <- paste("ssp.", ri[ j - 1 ], collapse = " ")
			ri <- paste(r1, r2)
			r[i] <- ri
		}
	}
	else {
		if (any(isSensuStricto(x))) {
			r <- sapply(x, .expandSensuStricto)
		} else {
			r <- x
		}
	}
	return(r)
}
