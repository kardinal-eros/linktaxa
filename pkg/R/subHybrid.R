subHybrid <- function (x) {
	if (any(isHybrid(x))) {
		r <- gsub("\u00D7", "x", x, fixed = TRUE)
		return(r)
	}
	else {
		#	message("pattern: \u00D7 (multiplication sign) not found")
		return(x)
	}
}