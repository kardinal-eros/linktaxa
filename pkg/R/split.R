#	split
".split0" <- function (x, split = " ") {
	strsplit(x, split = split, fixed = TRUE)
}

#	revert split
".paste0" <- function (x, split = " ") {
	paste(x, collapse = split)
}