tplFamily <- function (x) {
	r <- x
	g <- pruneGenus(x)
	f <- unique(g)
	pb <- txtProgressBar(min = 1, max = length(f), style = 3)
	for (i in seq_along(f)) {
		setTxtProgressBar(pb, i)		
		url <- paste("http://www.theplantlist.org/tpl/search?q=",
			f[i], "&csv=true", sep = "")
		ri <- read.table(url, header = TRUE, sep = ",", fill = TRUE,
			stringsAsFactors = FALSE)
		s <- ri$Taxonomic.status.in.TPL == "Accepted"
		if (dim(table(s)) == 2) {
			ri <- ri[ri$Taxonomic.status.in.TPL == "Accepted", ]
		}		
		ri <- table(ri$Family)
		ri <- names(ri)[which.max(ri)] # fingers crossed
		if (length(ri) > 0) {
			r[g == f[i]] <- ri
		} else {
			r[g == f[i]] <- NA
		}
	}
	close(pb)
	return(r)
}