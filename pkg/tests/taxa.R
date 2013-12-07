if (FALSE) {
	#library(vegsoup)
	#library(linktaxa)
	#https://github.com/kardinal-eros/vegsoup-data/blob/master/windsfeld%20dta/wf.rda
	#load("~/Documents/vegsoup-data/windsfeld dta/wf.rda")
	
	vs <- Taxonomy(wf)
	tv <- read.csv2("~/Documents/vegsoup-standards/turboveg/c europe.csv",
		stringsAsFactors = FALSE)
	y <- tv$ABBREVIAT[tv$SYNONYM == "FALSCH"]
	
	set.seed(1234)
	x <- sort(vs$taxon[sample(nrow(vs), 48)])
	x <- c(x, "Rhododendron × intermedium", "Leontodon hispidus s.lat.")
	res <- linktaxa(x, y)
	
	xx <- (res[,2, drop = T])
	xx <- xx[order(xx, decreasing = F)]
	
	yy <- unlist(res[,-c(1:2)])
	yy <- yy[yy != ""]
	yy <- unique(yy[order(yy, decreasing = F)])
	
	taxa <- xx
	reference <- yy
	
	save("taxa", "reference", file = "taxa.rda")
}