judgePenalty <- function (x, tools = TRUE, clean = TRUE, verbose = FALSE) {
	stopifnot(is.list(x))

	#	loop over list x[i] and store results in list r[i]
	r <- vector("list", length = length(x))
	names(r) <- names(x)
		
	for (i in seq_along(x)) {
		#	run tools
		if (tools) {
			#	clean string for multiplicatiuon sign
			if (clean) {
				ii <- subHybrid(names(x[i]))
			}
			else {
				ii <- names(x[i])
			}
			
			if (!zeroPenalty(x[i])) {
					if (relaxPenalty(x[i], "ssp2sstr")) {
						if (verbose) message("ssp2sstr")
						ii <- asSensuStricto(ii)
				}
				else {
					if (relaxPenalty(x[i], "sstr2ssp")) {
						if (verbose) message("sstr2ssp")
						ii <- expandSensuStricto(ii)
				}
				else {
					if (relaxPenalty(x[i], "slat2null")) {
						if (verbose) message("slat2null")
						ii <- stripSensuLato(ii)
				}
				else {
					if (relaxPenalty(x[i], "str2null")) {
						if (verbose) message("str2null")
						ii <- stripSensuStricto(ii)
				}
				else {
					if (relaxPenalty(x[i], "null2str")) {
						if (verbose) message("null2str")
						ii <- addSensuStricto(ii)
				}
				else {
					if (relaxPenalty(x[i], "var2ssp")) {
						if (verbose) message("var2ssp")
						#	ii <- 
				}
				else {
					if (relaxPenalty(x[i], "ssp2var")) {
						if (verbose) message("ssp2var")
						#	ii <- 
				}
				else {					
					if (verbose) message("no appropiate tool")
					ii <- dropIntraspecific(ii)
				}								
									}					
								}
							}
						}					
					}
				}	
			} # if !zeroPenalty
			ii <- stringdist(ii, unlist(x[i]))			
		} # if strip
		else {
			ii <- stringdist(names(x[i]), unlist(x[i]))
		}
					
		if (any(ii > 0)) {
			if (any(ii == 0)) {
				r[[i]] <- rank(ii) - 1
			}
		}
		else {
			r[[i]] <- ii
		}
	} # for i
	
	#	if stringdist returns nothing
	r[sapply(r, length) == 0] <- 1
	
	#	increase penalty if there is no match per genus
	r[!compareGenus(x)] <- sapply(r[!compareGenus(x)], "+", 1, simplify = FALSE)
	return(r)
}