\name{judgePenalty}
\alias{judgePenalty}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
judgePenalty(x, tools = TRUE, clean = TRUE, verbose = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{tools}{
%%     ~~Describe \code{tools} here~~
}
  \item{clean}{
%%     ~~Describe \code{clean} here~~
}
  \item{verbose}{
%%     ~~Describe \code{verbose} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x, tools = TRUE, clean = TRUE, verbose = FALSE) 
{
    stopifnot(is.list(x))
    require(stringdist)
    r <- vector("list", length = length(x))
    names(r) <- names(x)
    for (i in seq_along(x)) {
        if (tools) {
            if (clean) {
                ii <- subHybrid(names(x[i]))
            }
            else {
                ii <- names(x[i])
            }
            if (!zeroPenalty(x[i])) {
                if (relaxPenalty(x[i], "ssp2sstr")) {
                  if (verbose) 
                    message("ssp2sstr")
                  ii <- asSensuStricto(ii)
                }
                else {
                  if (relaxPenalty(x[i], "sstr2ssp")) {
                    if (verbose) 
                      message("sstr2ssp")
                    ii <- expandSensuStricto(ii)
                  }
                  else {
                    if (relaxPenalty(x[i], "slat2null")) {
                      if (verbose) 
                        message("slat2null")
                      ii <- stripSensuLato(ii)
                    }
                    else {
                      if (relaxPenalty(x[i], "str2null")) {
                        if (verbose) 
                          message("str2null")
                        ii <- stripSensuStricto(ii)
                      }
                      else {
                        if (relaxPenalty(x[i], "null2str")) {
                          if (verbose) 
                            message("null2str")
                          ii <- addSensuStricto(ii)
                        }
                        else {
                          if (relaxPenalty(x[i], "var2ssp")) {
                            if (verbose) 
                              message("var2ssp")
                          }
                          else {
                            if (relaxPenalty(x[i], "ssp2var")) {
                              if (verbose) 
                                message("ssp2var")
                            }
                            else {
                              if (verbose) 
                                message("no appropiate tool")
                              ii <- dropInfraspecific(ii)
                            }
                          }
                        }
                      }
                    }
                  }
                }
            }
            ii <- stringdist(ii, unlist(x[i]))
        }
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
    }
    r[sapply(r, length) == 0] <- 1
    r[!compareGenus(x)] <- sapply(r[!compareGenus(x)], "+", 1, 
        simplify = FALSE)
    return(r)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
