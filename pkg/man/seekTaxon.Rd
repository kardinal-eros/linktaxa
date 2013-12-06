\name{seekTaxon}
\alias{seekTaxon}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
seekTaxon(x, y, max.distance = 0.3, strip = TRUE, exact = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{y}{
%%     ~~Describe \code{y} here~~
}
  \item{max.distance}{
%%     ~~Describe \code{max.distance} here~~
}
  \item{strip}{
%%     ~~Describe \code{strip} here~~
}
  \item{exact}{
%%     ~~Describe \code{exact} here~~
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
function (x, y, max.distance = 0.3, strip = TRUE, exact = TRUE) 
{
    stopifnot(is.numeric(max.distance))
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
        stop("y not provided", call. = FALSE)
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
    require(pbapply)
    require(stringdist)
    x0 <- x
    if (strip) {
        x <- stripAll(x)
    }
    if (length(agrep(" sp.", x, max.distance = 0)) == 1) {
        r <- agrep(x, y, max.distance = 0)
    }
    else {
        t1 <- agrep(x, y, max.distance = 0.05, ignore.case = TRUE)
        if (length(t1) == 0) {
            xx <- dropInfraspecific(x)
            t2 <- agrep(xx, y, max.distance = 0.1, ignore.case = TRUE)
            if (length(t2) == 0) {
                xx <- asSensuStricto(x)
                t3 <- agrep(xx, y, max.distance = 0.1, ignore.case = TRUE)
                if (length(t3) == 0) {
                  r <- agrep(x, y, max.distance = max.distance, 
                    ignore.case = TRUE)
                }
                else {
                  r <- t3
                }
            }
            else {
                r <- t2
            }
        }
        else {
            r <- t1
        }
    }
    r <- sapply(r, function(x) y[x])
    if (length(r) > 1) {
        r <- r[order(stringdist(x, r))]
    }
    if (exact) {
        if (x0 == r[1]) {
            r <- r[1]
        }
    }
    if (length(r) == 0) {
        r <- ""
    }
    return(r)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
