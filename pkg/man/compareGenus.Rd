\name{compareGenus}
\alias{compareGenus}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
compareGenus(x, y, stop, verbose = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{y}{
%%     ~~Describe \code{y} here~~
}
  \item{stop}{
%%     ~~Describe \code{stop} here~~
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
function (x, y, stop, verbose = FALSE) 
{
    if (is.list(x) & missing(y)) {
        y <- sapply(x, "[[", 1)
        x <- names(x)
    }
    x0 <- x == ""
    y0 <- y == ""
    if (verbose) {
        cat("empty strings:\n")
        print(table(x = x0))
        print(table(y = y0))
    }
    x[x0] <- " "
    y[y0] <- " "
    x <- sapply(strsplit(x, " ", fixed = TRUE), "[[", 1)
    y <- sapply(strsplit(y, " ", fixed = TRUE), "[[", 1)
    if (missing(stop)) {
        xl <- sapply(x, nchar)
        yl <- sapply(y, nchar)
    }
    else {
        stopifnot(is.numeric(stop))
        xl <- yl <- stop
    }
    r <- substring(x, 1, xl) == substring(y, 1, yl)
    return(r)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
