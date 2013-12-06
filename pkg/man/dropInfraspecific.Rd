\name{dropInfraspecific}
\alias{dropInfraspecific}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
dropInfraspecific(x, y)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{y}{
%%     ~~Describe \code{y} here~~
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
function (x, y) 
{
    if (missing(y)) {
        y <- c("var.", "ssp.", "subsp.")
    }
    if (missing(x)) {
        print(y)
    }
    else {
        if (length(x) != 1) {
            stop("x must be of length one, use sapply(x, dropInfraspecific) for vectors", 
                call. = FALSE)
        }
        x <- strsplit(x, " ", fixed = TRUE)[[1]]
        i <- sapply(y, function(z) grep(z, x, fixed = TRUE))
        if (any(sapply(i, length) > 1)) {
            message("abigous matches for", y, "in", paste(x, 
                collapse = ""))
        }
        else {
            ii <- sapply(i, length) > 0
        }
        if (any(ii)) {
            if (sum(ii) > 1) {
                message("both ssp. and var. in taxon name!")
                r <- x
            }
            else {
                r <- x[1:(unlist(i[ii]) - 1)]
            }
        }
        else {
            r <- x
        }
        r <- paste(r, collapse = " ")
        return(r)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
