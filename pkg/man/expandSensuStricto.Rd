\name{expandSensuStricto}
\alias{expandSensuStricto}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
expandSensuStricto(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
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
function (x) 
{
    if (is.list(x)) {
        ii <- which(isSensuStricto(x))
        rl <- strsplit(names(x), " ", fixed = TRUE)
        r <- names(x)
        for (i in ii) {
            ri <- rl[[i]]
            j <- which(sapply(ri, isSensuStricto))
            r1 <- paste(ri[-j], collapse = " ")
            r2 <- paste("ssp.", ri[j - 1], collapse = " ")
            ri <- paste(r1, r2)
            r[i] <- ri
        }
    }
    else {
        if (isSensuStricto(x)) {
            r <- strsplit(x, " ", fixed = TRUE)[[1]]
            i <- which(sapply(r, isSensuStricto))
            r1 <- paste(r[-i], collapse = " ")
            r2 <- paste("ssp.", r[i - 1], collapse = " ")
            r <- paste(r1, r2)
        }
    }
    return(r)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
