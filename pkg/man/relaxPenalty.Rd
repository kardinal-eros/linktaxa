\name{relaxPenalty}
\alias{relaxPenalty}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
relaxPenalty(x, tool = c("ssp2sstr", "sstr2ssp", "slat2null", "str2null", "null2str"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{tool}{
%%     ~~Describe \code{tool} here~~
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
function (x, tool = c("ssp2sstr", "sstr2ssp", "slat2null", "str2null", 
    "null2str")) 
{
    r <- NULL
    if (tool == "ssp2sstr") {
        r <- asSensuStricto(names(x)) \%in\% x[[1]]
    }
    if (tool == "sstr2spp") {
        r <- expandSensuStricto(names(x)) \%in\% x[[1]]
    }
    if (tool == "slat2null") {
        r <- stripSensuLato(names(x)) \%in\% x[[1]]
    }
    if (tool == "str2null") {
        r <- stripSensuStricto(names(x)) \%in\% x[[1]]
    }
    if (tool == "null2str") {
        r <- addSensuStricto(names(x)) \%in\% x[[1]]
    }
    if (tool == "var2ssp") {
    }
    if (tool == "ssp2var") {
    }
    if (is.null(r)) {
        r <- FALSE
    }
    return(any(r))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
