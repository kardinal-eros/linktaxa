\name{linktaxa}
\alias{linktaxa}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Link two species lists
}
\description{
A single function internally calling subroutines and returning an ordered data frame of string matches.
}
\usage{
linktaxa(x, y, ...)
}

\arguments{
  \item{ x }{
a character.
}
  \item{ y }{
a character.
}
  \item{ \dots }{
arguments passed to
}
}
\details{
Nothing here yet.
}
\value{
a data.frame.
}

\author{
Roland Kaiser
}

\seealso{
\code{\link{isWhat}}, \code{\link{stripAll}}
}
\examples{
data(taxa)

res <- linktaxa(taxa, reference)
head(res[, 1:3])

tail(res[, 1:3])
}
\keyword{ manip }

