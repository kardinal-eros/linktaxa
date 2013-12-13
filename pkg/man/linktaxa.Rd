\name{linktaxa}
\alias{linktaxa}

\title{
Link two species lists
}

\description{
A single function internally calling subroutines and returning an ordered data frame of string matches.
}

\usage{
linktaxa(x, y, order = TRUE, ...)
}

\arguments{
  \item{ x }{
\code{character}.
}
  \item{ y }{
\code{character}.
}
  \item{ order }{
\code{logical}.
}
  \item{ \dots }{
arguments passed to \code{\link{queuePenalty}}.
}
}
\details{
If \code{order} is \code{TRUE} results are ordered according to penalty score, if \code{FASLE} input order of \code{x} is retained.
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

