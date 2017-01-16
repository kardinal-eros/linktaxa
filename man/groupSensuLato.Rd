\name{group}
\alias{groupSensuLato}

\title{
Find Combination of Higher Level Taxon
}

\description{
Search if subspecies can be pooled under \emph{s.lat. Taxon}.
}

\usage{
groupSensuLato(x)
}

\arguments{
  \item{ x }{
a character.
}
}
\value{
list.
}

\author{
Roland Kaiser
}
\seealso{
\code{\link{isWhat}}
}
\examples{
x <- c("Quercus cf. pubescens",
    "Quercus pubescens s.lat.",
    "Quercus pubescens s.str",
    "Quercus pubescens ssp. pubescens")

groupSensuLato(x)
}

\keyword{ manip }

