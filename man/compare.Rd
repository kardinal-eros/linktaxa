\name{compare}
\alias{compareGenus}

\title{
Compare to vectors of taxon names for matching genus
}

\description{
The function compares one vector of taxa names against a second and returns if at least the genus is matching.
}

\usage{
compareGenus(x, y, stop)
}

\arguments{
  \item{ x }{
a character.
}
  \item{ y }{
a character.
}
  \item{ stop }{
number of characters to match, if missing equals number of characters of respective genus.
}
}

\value{
logical.
}

\author{
Roland Kaiser
}

\seealso{
\code{\link{linktaxa}}
}

\examples{
compareGenus(x = "Bellis perennis", y = "Aster bellis")
compareGenus(x = "Bellis perennis var. fagetorum", y = "Bellis perennis var. hybrida")
}
\keyword{ manip }
