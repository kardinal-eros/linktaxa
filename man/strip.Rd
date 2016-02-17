\name{strip}
\alias{stripAll}
\alias{stripAffinis}
\alias{stripSensuLato}
\alias{stripSensuStricto}

\title{
Manipulate taxon string
}

\description{
The functions translate taxon strings by stripping of common taxonomic markups such as \emph{s.lat.} and \emph{s.str.} as well as orthographic variants thereof.
}

\usage{
stripAll(x)
stripAffinis(x, y)
stripSensuLato(x, y)
stripSensuStricto(x, y)
}

\arguments{
  \item{ x }{
a character.
}
  \item{ y }{
strings to be matched, if missing sensible defaults apply.
}
}
\details{
Call functions without arguments to see which strings are going to be removed from taxon names for a particular function.
}
\value{
character.
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
    "Quercus pubescens s.str")

stripAffinis()
stripAffinis(x)

stripSensuLato()
stripSensuLato(x)

stripSensuStricto()
stripSensuStricto(x)

stripAll()
stripAll(x)
}

\keyword{ manip }

