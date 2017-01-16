\name{prune}
\alias{pruneGenus}
\alias{pruneSpecies}

\title{ Prune taxa }

\description{
Prune Genus from taxon string
}

\usage{
pruneGenus(x)
pruneSpecies(x)
}

\arguments{
  \item{ x }{
a character.
}
}

\details{
Simple minded utility function.
}
\value{
a character.
}

\author{
Roland Kaiser
}
\seealso{
\code{\link{isWhat}}
}
\examples{
x <- c(
    "Thalictrum simplex s.lat.",
    "Thalictrum simplex s.str.",
    "Thalictrum simplex ssp. simplex",
    "Abietinella abietina var. abietina",
    "Mentha \u00D7 dumetorum", # MULTIPLICATION SIGN (U+00D7) \u00D7
    "Mentha x dumetorum", # standard x gylph as multiplication sign
    "Mentha cf. arvensis",
    "Achillea millefolium agg.",
    "Bellis perennis")
pruneSpecies(x)
}
\keyword{ manip }
