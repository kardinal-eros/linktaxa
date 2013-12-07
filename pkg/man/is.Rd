\name{is}
\alias{isWhat}
\alias{isSensuLato}
\alias{isSensuStricto}
\alias{isSubspecies}
\alias{isVariety}
\alias{isHybrid}
\alias{isAffinis}
\alias{isAggregate}

\title{ Test taxon string }

\description{
Functions to test character strings for belonging to specific cases used in scientific binomials, including intra specific taxa, determination ambiguities and level.
}

\usage{
isWhat(x)
isSensuLato(x)
isSensuStricto(x)
isSubspecies(x)
isVariety(x)
isHybrid(x)
isAffinis(x)
isAggregate(x)
}

\arguments{
  \item{ x }{
a character string, or a named list of character string, in which case names(x) are used
}
}
\details{
\describe{
\item{ \code{isSensuLato} }{test for \emph{sensu lato} (s.lat.) taxon.}

\item{ \code{isSensuStricto} }{test for \emph{sensu strico} (s.str.) taxon (nominotypical form).}

\item{ \code{isSubspecies} }{test for subspecies.}

\item{ \code{isVariety} }{test for variety.}

\item{ \code{isHybrid} }{test for Hybrid combination.}

\item{ \code{isAffinis} }{test for being ambiguously identified.}

\item{ \code{isAggregate} }{test for species aggregate.}

\item{ \code{isWhat} }{applies all the above functions at once.}

}
Calling functions without argument returns the strings searched by each function.
}

\value{
logical.
}

\author{
Roland Kaiser
}

\seealso{
\code{\link{stripAll}}
}

\examples{
x <- c("Quercus pubescens s.lat.",
   "Quercus pubescens ssp. pubescens",
   "Abietinella abietina var. abietina",
   "Quercus pubescens s.str.",
   "Mentha \u00D7 dumetorum", # MULTIPLICATION SIGN (U+00D7) \u00D7
   "Mentha x dumetorum", # standard x gylph as multiplication sign
   "Mentha cf. arvensis")


isSensuLato()	
isSensuLato(x)

isSensuStricto()
isSensuStricto(x)

isSubspecies()
isSubspecies(x)

isVariety()
isVariety(x)

isHybrid()
isHybrid(x)

data(taxa)

isWhat(taxa)
}

\keyword{ manip }

