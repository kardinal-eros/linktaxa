\name{is}
\alias{isWhat}
\alias{isSensuLato}
\alias{isSensuStricto}
\alias{isSensuStrictoSubspecies}
\alias{isSubspecies}
\alias{isVariety}
\alias{isSensuStrictoVariety}
\alias{isHybrid}
\alias{isAffinis}
\alias{isAggregate}
\alias{isGenus}

\title{ Test taxon string }

\description{
Functions to test character strings for belonging to specific cases used in scientific binomials, including intra specific taxa, determination ambiguities and depth.
}

\usage{
isWhat(x)
isSensuLato(x)
isSensuStricto(x)
isSensuStrictoSubspecies(x)
isSubspecies(x)
isVariety(x)
isSensuStrictoVariety(x)
isHybrid(x)
isAffinis(x)
isAggregate(x)
isGenus(x)
}

\arguments{
  \item{ x }{
a character string, or a named list of character string, in which case names(x) are used
}
}
\details{
\describe{
\item{ \code{isWhat} }{applies all the below listed (sub-)functions in a single step.}

\item{ \code{isSensuLato} }{test for \emph{sensu lato} (s.lat.) taxon.}

\item{ \code{isSensuStricto} }{test for \emph{sensu strico} (s.str.) taxon (nominotypical form).}

\item{ \code{isSensuStrictoSubspecies} }{test subspecies epithet if it is the nominotypical form.}

\item{ \code{isSubspecies} }{test for subspecies.}

\item{ \code{isVariety} }{test for variety.}

\item{ \code{isSensuVariety} }{test variety epithet if it is the nominotypical form.}

\item{ \code{isHybrid} }{test for Hybrid combination.}

\item{ \code{isAffinis} }{test for being ambiguously identified.}

\item{ \code{isAggregate} }{test for species aggregate.}

\item{ \code{isGenus} }{test for determination only at genus level (Genus \emph{sp.}).}
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
x <- c(
    "Thalictrum simplex s.lat.",
    "Thalictrum simplex s.str.",
    "Thalictrum simplex ssp. simplex",
    "Thalictrum simplex ssp. galioides",
    "Quercus pubescens ssp. pubescens",
    "Abietinella abietina var. abietina",
    "Mentha \u00D7 dumetorum", # MULTIPLICATION SIGN (U+00D7) \u00D7
    "Mentha x dumetorum", # standard x gylph as multiplication sign
    "Mentha cf. arvensis",
    "Achillea millefolium agg.")

isWhat(taxa)
}

\keyword{ manip }

