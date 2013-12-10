\name{abbreviate}
\alias{abbreviateTaxa}

\alias{abbreviateSensuLato}
\alias{abbreviateSensuStricto}
\alias{abbreviateSubspecies}
\alias{abbreviateVariety}
\alias{abbreviateHybrid}
\alias{abbreviateAffinis}
\alias{abbreviateAggregate}
\alias{abbreviateGenus}

\title{ Abbreviate taxon string }

\description{
Functions to abbreviate taxon names by four first letters of the generic name and four first letters of the specific epithet of a Latin name. Abbreviations may be extended by another four letters in case of infraspecific taxa. \emph{Sensu strico} (s.str.) taxa are expanded by \code{\link{expandSensuStricto}}. \emph{Sensu lato} (s.lat.) taxa get \code{"slat"} appended after stripping \code{\link{stripSensuLato}}.
}

\usage{
abbreviateTaxa(x)
abbreviateSensuLato(x)
abbreviateSensuStricto(x)
abbreviateSubspecies(x)
abbreviateVariety(x)
abbreviateHybrid(x)
abbreviateAffinis(x)
abbreviateAggregate(x)
abbreviateGenus(x)
}

\arguments{
  \item{ x }{
a character string, or a named list of character string, in which case names(x) are used
}
}
\details{
\describe{
\item{ \code{abbreviateSensuLato} }{adds \code{"slat"}}

\item{ \code{abbreviateSensuStricto} }{expands \code{"s.str."} to infraspecific given by species epithet.}

\item{ \code{abbreviateSubspecies} }{drops \emph{ssp.}}

\item{ \code{abbreviateVariety} }{drops \emph{var.}}

\item{ \code{abbreviateHybrid} }{not implemented yet}

\item{ \code{abbreviateAffinis} }{not implemented yet}

\item{ \code{abbreviateGenus} }{adds \code{"spec ies"}, this tries to ensure ambiguous abbreviations for taxa like \emph{Galeopsis speciosa}.}

}
}

\value{
character.
}

\author{
Roland Kaiser
}

\seealso{
\code{\link[vegan:make.cepnames]{make.cepnames}} in \pkg{vegan}
}

\examples{
# Nothing here yet
}

\keyword{ manip }

