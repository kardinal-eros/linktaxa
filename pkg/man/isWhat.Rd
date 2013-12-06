\name{isWhat}
\alias{isWhat}
\alias{isSensuLato}
\alias{isSensuStricto}
\alias{isSubspecies}
\alias{isHybrid}
\alias{isAffinis}

\title{ Test taxon string }
\description{
Functions to text charcter strings for beloning to specific cases uses in syntific binomials, including infraspecific taxa and determination abiguities.
}
\usage{
isWhat(x)
isSensuLato(x)
isSensuStricto(x)
isSubspecies(x)
isHybrid(x)
isAffinis(x)

}

\arguments{
  \item{ x }{
a character string, or a named list of character string, in which case names(x) are used
}
}
\details{
\code{isSensuLato} tests string for beeing a sensu lato (s.lat.) taxon.
\code{isSensuStricto}Test string for beeing a sensu strico (s.str.) taxon (nominotypical form).
\code{isSubspecies}Test string for beeing a subspecies.
\code{isHybrid}Test string for beeing a Hybrid combination.
\code{isAffinis} Test string for beeing unbigously identified.
\code{isWhat} applies all the above functions at once.

Calling functions without argument returns the strings searched by each function.
}
\value{
logical.
}
\author{
Roland Kaiser
}
\note{
%%  ~~further notes~~
}
\seealso{
\code{\link{stripWhat}}, ~~~
}
\examples{
x <- c(
   "Quercus pubescens s.lat.",
   "Quercus pubescens ssp. pubescens",
   "Quercus pubescens s.str.",
   "Mentha x dumetorum", # MULTIPLICATION SIGN (U+00D7) \u00D7
   "Mentha × dumetorum", # standard x gylph as multiplication sign
   "Mentha cf. arvensis")

isWhat(x)
isSensuLato()	
isSensuLato(x)

isSensuStricto()
isSensuStricto(x)

isSubspecies()
isSubspecies(x)

isHybrid()
isHybrid(x)
}

\keyword{ manip }

