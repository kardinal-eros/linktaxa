\name{seek}
\alias{seekTaxon}
\alias{seekTaxa}

\title{
Approximate string matches
}

\description{
Search for strings using fuzzy matching.
}

\usage{
seekTaxon(x, y, max.distance = 0.3, strip = TRUE, exact = TRUE)
seekTaxa(x, y, max.distance = 0.3, strip = TRUE, exact = TRUE)
}

\arguments{
  \item{ x }{
a character.
}
  \item{ y }{
a character to be compared to argument \code{x}.
}
  \item{ max.distance }{
see \code{\link{agrep}}.
}
  \item{ strip }{
if \code{TRUE} call \code{\link{stripAll}}.
}
  \item{ exact }{
if a single exact match are is found, retain only this.
}
}

\details{
The algorithm proceeds as follows:
\enumerate{
\item Seek matches with \code{\link{agrep}} and \code{max.distance = 0.05}.
\item If search is empty use \code{dropIntraspecific} followed by subsequent calls to \code{asSensuStricto} if necessary.
\item If all these still return no match (the taxon of question might be missing in \code{y}) relax string distance to return at least something similar (argument \code{max.distance}).
\item Finally order matches based on \code{\link{stringdist}} in package \pkg{stringdist}.
}
If taxon is \emph{Genus sp.} it has to match precisely.
}
\value{
a named list of approximate matches for each element of argument \code{x} retained as list names.
}

\author{
Roland Kaiser
}
\seealso{
\code{\link{judgePenalty}}
}
\examples{
seekTaxon("Leontodon", reference)

seekTaxa(taxa, reference)
}
\keyword{ manip }
