\name{penalty}
\alias{judgePenalty}
\alias{zeroPenalty}
\alias{relaxPenalty}
\alias{queuePenalty}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Compute penalty score
}
\description{
The functions compute penalty scores that can then be used to rank the quality of matches. A perfect match between two respective taxa results in a penalty score of zero. If a taxon has no exact match to the reference the penalty score is always bigger than zero. In case of approximate matches each match is ranked according to the string distance.
}
\usage{
judgePenalty(x, tools = TRUE, clean = TRUE, verbose = FALSE)
zeroPenalty(x)
relaxPenalty(x, tool = c("ssp2sstr", "sstr2ssp", "slat2null", "str2null", "null2str"))
queuePenalty(x, ...)
}

\arguments{
  \item{ x }{
a list as returned by \code{\link{seekTaxon}}.
}
  \item{ tools }{
if \code{TRUE} apply \code{relaxPanalty}.
}
  \item{ tool }{
selects a specific tool (defaults to all available tools) to handle critical cases. See \sQuote{Details}.
}
  \item{ clean }{
apply \code{\link{subHybrid}}.
}
  \item{ verbose }{
print diagnostic messages.
}
  \item{\dots}{
arguments passed to \code{judgePenalty}.
}
}

\details{

The functions perform the following tasks:
\describe{
\item{ \code{judgePenalty} }{ is the main function and calls the next two subroutines.}
\item{ \code{zeroPenalty} }{ checks for exact matches for which the penalty score is zero.}
\item{ \code{relaxPenalty} }{ allows for some exceptions (special cases) and does not increase the penalty for those matches (see below).}
\item{ \code{queuePenalty} }{ reorders list sub elements according to penalty score (internally calls \code{judgePenalty}).}
}

Jansen & Dengler (2010) urge for the use of abbreviations such as \emph{s.str.} or \emph{s.l.} for differently wide species concepts. \dQuote{critical} taxon names that can be misapplied (variants of the same taxonym with and without \emph{s.lat.}, \emph{s.str.}, \emph{agg.} or \emph{auct. non} or if monotypic species occur) requires a specific decision/confirmation of the researcher for such assignments. Allowing lower penalty scores for critical taxon matches is achieved by function \code{relaxPenalty}. The obtained penalty scores aid in the process of assigning a species to the reference.

The \code{tool} argument to \code{relaxPenalty} allows for matches where the species to be matched is transformed in various ways. It is then searched upon all ambiguous matches. The specific implementation of each tool is as follows:

\tabular{rl}{
\code{"ssp2sstr"}  \tab calls \code{\link{asSensuStricto}} to reduce intra specific epithet to \bold{s.str.}\cr
\code{"sstr2ssp"}  \tab calls \code{\link{expandSensuStricto}} to expand \bold{s.str.} to intra specific epithet\cr
\code{"slat2null"} \tab calls \code{\link{stripSensuLato}} to strip of \bold{s.lat.} \cr
\code{"str2null"}  \tab calls \code{\link{stripSensuStricto}} to strip of \bold{s.str.} \cr
\code{"null2str"}  \tab calls \code{\link{addSensuStricto}} adds \bold{s.str.} where this might be appropiate\cr


}
}

\value{
a named list.
}

\author{
Roland Kaiser
}
\references{
Jansen, F. & Dengler, J. (2010). Plant names in vegetation databases - a neglected source of bias. Journal of Vegetation Science, 21:1179–1186.
}
\seealso{
\code{\link{seekTaxon}}
}
\examples{
data(taxa)

#judgePenalty(seekTaxon(taxa, reference))
}
\keyword{ manip }
\keyword{ character }
