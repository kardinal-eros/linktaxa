\name{linktaxa-package}
\alias{linktaxa-package}

\docType{package}
\title{
Link two related but not necessarily identical species lists
}
\description{
A package to link two related but not necessarily identical scientific species lists using approximate string matching. The package further provides tools to accommodate various tasks on species names.
}
\details{
\tabular{ll}{
Package: \tab linktaxa\cr
Type: \tab Package\cr
Version: \tab 0.1-1\cr
Date: \tab 2013-12-06\cr
License: \tab  GPL (>= 2)\cr
}
The main function \code{\link{linktaxa}} takes two character vectors of scientific taxon names and performs an exhaustive search for possible analogs. \code{\link{judgePenalty}} computes penalty scores for retrieved matches. \code{\link{queuePenalty}} performs reordering based on these scores. 
}
\author{
Maintainer: Roland Kaiser <kardinal.eros@gmail.com>

}
\references{
Jansen, F. & Dengler, J. (2010). Plant names in vegetation databases - a neglected source of bias. Journal of Vegetation Science, 21:1179–1186.
}

\keyword{ package }

\seealso{
\code{\link[Taxonstand:TPL]{Taxonstand}}
}
\examples{
# nothing here yet
}
