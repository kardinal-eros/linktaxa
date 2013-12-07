\name{sensu stricto}
\alias{addSensuStricto}
\alias{expandSensuStricto}
\alias{asSensuStricto}

\title{
Manipulate the sensu stricto case in taxon names
}
\description{
The functions either expand \emph{s.str} to the corresponding intraspecific epithet (\code{expandSensuStricto}) or simply ad a trailing \emph{s.str.} to taxon names.
}
\usage{
addSensuStricto(x)
expandSensuStricto(x)
asSensuStricto(x, y = "s.str.")
}

\arguments{
  \item{ x }{
a character.
}
  \item{ y }{
a character.
}
}
\details{
Both functions first call \code{isSensuStricto}. For \code{expandSensuStricto} this function has to evaluate to \code{TRUE}. In contrary \code{addSensuStricto} appends only if \code{isSensuStricto(x) == FALSE}.
}

\value{
a character.
}

\author{
Roland Kaiser
}
\seealso{
\code{\link{isSensuStricto}}
}
\examples{
addSensuStricto(x = "Arenaria ciliata")
addSensuStricto(x = "Arenaria ciliata s.str.")

expandSensuStricto(x = "Arenaria ciliata s.str.")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
