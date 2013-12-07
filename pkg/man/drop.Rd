\name{drop}
\alias{dropIntraspecific}

\title{
Drop intraspecific taxon
}
\description{
Erase intraspecific epithet from string.
}
\usage{
dropIntraspecific(x, y)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ x }{
a character.
}
  \item{ y }{
a character, can be missing.
}
}
\details{
Nothing here yet.
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
# show default pattern
dropIntraspecific()
dropIntraspecific(x = "Luzula spicata ssp. conglomerata")
dropIntraspecific(x = "Abietinella abietina var. abietina")
dropIntraspecific(x = "Abietinella abietina Var. abietina", y = "Var.")
#for vectors use *apply
x <- c("Abietinella abietina var. abietina", "Luzula spicata ssp. conglomerata")
sapply(x, dropIntraspecific)

}

\keyword{ manip }
