\name{sub}
\alias{subHybrid}

\title{
Replace special strings
}
\description{
Replace multiplication sign (\code{"\u00D7"}) with x glyph.
}

\usage{
subHybrid(x)
}

\arguments{
  \item{ x }{
a character.
}
}

\value{
a character.}

\author{
Roland Kaiser
}
\seealso{
\code{\link{stripAll}}
}
\examples{
subHybrid(x = "Mentha × dumetorum") # \u00D7 (multiplication sign)
}
\keyword{ manip }

