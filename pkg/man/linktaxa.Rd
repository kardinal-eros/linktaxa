\name{linktaxa}
\alias{linktaxa}

\title{
Link two species lists
}

\description{
A single function internally calling subroutines and returning an ordered data frame of string matches or writes these results to a csv file.
}

\usage{
linktaxa(x, y, order = TRUE, file, sep = ";", overwrite = FALSE, ...)
}

\arguments{
  \item{ x }{
   \code{character}.
  }
  \item{ y }{
    \code{character}.
  }
  \item{ order }{
    \code{logical}.
  }
  \item{ file }{
    \code{character}. Output file name.
  }
  \item{ sep }{
    \code{character}. Field separator string of \code{\link{write.csv}}.
    All other options to \code{read.csv} are masked and the functions alters the
    defaults for \code{row.names} which is set to \code{FALSE} because it has no meaning.
  }
  \item{ overwrite }{
    \code{logical}. Overwrite existing files.
  }
  \item{ \dots }{
    arguments passed to \code{\link{queuePenalty}}.
 }
}

\details{
If \code{order} is \code{TRUE} results are ordered according to penalty score, if \code{FASLE} input order of \code{x} is retained.
}

\value{
  a data.frame.
}

\author{ Roland Kaiser s}

\seealso{
\code{\link{isWhat}}, \code{\link{stripAll}}
}

\examples{
data(taxa)

res <- linktaxa(taxa, reference)
head(res[, 1:3])

tail(res[, 1:3])
}
\keyword{ manip }

