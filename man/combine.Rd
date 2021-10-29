\name{combine}
\alias{c}
\alias{c,EMM-method}
\title{Combining EMM Objects}
\description{
    Combines two or more EMMs into a single object.
}
\usage{
%c(x, \ldots, recursive = FALSE)
\S4method{c}{EMM}(x, ..., copy=TRUE, recursive = FALSE)
}
\arguments{
  \item{x}{ first \code{EMM} object. Note that this object will be changed
  by the function if \code{copy=FALSE}! }
  \item{\dots}{ further objects of the same class as \code{x} to be combined.}
  \item{copy}{ a logical. Copy \code{x} first? Otherwise x will be changed! }
  \item{recursive}{ a logical. If \code{recursive=TRUE},
    the function recursively
    descends through lists combining all their elements into a
    vector.}
}
\value{
  Returns invisibly an object of the same class as \code{EMM}.
}
\seealso{
  \code{\link{EMM-class}},
}
\examples{
data("16S")

## create two EMMs for different data
emm1 <- EMM("Kullback", threshold=0.1, data=Mollicutes16S+1)
emm2 <- EMM("Kullback", threshold=0.1, data=Alphaproteobacteria16S+1)

## combine the two EMMs
emm12 <- c(emm1, emm2)
## this is the same as:
## emm12 <- copy(emm1)
## c(emm12, emm2, copy=FALSE)

## recluster states so similar states in the to EMMs will be merged
emm12r <- recluster_tNN(emm12)

op <- par(mfrow = c(1, 2), pty = "s")
plot(emm12, main="Two EMMs")
plot(emm12r, main="Two EMMs (reclustered)")
par(op)
}
\keyword{manip}
