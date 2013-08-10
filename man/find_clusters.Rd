\name{find_clusters}
\alias{find_clusters}
\alias{find_clusters,tNN,numeric-method}
\alias{find_clusters,tNN,matrix-method}
\alias{find_clusters,tNN,data.frame-method}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Find the EMM State/Cluster for an Observation }
\description{
Finds the cluster and thus the EMM states for observations.
}
\usage{
\S4method{find_clusters}{tNN,matrix}(x, newdata, match_cluster=c("exact", "nn"), dist = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ an \code{EMM} object.}
  \item{newdata}{ a matrix/data.frame with observations.}
  \item{match_cluster}{ find exact or nearest neighbor (nn) cluster/state. 
    If a number is supplied then the threshold times this number is
    used for exact matching.} 
  \item{dist}{ also report the distance to the chosen cluster/state (as a
  data.frame).}
}
%\details{
%  ~~ If necessary, more details than the description above ~~
%}
\value{
Returns the name of the matching clusters/states or a data.frame with
columns "state" and "dist" if \code{dist=TRUE}. 
}
%\references{ ~put references to the literature/web site here ~ }
%\author{ ~~who you are~~ }
\seealso{\code{\linkS4class{EMM}} and \code{\linkS4class{tNN}} }
\examples{
data("EMMTraffic")
emm <- EMM(measure="eJaccard", threshold=0.2)
emm <- build(emm, EMMTraffic)

find_clusters(emm, EMMTraffic)
find_clusters(emm, EMMTraffic, dist=TRUE)

## add noise to the data 
set.seed(1234)
newdata <- sapply(EMMTraffic, jitter, amount=15)
## default is exact match
find_clusters(emm, newdata, dist=TRUE)
## match with nearest neighbor
find_clusters(emm, newdata, match_cluster="nn", dist=TRUE)
## exact match only if within .5 times threshold
find_clusters(emm, newdata, match_cluster=.5, dist=TRUE)
## exact match only if within 2 times threshold
find_clusters(emm, newdata, match_cluster=2, dist=TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{models}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
