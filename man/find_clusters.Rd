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
  \item{match_cluster}{ find exact or nearest neighbor cluster/state.}
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
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
