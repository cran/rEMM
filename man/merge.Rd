\name{merge_clusters}
\alias{merge_clusters}
\alias{merge_clusters,EMM,character-method}
\alias{merge_clusters,EMM,integer-method}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Merge States of an EMM }
\description{
Merge several clusters/states of an EMM into a single cluster/state.}
\usage{
\S4method{merge_clusters}{EMM,character}(x, to_merge, clustering = FALSE, new_center = NULL, copy=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ an \code{"EMM"} object. Note that the function will change this EMM!}
  \item{to_merge}{ vector of names of the states/clusters to merge. The
      name of the first state in \code{to_merge}
      is used as the name for the new state representing the merged states.}
  \item{clustering}{ is \code{to_merge} a vector with
  cluster assignments as created by a clustering algorithm? }
  \item{new_center}{ supply new centers for the merged clusters.
    New centroids are automatically
    computed. If (pseudo) medoids are used, new medoids should be supplied.
    If none is supplied, the medoid of the cluster in \code{to_merge} 
	which has the most assigned observations 
    is used as the new medoid (warning: this is probably not a good medoid!)}
  \item{copy}{ logical; make a copy of x before reclustering? Otherwise the function will change \code{x}!}
}
%\details{
%}
\value{
    Returns the changed EMM with the states/clusters merged invisibly.
	If \code{copy=FALSE} then it returns a reference to the changes
	object passed as \code{x}.
}
%\references{ ~put references to the literature/web site here ~ }
%\author{ ~~who you are~~ }
%\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
data("EMMTraffic")
emm <- EMM(measure="eJaccard", threshold=0.2)
build(emm, EMMTraffic)
states(emm)

## create a new emm with states 1-3 merged
emm_m123 <- merge_clusters(emm, c("1", "2", "3"))
states(emm_m123)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
