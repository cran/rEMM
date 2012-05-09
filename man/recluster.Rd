\name{recluster}
\alias{recluster}
\alias{recluster_hclust}
\alias{recluster_hclust,EMM-method}
\alias{recluster_kmeans}
\alias{recluster_kmeans,EMM-method}
\alias{recluster_pam}
\alias{recluster_pam,EMM-method}
\alias{recluster_tNN}
\alias{recluster_tNN,EMM-method}
\alias{recluster_reachability}
\alias{recluster_reachability,EMM-method}
\title{Reclustering EMM states}
\description{
Use various clustering methods to recluster states/clusters
in an EMM. The centers of the clusters in the EMM object are used as 
data points by the reclustering algorithm. States/centers put by reclustering
into the same cluster are merged to produce a new reclustered EMM.
}
\usage{
\S4method{recluster_hclust}{EMM}(x, k=NULL, h=NULL, method="average", 
    ...,prune=NULL, copy=TRUE)
\S4method{recluster_kmeans}{EMM}(x, k, ..., prune=NULL, copy=TRUE)
\S4method{recluster_pam}{EMM}(x, k, ..., prune=NULL, copy=TRUE)
\S4method{recluster_reachability}{EMM}(x, h, ..., prune=NULL, copy=TRUE)
\S4method{recluster_tNN}{EMM}(x, threshold=NULL, ..., prune=NULL, copy=TRUE)
}
\arguments{
  \item{x}{ an \code{"EMM"} object.}
  \item{k}{ number of clusters. }
  \item{h}{ heights where the dendrogram tree should be cut.}
  \item{threshold}{ threshold used on the dissimilarity to join 
    clusters for tNN. If no threshold is specified then the threshold
    stored in the EMM is used. }
  \item{method}{ clustering method used by \code{hclust}. }
  \item{...}{ additional arguments passed on to the clustering algorithm. }
  \item{prune}{ logical; prune states with less 
	than \code{prune} counts before reclustering. }
  \item{copy}{ logical; make a copy of x before reclustering? Otherwise the function will change \code{x}! }
}
\details{
For \code{recluster_kmeans} \code{k} can also be a set of initial cluster 
centers (see argument \code{centers} for \code{kmeans} in package \pkg{stats}).

For \code{recluster_hclust} \code{k} or \code{h} can also be a vector. 
The result is then a list with several (nested) EMMs, one for each value.

For \code{recluster_reachability} reclusters all clusters which are reachable
from each other. A cluster \eqn{j} is reachable from \eqn{i} if
\eqn{j}'s center is closer to \eqn{i}'s center than \code{h} or if \eqn{j} is
reachable by any cluster reachable by \eqn{i}.
}
\value{
An object of class \code{"EMM"} or, if \code{copy=FALSE} a refernece 
to the changed object passed as \code{x}. 

Clustering information is available
as the attribute \code{"cluster_info"}.
The information provided depends
in the clustering algorithm (see \code{hclust}, \code{kmeans} and \code{pam}).


}
%\references{ ~put references to the literature/web site here ~ }
%\author{ ~~who you are~~ }
\seealso{\code{\link{merge_clusters}}, \code{\link{prune}}, 
	\code{\link[stats]{kmeans}}, \code{\link[stats]{hclust}},
	\code{\link[cluster]{pam}}}
\examples{
data("EMMTraffic")
emm <- EMM(measure="eJaccard", threshold=0.2)
build(emm, EMMTraffic)

## do reclustering on a copy of the emm and plot dendrogram
emm_hc <- recluster_hclust(emm, h=0.5)

attr(emm_hc, "cluster_info")

## compare original and clustered EMM
op <- par(mfrow = c(2, 2), pty = "m")   
plot(emm, method= "MDS", main ="original EMM", data = EMMTraffic, 
	ylim=c(-0.5, 0.5), xlim=c(-0.5, 0.5))
plot(emm_hc, method="MDS", main ="clustered EMM", data = EMMTraffic, 
	ylim=c(-0.5, 0.5), xlim=c(-0.5, 0.5))
plot(attr(emm_hc, "cluster_info")$dendrogram)
abline(h=0.5, col="red")
par(op)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}
\keyword{models}
\keyword{cluster}% __ONLY ONE__ keyword per line
