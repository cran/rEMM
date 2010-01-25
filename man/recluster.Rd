\name{recluster}
\alias{recluster}
\alias{recluster_hclust}
\alias{recluster_hclust,EMM-method}
\alias{recluster_kmeans}
\alias{recluster_kmeans,EMM-method}
\alias{recluster_pam}
\alias{recluster_pam,EMM-method}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Reclustering EMM states}
\description{
Use various clustering methods to recluster states/clusters
in an EMM. The centers of the clusters in the EMM object are used as 
data points by the reclustering algorithm. States/centers put by reclustering
into the same cluster are merged to produce a new reclustered EMM.
}
\usage{
\S4method{recluster_hclust}{EMM}(x, k=NULL, h=NULL, method="average", prune=NULL)
\S4method{recluster_kmeans}{EMM}(x, k, ..., prune=NULL)
\S4method{recluster_pam}{EMM}(x, k, ..., prune=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ an \code{"EMM"} object. }
  \item{prune}{ prune states/clusters with less 
	than \code{prune} counts before reclustering. }
  \item{k}{ number of clusters. }
  \item{h}{ heights where the dendrogram tree should be cut.}
  \item{method}{ clustering method used by \code{hclust}. }
  \item{...}{ additional arguments passed on to the clustering algorithm. }
}
\details{
For \code{recluster_kmeans} \code{k} can also be a set of initial cluster 
centers (see argument \code{centers} for \code{kmeans} in package \pkg{stats}).

For \code{recluster_hclust} \code{k} or \code{h} can also be a vector. 
The result is then a list with several (nested) EMMs, one for each value.
}
\value{
An object of class \code{"EMM"}. Clustering information is available
as the attribute \code{"cluster_info"}. The information provided depends
in the clustering algorithm (see \code{hclust}, \code{kmeans} and \code{pam}).
}
%\references{ ~put references to the literature/web site here ~ }
%\author{ ~~who you are~~ }
\seealso{\code{\link{merge_states}}, \code{\link{prune}}, 
	\code{\link[stats]{kmeans}}, \code{\link[stats]{hclust}},
	\code{\link[cluster]{pam}}}
\examples{
data("EMMTraffic")
emm <- EMM(measure="eJaccard", threshold=0.2)
emm <- build(emm, EMMTraffic)

## do clustering and plot dendrogram
emm_hc <- recluster_hclust(emm, h=0.5)

attr(emm_hc, "cluster_info")

## compare original and clustered EMM
op <- par(mfrow = c(2, 2), pty = "m")   
plot(emm, main ="original EMM", data = EMMTraffic, 
	ylim=c(-0.5, 0.5), xlim=c(-0.5, 0.5))
plot(emm_hc, main ="clustered EMM", data = EMMTraffic, 
	ylim=c(-0.5, 0.5), xlim=c(-0.5, 0.5))
plot(attr(emm_hc, "cluster_info")$dendrogram)
abline(h=0.5, col="red")
par(op)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
