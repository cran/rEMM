\name{tNN-class}
\Rdversion{1.1}
\docType{class}
\alias{tNN}
\alias{tNN-class}
\alias{StreamClustering-class} %%% currently unused
%\alias{find_clusters,tNN,numeric-method}
%\alias{find_clusters,tNN,matrix-method}
\alias{cluster_centers}
\alias{cluster_centers,tNN-method}
\alias{cluster_counts}
\alias{cluster_counts,tNN-method}
\alias{clusters,tNN-method}
\alias{size,tNN-method}

\title{Class "tNN"}
\description{Implements the threshold Nearest Neighbor clustering algorithm
used by EMM.}
\section{Objects from the Class}{
Objects can be created with \code{new()} or by the creator function 
\code{tNN()}. It is extended by the class \code{"EMM"}
to build the Extensible Markov Model.}
\section{Slots}{
	 \describe{
    \item{\code{measure}:}{Object of class \code{"character"} containing
		the name of the dissimilarity measure used  
		(see \code{dist} in \pkg{proxy} for available measures)}
    \item{\code{centroids}:}{Object of class \code{"logical"} indicating
		if centroids are used for clusters. If \code{FALSE},
		pseudo medians (first observation of a cluster) are used
		to represent a cluster.}
    \item{\code{threshold}:}{Object of class \code{"numeric"} 
		with the dissimilarity threshold used
	    by the NN clustering algorithm for assigning a new
		observation to existing clusters.}
    \item{\code{centers}:}{Object of class \code{"matrix"} containing 
		the cluster centers. }
    \item{\code{counts}:}{Object of class \code{"numeric"} with the 
		number of observations assigned to each cluster.}
    \item{\code{var_thresholds}:}{Object of class \code{"numeric"} with the
		dissimilarity thresholds for individual clusters (usually
		the same as threshold).}
    \item{\code{lambda}:}{Object of class \code{"numeric"} specifying the
		 rate for fading.}
    \item{\code{lambda_factor}:}{Object of class \code{"numeric"} expressing
		the fading rate expressed as a factor.}
    \item{\code{last}:}{A \code{"character"} vector containing the
		cluster names the points for the previous call of
		\code{cluster()} were assigned to.}
  }
}
\section{Methods}{
  \describe{
%    \item{find_clusters}{\code{signature(x = "tNN", newdata = "numeric")}: ... }
%    \item{find_clusters}{\code{signature(x = "tNN", newdata = "matrix")}: ... }
    \item{cluster_centers}{\code{signature(x = "tNN")}: returns the cluster
		centers as a matrix.}
    \item{cluster_counts}{\code{signature(x = "tNN")}: returns the cluster 
		counts as a vector.}

    \item{size}{\code{signature(x = "tNN")}: returns the number of clusters/states
	in the clustering.}
    \item{clusters}{\code{signature(x = "tNN")}: returns the names of the
	clusters.}

	 }
}
\references{
M.H. Dunham, Y. Meng, J. Huang (2004): Extensible Markov Model, In:
\emph{ICDM '04: Proceedings of the Fourth IEEE International
Conference on Data Mining,} pp. 371--374.
}
%\author{ ~~who you are~~ }
%\note{ ~~further notes~~ }
\seealso{
	\code{\link{find_clusters}} to find the nearest neighbor state for given data
	points.
	\code{\linkS4class{EMM}} extends "tNN".
}
%\examples{
%showClass("tNN")
%}
\keyword{classes}
