\name{tNN-class}
\Rdversion{1.1}
\docType{class}
\alias{tNN-class}
%\alias{find_states,tNN,numeric-method}
%\alias{find_states,tNN,matrix-method}
\alias{state_centers}
\alias{state_centers,tNN-method}
\alias{state_counts}
\alias{state_counts,tNN-method}

\title{Class "tNN"}
\description{Implements the threshold Nearest Neighbor clustering algorithm
used by EMM.}
\section{Objects from the Class}{
No objects are created from this class. It is extended by the class \code{"EMM"}
to build the Extensible Markov Model.}
\section{Slots}{
	 \describe{
    \item{\code{measure}:}{Object of class \code{"character"} containing
		the name of the dissimilarity measure used by the EMM 
		(see \code{dist} in \pkg{proxy} for available measures)}
    \item{\code{centroids}:}{Object of class \code{"logical"} indicating
		if centroids are used for states. If \code{FALSE},
		pseudo medians (first observation of a cluster/state) are used
		to represent a state.}
    \item{\code{threshold}:}{Object of class \code{"numeric"} 
		with the dissimilarity threshold used
	    by the NN clustering algorithm for assigning a new
		observation to existing clusters/states.}
    \item{\code{centers}:}{Object of class \code{"matrix"} containing 
		the cluster/state centers. }
    \item{\code{counts}:}{Object of class \code{"numeric"} with the 
		number of observations assigned to each cluster/state.}
    \item{\code{var_thresholds}:}{Object of class \code{"numeric"} with the
		dissimilarity thresholds for individual states (usually
		the same as threshold).}
    \item{\code{lambda}:}{Object of class \code{"numeric"} specifying the
		learning rate.}
    \item{\code{lambda_factor}:}{Object of class \code{"numeric"} expressing
		the learning rate expressed as a factor.}
  }
}
\section{Methods}{
  \describe{
%    \item{find_states}{\code{signature(x = "tNN", newdata = "numeric")}: ... }
%    \item{find_states}{\code{signature(x = "tNN", newdata = "matrix")}: ... }
    \item{state_centers}{\code{signature(x = "tNN")}: returns the state
		centers as a matrix.}
    \item{state_counts}{\code{signature(x = "tNN")}: returns the state 
		counts as a vector.}
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
	\code{\link{find_states}} to find the nearest neighbor state for given data
	points.
	\code{\linkS4class{EMM}} extends "tNN".
}
%\examples{
%showClass("tNN")
%}
\keyword{classes}
