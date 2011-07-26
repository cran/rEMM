\name{EMM-class}
\Rdversion{1.1}
\docType{class}
\alias{EMM-class}
\alias{show,EMM-method}
\alias{size}
\alias{size,EMM-method}
\alias{copy}
\alias{copy,EMM-method}
%\alias{build,EMM,numeric-method}
%\alias{build,EMM,data.frame-method}
%\alias{build,EMM,matrix-method}
%\alias{fade,EMM,numeric-method}
%\alias{merge_clusters,EMM,character-method}
%\alias{plot,EMM,missing-method}
%\alias{prune,EMM,numeric-method}
%\alias{rare_clusters,EMM,numeric-method}
%\alias{rare_transitions,EMM,numeric-method}
%\alias{recluster_hclust,EMM-method}
%\alias{recluster_kmeans,EMM-method}
%\alias{recluster_pam,EMM-method}
%\alias{remove_selftransitions,EMM-method}
%\alias{remove_clusters,EMM,character-method}
%\alias{remove_transitions,EMM,data.frame,missing-method}
%\alias{remove_transitions,EMM,character,character-method}
%\alias{score,EMM,numeric-method}
%\alias{score,EMM,data.frame-method}
%\alias{score,EMM,matrix-method}

\title{Class "EMM"}
\description{This class represents the extensible Markov Model.	It consists
of a simple data stream clustering algorithm (class \code{"tNN"}) and
a temporal layer (class \code{"TRACDS"}).}
\section{Objects from the Class}{
Objects can be created using the creator function \code{EMM} or by 
directly calling \code{new("EMM", ...)}. Most slots for the extended
classes can be used as parameters for \code{EMM}. 
}
\section{Slots}{
	The slots are described in corresponding the extended classes 
	(see section Extends). 
%	 \describe{
%    \item{\code{measure}:}{Object of class \code{"character"}}
%    \item{\code{centroids}:}{Object of class \code{"logical"}}
%    \item{\code{threshold}:}{Object of class \code{"numeric"}}
%    \item{\code{centers}:}{Object of class \code{"matrix"}}
%    \item{\code{counts}:}{Object of class \code{"numeric"}}
%    \item{\code{var_thresholds}:}{Object of class \code{"numeric"}}
%    \item{\code{lambda}:}{Object of class \code{"numeric"}}
%    \item{\code{lambda_factor}:}{Object of class \code{"numeric"}}
%    \item{\code{mm}:}{Object of class \code{"graphNEL"}}
%    \item{\code{current_state}:}{Object of class \code{"character"}}
%    \item{\code{initial_counts}:}{Object of class \code{"numeric"}}
%  }
}
\section{Extends}{
Class \code{"\linkS4class{tNN}"}, directly.
Class \code{"\linkS4class{TRACDS}"}, directly.
}
\section{Methods}{
  \describe{
    \item{copy}{\code{signature(x = "EMM")}: Make a copy of the EMM object. 
    Making explicit copies is necessary since the subclasses store 
    information in environments which are not copied for regular 
    assignements.}
    \item{size}{\code{signature(x = "EMM")}: Returns the size of
    the EMM (number of clusters/states).}
%    \item{build}{\code{signature(x = "EMM", newdata = "numeric")}: ... }
%    \item{build}{\code{signature(x = "EMM", newdata = "data.frame")}: ... }
%    \item{build}{\code{signature(x = "EMM", newdata = "matrix")}: ... }
%    \item{fade}{\code{signature(x = "EMM", t = "numeric")}: ... }
%    \item{merge_clusters}{\code{signature(x = "EMM", to_merge = "character")}: ... }
%    \item{plot}{\code{signature(x = "EMM", y = "missing")}: ... }
%    \item{prune}{\code{signature(x = "EMM", count_threshold = "numeric")}: ... }
%    \item{rare_clusters}{\code{signature(x = "EMM", count_threshold = "numeric")}: ... }
%    \item{rare_transitions}{\code{signature(x = "EMM", count_threshold = "numeric")}: ... }
%    \item{recluster_hclust}{\code{signature(x = "EMM")}: ... }
%    \item{recluster_kmeans}{\code{signature(x = "EMM")}: ... }
%    \item{recluster_pam}{\code{signature(x = "EMM")}: ... }
%    \item{remove_selftransitions}{\code{signature(x = "EMM")}: ... }
%    \item{remove_clusters}{\code{signature(x = "EMM", to_remove = "character")}: ... }
%    \item{remove_transitions}{\code{signature(x = "EMM", from = "data.frame", to = "missing")}: ... }
%    \item{remove_transitions}{\code{signature(x = "EMM", from = "character", to = "character")}: ... }
%    \item{score}{\code{signature(x = "EMM", newdata = "numeric")}: ... }
%    \item{score}{\code{signature(x = "EMM", newdata = "matrix")}: ... }
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
	\code{\link{build}},
	\code{\link{fade}},
	\code{\link{merge_clusters}},
	\code{\link{plot}},
	\code{\link{prune}},
	\code{\link{rare_clusters}},
	\code{\link{rare_transitions}},
	\code{\link{remove_clusters}},
	\code{\link{remove_transitions}},
	\code{\link{remove_selftransitions}},
	\code{\link{recluster}}, and
	\code{\link{score}}.
}
%\examples{
%showClass("EMM")
%}
\keyword{classes}
