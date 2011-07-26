\name{prune}
\alias{prune}
\alias{prune,EMM-method}
\alias{rare_clusters}
\alias{rare_clusters,tNN-method}
\alias{rare_transitions}
\alias{rare_transitions,TRACDS-method}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Prune States and/or Transitions}
\description{
Simplifies an EMM and/or the clustering by removing all 
clusters/states and/or transitions 
which have a count smaller than a given threshold.
}
\usage{
\S4method{prune}{EMM}(x, count_threshold, clusters = TRUE, transitions = TRUE)

rare_clusters(x, count_threshold, ...)
rare_transitions(x, count_threshold, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ an object of class \code{"EMM"}. }
  \item{count_threshold}{ all states/edges with a count below the
  threshold are removed from the model.}
  \item{clusters}{ prune clusters?}
  \item{transitions}{ prune transitions?}
  \item{...}{ further arguments (currently not used).}
}
%\details{
%}
\value{
    \code{prune} returns an object of class \code{EMM}.
    
	\code{rare_clusters} returns a vector of names of rare clusters.
    
	\code{rare_transitions} returns a data.frame of rare transitions.
}
%\references{ ~put references to the literature/web site here ~ }
%\author{ ~~who you are~~ }
\seealso{\code{\link{remove_transitions}},
\code{\link{remove_clusters}}
}
\examples{
data("EMMTraffic")

## For the example we use a very high learning rate
emm_l <- EMM(threshold=0.2, measure="eJaccard", lambda = 1)
emm_l <- build(emm_l, EMMTraffic)

## show state counts and transition counts
cluster_counts(emm_l)
transition_matrix(emm_l, type="counts")

## remove all states and edges with a threshold of 0.1
rare_clusters(emm_l, count_threshold=0.1)
rare_transitions(emm_l, count_threshold=0.1)
emm_lr <- prune(copy(emm_l), count_threshold=0.1)

## compare graphs
op <- par(mfrow = c(1, 2), pty = "m")
plot(emm_l, method = "graph", main = "EMM with high learning rate")
plot(emm_lr, method = "graph", main = "Simplified EMM")
par(op)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
