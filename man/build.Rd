\name{build}
\alias{build}
\alias{build,EMM,data.frame-method}
\alias{build,EMM,matrix-method}
\alias{build,EMM,numeric-method}
\alias{reset}
\alias{reset,EMMLayer-method}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Building an EMM using New Data}
\description{
Add new data to an EMM.
}
\usage{
build(x, newdata, ...)
reset(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ an \code{EMM} object. }
  \item{newdata}{ a vector (one observation), or a matrix or data.frame
    (each row is an observation) }
  \item{...}{ further arguments. If \code{newdata} is a matrix
  or a data.frame \code{verbose = TRUE} can be used to monitor the progress of
  building the model.}
}
\details{
	For \code{build()} the dissimilarity between the new observation and the
	centers of the clusters is calculated. The new observation is assigned to
	the closest cluster if the dissimilarity value is smaller than the
	threshold (for the state).  If no such state exists, a new state is created
	for the observation. This simple clustering algorithm is called
	nearest neighbor threshold nearest neighbor (threshold NN).

    After clustering, the EMM is updated. 
	An edge with weight 1 from the current state to the new state (determined
	by the chosen cluster) is added. If this edge already existed, 
	the weight is incremented by one.
    The new state becomes the current state.

    \code{NA}s are handled in the data by using only the other 
	dimensions if the data for dissimilarity computation 
	(see package~\pkg{proxy}).

    \code{reset} resets the current state to \code{NA} for reading in a
    new sequence. A row of all \code{NA} in \code{newdata} for \code{build}
    also resets the current state.
}
\value{
A new EMM object with the data added.
}
%\references{ ~put references to the literature/web site here ~ }
%\author{ ~~who you are~~ }
\seealso{ 
	Class \code{"\linkS4class{EMM}"}, 
	\code{\link{fade}} and \code{\link[proxy]{dist}} 
	in \pkg{proxy}.}
\examples{

## load EMMTraffic data
data("EMMTraffic")
EMMTraffic

## create EMM
emm <- EMM(measure="eJaccard", threshold=0.2)

## build model using EMMTraffic data
emm <- build(emm, EMMTraffic)

size(emm)
plot(emm, method = "graph") ## plot needs Rgraphviz installed
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
