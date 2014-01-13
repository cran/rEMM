\name{smooth_transitions}
\alias{smooth_transitions}
\alias{smooth_transitions,EMM-method}
\title{Smooths transition counts between neighboring states/clusters}
\description{
Each state/cluster gets the average count if all the outgoing transitions of
its neighbors (i.e., clusters which are within range x its threshold).
}
\usage{
\S4method{smooth_transitions}{EMM}(x, range = 2, copy = TRUE)
}
\arguments{
  \item{x}{ an object of class \code{"EMM"}}
  \item{range}{ threshold multiplier for the smoothing range.}
  \item{copy}{ logical; make a copy of x before reclustering? Otherwise the function will change \code{x}!}
}
%\details{
%}
\value{
    \code{smooth_transitions} returns invisibly an object of class \code{EMM}.
    If \code{copy=FALSE} then it returns a reference to the changes
    object passed as \code{x}.
}
%\references{ ~put references to the literature/web site here ~ }
%\author{ ~~who you are~~ }
\seealso{\code{\link{prune}}
}
\examples{
data("EMMTraffic")

## learn a model
emm <- EMM(threshold=0.2, measure="eJaccard")
build(emm, EMMTraffic)

## smooth the model by adding tansitions
emm_s <- smooth_transitions(emm)

## compare graphs
op <- par(mfrow = c(1, 2), pty = "m")
plot(emm, method="MDS", main="Original")
plot(emm_s, method="MDS", main="Smoothed")
par(op)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
