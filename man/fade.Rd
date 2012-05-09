\name{fade}
\alias{fade}
\alias{fade,EMM,numeric,numeric-method}
\alias{fade,EMM,missing,numeric-method}
\alias{fade,EMM,missing,missing-method}
\alias{fade,EMM,numeric,missing-method}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Fading Cluster Structure and EMM Layer}
\description{
Reduces the weight of old observations in the data stream. 
\code{build} has a learning rate parameter
\code{lambda}. If this parameter is set, \code{build} automatically
fades all counts before a new data point is added. The second
mechanism is to explicitly call the function~\code{fade} whenever
fading is needed. This has the advantage that the overhead of manipulating
all counts in the EMM can be reduced and that fading can be used in a more
flexible manner. For example, if the data points are arriving at an irregular
rate, \code{fade} could be called at regular time intervals
(e.g., every second).
}
\usage{
fade(x, t, lambda)
}
\arguments{
  \item{x}{ an object of class \code{"EMM"}. Note that this
  function will change \code{x}. }
  \item{t}{ number of time intervals (if missing 1 is used) }
  \item{lambda}{ learning rate. If \code{lambda} is missing, 
	the learning rate specified for the EMM is used.}
}
\details{
Old data points are faded by using a weight.
We define the weight
for data that is \eqn{t} timesteps in the past by the following strictly 
decreasing function:
\deqn{w_t = 2^{-\lambda t}}{w_t = 2^(-lambda t)}

Since the weight is multiplicative, it can be applied iteratively by 
multiplying at each time step all counts by \eqn{2^{-\lambda}}{2^-lambda}.
For the clustering algorithm the weight of the clusters (number of data
points assigned to the cluster) is faded. For the EMM the initial count vector
and all transition counts are faded.
}
\value{
    Returns a reference to the changed object \code{x}.
}
%\references{ ~put references to the literature/web site here ~ }
%\author{ ~~who you are~~ }
\seealso{\code{\linkS4class{EMM}} and \code{\link{build}}}
\examples{
data("EMMTraffic")

## For the example we use a very high learning rate
## this calls fade after each new data point
emm_l <- EMM(measure="eJaccard", threshold=0.2, lambda = 1)
build(emm_l, EMMTraffic)

## build a regular EMM for comparison
emm <- EMM(measure="eJaccard", threshold=0.2)
build(emm, EMMTraffic)

## compare the transition matrix
transition_matrix(emm)
transition_matrix(emm_l)

## compare graphs
op <- par(mfrow = c(1, 2), pty = "m")
plot(emm, main = "regular EMM")
plot(emm_l, main = "EMM with high learning rate")
par(op)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
