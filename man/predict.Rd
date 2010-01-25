\name{predict}
\alias{predict}
\alias{predict,EMMLayer-method}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Predict a Future State }
\description{
Predict a state or the probability distribution over states in \eqn{n} 
time steps.
}
\usage{
\S4method{predict}{EMMLayer}(object, n = 1, current_state = NULL, 
	probabilities = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{ an \code{"EMM"}/\code{"EMMLayer"} object. }
  \item{n}{ number of time steps. }
  \item{current_state}{ use a specified current state. 
    If \code{NULL}, the EMM's current state is used. }
  \item{probabilities}{ if \code{TRUE},
    instead of the predicted state, the probability distribution is returned.}
}
\details{
Prediction is done using \eqn{A^n} where \eqn{A} is the transition 
probability matrix maintained by the EMM.
Random tie-breaking is used.
}
\value{
    The name of the predicted state or a vector with the probability 
    distribution over all states.
}
%\references{ ~put references to the literature/web site here ~ }
%\author{ ~~who you are~~ }
\seealso{ \code{\link{transition_matrix}} }
\examples{
data("EMMTraffic")
emm <- EMM(measure="eJaccard", threshold=0.2)
emm <- build(emm, EMMTraffic)

#plot(emm) ## plot graph

## Predict state starting an state 1 after 1, 2 and 100 time intervals
## Note, state 7 is an absorbing state.
predict(emm, n=1, current_state="1")
predict(emm, n=2, current_state="1")
predict(emm, n=100, current_state="1")

## Get probability distribution
predict(emm, n=2, current_state="1", probabilities = TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
