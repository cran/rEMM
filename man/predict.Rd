\name{predict}
\alias{predict}
\alias{predict,TRACDS-method}
\title{ Predict a Future State }
\description{
Predict a state or the probability distribution over states in \eqn{n} 
time steps.
}
\usage{
\S4method{predict}{TRACDS}(object, current_state = NULL, n=1, 
	probabilities = FALSE, randomized = FALSE, prior=FALSE)
}
\arguments{
  \item{object}{ an \code{"EMM"}/\code{"TRACDS"} object. }
  \item{current_state}{ use a specified current state. 
    If \code{NULL}, the EMM's current state is used. }
  \item{n}{ number of time steps. }
  \item{probabilities}{ if \code{TRUE},
    instead of the predicted state, the probability distribution is returned.}
  \item{randomized}{ if \code{TRUE}, the predicted state is choosen
    randomly with a selection probability proportional to its transition 
    probability}
  \item{prior}{ add one to each transition count. This is equal
      to starting with a uniform prior for the transition count distribution,
      i.e. initially all transitions are equally likely. It also prevents the
      product of probabilities to be zero if a transition was never observed. }
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
\keyword{models}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
