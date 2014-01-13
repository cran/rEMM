\name{transition}
\alias{transition}
\alias{transition,TRACDS,character,character-method}
\alias{transition,TRACDS,matrix,missing-method}
\alias{transition,TRACDS,data.frame,missing-method}
\alias{transition_matrix}
\alias{transition_matrix,TRACDS-method}
\alias{initial_transition}
\alias{initial_transition,TRACDS-method}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Access Transition Probabilities/Counts in an EMM }
\description{
Calculates individual transition probabilities/counts 
or a complete transition matrix
for an EMM (which contains \code{"TRACDS"}).
}
\usage{
\S4method{transition}{TRACDS,character,character}(x, from, to, 
          type = c("probability", "counts", "log_odds"), prior = TRUE)
\S4method{transition_matrix}{TRACDS}(x, 
          type = c("probability", "counts", "log_odds"), prior = TRUE)
\S4method{initial_transition}{TRACDS}(x, 
          type = c("probability", "counts", "log_odds"), prior = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ an object of class \code{"EMM"}/\code{"TRACDS"}.}
  \item{from, to}{ Names a states. If \code{to} is missing, 
  \code{from} has
  to contain a matrix with two columns (a from column and a 
  to column as returned by \code{transitions}).}
  \item{type}{ What should be calculated? }
  \item{prior}{ add one to each transition count. This is equal
      to starting with a uniform prior for the transition count distribution, 
      i.e., initially all transitions are equally likely. }
}
\details{
Log odds are calculated as \eqn{ln(a/(1/n))} where \eqn{a} is the probability
of the transition and \eqn{n} is the number of states in the EMM.  \eqn{1/n} is
the probability of a transition under the null model which assumes that the
transition probability from each state to each other state (including staying
in the same state) is the same, i.e., the null model has a transition matrix
with all entries equal to \eqn{1/n}.
}
\value{
    A scalar (for \code{transition}), a square matrix 
    (for \code{transition_matrix}) or a vector (for \code{initial_transition}).
}
%\references{ ~put references to the literature/web site here ~ }
%\author{ ~~who you are~~ }
\seealso{\code{\linkS4class{EMM}} which contains 
\code{\linkS4class{TRACDS}}}
\examples{
data("EMMTraffic")
emm <- EMM(measure="eJaccard", threshold=0.2)
emm <- build(emm, EMMTraffic)

## get transition matrix
transition_matrix(emm, type="count", prior=FALSE)
transition_matrix(emm, type="count")
transition_matrix(emm, prior=FALSE)
transition_matrix(emm)

## get initial state probabilities
initial_transition(emm)

## access individual transition probability (state 1 -> 2)
transition(emm, "1","2")

## get counts for all existing transitions
tr <- transitions(emm)
tr
cbind(as.data.frame(tr), counts=transition(emm, tr, type="counts"))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{models}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
