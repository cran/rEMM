\name{remove}
\alias{remove_clusters}
\alias{remove_clusters,EMM,character-method}
\alias{remove_transitions}
\alias{remove_transitions,EMM,character,character-method}
\alias{remove_transitions,EMM,matrix,missing-method}
\alias{remove_selftransitions}
\alias{remove_selftransitions,EMM-method}
\title{ Remove States/Clusters or Transitions from an EMM }
\description{
Remove states/clusters or transitions from an EMM.
}
\usage{
remove_clusters(x, to_remove, copy = TRUE)
remove_transitions(x, from, to,copy = TRUE)
remove_selftransitions(x, copy = TRUE)
}
\arguments{
  \item{x}{ an \code{EMM} object.}
  \item{to_remove}{ Names of states/clusters to remove.}
  \item{from, to}{ Names of states for removing transitions. If \code{to} is 
    missing \code{from} has to contain a matrix with two columns
    (from and to state names).}
  \item{copy}{ logical; make a copy of x before reclustering? Otherwise the function will change \code{x}!}

}
\details{
\code{remove_selftransitions} removes the transitions from each state to itself.
}
\value{
Returns a EMM with removed states/transitions.
If \code{copy=FALSE} a reference to the object \code{x} 
with the states/transistions removed is returned.
}
%\references{ ~put references to the literature/web site here ~ }
%\author{ ~~who you are~~ }
%\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
data("EMMTraffic")
emm <- EMM(measure="eJaccard", threshold=0.2)
emm <- build(emm, EMMTraffic)

## remove state 3
emm_rs3 <- remove_clusters(emm, "3")

## remove transition 5->2
emm_rt52 <- remove_transitions(emm, "5", "2")

## compare EMMs
op <- par(mfrow = c(2, 2), pty = "m")
plot(emm, method = "graph", main = "original EMM")
plot(emm_rs3, method = "graph", main = "state 3 removed")
plot(emm_rt52, method = "graph", main = "transition 5->2 removed")
par(op)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
