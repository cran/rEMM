\name{transition_table}
\alias{transition_table}
\alias{transition_table,EMM,numeric-method}
\alias{transition_table,EMM,data.frame-method}
\alias{transition_table,EMM,matrix-method}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Extract a Transition Table for a New Sequence Given an EMM}
\description{
Finds the state sequence of a new sequence in an EMM and returns a table
with the transition probabilities or counts.
}
\usage{
\S4method{transition_table}{EMM,matrix}(x, newdata, 
  type = c("probability", "counts", "log_odds"), 
  match_cluster = "exact", prior=TRUE, initial_transition = FALSE)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ an \code{EMM} object. }
  \item{newdata}{ new sequence, }
  \item{type}{ the measure to return. }
  \item{match_cluster}{ do the new observations have to fall within
    the threshold of the cluster (\code{"exact"}) or is nearest neighbor 
    used (\code{"nn"})?}
  \item{prior}{ add one to each transition count. This is equal
      to starting with a uniform prior for the transition count distribution, 
	i.e. initially all transitions are equally likely. It also prevents the 
	product of probabilities to be zero if a transition was never observed. }
  \item{initial_transition}{ include the initial transition 
	 in the table?}
}
%\details{
%}
\value{
    A data.frame with three columns (from state, to state and the 
    transition probability/count.)
}
%\references{ ~put references to the literature/web site here ~ }
%\author{ ~~who you are~~ }
\seealso{\code{\link{transition}} to access transition probabilities
and \code{\link{find_clusters}} for assigning observations to states/clusters. }
\examples{
data("EMMsim")

emm <- EMM(threshold=.5)
emm <- build(emm, EMMsim_train)

head(transition_table(emm, EMMsim_test))
head(transition_table(emm, EMMsim_test, type ="prob", initial_transition=TRUE))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{models}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
