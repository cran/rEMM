\name{score}
\alias{score}
\alias{score,EMM,numeric-method}
\alias{score,EMM,data.frame-method}
\alias{score,EMM,matrix-method}
\alias{score,EMM,EMM-method}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Score a New Sequence Given an EMM}
\description{
Calculates a score of how likely it is that a new sequence was generated
by the same process as the sequences used to build the EMM.
}
\usage{
\S4method{score}{EMM,matrix}(x, newdata, method = c("product", "log_sum", "sum",
"weighted_product", "weighted_log_sum", "weighted_sum", "log_odds", 
"supported_transitions", "supported_states", "weighted_supported_states", 
"sum_transitions"), 
match_cluster = "nn", plus_one=FALSE, initial_transition = FALSE)
\S4method{score}{EMM,EMM}(x, newdata, method = c("product", "log_sum", "sum", 
"supported_transitions"), match_cluster = "nn", plus_one=FALSE, 
initial_transition = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ an \code{EMM} object. }
  \item{newdata}{ sequenc or another \code{EMM} object to score. }
  \item{method}{ method to calculate the score (see details) }
  \item{match_cluster}{ do the new observations have to fall within
    the threshold of the cluster (\code{"exact"}) or is nearest neighbor 
    used (\code{"nn"})? If \code{match_cluster} is a number n then observations 
    need to fall within n times the clustering threshold of the cluster.}
  \item{plus_one}{ add one to each transition count. This is equal
      to start with a count of one  for each transition, i.e. initially all
          transitions are equally likely. It prevents the product
          of probabilities to be zero if a transition was never observed. }
  \item{initial_transition}{ include the initial transition 
         in the computation?}
}
\details{
The scores for a new sequence \eqn{x} of length \eqn{l} can be computed
by the following methods:

\describe{
\item{"product"}{
    Product of transition probabilities along the path of \eqn{x} in the 
    model. A single missing transition (transition probability of zero) 
    will result in
    a score of 0. Use \code{plus_one} to avoid this.
    \deqn{S_\mathrm{product} = \sqrt[l-1]{\prod_{i=1}^{l-1}{a_{s(i),s(i+1)}}}}{
	S_product = prod(a_s(i),s(i+1))^(1/(l-1))}

  where \eqn{a_{s(i),s(j)}}{a_s(i),s(j)} is the transition probability
  between the state representing positions \eqn{i} and \eqn{j} in the sequence.
}

\item{"sum"}{
    Sum of transition probabilities along the path of \eqn{x} in the
    model.
    \deqn{S_\mathrm{sum} = \frac{1}{l-1} \sum_{i=1}^{l-1}{a_{s(i),s(i+1)}}}{
	S_sum = 1/(l-1) sum(a_s(i),s(i+1))}
}
\item{"log_sum"}{
    Sum of the log of the transition probabilities along the path of 
    \eqn{x} in the model. The ranking of the scores is equivalent to
    the product of probabilities, however, the calculation is more reliable 
    since the product of probabilities might become a very small number.

    A single missing transition (transition probability of zero)
    will result in a score of neg. infinity. 
    Use \code{plus_one} to avoid this. 
    
    \deqn{S_\mathrm{log\_sum} = \frac{1}{l-1} \sum_{i=1}^{l-1}{\mathrm{log}(a_{s(i),s(i+1)})}}{
	S_sum = 1/(l-1) sum(log(a_s(i),s(i+1)))}
}


\item{"weighted_product"}{
    Weighted version of the product of probabilities. The weight is
    the  similarity between a new data point and the state in the model
    it is assigned to.
    \deqn{S_\mathrm{weighted\_product} = \sqrt[l-1]{\prod_{i=1}^{l-1}{\mathrm{simil}(x_i,s(i))\mathrm{simil}(x_i,s(i+1))  a_{s(i),s(i+1)}}}}{
	P_weighted_product = prod(simil(x_i,s(i))simil(x_i,s(i+1)) a_s(i),s(i+1))^(1/(l-1))}
}
\item{"weighted_sum"}{
    Weighted version of the sum of probabilities.
    \deqn{S_\mathrm{weighted\_sum} = \frac{1}{l-1} \sum_{i=1}^{l-1}{\mathrm{simil}(x_i,s(i))\mathrm{simil}(x_i,s(i+1))  a_{s(i),s(i+1)}}}{
	S_weighted_sum = 1/(l-1) sum(simil(x_i,s(i))simil(x_i,s(i+1))  a_s(i),s(i+1))}
}
\item{"weighted_log_sum"}{
Weighted version of the sum of the log of probabilities.
\deqn{S_\mathrm{weighted\_log\_sum} = \frac{1}{l-1} \sum_{i=1}^{l-1}{\mathrm{log}(\mathrm{simil}(x_i,s(i))\mathrm{simil}(x_i,s(i+1))  a_{s(i),s(i+1)})}}{
	S_sum = 1/(l-1) sum(simil(x_i,s(i))simil(x_i,s(i+1))  a_s(i),s(i+1))}
}


\item{"supported_transitions"}{Fraction of transitions in the new sequence \eqn{x} supported (present) in the model after assigning each data point in \eqn{x} to a state in
the model.
\deqn{S_\mathrm{supported\_transitions} = \frac{1}{l-1} \sum_{i=1}^{l-1}{\mathrm{I}(a_{s(i),s(i+1)})}}{
            S_supported_transitions = 1/(l-1) sum(I(a_s(i),s(i+1)))}
}

\item{"sum_transitions"}{Sum of the counts on the edges in the model on the path of sequence \eqn{x} normalized by the total number of transition counts in the model.
                               \deqn{S_\mathrm{sum\_transitions} = \frac{1}{l-1} \sum_{i=1}^{l-1}c_{s(i),s(i+1)}}{
                                 S_sum_transitions = 1/(l-1) sum(c_s(i),s(i+1))}

  where \eqn{c_{s(i),s(i+1)}}{c_s(i),s(i+1)} is the transition count  between the state representing positions \eqn{i} and \eqn{j} in the sequence.

}

\item{"supported_states"}{Fraction of points in the new sequence \eqn{x} 
for which a state (cluster) exists in the model. \code{match_cluster}
is always \code{"exact"} because for \code{"nn"} this measure would
always give 1. Note that this measure ignors transition information.
}

\item{"weighted_supported_states"}{
Same as \code{"supported_states"} but instead of counting the 
supported states, the similarity \eqn{\mathrm{simil}(x_i,s(i))}{simil(x_i,s(i))}
is used as a weight. \code{match_cluster}
is always \code{"nn"}.
}

}

where 
\eqn{x_i} represents the \eqn{i}-th data point in the new sequence,
\eqn{a(i,j)} is the transition probability from state \eqn{i}
to state \eqn{j} in the model,
\eqn{s(i)} is the state the \eqn{i}-th data point (\eqn{x_i}) in
the new sequence is assigned to,
and \eqn{\mathrm{simil}(\cdot)}{simil(.)} is a modified and normalized 
similarity function.
It is normalzed for the threshold such that it is 
\eqn{\mathrm{simil} = 1}{simil = 1} for 
\eqn{d \le \mathrm{threshold}}{d<=threshold} and 
\eqn{\mathrm{simil} = .5^{(d/threshold -1)}}{simil = .5^(d/threshold -1)}
otherwise.
\eqn{d} is the same distance measure that was used to create the model.

For missing transitions \eqn{\mathrm{I(v)}}{I(v)} is an indicator function which is 0 for \eqn{v=0} and 1 otherwise.
}
\value{
    A scalar score value.
}
%\references{ ~put references to the literature/web site here ~ }
%\author{ ~~who you are~~ }
\seealso{\code{\link{transition}} to access transition probabilities
and \code{\link{find_clusters}} for assigning observations to states/clusters. }
\examples{
data("EMMsim")

emm <- EMM(threshold=.5)
emm <- build(emm, EMMsim_train)

### compute various scores
score(emm, EMMsim_test, method="supported_transitions")
score(emm, EMMsim_test) # default is "product"
score(emm, EMMsim_test, method="sum")
score(emm, EMMsim_test, method="log_sum")
score(emm, EMMsim_test, method="weighted_product")
score(emm, EMMsim_test, method="weighted_sum")
score(emm, EMMsim_test, method="weighted_log_sum")
score(emm, EMMsim_test, method="supported_states", match="exact")
score(emm, EMMsim_test, method="weighted_supported_states")

### shuffle the data and score again
EMMsim_test <- EMMsim_test[sample(1:nrow(EMMsim_test)),]
score(emm, EMMsim_test, method="supported_transitions")
score(emm, EMMsim_test) # default is "product"
score(emm, EMMsim_test, method="sum")
score(emm, EMMsim_test, method="log_sum")
score(emm, EMMsim_test, method="weighted_product")
score(emm, EMMsim_test, method="weighted_sum")
score(emm, EMMsim_test, method="weighted_log_sum")
score(emm, EMMsim_test, method="supported_states", match="exact")
score(emm, EMMsim_test, method="weighted_supported_states")

### deal with missing transitions
score(emm, EMMsim_test, method="product", plus_one=TRUE)
score(emm, EMMsim_test, method="log_sum", plus_one=TRUE)
score(emm, EMMsim_test, method="weighted_product", plus_one=TRUE)
score(emm, EMMsim_test, method="weighted_log_sum", plus_one=TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{models}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
