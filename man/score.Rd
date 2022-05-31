\name{score}
\alias{score}
\alias{score,EMM,numeric-method}
\alias{score,EMM,data.frame-method}
\alias{score,EMM,matrix-method}
\alias{score,EMM,EMM-method}
\title{Score a New Sequence Given an EMM}
\description{
  Calculates a score of how likely it is that a new sequence was generated
  by the same process as the sequences used to build the EMM.
}
\usage{
\S4method{score}{EMM,matrix}(x, newdata, method = c("product", "log_sum", "sum",
        "log_odds", "supported_transitions", "supported_states",
        "sum_transitions",  "log_loss", "likelihood", "log_likelihood", "AIC"),
        match_cluster = "exact", random = FALSE, prior = TRUE, normalize = TRUE,
        initial_transition = FALSE, threshold = NA)
\S4method{score}{EMM,EMM}(x, newdata, method = c("product", "log_sum", "sum",
        "supported_transitions"), match_cluster = "exact", random = FALSE, prior = TRUE,
        initial_transition = FALSE)
}
\arguments{
  \item{x}{ an \code{EMM} object. }
  \item{newdata}{ sequence or another \code{EMM} object to score. }
  \item{method}{ method to calculate the score (see details) }
  \item{match_cluster}{ do the new observations have to fall within
                        the threshold of the cluster (\code{"exact"}) or is nearest neighbor
                        (\code{"nn"}) or weighted nearest neighbor (\code{weighted}) used?
                        If \code{match_cluster} is a number n then observations
                        need to fall within n times the clustering threshold of the cluster.}
  \item{random}{ logical; should the order of newdata be randomized? Can be used to compare the score with the
                        actual score. }
  \item{prior}{ logical; add one to each transition count. This is equal
                   to start with a count of one  for each transition, i.e. initially all
                   transitions are equally likely. It prevents the product
                   of probabilities to be zero if a transition was never observed. }
  \item{normalize}{logical; normalize the score by the length of the sequence.}
  \item{initial_transition}{ logical; include the initial transition
                             in the computation?}
  \item{threshold}{ minimum count threshold used by supported transitions and supported states.}
}
\details{
  The scores for a new sequence \eqn{x} of length \eqn{l} can be computed
  by the following methods. For \code{match_cluster="exact"} or \code{"nn"}:

    \describe{
      \item{"product"}{
        Product of transition probabilities along the path of \eqn{x} in the
        model. A single missing transition (transition probability of zero)
        will result in
        a score of 0. Use \code{prior} to avoid this.
        \deqn{S_\mathrm{product} = \sqrt[l-1]{\prod_{i=1}^{l-1}{a_{s(i),s(i+1)}}}}{
          S_product = prod(a_s(i),s(i+1))^(1/(l-1))}

        where \eqn{a_{s(i),s(j)}}{a_s(i),s(j)} is the transition probability
        between the state representing positions \eqn{i} and \eqn{j} in the sequence.
      }

      \item{"sum"}{
        Average of transition probabilities along the path of \eqn{x} in the
        model.
        \deqn{S_\mathrm{sum} = \frac{1}{l-1} \sum_{i=1}^{l-1}{a_{s(i),s(i+1)}}}{
          S_sum = 1/(l-1) sum(a_s(i),s(i+1))}
      }
      \item{"log_sum"}{
        Average of the log of the transition probabilities along the path of
        \eqn{x} in the model. The ranking of the scores is equivalent to
        the product of probabilities, however, the calculation is more reliable
        since the product of probabilities might become a very small number.

        A single missing transition (transition probability of zero)
        will result in a score of neg. infinity.
        Use \code{prior} to avoid this.

        \deqn{S_\mathrm{log\_sum} = \frac{1}{l-1} \sum_{i=1}^{l-1}{\mathrm{log}(a_{s(i),s(i+1)})}}{
          S_sum = 1/(l-1) sum(log(a_s(i),s(i+1)))}
      }


      \item{"supported_transitions"}{Fraction of transitions in the new sequence \eqn{x} supported (present) in the model after assigning each data point in \eqn{x} to a state in
                                     the model.
                                     \deqn{S_\mathrm{supported\_transitions} = \frac{1}{l-1} \sum_{i=1}^{l-1}{\mathrm{I}(a_{s(i),s(i+1)})}}{
                                       S_supported_transitions = 1/(l-1) sum(I(a_s(i),s(i+1)))}
      }



      \item{"supported_states"}{Fraction of points in the new sequence \eqn{x}
                                for which a state (cluster) exists in the model. \code{match_cluster}
                                is always \code{"exact"} because for \code{"nn"} this measure would
                                always give 1. Note that this measure ignores transition information.

                                If threshold is given, then only states with a count greater than the given threshold are counted as supported.
      }

      \item{"sum_transitions"}{Sum of the counts on the edges in the model on the path of sequence \eqn{x} normalized by the total number of transition counts in the model.
                               \deqn{S_\mathrm{sum\_transitions} = \frac{1}{l-1} \sum_{i=1}^{l-1}c_{s(i),s(i+1)}}{
                                 S_sum_transitions = 1/(l-1) sum(c_s(i),s(i+1))}

                               where \eqn{c_{s(i),s(i+1)}}{c_s(i),s(i+1)} is the transition count  between the state representing positions \eqn{i} and \eqn{j} in the sequence.


                               If threshold is given, then only transitions with a count greater than the given threshold are counted as supported.
      }
      \item{"likelihood", "log_likelihood"}{ The likelihood of the model given the new data is the
      unnormalized product score (product of transition probabilities).}
      \item{"log_loss"}{ The average log loss is defined as
                         \deqn{-sum(log2(a_s(i),s(i+1)))/(l-1)}
      It represents the average compression rate of the new sequence
                         given the model.
      }
      \item{"AIC"}{ Akaike Information Criterion corrected for finite sample size.
      \deqn{2k - 2log(L) 2k(k-1)/(n-k-1)}
      where \eqn{n=l-1} and \eqn{k} is the model complexity measured by the number of
                    non-zero entries in the transition matrix.
                    We use the likelihood of the model given by the proportion
                    of supported transitions. AIC can be used for model selection
                    where the smallest value indicates the preferred model.
      }

    }
  where
  \eqn{x_i} represents the \eqn{i}-th data point in the new sequence,
  \eqn{a(i,j)} is the transition probability from state \eqn{i}
  to state \eqn{j} in the model,
  \eqn{s(i)} is the state the \eqn{i}-th data point (\eqn{x_i}) in
  the new sequence is assigned to.
  \eqn{\mathrm{I(v)}}{I(v)} is an indicator function which is 0 for \eqn{v=0} and 1 otherwise.



  For \code{match_cluster="weighted"}:
  \describe{
    \item{"product"}{
      Weighted version of the product of probabilities. The weight is
      the  similarity between a new data point and the state in the model
      it is assigned to.
      \deqn{S_\mathrm{weighted\_product} = \sqrt[l-1]{\prod_{i=1}^{l-1}{\mathrm{simil}(x_i,s(i))\mathrm{simil}(x_i,s(i+1))  a_{s(i),s(i+1)}}}}{
        P_weighted_product = prod(simil(x_i,s(i))simil(x_i,s(i+1)) a_s(i),s(i+1))^(1/(l-1))}
    }
  \item{"sum"}{
    Weighted version of the sum of probabilities.
    \deqn{S_\mathrm{weighted\_sum} = \frac{1}{l-1} \sum_{i=1}^{l-1}{\mathrm{simil}(x_i,s(i))\mathrm{simil}(x_i,s(i+1))  a_{s(i),s(i+1)}}}{
      S_weighted_sum = 1/(l-1) sum(simil(x_i,s(i))simil(x_i,s(i+1))  a_s(i),s(i+1))}
  }
  \item{"log_sum"}{
    Weighted version of the sum of the log of probabilities.
    \deqn{S_\mathrm{weighted\_log\_sum} = \frac{1}{l-1} \sum_{i=1}^{l-1}{\mathrm{log}(\mathrm{simil}(x_i,s(i))\mathrm{simil}(x_i,s(i+1))  a_{s(i),s(i+1)})}}{
      S_sum = 1/(l-1) sum(simil(x_i,s(i))simil(x_i,s(i+1))  a_s(i),s(i+1))}
  }

  \item{"supported_states"}{
    Same as \code{"supported_states"} but instead of counting the
    supported states, the similarity \eqn{\mathrm{simil}(x_i,s(i))}{simil(x_i,s(i))}
    is used as a weight. Threshold is not implemented.
  }
}

where \eqn{\mathrm{simil}(\cdot)}{simil(.)} is a modified and normalized
similarity function given by
\eqn{\mathrm{simil}(x,s) =  1- \frac{1}{1+e^{-\frac{\mathrm{d}(x, s)/t -1.5}{.2}}}}{simil(x,s)=1-1/(1+exp(-(d(x,s)/t-1.5)/.2))}
where \eqn{d} is the distance measure and \eqn{t} is the threshold that
  was used to create the model.

}
\value{
  A scalar score value.
}
\seealso{\code{\link{transition}} to access transition probabilities
         and \code{\link{find_clusters}} for assigning observations to states/clusters. }
\examples{
data("EMMsim")

emm <- EMM(threshold = .2)
emm <- build(emm, EMMsim_train)

# default is method "product". The score is much higher compared to a randomized order.
score(emm, EMMsim_test)
score(emm, EMMsim_test, random = TRUE)


### create shuffled data (destroy temporal relationship)
### and create noisy data
test_shuffled <- EMMsim_test[sample(1:nrow(EMMsim_test)), ]
test_noise <- jitter(EMMsim_test, amount = .3)

### helper for plotting
mybars <- function(...) {
  oldpar <- par(mar = c(5, 10, 4, 2))
  ss <- rbind(...)
  barplot(
    ss[, ncol(ss):1],
    xlim = c(-1, 4),
    beside = TRUE,
    horiz = TRUE,
    las = 2,
    legend = rownames(ss)
  )
  par(oldpar)
}


### compare various scores
methods <- c(
  "product",
  "sum",
  "log_sum",
  "supported_states",
  "supported_transitions",
  "sum_transitions",
  "log_loss",
  "likelihood"
)

### default is exact matching
clean <-
  sapply(
    methods,
    FUN = function(m)
      score(emm, EMMsim_test, method = m)
  )

shuffled <-
  sapply(
    methods,
    FUN = function(m)
      score(emm, test_shuffled, method = m)
  )

noise <-
  sapply(
    methods,
    FUN = function(m)
      score(emm, test_noise, method = m)
  )

mybars(shuffled, noise, clean)

### weighted matching is better for noisy data
clean <-
  sapply(
    methods,
    FUN = function(m)
      score(emm, EMMsim_test, method = m,
        match = "weighted")
  )

shuffled <-
  sapply(
    methods,
    FUN = function(m)
      score(emm, test_shuffled, method = m,
        match = "weighted")
  )

noise <-
  sapply(
    methods,
    FUN = function(m)
      score(emm, test_noise, method = m,
        match = "weighted")
  )

mybars(shuffled, noise, clean)
}
\keyword{models}
