\name{TRAC}
\alias{TRAC}
\title{TRAC: Creating a Markov Model from a Regular Clustering}
\description{
Create an Markov model from a regular clustering (k-means or PAM) of
sequence data.
}
\usage{
TRAC(x, data = NULL, centers = NULL, measure = "euclidean")
}
\arguments{
  \item{x}{ a clustering object (result of kmeans or PAM), a
    data set (a data matrix), or a vector with (integer) cluster assignments. }
  \item{data}{ the data used for clustering (only used if \code{x}
    is a cluster assignment vector).}
  \item{centers}{ if \code{x} is a cluster assignment vector, then a data.frame or matrix with the cluster centers needs to be supplies. Otherwise, \code{centers} is ignored.}
  \item{measure}{ used distance measure.}
}
\details{
    The order is inferred from the order in the original data set.
}
\value{
    A \code{EMM} object representing the clustering of sequence data.
}
\examples{
data("EMMsim")

## using kmeans
cl <- kmeans(EMMsim_train, 10)
emm <- TRAC(cl)
emm
plot(emm, method = "MDS")

## using a cluster assignment vector (taken from the k-means clustering above)
x <- cl$cluster
emm <- TRAC(x, data = EMMsim_train)
emm
plot(emm, method = "MDS")
}
\keyword{models}
