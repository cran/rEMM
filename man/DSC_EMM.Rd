\name{DSC_EMM}
\alias{DSC_EMM}
\alias{DSC_tNN}
\alias{get_EMM}
\alias{set_EMM}
\title{
DSC Interface for EMM and tNN (package stream)
}
\description{
Provides Data Stream Clusterer (DSC) interfaces for EMM and tNN so they can be used in the
\pkg{stream} framework.
}
\usage{
DSC_EMM(formula = NULL, threshold = 0.2, measure = "euclidean", distFun = NULL,
    centroids = identical(tolower(measure), "euclidean"),
    lambda = 0)

DSC_tNN(formula = NULL, threshold = 0.2, measure = "euclidean",
    centroids = identical(tolower(measure), "euclidean"), lambda = 0)

get_EMM(dsc)
set_EMM(dsc, x)
}
\arguments{
  \item{formula}{\code{NULL} to use all features in the stream or a model \link{formula} of the form \code{~ X1 + X2}
    to specify the features used for clustering. Only \code{.}, \code{+} and \code{-} are currently
    supported in the formula.}
  \item{threshold}{
    A \code{"numeric"} with the dissimilarity threshold used by the
    clustering algorithm for assigning a new observation to existing clusters.
  }
  \item{measure}{
    A \code{"character"} containing the name of the dissimilarity
    measure used (see \code{dist} in \pkg{proxy} for available measures).
  }
  \item{distFun}{
    Specify a function passed on as method to \code{dist} in \pkg{proxy}
    (see \code{dist} in \pkg{proxy}).
    The character string passed on as \code{measure} will be used as the
    measure's name. %'
}
  \item{centroids}{
    A \code{"logical"} indicating if centroids are used for clusters.
    If \code{FALSE}, pseudo medians (first observation of a cluster) are used to
    represent a cluster.
  }
  \item{lambda}{A \code{"numeric"} specifying the
    rate for fading.
  }
  \item{dsc}{ an object of class \code{"DSC_EMM"}. }
  \item{x}{ an object of class \code{"EMM"} }
}
\details{
DSC_tNN and DSC_EMM wrap the clustering algorithms so they can be used with the stream framework.
See \code{\link[stream]{DSC}} for details.

\code{get_EMM()} and \code{set_EMM()} can be used to access the EMM object inside the DSC_EMM object.
}
\value{
An object of class \code{"DSC_EMM"} or \code{"DSC_tNN"}.
}

\examples{
library(stream)

### tNN clustering example

stream <- DSD_Gaussians()
stream

cl <- DSC_tNN(threshold = .1)
cl

update(cl, stream, 100)
cl

get_centers(cl)
get_weights(cl)

plot(cl, stream)

## EMM clustering example
data("EMMsim")
plot(EMMsim_train, pch = NA)
lines(EMMsim_train, col = "gray")
points(EMMsim_train, pch = EMMsim_sequence_train)

stream <- DSD_Memory(EMMsim_train)
stream

cl <- DSC_EMM(threshold = 0.1, measure = "euclidean", lambda = .1)
update(cl, stream, n = 200)
cl

reset_stream(stream)
plot(cl, stream, n = 200, method = "pca")

# inspect and recluster the EMM in the DSC_EMM object
emm <- get_EMM(cl)
plot(emm)

emm <- recluster_hclust(emm, k = 4, method = "average")
plot(emm)

set_EMM(cl, emm)

reset_stream(stream)
plot(cl, stream, n = 200, method = "pca")
}
\keyword{models}
