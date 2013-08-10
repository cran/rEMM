\name{EMM}
\alias{EMM}
\alias{object.size,EMM-method}
\title{
Creator for Class "EMM"
}
\description{
Create a new object of class \code{"EMM"}.
}
\usage{
EMM(threshold = 0.2, measure = "euclidean", distFun = NULL, 
    centroids = identical(tolower(measure), "euclidean"), 
    lambda = 0, data = NULL)
}
\arguments{
  \item{threshold}{
Object of class \code{"numeric"} with the dissimilarity threshold used by the 
clustering algorithm for assigning a new observation to existing clusters.
}
  \item{measure}{
Object of class \code{"character"} containing the name of the dissimilarity
measure used (see \code{dist} in \pkg{proxy} for available measures).
}
  \item{distFun}{
Specify a function passed on as method to \code{dist} in \pkg{proxy} 
(see \code{dist} in \pkg{proxy}).
The character string passed on as \code{measure} will be used as the 
measure's name.
}
  \item{centroids}{
Object of class \code{"logical"} indicating if centroids are used for clusters.
If \code{FALSE}, pseudo medians (first observation of a cluster) are used to
represent a cluster.
}
  \item{lambda}{Object of class \code{"numeric"} specifying the
                 rate for fading.
		 }
  \item{data}{Initial data to build the EMM. 
    This just calls \code{build} on the new EMM.
		 }

}
%\details{
%}
\value{
An object of class \code{"EMM"}.
}

\seealso{
\code{\link{EMM-class}}
}
\examples{
## load EMMTraffic data
data(EMMTraffic)

## create empty EMM
emm <- EMM(threshold=0.2, measure="eJaccard", lambda=.1)
emm

## cluster some data
build(emm, EMMTraffic)
emm

## what clusters were the data points assigned to?
last_clustering(emm)

## plot the clustering as a graph
plot(emm)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{models}
