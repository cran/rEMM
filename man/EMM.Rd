\name{EMM}
\alias{EMM}
\title{
Creator for Class "EMM"
}
\description{
Create a new object of class \code{"EMM"}.
}
\usage{
EMM(threshold = 0.2, measure = "euclidean", 
    centroids = identical(tolower(measure), "euclidean"), lambda = 0)
}
\arguments{
  \item{threshold}{
Object of class \code{"numeric"} with the dissimilarity threshold used by the 
clustering algorithm for assigning a new observation to existing clusters.
}
  \item{measure}{
Object of class \code{"character"} containing the name of the dissimilarity
measure used (see \code{dist} in \pkg{proxy} for available measures)
}
  \item{centroids}{
Object of class \code{"logical"} indicating if centroids are used for clusters.
If \code{FALSE}, pseudo medians (first observation of a cluster) are used to
represent a cluster.
}
  \item{lambda}{Object of class \code{"numeric"} specifying the
                 rate for fading.
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
emm <- EMM(.3, measure = "Jaccard", lambda = .1)
emm
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
