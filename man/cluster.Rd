\name{cluster}
\alias{cluster}
\alias{cluster,tNN,data.frame-method}
\alias{cluster,tNN,matrix-method}
\alias{cluster,tNN,numeric-method}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Data stream clustering with tNN}
\description{
Cluster new data into an existing tNN object.
}
\usage{
cluster(x, newdata, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ a \code{tNN} object. Note that this function canges the original object!}
  \item{newdata}{ a vector (one observation), or a matrix or data.frame
    (each row is an observation). }
 \item{...}{ further arguments like \code{verbose}.}
}
\details{
	\code{cluster()} implements tNN clustering The dissimilarity between
	the new observation and the centers of the clusters is calculated. The
	new observation is assigned to the closest cluster if the dissimilarity
	value is smaller than the threshold (for the state).  If no such state
	exists, a new state is created for the observation. This simple
	clustering algorithm is called nearest neighbor threshold nearest
	neighbor (threshold NN).

	\code{NA}s are handled in the data by using only the other
	dimensions if the data for dissimilarity computation
	(see package~\pkg{proxy}).

	The clusters which the data points in the last \code{cluster()}
	operation where assigned to can be retrieved using the method 
	\code{last_clustering()}.

}
\value{
A reference to the changed tNN object with the data added. 
Note: tNN objects store all variable data in an environment which 
enables us to update partial data without copying the whole object. Assignment
will not create a copy! Use the provided method \code{copy()}. 
}
%\references{ ~put references to the literature/web site here ~ }
%\author{ ~~who you are~~ }
\seealso{ 
	Class \code{\linkS4class{tNN}}, 
	\code{\link{fade}} and \code{\link[proxy]{dist}} 
	in \pkg{proxy}.}
\examples{
## load EMMTraffic data
data(EMMTraffic)

## create empty clustering
tnn <- tNN(th=0.2, measure="eJaccard")
tnn

## cluster some data
cluster(tnn, EMMTraffic)
tnn

## what clusters were the data points assigned to?
last_clustering(tnn)

## plot the clustering as a scatterplot matrix of the cluster centers
plot(tnn)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{models}
\keyword{cluster}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
