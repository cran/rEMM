\name{build}
\alias{build}
\alias{build,EMM,data.frame-method}
\alias{build,EMM,matrix-method}
\alias{build,EMM,numeric-method}
\title{Building an EMM using New Data}
\description{
Add new data to an EMM.
}
\usage{
build(x, newdata, ...)
%\S4method{update}{EMM}(object, newdata, verbose=FALSE)
}
\arguments{
  \item{x}{ an \code{EMM} object. Note that the function will change the original EMM!}
  \item{newdata}{ a vector (one observation), or a matrix or data.frame
    (each row is an observation) }
  \item{...}{ further arguments. If \code{newdata} is a matrix
  or a data.frame then \code{verbose = TRUE} can be used to monitor
  the progress of
  building the model.}
}
\details{
    \code{build()} performs clustering and also updates the TRACDS temporal
    layer.

    \code{NA}s are handled in the data by using only the other
	dimensions if the data for dissimilarity computation
	(see package \pkg{proxy}).
}
\value{
A reference to the changed EMM object with the data added.
Note: EMM objects store all variable data in an environment which
enables us to update partial data without copying the whole object. Assignment
will not create a copy! Use the provided method \code{copy()}.
}
\seealso{
	Class \code{\linkS4class{TRACDS}},
	\code{\link{fade}} and \code{\link[proxy]{dist}}
	in \pkg{proxy}.}
\examples{

## load EMMTraffic data
data("EMMTraffic")
EMMTraffic

## create EMM
emm <- EMM(measure="eJaccard", threshold=0.2)

## build model using EMMTraffic data (note that the EMM object is
## changed without assignment!)
build(emm, EMMTraffic)
## same as: emm <- build(emm, EMMTraffic)

size(emm)
plot(emm)

## emm2 <- emm does not create a copy (just a reference)
## a "deep" copy is created using copy()
emm2<- copy(emm)

## convert the emm into a graph
as.igraph(emm)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{models}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
