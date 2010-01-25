\name{EMMTraffic}
\alias{EMMTraffic}
\docType{data}
\title{Hypothetical Traffic Data Set for EMM}
\description{
Each observation in this hypothetical data set is a vector of
seven values obtained from sensors located at specific
points on roads. Each sensor collects a count of the
number of vehicles which have crossed this sensor in
the preceding time interval.
}
\usage{data(EMMTraffic)}
\format{
  A matrix with 12 observations (rows).
}
%\details{
%  ~~ If necessary, more details than the __description__ above ~~
%}
%\source{
%  ~~ reference to a publication or URL from which the data were obtained ~~
%}
\references{
M.H. Dunham, Y. Meng, J. Huang (2004): Extensible Markov Model, In: 
\emph{ICDM '04: Proceedings of the Fourth IEEE International 
Conference on Data Mining,} pp. 371--374.
}
\examples{
data(EMMTraffic)
EMMTraffic
}
\keyword{datasets}
