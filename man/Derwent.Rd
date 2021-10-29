\name{Derwent}
\alias{Derwent}
\docType{data}
\title{Derwent Catchment Data}
\description{
Data set with flow readings (in cubic meter per second) for six catchments of
in the vicinity of the Derwent river in the northern UK. 
The data was collected daily from November 1, 1971 -- January 31, 1977.
The catchments are Long Bridge, Matlock Bath, Chat Sworth,
What Stand Well, Ashford (Wye) and Wind Field Park (Amber).

The owner of the data is the Ridings Area Office of the Environment 
Agency North-East, UK.
}
\usage{data(Derwent)}
\format{
  A matrix of size 1918 days times 6 catchments.
}
%\details{
%  ~~ If necessary, more details than the __description__ above ~~
%}
\source{
UK National River Flow Archive (NRFA),
\url{https://nrfa.ceh.ac.uk/}

The owner of the data is the Ridings Area Office of the Environment
Agency North-East, UK.
}
\references{
Wikipedia, River Derwent, Yorkshire,
\url{https://en.wikipedia.org/wiki/River_Derwent,_Yorkshire}

Wikipedia, River Wye, Derbyshire,
\url{https://en.wikipedia.org/wiki/River_Wye,_Derbyshire}

Wikipedia, River Amber,
\url{https://en.wikipedia.org/wiki/River_Amber}
}
\examples{
data(Derwent)

i <- 1
plot(Derwent[,i], type="l", main=colnames(Derwent[i]), ylab="Gauged Flows")
}
\keyword{datasets}
