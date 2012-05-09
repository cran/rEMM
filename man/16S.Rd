\name{16S}
\alias{16S}
\alias{Alphaproteobacteria16S}
\alias{Mollicutes16S}
\docType{data}
\title{Count Data for 16S rRNA Sequences}
\description{
This data set contains count data for 16S ribosomal RNA (rRNA) sequences for
the two phylogenetic classes Alphaproteobacteria and Mollicutes. 
The counts for 30
sequences for each class were obtained by counting the occurrence of triplets
of nucleotides in windows of length 100 without any overlap. To
separate sequences a row of dummy count of \code{NA} is used.}
\usage{data("16S")}
\format{
    \code{Alphaproteobacteria16S} and \code{Mollicutes16S} are matrices
    with about 449 rows and 64 (number of possible triplets) columns. 
}
%\details{
%  ~~ If necessary, more details than the __description__ above ~~
%}
\source{
The raw sequence information was obtained from the 
National center for biotechnology information (NCBI) website at
http://www.ncbi.nih.gov/
}
%\references{
%}
\examples{
data("16S")

emm <- EMM("Kullback", threshold=0.1)
build(emm, Mollicutes16S+1)

## start state for sequences have an initial state probability >0
it <- initial_transition(emm)
it[it>0]
}
\keyword{datasets}
