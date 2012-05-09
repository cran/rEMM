\name{TRAC}
\alias{TRAC}
\title{TRAC: Creating an EMM from a regulat clustering}
\description{
Create an EMM from a regular clustering (k-means or PAM) of
sequence data.
}
\usage{
TRAC(x)
}
\arguments{
  \item{x}{ a clustering object (result of kmeans or pam). }
}
%\details{
%}
\value{
    A \code{EMM} object representing the clustering of sequence data.
}
%\references{ ~put references to the literature/web site here ~ }
%\author{ ~~who you are~~ }
\examples{
data("EMMsim")

## cluster sequence data
cl <- kmeans(EMMsim_train, 4)

## create temporal/sequence model
emm <- TRAC(cl)

plot(emm)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{models}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
