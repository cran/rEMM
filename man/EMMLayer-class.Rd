\name{EMMLayer-class}
\Rdversion{1.1}
\docType{class}
\alias{EMMLayer-class}
\alias{current_state}
\alias{current_state,EMMLayer-method}
\alias{size}
\alias{size,EMMLayer-method}
\alias{states}
\alias{states,EMMLayer-method}
\alias{transitions}
\alias{transitions,EMMLayer-method}
%\alias{initial_transition,EMMLayer-method}
%\alias{transition_matrix,EMMLayer-method}
%\alias{transition,EMMLayer,data.frame,missing-method}
%\alias{transition,EMMLayer,character,character-method}
%\alias{predict,EMMLayer-method}

\title{Class "EMMLayer"}
\description{Representation of the temporal structure of a data stream clustering using a extensible Markov model.}
\section{Objects from the Class}{
No objects are created from this class. It is extended by the class \code{"EMM"}
to build the Extensible Markov Model.
}
\section{Slots}{
	 \describe{
    \item{\code{mm}:}{Object of class \code{"graphNEL"} representing the 
		first order Markov model of the EMM. }
    \item{\code{current_state}:}{Object of class \code{"character"} with the
		name of current state in the EMM. \code{NA} means
			no current state.}
	\item{\code{initial_counts}:}{Object of class \code{"numeric"} indicating
		how often a state is the first state in a sequence
		(used to calculate initial state probabilities for the Markov chain).}
	 }
}
\section{Methods}{
  \describe{
    \item{current_state}{\code{signature(x = "EMMLayer")}: returns the name of 
		the current state. }
%    \item{initial_transition}{\code{signature(x = "EMMLayer")}: ...}
%    \item{predict}{\code{signature(object = "EMMLayer")}: ... }
    \item{size}{\code{signature(x = "EMMLayer")}: returns the number of states 
		in the EMM.}
    \item{states}{\code{signature(x = "EMMLayer")}: returns the names of the
		states in the EMM.}
%    \item{transition_matrix}{\code{signature(x = "EMMLayer")}: ... }
%    \item{transition}{\code{signature(x = "EMMLayer", from = "data.frame", to = "missing")}: ... }
%    \item{transition}{\code{signature(x = "EMMLayer", from = "character", to = "character")}: ... }
    \item{transitions}{\code{signature(x = "EMMLayer")}: returns all transitions		as a matrix of state names with a from and a to column. }
	 }
}
\references{ 
M.H. Dunham, Y. Meng, J. Huang (2004): Extensible Markov Model, In:
\emph{ICDM '04: Proceedings of the Fourth IEEE International
Conference on Data Mining,} pp. 371--374.
}
%\author{ ~~who you are~~ }
%\note{ ~~further notes~~ }
\seealso{
	Look
	\code{\link{transition}},
	\code{\link{transition_matrix}} and 
	\code{\link{initial_transition}} to access the transition information in
	the EMM.
	\code{\link{predict}} is used to predict future states of an EMM.
	\code{\linkS4class{EMM}} extends \code{"EMMLayer"}.
}
%\examples{
%showClass("EMMLayer")
%}
\keyword{classes}
