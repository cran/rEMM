\name{TRACDS-class}
\Rdversion{1.1}
\docType{class}
\alias{TRACDS-class}
\alias{TRACDS}
\alias{as.igraph}
\alias{as.igraph.TRACDS}
\alias{as.graph}
\alias{as.graph.TRACDS}
\alias{current_state}
\alias{current_state,TRACDS-method}
\alias{nstates}
\alias{nstates,TRACDS-method}
\alias{ntransitions}
\alias{ntransitions,TRACDS-method}
\alias{copy,TRACDS-method}
\alias{states}
\alias{states,TRACDS-method}
\alias{show,TRACDS-method}
\alias{plot,TRACDS,missing-method}
\alias{transitions}
\alias{transitions,TRACDS-method}
\alias{object.size,TRACDS-method}
%\alias{initial_transition,TRACDS-method}
%\alias{transition_matrix,TRACDS-method}
%\alias{transition,TRACDS,data.frame,missing-method}
%\alias{transition,TRACDS,character,character-method}
%\alias{predict,TRACDS-method}

\title{Class "TRACDS"}
\description{Representation of the temporal structure of a data stream clustering using a extensible Markov model.}
\section{Objects from the Class}{
Objects can be created using the creator function \code{TRACDS} or by
directly calling \code{new("TRACDS", ...)}. Most slots for the extended
classes can be used as parameters.
}
\section{Slots}{
	 \describe{
	\item{\code{lambda}:}{Object of class \code{"numeric"} specifying the
		 rate for fading.}
       \item{\code{lambda_factor}:}{Object of class \code{"numeric"} expressing
		 the fading rate expressed as a factor.}

   \item{\code{tracds_d}:}{An environment containing all the
    variable data of the TRACDS object:
      \describe{
      \item{\code{mm}:}{Object of class \code{"SimpleMC"} representing the 
		first order Markov model of the EMM. }
    \item{\code{current_state}:}{Object of class \code{"character"} with the
		name of current state in the EMM. \code{NA} means
			no current state.}
%	\item{\code{initial_counts}:}{Object of class \code{"numeric"} indicating
%		how often a state is the first state in a sequence
%		(used to calculate initial state probabilities for the Markov chain).}
	 }}
	 }
}
\section{Methods}{
  \describe{
      \item{copy}{\code{signature(x = "TRACDS")}: Make a copy of the TRACDS object.
	  Making explicit copies is necessary since
	      information is stored in an environment which is not copied for regular
	      assignements.}

    \item{current_state}{\code{signature(x = "TRACDS")}: returns the name of 
		the current state. }
%    \item{initial_transition}{\code{signature(x = "TRACDS")}: ...}
%    \item{predict}{\code{signature(object = "TRACDS")}: ... }
    \item{nstates}{\code{signature(x = "TRACDS")}: returns the number of states.}
    \item{ntransitions}{\code{signature(x = "TRACDS")}: returns the number of transitions with a count larger than 0 stored in the object.}
    \item{plot}{\code{signature(x = "TRACDS", y = "missing")}: Plots the
    object as a directed graph.}
    \item{states}{\code{signature(x = "TRACDS")}: returns the names of the
		states.}
%    \item{transition_matrix}{\code{signature(x = "TRACDS")}: ... }
%    \item{transition}{\code{signature(x = "TRACDS", from = "data.frame", to = "missing")}: ... }
%    \item{transition}{\code{signature(x = "TRACDS", from = "character", to = "character")}: ... }
    \item{transitions}{\code{signature(x = "TRACDS")}: returns all transitions		as a matrix of state names with a from and a to column. }
	 }
}
\references{ 
Michael Hahsler and Margaret H. Dunham. Temporal structure learning for clustering massive data streams in real-time. In \emph{SIAM Conference on Data Mining} (SDM11), pages 664--675. SIAM, April 2011. \doi{10.1137/1.9781611972818.57}

M. Hahsler, M. H. Dunham (2010): rEMM: Extensible Markov Model for 
Data Stream Clustering in R, \emph{Journal of Statistical Software,} 35(5), 1-31,
URL \doi{10.18637/jss.v035.i05}

M.H. Dunham, Y. Meng, J. Huang (2004): Extensible Markov Model, In:
\emph{ICDM '04: Proceedings of the Fourth IEEE International
Conference on Data Mining,} pp. 371--374.
}
%\author{ ~~who you are~~ }
\note{A TRACDS object can be coerced to igraph or graph objects using 
\code{as.igraph}() and \code{as.graph()}.}
\seealso{
	Look at
	\code{\link{transition}},
	\code{\link{transition_matrix}} and 
	\code{\link{initial_transition}} to access the transition information in
	the EMM.
	\code{\link{predict}} is used to predict future states of an EMM.
	\code{\linkS4class{EMM}} extends \code{"TRACDS"}.
}
%\examples{
%}
\keyword{classes}
