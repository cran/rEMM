\name{plot}
\alias{plot,EMM,missing-method}
\alias{plot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Visualize EMM Objects }
\description{
Visualize EMM objects.}
\usage{
\S4method{plot}{EMM,missing}(x, y, method=c("MDS", "igraph", "interactive", 
    "graph", "cluster_counts", "transition_counts"), data = NULL, 
    parameter=NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ an \code{EMM} object. }
  \item{y}{ unused (just for compatibility with the generic for plot in
   \pkg{graphics}) }
  \item{method}{ see details section.}
  \item{data}{ Project the state centers onto these data. Points which do
    not belong to any cluster are shown in blue.}
  \item{parameter}{ a list of parameters for plotting. }
  \item{\dots}{ further arguments passed on to \code{plot.default}. }
}
\details{
\code{"MDS"} projects the cluster centers into 2-dimensional space.

\code{"igraph"} produces a graph representation of the EMM (using \pkg{igraph}).

\code{"interactive"} produces an interactive graph representation of the EMM (using \pkg{igraph}).

\code{"graph"} produces a graph representation of the EMM (using \pkg{Rgraphviz}).

\code{"cluster_counts"} produces a barplot for cluster counts.

\code{"transition_counts"} produces a barplot for transition counts.

The following plotting parameters are currently supported:
\describe{
\item{state_counts}{represent state counts by vertex size? 
	(default: \code{TRUE})}
\item{arrow_width}{represent transition counts/probabilities by arrow width?
	(default: \code{TRUE})}
\item{arrows}{use \code{"counts"} or \code{"probabilities"} for 
    arrow width. (default: "counts")}
\item{arrow_width_multiplier, state_size_multiplier}{ Controls the variation
    of vertex sizes and edge widths (default: 1).}
\item{add_labels}{add labels for centers (n/a for \code{type="graph"}).}
\item{cluster_labels}{cluster labels to use instead of 1,2,....}
\item{mark_clusters}{Use different markers for points depending on the
    state they belong to (only available when \code{data} is specified).}
\item{mark_states}{a vector of state names to be marked.}
}
}
%\value{
%}
%\references{ ~put references to the literature/web site here ~ }
%\author{ ~~who you are~~ }
\seealso{\code{\linkS4class{EMM}}}
\examples{
data("EMMTraffic")
emm <- EMM(threshold=0.2, measure="eJaccard")
emm <- build(emm, EMMTraffic)

op <- par(mfrow = c(2, 2), pty = "s")
plot(emm, method="graph", main="Graph (plain)", 
    parameter=list(cluster_counts=FALSE, arrow_width=FALSE))
plot(emm, method="graph", main="Graph")
plot(emm, method="MDS", main="MDS projection", 
    xlim=c(-0.5,0.5), ylim= c(-0.5,0.5))
plot(emm, method="MDS", data = EMMTraffic, 
	main = "Projection of centers on data")
par(op)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
