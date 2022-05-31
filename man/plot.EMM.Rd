\name{plot}
\alias{plot,EMM,missing-method}
\alias{plot}
\title{Visualize EMM Objects }
\description{
Visualize EMM objects.}
\usage{
\S4method{plot}{EMM,missing}(x, y,
  method=c("igraph", "interactive", "graph", "MDS",
           "cluster_counts", "transition_counts"),
  data = NULL, parameter=NULL, ...)
}
\arguments{
  \item{x}{ an \code{EMM} object. }
  \item{y}{ unused (just for compatibility with the generic for plot in
   \pkg{graphics}) }
  \item{method}{ see details section.}
  \item{data}{ Project the state centers onto these data. Points which do
    not belong to any cluster are shown in blue.}
  \item{parameter}{ a list of parameters for plotting (see Details section). }
  \item{\dots}{ further arguments passed on to \code{\link{plot.default}} or \code{\link{plot.igraph}}. }
}
\details{
There are several methods for plotting:

\code{"igraph"} produces a graph representation of the EMM using \pkg{igraph}.
Additional arguments like \code{layout} are passed on to plot for igraph.

\code{"interactive"} produces an interactive graph representation of the EMM (using \pkg{igraph}).
Arguments suitable for \code{plot} in \pkg{igraph} can be
passed on as \code{...}.

\code{"graph"} produces a graph representation of the EMM using \pkg{Rgraphviz}.
If \pkg{Rgraphviz} is not installed/available then the method reverts to
\code{"igraph"}.

\code{"MDS"} projects the cluster centers into 2-dimensional space.

\code{"cluster_counts"} produces a barplot for cluster counts.

\code{"transition_counts"} produces a barplot for transition counts.

The following plotting parameters are currently supported (by some of the visualizations):
\describe{
\item{state_counts}{represent state counts by vertex size?
	(default: \code{TRUE})}
\item{arrow_width}{represent transition counts/probabilities by arrow width?
	(default: \code{TRUE})}
\item{arrows}{use \code{"counts"} or \code{"probabilities"} for
    arrow width. (default: "counts")}
\item{arrow_width_multiplier, state_size_multiplier}{ Controls the variation
    of vertex sizes and edge widths (default: 1).}
\item{add_labels}{add labels for centers (n/a for \code{type = "graph"}).}
\item{cluster_labels}{cluster labels to use instead of 1,2,....}
\item{mark_clusters}{Use different markers for points depending on the
    state they belong to (only available for MDS when
    \code{data} is specified).}
\item{draw_threshold}{draw a circle around state centers to indicate the area in which points
  are assigned to the cluster (experimental, only available for
MDS when \code{data} is specified). }
\item{mark_states, mark_state_color}{a vector of state names to be marked
and the color(s) used for marking (default: red).}
\item{mark_transitions, mark_transitions_color}{a vector of transition names
in the format "3->2" to be marked and the color(s)
used for marking (default: red).}
}

For some plots (e.g., \code{"igraph"}) \code{...} is passed on to the
primitive plotting function and can be used to change the plot (colors, etc.)
See \code{? igraph.plotting}.
For \code{"graph"} the two special parameters "nAttrs" and "eAttrs"
for node and edge attributes can be used.

}
\seealso{\code{\linkS4class{EMM}}}
\examples{
data("EMMTraffic")

emm <- EMM(threshold = 0.2,
  measure = "eJaccard",
  data = EMMTraffic)

op <- par(mfrow = c(2, 2), pty = "s")

plot(emm, main = "Graph")

## Plot the graph as a tree with a set root node and an aspect ratio of 1:1.
g <- as.igraph(emm)
plot(emm, main = "Graph (tree layout)",
  layout = layout_as_tree(g, root = 1), asp = 1)

plot( emm, method = "MDS",
  main = "Graph (MDS projection)", xlim = c(-0.5, 0.5), ylim = c(-0.5, 0.5)
)

plot(emm, method = "MDS", data = EMMTraffic,
  main = "Projection of cluster \ncenters on data")
par(op)
}
\keyword{hplot}
