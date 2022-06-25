# Changes in version 1.2.0 (06/25/2022)
* added interface for package stream

# Changes in version 1.1.1 (05/31/2022)
* score gained parameter random
* cleanup.

# Changes in version 1.1.0 (10/27/2021)

* Moved to Github.
* Updated package structure to new standards.

# Changes in version 1.0-11 (07/23/2015)
* Fixed NAMESPACE (non-standard imports)

# Changes in version 1.0-9 (5/15/2015)
* Fixed dependencies in NAMESPACE
* Fixed problems with testthat

# Changes in version 1.0-8 (1/14/2014)
* score: match_clusters can now be "exact", "nn" or "weighted", where 
  weighted replaces all the weighted scores. 
* added smooth_transitions (experimental)
* plus_one is now called prior (since it adds a uniform prior for 
  transition probabilities)
* score has now an argument called normalize

# Changes in version 1.0-7
* improved plot for tNN
* tNN: centroids stop moving to prevent 2 clusters from colliding
* fixed bug with getting transitions from a TRACDS object with 0 transitions
* find_clusters also accepts now a threshold factor for match_clusters
* score: weights for scoring are now normalized for threshold
* transition from igraph0 to igraph

# Changes in version 1.0-6
* Improved performance for prune()
* Added object.size() which also reports memory used by the model 
* Switched to igraph0

# Changes in version 1.0-5
* Added several methods for score.
* Threshold for pruning and finding rare clusters/transitions 
	is now less or equal instead of just less

# Changes in version 1.0-3
* Finished the transition to reference objects implemented 
	using environments.
* transition_table() now has for consistency also the default of 
	add_one set to FALSE.
* last_clustering() was added as an accessor function to tNN.
* 'igraph' is now the default plot method.
* Added combining EMMs.
* Added reclustering using tNN.
* Added coloring of states and transitions to plot().
* Added coercion functions as.igraph() and as.graph from TRACDS.

# Changes in version 1.0-2 
* service release
