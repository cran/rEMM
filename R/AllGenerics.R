
## These generics already exist:
## setGeneric("predict", function(object, ...) standardGeneric("predict"))
## setGeneric("plot", function(x, y, ...) standardGeneric("plot"))

## TRACDS
## size is also used in package arules
setGeneric("size", function(x, ...) standardGeneric("size"))
setGeneric("current_state", function(x) standardGeneric("current_state"))
setGeneric("states", function(x) standardGeneric("states"))
setGeneric("transitions", function(x) standardGeneric("transitions"))
setGeneric("transition", function(x, from, to, ...) 
	standardGeneric("transition"))
setGeneric("transition_matrix", function(x, ...) 
	standardGeneric("transition_matrix"))
setGeneric("transition_table", function(x, newdata, ...) 
	standardGeneric("transition_table"))
setGeneric("initial_transition", function(x, ...) 
	standardGeneric("initial_transition"))
setGeneric("rare_transitions", function(x, count_threshold, ...) 
	standardGeneric("rare_transitions"))
setGeneric("remove_transitions", function(x, from, to) 
	standardGeneric("remove_transitions"))
setGeneric("remove_selftransitions", function(x) 
	standardGeneric("remove_selftransitions"))

## tNN
setGeneric("cluster", function(x, newdata, ...) standardGeneric("cluster"))
setGeneric("clusters", function(x) standardGeneric("clusters"))
setGeneric("cluster_counts", function(x) standardGeneric("cluster_counts"))
setGeneric("cluster_centers", function(x) standardGeneric("cluster_centers"))
setGeneric("find_clusters", function(x, newdata, ...) 
	standardGeneric("find_clusters"))
setGeneric("rare_clusters", function(x, count_threshold, ...) 
	standardGeneric("rare_clusters"))

## EMM
setGeneric("build", function(x, newdata, ...) standardGeneric("build"))
#setGeneric("update", function(object, ...) standardGeneric("update"))
setGeneric("reset", function(x) standardGeneric("reset"))
setGeneric("score", function(x, newdata, ...) standardGeneric("score"))
setGeneric("fade", function(x, t, lambda) standardGeneric("fade"))
setGeneric("prune", function(x, ...) 
	standardGeneric("prune"))
setGeneric("merge_clusters", function(x, to_merge, ...) 
	standardGeneric("merge_clusters"))
setGeneric("remove_clusters", function(x, to_remove) 
	standardGeneric("remove_clusters"))


## fixme: make it one recluster method
setGeneric("recluster_hclust", function(x, ...) 
	standardGeneric("recluster_hclust"))
setGeneric("recluster_kmeans", function(x, ...) 
	standardGeneric("recluster_kmeans"))
setGeneric("recluster_pam", function(x, ...) 
	standardGeneric("recluster_pam"))
setGeneric("recluster_reachability", function(x, ...) 
	standardGeneric("recluster_reachability"))

