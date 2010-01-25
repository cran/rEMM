
## These generics already exist:
## setGeneric("predict", function(object, ...) standardGeneric("predict"))
## setGeneric("plot", function(x, y, ...) standardGeneric("plot"))

## EMMLayer
## size is also used in package arules
setGeneric("size", function(x, ...) standardGeneric("size"))

setGeneric("current_state", function(x) standardGeneric("current_state"))
setGeneric("states", function(x) standardGeneric("states"))
setGeneric("transitions", function(x) standardGeneric("transitions"))
setGeneric("reset", function(x) standardGeneric("reset"))
setGeneric("transition", function(x, from, to, ...) 
	standardGeneric("transition"))
setGeneric("transition_matrix", function(x, ...) 
	standardGeneric("transition_matrix"))
setGeneric("transition_table", function(x, newdata, ...) 
	standardGeneric("transition_table"))
setGeneric("initial_transition", function(x, ...) 
	standardGeneric("initial_transition"))

## tNN
setGeneric("state_counts", function(x) standardGeneric("state_counts"))
setGeneric("state_centers", function(x) standardGeneric("state_centers"))
setGeneric("find_states", function(x, newdata, ...) 
	standardGeneric("find_states"))

## EMM
setGeneric("build", function(x, newdata, ...) standardGeneric("build"))
setGeneric("fade", function(x, t, lambda) standardGeneric("fade"))
setGeneric("merge_states", function(x, to_merge, ...) 
	standardGeneric("merge_states"))
setGeneric("prune", function(x, ...) 
	standardGeneric("prune"))
setGeneric("rare_states", function(x, count_threshold, ...) 
	standardGeneric("rare_states"))
setGeneric("rare_transitions", function(x, count_threshold, ...) 
	standardGeneric("rare_transitions"))
setGeneric("remove_states", function(x, to_remove) 
	standardGeneric("remove_states"))
setGeneric("remove_transitions", function(x, from, to) 
	standardGeneric("remove_transitions"))
setGeneric("remove_selftransitions", function(x) 
	standardGeneric("remove_selftransitions"))


## fixme: make it one recluster method
setGeneric("recluster_hclust", function(x, ...) 
	standardGeneric("recluster_hclust"))
setGeneric("recluster_kmeans", function(x, ...) 
	standardGeneric("recluster_kmeans"))
setGeneric("recluster_pam", function(x, ...) 
	standardGeneric("recluster_pam"))

setGeneric("score", function(x, newdata, ...) standardGeneric("score"))
