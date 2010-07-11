setMethod("prune", signature(x = "EMM"),
	function(x, count_threshold, clusters = TRUE, transitions = TRUE){

		if(clusters) x <- 
		remove_clusters(x, rare_clusters(x, 
				count_threshold=count_threshold))

		if(transitions) x <- remove_transitions(x, 
			rare_transitions(x, count_threshold=count_threshold))

		x
	}
)
