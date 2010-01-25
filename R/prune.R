setMethod("prune", signature(x = "EMM"),
	function(x, count_threshold, states = TRUE, transitions = TRUE){

		if(states) x <- 
		remove_states(x, rare_states(x, count_threshold=count_threshold))

		if(transitions) x <- remove_transitions(x, 
			rare_transitions(x, count_threshold=count_threshold))

		x
	}
)

setMethod("rare_states", signature(x = "EMM"),
	function(x, count_threshold) 
	names(which(x@counts < count_threshold))
)

setMethod("rare_transitions", signature(x = "EMM"),
	function(x, count_threshold) 
	transitions(x)[transition(x, transitions(x), 
			type="counts") < count_threshold,]
)
