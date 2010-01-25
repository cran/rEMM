setMethod("remove_states", signature(x = "EMM", to_remove = "character"),
	function(x, to_remove) {

		if(length(to_remove)==0) return(x)

		x@mm <- removeNode(to_remove, x@mm)
		to_remove <- states(x) %in% to_remove
		x@centers <- x@centers[!to_remove,]
		#x@sum_x <- x@sum_x[!to_remove,]
		#x@sum_x2 <- x@sum_x2[!to_remove,]
		x@counts <- x@counts[!to_remove]
		x@initial_counts <- x@initial_counts[!to_remove]
		x@var_thresholds <- x@var_thresholds[!to_remove]
		
		if(x@current_state == to_remove) x@current_state <- as.character(NA)


		x
	}
)

setMethod("remove_transitions", signature(x = "EMM", 
		from ="matrix", to="missing"),
	function(x, from, to) remove_transitions(x, from[,1], from[,2])
)

setMethod("remove_transitions", signature(x = "EMM", 
		from ="character", to="character"),
	function(x, from, to) {

		if(length(from) != length(to)) stop("length of from and to do not match!")
		if(length(from)==0) return(x)

		x@mm <- removeEdge(from, to, x@mm)
		x
	}
)

setMethod("remove_selftransitions", signature(x = "EMM"),
	function(x) {
		self <- states(x)[isAdjacent(x@mm, states(x), states(x))]
		remove_transitions(x, self, self)
	}
)
