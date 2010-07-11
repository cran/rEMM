setMethod("remove_clusters", signature(x = "EMM", to_remove = "character"),
	function(x, to_remove) {

		if(length(to_remove)==0) return(x)
		
		to_remove_pos <- states(x) %in% to_remove

		## TRACDS 
		x@mm <- smc_removeState(x@mm, to_remove)
		if(is.element(x@current_state, to_remove)) 
		    x@current_state <- as.character(NA)

		## tNN
		x@centers <- x@centers[!to_remove_pos,]
		#x@sum_x <- x@sum_x[!to_remove_pos,]
		#x@sum_x2 <- x@sum_x2[!to_remove_pos,]
		x@counts <- x@counts[!to_remove_pos]
		x@var_thresholds <- x@var_thresholds[!to_remove_pos]

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

		if(length(from) != length(to)) 
		    stop("length of from and to do not match!")
		if(length(from)==0) return(x)

		x@mm <- smc_removeTransition(x@mm,from, to)
		x
	}
)

setMethod("remove_selftransitions", signature(x = "EMM"),
	function(x) {
	   x@mm <- smc_removeSelfTransition(x@mm)	
	   x
	}
)
