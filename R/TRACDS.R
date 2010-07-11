setMethod("size", signature(x = "TRACDS"),
	function(x) smc_size(x@mm))

setMethod("states", signature(x = "TRACDS"),
	function(x) smc_states(x@mm))

setMethod("current_state", signature(x = "TRACDS"),
	function(x) x@current_state)

setMethod("transitions", signature(x = "TRACDS"),
	function(x) {
	    m <- smc_countMatrix(x@mm)
	    edges <- apply(which(m>0, arr.ind=T), 
		    MARGIN=2, FUN=function(x) colnames(m)[x])
	    colnames(edges) <- c("from", "to")
	    edges
	}
)

setMethod("rare_transitions", signature(x = "TRACDS"),
	function(x, count_threshold)
	transitions(x)[transition(x, transitions(x),
		type="counts") < count_threshold,]
	)

