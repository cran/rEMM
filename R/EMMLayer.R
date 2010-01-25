setMethod("size", signature(x = "EMMLayer"),
	function(x) length(x@initial_counts))

setMethod("states", signature(x = "EMMLayer"),
	function(x) names(x@initial_counts))

setMethod("current_state", signature(x = "EMMLayer"),
	function(x) x@current_state)

setMethod("transitions", signature(x = "EMMLayer"),
	function(x) {
		ed <- edges(x@mm)
		edges <- NULL
		for(i in 1:length(ed)) {
			to <- as.integer(ed[[i]])
			from <- rep(as.integer(names(ed)[i]), length(to))
			edges <- rbind(edges, cbind(as.character(from), as.character(to)))
		}
		colnames(edges) <- c("from", "to")
		edges
	}
)
