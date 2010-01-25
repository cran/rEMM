
## clustering = TRUE gets integer
setMethod("merge_states", signature(x = "EMM", to_merge = "integer"),
	function(x, to_merge, clustering = FALSE, new_center = NULL) {
		
		## handle a clustering
		if(!clustering) stop("to_merge needs to be all character!")
		k <- max(to_merge)

		if(!is.null(new_center) && nrow(new_center) != k) 
		stop("new_center has not the right number of rows.")

		for(i in 1:k) x <- merge_states(x, names(to_merge)[to_merge==i],
			clustering = FALSE, new_center[i,])

		return(x)
	}
)

## clustering = FALSE gets character
setMethod("merge_states", signature(x = "EMM", to_merge = "character"),
	function(x, to_merge, clustering = FALSE, new_center = NULL) {

		if(clustering) stop("to_merge has the wrong format for clustering!")

		## nothing to do
		if(length(to_merge) < 2) return(x)

		new_state <- to_merge[1]

		## handle edges between states to be merged
		new_w <- sum(sapply(edgeWeights(x@mm, to_merge), 
				FUN = function(e) sum(e[names(e) %in% to_merge])))

		## merge states into new_state
		x@mm <- combineNodes(to_merge, x@mm, new_state)

		## add edge new_state -> new_state
		if(new_w >0) x@mm <- addEdge(new_state, new_state, x@mm, new_w)

		## save old state centers
		old_centers <- state_centers(x)[to_merge,]

		## create new state
		if(is.null(new_center)) {
			if(x@centroids) {
				x@centers[new_state,] <- 
				colSums(old_centers*x@counts[to_merge])/
				sum(x@counts[to_merge])
				#x@sum_x[new_state,] <- colSums(x@sum_x[to_merge,]) 
				#x@sum_x2[new_state,] <- colSums(x@sum_x2[to_merge,]) 
			}else {
				## we take the medoid of the larger cluster
				x@centers[new_state,] <-
				old_centers[which.max(state_counts(x)[to_merge]),]
			}
		}else{ 
			## user supplied new center
			if(identical(length(new_center), ncol(x@centers))) 
			x@centers[new_state,] <- new_center
			else stop("new_center does not have the correct length/ncol!")
		}


		x@counts[new_state] <- sum(x@counts[to_merge])
		x@initial_counts[new_state] <- sum(x@initial_counts[to_merge])

		## remove states
		to_delete <- states(x) %in% to_merge[-1]
		x@centers <- x@centers[!to_delete,]
		#x@sum_x <- x@sum_x[!to_delete,]
		#x@sum_x2 <- x@sum_x2[!to_delete,]
		x@counts <- x@counts[!to_delete]
		x@initial_counts <- x@initial_counts[!to_delete]


		## fix current state
		if(x@current_state %in% to_delete) x@current_state <- new_state

		## fixme: this only works for metric dissimilarities (distances)
		## new threshold is max. dissimilarity vom new centroid to any old
		## centroid + its threshold

		d <- dist(state_centers(x)[new_state,,drop=FALSE], old_centers, 
			method=x@measure)[1,]

		new_threshold <- max(d + x@var_thresholds[names(d)])
		names(new_threshold) <- new_state

		x@var_thresholds[new_state] <- new_threshold

		## remove var. thresholds
		x@var_thresholds <- x@var_thresholds[!to_delete]

		x
	}
)

