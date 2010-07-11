
## clustering = TRUE gets integer
setMethod("merge_clusters", signature(x = "EMM", to_merge = "integer"),
	function(x, to_merge, clustering = FALSE, new_center = NULL) {
		
		## handle a clustering
		if(!clustering) stop("to_merge needs to be all character!")
		k <- max(to_merge)

		if(!is.null(new_center) && nrow(new_center) != k) 
		stop("new_center has not the right number of rows.")

		for(i in 1:k) x <- merge_clusters(x, names(to_merge)[to_merge==i],
			clustering = FALSE, new_center[i,])

		return(x)
	}
)

## clustering = FALSE gets character
setMethod("merge_clusters", signature(x = "EMM", to_merge = "character"),
	function(x, to_merge, clustering = FALSE, new_center = NULL) {

		if(clustering) stop("to_merge has the wrong format for clustering!")

		## nothing to do
		if(length(to_merge) < 2) return(x)

		new_state <- to_merge[1]
		to_delete <- states(x) %in% to_merge[-1]

		
		## TRACDS
		x@mm <- smc_mergeStates(x@mm, to_merge)
		
		## fix current state
		if(x@current_state %in% to_delete) 
		    x@current_state <- new_state
		

		## Clustering
		## save old state centers
		old_centers <- cluster_centers(x)[to_merge,]

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
				old_centers[which.max(cluster_counts(x)[to_merge]),]
			}
		}else{ 
			## user supplied new center
			if(identical(length(new_center), ncol(x@centers))) 
			x@centers[new_state,] <- new_center
			else stop("new_center does not have the correct length/ncol!")
		}


		x@counts[new_state] <- sum(x@counts[to_merge])

		x@centers <- x@centers[!to_delete,]
		#x@sum_x <- x@sum_x[!to_delete,]
		#x@sum_x2 <- x@sum_x2[!to_delete,]
		x@counts <- x@counts[!to_delete]



		## fixme: this only works for metric dissimilarities (distances)
		## new threshold is max. dissimilarity vom new centroid to any old
		## centroid + its threshold

		d <- dist(cluster_centers(x)[new_state,,drop=FALSE], old_centers, 
			method=x@measure)[1,]

		new_threshold <- max(d + x@var_thresholds[names(d)])
		names(new_threshold) <- new_state

		x@var_thresholds[new_state] <- new_threshold

		## remove var. thresholds
		x@var_thresholds <- x@var_thresholds[!to_delete]

		x
	}
)

