## make  newdata a matrix (with a single row)
setMethod("cluster", signature(x = "tNN", newdata = "numeric"),
	function(x, newdata, verbose = FALSE) cluster(x, 
		as.matrix(rbind(newdata), verbose))
	)

setMethod("cluster", signature(x = "tNN", newdata = "data.frame"),
	function(x, newdata, verbose = FALSE) cluster(x, as.matrix(newdata), 
		verbose)
	)

setMethod("cluster", signature(x = "tNN", newdata = "matrix"),
	function(x, newdata, verbose = FALSE) {

	    #x@last <- character(nrow(newdata)) 
	    last <- character(nrow(newdata)) 
	    counts <- x@counts
	    centers <- x@centers
	    var_thresholds <- x@var_thresholds

	    for(i in 1:nrow(newdata)) 
	    {

		nd <- newdata[i,, drop = FALSE]
		if(verbose && i%%50==0) 
		    cat("Added", i, "observations -",nrow(centers), "states.\n")
		    #cat("Added", i, "observations -",size(x), "states.\n")

		## reset on all NAs
		if(all(is.na(nd))) {
		    #x@last[i] <- as.character(NA)
		    last[i] <- as.character(NA)
		    next
		}

		## fade cluster structure?
		#if(x@lambda>0) x@counts <- x@counts * x@lambda_factor
		if(x@lambda>0) counts <- counts * x@lambda_factor

		## first cluster
		#if(size(x)==0) {
		if(nrow(centers)==0) {
		    sel <- "1"
		    rownames(nd) <- sel
		    #x@centers <- nd
		    centers <- nd
		    #x@counts[sel] <- 1 
		    counts[sel] <- 1 
		    ## initialize threshold
		    #x@var_thresholds[sel] <- x@threshold
		    var_thresholds[sel] <- x@threshold

		}else{
		    ## find a matching state
		    #sel <- find_clusters(x, nd, match_cluster="exact")
		    inside <- dist(nd, centers, method=x@measure) - var_thresholds
		    min <- which.min(inside)
		    if(inside[min]<=0) sel <- rownames(centers)[min]
		    else sel <- NA

		    ## NA means no match -> create a new node
		    if(is.na(sel)) {
			## New node
			## get new node name (highest node 
			## number is last entry in count)
			#sel <- as.character(as.integer(tail(names(x@counts),1)) + 1)
			sel <- as.character(as.integer(tail(names(counts),1)) + 1)

			rownames(nd) <- sel
			#x@centers <- rbind(x@centers, nd)
			centers <- rbind(centers, nd)
			#x@sum_x <- rbind(x@sum_x, nd)
			#x@sum_x2 <- rbind(x@sum_x2, nd^2)
			#x@counts[sel] <- 1
			counts[sel] <- 1
			## initialize threshold
			#x@var_thresholds[sel] <- x@threshold
			var_thresholds[sel] <- x@threshold

		    }else{ 
			## assign observation to existing node

			## update center (if we use centroids)
			if(x@centroids) {

			    #nnas <- !is.na(nd)
			    #x@centers[sel,nnas] <- (x@centers[sel,nnas] * 
			    #    x@counts[sel] + nd[nnas])/(x@counts[sel]+1)
			    #nas <- is.na(x@centers[sel,])
			    #x@centers[sel,nas] <- nd[nas]
			    
			    nnas <- !is.na(nd)
			    centers[sel,nnas] <- (centers[sel,nnas] * 
				    counts[sel] + nd[nnas])/(counts[sel]+1)
			    nas <- is.na(centers[sel,])
			    centers[sel,nas] <- nd[nas]

			    #nnas <- !is.na(nd)
			    ## for sum_x and sum_x2 we have additivity
			    #x@sum_x[sel,nnas] <- x@sum_x[sel,nnas] + nd[nnas]
			    #x@sum_x2[sel,nnas] <- x@sum_x2[sel,nnas] + nd[nnas]^2
			    #nas <- is.na(x@sum_x[sel,])
			    #if(any(nas)) {
			    #    x@sum_x[sel,nas] <- nd[nas]
			    #x@sum_x2[sel,nas] <- nd[nas]^2
			    #}
			}

			## update counts
			#x@counts[sel] <- x@counts[sel] + 1
			counts[sel] <- counts[sel] + 1
		    }
		}

		#x@last[i] <- sel
		last[i] <- sel

	    }

	    ## fix emm object
	    x@last <- last 
	    x@counts <- counts
	    x@centers <- centers
	    x@var_thresholds <- var_thresholds

	    if(verbose) cat ("Done -",size(x), "states.\n")
	    x

	}
	)


