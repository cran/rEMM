## creator function
tNN <- function(threshold = 0.2, measure = "euclidean", 
	centroids = identical(tolower(measure), "euclidean"), lambda=0) {
    new("tNN", threshold=threshold, measure=measure, centroids=centroids,
	    lambda=lambda, lambda_factor = 2^(-lambda))
}


setMethod("cluster_counts", signature(x = "tNN"),
	function(x) x@counts)

setMethod("cluster_centers", signature(x = "tNN"),
	function(x) x@centers)

setMethod("size", signature(x = "tNN"),
	function(x) nrow(x@centers))

setMethod("clusters", signature(x = "tNN"),
	function(x) rownames(x@centers))

setMethod("rare_clusters", signature(x = "tNN"),
	function(x, count_threshold)
	names(which(x@counts < count_threshold))
	)


## find clusters
setMethod("find_clusters", signature(x = "tNN", newdata = "numeric"),
	function(x, newdata, match_cluster=c("exact", "nn"), dist=FALSE)
	find_clusters(x, as.matrix(rbind(newdata), match_cluster, dist))
)	

setMethod("find_clusters", signature(x = "tNN", newdata = "data.frame"),
	function(x, newdata, match_cluster=c("exact", "nn"), dist=FALSE) 
	find_clusters(x, as.matrix(newdata), match_cluster, dist))

setMethod("find_clusters", signature(x = "tNN", newdata = "matrix"),
	function(x, newdata, match_cluster=c("exact", "nn"), dist=FALSE) {

		match_cluster <- match.arg(match_cluster)

		## cross-dissimilarities
                ## matrix can become too large for main memory
                ## estimate block size with 64 bit per distance entry
                ## and dist computation takes about 5* the memory
                
                maxmem <- 128L  ## max. approx. 128 MBytes
                blocksize <- as.integer(floor(maxmem * 1024 * 1024 
                                / size(x) / 8 / 5))

                if(nrow(newdata) > blocksize && nrow(newdata)!=1) {
                    states <- character(nrow(newdata))
                    if(dist) d_state <- numeric(nrow(newdata))

                    blockStart <- 1L
                    while(blockStart < nrow(newdata)) {
                        blockEnd <- min(blockStart+blocksize-1L, nrow(newdata))
                        #cat("doing",blockStart,blockEnd,"\n")
                        if(dist) {
                            tmp <- find_clusters(x,
                                    newdata[blockStart:blockEnd,],
                                    match_cluster, dist)

                            states[blockStart:blockEnd] <- as.character(tmp[,1])
                            d_state[blockStart:blockEnd] <- tmp[,2]

                        }else states[blockStart:blockEnd] <- find_clusters(x, 
                                newdata[blockStart:blockEnd,],
                                match_cluster, dist)

                        blockStart <- blockEnd+1L
                    }
                    if(dist) return(data.frame(state = states, dist = d_state))
                    else return(states)
                }
                
                ## do it in one run 
                d <- dist(newdata, cluster_centers(x), method=x@measure)

                .which.min_NA <- function(x) {
			m <- which.min(x)
			if(length(m)==0) m <- NA
		        m	
		}
		
                if(match_cluster=="nn") {
                    min <- apply(d, MARGIN=1, .which.min_NA)
                    closest <- states(x)[min]
                    if(dist) { 
                        d_state <- sapply(1:nrow(newdata),
                                FUN = function(i) d[i,min[i]])
                        return(data.frame(state = closest, dist = d_state))
                    }else return(closest)
                }

		## exact matching using thresholds (using the largest margin)
		## NA ... no match

		## subtract threshold and take the smallest value if <=0
		d2 <- d - matrix(x@var_thresholds,
			ncol=length(x@var_thresholds), nrow=nrow(d), byrow=TRUE)

                min <- apply(d2, MARGIN=1, .which.min_NA)
                closest <- states(x)[min]
                closest_val <- sapply(1:nrow(newdata),
                        FUN = function(i) d2[i,min[i]])
		closest[closest_val>0] <- NA
		
                if(dist) {
                    d_state <- sapply(1:nrow(newdata),
                            FUN = function(i) d[i,min[i]])
                    return(data.frame(state=closest, dist = d_state))
                }else return(closest)
	}
)
