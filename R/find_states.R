
setMethod("find_states", signature(x = "tNN", newdata = "numeric"),
	function(x, newdata, match_state=c("exact", "nn"), dist=FALSE)
	find_states(x, as.matrix(rbind(newdata), match_state, dist))
)	

setMethod("find_states", signature(x = "tNN", newdata = "data.frame"),
	function(x, newdata, match_state=c("exact", "nn"), dist=FALSE) 
	find_states(x, as.matrix(newdata), match_state, dist))

setMethod("find_states", signature(x = "tNN", newdata = "matrix"),
	function(x, newdata, match_state=c("exact", "nn"), dist=FALSE) {

		match_state <- match.arg(match_state)

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
                            tmp <- find_states(x,
                                    newdata[blockStart:blockEnd,],
                                    match_state, dist)

                            states[blockStart:blockEnd] <- as.character(tmp[,1])
                            d_state[blockStart:blockEnd] <- tmp[,2]

                        }else states[blockStart:blockEnd] <- find_states(x, 
                                newdata[blockStart:blockEnd,],
                                match_state, dist)

                        blockStart <- blockEnd+1L
                    }
                    if(dist) return(data.frame(state = states, dist = d_state))
                    else return(states)
                }
                
                ## do it in one run 
                d <- dist(newdata, state_centers(x), method=x@measure)

                .which.min_NA <- function(x) {
			m <- which.min(x)
			if(length(m)==0) m <- NA
		        m	
		}
		
                if(match_state=="nn") {
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
