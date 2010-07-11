
### alias update
#setMethod("update", signature(object = "EMM"),
#	function(object, newdata, verbose = FALSE) build(object, 
#		newdata, verbose)
#)

## make  newdata a matrix (with a single row)
setMethod("build", signature(x = "EMM", newdata = "numeric"),
	function(x, newdata, verbose = FALSE) build(x, 
		as.matrix(rbind(newdata), verbose))
	)

setMethod("build", signature(x = "EMM", newdata = "data.frame"),
	function(x, newdata, verbose = FALSE) build(x, as.matrix(newdata), 
		verbose)
	)

setMethod("build", signature(x = "EMM", newdata = "matrix"),
	function(x, newdata, verbose = FALSE) {

	    ## inline functions for performance
	    .fade <- function() {
		#x@mm@initial_counts <<- x@mm@initial_counts * x@lambda_factor
		#x@mm@counts <<- x@mm@counts * x@lambda_factor
		initial_counts <<- initial_counts * x@lambda_factor
		counts <<- counts * x@lambda_factor
	    }

	    
	    .addNode <- function(name) {
		## expand?
		#if(x@mm@top < 1) x@mm <<- smc_expand(x@mm)
		if(top < 1) {
		    #.expand <- function() {
		    old_size <- length(initial_counts)

		    new_size <- old_size*2L
		    new_counts <- matrix(0, ncol=new_size, nrow=new_size)
		    new_counts[1:old_size, 1:old_size] <- counts
		    counts <<- new_counts

		    new_initial_counts <- numeric(new_size)
		    new_initial_counts[1:old_size] <- initial_counts
		    names(new_initial_counts)[1:old_size] <- names(initial_counts)
		    initial_counts <<- new_initial_counts

		    new_unused <- new_size:1
		    new_unused[(old_size+1):length(new_unused)] <- unused
		    unused <<- new_unused

		    top <<- old_size+top
		    #}
		
		}

		## add node
		#pos <- x@mm@unused[x@mm@top]
		#x@mm@unused[x@mm@top] <<- NA
		#x@mm@top <<- x@mm@top-1L
		#names(x@mm@initial_counts)[pos] <<- name

		pos <- unused[top]
		unused[top] <<- NA
		top <<- top-1L
		names(initial_counts)[pos] <<- name

		## delete takes care of this
		#x@mm@mat[pos,] <<- 0
		#x@mm@mat[,pos] <<- 0
		#x@initial_counts[pos] <<- 0

		pos
	    }

	    .incEdge <- function(from, to) {
		#x@mm@counts[from, to] <<- x@mm@counts[from, to] + 1
		counts[from, to] <<- counts[from, to] + 1
	    }
	    
	    ## take some elements out to improve performance
	    initial_counts <- x@mm@initial_counts
	    unused <- x@mm@unused
	    top <- x@mm@top
	    counts <- x@mm@counts
	    x@mm <- SimpleMC(0) ## otherwise cluster will copy it x times

	    ## cluster all the data
	    x <- cluster(x, newdata)

	    pos_current <- which(names(x@mm@initial_counts) == x@current_state)
	    
	    ## iterate over cluster assignments
	    for(sel in x@last) {

		## reset?
		if(is.na(sel)) {
		    pos_current <- numeric(0)
		    next
		}

		## fade cluster structure?
		if(x@lambda>0) .fade()

		#pos_new <- which(names(x@mm@initial_counts) == sel)
		pos_new <- which(names(initial_counts) == sel)
		
		## create state?
		if(!length(pos_new)) {
		    pos_new <- .addNode(sel)
		    #x@mm@initial_counts[pos_new] <- 0 
		    initial_counts[pos_new] <- 0 
		}

		## add transition
		## no current state
		if(!length(pos_current)) {
		    #x@mm@initial_counts[pos_new] <- x@mm@initial_counts[pos_new]+1 
		    initial_counts[pos_new] <- initial_counts[pos_new]+1 
		}else{
		    #.incEdge(pos_current, pos_new)
		    ### this is O(n^2) since "[<-" copies
		    counts[pos_current, pos_new] <- counts[pos_current, pos_new] + 1
		}

		## update current_state
		pos_current <- pos_new
	    }

	    ## put elements back in
	    x@mm@initial_counts <- initial_counts
	    x@mm@unused <- unused
	    x@mm@top <- top
	    x@mm@counts <- counts
	    ## save the last current state
	    x@current_state <- sel
	    
	    x
	}
	)
