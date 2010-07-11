## build uses its own implementation of fade! See build.R

.fade <- function(x, t=1, lambda = NULL) {
	if(is.null(lambda)) lambda_factor <- x@lambda_factor
	else lambda_factor <- 2^(-lambda)

	## fade counts (clustering)
	x@counts <- x@counts * lambda_factor^t 

	## fade transition counts (TRACDS)
	x@initial_counts <- x@initial_counts * lambda_factor^t 
	x <- smc_fade(x, lambda_factor^t)
	
	x
}


setMethod("fade", signature(x = "EMM", t= "numeric", lambda = "missing"),
	function(x, t, lambda) .fade(x, t)
)

setMethod("fade", signature(x = "EMM", t= "missing", lambda = "missing"),
	function(x, t, lambda) .fade(x, t=1)
)

setMethod("fade", signature(x = "EMM", t= "numeric", lambda = "numeric"),
	function(x, t, lambda) .fade(x, t, lambda) 
)
