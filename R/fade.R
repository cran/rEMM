## build uses its own implementation of fade! See build.R

.fade <- function(x, t=1, lambda = NULL) {
	if(is.null(lambda)) lambda_factor <- x@lambda_factor
	else lambda_factor <- 2^(-lambda)

	## fade counts (clustering)
	x@counts <- x@counts * lambda_factor^t 

	## fade transition counts (EMMLayer)
	x@initial_counts <- x@initial_counts * lambda_factor^t 
	#edgeWeights(x@mm) <-  lapply(edgeWeights(x@mm), "*", fade)
	## edgeWeights<- not implemented in graph so we have to do it low-level
	x@mm@edgeData@data <- lapply(x@mm@edgeData@data, FUN=function(z) {
			z$weight <- z$weight* lambda_factor^t
			z
		})

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
