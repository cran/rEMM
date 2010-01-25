
## constructor
EMM <- function(measure="euclidean", threshold=0.2, lambda=0, 
	centroids=identical(tolower(measure), "euclidean")) {

	emm <- new("EMM", measure=measure, threshold=threshold, lambda=lambda,
		centroids=centroids, lambda_factor = 2^(-lambda))	
}

## show
setMethod("show", signature(object = "EMM"),
        function(object) {
            cat("EMM with", size(object), "states.\n", 
                    "Measure:", object@measure, "\n",
                    "Threshold:", object@threshold, "\n",
                    "Centroid:", object@centroids, "\n",
                    "Lambda:", object@lambda, "\n"
                    )
            invisible(NULL)
        })

