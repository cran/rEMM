
## constructor
EMM <- function(threshold=0.2, measure="euclidean", 
	centroids=identical(tolower(measure), "euclidean"), lambda=0) {

	new("EMM", measure=measure, threshold=threshold, lambda=lambda,
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

