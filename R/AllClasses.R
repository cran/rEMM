setClass("tNN",
	representation(
		measure     = "character",
		centroids   = "logical",
		threshold   = "numeric",
		centers		= "matrix",
		counts		= "numeric",
		var_thresholds	= "numeric",
		lambda		= "numeric",
		lambda_factor = "numeric"
	),

	prototype(
		measure = "euclidean", 
		centroids = TRUE,
		threshold = 0.2,
		centers = matrix(),
		counts = numeric(),
		var_thresholds  = numeric(),
		lambda = 0,
		lambda_factor = 1
	)

	## FIXME: Implement check
	#validity= function(object) {}
)



setClass("EMMLayer",
	    representation(
            mm		= "graphNEL", 
            current_state = "character",
			initial_counts = "numeric"
		),

		prototype(
			mm = new("graphNEL", edgemode="directed"),
			current_state = as.character(NA),
			initial_counts = numeric()
		),

		## FIXME: Implement check
		#validity= function(object) {
			#}
	)

setClass("EMM", contains = c("tNN", "EMMLayer"))

