setClass("StreamClustering")

setClass("tNN",
	contains = ("StreamClustering"),
	representation(
		measure		= "character",
		centroids	= "logical",
		threshold	= "numeric",
		centers		= "matrix",	## row names are cluster names
		counts		= "numeric",
		var_thresholds	= "numeric",
		lambda		= "numeric",
		lambda_factor	= "numeric",
		last		= "character"
	),

	prototype(
		measure		= "euclidean", 
		centroids	= TRUE,
		threshold	= 0.2,
		centers		= matrix(nrow=0, ncol=0),
		counts		= numeric(),
		var_thresholds  = numeric(),
		lambda		= 0,
		lambda_factor	= 1,
		last		= as.character(NA)
	)

	## FIXME: Implement check
	#validity= function(object) {}
)

.smc_size <- 10L
setClass("SimpleMC",
	representation(
		unused      = "integer", ## list of unused cols/rows
		top         = "integer", ## top of unused
		counts      = "matrix",
		initial_counts = "numeric" 
		),
	
	prototype(
		unused	    = .smc_size:1,
		top	    = .smc_size,
		counts	    = matrix(0, ncol=.smc_size, nrow=.smc_size),
		initial_counts = structure(rep(0, .smc_size), 
			names=rep(NA, .smc_size))  ## also holds cluster names
		)

	## FIXME: Implement check
	#validity= function(object) {
	#}
	)

setClass("TRACDS",
	representation(
		mm		= "SimpleMC", 
		current_state	= "character"
		),

	prototype(
		mm		= new("SimpleMC"),
		current_state	= as.character(NA)
		),

	## FIXME: Implement check
	#validity= function(object) {
	#}
	)


setClass("EMM", contains = c("tNN", "TRACDS"))

