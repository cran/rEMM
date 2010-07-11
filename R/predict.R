## predict next n states using P^n
setMethod("predict", signature(object = "TRACDS"),
	function(object, n=1, current_state=NULL, 
		probabilities = FALSE) {

		## probabilistic max with random tie breaking
		.prob_max <- function(x) {
			m <- which(x==max(x))
			if(length(m)>1) m <- sample(m,1)
			m
		}

		emm <- object
		
		if(is.null(current_state)) current_state <- emm@current_state
		else current_state <- as.character(current_state)


		P <- transition_matrix(emm)
		## calculate P^n
		if(n>1) for(i in 1:(n-1)) P <- P%*%P

		prob <- P[current_state,]
		if(probabilities) return(prob)

		## we need random-tie breaking
		return(states(emm)[.prob_max(prob)])
	}
)
