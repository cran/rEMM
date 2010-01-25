
setMethod("transition", signature(x = "EMMLayer", 
		from = "matrix", to = "missing"),
	function(x, from, to, 
		type=c("probability", "counts", "log_odds"), plus_one = FALSE){

		to <- from[,2]
		from <- from[,1]

		transition(x, from, to, type, plus_one)
	}
)
		

setMethod("transition", signature(x = "EMMLayer", from = "character", to =
                "character"), function(x, from, to, type=c("probability",
                        "counts", "log_odds"), plus_one = FALSE){ 
            type <- match.arg(type)

            tm <- transition_matrix(x, type, plus_one)

            from <- match(from, states(x)) 
            to <- match(to, states(x)) 
            res <- sapply(1:length(from), FUN = function(i) tm[from[i], to[i]])
        
            ## handle missing states (NA) 
            res[is.na(res)] <- 0 
            res 
        })



setMethod("transition_matrix", signature(x = "EMMLayer"),
	function(x,
		type=c("probability", "counts", "log_odds"), plus_one = FALSE){
		type <- match.arg(type)

		#tm <- outer(states(x), states(x),
			#FUN = function(x, y) transition(x, x, y, type=type))
		#dimnames(tm) <- list(states(x), states(x))
		#tm

		## doing it sparse is much more efficient
		m <- matrix(0, ncol=size(x), nrow=size(x), 
			dimnames=list(states(x), states(x)))
		ew <- edgeWeights(x@mm, states(x))
		for(i in 1:length(ew)) m[i, names(ew[[i]])] <- ew[[i]]
		if(plus_one) m <- m+1

		if(type=="counts") return(m)

		rs <- rowSums(m)
		prob <- m/rs

		## we have to handle absorbing states here (row sum is 0)
		absorbing <- which(rs==0)
		prob[absorbing,] <- 0
		for(i in absorbing) prob[i,i] <- 1

		switch(type,
			probability = prob,
			log_odds = log(prob*size(x))
		)
	}
)


setMethod("initial_transition", signature(x = "EMMLayer"),
	function(x, 
		type=c("probability", "counts", "log_odds"), plus_one = FALSE){
		type <- match.arg(type)

		ic <- x@initial_counts
		if(plus_one) ic <- ic+1

		switch(type,
			probability = ic / sum(ic),
			counts = ic,
			log_odds = log(ic / sum(ic)* size(x))
		)
	}
)


setMethod("transition_table", signature(x = "EMM", newdata = "numeric"),
	function(x, newdata, method = c("prob", "counts", "log_odds"), 
		match_state="nn", plus_one = TRUE, 
		initial_transition = FALSE) 
	transition_table(x, as.matrix(rbind(newdata)), method, 
		match_state, plus_one, initial_transition)
)

setMethod("transition_table", signature(x = "EMM", newdata = "data.frame"),
	function(x, newdata, method = c("prob", "counts", "log_odds"), 
		match_state="nn", plus_one = TRUE, 
		initial_transition = FALSE) 
	transition_table(x, as.matrix(newdata), method, 
		match_state, plus_one, initial_transition)
)

setMethod("transition_table", signature(x = "EMM", newdata = "matrix"),
        function(x, newdata, method = c("prob", "counts", "log_odds"), 
                match_state="nn", plus_one = TRUE, 
                initial_transition = FALSE) {

            method <- match.arg(method)

            ## make sure  newdata is a matrix (maybe a single row)
            if(!is.matrix(newdata)) newdata <- as.matrix(rbind(newdata))
            n <- nrow(newdata)

            ## empty EMM or single state?
            if(n<2) { 
                df <- data.frame(from=NA, to=NA, val=NA)
                names(df)[3] <- method
                return(df)
            }

            ## get sequence
            ssequence <- find_states(x, newdata, match_state=match_state, 
                    dist=FALSE)
            from <- ssequence[1:(n-1)]
            to <- ssequence[2:n]

            ## get values
            res <- transition(x, from, to, type=method, 
                    plus_one=plus_one)

            if(initial_transition) {
                from <- c(NA, from)
                to <- c(ssequence[1], to)
                res <- c(initial_transition(x, type=method, 
                                plus_one=plus_one)[ssequence[1]], 
                        res)
            }

            df <- data.frame(from=from, to=to, val=res)
            names(df)[3] <- method
            return(df)
        })
